/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.il.DemoExitDataHelper.SpansFactory.DemoExitSpan;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Illinois state export procedure for Exit Enrollment
 *
 * Searches withdrawn student enrollments from active students.
 *
 * @author X2 Development Corporation
 */
public class ILExitEnrollment extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the IL.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class ExitEnrollmentEntity extends StateReportEntity {
        /**
         *
         */
        private static final String EXIT_CODE_06 = "06";
        /**
         * Students retrieving data collection.
         */
        ArrayList<DemoExitSpan> m_spans = null;
        Set<PlainDate> m_yearInSessionDates = null;

        /**
         * ExitEnrollment data.
         */
        ILExitEnrollment m_eeData = null;

        private SisStudent m_student = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ExitEnrollmentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * return student attendances limited by start and end days in current span
         *
         * @return
         */
        public List<StudentAttendance> getAttendancesLimitedByCurrentSpan() {
            List<StudentAttendance> attendanceInSpan = new ArrayList<StudentAttendance>();
            DemoExitSpan demoExitSpan = getCurrentSpan();

            String schoolOid = m_eeData.m_dataHelper.getSchoolCodeBySchOid(demoExitSpan.getHomeSchool());
            PlainDate beginDate = demoExitSpan.getStartDate();
            PlainDate endDate = demoExitSpan.getExitDate();

            Map<String, Collection<StudentAttendance>> schoolStdAtt = m_eeData.m_attMap.get(schoolOid);
            List<StudentAttendance> stdAttendances = null;
            if (schoolStdAtt != null && schoolStdAtt.containsKey(m_student.getOid())) {
                stdAttendances = new ArrayList<StudentAttendance>(schoolStdAtt.get(m_student.getOid()));
            } else {
                stdAttendances = new ArrayList<StudentAttendance>();
            }

            for (StudentAttendance attendance : stdAttendances) {
                PlainDate currentDate = attendance.getDate();
                if ((endDate == null || !currentDate.after(endDate)) && !currentDate.before(beginDate)) {
                    attendanceInSpan.add(attendance);
                }
            }
            return attendanceInSpan;
        }

        /**
         * Gets the current span.
         *
         * @return Demo exit span
         */
        public DemoExitSpan getCurrentSpan() {
            return m_spans.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = m_student.getNameView() +
                    " [LASID: " + m_student.getLocalId() +
                    ", SASID: " + m_student.getStateId() + "]" +
                    " Withdraw date: " + getCurrentSpan().getExitDate();

            return name;
        }

        /**
         * return count in session days limited by start and end days in current span
         *
         * @return
         */
        public int getMembershipCountForCurrentSpan() {
            int totalMembership = 0;
            DemoExitSpan demoExitSpan = getCurrentSpan();
            PlainDate beginDate = demoExitSpan.getStartDate();
            PlainDate endDate = demoExitSpan.getExitDate();
            Set<PlainDate> yearInSessionDates = getYearInSessionDaysForCurrentStudent();
            for (PlainDate currentDate : yearInSessionDates) {
                if ((endDate == null || !currentDate.after(endDate)) && !currentDate.before(beginDate)) {
                    totalMembership++;
                }
            }
            return totalMembership;
        }

        /**
         * return in session days, belong to current school year context, span school and student
         * calendar id.
         * if school hasn't student calendar id- using any first existing.
         *
         * @return
         */
        public Set<PlainDate> getYearInSessionDaysForCurrentStudent() {
            if (m_yearInSessionDates == null) {
                DemoExitSpan demoExitSpan = getCurrentSpan();
                String schoolOid = demoExitSpan.getSchool().getOid();
                String calendarId = m_student.getCalendarCode();

                Map<String, Collection<SisSchoolCalendarDate>> calendars = m_eeData.m_calendarData.get(schoolOid);
                Collection<SisSchoolCalendarDate> tempColl = null;
                if (calendars != null) {
                    tempColl = calendars.get(calendarId);

                    if (tempColl == null && !calendars.isEmpty()) {
                        calendarId = calendars.keySet().iterator().next();
                        tempColl = calendars.get(calendarId);
                    }
                }
                if (tempColl != null) {
                    m_yearInSessionDates = new HashSet<PlainDate>();
                    for (SisSchoolCalendarDate calendarDate : tempColl) {
                        m_yearInSessionDates.add(calendarDate.getDate());
                    }
                }
            }

            if (m_yearInSessionDates == null) {
                m_yearInSessionDates = new HashSet<PlainDate>();
            }
            return m_yearInSessionDates;
        }

        /**
         * Initialize and increment counter.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_eeData = (ILExitEnrollment) data;
            m_student = ((SisStudent) bean);

            HashSet<DemoExitSpan> allSpans = m_eeData.m_dataHelper.getSpans(m_student);
            TreeSet<DemoExitSpan> orderedByDateSpans = new TreeSet<DemoExitSpan>(new Comparator<DemoExitSpan>() {
                @Override
                public int compare(DemoExitSpan o1, DemoExitSpan o2) {
                    if (o1.equals(o2)) {
                        return 0;
                    } else if (o1.getExitDate() != null && o2.getExitDate() != null) {
                        return o1.getExitDate().compareTo(o2.getExitDate());
                    } else if (o1.getExitDate() == null && o2.getExitDate() != null) {
                        return 1;
                    } else if (o1.getExitDate() != null && o2.getExitDate() == null) {
                        return -1;
                    }

                    return String.valueOf(o1.hashCode()).compareTo(String.valueOf(o2.hashCode()));
                }
            });


            if (allSpans != null) {
                orderedByDateSpans.addAll(allSpans);
                if (m_eeData.m_exitEnrollment.booleanValue()) {
                    // When EOY = True and student current status is Inactive/Not Active, don't
                    // create a withdrawal record for the student.
                    if (!StringUtils.isEmpty(m_student.getEnrollmentStatus()) &&
                            !m_student.getEnrollmentStatus().equals(m_eeData.m_preferenceStudentActiveStatus)) {
                        setRowCount(0);
                        return;
                    }

                    deleteExitSpans(allSpans);
                    finishSpans(allSpans);
                } else {
                    finishEarlyGraduated(allSpans);
                }

                if (m_eeData.m_allWithdrawals.booleanValue()) {
                    remainExitSpans(allSpans);
                } else {
                    remainLastExitSpans(allSpans);
                }

                m_spans = new ArrayList<DemoExitSpan>(allSpans);

                // keep count of records
                ((ILExitEnrollment) data).m_totalEnrollmentCount += m_spans.size();
                setRowCount(m_spans.size());
            } else {
                setRowCount(0);
            }
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * Delete exit spans.
         *
         * @param spans HashSet<DemoExitSpan>
         */
        private void deleteExitSpans(HashSet<DemoExitSpan> spans) {
            // remove spans with exit dates.
            Iterator<DemoExitSpan> iterator = spans.iterator();

            while (iterator.hasNext()) {
                DemoExitSpan span = iterator.next();

                if (span.getExitDate() != null) {
                    iterator.remove();
                }
            }
        }

        /**
         * Finish early graduated.
         *
         * @param spans HashSet<DemoExitSpan>
         */
        private void finishEarlyGraduated(HashSet<DemoExitSpan> spans) {
            // remove spans without exit dates.
            Iterator<DemoExitSpan> iterator = spans.iterator();
            Date parsedDate = null;
            String date = (String) m_student.getFieldValueByBeanPath(m_eeData.m_fieldProjGradDate);
            if (!StringUtils.isEmpty(date)) {
                try {
                    parsedDate = m_eeData.m_projGradDateFormat.parse(date);
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }

            if (parsedDate != null) {
                boolean projectedDateInReportDays = !parsedDate.before(m_eeData.m_lastSubmissionDate) &&
                        !parsedDate.after(m_eeData.m_curSubmissionDate);

                while (iterator.hasNext()) {
                    DemoExitSpan span = iterator.next();
                    PlainDate exitDate = span.getExitDate();
                    boolean hasExitDay = exitDate != null;
                    boolean exitDaybeforeProjected = hasExitDay && exitDate.before(parsedDate);
                    if (projectedDateInReportDays && !exitDaybeforeProjected) {
                        span.setExitCode(EXIT_CODE_06);
                        span.setExitDate(new PlainDate(parsedDate));
                    }
                }
            }
        }

        /**
         * Finish spans.
         *
         * @param spans HashSet<DemoExitSpan>
         */
        private void finishSpans(HashSet<DemoExitSpan> spans) {
            // remove spans without exit dates.
            Iterator<DemoExitSpan> iterator = spans.iterator();

            PlainDate assignedDate = null;
            String assignedCode = null;

            while (iterator.hasNext()) {
                DemoExitSpan span = iterator.next();

                if (StringUtils.isEmpty(m_eeData.m_helperMode) ||
                        m_eeData.m_helperMode.equals(DemoExitDataHelper.MODE_PRIMARY_SCHOOL)) {
                    // EXCLUDE exit enrollment records from being created for students enrolled
                    // in a school where School.[DOE SUMMER IND] = True
                    School school = span.getSchool();
                    if (school != null && school.getFieldValueByBeanPath(m_eeData.m_fieldSummerIndicator) != null &&
                            school.getFieldValueByBeanPath(m_eeData.m_fieldSummerIndicator)
                                    .equals(BooleanAsStringConverter.TRUE)) {
                        continue;
                    }
                }

                if (span.getExitDate() == null) {
                    if (span.getDemoDataset() != null && span.getDemoDataset().getGradeLvl() != null
                            && span.getDemoDataset().getGradeLvl().equals("12")) {
                        String date = (String) m_student.getFieldValueByBeanPath(m_eeData.m_fieldProjGradDate);
                        if (!StringUtils.isEmpty(date)) {
                            span.setExitCode(EXIT_CODE_06);
                            Date parsedDate = null;
                            try {
                                parsedDate = m_eeData.m_projGradDateFormat.parse(date);
                            } catch (ParseException e) {
                                e.printStackTrace();
                            }
                            span.setExitDate(new PlainDate(parsedDate));
                        } else {
                            span.setExitCode("12");

                            Map<String, PlainDate> calIdDateMap = getCalIdDateMap();
                            if (calIdDateMap != null) {
                                span.setExitDate(calIdDateMap.get(m_student.getCalendarCode()));
                            }
                        }
                    } else {
                        span.setExitCode("05");
                        Map<String, PlainDate> calIdDateMap = null;
                        calIdDateMap = getCalIdDateMap();

                        if (calIdDateMap != null) {
                            span.setExitDate(calIdDateMap.get(m_student.getCalendarCode()));
                        }
                    }
                    assignedDate = span.getExitDate();
                    assignedCode = span.getExitCode();
                }
                /*
                 * When a 12th grade student meets to requirements of being reported as a Graduate
                 * (WD 06)
                 * all subsequent secondary records must also return as (06). When a 12th grade
                 * student meets
                 * to requirements of being reported as a "Retained" (WD 12) all subsequent
                 * secondary records must
                 * also return as 12. Any student reported as '05' Promoted, must also be reported
                 * as promoted on the
                 * secondary exit record.
                 */
                else if (assignedDate != null && !span.getExitDate().before(assignedDate)) {
                    span.setExitCode(assignedCode);
                }
            }
        }

        /**
         * Gets the cal id date map.
         *
         * @return Map
         */
        private Map<String, PlainDate> getCalIdDateMap() {
            Map<String, PlainDate> calIdDateMap = null;
            if (StringUtils.isEmpty(m_eeData.m_helperMode) ||
                    m_eeData.m_helperMode.equals(DemoExitDataHelper.MODE_PRIMARY_SCHOOL)) {
                calIdDateMap = m_eeData.m_lastInstrDateMap.get(m_student.getSchoolOid());
            } else {
                // If summer school mode, use one of student's secondary summer schools for this
                // year.
                Collection<String> summerSchoolOids = m_eeData.m_studentSummerSchoolsMap.get(m_student.getOid());
                if (summerSchoolOids != null) {
                    for (String summerSchoolOid : summerSchoolOids) {
                        calIdDateMap = m_eeData.m_lastInstrDateMap.get(summerSchoolOid);
                        if (calIdDateMap != null) {
                            break;
                        }
                    }
                }
            }
            return calIdDateMap;
        }

        /**
         * Remain exit spans.
         *
         * @param spans HashSet<DemoExitSpan>
         */
        private void remainExitSpans(HashSet<DemoExitSpan> spans) {
            // remove spans without exit dates.
            Iterator<DemoExitSpan> iterator = spans.iterator();

            while (iterator.hasNext()) {
                DemoExitSpan span = iterator.next();

                if (span.getExitDate() == null) {
                    iterator.remove();
                }
            }
        }

        /**
         * Remain last exit spans.
         *
         * @param spans HashSet<DemoExitSpan>
         */
        private void remainLastExitSpans(HashSet<DemoExitSpan> spans) {
            PlainDate lastDate = null;

            Iterator<DemoExitSpan> iterator = spans.iterator();

            while (iterator.hasNext()) {
                DemoExitSpan span = iterator.next();

                if (span.getExitDate() != null) {
                    if (lastDate == null) {
                        lastDate = span.getExitDate();
                    } else {
                        if (!lastDate.after(span.getExitDate())) {
                            lastDate = span.getExitDate();
                        } else {
                            iterator.remove();
                        }
                    }
                } else {
                    iterator.remove();
                }
            }

            if (lastDate != null) {
                iterator = spans.iterator();

                while (iterator.hasNext()) {
                    DemoExitSpan span = iterator.next();

                    // Determine spans with last date.

                    if (span.getExitDate().before(lastDate)) {
                        iterator.remove();
                    }
                }
            }
        }
    }

    /**
     * retriever for calculate unexcused days.
     * sum current span attendance portion where absent indicator true and excused indicator false.
     *
     * @author Follett Software Company
     * @copyright 2017
     */
    protected class RetrieveUnExcusedDays implements FieldRetriever {

        private final static String CAL_ID = "EX-EXIT-UNEXCUSED";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            ExitEnrollmentEntity eeEntity = (ExitEnrollmentEntity) entity;
            double spanUnExcusedCount = 0;
            for (StudentAttendance att : eeEntity.getAttendancesLimitedByCurrentSpan()) {
                if (att.getAbsentIndicator() && !att.getExcusedIndicator()) {
                    double portion = att.getPortionAbsent().doubleValue();
                    spanUnExcusedCount += portion;
                }
            }
            return new BigDecimal(spanUnExcusedCount);
        }
    }

    /**
     * retriever for calculate excused days.
     * sum current span attendance portion where absent indicator true and excused indicator true.
     *
     * @author Follett Software Company
     * @copyright 2017
     */
    protected class RetrieveExcusedDays implements FieldRetriever {

        private final static String CAL_ID = "EX-EXIT-EXCUSED";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            ExitEnrollmentEntity eeEntity = (ExitEnrollmentEntity) entity;
            double spanExcusedCount = 0;
            for (StudentAttendance att : eeEntity.getAttendancesLimitedByCurrentSpan()) {
                if (att.getAbsentIndicator() && att.getExcusedIndicator()) {
                    double portion = att.getPortionAbsent().doubleValue();
                    spanExcusedCount += portion;
                }
            }
            return new BigDecimal(spanExcusedCount);
        }

    }

    /**
     *
     * retriever for calculate present days.
     * current span total membership (in session) days minus sum current span attendance portion
     * where absent indicator true
     *
     * @author Follett Software Company
     * @copyright 2017
     */
    protected class RetrievePresentDays implements FieldRetriever {

        private final static String CAL_ID = "EX-EXIT-PRESENT";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            ExitEnrollmentEntity eeEntity = (ExitEnrollmentEntity) entity;
            double spanPresentDaysCount = eeEntity.getMembershipCountForCurrentSpan();
            for (StudentAttendance att : eeEntity.getAttendancesLimitedByCurrentSpan()) {
                if (att.getAbsentIndicator()) {
                    double portion = att.getPortionAbsent().doubleValue();
                    spanPresentDaysCount -= portion;
                }
            }
            return new BigDecimal(spanPresentDaysCount);
        }

    }


    /**
     * Retrieve the exit date.
     *
     * @author Follett Software
     *
     */
    protected class RetrieveExitDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            ExitEnrollmentEntity exitEnrollmentEntity = (ExitEnrollmentEntity) entity;
            return exitEnrollmentEntity.getCurrentSpan().getExitDate();
        }

    }

    /**
     * Retrieve the exit code.
     *
     * @author Follett Software
     *
     */
    protected class RetrieveExitType implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ExitEnrollmentEntity exitEnrollmentEntity = (ExitEnrollmentEntity) entity;

            return exitEnrollmentEntity.getCurrentSpan().getExitCode();
        }

    }

    /**
     * Retrieve the RCDTS of the student's home/serving school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS
     * - "S" for the serving school's RCDTS.
     *
     * @author Follett Software
     */
    protected class RetrieveRCDTS implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = ((String) field.getParameter()).substring(0, 1);
            String rcdts = null;

            if (param.equals("H")) {
                rcdts = ((ExitEnrollmentEntity) entity).getCurrentSpan().getHomeSchool();
            } else if (param.equals("S")) {
                rcdts = ((ExitEnrollmentEntity) entity).getCurrentSpan().getServingSchool();
            }
            return rcdts;
        }

    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
     *
     * @author Follett Software
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (nameValue != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll("");
            } else {
                cleanValue = "";
            }

            return cleanValue;
        }

    }


    /**
     * Checks for the following validations:
     *
     * - A student must be IEP and in Grades 11 or 12 before being exited with
     * Exit Type Codes "14" Aged Out or "15" Certificate of Completion.
     *
     * - A Grade 12 student cannot have an Exit Type code of "05" Promoted
     *
     * - Only Grades 11 or 12 can have a "06" Graduation Exit Type code.
     *
     * - If a Birth to 3 Student is Exited, only the following Exit Type Codes
     * can be entered (New for School Year 2012):
     * 01, 02, 03, 04, 05, 07, 09, 11, 12, 17, 18
     *
     *
     * @author Follett Software
     *
     */
    protected class ValidateExitCode implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String exitCode = entity.getFieldValue("Exit/Withdrawal Type");
            SisStudent student = (SisStudent) entity.getBean();
            String gradeLevel = student.getGradeLevel();

            String spedStatusCode = student.getSpedStatusCode();
            Boolean isIep = Boolean.valueOf(StringUtils.isEqual(spedStatusCode, m_spedActiveCode));
            if (student.getSpedExitDate() != null && m_curSubmissionDate.after(student.getSpedExitDate())) {
                isIep = Boolean.FALSE;
            }

            /*
             * A student must be IEP and in Grades 11 or 12 before being exited with
             * Exit Type Codes "14" Aged Out or "15" Certificate of Completion.
             */
            if ((!isIep.booleanValue() || !gradeLevel.matches("11|12")) && exitCode.matches("14|15")) {
                errors.add(new StateReportValidationError(entity, field,
                        "A student must have an IEP and in Grades 11 or 12 for exit code to be 14 or 15",
                        "Student grade level = " + STYLE_BOLD + gradeLevel + STYLE_END +
                                ", exit code = " + STYLE_BOLD + exitCode + STYLE_END));
            }

            /*
             * A Grade 12 student cannot have an Exit Type code of "05" Promoted
             */
            if (gradeLevel.equals("12") && exitCode.equals("05")) {
                errors.add(new StateReportValidationError(entity, field,
                        "A Grade 12 student cannot have an Exit Type code of 05 Promoted",
                        "Student grade level = " + STYLE_BOLD + gradeLevel + STYLE_END +
                                ", exit code = " + STYLE_BOLD + exitCode + STYLE_END));
            }

            /*
             * Only Grades 11 or 12 can have a "06" Graduation Exit Type code.
             */
            if (!gradeLevel.matches("11|12") && exitCode.equals("06")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Only Grades 11 or 12 can have a 06 Graduation Exit Type code.",
                        "Student grade level = " + STYLE_BOLD + gradeLevel + STYLE_END +
                                ", exit code = " + STYLE_BOLD + exitCode + STYLE_END));
            }

            /*
             * If a Birth to 3 (00) Student is Exited, only the following Exit Type Codes
             * can be entered (New for School Year 2012):
             * 01, 02, 03, 04, 05, 07, 09, 11, 12, 17, 18
             */
            if (gradeLevel.equals("00") && !exitCode.matches("01|02|03|04|05|07|09|11|12|17|18")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Birth to 3 students can only exit with the following exit type codes: " +
                                "01, 02, 03, 04, 05, 07, 09, 11, 12, 17, 18",
                        "Student grade level = " + STYLE_BOLD + gradeLevel + STYLE_END +
                                ", exit code = " + STYLE_BOLD + exitCode + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Exit Date cannot be a future date (relative to the report date).
     *
     * @author Follett Software
     */
    protected class ValidateExitDate implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            Date exitDate;
            try {
                exitDate = m_dateFormat.parse(entity.getFieldValue(COLUMN_EXIT_DATE));
                if (exitDate.after(m_curSubmissionDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Exit Date cannot be a future date",
                            "Exit Date = " + STYLE_BOLD + m_dateFormat.format(exitDate) + STYLE_END));
                }
            } catch (ParseException e) {
                errors.add(new StateReportValidationError(entity, field,
                        "Exit date formatted incorrectly, must be in MM/dd/yyyy",
                        "Exit Date = " + STYLE_BOLD + entity.getFieldValue("Enrollment Exit Date") + STYLE_END));
            }
            return errors;
        }

    }

    // aliases
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_PROJ_GRAD_DATE = "DOE PROJECTED GRADUATION DATE";
    protected static final String ALIAS_SUMMER_IND = "DOE SUMMER IND";

    // columns
    protected static final String COLUMN_EXIT_DATE = "Enrollment Exit Date";

    // parameters
    private static final String PARAM_ALL_WITHDRAWALS = "allWithdrawals";
    private static final String PARAM_CURRENT_SUBMISSION_DATE = "curSubmissionDate";
    private static final String PARAM_EXIT_ENROLLMENT = "exitEnrollment";
    private static final String PARAM_HELPER_MODE = "helperMode";
    private static final String PARAM_LAST_SUBMISSION_DATE = "lastSubmissionDate";
    private static final String PARAM_SORT = "sort";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";

    /*
     * Other internal constants
     * This is a regex format to only allow for whitespace or -
     */
    protected static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-\\s]]";

    // javanames
    protected Boolean m_allWithdrawals;
    protected Boolean m_exitEnrollment;
    protected String m_fieldProjGradDate = null;
    protected String m_fieldSummerIndicator = null;
    protected PlainDate m_curSubmissionDate;
    protected DemoExitDataHelper m_dataHelper = null;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected String m_helperMode;
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected ArrayList<DistrictCalendar> m_inSessionDates;
    protected PlainDate m_lastSubmissionDate;
    protected Map<String, Map<String, PlainDate>> m_lastInstrDateMap;
    protected String m_preferenceStudentActiveStatus;
    protected SimpleDateFormat m_projGradDateFormat = new SimpleDateFormat("yyyy-MM-dd");
    protected String m_spedActiveCode;
    protected Map<String, Collection<String>> m_studentSummerSchoolsMap;
    protected int m_totalEnrollmentCount;
    protected Map<String, Map<String, Collection<SisSchoolCalendarDate>>> m_calendarData;
    protected Map<String, Map<String, Collection<StudentAttendance>>> m_attMap;
    private String m_fieldDistrictCode = null;

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        heading.append("Exit Student Enrollment V2");
        heading.append(',');
        heading.append(m_totalEnrollmentCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(m_dateFormat.format(new Date()));
        heading.append(',');
        heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        heading.append("\n");
        return heading.toString();
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldProjGradDate = translateAliasToJavaName(ALIAS_PROJ_GRAD_DATE, true);
        m_fieldSummerIndicator = translateAliasToJavaName(ALIAS_SUMMER_IND, true);

        m_helperMode = (String) getParameter(PARAM_HELPER_MODE);

        initInSessionDates();

        m_spedActiveCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_ACTIVE_CODE);
        m_preferenceStudentActiveStatus =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        m_exitEnrollment = (Boolean) getParameter(PARAM_EXIT_ENROLLMENT);

        if (m_exitEnrollment.booleanValue()) {
            m_curSubmissionDate = getLastInSessionDate(getCurrentContext().getEndDate());
            // do not use last inSessionDate
            if (m_curSubmissionDate.equals(m_inSessionDates.get(m_inSessionDates.size() - 1).getDate())) {
                m_curSubmissionDate = m_inSessionDates.get(m_inSessionDates.size() - 2).getDate();
            }
        } else {
            m_lastSubmissionDate = (PlainDate) getParameter(PARAM_LAST_SUBMISSION_DATE);
            m_curSubmissionDate = (PlainDate) getParameter(PARAM_CURRENT_SUBMISSION_DATE);
        }

        m_allWithdrawals = (Boolean) getParameter(PARAM_ALL_WITHDRAWALS);

        if (m_curSubmissionDate == null) {
            m_curSubmissionDate = new PlainDate(new Date());
        }

        if (m_lastSubmissionDate == null) {
            m_lastSubmissionDate = getLastInSessionDate(m_curSubmissionDate);
        }

        m_dataHelper = new DemoExitDataHelper(this);
        if (getSetupErrors().isEmpty()) {
            if (!StringUtils.isEmpty(m_helperMode) &&
                    m_helperMode.equals(DemoExitDataHelper.MODE_SUMMER_SCHOOL)) {
                m_dataHelper.setHelperMode(DemoExitDataHelper.MODE_SUMMER_SCHOOL);
                initStudentSummerSchoolsMap();
            }
            m_dataHelper.setCurrentSubmissionDate(m_curSubmissionDate);
            m_dataHelper.setLastSubmissionDate(m_lastSubmissionDate);

            X2Criteria studentCriteria = m_dataHelper.getEnrollmentHelper().getStudentCriteria();

            String queryString = (String) getParameter(PARAM_QUERY_STRING);
            int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
            switch (queryBy) {
                case 1: // YOG
                    studentCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                    break;

                case 2: // LASID
                    studentCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                    break;

                case 3: // SASID
                    studentCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                    break;

                case 4: // Snapshot
                    addRecordSetCriteria(studentCriteria, queryString);
                    break;

                default:
                    // Take all students in the district
                    break;
            }

            initCasMap();
            initAttendanceMap(studentCriteria.copy());
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

            Integer sort = (Integer) getParameter(PARAM_SORT);
            switch (sort != null ? sort.intValue() : 0) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentQuery
                            .addOrderByAscending(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER
                                    + SisSchool.COL_NAME);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            setQuery(studentQuery);
            m_dataHelper.initializeSpans();

            setEntityClass(ExitEnrollmentEntity.class);

            // Build a map of calculations/retrievers.
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("EX-ENROLL-RCDTS", new RetrieveRCDTS());
            calcs.put("EX-EXIT-TYPE", new RetrieveExitType());
            calcs.put("EX-DATE-TYPE", new RetrieveExitDate());
            calcs.put("EX-CLEAN-NAME", new RetrieveStripNameChar());
            calcs.put(RetrieveUnExcusedDays.CAL_ID, new RetrieveUnExcusedDays());
            calcs.put(RetrieveExcusedDays.CAL_ID, new RetrieveExcusedDays());
            calcs.put(RetrievePresentDays.CAL_ID, new RetrievePresentDays());
            super.addCalcs(calcs);

            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("EX-EXIT-DATE", new ValidateExitDate());
            validators.put("EX-EXIT-CODE", new ValidateExitCode());
            super.addValidators(validators);

            initLastInstrDateMap();
        }
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria X2Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(X2Criteria criteria, String recordSetName) {
        X2Criteria recordSetCriteria = new X2Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + ModelProperty.PATH_DELIMITER
                + RecordSet.COL_NAME, recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Generate the filename for the report.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        fileName.append("_");
        fileName.append(new SimpleDateFormat("MMddyyyy").format(new Date()));
        fileName.append("_");
        fileName.append("001.csv");
        return fileName.toString();
    }

    /**
     * Gets the last in session date.
     *
     * @param date PlainDate
     * @return first in session date in district calendar after passed date.
     */
    private PlainDate getLastInSessionDate(PlainDate date) {
        PlainDate lastInSessionDate = null;
        for (DistrictCalendar day : m_inSessionDates) {
            if (day.getInSessionIndicator() && day.getDate().before(date)) {
                // Initialize by first day from collection if wasn't initialized to this point.
                if (lastInSessionDate == null) {
                    lastInSessionDate = day.getDate();
                }

                if (lastInSessionDate.before(day.getDate())) {
                    lastInSessionDate = day.getDate();
                }
            }
        }
        return lastInSessionDate;
    }

    private void initAttendanceMap(X2Criteria studentCriteria) {
        X2Criteria attCriteria = new X2Criteria();
        SubQuery subQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
        attCriteria.addIn(StudentAttendance.COL_STUDENT_OID, subQuery);

        if (isSchoolContext()) {
            attCriteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            attCriteria.addNotEqualTo(StudentAttendance.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            attCriteria.addNotEqualTo(StudentAttendance.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
        String[] columnKeys =
                {StudentAttendance.COL_SCHOOL_OID, StudentAttendance.COL_STUDENT_OID};

        int[] columnSize = {20, 1000};

        QueryByCriteria byCriteria = new QueryByCriteria(StudentAttendance.class, attCriteria);

        m_attMap = getBroker().getGroupedCollectionByQuery(byCriteria, columnKeys, columnSize);

    }

    /**
     * initialize calendar map. Key - school oid. Value - map where
     * key calendar ID. Value - SisSchoolCalendarDate in session dates for current context
     *
     * @return
     */
    private void initCasMap() {

        Criteria criteria = new X2Criteria();

        if (isSchoolContext()) {
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addNotEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                    SchoolCalendar.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            criteria.addNotEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                    SchoolCalendar.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }


        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        criteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

        QueryByCriteria calendarQuery = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
        String[] columnKeys =
                {SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                        SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID};
        int[] columnSize = {10, 10};
        m_calendarData = getBroker().getGroupedCollectionByQuery(calendarQuery, columnKeys, columnSize);

    }

    /**
     * Initialize in session dates. If export run not in Summer mode, use only Calendars of Schools
     * without
     * Summer School Indicator or where Summer School Indicator is false.
     */
    private void initInSessionDates() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        X2Criteria sklCalDatesCriteria = new X2Criteria();
        sklCalDatesCriteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        sklCalDatesCriteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

        if (StringUtils.isEmpty(m_helperMode) ||
                m_helperMode.equals(DemoExitDataHelper.MODE_PRIMARY_SCHOOL)) {
            sklCalDatesCriteria.addNotEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    m_fieldSummerIndicator, BooleanAsStringConverter.TRUE);
        }
        SubQuery sklCalDatesSub =
                new SubQuery(SisSchoolCalendarDate.class, SisSchoolCalendarDate.COL_DATE, sklCalDatesCriteria);

        criteria.addIn(DistrictCalendar.COL_DATE, sklCalDatesSub);

        QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);
        query.addOrderBy(DistrictCalendar.COL_DATE, true);

        m_inSessionDates = new ArrayList<DistrictCalendar>(getBroker().getCollectionByQuery(query));
    }

    /**
     * Initialize map of Last Instructional Dates for Calendars. Id Export run in Summer mode, join
     * on Schools
     * with Summer School Indicator = true.
     */
    private void initLastInstrDateMap() {
        m_lastInstrDateMap = new HashMap<String, Map<String, PlainDate>>();

        X2Criteria calLastDatesCriteria = new X2Criteria();
        calLastDatesCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        calLastDatesCriteria.addEqualTo(SchoolCalendar.REL_SCHOOL_CALENDAR_DATES + ModelProperty.PATH_DELIMITER +
                SchoolCalendarDate.COL_IN_SESSION_INDICATOR, BooleanAsStringConverter.TRUE);
        String[] columns = {SchoolCalendar.COL_SCHOOL_OID, SchoolCalendar.COL_CALENDAR_ID,
                SchoolCalendar.REL_SCHOOL_CALENDAR_DATES + ModelProperty.PATH_DELIMITER + SchoolCalendarDate.COL_DATE};
        if (!StringUtils.isEmpty(m_helperMode) &&
                m_helperMode.equals(DemoExitDataHelper.MODE_SUMMER_SCHOOL)) {
            calLastDatesCriteria.addEqualTo(SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    m_fieldSummerIndicator, BooleanAsStringConverter.TRUE);
        }
        ReportQueryByCriteria query = new ReportQueryByCriteria(SchoolCalendar.class, columns, calLastDatesCriteria);
        query.addOrderBy(
                SchoolCalendar.REL_SCHOOL_CALENDAR_DATES + ModelProperty.PATH_DELIMITER + SchoolCalendarDate.COL_DATE,
                false);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

        while (iterator.hasNext()) {
            Object[] item = (Object[]) iterator.next();
            String schoolOid = (String) item[0];
            String calendarId = (String) item[1];
            PlainDate date = new PlainDate((Timestamp) item[2]);

            Map<String, PlainDate> calIdDatesMap = m_lastInstrDateMap.get(schoolOid);

            if (calIdDatesMap == null) {
                calIdDatesMap = new HashMap<String, PlainDate>();
                m_lastInstrDateMap.put(schoolOid, calIdDatesMap);
            }
            PlainDate lastDate = calIdDatesMap.get(calendarId);
            if (lastDate == null) {
                calIdDatesMap.put(calendarId, date);
            }
        }
    }

    /**
     * Initialize Summer Schools Oids of Students. Used only in Summer Mode.
     */
    private void initStudentSummerSchoolsMap() {
        m_studentSummerSchoolsMap = new HashMap<String, Collection<String>>();

        X2Criteria summerSchoolCriteria = new X2Criteria();
        summerSchoolCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        String[] columns = {StudentSchool.REL_STUDENT + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                StudentSchool.REL_SCHOOL + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID};
        summerSchoolCriteria.addEqualTo(StudentSchool.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                m_fieldSummerIndicator, BooleanAsStringConverter.TRUE);

        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentSchool.class, columns, summerSchoolCriteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

        while (iterator.hasNext()) {
            Object[] item = (Object[]) iterator.next();
            String stdOid = (String) item[0];
            String stdSklOid = (String) item[1];

            Collection<String> summerSchools = m_studentSummerSchoolsMap.get(stdOid);

            if (summerSchools == null) {
                summerSchools = new ArrayList<String>();
                m_studentSummerSchoolsMap.put(stdOid, summerSchools);
            }
            summerSchools.add(stdSklOid);
        }
    }
}
