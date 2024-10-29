/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Illinois report data source for End of Year report data.
 *
 * @author X2 Development Corporation
 */
public class EndOfYear extends StateReportData {
    /**
     * Entity class for End of Year report data.
     *
     * @author X2 Development Corporation
     */
    public static class EndOfYearEntity extends StateReportEntity {
        /**
         * Class needed to aggregate row data. For each student should be created instances per
         * serving school
         * where School.DOE SCHOOL ID is not blank or null and with FTE for any instructional date
         * >= 50%.
         * Instances of the class are used as containers of data, also it can recalculate itself
         * based on passed date.
         *
         * @author Follett Software Company
         */
        public class RowData {
            /**
             * Students serving school code.
             */
            protected String m_homeSchoolCode;

            /**
             * Students serving school code.
             */
            protected String m_servingSchoolCode;

            /**
             * The total number of days the student was absent at the serving school.
             */
            private double m_absent = 0;

            /**
             * The total number of days the student was in attendance at the serving school.
             */
            private double m_attendance = 0;

            /**
             * Last school date for <code>m_servingSchoolCode</code> and
             * <code>m_student.getCalendarCode()</code>
             */
            private PlainDate m_lastSchoolDate = null;

            /**
             * Indicates if the student was Active in the serving school on the last instructional
             * date in the serving school calendar.
             */
            private double m_memberOnLastDay = 0;

            /**
             * The total number of membership dates for the student in the serving school.
             */
            private double m_membership = 0;

            /**
             * The total number of absences where attExcusedInd = False for the student at the
             * serving school.
             */
            private double m_truant = 0;

            /**
             * Instantiates a new row data.
             *
             * @param primarySchoolId String
             * @param servingSchoolId String
             */
            public RowData(String primarySchoolId, String servingSchoolId) {
                m_homeSchoolCode = primarySchoolId;
                m_servingSchoolCode = servingSchoolId;
                m_lastSchoolDate = ((EndOfYear) getData()).getLastSchoolDate(servingSchoolId, m_student);
            }

            /**
             * Gets the absent.
             *
             * @return Double
             */
            public Double getAbsent() {
                return Double.valueOf(m_absent);
            }

            /**
             * Gets the attendance.
             *
             * @return Double
             */
            public Double getAttendance() {
                return Double.valueOf(m_attendance);
            }

            /**
             * Gets the chronic truant.
             *
             * @return Double
             */
            public Double getChronicTruant() {
                double value = 0;
                if (m_truant >= 9) {
                    value = 1;
                }
                return Double.valueOf(value);
            }

            /**
             * Gets the last day.
             *
             * @return Double
             */
            public Double getLastDay() {
                return Double.valueOf(m_memberOnLastDay);
            }

            /**
             * Gets the membership.
             *
             * @return Double
             */
            public Double getMembership() {
                return Double.valueOf(m_membership);
            }

            /**
             * Gets the truant.
             *
             * @return Double
             */
            public Double getTruant() {
                return Double.valueOf(m_truant);
            }

            /**
             * @see java.lang.Object#equals(java.lang.Object)
             */
            @Override
            public boolean equals(Object obj) {
                if (!(obj instanceof RowData)) {
                    return false;
                }
                if (obj == this) {
                    return true;
                }

                RowData rhs = (RowData) obj;
                return m_servingSchoolCode.equals(rhs.m_servingSchoolCode);
            }

            /**
             * @see java.lang.Object#hashCode()
             */
            @Override
            public int hashCode() {
                return m_servingSchoolCode.hashCode();
            }

            /**
             * Count passed date to recalculate RowData values. Will be recalculated only if passed
             * date is membership date.
             * Return true if recalculated, otherwise return false.
             *
             * @param date PlainDate
             * @param mebershipDatesSize
             * @param withdrawalIsMember
             */
            public void recalculateOnDate(PlainDate date, int mebershipDatesSize, boolean withdrawalIsMember) {
                StudentAttendance attendance = getAttendanceOnDate(date);

                if (attendance != null && attendance.getPortionAbsent() != null) {
                    double portionAbsent = attendance.getPortionAbsent().doubleValue();
                    m_absent += portionAbsent;
                    if (attendance.getPortionAbsent().doubleValue() != 1) {
                        m_attendance += (1 - portionAbsent);
                    }
                    if (!attendance.getExcusedIndicator()) {
                        m_truant += attendance.getPortionAbsent().doubleValue();
                    }
                } else {
                    if (mebershipDatesSize > 1 || withdrawalIsMember) {
                        m_attendance += 1;
                    }
                }
                if (mebershipDatesSize > 1 || withdrawalIsMember) {
                    m_membership += 1;
                }
                if (m_lastSchoolDate.equals(date)) {
                    m_memberOnLastDay = 1;
                }
            }
        }

        protected EndOfYear m_eoyData = null;
        protected SisStudent m_student = null;
        protected StringBuilder m_data = new StringBuilder();

        private ArrayList<RowData> m_rowsData = null;
        private Map<PlainDate, StudentAttendance> m_studentAttendances = null;

        /**
         * Return current RowData.
         *
         * @return Row data
         */
        public RowData getCurrentRowData() {
            return m_rowsData.get(getCurrentRow());
        }

        /**
         * Initialize student, calculate membership and attendance counts.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            m_data = new StringBuilder();

            super.intitialize(data, bean);

            m_eoyData = (EndOfYear) data;
            m_student = (SisStudent) bean;
            m_studentAttendances = m_eoyData.getStudentAttendances(m_student.getOid());

            Collection<StudentScheduleSpan> scheduleSpans = m_eoyData.m_helper.getStudentScheduleSpans(m_student);
            Map<String, TreeSet<PlainDate>> schools = getSchoolsMembDays();

            for (String mySkl : schools.keySet()) {
                m_data.append("School: ");
                m_data.append(mySkl);
                m_data.append(" Count: ");
                m_data.append(schools.get(mySkl).size());
                m_data.append("\n");
            }

            for (StudentScheduleSpan mySpan : scheduleSpans) {
                m_data.append("Schedule School: ");
                m_data.append(mySpan.getSection().getSchedule().getSchool()
                        .getFieldValueByBeanPath(m_eoyData.m_fieldSchoolId));
                m_data.append(" Span: ");
                m_data.append(mySpan.toString());
                m_data.append("\n");
            }

            TreeSet<PlainDate> membershipDates = new TreeSet<PlainDate>();
            // Merge membership days
            for (TreeSet<PlainDate> dates : schools.values()) {
                membershipDates.addAll(dates);
            }

            /*
             * If a student has multiple serving schools for a single date, the student is counted
             * ONLY at the school
             * where the student was served for 50% or more on any given instructional date.
             * So we need determine pairs <PlainDate, School> where PlainDate is InSessionDate and
             * School is
             * school where the student was served for 50%.
             */

            HashMap<String, RowData> rowsDataMap = new HashMap<String, RowData>();

            /*
             * Utilize the FTE Calculation from the Student Demographics and Enrollment export to
             * determine FTE on a
             * Daily Basis - to determine if the student qualifies and what Serving School they
             * should be reported from.
             */

            // Try to cache calculations.
            RowData previousRowData = null;
            Collection<StudentScheduleSpan> scheduleSpansOnPreviousDate = null;

            for (PlainDate date : membershipDates) {
                // All needed data was prepared to this point to calculate FTE
                String servingSchoolId = null;
                String primarySchoolId = null;

                StudentEnrollment primaryEnrollment =
                        m_eoyData.m_helper.getEnrollmentForDate(m_student.getOid(), date, "E");
                if (primaryEnrollment == null) {
                    continue;
                }
                School primarySchool = primaryEnrollment.getSchool();
                primarySchoolId = (String) primarySchool.getFieldValueByBeanPath(m_eoyData.m_fieldSchoolId);
                Map<String, Float> schoolsPeriods = new HashMap<String, Float>();
                Collection<StudentScheduleSpan> scheduleSpansOnDate = getScheduleSpansOnDate(scheduleSpans, date);
                // Try to cache calculations.
                if (scheduleSpansOnPreviousDate != null && scheduleSpansOnPreviousDate.equals(scheduleSpansOnDate)) {
                    previousRowData.recalculateOnDate(date, membershipDates.size(), m_eoyData.m_withdrawalIsMember);
                    continue;
                }
                for (String school : schools.keySet()) {
                    // Count only if it is membership date for the school.
                    if (schools.get(school).contains(date)) {
                        schoolsPeriods.put(school, Float.valueOf(0));
                    }
                }

                /*
                 * Calculate FTE based on SUM of the student's number of class periods by schools.
                 * o The total number of class periods for the student where crsCredit not null and
                 * > 0 becomes the denominator in the equation.
                 * o The numerator is the total number of periods for credit earning classes from
                 * school.
                 */
                for (StudentScheduleSpan span : scheduleSpansOnDate) {
                    School spanSchool = span.getSection().getSchedule().getSchool();
                    String schoolId = (String) spanSchool.getFieldValueByBeanPath(m_eoyData.m_fieldSchoolId);

                    // Count span only if membership dates of the school contain current date
                    if (schoolsPeriods.keySet().contains(schoolId)) {
                        BigDecimal credit = span.getSection().getSchoolCourse().getCourse().getCredit();

                        if (credit != null && credit.compareTo(new BigDecimal(0)) > 0) {
                            /*
                             * Get number of periods for this class per day.
                             */
                            Integer numOfPeriods = m_eoyData.m_sectionNumOfPeriodsMap.get(span.getSection().getOid());

                            if (numOfPeriods == null) {
                                numOfPeriods = Integer.valueOf(0);
                            }

                            Integer numOfDays = m_eoyData.m_sectionNumOfSheduleDays.get(span.getSection().getOid());

                            float periodsPerScheduleDay = numOfPeriods.floatValue() / numOfDays.floatValue();

                            float totalSchoolPeriodsNum = schoolsPeriods.get(schoolId).floatValue();
                            schoolsPeriods.put(schoolId, Float.valueOf(totalSchoolPeriodsNum + periodsPerScheduleDay));
                        }
                    }
                }

                float totalPeriodsNum = 0F;
                for (Float periodNumbers : schoolsPeriods.values()) {
                    totalPeriodsNum += periodNumbers.floatValue();
                }

                if (totalPeriodsNum != 0) {
                    String schoolWithGreatestFTE = null;
                    float greatestFTE = 0;

                    for (Entry<String, Float> schoolPeriods : schoolsPeriods.entrySet()) {
                        String schoolId = schoolPeriods.getKey();
                        float currentFte = schoolPeriods.getValue().floatValue() / totalPeriodsNum * 100;

                        if (currentFte > greatestFTE) {
                            greatestFTE = currentFte;
                            schoolWithGreatestFTE = schoolId;
                        }
                        /*
                         * If current FTE > 50, use this school.
                         */
                        if (currentFte > 50) {
                            servingSchoolId = schoolId;
                            break;
                        } else if (currentFte == 50 && servingSchoolId == null) {
                            servingSchoolId = schoolId;
                        }
                        /*
                         * Student attendance, membership and absence data must be determined on a
                         * "daily" basis
                         * and a student cannot be split between two schools.
                         * In the event that a student has an equal 50/50 split between two serving
                         * schools where
                         * [DOE SCHOOL ID] is not null or blank, count the student at their
                         * "Homeschool".
                         */
                        else if (currentFte == 50 && servingSchoolId != null) {
                            servingSchoolId = primarySchoolId;
                        }
                    }

                    if (servingSchoolId == null) {
                        servingSchoolId = schoolWithGreatestFTE;
                    }
                }
                // Even if total periods num is 0 but there is only one Serving School - use it.
                else if (schoolsPeriods.size() == 1) {
                    servingSchoolId = schoolsPeriods.entrySet().iterator().next().getKey();
                }
                // In other cases recalculate for Primary school
                else if (schoolsPeriods.size() > 1) {
                    servingSchoolId = primarySchoolId;
                }

                if (servingSchoolId != null) {
                    RowData rowData = rowsDataMap.get(servingSchoolId);
                    if (rowData == null) {
                        rowData = new RowData(primarySchoolId, servingSchoolId);
                        rowsDataMap.put(servingSchoolId, rowData);
                    }
                    rowData.recalculateOnDate(date, membershipDates.size(), m_eoyData.m_withdrawalIsMember);

                    previousRowData = rowData;
                    scheduleSpansOnPreviousDate = scheduleSpansOnDate;
                } else {
                    previousRowData = null;
                    scheduleSpansOnPreviousDate = null;
                }
            }

            m_rowsData = new ArrayList<RowData>(rowsDataMap.values());

            setRowCount(m_rowsData.size());
        }

        /**
         * Return student's attendance on date.
         *
         * @param date PlainDate
         * @return StudentAttendance
         */
        protected StudentAttendance getAttendanceOnDate(PlainDate date) {
            return m_studentAttendances.get(date);
        }

        /**
         * Add membership days for <code>schoolId</code> to passed <code>membDates</code> that
         * between or on
         * <code>firstDate</code> and <code>lastDate</code>.
         *
         * @param schoolId String
         * @param firstDate PlainDate
         * @param lastDate PlainDate
         * @param membDates TreeSet<PlainDate>
         */
        private void addMembDays(String schoolId,
                                 PlainDate firstDate,
                                 PlainDate lastDate,
                                 TreeSet<PlainDate> membDates) {
            Map<String, ArrayList<PlainDate>> datesBySchoolCalendarIds = m_eoyData.getSchoolCalendarDates(schoolId);
            if (datesBySchoolCalendarIds != null) {
                ArrayList<PlainDate> dates = null;
                String studentCalendarId = m_student.getCalendarCode();
                if (!StringUtils.isEmpty(studentCalendarId) &&
                        datesBySchoolCalendarIds.containsKey(studentCalendarId)) {
                    dates = datesBySchoolCalendarIds.get(studentCalendarId);
                }
                // Otherwise use any calendar.
                else {
                    dates = datesBySchoolCalendarIds.entrySet().iterator().next().getValue();
                }

                for (PlainDate date : dates) {
                    if (!date.before(firstDate) &&
                            (lastDate == null || !date.after(lastDate))) {
                        membDates.add(date);
                    }
                }
            }
        }

        /**
         * Return map of serving schools ids with membership days for student.
         *
         * @return Collection<School>
         */
        private Map<String, TreeSet<PlainDate>> getSchoolsMembDays() {
            Map<String, TreeSet<PlainDate>> schoolMembDaysMap = new HashMap<String, TreeSet<PlainDate>>();

            /*
             * Determine membership dates in primary schools
             */
            Collection<StudentEnrollmentSpan> enrSpans = m_eoyData.m_helper.getStudentEnrollmentSpans(m_student, true);
            for (StudentEnrollmentSpan enrSpan : enrSpans) {
                String schoolId = (String) enrSpan.getSchool().getFieldValueByBeanPath(m_eoyData.m_fieldSchoolId);
                String excludeSchoolInd =
                        (String) enrSpan.getSchool().getFieldValueByBeanPath(m_eoyData.m_excludeSklField);
                if (StringUtils.isEmpty(schoolId) ||
                        (((EndOfYear) getData()).getSummerSchoolOids().contains(enrSpan.getSchool().getOid())) ||
                        (!StringUtils.isEmpty(excludeSchoolInd)
                                && BooleanAsStringConverter.TRUE.equals(excludeSchoolInd))
                        || (m_eoyData.isSchoolContext()
                                && !m_eoyData.getSchool().getOid().equals(enrSpan.getSchool().getOid()))) {
                    continue;
                }
                PlainDate firstDate = enrSpan.getFirstActiveDate();
                PlainDate lastDate = enrSpan.getLastActiveDate();

                TreeSet<PlainDate> membDates = schoolMembDaysMap.get(schoolId);
                if (membDates == null) {
                    membDates = new TreeSet<PlainDate>();
                    schoolMembDaysMap.put(schoolId, membDates);
                }
                addMembDays(schoolId, firstDate, lastDate, membDates);
            }
            /*
             * Determine membership dates in secondary schools
             */
            Map<String, Map<PlainDate, PlainDate>> schoolDatesMap =
                    m_eoyData.m_studentSecondDatesMap.get(m_student.getOid());
            if (schoolDatesMap != null) {
                for (Entry<String, Map<PlainDate, PlainDate>> schoolDates : schoolDatesMap.entrySet()) {
                    String schoolId = schoolDates.getKey();
                    TreeSet<PlainDate> membDates = schoolMembDaysMap.get(schoolId);
                    if (membDates == null) {
                        membDates = new TreeSet<PlainDate>();
                        schoolMembDaysMap.put(schoolId, membDates);
                    }
                    for (Entry<PlainDate, PlainDate> datesPairs : schoolDates.getValue().entrySet()) {
                        addMembDays(schoolId, datesPairs.getKey(), datesPairs.getValue(), membDates);
                    }
                }
            }
            return schoolMembDaysMap;
        }

        /**
         * Return collection of StudentScheduleSpan for student on passed date.
         *
         * @param spans Collection<StudentScheduleSpan>
         * @param date PlainDate
         * @return Collection<StudentScheduleSpan>
         */
        private Collection<StudentScheduleSpan> getScheduleSpansOnDate(Collection<StudentScheduleSpan> spans,
                                                                       PlainDate date) {
            ArrayList<StudentScheduleSpan> schedSpansOnDate = new ArrayList<StudentScheduleSpan>();
            for (StudentScheduleSpan span : spans) {
                if (span != null && date != null && span.getEntryDate() != null && !span.getEntryDate().after(date)
                        && span.getExitDate() != null &&
                        !span.getExitDate().before(date)) {
                    schedSpansOnDate.add(span);
                }
            }
            return schedSpansOnDate;
        }
    }

    /**
     * Retrieve an indicator of whether the student was in membership at the end of school.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDebug implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            EndOfYearEntity eoyEntity = (EndOfYearEntity) entity;
            return eoyEntity.m_data.toString();
        }
    }

    /**
     * Retrieve an indicator of whether the student was in membership at the end of school.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEOY implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            EndOfYearEntity eoyEntity = (EndOfYearEntity) entity;
            return eoyEntity.getCurrentRowData().getLastDay();
        }
    }

    /**
     * Retrieve an indicator of whether the student was in membership at the end of school.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGrade implements FieldRetriever {
        private Map<String, StudentContextAttributes> m_contextAttributesMap = null;

        /**
         * Instantiates a new retrieve grade.
         *
         * @param studentCriteria X2Criteria
         */
        public RetrieveGrade(X2Criteria studentCriteria) {
            X2Criteria crit = new X2Criteria();
            crit.addEqualTo(StudentContextAttributes.COL_CONTEXT_OID, getCurrentContext().getOid());
            crit.addIn(StudentContextAttributes.COL_STUDENT_OID,
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria));
            m_contextAttributesMap =
                    getBroker().getMapByQuery(new QueryByCriteria(StudentContextAttributes.class, crit),
                            StudentContextAttributes.COL_STUDENT_OID, 2048);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            StudentContextAttributes attr = m_contextAttributesMap.get(student.getOid());
            return attr == null ? student.getGradeLevel() : attr.getGradeLevel();
        }
    }

    /**
     * Retrieve a calculated membership value from the entity.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveMembership implements FieldRetriever {
        private final String PARAM_ABSENT = "ABSENT";
        private final String PARAM_ATTENDANCE = "ATTENDANCE";
        private final String PARAM_CHRONIC_TRUANT = "TRUANT_IND";
        private final String PARAM_MEMBERSHIP = "MEMBERSHIP";
        private final String PARAM_TRUANT = "TRUANT";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            EndOfYearEntity srcEntity = (EndOfYearEntity) entity;

            String fieldParameter = (String) field.getParameter();

            Object value = null;

            switch (fieldParameter) {
                case PARAM_ABSENT:
                    value = srcEntity.getCurrentRowData().getAbsent();
                    break;
                case PARAM_ATTENDANCE:
                    value = srcEntity.getCurrentRowData().getAttendance();
                    break;
                case PARAM_CHRONIC_TRUANT:
                    value = srcEntity.getCurrentRowData().getChronicTruant();
                    break;
                case PARAM_MEMBERSHIP:
                    value = srcEntity.getCurrentRowData().getMembership();
                    break;
                case PARAM_TRUANT:
                    value = srcEntity.getCurrentRowData().getTruant();
                    break;

                default:
                    break;
            }

            return value;
        }
    }

    /**
     * Retrieve the RCDTS of the student's home/serving school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS
     * - "S" for the serving school's RCDTS.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRcdts implements FieldRetriever {
        public final static String PARAM_HOME_SCHOOL = "H";
        public final static String PARAM_SERVING_SCHOOL = "S";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String rcdts = null;
            if (PARAM_HOME_SCHOOL.equals(field.getParameter())) {
                rcdts = ((EndOfYearEntity) entity).getCurrentRowData().m_homeSchoolCode;
            } else if (PARAM_SERVING_SCHOOL.equals(field.getParameter())) {
                rcdts = ((EndOfYearEntity) entity).getCurrentRowData().m_servingSchoolCode;
            }

            return rcdts;
        }
    }

    /**
     * Constants: aliases, codes, fields, parameters
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SCHOOL_SERVICE = "DOE SCHOOL SERVICE";
    protected static final String CALENDAR_ANY = "*";
    protected static final String CALENDAR_DEFAULT_ID = "Standard";
    protected static final String PARAM_SCHEDULE_END_DATE = "endDate";
    protected static final String PARAM_SCHEDULE_START_DATE = "startDate";
    protected static final String PARAM_SCHOOL_YEAR_CONTEXT = "schoolYearContext";

    /**
     * Instance variables.
     */
    protected String m_districtId;
    protected Map<String, List<StudentProgramParticipation>> m_ellParticipationMap;
    protected String m_excludeSklField;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolId;
    protected String m_fieldSchoolServe;
    protected List<String> m_graduationCodes;
    protected StudentHistoryHelper m_helper;
    protected Map<String, Map<String, PlainDate>> m_lastDatesMap;
    protected Map<String, String> m_mostCommonCalendarCode;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected Map<String, PlainDate> m_schoolEndDate;
    protected Map<String, Integer> m_sectionNumOfPeriodsMap;
    protected Map<String, Integer> m_sectionNumOfSheduleDays;
    protected Map<String, Map<String, ArrayList<PlainDate>>> m_sklCalDatesMap;
    protected Map<String, Map<PlainDate, StudentAttendance>> m_studentAttendanceMap;
    protected Map<String, Map<String, Map<PlainDate, PlainDate>>> m_studentSecondDatesMap;
    protected Set<String> m_summerSchoolOids;
    protected boolean m_withdrawalIsMember;

    /**
     * Initialize the export.
     * Prepare the StudentHistoryHelper and retrievers.
     */
    @Override
    public void initialize() {
        String contextOid = (String) getParameter(PARAM_SCHOOL_YEAR_CONTEXT);
        if (contextOid != null) {
            DistrictSchoolYearContext context =
                    (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class, contextOid);
            if (context != null) {
                this.setCurrentContext(context);
            }
        }
        initializeFields();

        populateSklCalDatesMap();

        // Prepare the StudentHistoryHelper.
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getCurrentContext().getEndDate());

        initializeNumOfPeriods();
        initializeNumOfScheduleDays();
        initSecondarySchoolsDatesMap();

        // Prepare the StateReportData.
        setQuery(m_helper.getStudentQuery(false));
        setEntityClass(EndOfYearEntity.class);

        // Build a map of calculations/retrievers.
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("EOY-MEMB", new RetrieveMembership());
        calcs.put("EOY-EOY", new RetrieveEOY());
        calcs.put("EOY-SCHOOL", new RetrieveRcdts());
        calcs.put("EOY-GRADE", new RetrieveGrade(m_helper.getStudentCriteria()));
        calcs.put("EOY-DEBUG", new RetrieveDebug());
        super.addCalcs(calcs);
    }

    /**
     * @param servingSchoolId
     * @param m_student
     * @return
     */
    protected PlainDate getLastSchoolDate(String servingSchoolId, SisStudent m_student) {
        PlainDate lastDate = null;
        Map<String, PlainDate> calLastDateMap = m_lastDatesMap.get(servingSchoolId);
        if (calLastDateMap != null) {
            lastDate = calLastDateMap.get(m_student.getCalendarCode());
            if (lastDate == null) {
                String calendarCode = getMostCommonCalendar(servingSchoolId);
                lastDate = calLastDateMap.get(calendarCode);
                if (lastDate == null && !calLastDateMap.values().isEmpty()) {
                    lastDate = calLastDateMap.values().iterator().next();
                }
            }
        }
        return lastDate;
    }

    /**
     * Find the last day of school for the specified school. Check the school current year calendars
     *
     * @param schoolId String
     * @param calendarId String
     * @param start boolean
     * @return PlainDate
     */
    protected PlainDate getSchoolStartOrEndDate(String schoolId, String calendarId, boolean start) {
        PlainDate endDate = null;
        Map<String, ArrayList<PlainDate>> calDatesMap = null;
        List<PlainDate> dates = null;
        if ((calDatesMap = m_sklCalDatesMap.get(schoolId)) != null) {
            if (calendarId != null) {
                if ((dates = calDatesMap.get(calendarId)) != null) {
                    if (start) {
                        endDate = dates.get(0);
                    } else {
                        endDate = dates.get(dates.size() - 1);
                    }
                }
            } else {
                dates = calDatesMap.values().iterator().next();
                if ((dates != null)) {
                    if (start) {
                        endDate = dates.get(0);
                    } else {
                        endDate = dates.get(dates.size() - 1);
                    }
                }
            }
        }

        return endDate;
    }

    /**
     * Gets the summer school oids. A summer school is a school that does not have a scheduled term
     * between the start date specified in the input definition and the end date.
     *
     * @return Sets the
     */
    protected Set<String> getSummerSchoolOids() {
        if (m_summerSchoolOids == null) {
            m_summerSchoolOids = new HashSet();

            PlainDate startDate = (PlainDate) getParameter(PARAM_SCHEDULE_START_DATE);
            PlainDate endDate = (PlainDate) getParameter(PARAM_SCHEDULE_END_DATE);
            if (startDate != null && endDate == null) {
                endDate = startDate;
            }
            if (endDate != null && startDate == null) {
                startDate = endDate;
            }
            if (startDate != null && endDate != null) {
                X2Criteria criteria = new X2Criteria();

                // Add all schools that are not excluded
                criteria.addNotEqualTo(m_excludeSklField, Boolean.TRUE);
                String[] columns = new String[] {X2BaseBean.COL_OID};
                ColumnQuery query = new ColumnQuery(SisSchool.class, columns, criteria);
                ReportQueryIterator queryItr = getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (queryItr.hasNext()) {
                        Object[] row = (Object[]) queryItr.next();
                        String oid = (String) row[0];
                        m_summerSchoolOids.add(oid);
                    }
                } finally {
                    queryItr.close();
                }

                // Remove schools that are scheduled
                criteria = new X2Criteria();
                // Overlaps input date range
                criteria.addGreaterOrEqualThan(ScheduleTermDate.COL_END_DATE, startDate);
                criteria.addLessOrEqualThan(ScheduleTermDate.COL_START_DATE, endDate);

                // From active Schedule for the selected year.
                criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                        ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                        SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                        getCurrentContext().getOid());

                columns = new String[] {ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER
                        + ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID};
                query = new ColumnQuery(ScheduleTermDate.class, columns, criteria);
                queryItr = getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (queryItr.hasNext()) {
                        Object[] row = (Object[]) queryItr.next();
                        String oid = (String) row[0];
                        m_summerSchoolOids.remove(oid);
                    }
                } finally {
                    queryItr.close();
                }
            }
        }
        return m_summerSchoolOids;
    }

    /**
     * Return by passed schoolId inSession days for the school keyed by school calendar id.
     *
     * @param schoolId String
     * @return ArrayList<SchoolCalendarDate>
     */
    protected Map<String, ArrayList<PlainDate>> getSchoolCalendarDates(String schoolId) {
        return m_sklCalDatesMap.get(schoolId);
    }

    /**
     * Map of student attendances keyed by date.
     *
     * @param studentOid String
     * @return Map<PlainDate, StudentAttendance>
     */
    protected Map<PlainDate, StudentAttendance> getStudentAttendances(String studentOid) {
        return m_studentAttendanceMap.get(studentOid) != null ? m_studentAttendanceMap.get(studentOid)
                : new HashMap<PlainDate, StudentAttendance>();
    }

    /**
     * Find the most common calendar for this school based on the calendar codes of currently active
     * students
     *
     * @param schoolId
     * @return
     */
    private String getMostCommonCalendar(String schoolId) {
        if (m_mostCommonCalendarCode == null) {
            m_mostCommonCalendarCode = new HashMap();
            X2Criteria criteria = new X2Criteria();
            criteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

            String[] columns = new String[] {SisStudent.REL_SCHOOL + PATH_DELIMITER + m_fieldSchoolId,
                    SisStudent.COL_CALENDAR_CODE, "count(*)"};

            ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
            query.addGroupBy(columns[0]);
            query.addGroupBy(columns[1]);
            query.addOrderByDescending("count(*)");

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    String id = (String) row[0];
                    String calendarCode = (String) row[1];

                    if (!m_mostCommonCalendarCode.containsKey(id)) {
                        m_mostCommonCalendarCode.put(id, calendarCode);
                    }
                }
            } finally {
                iterator.close();
            }
        }

        return m_mostCommonCalendarCode.get(schoolId);
    }

    /**
     * @param object
     * @return
     */
    private PlainDate getSqlDate(Object object) {
        PlainDate date = null;
        if (object != null) {
            if (object instanceof Timestamp) {
                date = new PlainDate(((Timestamp) object).getTime());
            } else {
                date = new PlainDate((java.sql.Date) object);
            }
        }
        return date;
    }

    /**
     * Initialize variables and fields for the export.
     */
    private void initializeFields() {
        m_graduationCodes = new ArrayList<String>(2);
        m_graduationCodes.add("P00");
        m_graduationCodes.add("Q00");

        m_schoolEndDate = new HashMap<String, PlainDate>();

        m_fieldSchoolId = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldSchoolServe = translateAliasToJavaName(ALIAS_SCHOOL_SERVICE, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_withdrawalIsMember = Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

        // Find a sample District ID (RCDTS places 1-9) to compare with serving school to make sure
        // it is in district.
        m_districtId = null;
        Collection<School> schools = getOrganization().getSchools();
        for (School school : schools) {
            String schoolId = (String) school.getFieldValueByBeanPath(m_fieldSchoolId);
            if (!StringUtils.isEmpty(schoolId)) {
                m_districtId = schoolId.substring(0, 9);
                break;
            }
        }
        if (m_districtId == null) {
            m_districtId = (String) getOrganization().getFieldValueByAlias(ALIAS_DISTRICT_ID);
            if (m_districtId != null && m_districtId.length() > 9) {
                m_districtId = m_districtId.substring(0, 9);
            }
        }

        // Load student attendance, all schools.
        X2Criteria studentAttendanceCriteria = new X2Criteria();
        studentAttendanceCriteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);
        studentAttendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, getCurrentContext().getStartDate());
        studentAttendanceCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, getCurrentContext().getEndDate());

        applyInputCriteria(studentAttendanceCriteria, false, StudentAttendance.REL_STUDENT);

        QueryByCriteria studentAttendanceQuery =
                new QueryByCriteria(StudentAttendance.class, studentAttendanceCriteria);
        studentAttendanceQuery.addOrderBy(StudentAttendance.COL_STUDENT_OID, true);
        studentAttendanceQuery.addOrderBy(StudentAttendance.COL_DATE, false);

        m_studentAttendanceMap = getBroker().getNestedMapByQuery(studentAttendanceQuery,
                StudentAttendance.COL_STUDENT_OID, StudentAttendance.COL_DATE,
                50, 1);
    }

    /**
     * Initialize number of periods for sections.
     */
    private void initializeNumOfPeriods() {
        if (m_sectionNumOfPeriodsMap == null) {
            m_sectionNumOfPeriodsMap = new HashMap<String, Integer>();

            X2Criteria mstCriteria = new X2Criteria();
            X2Criteria mstOrCriteria = new X2Criteria();

            // Student Schedule sections
            X2Criteria criteria = m_helper.getStudentScheduleCriteria();
            SubQuery subQuery = new SubQuery(StudentSchedule.class,
                    StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, criteria);
            mstCriteria.addIn(X2BaseBean.COL_OID, subQuery);

            // Student Schedule sections
            criteria = m_helper.getStudentScheduleChangeCriteria();
            subQuery = new SubQuery(StudentScheduleChange.class,
                    StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                    criteria);
            mstOrCriteria.addIn(X2BaseBean.COL_OID, subQuery);
            mstCriteria.addOrCriteria(mstOrCriteria);

            String[] columns = {X2BaseBean.COL_OID, MasterSchedule.REL_MASTER_TERMS + ModelProperty.PATH_DELIMITER +
                    MasterTerm.REL_MASTER_MATRICES + ModelProperty.PATH_DELIMITER +
                    MasterScheduleMatrix.REL_SCHEDULE_MATRIX + ModelProperty.PATH_DELIMITER +
                    ScheduleMatrix.REL_SCHEDULE_PERIOD + ModelProperty.PATH_DELIMITER +
                    X2BaseBean.COL_OID};

            ReportQueryByCriteria query = new ReportQueryByCriteria(MasterSchedule.class, columns, mstCriteria, true);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    Object[] item = (Object[]) iterator.next();

                    String sectionOid = (String) item[0];

                    Integer curNumOfPeriods = m_sectionNumOfPeriodsMap.get(sectionOid);
                    if (curNumOfPeriods == null) {
                        curNumOfPeriods = Integer.valueOf(0);
                    }
                    int newNumOfPeriods = curNumOfPeriods.intValue() + 1;
                    m_sectionNumOfPeriodsMap.put(sectionOid, Integer.valueOf(newNumOfPeriods));
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Initialize number of schedule days for sections.
     */
    private void initializeNumOfScheduleDays() {
        if (m_sectionNumOfSheduleDays == null) {
            m_sectionNumOfSheduleDays = new HashMap<String, Integer>();

            X2Criteria ssCriteria = m_helper.getStudentScheduleCriteria();
            SubQuery ssSubQuery =
                    new SubQuery(StudentSchedule.class, StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                            X2BaseBean.COL_OID, ssCriteria);

            X2Criteria sscCriteria = m_helper.getStudentScheduleChangeCriteria();
            SubQuery sscSubQuery = new SubQuery(StudentScheduleChange.class, StudentScheduleChange.REL_MASTER_SCHEDULE +
                    ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, sscCriteria);

            X2Criteria andCriteria = new X2Criteria();
            X2Criteria orCriteria = new X2Criteria();

            andCriteria.addIn(X2BaseBean.COL_OID, ssSubQuery);
            orCriteria.addIn(X2BaseBean.COL_OID, sscSubQuery);
            andCriteria.addOrCriteria(orCriteria);

            X2Criteria mstCriteria = new X2Criteria();
            mstCriteria.addAndCriteria(andCriteria);

            String[] columns = {X2BaseBean.COL_OID, MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    Schedule.REL_SCHEDULE_DAYS + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID};

            ReportQueryByCriteria query = new ReportQueryByCriteria(MasterSchedule.class, columns, mstCriteria, true);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    Object[] item = (Object[]) iterator.next();

                    String sectionOid = (String) item[0];

                    Integer curNumOfScheduleDays = m_sectionNumOfSheduleDays.get(sectionOid);
                    if (curNumOfScheduleDays == null) {
                        curNumOfScheduleDays = Integer.valueOf(0);
                    }
                    int newNumOfScheduleDays = curNumOfScheduleDays.intValue() + 1;
                    m_sectionNumOfSheduleDays.put(sectionOid, Integer.valueOf(newNumOfScheduleDays));
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Initialize secondary schools dates map. First key is studentOid, second key is schoolId,
     * dates are start date and end date of the secondary school.
     */
    private void initSecondarySchoolsDatesMap() {
        m_studentSecondDatesMap = new HashMap<String, Map<String, Map<PlainDate, PlainDate>>>();

        X2Criteria secondaryDatesCriteria = new X2Criteria();
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
        secondaryDatesCriteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubQuery);
        secondaryDatesCriteria.addNotEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.FORMER));
        secondaryDatesCriteria.addLessOrEqualThan(StudentSchool.COL_START_DATE, getCurrentContext().getEndDate());
        X2Criteria andCriteria = new X2Criteria();
        andCriteria.addGreaterOrEqualThan(StudentSchool.COL_END_DATE, getCurrentContext().getStartDate());
        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addEmpty(StudentSchool.COL_END_DATE, getBroker().getPersistenceKey());
        andCriteria.addOrCriteria(orCriteria);
        secondaryDatesCriteria.addAndCriteria(andCriteria);

        secondaryDatesCriteria.addNotEmpty(StudentSchool.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                m_fieldSchoolId, getBroker().getPersistenceKey());
        secondaryDatesCriteria.addNotIn(StudentSchool.REL_SCHOOL + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                getSummerSchoolOids());
        secondaryDatesCriteria.addNotEqualTo(StudentSchool.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                m_excludeSklField, BooleanAsStringConverter.TRUE);

        secondaryDatesCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        String[] columns = {StudentSchool.REL_STUDENT + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                StudentSchool.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSchoolId,
                StudentSchool.COL_START_DATE,
                StudentSchool.COL_END_DATE};

        ReportQueryByCriteria reportQuery =
                new ReportQueryByCriteria(StudentSchool.class, columns, secondaryDatesCriteria);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(reportQuery);
        try {
            while (iterator.hasNext()) {
                Object[] items = (Object[]) iterator.next();
                String studentOid = (String) items[0];
                String schoolId = (String) items[1];
                PlainDate startDate = getSqlDate(items[2]);
                PlainDate endDate = getSqlDate(items[3]);

                Map<String, Map<PlainDate, PlainDate>> schoolIdDatesMap = m_studentSecondDatesMap.get(studentOid);
                if (schoolIdDatesMap == null) {
                    schoolIdDatesMap = new HashMap<String, Map<PlainDate, PlainDate>>();
                    m_studentSecondDatesMap.put(studentOid, schoolIdDatesMap);
                }
                Map<PlainDate, PlainDate> datesMap = schoolIdDatesMap.get(schoolId);
                if (datesMap == null) {
                    datesMap = new HashMap<PlainDate, PlainDate>();
                    schoolIdDatesMap.put(schoolId, datesMap);
                }
                datesMap.put(startDate, endDate);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Populate <code>m_sklCalDatesMap</code> where values are collection of
     * <code>SchoolCalendarDate</code>,
     * outer key is <code>schoolId</code> and inner key is <code>calendarId</code>.
     * Call <code>populateLastDatesMap()</code> at the end of the function.
     */
    private void populateSklCalDatesMap() {
        m_sklCalDatesMap = new HashMap<String, Map<String, ArrayList<PlainDate>>>();

        X2Criteria criteria = new X2Criteria();

        if (isSchoolContext()) {
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        }
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());
        criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
        criteria.addNotIn(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                X2BaseBean.COL_OID, getSummerSchoolOids());
        criteria.addNotEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                m_excludeSklField, Boolean.TRUE);
        criteria.addNotEmpty(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                m_fieldSchoolId, getBroker().getPersistenceKey());

        String[] columns = new String[] {SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSchoolId,
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                        SchoolCalendar.COL_CALENDAR_ID,
                SchoolCalendarDate.COL_DATE};

        ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(SchoolCalendarDate.class, columns, criteria);
        reportQuery.addOrderBy(SchoolCalendarDate.COL_DATE, true);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(reportQuery);

        try {
            while (iterator.hasNext()) {
                Object[] items = (Object[]) iterator.next();
                String schoolId = (String) items[0];
                String calendarId = (String) items[1];
                PlainDate date = getSqlDate(items[2]);

                Map<String, ArrayList<PlainDate>> calendarIdDates = m_sklCalDatesMap.get(schoolId);
                if (calendarIdDates == null) {
                    calendarIdDates = new HashMap<String, ArrayList<PlainDate>>();
                    m_sklCalDatesMap.put(schoolId, calendarIdDates);
                }
                ArrayList<PlainDate> dates = calendarIdDates.get(calendarId);
                if (dates == null) {
                    dates = new ArrayList<PlainDate>();
                    calendarIdDates.put(calendarId, dates);
                }
                dates.add(date);
            }
        } finally {
            iterator.close();
        }

        populateLastDatesMap();
    }

    /**
     * Populate <code>m_lastDatesMap</code> where values are last <code>SchoolCalendarDate</code>,
     * outer key is <code>schoolId</code> and inner key is <code>calendarId</code>.
     */
    private void populateLastDatesMap() {
        m_lastDatesMap = new HashMap<String, Map<String, PlainDate>>();

        for (Entry<String, Map<String, ArrayList<PlainDate>>> outerEntry : m_sklCalDatesMap.entrySet()) {
            String schoolId = outerEntry.getKey();

            for (Entry<String, ArrayList<PlainDate>> innerEntry : outerEntry.getValue().entrySet()) {
                Map<String, PlainDate> calDateMap = m_lastDatesMap.get(schoolId);
                if (calDateMap == null) {
                    calDateMap = new HashMap<String, PlainDate>();
                    m_lastDatesMap.put(schoolId, calDateMap);
                }

                String calendarId = innerEntry.getKey();

                ArrayList<PlainDate> dates = innerEntry.getValue();
                if (dates != null && !dates.isEmpty()) {
                    PlainDate lastDate = dates.get(dates.size() - 1);
                    calDateMap.put(calendarId, lastDate);
                }
            }
        }
    }
}
