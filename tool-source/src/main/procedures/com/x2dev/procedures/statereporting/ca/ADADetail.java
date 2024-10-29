/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ca;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Export procedure for Average Daily Attendance File.
 *
 * @author X2 Development Corporation
 */
public class ADADetail extends StateReportData {

    /**
     * Entity class for Student Absence export.
     *
     */
    public static class ADADetailEntity extends StateReportEntity {

        /**
         * The Class SchoolFundGradeCombination.
         */
        protected class SchoolFundGradeCombination {
            Map<PlainDate, SchoolCalendarDate> m_calDates;
            SchoolCalendar m_calendar;
            String m_fund;
            String m_gradeLevel;
            SisSchool m_school;
            List<StudentEnrollmentSpan> m_stdEnrlSpans;

            /**
             * Instantiates a new school fund grade combination.
             *
             * @param school SisSchool
             * @param fund String
             * @param gradeLevel String
             */
            public SchoolFundGradeCombination(SisSchool school, String fund, String gradeLevel) {
                m_school = school;
                m_fund = fund;
                m_gradeLevel = gradeLevel;
            }

            /**
             * Adds the std enrl span.
             *
             * @param stdEnrlSpan StudentEnrollmentSpan
             */
            public void addStdEnrlSpan(StudentEnrollmentSpan stdEnrlSpan) {
                if (m_stdEnrlSpans == null) {
                    m_stdEnrlSpans = new LinkedList<StudentEnrollmentSpan>();
                }
                m_stdEnrlSpans.add(stdEnrlSpan);
            }

            /**
             * Equals.
             *
             * @param obj Object
             * @return true, if successful
             * @see java.lang.Object#equals(java.lang.Object)
             */
            @Override
            public boolean equals(Object obj) {

                boolean value = false;

                if (obj instanceof SchoolFundGradeCombination) {
                    if (m_school.equals(((SchoolFundGradeCombination) obj).getSchool()) &&
                            m_fund.equals(((SchoolFundGradeCombination) obj).getFund()) &&
                            m_gradeLevel.equals(((SchoolFundGradeCombination) obj).getGradeLevel())) {
                        value = true;
                    }
                }
                return value;

            }

            /**
             * Gets the calendar.
             *
             * @return the m_calendar
             */
            public SchoolCalendar getCalendar() {
                return m_calendar;
            }

            /**
             * Gets the calendar dates.
             *
             * @return the m_calDates
             */
            public Map<PlainDate, SchoolCalendarDate> getCalendarDates() {
                return m_calDates;
            }

            /**
             * Gets the fund.
             *
             * @return the m_fund
             */
            public String getFund() {
                return m_fund;
            }

            /**
             * Gets the grade level.
             *
             * @return the m_gradeLevel
             */
            public String getGradeLevel() {
                return m_gradeLevel;
            }

            /**
             * Gets the school.
             *
             * @return the m_school
             */
            public SisSchool getSchool() {
                return m_school;
            }

            /**
             * NOTE: This method is called using reflection in ADADataHelper.java
             *
             * @return the m_stdEnlSpanCollection
             */
            public List<StudentEnrollmentSpan> getStdEnrlSpans() {
                return m_stdEnrlSpans;
            }

            /**
             * Hash code.
             *
             * @return int
             * @see java.lang.Object#hashCode()
             */
            @Override
            public int hashCode() {
                return m_school.hashCode() + m_fund.hashCode() + m_gradeLevel.hashCode();
            }

            /**
             * Checks if is enrolled.
             *
             * @param currDate PlainDate
             * @return true, if is enrolled
             */
            public boolean isEnrolled(PlainDate currDate) {
                boolean value = false;
                for (StudentEnrollmentSpan span : m_stdEnrlSpans) {
                    PlainDate startDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                    PlainDate endDate = span.getFirstInactiveEnrollment() == null
                            ? m_adaData.m_endDate
                            : span.getFirstInactiveEnrollment().getEnrollmentDate() == null
                                    ? m_adaData.m_endDate
                                    : span.getFirstInactiveEnrollment().getEnrollmentDate();
                    if (!currDate.before(startDate) && !currDate.after(endDate)) {
                        // Not special ed and within enrollment span
                        if (!FUND_SPECIAL_EDUCATION.equals(m_fund)) {
                            value = true;
                        }
                        ReferenceCode refCodeSped = m_adaData.m_referenceSpedCodeMap.get(m_student.getSpedStatusCode());
                        if (isSpecialEdByStudent(refCodeSped)) {
                            // test program code
                            List<StudentProgramParticipation> programs = m_adaData.getStudentPrograms(m_student);
                            if (programs != null) {
                                for (StudentProgramParticipation program : programs) {
                                    if (!currDate.before(program.getStartDate()) &&
                                            (program.getEndDate() == null || !currDate.after(program.getEndDate()))) {
                                        // found matching program
                                        if (!FUND_SPECIAL_EDUCATION.equals(m_fund)) {
                                            // not special ed
                                            value = false;
                                        } else {
                                            // special ed
                                            value = true;
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
                return value;
            }

            /**
             * Sets the calendar.
             *
             * @param calendar void
             */
            public void setCalendar(SchoolCalendar calendar) {
                m_calendar = calendar;
            }

            /**
             * Sets the calendar dates.
             *
             * @param calDates Map<PlainDate,SchoolCalendarDate>
             */
            public void setCalendarDates(Map<PlainDate, SchoolCalendarDate> calDates) {
                m_calDates = calDates;
            }
        }

        ADADetail m_adaData;
        Map<PlainDate, StudentAttendance> m_attendanceMap;
        X2Broker m_broker;
        List<SchoolFundGradeCombination> m_combinations;
        List<PlainDate> m_dates;
        SisStudent m_student;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ADADetailEntity() {
            // Must have public no argument constructor
        }

        /**
         * Gets the ADA detail.
         *
         * @return ADA detail
         */
        public ADADetail getADADetail() {
            return m_adaData;
        }

        /**
         * NOTE: This method is called using reflection in ADADataHelper.java
         *
         * @return the m_combinations
         */
        public SchoolFundGradeCombination getCombination() {
            return m_combinations.get(getCurrentRow() / m_dates.size());

        }

        /**
         * Returns the StudentAttendance record for the current index.
         *
         * @return StudentAttendance
         */
        public PlainDate getDate() {
            return m_dates.get(getCurrentRow() % m_dates.size());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();

            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";

            PlainDate date = getDate();
            if (date != null) {
                name += date.toString();
            }

            return name;
        }

        /**
         * NOTE: This method is called using reflection in ADADataHelper.java
         * Returns student attendance by date.
         *
         * @return Student attendance
         */
        public StudentAttendance getStdAtt() {
            StudentAttendance att = null;
            if (m_attendanceMap.containsKey(getDate())) {
                att = m_attendanceMap.get(getDate());
            }
            return att;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_student = (SisStudent) bean;
            m_adaData = (ADADetail) data;
            m_broker = data.getBroker();
            m_combinations = new ArrayList<SchoolFundGradeCombination>();
            m_attendanceMap = new HashMap<PlainDate, StudentAttendance>();
            m_dates = new ArrayList<PlainDate>();

            List<StudentEnrollmentSpan> studentEnrollmentSpans = m_adaData.m_helper.getStudentEnrollmentSpans(m_student,
                    true);

            List<StudentSchool> secondarySchools = m_adaData.m_includeSecondarySchool
                    ? m_adaData.getStudentSchools(m_student)
                    : null;

            for (StudentEnrollmentSpan stdEnrSpan : studentEnrollmentSpans) {
                SisSchool school = stdEnrSpan.getSchool();
                ReferenceCode refCode = getGradeLevel(m_student, stdEnrSpan);

                String grade = refCode.getCode();
                if (isRegular(stdEnrSpan)) {
                    setCombination(school, FUND_REGULAR, grade, stdEnrSpan);
                    if (secondarySchools != null) {
                        for (StudentSchool secondarySchool : secondarySchools) {
                            setCombination((SisSchool) secondarySchool.getSchool(), FUND_REGULAR, grade, stdEnrSpan);
                        }
                    }
                }
                if (isSpecialEd(stdEnrSpan)) {
                    setCombination(school, FUND_SPECIAL_EDUCATION, grade, stdEnrSpan);
                    if (secondarySchools != null) {
                        for (StudentSchool secondarySchool : secondarySchools) {
                            setCombination((SisSchool) secondarySchool.getSchool(), FUND_REGULAR, grade, stdEnrSpan);
                        }
                    }
                }
                m_attendanceMap.putAll(buildStudentAttendanceMap(stdEnrSpan));
            }

            m_dates.addAll(getDaysToRetrieve(m_adaData.m_startDate, m_adaData.m_endDate));
            setRowCount(m_dates.size() * m_combinations.size());
            // Must be processed in sorted order
            Collections.sort(m_dates);
        }

        /**
         * Check if given student's enrollment span is regular education.
         *
         * @param refCodeSped ReferenceCode
         * @return true, if is special ed by student
         */
        boolean isSpecialEdByStudent(ReferenceCode refCodeSped) {
            boolean value = false;

            if (refCodeSped != null && "Active".equals(refCodeSped.getCode())) {
                value = true;
            }
            return value;
        }

        /**
         * Build student attendance map. Values are got from the given StudentEnrollmentSpan
         *
         * @param enrSpan StudentEnrollmentSpan
         * @return Map
         */
        private Map<PlainDate, StudentAttendance> buildStudentAttendanceMap(StudentEnrollmentSpan enrSpan) {

            Map<PlainDate, StudentAttendance> stdAttMap = new HashMap<PlainDate, StudentAttendance>();
            List<StudentAttendance> stdAtts = enrSpan.getStudentAttendance();

            if (stdAtts != null && !stdAtts.isEmpty()) {
                for (StudentAttendance stdAtt : stdAtts) {
                    stdAttMap.put(stdAtt.getDate(), stdAtt);

                }
            }

            return stdAttMap;
        }

        /**
         * Returns list of all dates which are possible to be enrolled by
         * student in the range of input parameters dates.
         *
         * @param start PlainDate
         * @param end PlainDate
         * @return dates List
         */
        private ArrayList<PlainDate> getDaysToRetrieve(PlainDate start, PlainDate end) {
            ArrayList<PlainDate> days = new ArrayList<PlainDate>();
            PlainDate tempDate = start;
            Calendar calEndDate = Calendar.getInstance();
            calEndDate.setTime(end);
            calEndDate.add(Calendar.DATE, 1);
            PlainDate endDate = new PlainDate(calEndDate.getTime());
            while (tempDate.before(endDate)) {

                Calendar cal = Calendar.getInstance();
                cal.setTime(tempDate);
                if (1 != cal.get(Calendar.DAY_OF_WEEK) && 7 != cal.get(Calendar.DAY_OF_WEEK)) {
                    days.add(tempDate);
                }
                cal.add(Calendar.DATE, 1);
                tempDate = new PlainDate(cal.getTime());
            }

            return days;
        }

        /**
         * Calculate grade code from StudentEnrollmentSpan.
         *
         * @param student SisStudent
         * @param span StudentEnrollmentSpan
         * @return Reference code
         */
        private ReferenceCode getGradeLevel(SisStudent student, StudentEnrollmentSpan span) {
            ReferenceCode gradeCode = null;
            if (span.getFirstActiveEnrollment().getYog() == student.getYog()) {
                String gradeLevel = student.getGradeLevel();
                gradeCode = m_adaData.m_referenceGradeCodeMap.get(gradeLevel);
            } else {
                ModelBroker broker = new ModelBroker(m_adaData.getPrivilegeSet());
                TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
                List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(
                        StudentManager.getMaxGradeLevel(m_adaData.getBroker()),
                        span.getFirstActiveEnrollment().getYog(),
                        m_adaData.m_context.getSchoolYear(),
                        sortedGradeLevels);
                for (String matchingGradeLevel : matchingGradeLevels) {
                    gradeCode = m_adaData.m_referenceGradeCodeMap.get(matchingGradeLevel);
                    if (gradeCode != null) {
                        break;
                    }
                }
            }

            return gradeCode;
        }

        /**
         * Returns next in session date after passed beginDate. If there are no in session
         * dates after passed beginDate, returns date after endDate
         *
         * @param stdEnrSpan StudentEnrollmentSpan
         * @param beginDate PlainDate
         * @param endDate PlainDate
         * @return Plain date
         */
        private PlainDate getNextInSessionDate(StudentEnrollmentSpan stdEnrSpan,
                                               PlainDate beginDate,
                                               PlainDate endDate) {
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(beginDate);
            calendar.add(Calendar.DATE, 1);
            beginDate = new PlainDate(calendar.getTime());

            SchoolCalendar sklCalendar = m_adaData.getSchoolCalendar(stdEnrSpan.getSchool(), m_student);

            if (sklCalendar != null && m_adaData.m_calendarsMap.containsKey(sklCalendar)) {
                while (!beginDate.after(endDate)) {
                    SchoolCalendarDate sklCalDate = m_adaData.m_calendarsMap.get(sklCalendar).get(beginDate);
                    if (sklCalDate != null) {
                        if (sklCalDate.getInSessionIndicator()) {
                            break;
                        }
                    }
                    calendar.add(Calendar.DATE, 1);
                    beginDate = new PlainDate(calendar.getTime());
                }
            } else {
                calendar.setTime(endDate);
                calendar.add(Calendar.DATE, 1);
                beginDate = new PlainDate(calendar.getTime());
            }

            return beginDate;
        }

        /**
         * Check if given student's enrollment span is sped.
         *
         * @param span StudentEnrollmentSpan
         * @return true, if is regular
         */
        private boolean isRegular(StudentEnrollmentSpan span) {
            boolean value = false;

            ReferenceCode refCodeSped = m_adaData.m_referenceSpedCodeMap.get(m_student.getSpedStatusCode());
            if (isSpecialEdByStudent(refCodeSped)) {
                PlainDate beginDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                if (beginDate.before(m_adaData.m_startDate)) {
                    beginDate = m_adaData.m_startDate;
                }
                PlainDate endDate = span.getFirstInactiveEnrollment() == null
                        ? m_adaData.m_endDate
                        : span.getFirstInactiveEnrollment().getEnrollmentDate() == null
                                ? m_adaData.m_endDate
                                : span.getFirstInactiveEnrollment().getEnrollmentDate();
                List<StudentProgramParticipation> programs = m_adaData.getStudentPrograms(m_student);
                if (programs != null) {
                    Iterator<StudentProgramParticipation> iterator = programs.iterator();
                    while (iterator.hasNext() && beginDate.before(endDate)) {
                        StudentProgramParticipation program = iterator.next();
                        if (beginDate.before(program.getStartDate())) {
                            value = true;
                        }
                        if (program.getEndDate() == null || program.getEndDate().after(beginDate)) {
                            beginDate = program.getEndDate() == null ? m_adaData.m_endDate : program.getEndDate();
                            beginDate = getNextInSessionDate(span, beginDate, endDate);
                        }
                    }
                }
                if (!beginDate.after(endDate)) {
                    value = true;
                }
            } else {
                value = true;
            }
            return value;
        }

        /**
         * Check if given student's enrollment span is regular education.
         *
         * @param span StudentEnrollmentSpan
         * @return true, if is special ed
         */
        private boolean isSpecialEd(StudentEnrollmentSpan span) {
            boolean value = false;

            ReferenceCode refCodeSped = m_adaData.m_referenceSpedCodeMap.get(m_student.getSpedStatusCode());
            if (isSpecialEdByStudent(refCodeSped)) {
                PlainDate beginDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                if (beginDate.before(m_adaData.m_startDate)) {
                    beginDate = m_adaData.m_startDate;
                }
                PlainDate endDate = span.getFirstInactiveEnrollment() == null
                        ? m_adaData.m_endDate
                        : span.getFirstInactiveEnrollment().getEnrollmentDate() == null
                                ? m_adaData.m_endDate
                                : span.getFirstInactiveEnrollment().getEnrollmentDate();
                List<StudentProgramParticipation> programs = m_adaData.getStudentPrograms(m_student);
                if (programs != null) {
                    Iterator<StudentProgramParticipation> iterator = programs.iterator();
                    while (iterator.hasNext()) {
                        StudentProgramParticipation program = iterator.next();
                        PlainDate lastDate = program.getEndDate() == null ? m_adaData.m_endDate : program.getEndDate();
                        if (!beginDate.after(lastDate) && !endDate.before(program.getStartDate())) {
                            value = true;
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Add or update combination generated by given school, grade, fund.
         *
         * @param school SisSchool
         * @param fund String
         * @param grade String
         * @param stdEnrSpan StudentEnrollmentSpan
         */
        private void setCombination(SisSchool school, String fund, String grade, StudentEnrollmentSpan stdEnrSpan) {
            SchoolFundGradeCombination combination = new SchoolFundGradeCombination(school, fund, grade);
            int index = m_combinations.indexOf(combination);
            if (index >= 0) {
                combination = m_combinations.get(index);
            } else {
                m_combinations.add(combination);
                SchoolCalendar sklCalendar = m_adaData.getSchoolCalendar(combination.getSchool(), m_student);
                if (sklCalendar != null && m_adaData.m_calendarsMap.containsKey(sklCalendar)) {
                    combination.setCalendar(sklCalendar);
                    combination.setCalendarDates(m_adaData.m_calendarsMap.get(sklCalendar));
                }
            }
            combination.addStdEnrlSpan(stdEnrSpan);
        }



    }

    /**
     * Retrieve a value from the attendance record.
     */
    protected class RetrieveReportValue implements FieldRetriever {
        public static final String CALCULATION_ID = "EXPDATA-CA-ADAD";
        private static final String ATT_CODE_ABSENT = "A";
        private static final String ATT_CODE_ABSENT_EX = "EX";
        private static final String ATT_CODE_NOT_ENROLLED = "NE";
        private static final String ATT_CODE_PRESENT = "+";
        private static final String ATT_CODE_SCHOOL_CLOSED = "KC";
        private static final String ATT_CODE_TARDY = "T-";
        private static final String ATT_CODE_TARDY_EX = "T+";
        private static final String ATT_CODE_TRUANT = "TR";
        private final List<String> ATT_CODES_SUSPENSION = Arrays.asList(new String[] {"IS", "IH", "SU"});

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();
            ADADetailEntity myEntity = (ADADetailEntity) entity;
            ADADetail myData = (ADADetail) data;
            PlainDate currDate = myEntity.getDate();
            String enrollmentType = "";

            if (param.equals(ADADetail.PARAM_ATT_DATE)) {
                value = currDate;
            }

            else if (param.equals(ADADetail.PARAM_ATT_CODE)) {
                value = evaluateAttCode(myEntity, currDate);
            } else if (param.equals(ADADetail.PARAM_ENR_TYPE)) {
                for (StudentEnrollmentSpan span : myEntity.getCombination().getStdEnrlSpans()) {
                    if (currDate.equals(span.getFirstActiveDate())) {
                        enrollmentType = "First";
                    }
                    if (currDate.equals(span.getLastActiveDate())) {
                        enrollmentType = "Last";
                    }
                }
                value = enrollmentType;
            } else if (param.equals(ADADetail.PARAM_ENROLLED)) {
                value = myEntity.getCombination().isEnrolled(currDate) ? BooleanAsStringConverter.TRUE
                        : BooleanAsStringConverter.FALSE;
            } else if (param.equals(ADADetail.PARAM_TRUANT)) {
                value = ATT_CODE_TRUANT.equals(evaluateAttCode(myEntity, currDate)) ? BooleanAsStringConverter.TRUE
                        : BooleanAsStringConverter.FALSE;
            } else if (param.equals(ADADetail.PARAM_APPORT)) {

                value = BooleanAsStringConverter.FALSE;

                String attCode = evaluateAttCode(myEntity, currDate);
                if (!ATT_CODE_NOT_ENROLLED.equals(attCode) && !ATT_CODE_SCHOOL_CLOSED.equals(attCode)) {
                    StudentAttendance stdAtt = myEntity.getStdAtt();
                    if (stdAtt == null || !stdAtt.getAbsentIndicator() || !StringUtils.isEmpty(stdAtt.getOtherCode())) {
                        value = BooleanAsStringConverter.TRUE;
                    }
                }
            } else if (param.equals(ADADetail.PARAM_SUSPENDED)) {
                value = BooleanAsStringConverter.FALSE;

                StudentAttendance stdAttEX = myEntity.getStdAtt();
                if (stdAttEX != null) {
                    if (ATT_CODES_SUSPENSION.contains(stdAttEX.getReasonCode())) {
                        value = BooleanAsStringConverter.TRUE;
                    }
                }
            }

            else if (param.equals(ADADetail.PARAM_SCHEDULED)) {
                value = BooleanAsStringConverter.FALSE;
                Map<PlainDate, SchoolCalendarDate> calDates = myEntity.getCombination().getCalendarDates();

                if (calDates != null) {
                    SchoolCalendarDate date = calDates.get(currDate);
                    if (date != null && date.getInSessionIndicator()) {
                        value = BooleanAsStringConverter.TRUE;
                    }
                }
            } else if (param.equals(ADADetail.PARAM_EXCUSED)) {
                String excusedField = BooleanAsStringConverter.FALSE;
                StudentAttendance stdAttEX = myEntity.getStdAtt();
                if (stdAttEX != null) {
                    if (stdAttEX.getExcusedIndicator()) {
                        excusedField = BooleanAsStringConverter.TRUE;
                    }
                }
                value = excusedField;
            } else if (param.equals(ADADetail.PARAM_ABSENT)) {
                String absentField = BooleanAsStringConverter.FALSE;
                StudentAttendance stdAtt = myEntity.getStdAtt();
                if (stdAtt != null) {
                    if (stdAtt.getAbsentIndicator()) {
                        absentField = BooleanAsStringConverter.TRUE;
                    }

                }
                value = absentField;
            }

            else if (param.equals(ADADetail.PARAM_ATT_PERIOD)) {
                DistrictCalendar cal = myData.m_districtCalendarMap.get(currDate);
                if (cal != null) {
                    value = cal.getCycle();
                }
            } else if (param.equals(ADADetail.PARAM_STD_FUND)) {
                value = myEntity.getCombination().getFund();
            } else if (param.equals(ADADetail.PARAM_GRADE_LEVEL)) {

                value = myEntity.getCombination().getGradeLevel();
            } else if (param.equals(ADADetail.PARAM_STD_SCHOOL)) {

                value = myEntity.getCombination().getSchool().getOid();
            }

            return value;
        }

        /**
         * Generate attendance code according to specification: <br>
         * <br>
         * if not enrolled then code will be 'NE' <br>
         * else if school closed the code will be 'KC' <br>
         * else if other code field is populated that code will be used <br>
         * else if the reason is populated the state code from the reason reference table will be
         * used <br>
         * else if the record is absent { if excused code 'EX' else 'A'} <br>
         * else if the record is tardy { if excused code 'T+' else 'T-'} <br>
         * else present "+" <br>
         * .
         *
         * @param myEntity ADADetailEntity
         * @param currDate PlainDate
         * @return String
         */
        private String evaluateAttCode(ADADetailEntity myEntity, PlainDate currDate) {
            String value = ATT_CODE_PRESENT;

            Map<PlainDate, SchoolCalendarDate> calDates = myEntity.getCombination().getCalendarDates();
            SchoolCalendarDate date = null;
            if (calDates != null) {
                date = calDates.get(currDate);
            }

            if (date == null || !date.getInSessionIndicator()) {
                value = ATT_CODE_SCHOOL_CLOSED;
            } else if (myEntity.getCombination().isEnrolled(currDate)) {
                StudentAttendance stdAtt = myEntity.getStdAtt();
                if (stdAtt != null) {
                    if (!StringUtils.isEmpty(stdAtt.getOtherCode())) {
                        value = stdAtt.getOtherCode();
                    } else if (!StringUtils.isEmpty(getReasonCode(myEntity, stdAtt))) {
                        value = getReasonCode(myEntity, stdAtt);
                    } else if (stdAtt.getAbsentIndicator()) {
                        value = stdAtt.getExcusedIndicator() ? ATT_CODE_ABSENT_EX : ATT_CODE_ABSENT;
                    } else if (stdAtt.getTardyIndicator()) {
                        value = stdAtt.getExcusedIndicator() ? ATT_CODE_TARDY_EX : ATT_CODE_TARDY;
                    }
                }
            } else {
                value = ATT_CODE_NOT_ENROLLED;
            }

            return value;
        }

        /**
         * Returns reason code.
         *
         * @param myEntity ADADetailEntity
         * @param stdAtt StudentAttendance
         * @return String
         */
        private String getReasonCode(ADADetailEntity myEntity, StudentAttendance stdAtt) {
            return myEntity.m_adaData.lookupStateValue(StudentAttendance.class, StudentAttendance.COL_REASON_CODE,
                    stdAtt.getReasonCode());
        }

    }

    protected static final String FUND_REGULAR = "Regular";
    protected static final String FUND_SPECIAL_EDUCATION = "Special Education";

    /*
     * Input parameters
     */
    private static final String IN_PARAM_DAY_END = "endDate";
    private static final String IN_PARAM_DAY_START = "startDate";
    private static final String IN_PARAM_INCL_SECONDARY = "includeSecondarySchool";

    /*
     * Fields parameters
     */
    private static final String PARAM_ATT_DATE = "ATT-DATE";
    private static final String PARAM_ATT_CODE = "ATT-CODE";
    private static final String PARAM_ATT_PERIOD = "ATT-PERIOD";
    private static final String PARAM_STD_FUND = "STD-FUND";
    private static final String PARAM_GRADE_LEVEL = "GRADE-LEVEL";
    private static final String PARAM_STD_SCHOOL = "STD-SCHOOL";
    private static final String PARAM_SCHEDULED = "SCHEDULED";
    private static final String PARAM_EXCUSED = "EXCUSED";
    private static final String PARAM_ABSENT = "ABSENT";
    private static final String PARAM_ENR_TYPE = "ENR_TYPE";
    private static final String PARAM_ENROLLED = "ENROLLED";
    private static final String PARAM_SUSPENDED = "SUSPENDED";
    private static final String PARAM_TRUANT = "TRUANT";
    private static final String PARAM_APPORT = "APPORT";

    private static final String SPECIAL_ED_CODE = "144";

    /*
     * Other constants
     */
    protected Map<SchoolCalendar, Collection<SchoolCalendarDate>> m_calendars;
    protected Map<SchoolCalendar, Map<PlainDate, SchoolCalendarDate>> m_calendarsMap;
    protected SisDistrictSchoolYearContext m_context;
    protected PlainDate m_endDate;
    protected Boolean m_excludeSchoolsParam;
    protected final SimpleDateFormat m_formatRange = new SimpleDateFormat("dd/MM/yyyy");
    protected StudentHistoryHelper m_helper;
    protected Map<PlainDate, DistrictCalendar> m_districtCalendarMap;
    protected Boolean m_includeSecondarySchool;
    protected Map<String, ReferenceCode> m_referenceGradeCodeMap;
    protected Map<String, ReferenceCode> m_referenceSpedCodeMap;
    protected Map<String, Map<String, Collection<SchoolCalendar>>> m_schoolCalendarsMap;
    protected PlainDate m_startDate;
    protected Map<String, List<StudentProgramParticipation>> m_studentProgramMap;
    protected Map<String, List<StudentSchool>> m_studentSchoolMap;

    /**
     * Returns StudentHistoryHelper instance.
     *
     * @return Student history helper
     */
    public StudentHistoryHelper getHelper() {
        return m_helper;
    }

    /**
     * Get most common calendar.
     *
     * @param school SisSchool
     * @param student SisStudent
     * @return School calendar
     */
    protected SchoolCalendar getSchoolCalendar(SisSchool school, SisStudent student) {
        SchoolCalendar calendar = null;

        if (m_schoolCalendarsMap == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());

            QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, criteria);
            m_schoolCalendarsMap = getBroker().getGroupedCollectionByQuery(query,
                    new String[] {SchoolCalendar.COL_SCHOOL_OID,
                            SchoolCalendar.COL_CALENDAR_ID},
                    new int[] {100, 10});
        }

        Map<String, Collection<SchoolCalendar>> calendars = m_schoolCalendarsMap.get(school.getOid());
        if (calendars != null) {
            Collection<SchoolCalendar> calendarCollection = calendars.get(student.getCalendarCode());
            if (calendarCollection != null) {
                calendar = calendarCollection.iterator().next();
                if (calendar == null) {
                    calendar = calendars.values().iterator().next().iterator().next();
                }
            } else {
                calendar = calendars.values().iterator().next().iterator().next();
            }
        }

        return calendar;
    }

    /**
     * Get all programs of the given student.
     *
     * @param student SisStudent
     * @return List
     */
    protected List<StudentProgramParticipation> getStudentPrograms(SisStudent student) {
        return m_studentProgramMap.get(student.getOid());
    }

    /**
     * Get all secondary schools of the given student.
     *
     * @param student SisStudent
     * @return List
     */
    protected List<StudentSchool> getStudentSchools(SisStudent student) {
        return m_studentSchoolMap.get(student.getOid());
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {

        if (m_context == null) {
            m_context = (SisDistrictSchoolYearContext) getCurrentContext();
        }

        m_startDate = (PlainDate) getParameter(IN_PARAM_DAY_START);
        m_endDate = (PlainDate) getParameter(IN_PARAM_DAY_END);
        Object includeSecondary = getParameter(IN_PARAM_INCL_SECONDARY);
        m_includeSecondarySchool = includeSecondary != null && includeSecondary instanceof Boolean
                ? ((Boolean) includeSecondary).booleanValue()
                : false;

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
        if (m_includeSecondarySchool) {
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.TRUE);
        }

        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);

        loadGradeCodes();
        loadSPEDCodes();
        loadDistrictCalendar();
        loadStudentPrograms();
        if (m_includeSecondarySchool) {
            loadStudentSecondarySchool();
        }

        m_calendarsMap = getSchoolCalendarMap(m_startDate, m_endDate);

        // Setup OK
        if (getSetupErrors().size() == 0) {
            // create query - use the appropriate class
            setQuery(m_helper.getStudentQuery(false));
            getBroker().getCollectionByQuery(m_helper.getStudentQuery(false));
            // Set Custom Entity
            setEntityClass(ADADetailEntity.class);

            // Build and attach retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveReportValue.CALCULATION_ID, new RetrieveReportValue());
            super.addCalcs(calcs);
        }
    }

    /**
     * Get collection of program codes corresponding to given state reference code.
     *
     * @param programCategory String
     * @return Collection of program codes
     */
    private Collection<String> getProgramCodes(String programCategory) {
        X2Criteria criteria = new X2Criteria();
        DataDictionaryField field = getDataDictionaryField(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_PROGRAM_CODE);

        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, programCategory);

        String[] columns = new String[] {ReferenceCode.COL_CODE};

        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

        List<String> result = new ArrayList<String>();
        result.add("__dummy__");
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                result.add(code);
            }
        } finally {
            iterator.close();
        }
        return result;
    }

    /**
     * Build map of all possible school calendars in the given date range.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Map
     */
    private Map<SchoolCalendar, Map<PlainDate, SchoolCalendarDate>> getSchoolCalendarMap(PlainDate startDate,
                                                                                         PlainDate endDate) {

        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(SchoolCalendarDate.COL_DATE, startDate);
        criteria.addLessOrEqualThan(SchoolCalendarDate.COL_DATE, endDate);
        QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        Collection<SchoolCalendarDate> datesList = getBroker().getCollectionByQuery(query);
        Map<SchoolCalendar, Map<PlainDate, SchoolCalendarDate>> map =
                new HashMap<SchoolCalendar, Map<PlainDate, SchoolCalendarDate>>();
        for (SchoolCalendarDate calendarDate : datesList) {
            if (map.containsKey(calendarDate.getSchoolCalendar())) {
                map.get(calendarDate.getSchoolCalendar()).put(calendarDate.getDate(), calendarDate);
            } else {
                Map<PlainDate, SchoolCalendarDate> dates = new HashMap<PlainDate, SchoolCalendarDate>();
                dates.put(calendarDate.getDate(), calendarDate);
                map.put(calendarDate.getSchoolCalendar(), dates);
            }
        }

        return map;

    }

    /**
     * Initialize map of all district's calendars keyed on dates.
     */
    private void loadDistrictCalendar() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());
        QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);
        m_districtCalendarMap = getBroker().getMapByQuery(query, DistrictCalendar.COL_DATE, 400);
    }

    /**
     * Load grade codes.
     */
    private void loadGradeCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop =
                new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        m_referenceGradeCodeMap = referenceTable.getCodeMap();
    }

    /**
     * Load grade codes.
     */
    private void loadSPEDCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop =
                new ModelProperty(SisStudent.class, SisStudent.COL_SPED_STATUS_CODE, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        m_referenceSpedCodeMap = referenceTable.getCodeMap();
    }

    /**
     * Load student programs for special education for entire year.
     */
    private void loadStudentPrograms() {
        X2Criteria studentCriteria = m_helper.getStudentCriteria();

        X2Criteria programCriteria = new X2Criteria();
        programCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getEndDate());
        X2Criteria endDate1Criteria = new X2Criteria();
        endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());

        X2Criteria endDate2Criteria = new X2Criteria();
        endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                getCurrentContext().getStartDate());

        endDate1Criteria.addOrCriteria(endDate2Criteria);
        programCriteria.addAndCriteria(endDate1Criteria);

        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        programCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);

        programCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, getProgramCodes(SPECIAL_ED_CODE));

        QueryByCriteria programQuery = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);
        programQuery.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
        programQuery.addOrderBy(StudentProgramParticipation.COL_START_DATE, true);
        m_studentProgramMap = getBroker().getGroupedCollectionByQuery(programQuery,
                StudentProgramParticipation.COL_STUDENT_OID, 1024);
    }

    /**
     * Load student programs for special education for entire year.
     */
    private void loadStudentSecondarySchool() {
        X2Criteria studentCriteria = m_helper.getStudentCriteria();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisBeanPaths.STUDENT_SCHOOL.type().getPath(), Integer.valueOf(StudentSchool.SECONDARY));
        criteria.addEqualTo(SisBeanPaths.STUDENT_SCHOOL.districtContextOid().getPath(), getCurrentContext().getOid());
        criteria.addNotEqualTo(SisBeanPaths.STUDENT_SCHOOL.school().inactiveIndicator().getPath(), Boolean.TRUE);
        criteria.addNotEqualTo(SisBeanPaths.STUDENT_SCHOOL.school().archiveIndicator().getPath(), Boolean.TRUE);

        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        criteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubQuery);

        QueryByCriteria query = new QueryByCriteria(StudentSchool.class, criteria);
        query.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
        m_studentSchoolMap = getBroker().getGroupedCollectionByQuery(query,
                SisBeanPaths.STUDENT_SCHOOL.studentOid().getPath(), 1024);
    }

}
