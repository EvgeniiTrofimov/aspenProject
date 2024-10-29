/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.nj;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class StudentRegister.
 */
public class StudentRegister extends StateReportData {

    /**
     * The Class StudentRegisterEntity.
     */
    public static class StudentRegisterEntity extends StateReportEntity {
        StudentRegister m_studentRegisterData;
        Iterator m_attendanceByGradeIterator;
        Iterator m_attendanceDetailsIterator;
        Map<String, Object> m_currentAttendanceMap;


        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentRegisterEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_studentRegisterData = (StudentRegister) data;
            Map<String, Map<String, Map<String, Integer>>> attendanceMap =
                    m_studentRegisterData.m_attendanceDetails.m_attendanceMap;
            int numAttendanceElements = 22;
            setRowCount(attendanceMap.size() * numAttendanceElements);

            m_attendanceByGradeIterator = attendanceMap.entrySet().iterator();
            // TODO create iterator for attendanceMap and then method to grab each category iterator
        }

        /**
         * Gets the row name.
         *
         * @return String
         */
        public String getRowName() {
            String name = null;

            if (m_attendanceByGradeIterator.hasNext() &&
                    (m_attendanceDetailsIterator == null || !m_attendanceDetailsIterator.hasNext())) {
                Map.Entry pairs = (Map.Entry) m_attendanceByGradeIterator.next();

                // set name to be grade value
                name = "Grade " + (String) pairs.getKey();

                // get attendance value map
                Map<String, Map<String, Integer>> attendanceValuesMap =
                        (Map<String, Map<String, Integer>>) pairs.getValue();
                m_attendanceDetailsIterator = attendanceValuesMap.entrySet().iterator();

                // set values to be heading row
                m_currentAttendanceMap = new HashMap<String, Object>();
                m_currentAttendanceMap.put(DAYS_SCHOOL_OPEN, DAYS_SCHOOL_OPEN);
                m_currentAttendanceMap.put(DAYS_POSSIBLE, DAYS_POSSIBLE);
                m_currentAttendanceMap.put(DAYS_PRESENT, DAYS_PRESENT);
            } else if (m_attendanceDetailsIterator != null && m_attendanceDetailsIterator.hasNext()) {
                Map.Entry pairs = (Map.Entry) m_attendanceDetailsIterator.next();
                name = "\t" + (String) pairs.getKey();
                m_currentAttendanceMap = (Map<String, Object>) pairs.getValue();
            }

            return name;
        }

        /**
         * Gets the row value.
         *
         * @param key String
         * @return Object
         */
        public Object getRowValue(String key) {
            Object value = null;
            if (m_currentAttendanceMap.containsKey(key)) {
                value = m_currentAttendanceMap.get(key);
            }

            return value;
        }
    }

    /**
     * The Class AttendanceDetails.
     */
    protected class AttendanceDetails {
        protected Map<String, Map<String, Map<String, Integer>>> m_attendanceMap = null;
        protected Map<String, Map<String, Integer>> m_gradeAttendanceDetails = null;

        protected final String LOW_INCOME = "Low Income";
        protected final String NOT_LOW_INCOME = "Not Low Income";
        protected final String TOTAL_LOW_INCOME = "Total All Income";
        protected final String LEP = "LEP";
        protected final String NOT_LEP = "Not LEP";
        protected final String TOTAL_LEP = "Total LEP Not LEP";
        protected final String IEP = "IEP";
        protected final String NOT_IEP = "Not IEP";
        protected final String TOTAL_IEP = "Total IEP Not IEP";
        protected final String TOTAL_RACE = "Total Race";
        protected final String TOTAL_GENDER = "Total Gender";

        /**
         * Instantiates a new attendance details.
         */
        public AttendanceDetails() {
            m_attendanceMap = new TreeMap<String, Map<String, Map<String, Integer>>>();
        }

        /**
         * Creates the initial counts map.
         *
         * @return Map
         */
        private Map<String, Integer> createInitialCountsMap() {
            Map<String, Integer> initialCounts = new HashMap<String, Integer>();
            initialCounts.put(DAYS_SCHOOL_OPEN, Integer.valueOf(180));
            initialCounts.put(DAYS_POSSIBLE, Integer.valueOf(0));
            initialCounts.put(DAYS_PRESENT, Integer.valueOf(0));

            return initialCounts;
        }

        /**
         * Sets the attendance details.
         *
         * @param studentDetails void
         */
        public void setAttendanceDetails(StudentDetails studentDetails) {
            m_gradeAttendanceDetails = new LinkedHashMap<String, Map<String, Integer>>();
            SisStudent student = studentDetails.m_student;
            String gradeLevel = student.getGradeLevel();

            // retrieve the attendance details for the grade if they exist.
            if (m_attendanceMap.containsKey(gradeLevel)) {
                m_gradeAttendanceDetails = m_attendanceMap.get(gradeLevel);
            }

            // initialize category counts for grade if they don't exist.
            if (m_gradeAttendanceDetails.isEmpty()) {
                m_gradeAttendanceDetails.put(LOW_INCOME, createInitialCountsMap());
                m_gradeAttendanceDetails.put(NOT_LOW_INCOME, createInitialCountsMap());
                m_gradeAttendanceDetails.put(TOTAL_LOW_INCOME, createInitialCountsMap());
                m_gradeAttendanceDetails.put(LEP, createInitialCountsMap());
                m_gradeAttendanceDetails.put(NOT_LEP, createInitialCountsMap());
                m_gradeAttendanceDetails.put(TOTAL_LEP, createInitialCountsMap());
                m_gradeAttendanceDetails.put(IEP, createInitialCountsMap());
                m_gradeAttendanceDetails.put(NOT_IEP, createInitialCountsMap());
                m_gradeAttendanceDetails.put(TOTAL_IEP, createInitialCountsMap());
                m_gradeAttendanceDetails.put(HISPANIC, createInitialCountsMap());
                m_gradeAttendanceDetails.put(AMERICAN_INDIAN, createInitialCountsMap());
                m_gradeAttendanceDetails.put(ASIAN, createInitialCountsMap());
                m_gradeAttendanceDetails.put(AFRICAN_AMERICAN, createInitialCountsMap());
                m_gradeAttendanceDetails.put(HAWAIIAN, createInitialCountsMap());
                m_gradeAttendanceDetails.put(WHITE, createInitialCountsMap());
                m_gradeAttendanceDetails.put(MULTI_RACE, createInitialCountsMap());
                m_gradeAttendanceDetails.put(TOTAL_RACE, createInitialCountsMap());
                m_gradeAttendanceDetails.put(REPORTABLE_GENDER_MALE, createInitialCountsMap());
                m_gradeAttendanceDetails.put(REPORTABLE_GENDER_FEMALE, createInitialCountsMap());
                m_gradeAttendanceDetails.put(REPORTABLE_GENDER_UNKNOWN, createInitialCountsMap());
                m_gradeAttendanceDetails.put(TOTAL_GENDER, createInitialCountsMap());
            }

            setLowIncomeStatusCounts(studentDetails);
            setLepStatusCounts(studentDetails);
            setIepStatusCounts(studentDetails);
            setRaceCounts(studentDetails);
            setGenderCounts(studentDetails);

            m_attendanceMap.put(gradeLevel, m_gradeAttendanceDetails);
        }

        /**
         * Sets the catetory attendance.
         *
         * @param category String
         * @param daysPossible int
         * @param daysPresent int
         */
        private void setCatetoryAttendance(String category, int daysPossible, int daysPresent) {
            if (m_gradeAttendanceDetails.containsKey(category)) {
                Map<String, Integer> categoryAttendanceDetails = m_gradeAttendanceDetails.get(category);
                int reportableDaysPossible = categoryAttendanceDetails.get(DAYS_POSSIBLE).intValue() + daysPossible;
                categoryAttendanceDetails.put(DAYS_POSSIBLE, Integer.valueOf(reportableDaysPossible));

                categoryAttendanceDetails = m_gradeAttendanceDetails.get(category);
                int reportableDaysPresent = categoryAttendanceDetails.get(DAYS_PRESENT).intValue() + daysPresent;
                categoryAttendanceDetails.put(DAYS_PRESENT, Integer.valueOf(reportableDaysPresent));
            }
        }

        /**
         * Sets the gender counts.
         *
         * @param studentDetails void
         */
        private void setGenderCounts(StudentDetails studentDetails) {
            // get days possible & days present
            int daysPossible = studentDetails.m_studentDaysPossible;
            int daysPresent = studentDetails.m_studentDaysPresent;

            setCatetoryAttendance(studentDetails.m_studentGender, daysPossible, daysPresent);

            // update totals
            setCatetoryAttendance(TOTAL_GENDER, daysPossible, daysPresent);
        }

        /**
         * Sets the low income status counts.
         *
         * @param studentDetails void
         */
        private void setLowIncomeStatusCounts(StudentDetails studentDetails) {
            // get days possible & days present
            int daysPossible = studentDetails.m_studentDaysPossible;
            int daysPresent = studentDetails.m_studentDaysPresent;

            if (studentDetails.m_studentLowIncome) {
                setCatetoryAttendance(LOW_INCOME, daysPossible, daysPresent);
            }

            // set student as not having low income
            else {
                setCatetoryAttendance(NOT_LOW_INCOME, daysPossible, daysPresent);
            }

            // update totals
            setCatetoryAttendance(TOTAL_LOW_INCOME, daysPossible, daysPresent);
        }

        /**
         * Sets the lep status counts.
         *
         * @param studentDetails void
         */
        private void setLepStatusCounts(StudentDetails studentDetails) {
            // get days possible & days present
            int daysPossible = studentDetails.m_studentDaysPossible;
            int daysPresent = studentDetails.m_studentDaysPresent;

            if (studentDetails.m_studentLEP) {
                setCatetoryAttendance(LEP, daysPossible, daysPresent);
            }

            // set student as not having low income
            else {
                setCatetoryAttendance(NOT_LEP, daysPossible, daysPresent);
            }

            // update totals
            setCatetoryAttendance(TOTAL_LEP, daysPossible, daysPresent);
        }

        /**
         * Sets the iep status counts.
         *
         * @param studentDetails void
         */
        private void setIepStatusCounts(StudentDetails studentDetails) {
            // get days possible & days present
            int daysPossible = studentDetails.m_studentDaysPossible;
            int daysPresent = studentDetails.m_studentDaysPresent;

            if (studentDetails.m_studentIEP) {
                setCatetoryAttendance(IEP, daysPossible, daysPresent);
            }

            // set student as not having low income
            else {
                setCatetoryAttendance(NOT_IEP, daysPossible, daysPresent);
            }

            // update totals
            setCatetoryAttendance(TOTAL_IEP, daysPossible, daysPresent);
        }

        /**
         * Sets the race counts.
         *
         * @param studentDetails void
         */
        private void setRaceCounts(StudentDetails studentDetails) {
            // get days possible & days present
            int daysPossible = studentDetails.m_studentDaysPossible;
            int daysPresent = studentDetails.m_studentDaysPresent;

            if (m_gradeAttendanceDetails.containsKey(studentDetails.m_studentRace)) {
                setCatetoryAttendance(studentDetails.m_studentRace, daysPossible, daysPresent);
            }
            setCatetoryAttendance(TOTAL_RACE, daysPossible, daysPresent);
        }
    }

    /**
     * The Class StudentDetails.
     */
    protected class StudentDetails {
        // StudentDetails member variables
        protected SisStudent m_student;
        protected int m_studentDaysPossible;
        protected int m_studentDaysPresent;
        protected String m_studentGender;
        protected boolean m_studentIEP;
        protected boolean m_studentLEP;
        protected boolean m_studentLowIncome;
        protected String m_studentRace;
        protected List<String> m_lowIncomeValues = Arrays.asList(new String[] {"F", "R"});

        // Aliases used by StudentDetails
        final String ALIAS_FREE_REDUCED_LUNCH = "DOE FREE REDUCED LUNCH";

        // Bean paths used by StudentDetails
        final String programCodeBeanPath = "programCode";
        final String raceCodeBeanPath = "raceCode";

        // Constants used by StudentDetails
        final String CODE_LEP_PROGRAM = "LEP";

        /**
         * Instantiates a new student details.
         *
         * @param student SisStudent
         */
        public StudentDetails(SisStudent student) {
            m_student = student;
            initializeStudentDetails();
            setGender();
            setIepStatus();
            setLepStatus();
            setLowIncome();
            setRace();
        }

        /**
         * Initialize student details.
         */
        private void initializeStudentDetails() {
            m_studentGender = null;
            m_studentDaysPossible = 0;
            m_studentDaysPresent = 0;
            m_studentIEP = false;
            m_studentLEP = false;
            m_studentLowIncome = false;
            m_studentRace = null;
        }

        /**
         * Sets the days possible.
         *
         * @param daysPossible void
         */
        public void setDaysPossible(int daysPossible) {
            m_studentDaysPossible += daysPossible;
        }

        /**
         * Sets the days present.
         *
         * @param daysPresent void
         */
        public void setDaysPresent(int daysPresent) {
            m_studentDaysPresent += daysPresent;
        }

        /**
         * Sets the iep status.
         */
        private void setIepStatus() {
            if (m_activeIeps.containsKey(m_student.getOid())) {
                m_studentIEP = true;
            }
        }

        /**
         * Sets the lep status.
         */
        private void setLepStatus() {
            Collection<StudentProgramParticipation> programs = m_student.getProgramParticipation();
            for (StudentProgramParticipation program : programs) {
                if (program.getEndDate() == null ||
                        !m_reportStartDate.after(program.getEndDate())) {
                    String programCode = program.getProgramCode();
                    if (!StringUtils.isEmpty(programCode)) {
                        programCode = lookupStateValue(StudentProgramParticipation.class, "programCode", programCode);
                        if (CODE_LEP_PROGRAM.equals(programCode)) {
                            m_studentLEP = true;
                        }
                    }
                }
            }
        }

        /**
         * Sets the gender.
         */
        private void setGender() {
            if (m_student.getPerson() != null && !StringUtils.isEmpty(m_student.getPerson().getGenderCode())) {
                String studentGenderCode = m_student.getPerson().getGenderCode();
                if (GENDER_MALE.equals(studentGenderCode)) {
                    m_studentGender = REPORTABLE_GENDER_MALE;
                } else if (GENDER_FEMALE.equals(studentGenderCode)) {
                    m_studentGender = REPORTABLE_GENDER_FEMALE;
                } else if (GENDER_UNKNOWN.equals(studentGenderCode)) {
                    m_studentGender = REPORTABLE_GENDER_UNKNOWN;
                }
            }
        }

        /**
         * Sets the low income.
         */
        private void setLowIncome() {
            String freeReducedLunch = null;
            freeReducedLunch = (String) m_student.getFieldValueByAlias(ALIAS_FREE_REDUCED_LUNCH);
            if (!StringUtils.isEmpty(freeReducedLunch)) {
                freeReducedLunch = lookupReferenceCodeByAlias(ALIAS_FREE_REDUCED_LUNCH, freeReducedLunch,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                if (m_lowIncomeValues.contains(freeReducedLunch)) {
                    m_studentLowIncome = true;
                }
            }
        }

        /**
         * Sets the race.
         */
        private void setRace() {
            SisPerson person = m_student.getPerson();
            String race = null;

            if (person != null) {
                // If the student is hispanic, this should be the only race considered
                if (person.getHispanicLatinoIndicator()) {
                    race = HISPANIC;
                }

                // Otherwise consider the student's race(s)
                else {
                    ArrayList<Race> raceCollection = new ArrayList<Race>(person.getRaces());
                    if (!raceCollection.isEmpty()) {
                        // If the student is more than one race mark as MULTI_RACE
                        if (raceCollection.size() > 1) {
                            race = MULTI_RACE;
                        }
                        // Otherwise record the state value of their race.
                        // Note these values should match the race values defined in
                        // StudentRegister.class
                        else {
                            race = raceCollection.get(0).getRaceCode();
                            /*
                             * if (!StringUtils.isEmpty(race))
                             * {
                             * race = lookupStateValue(Race.class, raceCodeBeanPath, race);
                             * }
                             */
                        }
                    }
                }
            }

            if (!StringUtils.isEmpty(race)) {
                m_studentRace = race;
            }
        }
    }

    /**
     * The Class RetrieveAttendanceData.
     */
    protected class RetrieveAttendanceData implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();
            StudentRegisterEntity studentRegisterEntity = (StudentRegisterEntity) entity;

            if ("name".equals(param)) {
                value = studentRegisterEntity.getRowName();
            } else {
                value = studentRegisterEntity.getRowValue(param);
            }

            return value;
        }

    }

    protected static final String PARAM_REPORT_DATE = "lastDayOfSchool";
    protected static final String PARAM_STUDENT_LASID = "studentLasid";

    protected static final String ENTRY_ENROLLMENT_CODE = StudentEnrollment.ENTRY;
    protected static final String WHITE = "White";
    protected static final String AFRICAN_AMERICAN = "Black";
    protected static final String HISPANIC = "Hispanic";
    protected static final String AMERICAN_INDIAN = "Native American";
    protected static final String ASIAN = "Asian";
    protected static final String HAWAIIAN = "Pacific Islander";
    protected static final String MULTI_RACE = "Multi Race";

    protected static final String GENDER_MALE = "M";
    protected static final String GENDER_FEMALE = "F";
    protected static final String GENDER_UNKNOWN = "U";

    protected static final String REPORTABLE_GENDER_MALE = "Gender Male";
    protected static final String REPORTABLE_GENDER_FEMALE = "Gener Female";
    protected static final String REPORTABLE_GENDER_UNKNOWN = "Gender Unknown";

    protected static final String ABSENT_REASON_EXCUSED_ABSENCE_STATE_CODE = "3";
    protected static final String ABSENT_REASON_HOME_INSTRUCTION_STATE_CODE = "7";
    protected static final String CODE_SHARED_TIME_STUDENT = "S";

    protected static final String DEFAULT_CALENDAR_ID = "Standard";

    protected static final String DAYS_SCHOOL_OPEN = "daysOpen";
    protected static final String DAYS_POSSIBLE = "daysPossible";
    protected static final String DAYS_PRESENT = "daysPresent";


    protected static final String ALIAS_ENROLLMENT_CLASSIFIER = "DOE ENROLL CLASSIFIER";

    protected Map<String, IepData> m_activeIeps;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportEndDate;
    protected PlainDate m_reportStartDate;
    protected AttendanceDetails m_attendanceDetails;
    protected String m_enrollmentClassifier;


    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_reportStartDate = getCurrentContext().getStartDate();
        m_reportEndDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        String studentLasid = (String) getParameter(PARAM_STUDENT_LASID);

        m_attendanceDetails = new AttendanceDetails();

        m_enrollmentClassifier = translateAliasToJavaName(ALIAS_ENROLLMENT_CLASSIFIER, false);

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportEndDate);

        /*
         * Populate map of active IEPs
         */
        getActiveIeps(m_helper.getStudentCriteria());

        /*
         * Get students active any time this year.
         */
        QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
        Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);

        for (SisStudent student : students) {
            if ("ALL".equalsIgnoreCase(studentLasid) || studentLasid.equals(student.getLocalId())) {
                StudentDetails studentDetails = new StudentDetails(student);

                /*
                 * Select the enrollment spans that are after school's start date.
                 * Note there can be more than one if the student withdrew, then enrolled again
                 * in the same year.
                 */
                List<StudentEnrollmentSpan> enrollmentSpans = m_helper.getStudentEnrollmentSpans(student, true);
                for (StudentEnrollmentSpan enrollmentSpan : enrollmentSpans) {
                    if (enrollmentSpan != null) {
                        calculateAttendance(enrollmentSpan, studentDetails);
                    }
                }

                m_attendanceDetails.setAttendanceDetails(studentDetails);
            }
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {

            Criteria criteria = new Criteria();
            criteria.addEqualTo(X2BaseBean.COL_OID, getOrganization().getOid());
            QueryByCriteria query = new QueryByCriteria(Organization.class, criteria);

            setQuery(query);
            setEntityClass(StudentRegisterEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STD-REG-DATA", new RetrieveAttendanceData());

            super.addCalcs(calcs);
        }
    }

    /**
     * Calculate attendance.
     *
     * @param enrollmentSpan StudentEnrollmentSpan
     * @param studentDetails StudentDetails
     */
    private void calculateAttendance(StudentEnrollmentSpan enrollmentSpan, StudentDetails studentDetails) {
        boolean sharedTimeStudent = false;
        StudentEnrollment latestEnrollment = getLastActiveEnrollment(enrollmentSpan);
        if (!StringUtils.isEmpty(m_enrollmentClassifier) && (null != latestEnrollment)) {
            String enrollmentClassification = (String) latestEnrollment.getFieldValueByBeanPath(m_enrollmentClassifier);
            enrollmentClassification =
                    lookupStateValue(StudentEnrollment.class, m_enrollmentClassifier, enrollmentClassification);
            if (CODE_SHARED_TIME_STUDENT.equals(enrollmentClassification)) {
                sharedTimeStudent = true;
            }
        }

        List<StudentAttendance> attendances = getStudentAttendance(studentDetails, enrollmentSpan);
        BigDecimal totalAbsentDays = new BigDecimal(0.0);
        int excusedAbsentDays = 0;
        for (StudentAttendance attendance : attendances) {
            // Hunterdon doesn't follow the general rule for calculating the
            // membership days. Hunterdon checks the reason for absent.
            // If it is one of the below reasons, then that day is not counted
            // towards actual membership days.
            // State Code Description
            // 3 Excused Absence - Religious Holiday or Bring Your Child to Work Day
            // 7 Home Instruction
            String reason = attendance.getReasonCode();
            reason = lookupReferenceCodeByBeanPath(StudentAttendance.class, StudentAttendance.COL_REASON_CODE, reason,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if (ABSENT_REASON_EXCUSED_ABSENCE_STATE_CODE.equals(reason) ||
                    ABSENT_REASON_HOME_INSTRUCTION_STATE_CODE.equals(reason)) {
                excusedAbsentDays++;
            }
            if (attendance.getAbsentIndicator()) {
                if (sharedTimeStudent) {
                    totalAbsentDays = totalAbsentDays.add(new BigDecimal(1.0));
                } else // is a full time student and should only consider portion of day absent
                {
                    totalAbsentDays = totalAbsentDays.add(attendance.getPortionAbsent());
                }
            }
        }

        // set days possible to be number of membership days minus excused absent days
        int membershipDays = getMembershipDays(studentDetails, enrollmentSpan) - excusedAbsentDays;
        studentDetails.setDaysPossible(membershipDays);

        // round up the totalAbsentDays to the nearest whole number
        BigDecimal roundedAbsentDays = totalAbsentDays.setScale(0, RoundingMode.HALF_UP);
        // set days present
        int unexcusedAbsences = roundedAbsentDays.intValue() - excusedAbsentDays;
        studentDetails.setDaysPresent(membershipDays - unexcusedAbsences);
    }

    /**
     * Get IEPS that were active as of report date.
     *
     * @param studentCriteria X2Criteria
     * @return void
     */
    private void getActiveIeps(X2Criteria studentCriteria) {
        SubQuery activeStudentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        // Preload the Active IEPs for students
        Criteria activeIepCriteria = new Criteria();
        Criteria iepExitedBeforeReportDate = new Criteria();
        Criteria currentlyActiveIepCriteria = new Criteria();

        // Consider any currently active IEPs
        currentlyActiveIepCriteria.addEqualTo(IepData.COL_STATUS_CODE,
                Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));
        // Consider any IEPs exited before or on report end date
        iepExitedBeforeReportDate.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(2));
        iepExitedBeforeReportDate.addGreaterOrEqualThan(IepData.COL_EXIT_DATE, m_reportEndDate);
        currentlyActiveIepCriteria.addOrCriteria(iepExitedBeforeReportDate);

        // Order by status first, to have active appear before exited,
        // and order descending by exit date second to get most recent exited iep.
        activeIepCriteria.addAndCriteria(currentlyActiveIepCriteria);
        activeIepCriteria.addIn(IepData.COL_STUDENT_OID, activeStudentSubQuery);
        QueryByCriteria activeIepQuery = new QueryByCriteria(IepData.class, activeIepCriteria);
        activeIepQuery.addOrderByAscending(IepData.COL_STATUS_CODE);
        activeIepQuery.addOrderByDescending(IepData.COL_EXIT_DATE);

        m_activeIeps = (getBroker().getMapByQuery(activeIepQuery, IepData.COL_STUDENT_OID, 128));
    }

    /**
     * Derives and returns the latest StudentEnrollment based on the enrollment date
     * as retrieved from the latest student enrollment span.
     *
     * @param latestSpan StudentEnrollmentSpan
     * @return lastActiveEnrollment
     */
    private StudentEnrollment getLastActiveEnrollment(StudentEnrollmentSpan latestSpan) {
        StudentEnrollment lastActiveEnrollment = null;

        Collection<StudentEnrollment> studentEnrollments = latestSpan.getEnrollments();
        for (StudentEnrollment enrollment : studentEnrollments) {
            if (ENTRY_ENROLLMENT_CODE.equals(enrollment.getEnrollmentType())) {
                if (lastActiveEnrollment == null ||
                        lastActiveEnrollment.getEnrollmentDate() == null ||
                        (enrollment.getEnrollmentDate() != null
                                && enrollment.getEnrollmentDate().after(lastActiveEnrollment.getEnrollmentDate()))) {
                    lastActiveEnrollment = enrollment;
                }
            }
        }

        return lastActiveEnrollment;
    }

    /**
     * Gets the student attendance.
     *
     * @param studentDetails StudentDetails
     * @param enrollmentSpan StudentEnrollmentSpan
     * @return List
     */
    private List<StudentAttendance> getStudentAttendance(StudentDetails studentDetails,
                                                         StudentEnrollmentSpan enrollmentSpan) {
        PlainDate attendanceStartDate = m_reportStartDate;
        PlainDate attendanceEndDate = m_reportEndDate;
        SisStudent student = studentDetails.m_student;

        ArrayList<StudentAttendance> studentAttendance = new ArrayList<StudentAttendance>(1);
        List<StudentAttendance> parentList = m_helper.getStudentAttendances(student.getOid());

        if (attendanceStartDate.before(enrollmentSpan.getFirstActiveDate())) {
            attendanceStartDate = enrollmentSpan.getFirstActiveDate();
        }
        if (enrollmentSpan.getLastActiveDate() != null
                && enrollmentSpan.getLastActiveDate().before(attendanceEndDate)) {
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(enrollmentSpan.getLastActiveDate());
            calendar.add(Calendar.DATE, 1);
            attendanceEndDate = new PlainDate(calendar.getTime());
        }


        if (parentList != null) {
            for (StudentAttendance attendance : parentList) {
                if (attendanceStartDate != null && !attendance.getDate().before(attendanceStartDate)
                        && !attendance.getDate().after(attendanceEndDate)) {
                    studentAttendance.add(attendance);
                }
            }
        }

        return studentAttendance;
    }

    /**
     * Gets the membership days.
     *
     * @param studentDetails StudentDetails
     * @param enrollmentSpan StudentEnrollmentSpan
     * @return int
     */
    private int getMembershipDays(StudentDetails studentDetails, StudentEnrollmentSpan enrollmentSpan) {
        SisStudent student = studentDetails.m_student;
        SisSchool school = student.getSchool();
        PlainDate attendanceStartDate = m_reportStartDate;

        // Get the in session days for the school and calendar.
        Set<PlainDate> insessionDates = m_helper.getCalendarDays(school, student.getCalendarCode());
        if (insessionDates == null && !DEFAULT_CALENDAR_ID.equals(student.getCalendarCode())) {
            insessionDates = m_helper.getCalendarDays(school, DEFAULT_CALENDAR_ID);
        }

        // Count in session days between (and including) first and last active dates.
        PlainDate endDate = enrollmentSpan.getLastActiveDate();
        if (endDate == null) {
            endDate = m_reportEndDate;
        }

        // if the requested attendance start date is before the first active date,
        // set the first attendance date to be the first active date
        if (attendanceStartDate.before(enrollmentSpan.getFirstActiveDate())) {
            attendanceStartDate = enrollmentSpan.getFirstActiveDate();
        }

        int membershipDays = 0;
        if (insessionDates != null) {
            for (PlainDate date : insessionDates) {
                if (attendanceStartDate != null && !date.before(attendanceStartDate) && !date.after(endDate)) {
                    membershipDays++;
                }
            }
        }
        return membershipDays;
    }

}
