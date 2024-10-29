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
package com.x2dev.reports.statereporting.ny;
/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) Follett School Solutions
 * All rights reserved.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.procedures.statereporting.ny.NYEnrollmentHelper;
import com.x2dev.procedures.statereporting.ny.NYEnrollmentHelper.NYStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.ny.NYEnrollmentHelper.NYStudentHistoryHelper;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Report that builds the following parameters:
 *
 * - attendancePercentage: the percentage of attendance for the school year (see
 * processStudentEnrollmentData)
 * - suspensionCounts: a map of students with out of school suspensions (see addSuspensionCounts)
 * - unexcusedAbsenceCounts: a map of students with unexcused absences by count threshold (see
 * addUnexcusedAbsenceCounts)
 *
 * An empty data source is used as this report doesn't display rows.
 *
 */
public class NYBEDSData extends ReportJavaSourceNet {
    // Constants
    private static final String HISPANIC_LATINO_PREFIX = "hispanicLatino_";
    private static final String KEY_1_TO_4 = "1-4";
    private static final String KEY_10_PLUS = "10+";
    private static final String KEY_5_TO_9 = "5-9";
    private static final String KEY_TOTAL = "total";
    private static final String MULTI_PREFIX = "multi_";
    private static final String OSS_CODE = "OSS";
    private static final String STRING_COMMA = ",";
    private static final String STRING_UNDERSCORE = "_";

    // "Input" parameters - selected by user in report input
    private static final String PARAM_DISTRICT_CONTEXT_OID = "contextOid";

    // "Output" parameters - created by this procedure for use in format
    private static final String PARAM_SUSPENSION_COUNTS_MAP = "suspensionCounts";
    private static final String PARAM_ATTENDANCE_PERCENTAGE = "attendancePercentage";
    private static final String PARAM_DISTRICT_CONTEXT = "districtContext";
    private static final String PARAM_UNEXCUSED_ABSENCE_COUNTS = "unexcusedAbsenceCounts";

    // Member variables
    private Map<String, Integer> m_unexcusedAbsencesByStudent = new HashMap<>();
    private Set m_studentsWithOSSRecords = new HashSet();
    protected DistrictSchoolYearContext m_context;
    protected String m_contextOid;

    /**
     * Gather data.
     *
     * @return net.sf.jasperreports3.engine.JRDataSource
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected net.sf.jasperreports3.engine.JRDataSource gatherData() throws X2BaseException {
        m_contextOid = (String) getParameter(PARAM_DISTRICT_CONTEXT_OID);
        m_context = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class, m_contextOid);


        processStudentEnrollmentData();
        addSuspensionCounts();
        addUnexcusedAbsenceCounts();

        return new net.sf.jasperreports3.engine.JREmptyDataSource(1);
    }

    /**
     * Builds a map containing # of students suspended in the school year selected,
     * grouped by race and gender. The map is added as a report parameter for use by the format.
     *
     * Each student is only counted once in the map.
     * - If a student has the hispanic/latino flag set, they are counted in the hispanicLatino
     * category.
     * - If a student does not have the hispanic/latino flag set, and have multiple race records,
     * they are counted in the "multi" category.
     * - Otherwise, the state code equivalent of the student's race code is used as the category.
     *
     * Each category is broken down by gender; the category keys above are appended with an _M or
     * _F to distinguish.
     *
     * Suspensions counts are determined by attendance records with an other code of "OSS".
     *
     */
    private void addSuspensionCounts() {
        Map<String, Integer> suspensionCounts = new HashMap<>();
        addToMapValue(suspensionCounts, KEY_TOTAL, Integer.valueOf(0));

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField raceCodeField =
                dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);
        DataDictionaryField genderCodeField =
                dictionary.findDataDictionaryField(Person.class.getName(), Person.COL_GENDER_CODE);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addIn(X2BaseBean.COL_OID, m_studentsWithOSSRecords);

        // --- Use a count(*) aggregate grouping students on race view, hispanic latino flag, and
        // gender
        ColumnQuery studentQuery = new ColumnQuery(Student.class,
                new String[] {Student.REL_PERSON + "." + Person.COL_RACE_VIEW,
                        Student.REL_PERSON + "." + Person.COL_HISPANIC_LATINO_INDICATOR,
                        Student.REL_PERSON + "." + Person.COL_GENDER_CODE, "count(*) as student_count"},
                studentCriteria);
        studentQuery.addGroupBy(Student.REL_PERSON + "." + Person.COL_RACE_VIEW);
        studentQuery.addGroupBy(Student.REL_PERSON + "." + Person.COL_HISPANIC_LATINO_INDICATOR);
        studentQuery.addGroupBy(Student.REL_PERSON + "." + Person.COL_GENDER_CODE);

        // --- Populate the suspension counts map with values
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(studentQuery);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String raceView = (String) row[0];
                boolean hispanicLatinoInd = "1".equals(row[1]);
                String genderCode = (String) row[2];
                Integer studentCount = row[3] == null ? Integer.valueOf(0) : Integer.valueOf(row[3].toString());

                // Use the state gender code if assigned. Report relies on expected values of M and
                // F.
                if (genderCodeField.getReferenceTableOid() != null) {
                    String genderStateCode =
                            dictionary.findStateReferenceCode(genderCodeField.getReferenceTableOid(), genderCode);
                    if (!StringUtils.isEmpty(genderStateCode)) {
                        genderCode = genderStateCode;
                    }
                }
                if (hispanicLatinoInd) {
                    addToMapValue(suspensionCounts, HISPANIC_LATINO_PREFIX + genderCode, studentCount);
                } else if (!StringUtils.isEmpty(raceView)) {
                    if (raceView.contains(STRING_COMMA)) {
                        addToMapValue(suspensionCounts, MULTI_PREFIX + genderCode, studentCount);
                    } else {
                        String stateCode =
                                dictionary.findStateReferenceCode(raceCodeField.getReferenceTableOid(), raceView);
                        if (stateCode != null) {
                            addToMapValue(suspensionCounts, stateCode + STRING_UNDERSCORE + genderCode, studentCount);
                        }
                    }
                }

                addToMapValue(suspensionCounts, KEY_TOTAL, studentCount);
            }
        } finally {
            iterator.close();
        }

        addParameter(PARAM_SUSPENSION_COUNTS_MAP, suspensionCounts);
    }

    /**
     * Calculates an overall attendance percentage. The resulting value (a Double) is added as a
     * report parameter
     * for use by the format.
     *
     * Attendance percentage is calculated via [ totalDaysInAttendance / totalMembershipDays ].
     * totalDaysInAttendance is calculated via [ totalMembershipDays - totalAbsences ].
     *
     * totalMembershipDays and totalAbsences are obtained by summing the membership days from all of
     * the NYStudentEnrollmentSpans
     * which come from the NYEnrollmentHelper (m_enrollmentHelper) in the StudentStatistics inner
     * class.
     *
     * While iterating over the spans, also builds out maps of student unexcused absences and OSS
     * records used in the
     * addSuspensionCounts and addUnexcusedAbsenceCounts methods.
     *
     * @throws X2BaseException exception
     */
    private void processStudentEnrollmentData() throws X2BaseException {
        Map<String, Object> parametersMap = new HashMap<String, Object>();
        School selectedSchool = getSchool();

        StudentStatistics statistics = new StudentStatistics();
        statistics.setBroker(getBroker());
        statistics.setCurrentContext(getCurrentContext());
        statistics.setPrivilegeSet(getPrivilegeSet());
        statistics.setOrganization(OrganizationManager.getRootOrganization(getOrganization()));
        statistics.setSchoolContext(isSchoolContext());
        statistics.setSchool(selectedSchool);
        statistics.setParameters(parametersMap);
        statistics.initializeExport();

        int totalMemberDays = 0;
        int totalAbsences = 0;

        // For each student returned by StudentHistoryHelper, add their total member days and
        // absences to the counts
        QueryIterator students =
                getBroker().getIteratorByQuery(statistics.getStudentHistoryHelper().getStudentQuery(false));

        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                String studentOid = student.getOid();
                List<NYStudentEnrollmentSpan> spans =
                        statistics.getStudentHistoryHelper().getNYStudentEnrollmentSpans(student, true);
                for (NYStudentEnrollmentSpan span : spans) {
                    SisSchool spanSchool = span.getSchool();
                    if (spanSchool != null && spanSchool.getOid().equals(selectedSchool.getOid())) {
                        totalMemberDays += span.getMembershipDays();
                        List<StudentAttendance> spanAttendances = span.getStudentAttendance();
                        for (StudentAttendance spanAttendance : spanAttendances) {
                            boolean isAbsence = spanAttendance.getAbsentIndicator();
                            boolean isExcused = spanAttendance.getExcusedIndicator();
                            boolean isOutOfSchoolSuspension = OSS_CODE.equals(spanAttendance.getOtherCode())
                                    || OSS_CODE.equals(spanAttendance.getOtherCode02());
                            if (isOutOfSchoolSuspension) {
                                m_studentsWithOSSRecords.add(studentOid);
                            }

                            if (isAbsence && !isExcused) {
                                addToMapValue(m_unexcusedAbsencesByStudent, studentOid, Integer.valueOf(1));
                            }
                        }
                        totalAbsences += span.getAttendanceDays();
                    }
                }
            }
        } finally {
            if (students != null) {
                students.close();
            }
        }

        // --- Calculate the attendance percentage
        Double attendancePercentage = Double.valueOf((totalMemberDays - totalAbsences) / (double) totalMemberDays);

        addParameter(PARAM_ATTENDANCE_PERCENTAGE, attendancePercentage);
        addParameter(PARAM_DISTRICT_CONTEXT, m_context);
    }

    /**
     * Populates a map of students with unexcused absences in 3 count thresholds: "10+", "5-9",
     * and "1-4". These values are obtained from m_unexcusedAbsencesByStudent which is loaded when
     * processStudentEnrollmentData is called. Adds map as parameter for usage in iReport format. s
     */
    private void addUnexcusedAbsenceCounts() {
        // initialize values to zero so that report still shows 0 when no students with absences are
        // found.
        Map<String, Integer> absenceCounts = new HashMap<>();
        addToMapValue(absenceCounts, KEY_10_PLUS, Integer.valueOf(0));
        addToMapValue(absenceCounts, KEY_5_TO_9, Integer.valueOf(0));
        addToMapValue(absenceCounts, KEY_1_TO_4, Integer.valueOf(0));
        for (String studentOid : m_unexcusedAbsencesByStudent.keySet()) {
            int absenceCount = m_unexcusedAbsencesByStudent.get(studentOid).intValue();

            if (absenceCount >= 10) {
                addToMapValue(absenceCounts, KEY_10_PLUS, Integer.valueOf(1));
            } else if (absenceCount >= 5) {
                addToMapValue(absenceCounts, KEY_5_TO_9, Integer.valueOf(1));
            } else {
                addToMapValue(absenceCounts, KEY_1_TO_4, Integer.valueOf(1));
            }
        }

        addParameter(PARAM_UNEXCUSED_ABSENCE_COUNTS, absenceCounts);
    }

    /**
     * Increments the value associated with the passed key in the passed map.
     *
     * @param map Map<String,Integer>
     * @param key String
     * @param valueToAdd Integer
     */
    private void addToMapValue(Map<String, Integer> map, String key, Integer valueToAdd) {
        Integer value = map.get(key);
        if (value == null) {
            value = valueToAdd;
        } else {
            value = Integer.valueOf(value.intValue() + valueToAdd.intValue());
        }

        map.put(key, value);
    }

    /**
     * State Report Data class for gathering data, using NYEnrollmentHelper, calculating enrollment
     * history.
     *
     */
    class StudentStatistics extends StateReportData {
        /**
         * Instance variables.
         */
        private NYEnrollmentHelper m_enrollmentHelper;

        /**
         * Initialize the export.
         * Set up the student history helper.
         */
        @Override
        public void initialize() {

            m_enrollmentHelper = new NYEnrollmentHelper(m_context.getStartDate(), m_context.getEndDate(), this);
            setCurrentContext((DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                    m_contextOid));
            setQuery(m_enrollmentHelper.getStudentHistoryHelper().getStudentQuery(false));
        }

        /**
         * Gets the student history helper.
         *
         * @return NY student history helper
         */
        public NYStudentHistoryHelper getStudentHistoryHelper() {
            return m_enrollmentHelper.getStudentHistoryHelper();
        }
    }
}
