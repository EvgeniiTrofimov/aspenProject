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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.procedures.statereporting.ny.NYEnrollmentHelper;
import com.x2dev.procedures.statereporting.ny.NYEnrollmentHelper.NYStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.ny.NYEnrollmentHelper.NYStudentHistoryHelper;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "NY SA129 Detail" report.
 *
 * @author X2 Development Corporation
 */
public class NYSA129DetailData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    /**
     * Input definition parameter names.
     */
    public static final String PARAM_END_DATE = "endDate";
    public static final String PARAM_START_DATE = "startDate";

    /**
     * Grid column constants
     */
    protected static final String FIELD_COLUMN_DATE_END = "dateEnd";
    protected static final String FIELD_COLUMN_DATE_START = "dateStart";
    protected static final String FIELD_COLUMN_STUDENT_ID = "studentID";
    protected static final String FIELD_COLUMN_STUDENT_NAME = "studentName";
    protected static final String FIELD_COLUMN_STUDENT_SCHOOL_NAME = "schoolName";
    protected static final String FIELD_COLUMN_DAYS_ATTENDANCE = "daysAttendance";
    protected static final String FIELD_COLUMN_DAYS_MEMBERSHIP = "daysMembership";
    protected static final String FIELD_COLUMN_GRADE = "grade";

    private static final String ALIAS_NUMERIC_GRADE = "NumericGradeLevel";
    private static final String ALIAS_SA129_SKL_EXCLUDE = "all-skl-SA129Exclude";
    private static final String EXTENDED_DICTIONARY_ID_GRADE_NUMERIC = "REF-GRADE-LEVELS";

    /**
     * Variables
     */
    protected PlainDate m_endDate = null;
    protected PlainDate m_startDate = null;
    protected DistrictSchoolYearContext m_context;
    private StudentStatistics m_statistics;
    private HashSet<String> m_excludedSchools = new HashSet<String>();
    protected Map<Integer, String> m_numericStateGradeMap;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {

        X2Broker broker = getBroker();
        initializeFields();
        loadExcludedSchools(broker);

        m_statistics = new StudentStatistics();
        m_statistics.setBroker(broker);
        m_statistics.setPrivilegeSet(getPrivilegeSet());
        m_statistics.setOrganization(getOrganization());
        m_statistics.setSchoolContext(false);
        m_statistics.setParameters(getParameters());
        m_statistics.initializeExport();
        ReportDataGrid grid = populateGrid();

        return grid;
    }

    /**
     * Initialize Fields.
     *
     * @return true, if successful
     */
    protected boolean initializeFields() {
        boolean haveErrors = false;

        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);

        if (m_startDate.before(m_endDate)) {
            X2Criteria ctxCriteria = new X2Criteria();
            ctxCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_startDate);
            ctxCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_startDate);

            m_context = (DistrictSchoolYearContext) getBroker()
                    .getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, ctxCriteria));
        } else {
            haveErrors = true;
        }

        return haveErrors;
    }

    /**
     * @param string
     * @return
     */
    private String getGradeLevelFromNumeric(String gradeNumberString) {
        if (m_numericStateGradeMap == null) {
            m_numericStateGradeMap = new HashMap();

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExtendedDataDictionary.COL_ID, EXTENDED_DICTIONARY_ID_GRADE_NUMERIC);
            ExtendedDataDictionary extendedDictionary = (ExtendedDataDictionary) getBroker()
                    .getBeanByQuery(new QueryByCriteria(ExtendedDataDictionary.class, criteria));

            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());
            DataDictionaryField gradeLevelField = dictionary.findDataDictionaryField(Student.class.getName(),
                    Student.COL_GRADE_LEVEL);

            DataDictionaryField numericGradeLevelField = dictionary.findDataDictionaryFieldByAlias(ALIAS_NUMERIC_GRADE);
            if (gradeLevelField.hasReferenceTable()) {
                for (ReferenceCode code : gradeLevelField.getReferenceTable().getReferenceCodes(getBroker())) {
                    if (!code.getDisabledIndicator()) {
                        String numericString =
                                (String) code.getFieldValueByBeanPath(numericGradeLevelField.getJavaName());

                        if (!StringUtils.isEmpty(numericString) && !StringUtils.isEmpty(code.getStateCode())) {
                            int numericValue = 1000;
                            try {
                                numericValue = Integer.parseInt(numericString);
                            } catch (NumberFormatException e) {
                                // Fail on format exception
                            }
                            if (numericValue < 1000) {
                                m_numericStateGradeMap.put(Integer.valueOf(numericValue), code.getStateCode());
                            }
                        }
                    }
                }
            }

        }

        String gradeLevel = "";
        if (!StringUtils.isEmpty(gradeNumberString)) {
            int numericValue = 1000;
            try {
                numericValue = Integer.parseInt(gradeNumberString);
            } catch (NumberFormatException e) {
                // Fail on format exception
            }
            if (numericValue < 1000) {
                Integer key = Integer.valueOf(numericValue);
                if (m_numericStateGradeMap.containsKey(key)) {
                    gradeLevel = m_numericStateGradeMap.get(key);
                }
            }
        }
        return gradeLevel;
    }

    /**
     * Loads additional schools to be excluded specific to the NYSA 129 reports.
     *
     * @param broker X2Broker
     */
    private void loadExcludedSchools(X2Broker broker) {
        X2Criteria excludeSchoolCriteria = new X2Criteria();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
        DataDictionaryField schoolExclusionField = dictionary.findDataDictionaryFieldByAlias(ALIAS_SA129_SKL_EXCLUDE);
        if (schoolExclusionField != null) {
            excludeSchoolCriteria.addEqualTo(schoolExclusionField.getJavaName(), BooleanAsStringConverter.TRUE);

            QueryByCriteria excludedSchoolQuery = new QueryByCriteria(SisSchool.class, excludeSchoolCriteria);
            QueryIterator schools = broker.getIteratorByQuery(excludedSchoolQuery);
            try {
                while (schools.hasNext()) {
                    SisSchool school = (SisSchool) schools.next();
                    m_excludedSchools.add(school.getOid());
                }
            } finally {
                if (schools != null) {
                    schools.close();
                }
            }
        }
    }

    /**
     * Populate Report Grid.
     *
     * @return ReportDataGrid
     */
    private ReportDataGrid populateGrid() {
        X2Broker broker = getBroker();
        int schoolYear = m_context.getSchoolYear();
        String delimiter = ",";
        // Key for both maps = concatenation of studentOid, schoolOid, and the grade level for the
        // span using a delimiter
        Map<String, Integer> membershipDaysMap = new HashMap<>();
        Map<String, Integer> absentDaysMap = new HashMap<>();

        ReportDataGrid grid = new ReportDataGrid(10000, 20);

        QueryIterator students =
                broker.getIteratorByQuery(m_statistics.getStudentHistoryHelper().getStudentQuery(false));

        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                List<NYStudentEnrollmentSpan> spans =
                        m_statistics.getStudentHistoryHelper().getNYStudentEnrollmentSpans(student, true);
                for (NYStudentEnrollmentSpan span : spans) {
                    SisSchool spanSchool = span.getSchool();
                    if (spanSchool != null && !m_excludedSchools.contains(spanSchool.getOid())) {
                        if (isSchoolContext()) {
                            School selectedSchool = getSchool();
                            if (selectedSchool != null && selectedSchool.equals(span.getSchool())) {
                                processSpan(schoolYear, delimiter, membershipDaysMap, absentDaysMap, student, span,
                                        spanSchool);
                            }
                        } else {
                            processSpan(schoolYear, delimiter, membershipDaysMap, absentDaysMap, student, span,
                                    spanSchool);
                        }
                    }
                }
            }
        } finally {
            if (students != null) {
                students.close();
            }
        }
        // Create the grid based on the membership days map. Each entry represents a unique row for
        // a student/school/grade.
        if (!membershipDaysMap.isEmpty()) {
            for (Map.Entry<String, Integer> entry : membershipDaysMap.entrySet()) {
                Integer membershipDays = entry.getValue();
                if (membershipDays.intValue() > 0) {
                    String key = entry.getKey();
                    String[] keyValues = key.split(delimiter);
                    String studentOid = keyValues[0];
                    String schoolOid = keyValues[1];
                    String gradeLevel = getGradeLevelFromNumeric(keyValues[2]);

                    SisStudent student = (SisStudent) broker.getBeanByOid(SisStudent.class, studentOid);
                    SisSchool school = (SisSchool) broker.getBeanByOid(SisSchool.class, schoolOid);
                    String schoolName = "";
                    if (school != null) {
                        schoolName = school.getName();
                    }

                    // Look to see if there are any absences for this row
                    Integer absentDays = Integer.valueOf(0);
                    if (absentDaysMap.containsKey(key)) {
                        absentDays = absentDaysMap.get(key);
                    }

                    grid.append();
                    grid.set(FIELD_COLUMN_DATE_END, m_endDate);
                    grid.set(FIELD_COLUMN_DATE_START, m_startDate);
                    grid.set(FIELD_COLUMN_STUDENT_SCHOOL_NAME, schoolName);
                    grid.set(FIELD_COLUMN_GRADE, gradeLevel);
                    grid.set(FIELD_COLUMN_STUDENT_ID, student.getLocalId());
                    grid.set(FIELD_COLUMN_STUDENT_NAME, student.getNameView());
                    grid.set(FIELD_COLUMN_DAYS_ATTENDANCE,
                            Integer.valueOf(membershipDays.intValue() - absentDays.intValue()));
                    grid.set(FIELD_COLUMN_DAYS_MEMBERSHIP, membershipDays);
                    grid.beforeTop();
                }
            }
        }

        grid.sort(FIELD_COLUMN_STUDENT_NAME, false);
        grid.sort(FIELD_COLUMN_GRADE, false);
        grid.sort(FIELD_COLUMN_STUDENT_SCHOOL_NAME, false);

        return grid;
    }

    /**
     * Process span.
     *
     * @param schoolYear int
     * @param delimiter String
     * @param membershipDaysMap Map<String,Integer>
     * @param absentDaysMap Map<String,Integer>
     * @param student SisStudent
     * @param span NYStudentEnrollmentSpan
     * @param spanSchool SisSchool
     */
    private void processSpan(int schoolYear,
                             String delimiter,
                             Map<String, Integer> membershipDaysMap,
                             Map<String, Integer> absentDaysMap,
                             SisStudent student,
                             NYStudentEnrollmentSpan span,
                             SisSchool spanSchool) {
        int spanGrade = 12 - (span.getYog() - schoolYear);
        // Grade level is formatted as a 2 digit decimal. This is so that we can properly sort 9
        // before 10.
        String key = student.getOid() + delimiter + spanSchool.getOid() + delimiter
                + String.format("%02d", Integer.valueOf(spanGrade));
        addMembershipDaysToMap(membershipDaysMap, span, key);
        addAbsentDaysToMap(absentDaysMap, span, key);
    }

    /**
     * Adds the absent days to map.
     *
     * @param absentDaysMap Map<String,Integer>
     * @param span NYStudentEnrollmentSpan
     * @param key String
     */
    private void addAbsentDaysToMap(Map<String, Integer> absentDaysMap, NYStudentEnrollmentSpan span, String key) {
        if (!absentDaysMap.containsKey(key)) {
            absentDaysMap.put(key, Integer.valueOf(span.getAttendanceDays(m_startDate, m_endDate)));
        } else {
            Integer currentCount = absentDaysMap.get(key);
            Integer newCount = Integer.valueOf(currentCount.intValue() + span.getAttendanceDays(m_startDate, m_endDate));
            absentDaysMap.put(key, newCount);
        }
    }

    /**
     * Adds the membership days to map.
     *
     * @param membershipDaysMap Map<String,Integer>
     * @param span NYStudentEnrollmentSpan
     * @param key String
     */
    private void addMembershipDaysToMap(Map<String, Integer> membershipDaysMap,
                                        NYStudentEnrollmentSpan span,
                                        String key) {
        if (!membershipDaysMap.containsKey(key)) {
            membershipDaysMap.put(key, Integer.valueOf(span.getMembershipDays(m_startDate, m_endDate)));
        } else {
            Integer currentValue = membershipDaysMap.get(key);
            Integer newValue = Integer.valueOf(currentValue.intValue() + span.getMembershipDays(m_startDate, m_endDate));
            membershipDaysMap.put(key, newValue);
        }
    }

    /**
     * State Report Data class for gathering data, using NYEnrollmentHelper, calculating enrollment
     * history.
     * This export should report a row for each student/school/grade combination.
     * It should report positive attendance, and report positive codes for each day in membership.
     * Days not in membership should be reported as empty.
     *
     * @author Follett Development Corporation
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

            m_enrollmentHelper = new NYEnrollmentHelper(m_startDate, m_endDate, this);
            setCurrentContext(m_context);

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
