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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.ca.ADADataHelper.ADADetailItem;
import com.x2dev.procedures.statereporting.ca.ADADataHelper.ADADetailIterator;
import com.x2dev.procedures.statereporting.ca.CAStudentAttendanceSummary.CAStudentAttendanceFileEntity.BeanRow;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

/**
 * Export procedure for Average Daily Attendance File.
 *
 * @author X2 Development Corporation
 */
public class CAStudentAttendanceSummary extends StateReportData {

    /**
     * Entity class for Student Attendance File export.
     *
     */
    public static class CAStudentAttendanceFileEntity extends StateReportEntity {
        /**
         * Helper class to save current row for bean.
         *
         * @author Follett Software Company
         */
        class BeanRow {
            private SisSchool m_school;
            private DetailItem m_detailItem;

            /**
             * Instantiates a new bean row.
             *
             * @param school SisSchool
             * @param detailItem DetailItem
             */
            public BeanRow(SisSchool school, DetailItem detailItem) {
                this.m_school = school;
                this.m_detailItem = detailItem;
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
             * Gets the detail item.
             *
             * @return the m_detailItem
             */
            public DetailItem getDetailItem() {
                return m_detailItem;
            }
        }

        List<BeanRow> m_rows;

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_rows = new ArrayList<BeanRow>();

            CAStudentAttendanceSummary exportData = (CAStudentAttendanceSummary) data;
            SisStudent student = (SisStudent) bean;

            Map<String, DetailItem> mapForBean = exportData.m_data.get(student.getOid());

            if (mapForBean != null) {

                for (Entry<String, DetailItem> entry : mapForBean.entrySet()) {
                    SisSchool school = (SisSchool) exportData.getBroker().getBeanByOid(SisSchool.class, entry.getKey());

                    BeanRow row = new BeanRow(school, entry.getValue());
                    m_rows.add(row);
                }
            }

            setRowCount(m_rows.size());
        }

        /**
         * Gets the current bean row.
         *
         * @return the m_combinations
         */
        public BeanRow getCurrentBeanRow() {
            return m_rows.get(getCurrentRow());
        }
    }

    /**
     * Helper class to save all necessary information for the given student.
     *
     * @author Follett Software Company
     */
    class DetailItem {

        /**
         * Constants
         */
        private static final String ATT_CODE_ASY = "ASY";
        private static final String ATT_CODE_SYN = "SYN";
        private static final String ATT_REASON_IA = "IA";
        private static final String INDEPENDENT_STUDY_CODE = "I";
        private static final String NOT_ENROLLED_CODE = "NE";
        private static final String SCHOOL_CLOSED_CODE = "KC";
        private static final String SCHOOL_NAME_INDEP_STUDY = "Independent Studies";

        private List<StudentEnrollmentSpan> m_stdSpans;
        private SisStudent m_student;
        private int m_numAbsent;
        private int m_numAdaGenIndepDays;
        private int m_numExcused;
        private int m_numExpectedDays;
        private int m_numNonAdaGenIndepDays;
        private int m_numSuspendedIn;
        private int m_numSuspendedOut;
        private int m_numUnExcused;

        /**
         * Constructor.
         *
         * @param student SisStudent
         * @param stdSpans List<StudentEnrollmentSpan>
         */
        public DetailItem(SisStudent student, List<StudentEnrollmentSpan> stdSpans) {
            this.m_student = student;
            this.m_stdSpans = stdSpans;
        }

        /**
         * Gets the num absent.
         *
         * @return the m_numAbsent
         */
        public int getNumAbsent() {
            return m_numAbsent;
        }

        /**
         * Gets the num ADA-Generating Independent Study Days.
         *
         * @return the m_numAdaGenIndepDays
         */
        public int getNumAdaGenIndepDays() {
            return m_numAdaGenIndepDays;
        }

        /**
         * Gets the num days attended.
         *
         * @return double
         */
        public double getNumDaysAttended() {
            return getNumExpectedDays() - getNumAbsent();
        }

        /**
         * Gets the num excused.
         *
         * @return the m_numExcused
         */
        public int getNumExcused() {
            return m_numExcused;
        }

        /**
         * Gets the num expected days.
         *
         * @return the m_numExpectedDays
         */
        public int getNumExpectedDays() {
            return m_numExpectedDays;
        }

        /**
         * Gets the num Non-ADA Generating Independent Study Days.
         *
         * @return the m_numNonAdaGenIndepDays
         */
        public int getNumNonAdaGenIndepDays() {
            return m_numNonAdaGenIndepDays;
        }

        /**
         * Gets the num of in school suspended.
         *
         * @return the m_numSuspended
         */
        public int getNumSuspendedIn() {
            return m_numSuspendedIn;
        }

        /**
         * Gets the num of out of school suspended.
         *
         * @return the m_numSuspended
         */
        public int getNumSuspendedOut() {
            return m_numSuspendedOut;
        }

        /**
         * Gets the num un excused.
         *
         * @return the m_numUnExcused
         */
        public int getNumUnExcused() {
            return m_numUnExcused;
        }

        /**
         * Gets the std spans.
         *
         * @return the m_stdSpans
         */
        public List<StudentEnrollmentSpan> getStdSpans() {
            return m_stdSpans;
        }

        /**
         * Gets the student.
         *
         * @return the m_student
         */
        public SisStudent getStudent() {
            return m_student;
        }

        /**
         * Increment.
         *
         * @param item ADADetailItem
         */
        public void increment(ADADetailItem item) {
            String attCode = item.getAttendanceCode();
            StudentAttendance att = item.getAttendance();
            String attReason = att != null ? att.getReasonCode() : null;
            String attOtherCode = att != null ? att.getOtherCode() : null;
            BigDecimal portionAbsent = att != null ? att.getPortionAbsent() : null;
            String schoolName = item.getSchool() != null ? item.getSchool().getName() : null;
            if (!item.getAttendanceCode().equals(SCHOOL_CLOSED_CODE) && !NOT_ENROLLED_CODE.equals(attCode)) {
                m_numExpectedDays += 1;
                m_numAbsent += item.isAbsent() ? 1 : 0;
                m_numExcused += item.isAbsent() && !item.isSuspended() && item.isExcused() ? 1 : 0;
                m_numUnExcused += item.isAbsent() && !item.isSuspended() && !item.isTruant()
                        && !item.isExcused() ? 1 : 0;

                m_numSuspendedIn += item.isSuspended() && "IS".equals(attReason) ? 1 : 0;
                m_numSuspendedOut += item.isSuspended() && "SU".equals(attReason) ? 1 : 0;
            }
            if (att != null) {
                if (!att.getAbsentIndicator()) {
                    if (ATT_REASON_IA.equals(attReason) && INDEPENDENT_STUDY_CODE.equals(attCode)
                            || BIG_DECIMAL_01.compareTo(portionAbsent) > 0
                                    && (ATT_CODE_ASY.equals(attCode) || ATT_CODE_ASY.equals(attOtherCode)
                                            || ATT_CODE_SYN.equals(attCode) || ATT_CODE_SYN.equals(attOtherCode))) {
                        m_numAdaGenIndepDays += 1;
                    }
                } else {
                    if ((ATT_REASON_IA.equals(attReason) || SCHOOL_NAME_INDEP_STUDY.equals(schoolName))
                            && BIG_DECIMAL_01.compareTo(portionAbsent) < 1) {
                        m_numNonAdaGenIndepDays += 1;
                    }
                }
            }
        }
    }

    /**
     * Helper class to be the key for the data map.
     *
     * @author Follett Software Company
     */
    class DetailItemKey implements Comparable<DetailItemKey> {
        private SisSchool m_school;
        private SisStudent m_student;

        /**
         * Instantiates a new detail item key.
         *
         * @param student SisStudent
         * @param school SisSchool
         */
        public DetailItemKey(SisStudent student, SisSchool school) {
            this.m_student = student;
            this.m_school = school;
        }

        /**
         * Compare to.
         *
         * @param other DetailItemKey
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(DetailItemKey other) {
            int value = m_student.getNameView().compareTo(other.getStudent().getNameView());
            if (value == 0) {
                value = m_student.getOid().compareTo(other.getStudent().getOid());
            }
            if (value == 0) {
                value = m_school.getOid().compareTo(other.getSchool().getOid());
            }

            return value;
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
            return compareTo((DetailItemKey) obj) == 0 ? true : false;
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_student.getOid().hashCode() + m_school.getOid().hashCode();
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
         * Gets the student.
         *
         * @return the m_student
         */
        public SisStudent getStudent() {
            return m_student;
        }
    }

    /**
     * Retrieve a value from the attendance record.
     */
    protected class RetrieveAttendanceData implements FieldRetriever {
        public static final String CALCULATION_ID = "ATT-DATA";

        private static final String CALC_PARAM_ABS_DAYS_EXC_NSS = "ABS_DAYS_EXC_NSS";
        private static final String CALC_PARAM_ABS_DAYS_OSS = "ABS_DAYS_OSS";
        private static final String CALC_PARAM_ABS_DAYS_UNEXC_NSS = "ABS_DAYS_UNEXC_NSS";
        private static final String CALC_PARAM_ADA_GEN_INDEP_STUDY = "ADA_GEN_INDEP_STUDY";
        private static final String CALC_PARAM_ATT_DAYS_ISS = "ATT_DAYS_ISS";
        private static final String CALC_PARAM_ATT_EXPECTED = "EXPECTED_ATT_DAYS";
        private static final String CALC_PARAM_ATT_METHOD_IND = "ATT_METHOD_IND";
        private static final String CALC_PARAM_ATT_SCHOOL = "ATT_SCHOOL";
        private static final String CALC_PARAM_DAYS_ATTENDED = "DAYS_ATTENDED";
        private static final String CALC_PARAM_NON_ADA_GEN_INDEP_STUDY = "NON_ADA_GEN_INDEP_STUDY";

        private static final String GRADE_LEVEL_07 = "07";
        private static final String GRADE_LEVEL_08 = "08";

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
            CAStudentAttendanceFileEntity attEntity = (CAStudentAttendanceFileEntity) entity;
            BeanRow currentRow = attEntity.getCurrentBeanRow();

            if (CALC_PARAM_ATT_EXPECTED.equals(param)) {
                DetailItem item = null;
                if ((item = currentRow.getDetailItem()) != null) {
                    value = BigDecimal.valueOf(item.getNumExpectedDays());
                }
            } else if (CALC_PARAM_DAYS_ATTENDED.equals(param)) {
                DetailItem item = null;
                if ((item = currentRow.getDetailItem()) != null) {
                    int daysToSubtract = item.getNumAdaGenIndepDays() + item.getNumNonAdaGenIndepDays();
                    double daysAttend = item.getNumDaysAttended();
                    value = BigDecimal.valueOf(daysAttend - daysToSubtract);
                }
            } else if (CALC_PARAM_ABS_DAYS_OSS.equals(param)) {
                DetailItem item = null;
                if ((item = currentRow.getDetailItem()) != null) {
                    value = BigDecimal.valueOf(item.getNumSuspendedOut());
                }
            } else if (CALC_PARAM_ATT_DAYS_ISS.equals(param)) {
                DetailItem item = null;
                if ((item = currentRow.getDetailItem()) != null) {
                    value = BigDecimal.valueOf(item.getNumSuspendedIn());
                }
            } else if (CALC_PARAM_ABS_DAYS_EXC_NSS.equals(param)) {
                DetailItem item = null;
                if ((item = currentRow.getDetailItem()) != null) {
                    value = BigDecimal.valueOf(item.getNumExcused());
                }
            } else if (CALC_PARAM_ABS_DAYS_UNEXC_NSS.equals(param)) {
                DetailItem item = null;
                if ((item = currentRow.getDetailItem()) != null) {
                    value = BigDecimal.valueOf(item.getNumUnExcused());
                }
            } else if (CALC_PARAM_ADA_GEN_INDEP_STUDY.equals(param)) {
                DetailItem item = null;
                if ((item = currentRow.getDetailItem()) != null) {
                    value = BigDecimal.valueOf(item.getNumAdaGenIndepDays());
                }
            } else if (CALC_PARAM_NON_ADA_GEN_INDEP_STUDY.equals(param)) {
                DetailItem item = null;
                if ((item = currentRow.getDetailItem()) != null) {
                    value = BigDecimal.valueOf(item.getNumNonAdaGenIndepDays());
                }
            } else if (CALC_PARAM_ATT_METHOD_IND.equals(param)) {
                DetailItem item = null;
                if ((item = currentRow.getDetailItem()) != null) {
                    String gradeLevel = item.getStudent().getGradeLevel();
                    value = GRADE_LEVEL_07.equals(gradeLevel) || GRADE_LEVEL_08.equals(gradeLevel)
                            ? Boolean.TRUE
                            : Boolean.FALSE;
                }
            } else if (CALC_PARAM_ATT_SCHOOL.equals(param)) {
                value = currentRow.getSchool().getFieldValueByAlias(ALIAS_SCHOOL_ID);
            }

            return value;
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_SCHOOL_EXCLUDE = "DOE EXCLUDE SKL";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";

    /**
     * Constants
     */
    protected static final BigDecimal BIG_DECIMAL_01 = BigDecimal.valueOf(0.01);
    protected static final BigDecimal BIG_DECIMAL_99 = BigDecimal.valueOf(0.99);

    /**
     * Input Definition Parameters
     */
    protected static final String PARAM_DATE_END = "startDate";
    protected static final String PARAM_DATE_START = "endDate";
    protected static final String PARAM_SCHOOL_YEAR_CONTEXT = "schoolYearContext";
    protected static final String PARAM_SKL_EXCLUDE = "excludeSchool";

    /**
     * Fields parameters
     */
    private static final String PRC_ID_ADA_DETAIL = "EXPDATA-CA-ADAD";

    /**
     * Members
     */
    protected TreeMap<String, Map<String, DetailItem>> m_data = new TreeMap<String, Map<String, DetailItem>>();
    protected ADADataHelper m_dataHelper;
    protected PlainDate m_endDate;
    protected String m_excludeSchool;
    protected Map m_excludeSchoolMap;
    protected PlainDate m_startDate;


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

        m_startDate = getCurrentContext().getStartDate();
        m_endDate = getCurrentContext().getEndDate();
        m_excludeSchool = translateAliasToJavaName(ALIAS_SCHOOL_EXCLUDE, true);

        if (((Boolean) getParameter(PARAM_SKL_EXCLUDE)).booleanValue()) {
            loadSchoolExcludeMap();
        }

        Map<String, Object> params = new HashMap<String, Object>();
        params.put("startDate", m_startDate);
        params.put("endDate", m_endDate);

        int queryCount = 1;
        String queryBy = (String) getParameter(PARAM_QUERY_BY_FIELD + Integer.toString(queryCount));
        String queryString = (String) getParameter(PARAM_QUERY_BY_CRITERIA + Integer.toString(queryCount));
        while (!StringUtils.isEmpty(queryBy) && !StringUtils.isEmpty(queryString)) {
            params.put(PARAM_QUERY_BY_FIELD + Integer.toString(queryCount), queryBy);
            params.put(PARAM_QUERY_BY_CRITERIA + Integer.toString(queryCount), queryString);
            queryCount++;
            queryBy = (String) getParameter(PARAM_QUERY_BY_FIELD + Integer.toString(queryCount));
            queryString = (String) getParameter(PARAM_QUERY_BY_CRITERIA + Integer.toString(queryCount));
        }


        m_dataHelper = new ADADataHelper(getBroker(), getOrganization());
        m_dataHelper.setCycle(m_startDate, null, null, null, false);

        m_dataHelper.initialize(getPrivilegeSet(), isSchoolContext(), getSchool(), params, getUser(),
                PRC_ID_ADA_DETAIL);

        loadInputData();

        setEntityClass(CAStudentAttendanceFileEntity.class);

        StudentHistoryHelper studentHelper = new StudentHistoryHelper(this);
        studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);
        studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);

        this.setQuery(studentHelper.getStudentQuery(false));

        // Build and attach retrievers
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveAttendanceData.CALCULATION_ID, new RetrieveAttendanceData());
        super.addCalcs(calcs);

    }

    /**
     * Add detail item to the data map and increment it.
     *
     * @param item ADADetailItem
     */
    private void addDetailItem(ADADetailItem item) {
        String stdKey = item.getStudent().getOid();
        String sklKey = item.getSchool().getOid();

        if (includeSchool(sklKey)) {
            Map<String, DetailItem> mapByStdKey = m_data.get(stdKey);

            if (mapByStdKey == null) {
                mapByStdKey = new HashMap<String, CAStudentAttendanceSummary.DetailItem>();

                DetailItem newDetailItem = new DetailItem(item.getStudent(), item.getStdSpans());
                newDetailItem.increment(item);
                mapByStdKey.put(sklKey, newDetailItem);
                m_data.put(stdKey, mapByStdKey);
            } else {
                DetailItem detailItem = mapByStdKey.get(sklKey);

                if (detailItem == null) {
                    detailItem = new DetailItem(item.getStudent(), item.getStdSpans());
                    mapByStdKey.put(sklKey, detailItem);
                }
                detailItem.increment(item);
            }
        }
    }

    /**
     * Include school.
     *
     * @param sklOid String
     * @return true, if successful
     */
    private boolean includeSchool(String sklOid) {
        boolean include = true;

        if (isSchoolContext() && !getSchool().getOid().equals(sklOid)) {
            include = false;
        }

        if (m_excludeSchoolMap != null && m_excludeSchoolMap.keySet().contains(sklOid)) {
            include = false;
        }

        return include;
    }

    /**
     * Load data and populate data map with necessary values.
     *
     * @throws X2BaseException exception
     */
    private void loadInputData() throws X2BaseException {
        if (m_dataHelper != null) {
            ADADetailIterator iterator = m_dataHelper.iterator();
            if (iterator != null) {
                try {
                    ADADetailItem item = null;
                    while ((item = iterator.next()) != null) {
                        addDetailItem(item);
                    }
                } finally {
                    iterator.close();
                }
            }
        }
    }

    /**
     * Build a map of schools with the alias set to exclude school.
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);

        BeanQuery query = new BeanQuery(School.class, schoolCriteria);

        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }

}
