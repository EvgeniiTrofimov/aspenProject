/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.quickletter;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.TempTable;
import com.follett.fsc.core.framework.persistence.adjusters.DistinctAdjuster;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.procedures.QuickLetterData;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;

/**
 * Class which creates the data source for Quick Letter - Attendance.
 */
public class QuickLetterAttendanceData extends QuickLetterData {
    // Attribute (A, T, D) query parameters
    private static final String ATTRIBUTE_CONNECTOR_PARAM_TARDY = "attribute_connector_t";
    private static final String ATTRIBUTE_CONNECTOR_PARAM_DISMISSED = "attribute_connector_d";
    private static final String ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_ABSENT = "attribute_excludeExcused_a";
    private static final String ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_TARDY = "attribute_excludeExcused_t";
    private static final String ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_DISMISSED = "attribute_excludeExcused_d";
    private static final String ATTRIBUTE_OPERATOR_PARAM_ABSENT = "attribute_operator_a";
    private static final String ATTRIBUTE_OPERATOR_PARAM_TARDY = "attribute_operator_t";
    private static final String ATTRIBUTE_OPERATOR_PARAM_DISMISSED = "attribute_operator_d";
    private static final String ATTRIBUTE_REASON_PARAM_ABSENT = "attribute_reason_a";
    private static final String ATTRIBUTE_REASON_PARAM_TARDY = "attribute_reason_t";
    private static final String ATTRIBUTE_REASON_PARAM_DISMISSED = "attribute_reason_d";
    private static final String ATTRIBUTE_VALUE_PARAM_ABSENT = "attribute_value_a";
    private static final String ATTRIBUTE_VALUE_PARAM_TARDY = "attribute_value_t";
    private static final String ATTRIBUTE_VALUE_PARAM_DISMISSED = "attribute_value_d";

    // Code query parameters
    private static final String CODE_CONNECTOR_PARAM_PREFIX = "code_connector_";
    private static final String CODE_EXCLUDE_EXCUSED_PARAM_PREFIX = "code_excludeExcused_";
    private static final String CODE_OPERATOR_PARAM_PREFIX = "code_operator_";
    private static final String CODE_PARAM_PREFIX = "code_";
    private static final String CODE_REASON_PARAM_PREFIX = "code_reason_";
    private static final String CODE_VALUE_PARAM_PREFIX = "code_value_";

    // All other parameters
    private static final int CODE_COUNT = 4;
    private static final String END_DATE_PARAM = "endDate";
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String START_DATE_PARAM = "startDate";

    /**
     * Calculation cache, lazy loaded in getCalculationValue() if used
     *
     * <calculation key, <student OID, count>>
     */
    private Map<String, Map<String, Integer>> m_cache = new HashMap<String, Map<String, Integer>>();

    private String[] m_codes = new String[CODE_COUNT];
    private boolean m_criteriaAdded = false;
    private String m_endDate = null;
    private String m_startDate = null;

    /**
     * Gets the available calculations.
     *
     * @return Map
     * @see com.follett.fsc.core.k12.tools.reports.QuickLetterData.getAvailableCalculations()
     */
    @Override
    public Map<String, String> getAvailableCalculations() {
        LinkedHashMap<String, String> calculationsById = new LinkedHashMap<String, String>();

        calculationsById.put("total_abs", "Total # of absences");
        calculationsById.put("total_tar", "Total # of tardies");
        calculationsById.put("total_dis", "Total # of dismissals");
        calculationsById.put("total_unex_abs", "Total # of unexcused absences");
        calculationsById.put("total_unex_tar", "Total # of unexcused tardies");
        calculationsById.put("total_unex_dis", "Total # of unexcused dismissals");
        calculationsById.put("total_other", "Total # of other codes");

        return calculationsById;
    }

    /**
     * Creates the query.
     *
     * @return BeanQuery
     * @see com.follett.fsc.core.k12.tools.procedures.QuickLetterData.createQuery()
     *
     *      Example sql generated for
     *
     *      - dates between 6/1/2008 and 6/26/2014
     *      - current student list
     *      - More than 5 medical absences between date range OR
     *      More than 10 sick tardies between date range
     *
     *      SELECT DISTINCT S0.*
     *      FROM STUDENT S0
     *      JOIN SELECTION_OBJECT ON STD_OID = SEO_OBJ_OID
     *      JOIN SCHOOL K0 ON STD_SKL_OID = K0.SKL_OID
     *      WHERE SKL_ORG_OID_1 = '*dst'
     *      AND SEO_SEL_OID = 'SEL000000Jg004'
     *      AND STD_OID IN (SELECT DISTINCT STD_OID
     *      FROM STUDENT
     *      WHERE EXISTS (SELECT ATT_STD_OID, COUNT(*) AS TOTAL_ABS
     *      FROM STUDENT_ATTENDANCE ALIAS_ABS
     *      WHERE ATT_DATE >= '2008-06-01'
     *      AND ATT_DATE <= '2014-06-26'
     *      AND STD_OID = ALIAS_ABS.ATT_STD_OID
     *      AND ATT_ABSENT_IND = '1'
     *      AND ATT_REASON_CODE = 'Medical'
     *      GROUP BY ATT_STD_OID
     *      HAVING SUM(ATT_PORTION_ABSENT) > 5)
     *      OR EXISTS (SELECT ATT_STD_OID, COUNT(*) AS TOTAL_TRD
     *      FROM STUDENT_ATTENDANCE ALIAS_TRD
     *      WHERE ATT_DATE >= '2008-06-01'
     *      AND ATT_DATE <= '2014-06-26'
     *      AND STD_OID = ALIAS_TRD.ATT_STD_OID
     *      AND ATT_TARDY_IND = '1'
     *      AND ATT_REASON_CODE = 'Sick'
     *      GROUP BY ATT_STD_OID
     *      HAVING COUNT(ATT_PORTION_ABSENT) > 10) )
     *      ORDER BY STD_NAME_VIEW
     */
    @Override
    protected BeanQuery createQuery() {
        ToolInput toolInput = getToolInput();

        int studentsToInclude = Integer.valueOf(toolInput.getParameterValue(QUERY_BY_PARAM)).intValue();
        boolean currentSelection = studentsToInclude == 0;
        boolean secondaryStudents =
                isSchoolContext() && Boolean.valueOf(PreferenceManager.getPreferenceValue(getSchool(),
                        SystemPreferenceDefinition.SECONDARY_STUDENTS_INCLUDED)).booleanValue() ||
                        !isSchoolContext() && Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                                SystemPreferenceDefinition.SECONDARY_STUDENTS_INCLUDED)).booleanValue();

        StringBuilder queryString = new StringBuilder(3000);

        /*
         * Build the outer query that defines the tables and fields involved. Only include
         * active students from the selected school or district.
         */
        queryString.append("SELECT DISTINCT S0.* ");
        queryString.append("FROM STUDENT S0 ");

        // JOIN - Current selection
        if (currentSelection) {
            queryString.append("JOIN " + getDatabaseSyntax().getTempTableName() + " ON STD_OID = "
                    + TempTable.DATABASE_STORAGE_OID + " ");
        }

        // JOIN - Primary school
        queryString.append("JOIN SCHOOL K0 ON STD_SKL_OID = K0.SKL_OID ");

        // JOIN - Secondary school
        if (secondaryStudents) {
            queryString.append("LEFT OUTER JOIN STUDENT_SCHOOL ON STD_OID = SSK_STD_OID ");

            if (!isSchoolContext()) {
                queryString.append("LEFT OUTER JOIN SCHOOL K1 ON SSK_SKL_OID = K1.SKL_OID ");
            }
        }

        queryString.append("WHERE ");

        // Scope for school
        if (isSchoolContext()) {
            queryString.append("(STD_SKL_OID = '");
            queryString.append(getSchool().getOid());
            queryString.append("' ");

            if (secondaryStudents) {
                queryString.append("OR (SSK_SKL_OID = '");
                queryString.append(getSchool().getOid());
                queryString.append("' ");

                queryString.append("AND SSK_CTX_OID = '");
                queryString.append(getCurrentContext().getOid());
                queryString.append("' ");

                queryString.append("AND SSK_ASSOCIATION_TYPE = 1 ");

                queryString.append(") ");
            }

            queryString.append(") ");
        } else {
            int level = getOrganization().getOrganizationDefinition().getLevel();
            queryString.append("(K0.SKL_ORG_OID_");
            queryString.append(level + 1);
            queryString.append(" = '");
            queryString.append(getOrganization().getOid());
            queryString.append("' ");

            if (secondaryStudents) {
                queryString.append("OR (K1.SKL_ORG_OID_");
                queryString.append(level + 1);
                queryString.append(" = '");
                queryString.append(getOrganization().getOid());
                queryString.append("' ");

                queryString.append("AND SSK_CTX_OID = '");
                queryString.append(getCurrentContext().getOid());
                queryString.append("' ");

                queryString.append("AND SSK_ASSOCIATION_TYPE = 1 ");

                queryString.append(") ");
            }

            queryString.append(") ");
        }

        // Scope for student
        switch (studentsToInclude) {
            case 1: // YOG
                queryString.append(" AND STD_YOG = " + getParameter(QUERY_STRING_PARAM) + " ");

                queryString.append(
                        "AND " + StudentManager.getActiveStudentDirectSQL(getOrganization(), "STD_ENROLLMENT_STATUS"));

                break;

            case 2: // Snapshot
                String subQuery = ReportUtils.getRecordSetSqlSubQuery(Student.DICTIONARY_ID,
                        (String) getParameter(QUERY_STRING_PARAM),
                        getUser(),
                        getSchool(),
                        getOrganization());
                queryString.append(" AND STD_OID IN (" + subQuery + ") ");
                break;

            default:
                // Current selection (join handles student scoping)
                break;
        }

        queryString.append("AND STD_OID IN (SELECT DISTINCT STD_OID FROM STUDENT");

        // Get list of active connectors
        LinkedList<Integer> activeConnectors = getActiveConnectors();

        /*
         * Add criteria one by one as necessary.
         */
        BigDecimal absentCount = (BigDecimal) getParameter(ATTRIBUTE_VALUE_PARAM_ABSENT);
        BigDecimal tardyCount = (BigDecimal) getParameter(ATTRIBUTE_VALUE_PARAM_TARDY);
        BigDecimal dismissalCount = (BigDecimal) getParameter(ATTRIBUTE_VALUE_PARAM_DISMISSED);

        if (absentCount != null) {
            boolean excused = ((Boolean) getParameter(ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_ABSENT)).booleanValue();
            int operator = ((Integer) getParameter(ATTRIBUTE_OPERATOR_PARAM_ABSENT)).intValue();
            String reason = (String) getParameter(ATTRIBUTE_REASON_PARAM_ABSENT);

            if (operator != 5) {
                appendSubQuery(queryString, activeConnectors.remove().intValue(), "ATT_ABSENT_IND", "1", reason,
                        operator, excused, absentCount.toString(), "ABS", false,
                        absentCount.compareTo(new BigDecimal(0.0)) == 0);
            }
        }

        if (tardyCount != null) {
            boolean excused = ((Boolean) getParameter(ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_TARDY)).booleanValue();
            int operator = ((Integer) getParameter(ATTRIBUTE_OPERATOR_PARAM_TARDY)).intValue();
            String reason = (String) getParameter(ATTRIBUTE_REASON_PARAM_TARDY);

            if (operator != 5) {
                appendSubQuery(queryString, activeConnectors.remove().intValue(), "ATT_TARDY_IND", "1", reason,
                        operator, excused, tardyCount.toString(), "TRD", false, tardyCount.intValue() == 0);
            }
        }

        if (dismissalCount != null) {
            boolean excused = ((Boolean) getParameter(ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_DISMISSED)).booleanValue();
            int operator = ((Integer) getParameter(ATTRIBUTE_OPERATOR_PARAM_DISMISSED)).intValue();
            String reason = (String) getParameter(ATTRIBUTE_REASON_PARAM_DISMISSED);

            if (operator != 5) {
                appendSubQuery(queryString, activeConnectors.remove().intValue(), "ATT_DISMISSED_IND", "1", reason,
                        operator, excused, dismissalCount.toString(), "DSM", false, dismissalCount.intValue() == 0);
            }
        }

        for (int i = 0; i < CODE_COUNT; i++) {
            if (!isEmptyValue(m_codes[i])) {
                boolean excused = ((Boolean) getParameter(CODE_EXCLUDE_EXCUSED_PARAM_PREFIX + i)).booleanValue();
                int operator = ((Integer) getParameter(CODE_OPERATOR_PARAM_PREFIX + i)).intValue();
                String reason = (String) getParameter(CODE_REASON_PARAM_PREFIX + i);
                BigDecimal value = (BigDecimal) getParameter(CODE_VALUE_PARAM_PREFIX + i);

                if (operator != 5) {
                    int connector = activeConnectors.remove().intValue();

                    appendSubQuery(queryString, connector, "ATT_OTHER_CODE", m_codes[i], reason,
                            operator, excused, value.toString(), String.valueOf(i), true, value.intValue() == 0);
                    appendSubQuery(queryString, 1, "ATT_OTHER_CODE_02", m_codes[i], reason,
                            operator, excused, value.toString(), String.valueOf(i), false, value.intValue() == 0);
                    queryString.append(") ");
                }
            }
        }

        queryString.append(") ");
        queryString.append("ORDER BY STD_NAME_VIEW");

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryTable table = dictionary.findDataDictionaryTableByClass(Student.class.getName());
        DistinctAdjuster adjuster = new DistinctAdjuster(table.getPrimaryKeyColumn(), getBroker().getPersistenceKey());

        BeanQuery query = new BeanQuery(Student.class);
        query.setSql(adjuster.adjustSql(queryString.toString()));
        query.setQueryAdjuster(adjuster);
        query.setDistinct(true);

        return query;
    }

    /**
     * Gets the calculation value.
     *
     * @param bean X2BaseBean
     * @param calculationId String
     * @return String
     * @see com.follett.fsc.core.k12.tools.procedures.QuickLetterData.getCalculationValue(X2BaseBean
     *      bean, String calculationId)
     */
    @Override
    protected String getCalculationValue(X2BaseBean bean, String calculationId) {
        String value = "0";

        // Lazy load
        if (!m_cache.containsKey(calculationId)) {
            loadCache(calculationId);
        }

        Integer count = m_cache.get(calculationId).get(bean.getOid());
        if (count != null) {
            value = String.valueOf(count.intValue());
        }

        return value;
    }

    /**
     * Gets the data class.
     *
     * @return Class
     * @see com.follett.fsc.core.k12.tools.procedures.QuickLetterData.getDataClass()
     */
    @Override
    protected Class getDataClass() {
        return Student.class;
    }

    /**
     * Setup.
     *
     * @param job ToolJob
     * @param userData UserDataContainer
     * @return true, if successful
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.procedures.QuickLetterData.setup(ToolJob job,
     *      UserDataContainer userData) throws X2BaseException
     */
    @Override
    protected boolean setup(ToolJob job, UserDataContainer userData) throws X2BaseException {
        boolean result = super.setup(job, userData);

        for (int i = 0; i < CODE_COUNT; i++) {
            m_codes[i] = getToolInput().getParameterValue(CODE_PARAM_PREFIX + i);
        }

        if (!StringUtils.isEmpty(getToolInput().getParameterValue(START_DATE_PARAM))) {
            m_startDate = formatInputDate(getToolInput().getParameterValue(START_DATE_PARAM), "MM/dd/yyyy");
        }

        if (!StringUtils.isEmpty(getToolInput().getParameterValue(END_DATE_PARAM))) {
            m_endDate = formatInputDate(getToolInput().getParameterValue(END_DATE_PARAM), "MM/dd/yyyy");
        }

        return result;
    }

    /**
     * Teardown.
     *
     * @see com.follett.fsc.core.k12.tools.procedures.QuickLetterData.teardown()
     */
    @Override
    protected void teardown() {
        super.teardown();

        for (Map cache : m_cache.values()) {
            cache.clear();
        }
        m_cache.clear();
        m_cache = null;
        m_codes = null;
        m_endDate = null;
        m_startDate = null;
    }

    /**
     * Appends the appropriate SQL syntax to the buffer for the given connector. The appended string
     * begins and ends with a space character.
     *
     * @param sqlBuffer StringBuilder
     * @param connector int
     * @param openParenthesis - if true, an open parenthesis will be placed after the operator
     * @param useNotExists - if true, subquery will be connected with a "NOT EXISTS" rather than an
     *        "EXISTS"
     */
    private void appendConnectorQuery(StringBuilder sqlBuffer,
                                      int connector,
                                      boolean openParenthesis,
                                      boolean useNotExists) {
        if (m_criteriaAdded) {
            if (connector == 0) {
                sqlBuffer.append(" AND");
            } else if (connector == 1) {
                sqlBuffer.append(" OR");
            }
        } else {
            sqlBuffer.append(" WHERE");
            m_criteriaAdded = true;
        }

        if (openParenthesis) {
            sqlBuffer.append(" (");
        }

        if (useNotExists) {
            sqlBuffer.append(" NOT");
        }

        sqlBuffer.append(" EXISTS ");
    }

    /**
     * Appends the subquery for the the given code criteria to the SQL buffer. The subquery begins
     * and ends with a space character.
     *
     * @param sqlBuffer StringBuilder
     * @param connector int
     * @param searchColumn String
     * @param searchValue String
     * @param reason String
     * @param operator int
     * @param excludeExcused boolean
     * @param countValue String
     * @param index String
     * @param openParenthesis - if true, an open parenthesis will be placed after the connector
     * @param valueIsZero boolean
     */
    private void appendSubQuery(StringBuilder sqlBuffer,
                                int connector,
                                String searchColumn,
                                String searchValue,
                                String reason,
                                int operator,
                                boolean excludeExcused,
                                String countValue,
                                String index,
                                boolean openParenthesis,
                                boolean valueIsZero) {
        /*
         * There are two special cases for building the subquery related to the "equals" and
         * "greater than or equal to" operators and the value 0. The problem is that the database
         * can't use a "HAVING count(*) = 0" clause because the result set doesn't return any rows!
         * So we need to take a different approach...
         *
         * Special Case 1: Code == 0
         *
         * We want to return all student OIDs that do NOT appear in the normal subquery. We can
         * accomplish this by using a NOT EXISTS and removing the COUNT column as well as the
         * GROUP BY and HAVING clauses.
         *
         * Special Case 2: Code >= 0
         *
         * We want to return all student OIDs. This criteria really isn't a criteria, it means
         * "give me the totals for this code for all students regardless of count". We can
         * accomplish this with the subquery "WHERE EXISTS (SELECT 1)".
         *
         * Special Case 3: Code < n where n > 0 (this case also applies for the <= operator)
         *
         * We want to return all student OIDs for 0 < code < n along with all student OIDs
         * from special case 1 (above). We'll do that with a recursive call with arguments to
         * simulate case 1.
         */

        // Special case 3 calculation
        boolean case3 = (operator == 3 || operator == 4);

        if (valueIsZero && operator == 2) {
            // Special case 2
            appendConnectorQuery(sqlBuffer, connector, openParenthesis, false);
            sqlBuffer.append("(SELECT 1)");
        } else {
            // Special case 1 calculation
            boolean case1 = (valueIsZero && operator == 0);

            if (case3) {
                appendConnectorQuery(sqlBuffer, connector, true, false);
            } else {
                appendConnectorQuery(sqlBuffer, connector, openParenthesis, case1);
            }


            sqlBuffer.append("(SELECT ATT_STD_OID");

            if (!case1) {
                sqlBuffer.append(", COUNT(*) AS TOTAL_");
                sqlBuffer.append(index);
            }

            sqlBuffer.append(" FROM STUDENT_ATTENDANCE ALIAS_");
            sqlBuffer.append(index);

            sqlBuffer.append(" WHERE ATT_DATE >= " + m_startDate);
            sqlBuffer.append(" AND ATT_DATE <= " + m_endDate);
            sqlBuffer.append(" AND STD_OID = ALIAS_");
            sqlBuffer.append(index);
            sqlBuffer.append(".ATT_STD_OID ");

            /*
             * Filter by the column/value combination specified
             */
            sqlBuffer.append(" AND ");
            sqlBuffer.append(searchColumn);
            sqlBuffer.append(" = '");
            sqlBuffer.append(searchValue);
            sqlBuffer.append("'");

            /*
             * Filter by reason, if necessary
             */
            if (!StringUtils.isEmpty(reason)) {
                sqlBuffer.append(" AND ATT_REASON_CODE = '" + reason + "'");
            }

            /*
             * Exclude the excused records, if necessary
             */
            if (excludeExcused) {
                sqlBuffer.append(" AND ATT_EXCUSED_IND = '0'");
            }

            if (!case1) {
                /*
                 * Add the operator and count
                 */
                String aggregateOperator = "COUNT";
                if (searchColumn.equals("ATT_ABSENT_IND")) {
                    aggregateOperator = "SUM";
                }
                sqlBuffer.append(" GROUP BY ATT_STD_OID ");
                sqlBuffer.append(" HAVING " + aggregateOperator + "(ATT_PORTION_ABSENT) ");
                switch (operator) {
                    case 0:
                        sqlBuffer.append("=");
                        break;

                    case 1:
                        sqlBuffer.append(">");
                        break;

                    case 2:
                        sqlBuffer.append(">=");
                        break;

                    case 3:
                        sqlBuffer.append("<");
                        break;

                    case 4:
                        sqlBuffer.append("<=");
                        break;
                }
                sqlBuffer.append(" ");
                sqlBuffer.append(countValue);
            }
            sqlBuffer.append(") ");
        }

        if (case3) {
            // Special case 3
            appendSubQuery(sqlBuffer, 1, searchColumn, searchValue, reason, 0, excludeExcused, "0", index, false, true);
            sqlBuffer.append(") ");
        }
    }

    /**
     * Returns only the active connectors based on the OPERATOR value.
     *
     * @return LinkedList<Integer>
     */
    private LinkedList<Integer> getActiveConnectors() {
        LinkedList<Integer> activeConnectors = new LinkedList<Integer>();
        activeConnectors.add(Integer.valueOf(-1));

        int operatorA = ((Integer) getParameter(ATTRIBUTE_OPERATOR_PARAM_ABSENT)).intValue();
        if (operatorA != 5) {
            int connectorT = ((Integer) getParameter(ATTRIBUTE_CONNECTOR_PARAM_TARDY)).intValue();
            activeConnectors.add(Integer.valueOf(connectorT));
        }

        int operatorT = ((Integer) getParameter(ATTRIBUTE_OPERATOR_PARAM_TARDY)).intValue();
        if (operatorT != 5) {
            int connectorD = ((Integer) getParameter(ATTRIBUTE_CONNECTOR_PARAM_DISMISSED)).intValue();
            activeConnectors.add(Integer.valueOf(connectorD));
        }

        int operatorD = ((Integer) getParameter(ATTRIBUTE_OPERATOR_PARAM_DISMISSED)).intValue();
        if (operatorD != 5 && !isEmptyValue(m_codes[0])) {
            int connectorCode0 = ((Integer) getParameter(CODE_OPERATOR_PARAM_PREFIX + "0")).intValue();
            activeConnectors.add(Integer.valueOf(connectorCode0));
        }

        for (int i = 0; i < CODE_COUNT - 1; i++) {
            if (!isEmptyValue(m_codes[i])) {
                int operator = ((Integer) getParameter(CODE_OPERATOR_PARAM_PREFIX + i)).intValue();
                if (operator != 5) {
                    int connector = ((Integer) getParameter(CODE_CONNECTOR_PARAM_PREFIX + (i + 1))).intValue();
                    activeConnectors.add(Integer.valueOf(connector));
                }
            }
        }

        return activeConnectors;
    }

    /**
     * Loads the count lookup cache for the passed calculation.
     * <p>
     * Example sql generated for # Total Absences
     *
     * - dates between 6/1/2008 and 6/26/2014
     * - current student list
     *
     * SELECT ATT_STD_OID, COUNT(*)
     * FROM STUDENT_ATTENDANCE, SELECTION_OBJECT
     * WHERE ATT_STD_OID = SEO_OBJ_OID
     * AND SEO_SEL_OID = 'SEL000000Jh004'
     * AND ATT_DATE >= '2008-06-01'
     * AND ATT_DATE <= '2014-06-26'
     * AND ATT_ABSENT_IND = '1'
     * GROUP BY ATT_STD_OID
     *
     * @param calculationId String
     */
    private void loadCache(String calculationId) {
        boolean unexcused = false;
        String columnSql = null;

        switch (calculationId) {
            case "total_abs":
                columnSql = "ATT_ABSENT_IND = '1'";
                break;

            case "total_tar":
                columnSql = "ATT_TARDY_IND = '1'";
                break;

            case "total_dis":
                columnSql = "ATT_DISMISSED_IND = '1'";
                break;

            case "total_unex_abs":
                columnSql = "ATT_ABSENT_IND = '1'";
                unexcused = true;
                break;

            case "total_unex_tar":
                columnSql = "ATT_TARDY_IND = '1'";
                unexcused = true;
                break;

            case "total_unex_dis":
                columnSql = "ATT_DISMISSED_IND = '1'";
                unexcused = true;
                break;

            case "total_other":
                columnSql = "(" + getDatabaseSyntax().isNotEmpty("ATT_OTHER_CODE") + " OR " +
                        getDatabaseSyntax().isNotEmpty("ATT_OTHER_CODE_02") + ")";
                break;
        }

        if (columnSql != null) {
            int studentsToInclude = Integer.valueOf(getToolInput().getParameterValue(QUERY_BY_PARAM)).intValue();
            boolean currentSelection = studentsToInclude == 0;

            StringBuilder sql = new StringBuilder();
            sql.append("SELECT ATT_STD_OID, COUNT(*) ");
            sql.append("FROM STUDENT_ATTENDANCE"
                    + (currentSelection ? ", " + getDatabaseSyntax().getTempTableName() + " " : ", STUDENT "));
            sql.append("WHERE ");

            // Joins
            if (currentSelection) {
                sql.append("ATT_STD_OID = " + TempTable.DATABASE_STORAGE_OID + " ");
            } else {
                sql.append("ATT_STD_OID = STD_OID ");
            }

            // Scope student
            switch (studentsToInclude) {
                case 1: // YOG
                    sql.append("AND STD_YOG = " + getParameter(QUERY_STRING_PARAM) + " ");

                    sql.append("AND "
                            + StudentManager.getActiveStudentDirectSQL(getOrganization(), "STD_ENROLLMENT_STATUS"));
                    break;

                case 2: // Snapshot
                    String subQuery = ReportUtils.getRecordSetSqlSubQuery(Student.DICTIONARY_ID,
                            (String) getParameter(QUERY_STRING_PARAM),
                            getUser(),
                            getSchool(),
                            getOrganization());
                    sql.append("AND STD_OID IN (" + subQuery + ") ");
                    break;

                default:
                    // Current selection (join handles student scoping)
                    break;
            }

            // Date range
            sql.append("AND ATT_DATE >= " + m_startDate + " ");
            sql.append("AND ATT_DATE <= " + m_endDate + " ");

            // Scope to column
            sql.append("AND " + columnSql + " ");

            // Unexcused only
            if (unexcused) {
                sql.append("AND ATT_EXCUSED_IND = '0' ");
            }

            sql.append("GROUP BY ATT_STD_OID");

            ColumnQuery query =
                    new ColumnQuery(StudentAttendance.class, new String[] {"ATT_STD_OID", "COUNT(*)"}, null);
            query.setSql(sql.toString());

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                Map<String, Integer> cache = new HashMap<String, Integer>();

                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();

                    String studentOid = (String) row[0];
                    Integer count = Integer.valueOf(row[1].toString());

                    cache.put(studentOid, count);
                }

                m_cache.put(calculationId, cache);
            } finally {
                iterator.close();
            }
        } else {
            m_cache.put(calculationId, new HashMap<String, Integer>(0));
        }
    }
}
