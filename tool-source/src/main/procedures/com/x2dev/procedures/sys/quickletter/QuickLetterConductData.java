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
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductAction.ConductActionCodeType;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.business.ConductManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Class which creates the data source for Quick Letter - Attendance.
 */
public class QuickLetterConductData extends QuickLetterData {
    // Attribute (A, I) query parameters
    private static final String ATTRIBUTE_CODE_PARAM_ACTION = "attribute_code_a";
    private static final String ATTRIBUTE_CODE_PARAM_INCIDENT = "attribute_code_i";
    private static final String ATTRIBUTE_CONNECTOR_PARAM_ACTION = "attribute_connector_a";
    private static final String ATTRIBUTE_OPERATOR_PARAM_ACTION = "attribute_operator_a";
    private static final String ATTRIBUTE_OPERATOR_PARAM_INCIDENT = "attribute_operator_i";
    private static final String ATTRIBUTE_VALUE_PARAM_ACTION = "attribute_value_a";
    private static final String ATTRIBUTE_VALUE_PARAM_INCIDENT = "attribute_value_i";

    // Code query parameters
    private static final String CODE_CONNECTOR_PARAM_PREFIX = "code_connector_";
    private static final String CODE_OPERATOR_PARAM_PREFIX = "code_operator_";
    private static final String CODE_PARAM_PREFIX = "code_";
    private static final String CODE_PRIMARY_PARAM_PREFIX = "code_primary_";
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
    private Map<String, Map<String, Object>> m_cache = new HashMap<String, Map<String, Object>>();

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

        calculationsById.put("total_cnd", "Total # of conduct incidents");
        calculationsById.put("total_sus", "Total # of suspensions");
        calculationsById.put("total_det", "Total # of detentions");
        calculationsById.put("total_days_sus", "Total # of days suspended");
        calculationsById.put("total_days_det", "Total # of days in detention");

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
     *      - More than 1 cheating incident between date range OR
     *      More than 1 detention action between date range
     *
     *      SELECT DISTINCT S0.*
     *      FROM STUDENT S0
     *      JOIN SELECTION_OBJECT ON STD_OID = SEO_OBJ_OID
     *      JOIN SCHOOL K0 ON STD_SKL_OID = K0.SKL_OID
     *      WHERE SKL_ORG_OID_1 = '*dst'
     *      AND SEO_SEL_OID = 'SEL000000Jh0vj'
     *      AND STD_OID IN (SELECT DISTINCT STD_OID
     *      FROM STUDENT
     *      WHERE EXISTS (SELECT CND_STD_OID, COUNT(*) AS TOTAL_CND
     *      FROM STUDENT_CONDUCT_INCIDENT ALIAS_CND
     *      WHERE CND_INCIDENT_DATE >= '2008-06-01'
     *      AND CND_INCIDENT_DATE <= '2014-06-26'
     *      AND STD_OID = ALIAS_CND.CND_STD_OID
     *      AND CND_INCIDENT_CODE = 'Cheating'
     *      GROUP BY CND_STD_OID
     *      HAVING COUNT(CND_OID) > 1)
     *      OR EXISTS (SELECT SCS_STD_OID, COUNT(*) AS TOTAL_ACT
     *      FROM STUDENT_CONDUCT_ACTION ALIAS_AACT, STUDENT_CONDUCT_ACTION_DATE ALIAS_ACT
     *      WHERE SCS_DATE >= '2008-06-01'
     *      AND SCS_DATE <= '2014-06-26'
     *      AND STD_OID = ALIAS_ACT.SCS_STD_OID
     *      AND ALIAS_AACT.ACT_OID = ALIAS_ACT.SCS_ACT_OID
     *      AND ACT_ACTION_CODE = 'Detention'
     *      GROUP BY SCS_STD_OID
     *      HAVING COUNT(SCS_OID) > 1) )
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
                // Current selection (join handles students scoping)
                break;
        }


        queryString.append("AND STD_OID IN (SELECT DISTINCT STD_OID FROM STUDENT");

        // Get list of active connectors
        LinkedList<Integer> activeConnectors = getActiveConnectors();

        /*
         * Add criteria one by one as necessary.
         */
        BigDecimal actionCount = (BigDecimal) getParameter(ATTRIBUTE_VALUE_PARAM_ACTION);
        BigDecimal incidentCount = (BigDecimal) getParameter(ATTRIBUTE_VALUE_PARAM_INCIDENT);

        if (incidentCount != null) {
            int operator = ((Integer) getParameter(ATTRIBUTE_OPERATOR_PARAM_INCIDENT)).intValue();
            String code = (String) getParameter(ATTRIBUTE_CODE_PARAM_INCIDENT);

            if (operator != 5) {
                appendSubQuery(queryString, activeConnectors.remove().intValue(), "0", code, operator,
                        incidentCount.toString(),
                        "CND", incidentCount.compareTo(new BigDecimal(0.0)) == 0);
            }
        }

        if (actionCount != null) {
            int operator = ((Integer) getParameter(ATTRIBUTE_OPERATOR_PARAM_ACTION)).intValue();
            String code = (String) getParameter(ATTRIBUTE_CODE_PARAM_ACTION);

            if (operator != 5) {
                appendSubQuery(queryString, activeConnectors.remove().intValue(), "1", code, operator,
                        actionCount.toString(),
                        "ACT", actionCount.intValue() == 0);
            }
        }

        for (int i = 0; i < CODE_COUNT; i++) {
            if (!isEmptyValue(m_codes[i])) {
                int operator = ((Integer) getParameter(CODE_OPERATOR_PARAM_PREFIX + i)).intValue();
                String code = (String) getParameter(CODE_PRIMARY_PARAM_PREFIX + i);
                BigDecimal value = (BigDecimal) getParameter(CODE_VALUE_PARAM_PREFIX + i);

                if (operator != 5) {
                    appendSubQuery(queryString, activeConnectors.remove().intValue(), m_codes[i], code, operator,
                            value.toString(), String.valueOf(i), value.intValue() == 0);
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

        Object count = m_cache.get(calculationId).get(bean.getOid());
        if (count != null) {
            if (calculationId.equals("total_days_sus") || calculationId.equals("total_days_det")) {
                value = String.valueOf(((BigDecimal) count).doubleValue());
            } else {
                value = count.toString();
            }
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
     * Returns true if the given value is null, the empty string, or "2" (the empty select value).
     *
     * @param value String
     * @return true, if is empty value
     * @see com.follett.fsc.core.k12.tools.procedures.QuickLetterData.isEmptyValue(String value)
     */
    @Override
    protected boolean isEmptyValue(String value) {
        return super.isEmptyValue(value) || value.equals("2");
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
     * @param openParenthesis if true, an open parenthesis will be placed after the AND/OR/WHERE
     *        keyword
     * @param useNotExists if true then the subquery will be connected with a "NOT EXISTS"
     *        rather than an "EXISTS"
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
     * @param type - 0=incident, 1=action
     * @param code String
     * @param operator int
     * @param countValue String
     * @param index String
     * @param valueIsZero boolean
     */
    private void appendSubQuery(StringBuilder sqlBuffer,
                                int connector,
                                String type,
                                String code,
                                int operator,
                                String countValue,
                                String index,
                                boolean valueIsZero) {
        boolean incident = "0".equals(type);

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
            appendConnectorQuery(sqlBuffer, connector, false, false);
            sqlBuffer.append("(SELECT 1)");
        } else {
            // Special case 1 calculation
            boolean case1 = (valueIsZero && operator == 0);

            if (case3) {
                appendConnectorQuery(sqlBuffer, connector, true, false);
            } else {
                appendConnectorQuery(sqlBuffer, connector, false, case1);
            }

            sqlBuffer.append("(SELECT " + (incident ? "CND" : "SCS") + "_STD_OID");

            if (!case1) {
                sqlBuffer.append(", COUNT(*) AS TOTAL_");
                sqlBuffer.append(index);
            }

            String from;
            if (incident) {
                from = "STUDENT_CONDUCT_INCIDENT ALIAS_" + index;
            } else {
                from = "STUDENT_CONDUCT_ACTION ALIAS_A" + index;
            }
            sqlBuffer.append(" FROM " + from);

            if (!incident) {
                sqlBuffer.append(" INNER JOIN STUDENT_CONDUCT_ACTION_DATE ALIAS_" + index +
                        " ON ALIAS_A" + index + ".ACT_OID = ALIAS_" + index + ".SCS_ACT_OID ");
            }

            String date = incident ? "CND_INCIDENT_DATE" : "SCS_DATE";
            sqlBuffer.append(" WHERE " + date + " >= " + m_startDate);
            sqlBuffer.append(" AND " + date + " <= " + m_endDate);
            sqlBuffer.append(" AND STD_OID = ALIAS_");
            sqlBuffer.append(index);
            sqlBuffer.append("." + (incident ? "CND" : "SCS") + "_STD_OID ");


            /*
             * Filter by reason, if necessary
             */
            if (!StringUtils.isEmpty(code)) {
                String codeCol = incident ? "CND_INCIDENT_CODE" : "ACT_ACTION_CODE";
                sqlBuffer.append(" AND " + codeCol + " = '" + code + "'");
            }

            if (!case1) {
                /*
                 * Add the operator and count
                 */
                sqlBuffer.append(" GROUP BY " + (incident ? "CND" : "SCS") + "_STD_OID");
                sqlBuffer.append(" HAVING COUNT(" + (incident ? "CND" : "SCS") + "_OID) ");
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
            appendSubQuery(sqlBuffer, 1, type, code, 0, "0", index, true);
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

        int operatorI = ((Integer) getParameter(ATTRIBUTE_OPERATOR_PARAM_INCIDENT)).intValue();
        if (operatorI != 5) {
            int connectorA = ((Integer) getParameter(ATTRIBUTE_CONNECTOR_PARAM_ACTION)).intValue();
            activeConnectors.add(Integer.valueOf(connectorA));
        }

        int operatorA = ((Integer) getParameter(ATTRIBUTE_OPERATOR_PARAM_ACTION)).intValue();
        if (operatorA != 5) {
            int connectorCode0 = ((Integer) getParameter(CODE_CONNECTOR_PARAM_PREFIX + "0")).intValue();
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
     * Example sql generated for # Total Incidents
     *
     * - dates between 6/1/2008 and 6/26/2014
     * - current student list
     *
     * SELECT CND_STD_OID, COUNT(*)
     * FROM STUDENT_CONDUCT_INCIDENT, SELECTION_OBJECT
     * WHERE CND_STD_OID = SEO_OBJ_OID
     * AND SEO_SEL_OID = 'SEL000000Jh1rP'
     * AND CND_INCIDENT_DATE >= '2008-06-01'
     * AND CND_INCIDENT_DATE <= '2014-06-26'
     * GROUP BY CND_STD_OID
     *
     * @param calculationId String
     */
    private void loadCache(String calculationId) {
        boolean sum = false;
        boolean incident = false;
        List<String> codes = null;

        switch (calculationId) {
            case "total_sus":
                codes = ConductManager.getActionCodesForType(ConductActionCodeType.SUSPENSION, getBroker());
                break;

            case "total_det":
                codes = ConductManager.getActionCodesForType(ConductActionCodeType.DETENTION, getBroker());
                break;

            case "total_days_sus":
                codes = ConductManager.getActionCodesForType(ConductActionCodeType.SUSPENSION, getBroker());
                sum = true;
                break;

            case "total_days_det":
                codes = ConductManager.getActionCodesForType(ConductActionCodeType.DETENTION, getBroker());
                sum = true;
                break;

            case "total_cnd":
                incident = true;
                codes = new ArrayList<String>(0);
                break;
        }

        if (codes != null) {
            int studentsToInclude = Integer.valueOf(getToolInput().getParameterValue(QUERY_BY_PARAM)).intValue();
            boolean currentSelection = studentsToInclude == 0;

            String student = (incident ? "CND_STD_OID" : "ACT_STD_OID");
            String function = (sum ? "SUM(ACT_PENALTY_TIME)" : "COUNT(*)");
            String table = (incident ? "STUDENT_CONDUCT_INCIDENT" : "STUDENT_CONDUCT_ACTION");
            String joinTable = (currentSelection ? getDatabaseSyntax().getTempTableName() + " " : "STUDENT ");
            String start = (incident ? "CND_INCIDENT_DATE" : "ACT_START_DATE");
            String end = (incident ? "CND_INCIDENT_DATE" : "ACT_END_DATE");
            String code = (incident ? "CND_INCIDENT_CODE" : "ACT_ACTION_CODE");
            Class baseClass = (incident ? ConductIncident.class : ConductAction.class);

            StringBuilder sql = new StringBuilder();
            sql.append("SELECT " + student + ", " + function + " ");
            sql.append("FROM " + table + ", " + joinTable);
            sql.append("WHERE ");

            // Joins
            if (currentSelection) {
                sql.append(student + " = " + TempTable.DATABASE_STORAGE_OID + " ");
            } else {
                sql.append(student + " = STD_OID ");
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
            sql.append("AND " + end + " >= " + m_startDate + " ");
            sql.append("AND " + start + " <= " + m_endDate + " ");

            // Scope to column
            if (!codes.isEmpty()) {
                sql.append("AND " + code + " IN (" + StringUtils.convertCollectionToDelimitedString(codes, ",", "'")
                        + ") ");
            }

            sql.append("GROUP BY " + student);

            ColumnQuery query = new ColumnQuery(baseClass, new String[] {student, function}, null);
            query.setSql(sql.toString());

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                Map<String, Object> cache = new HashMap<String, Object>();

                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();

                    String studentOid = (String) row[0];
                    Object count = row[1];

                    cache.put(studentOid, count);
                }

                m_cache.put(calculationId, cache);
            } finally {
                iterator.close();
            }
        } else {
            m_cache.put(calculationId, new HashMap<String, Object>(0));
        }
    }
}
