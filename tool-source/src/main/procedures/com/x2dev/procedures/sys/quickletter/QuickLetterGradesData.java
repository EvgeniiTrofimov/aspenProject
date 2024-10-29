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
import com.follett.fsc.core.framework.persistence.DatabaseOptimizer;
import com.follett.fsc.core.framework.persistence.DatabaseOptimizerFactory;
import com.follett.fsc.core.framework.persistence.TempTable;
import com.follett.fsc.core.framework.persistence.adjusters.DistinctAdjuster;
import com.follett.fsc.core.k12.beans.BeanManager;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.BusinessRules;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.procedures.QuickLetterData;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BigDecimalConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Class which creates the data source for Quick Letter - Attendance.
 */
public class QuickLetterGradesData extends QuickLetterData {
    /**
     * Constant for the transcript definition OID parameter, this has to align with
     * quickLetterGrades.jsp
     */
    private static final String TRANSCRIPT_PARAM = "transcriptDefinitionOid";

    // Code query parameters
    private static final String FIELD_PARAM_PREFIX = "field_";
    private static final String FIELD_CONNECTOR_PARAM_PREFIX = "field_connector_";
    private static final String FIELD_OPERATOR_PARAM_PREFIX = "field_operator_";
    private static final String FIELD_VALUE_PARAM_PREFIX = "field_value_";

    // All other parameters
    private static final int CODE_COUNT = 6;
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";

    private String[] m_codes = new String[CODE_COUNT];
    private boolean m_criteriaAdded = false;
    private GradesManager m_manager = null;

    /**
     * Gets the available calculations.
     *
     * @return Map
     * @see com.follett.fsc.core.k12.tools.reports.QuickLetterData.getAvailableCalculations()
     */
    @Override
    public Map<String, String> getAvailableCalculations() {
        return new LinkedHashMap<String, String>();
    }

    /**
     * Creates the query.
     *
     * @return BeanQuery
     * @see com.follett.fsc.core.k12.tools.procedures.QuickLetterData.createQuery()
     *
     *      Example sql generated for
     *
     *      - current student list
     *      - Final Grade <= D AND
     *      Semester 1 Exam <= D AND
     *      Semester 2 Exam <= D
     *
     *      SELECT DISTINCT S0.*
     *      FROM STUDENT S0
     *      JOIN SELECTION_OBJECT ON STD_OID = SEO_OBJ_OID
     *      JOIN SCHOOL K0 ON STD_SKL_OID = K0.SKL_OID
     *      WHERE STD_SKL_OID = 'sklWW00024076'
     *      AND SEO_SEL_OID = 'SEL000000Jd0vk'
     *      AND STD_OID IN (SELECT DISTINCT STD_OID
     *      FROM STUDENT
     *      WHERE EXISTS (SELECT TRN_STD_OID, COUNT(*) AS TOTAL_0
     *      FROM STUDENT_TRANSCRIPT ALIAS_0
     *      WHERE TRN_GTD_OID = 'GTD0000001B02H'
     *      AND STD_OID = ALIAS_0.TRN_STD_OID
     *      AND (TRN_FINAL_GRADE IN ('D','D-','F') OR TRN_FINAL_GRADE <= 66.0)
     *      GROUP BY TRN_STD_OID)
     *      AND EXISTS (SELECT TRN_STD_OID, COUNT(*) AS TOTAL_1
     *      FROM STUDENT_TRANSCRIPT ALIAS_1
     *      WHERE TRN_GTD_OID = 'GTD0000001B02H'
     *      AND STD_OID = ALIAS_1.TRN_STD_OID
     *      AND (TRN_FIELDA_008 IN ('D','D-','F') OR TRN_FIELDA_008 <= 66.0)
     *      GROUP BY TRN_STD_OID)
     *      AND EXISTS (SELECT TRN_STD_OID, COUNT(*) AS TOTAL_2
     *      FROM STUDENT_TRANSCRIPT ALIAS_2
     *      WHERE TRN_GTD_OID = 'GTD0000001B02H'
     *      AND STD_OID = ALIAS_2.TRN_STD_OID
     *      AND (TRN_FIELDA_007 IN ('D','D-','F') OR TRN_FIELDA_007 <= 66.0)
     *      GROUP BY TRN_STD_OID) )
     *      ORDER BY STD_NAME_VIEW;
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
        for (int i = 0; i < CODE_COUNT; i++) {
            if (!isEmptyValue(m_codes[i])) {
                int operator = ((Integer) getParameter(FIELD_OPERATOR_PARAM_PREFIX + i)).intValue();
                String value = (String) getParameter(FIELD_VALUE_PARAM_PREFIX + i);

                if (operator != 5) {
                    boolean isZero = false;
                    if (StringUtils.isNumeric(value)) {
                        BigDecimal valueAsDecimal = new BigDecimal(value);
                        isZero = valueAsDecimal != null && valueAsDecimal.doubleValue() == 0.0;
                    }

                    appendSubQuery(queryString, activeConnectors.remove().intValue(), m_codes[i], operator,
                            value.toString(), String.valueOf(i), false, isZero);
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
        // No calculations
        return "0";
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
     * Not all parameters were necessarily generated as ToolInputParameters in the tool input XML.
     * Unfortunately ToolJavaSource filters to only include those. Parameters in the ToolInput
     * generated through other means (like through custom jsp pages) are not included. So since
     * we stored the actual ToolInput object, we'll look up directly there if not found.
     *
     * @param key String
     * @return Object
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource.getParameter(String key)
     */
    @Override
    protected Object getParameter(String key) {
        Object value;
        if (getParameters().containsKey(key)) {
            value = super.getParameter(key);
        } else {
            value = getToolInput().getParameterValue(key);
        }

        return value;
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
            m_codes[i] = getToolInput().getParameterValue(FIELD_PARAM_PREFIX + i);
        }

        m_manager = new GradesManager(getBroker());

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

        m_codes = null;
        m_manager.destroy();
        m_manager = null;
    }

    /**
     * Appends the appropriate SQL syntax to the buffer for the given connector. The appended string
     * begins and ends with a space character.
     *
     * @param sqlBuffer StringBuilder
     * @param connector int
     * @param openParenthesis - if true, an open parenthesis will be placed after the AND/OR/WHERE
     *        keyword
     * @param useNotExists - if true then the subquery will be connected with a "NOT EXISTS" rather
     *        than an "EXISTS"
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
     * @param field String
     * @param operator int
     * @param score String
     * @param index String
     * @param openParenthesis boolean
     * @param valueIsZero boolean
     */
    private void appendSubQuery(StringBuilder sqlBuffer,
                                int connector,
                                String field,
                                int operator,
                                String score,
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
         */

        if (valueIsZero && operator == 2) {
            // Special case 2
            appendConnectorQuery(sqlBuffer, connector, false, false);
            sqlBuffer.append("(SELECT 1)");
        } else {
            // Special case 1 calculation
            boolean case1 = (valueIsZero && operator == 0);

            appendConnectorQuery(sqlBuffer, connector, false, case1);

            sqlBuffer.append("(SELECT TRN_STD_OID");

            if (!case1) {
                sqlBuffer.append(", COUNT(*) AS TOTAL_");
                sqlBuffer.append(index);
            }

            sqlBuffer.append(" FROM STUDENT_TRANSCRIPT ALIAS_");
            sqlBuffer.append(index);

            sqlBuffer.append(" WHERE TRN_GTD_OID = '" + getToolInput().getParameterValue(TRANSCRIPT_PARAM) + "'");
            sqlBuffer.append(" AND STD_OID = ALIAS_");
            sqlBuffer.append(index);
            sqlBuffer.append(".TRN_STD_OID ");

            if (!case1) {
                sqlBuffer.append(getGradesSQL(field, score, operator, getBroker()));

                sqlBuffer.append(" GROUP BY TRN_STD_OID");
                sqlBuffer.append(") ");
            }
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

        for (int i = 0; i < CODE_COUNT - 1; i++) {
            if (!isEmptyValue(m_codes[i])) {
                int operator = ((Integer) getParameter(FIELD_OPERATOR_PARAM_PREFIX + i)).intValue();
                if (operator != 5) {
                    int connector = ((Integer) getParameter(FIELD_CONNECTOR_PARAM_PREFIX + (i + 1))).intValue();
                    activeConnectors.add(Integer.valueOf(connector));
                }
            }
        }

        return activeConnectors;
    }

    /**
     * Returns a comma-separated and single-quote enclosed list as string of grade codes which are
     * in the
     * passed grade scale that match the criteria specified by the operator (coded as two boolean
     * parameters).
     * <p>
     * Example: a scale with [A, B, C, D, F], a passed value of C, and operator of
     * <= (greaterThan=false, equals=true) will return: 'C','D','F'
     *
     * @param value String
     * @param greaterThan boolean
     * @param equals boolean
     * @param scale GradeScale
     * @return String
     */
    private String getGrades(String value, boolean greaterThan, boolean equals, GradeScale scale) {
        ArrayList<String> grades = new ArrayList<String>();

        // Handle equals case
        if (equals) {
            grades.add(value);
        }

        /*
         * If >, add all codes found until the passed code is found.
         * If <, find the passed code and then add all remaining codes.
         */
        boolean found = false;
        for (GradeScaleGradeDefinition def : (List<GradeScaleGradeDefinition>) m_manager.getGradeDefinitions(scale,
                null, null, false)) {
            if (def.getGradeCode().equalsIgnoreCase(value)) {
                if (!greaterThan) {
                    break;
                }

                found = true;
                continue;
            }

            if (!greaterThan || found) {
                grades.add(def.getGradeCode());
            }
        }

        // If the value is a letter, but matches no codes, log a message
        if (grades.isEmpty() && !StringUtils.isNumeric(value)) {
            LocalizationMessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey());
            String msg = messages.getMessage("valid." + BusinessRules.GRADEBOOK_SCORE_VALID, scale.getGradeScaleName());
            logMessage(msg);
        }

        return !grades.isEmpty() ? StringUtils.convertCollectionToDelimitedString(grades, ",", "'") : "''";
    }

    /**
     * Returns the statement to scope transcripts by a score.
     *
     * @param transcriptColumnOid String
     * @param score String
     * @param operator int
     * @param broker X2Broker
     * @return String
     */
    private String getGradesSQL(String transcriptColumnOid, String score, int operator, X2Broker broker) {
        String sql = "";

        TranscriptColumnDefinition gtc =
                (TranscriptColumnDefinition) broker.getBeanByOid(TranscriptColumnDefinition.class,
                        BeanManager.getFullOid(transcriptColumnOid, broker.getPersistenceKey()));

        if (gtc != null) {
            String dbColumn = gtc.getDataFieldConfig().getDataField().getDatabaseName();

            GradeScale scale = gtc.getGradeScale();

            switch (gtc.getEntryModeEnum()) {
                case BOTH:
                    sql = " AND (";
                    String letterScore = getLetterScore(score, scale);
                    sql += getLetterSQL(dbColumn, operator, letterScore, scale);

                    sql += " OR ";

                    String numericScore = getNumericScore(score, scale);
                    sql += getNumericSQL(dbColumn, operator, numericScore);
                    sql += ") ";
                    break;

                case LETTER_ONLY:
                    String letterOnlyScore = getLetterScore(score, scale);
                    sql = " AND " + getLetterSQL(dbColumn, operator, letterOnlyScore, scale) + " ";
                    break;

                case NUMERIC_ONLY:
                    String numericOnlyScore = getNumericScore(score, scale);
                    sql = " AND " + getNumericSQL(dbColumn, operator, numericOnlyScore) + " ";
                    break;
            }
        }

        return sql;
    }

    /**
     * Returns the letter code for the passed score.
     * <p>
     * If it is already a non-numeric value, the code is returned as is. An invalid
     * letter code will simply produce no results.
     * <p>
     * If it is a numeric score, first look for a code with the exact numeric score. Some
     * customers will use a 1-100 scale with each code as an integer value. If no exact code
     * is found, the grade cutoff value of each scale code will be used to determine the range
     * in which the passed numeric code resides. The letter code for that range will be returned.
     *
     * @param score String
     * @param scale GradeScale
     * @return String
     */
    private String getLetterScore(String score, GradeScale scale) {
        String result = null;

        if (!StringUtils.isEmpty(score)) {
            if (StringUtils.isNumeric(score)) {
                /*
                 * First look for a grade scale code exact match, if found there is no
                 * need to convert the string value.
                 */
                GradeScaleGradeDefinition gradeDefinition = m_manager.getGradeDefinition(score, scale, null, null);
                if (gradeDefinition == null) {
                    /*
                     * If not found, look for the code whose range contains the numeric value
                     */
                    BigDecimal numericScore = new BigDecimal(score);
                    if (numericScore != null) {
                        gradeDefinition = m_manager.getGradeDefinition(numericScore, scale, null, null);
                        if (gradeDefinition != null) {
                            result = gradeDefinition.getGradeCode();
                        }
                    }
                }
            } else {
                result = score;
            }
        }

        return result;
    }

    /**
     * Returns a SQL statement for a letter comparison of the passed value.
     * <p>
     * An "IN" statement is used with a collection of grade scale grade codes which depends
     * on the operator to include lower, equal, or higher valued codes.
     * <p>
     * Example: a scale with [A, B, C, D, F], a passed value of C, and operator of <= will result
     * in SQL of: TRN_FIELD IN ('C','D','F')
     * <p>
     * NOTE: A code with no numeric equivalent is only included in "=" operator.
     *
     *
     * @param dbColumn String
     * @param operator int
     * @param value String
     * @param scale GradeScale
     * @return String
     */
    private String getLetterSQL(String dbColumn, int operator, String value, GradeScale scale) {
        String sql = "0 = 1";

        if (!StringUtils.isEmpty(value)) {
            sql = dbColumn;

            switch (operator) {
                case 0:
                    sql += " = '" + value + "'";
                    break;

                case 1: // '>'
                    sql += " IN (" + getGrades(value, true, false, scale) + ")";
                    break;

                case 2: // '>='
                    sql += " IN (" + getGrades(value, true, true, scale) + ")";
                    break;

                case 3: // '<'
                    sql += " IN (" + getGrades(value, false, false, scale) + ")";
                    break;

                case 4: // '<='
                    sql += " IN (" + getGrades(value, false, true, scale) + ")";
                    break;
            }
        }

        return sql;
    }

    /**
     * Returns the passed score as a numeric string. If it is already a valid number, it is
     * returned as is. If it is a letter score and it is capable of being converted, the
     * grade value on the grade scale grade associated with the code is returned.
     *
     * @param score String
     * @param scale GradeScale
     * @return String
     */
    private String getNumericScore(String score, GradeScale scale) {
        String result = null;

        if (!StringUtils.isNumeric(score)) {
            /*
             * Find the definition for the letter score. If not found or definition flagged with no
             * numeric equivalent, there is no conversion that can be done.
             */
            GradeScaleGradeDefinition gradeDefinition = m_manager.getGradeDefinition(score, scale, null, null);
            if (gradeDefinition != null && !gradeDefinition.getNoNumericIndicator()) {
                /*
                 * If found, use the numeric grade equivalent of the letter and convert it back to
                 * a string for use in the SQL statement.
                 */
                BigDecimal scoreAsDecimal = gradeDefinition.getGradeValue();
                if (scoreAsDecimal != null) {
                    BigDecimalConverter converter = (BigDecimalConverter) ConverterFactory
                            .getConverterForClass(Converter.BIG_DECIMAL_CONVERTER);
                    result = converter.javaToString(scoreAsDecimal);
                }
            }
        } else {
            result = score;
        }

        return result;
    }

    /**
     * Returns the SQL statement for a numeric comparison of the passed value.
     *
     * @param dbColumn String
     * @param operator int
     * @param value String
     * @return String
     */
    private String getNumericSQL(String dbColumn, int operator, String value) {
        String sql = "0 = 1";

        if (value != null) {
            DatabaseOptimizer optimizer =
                    DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey());

            String op = getOperatorAsString(operator);

            sql = "1 = (CASE WHEN ((" + optimizer.sqlStringIsNumeric(dbColumn) + ") ";
            sql += "AND (" + optimizer.sqlStringToNumeric(dbColumn) + " " + op + " " + value + "))";
            sql += " THEN 1 ELSE 0 END)";
        }

        return sql;
    }

    /**
     * Returns the passed operator as a string.
     *
     * @param operator int
     * @return String
     */
    private String getOperatorAsString(int operator) {
        String sql;

        switch (operator) {
            case 0:
                sql = " = ";
                break;

            case 1:
                sql = " > ";
                break;

            case 2:
                sql = " >= ";
                break;

            case 3:
                sql = " < ";
                break;

            case 4:
                sql = " <= ";
                break;

            default:
                sql = " = ";
        }

        return sql;
    }
}
