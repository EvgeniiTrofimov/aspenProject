/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentScheduleAttributes;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Principal's Attendance" report. This report lists the students and
 * attendance totals matching the entered criteria within a given date range.
 *
 * @author X2 Development Corporation
 */
public class PrincipalAttendanceData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;
    // Attribute (A, T, D) query parameters
    public static final String ATTRIBUTE_CONNECTOR_PARAM_TARDY = "attribute_connector_t";
    public static final String ATTRIBUTE_CONNECTOR_PARAM_DISMISSED = "attribute_connector_d";
    public static final String ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_ABSENT = "attribute_excludeExcused_a";
    public static final String ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_TARDY = "attribute_excludeExcused_t";
    public static final String ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_DISMISSED = "attribute_excludeExcused_d";
    public static final String ATTRIBUTE_OPERATOR_PARAM_ABSENT = "attribute_operator_a";
    public static final String ATTRIBUTE_OPERATOR_PARAM_TARDY = "attribute_operator_t";
    public static final String ATTRIBUTE_OPERATOR_PARAM_DISMISSED = "attribute_operator_d";
    public static final String ATTRIBUTE_REASON_PARAM_ABSENT = "attribute_reason_a";
    public static final String ATTRIBUTE_REASON_PARAM_TARDY = "attribute_reason_t";
    public static final String ATTRIBUTE_REASON_PARAM_DISMISSED = "attribute_reason_d";
    public static final String ATTRIBUTE_VALUE_PARAM_ABSENT = "attribute_value_a";
    public static final String ATTRIBUTE_VALUE_PARAM_TARDY = "attribute_value_t";
    public static final String ATTRIBUTE_VALUE_PARAM_DISMISSED = "attribute_value_d";

    // Code query parameters
    public static final String CODE_PARAM_PREFIX = "code_";
    public static final String CODE_CONNECTOR_PARAM_PREFIX = "code_connector_";
    public static final String CODE_EXCLUDE_EXCUSED_PARAM_PREFIX = "code_excludeExcused_";
    public static final String CODE_OPERATOR_PARAM_PREFIX = "code_operator_";
    public static final String CODE_REASON_PARAM_PREFIX = "code_reason_";
    public static final String CODE_VALUE_PARAM_PREFIX = "code_value_";

    // All other parameters
    public static final String END_DATE_PARAM = "endDate";
    public static final String GROUP_BY_PARAM = "groupBy";
    public static final String HOMEROOM_TO_STAFF_MAP = "homeroomToStaffMap";
    public static final String QUERY_BY_PARAM = "queryBy";
    public static final String QUERY_STRING_PARAM = "queryString";
    public static final String SORT_ORDER_PARAM = "sort";
    public static final String START_DATE_PARAM = "startDate";

    /*
     * Pseudo-OIDs for the buildStudentAttendanceTotalQuery() method. These three values are used to
     * store the counts for the absent, tardy, and dismissal indicators.
     */
    private static final String ABSENT_CODE_NAME = "___A___";
    private static final String TARDY_CODE_NAME = "___T___";
    private static final String DISMISSAL_CODE_NAME = "___D___";

    private static final int CODE_COUNT = 4;

    private String[] m_codes = new String[CODE_COUNT];
    private boolean m_criteriaAdded = false;
    private PlainDate m_endDate = null;
    private PlainDate m_startDate = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid statistics = null;

        try {
            /*
             * Initialize the member variables
             */
            for (int i = 0; i < CODE_COUNT; i++) {
                m_codes[i] = (String) getParameter(CODE_PARAM_PREFIX + i);
            }

            m_startDate = (PlainDate) getParameter(START_DATE_PARAM);
            m_endDate = (PlainDate) getParameter(END_DATE_PARAM);

            if (m_startDate != null && m_endDate != null) {
                StringBuilder queryString = new StringBuilder(3000);

                /*
                 * Build the outer query that defines the tables and fields involved. Only include
                 * active students from the selected school or district.
                 */
                queryString.append(
                        "SELECT DISTINCT SKL_SCHOOL_NAME, STD_OID, STD_YOG, STD_NAME_VIEW, STD_HOMEROOM, CODE_NAME, TOTAL ");
                queryString.append("FROM SCHOOL ");
                queryString.append("INNER JOIN STUDENT ON STD_SKL_OID = SKL_OID ");
                queryString.append("LEFT OUTER JOIN (");
                queryString.append(buildStudentAttendanceTotalQuery());
                queryString.append(") ALIASX ON STD_OID = ATT_STD_OID ");

                queryString.append("WHERE ");
                if (isSchoolContext()) {
                    queryString.append("STD_SKL_OID = '");
                    queryString.append(getSchool().getOid());
                    queryString.append("' ");
                } else {
                    int level = getOrganization().getOrganizationDefinition().getLevel();
                    queryString.append("SKL_ORG_OID_");
                    queryString.append(level + 1);
                    queryString.append(" = '");
                    queryString.append(getOrganization().getOid());
                    queryString.append("' ");
                }

                queryString.append(
                        "AND " + StudentManager.getActiveStudentDirectSQL(getOrganization(), "STD_ENROLLMENT_STATUS"));

                switch (((Integer) getParameter(QUERY_BY_PARAM)).intValue()) {
                    case 1: // YOG
                        queryString.append(" AND STD_YOG = " + getParameter(QUERY_STRING_PARAM) + " ");
                        break;

                    case 2: // Snapshot
                        String subQuery = ReportUtils.getRecordSetSqlSubQuery(SisStudent.DICTIONARY_ID,
                                (String) getParameter(QUERY_STRING_PARAM),
                                getUser(),
                                getSchool(),
                                getOrganization());
                        queryString.append(" AND STD_OID IN (" + subQuery + ") ");
                        break;

                    default: // All (do nothing)
                        break;
                }

                queryString.append("AND STD_OID IN (SELECT DISTINCT STD_OID FROM STUDENT");

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
                        appendSubQuery(queryString, -1, "ATT_ABSENT_IND", "1", reason,
                                operator, excused, absentCount.toString(), "ABS", false,
                                absentCount.compareTo(new BigDecimal(0.0)) == 0);
                    }
                }

                if (tardyCount != null) {
                    int connector = ((Integer) getParameter(ATTRIBUTE_CONNECTOR_PARAM_TARDY)).intValue();
                    boolean excused = ((Boolean) getParameter(ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_TARDY)).booleanValue();
                    int operator = ((Integer) getParameter(ATTRIBUTE_OPERATOR_PARAM_TARDY)).intValue();
                    String reason = (String) getParameter(ATTRIBUTE_REASON_PARAM_TARDY);

                    if (operator != 5) {
                        appendSubQuery(queryString, connector, "ATT_TARDY_IND", "1", reason,
                                operator, excused, tardyCount.toString(), "TRD", false, tardyCount.intValue() == 0);
                    }
                }

                if (dismissalCount != null) {
                    int connector = ((Integer) getParameter(ATTRIBUTE_CONNECTOR_PARAM_DISMISSED)).intValue();
                    boolean excused =
                            ((Boolean) getParameter(ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_DISMISSED)).booleanValue();
                    int operator = ((Integer) getParameter(ATTRIBUTE_OPERATOR_PARAM_DISMISSED)).intValue();
                    String reason = (String) getParameter(ATTRIBUTE_REASON_PARAM_DISMISSED);

                    if (operator != 5) {
                        appendSubQuery(queryString, connector, "ATT_DISMISSED_IND", "1", reason,
                                operator, excused, dismissalCount.toString(), "DSM", false,
                                dismissalCount.intValue() == 0);
                    }
                }

                for (int i = 0; i < CODE_COUNT; i++) {
                    if (!isEmptyValue(m_codes[i])) {
                        int connector = ((Integer) getParameter(CODE_CONNECTOR_PARAM_PREFIX + i)).intValue();
                        boolean excused =
                                ((Boolean) getParameter(CODE_EXCLUDE_EXCUSED_PARAM_PREFIX + i)).booleanValue();
                        int operator = ((Integer) getParameter(CODE_OPERATOR_PARAM_PREFIX + i)).intValue();
                        String reason = (String) getParameter(CODE_REASON_PARAM_PREFIX + i);
                        BigDecimal value = (BigDecimal) getParameter(CODE_VALUE_PARAM_PREFIX + i);

                        if (operator != 5) {
                            appendSubQuery(queryString, connector, "ATT_OTHER_CODE", m_codes[i], reason,
                                    operator, excused, value.toString(), String.valueOf(i), true,
                                    value.intValue() == 0);
                            appendSubQuery(queryString, 1, "ATT_OTHER_CODE_02", m_codes[i], reason,
                                    operator, excused, value.toString(), String.valueOf(i), false,
                                    value.intValue() == 0);
                            queryString.append(") ");
                        }
                    }
                }

                queryString.append(") ");

                appendOrderBy(queryString);

                Connection connection = getBroker().borrowConnection();
                Statement statement = connection.createStatement();
                ResultSet resultSet = statement.executeQuery(queryString.toString());

                // Create a map to add StudentScheduleAttribute.houseCode for each student to the
                // DataGrid
                Map stdToSsaMap = buildStdToSsaMap();

                /*
                 * Finally we flatten the results into a DataGrid and add a link to the student bean
                 */
                statistics = formatData(resultSet, stdToSsaMap);
                statistics.beforeTop();

                resultSet.close();
                statement.close();

                switch (((Integer) getParameter(GROUP_BY_PARAM)).intValue()) {
                    case 0: // none
                        break;
                    case 1: // homeroom
                        Map homeroomToStaff = ReportUtils.buildHomeroomToStaffMap(getBroker(),
                                getOrganization(), isSchoolContext() ? getSchool() : null);
                        addParameter(HOMEROOM_TO_STAFF_MAP, homeroomToStaff);
                        break;
                    case 2: // houseCode
                        statistics.sort("house_code", false);
                        break;
                    case 3: // teamCode
                        statistics.sort("team_code", false);
                        break;
                }
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }

        return statistics;
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
     * Appends an ORDER BY clause to the given buffer based on the report parameters. The sort order
     * always ends with the student OID so the "flattening" will work properly.
     * <p>
     * Two fields may be prepended to the sort order depending upon other parameters (they will be
     * prepended in the given order if both conditions apply):
     * <ol>
     * <li>If not in the school context then prefix the ordering with the school name so the school
     * grouping in the Jasper format displays properly
     * <li>If grouping by homeroom then prefix the ordering with the homeroom so the homeroom
     * grouping in the Jasper format displays properly
     * </ol>
     *
     * @param queryString StringBuilder
     */
    private void appendOrderBy(StringBuilder queryString) {
        queryString.append("ORDER BY ");

        if (!isSchoolContext()) {
            queryString.append("SKL_SCHOOL_NAME, ");
        }

        if (((Integer) getParameter(GROUP_BY_PARAM)).intValue() == 1) {
            queryString.append("STD_HOMEROOM, ");
        }

        int sortCode = ((Integer) getParameter(SORT_ORDER_PARAM)).intValue();
        switch (sortCode) {
            case 0: // Name
                queryString.append("STD_NAME_VIEW, STD_YOG, STD_OID");
                break;

            case 1: // YOG
                queryString.append("STD_YOG, STD_NAME_VIEW, STD_OID");
                break;

            case 2: // Homeroom (we may have already added homeroom because of the grouping)
                if (((Integer) getParameter(GROUP_BY_PARAM)).intValue() != 1) {
                    queryString.append("STD_HOMEROOM, ");
                }
                queryString.append("STD_NAME_VIEW, STD_YOG, STD_OID");
                break;
        }
    }

    /**
     * Appends the subquery for the the given code criteria to the SQL buffer. The subquery begins
     * and ends with a space character.
     *
     * @param sqlBuffer StringBuilder
     * @param connector the connector (AND/OR) to use for linking the subquery to
     *        the parent
     * @param searchColumn the column to search
     * @param searchValue the value to search for
     * @param reason String
     * @param operator int
     * @param excludeExcused boolean
     * @param countValue String
     * @param index String
     * @param openParenthesis if true, an open parenthesis will be placed after the
     *        connector keyword (WHERE/AND/OR); note that the caller is
     *        responsible for adding the close parenthesis
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

            sqlBuffer.append(" WHERE ATT_DATE >= '" + m_startDate + "'");
            sqlBuffer.append(" AND ATT_DATE <= '" + m_endDate + "'");
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
                sqlBuffer.append(" GROUP BY ATT_STD_OID HAVING " + aggregateOperator + "(ATT_PORTION_ABSENT) ");
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
     * Builds a Student to StudentScheduleAttributes map. Map<Student, StudentScheduleAttributes>
     * <p>
     * Query Built Ex:
     * <p>
     * select *
     * from schedule
     * where ((SCH_START_DATE < Schedule.COL_END_DATE) and
     * (SCH_END_DATE > Schedule.COL_START_DATE))
     * and SCH_OID in
     * (select SKX_SCH_OID_ACTIVE
     * from school_schedule_context
     * where SKX_SKL_OID = 'SKL0000000101P')
     * order by SCH_END_DATE desc
     * 
     * @return Map
     */
    private Map buildStdToSsaMap() {
        X2Criteria subCriteria = new X2Criteria();

        if (isSchoolContext()) {
            subCriteria.addEqualTo(SchoolScheduleContext.COL_SCHOOL_OID, getSchool().getOid());
        }

        SubQuery subQuery =
                new SubQuery(SchoolScheduleContext.class, SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, subCriteria);

        X2Criteria criteria = new X2Criteria();
        criteria.addLessOrEqualThan(Schedule.COL_START_DATE, getParameter(END_DATE_PARAM));
        criteria.addGreaterOrEqualThan(Schedule.COL_END_DATE, getParameter(START_DATE_PARAM));
        criteria.addIn(X2BaseBean.COL_OID, subQuery);

        QueryByCriteria activeSchedulesInDateRange = new QueryByCriteria(Schedule.class, criteria);
        activeSchedulesInDateRange.addOrderBy(Schedule.COL_END_DATE, false);

        Schedule schedule = (Schedule) getBroker().getBeanByQuery(activeSchedulesInDateRange);

        X2Criteria finalCriteria = new X2Criteria();

        if (isSchoolContext()) {
            finalCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHOOL_OID, getSchool().getOid());
        }

        if (schedule != null) {
            finalCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_OID, schedule.getOid());
        } else {
            addNoMatchCriteria(finalCriteria);
        }

        QueryByCriteria finalQuery = new QueryByCriteria(StudentScheduleAttributes.class, finalCriteria);

        return getBroker().getMapByQuery(finalQuery, StudentScheduleAttributes.COL_STUDENT_OID, 150);
    }


    /**
     * Builds the attendance totals query for the absent, tardy, and dismissed counts as well as the
     * counts for each of the four codes. The query is limited to the report date range.
     *
     * @return StringBuilder
     */
    private StringBuilder buildStudentAttendanceTotalQuery() {
        StringBuilder queryString = new StringBuilder(512);

        /*
         * Absences
         */
        queryString.append(
                "SELECT ATT_STD_OID, '" + ABSENT_CODE_NAME + "' AS CODE_NAME, SUM(ATT_PORTION_ABSENT) AS TOTAL ");
        queryString.append("FROM STUDENT_ATTENDANCE ");
        queryString.append("WHERE ATT_DATE >= '" + m_startDate + "' ");
        queryString.append("AND ATT_DATE <= '" + m_endDate + "' ");
        queryString.append("AND ATT_ABSENT_IND = '1' ");

        if (((Boolean) getParameter(ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_ABSENT)).booleanValue()) {
            queryString.append("AND ATT_EXCUSED_IND = '0' ");
        }

        String reason = (String) getParameter(ATTRIBUTE_REASON_PARAM_ABSENT);
        if (!StringUtils.isEmpty(reason)) {
            queryString.append(" AND ATT_REASON_CODE = '" + reason + "'");
        }

        queryString.append("GROUP BY ATT_STD_OID ");

        /*
         * Tardies
         */
        queryString.append("UNION ");

        queryString.append("SELECT ATT_STD_OID, '" + TARDY_CODE_NAME + "' AS CODE_NAME, COUNT(*) AS TOTAL ");
        queryString.append("FROM STUDENT_ATTENDANCE ");
        queryString.append("WHERE ATT_DATE >= '" + m_startDate + "' ");
        queryString.append("AND ATT_DATE <= '" + m_endDate + "' ");
        queryString.append("AND ATT_TARDY_IND = '1' ");

        if (((Boolean) getParameter(ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_TARDY)).booleanValue()) {
            queryString.append("AND ATT_EXCUSED_IND = '0' ");
        }

        reason = (String) getParameter(ATTRIBUTE_REASON_PARAM_TARDY);
        if (!StringUtils.isEmpty(reason)) {
            queryString.append(" AND ATT_REASON_CODE = '" + reason + "'");
        }

        queryString.append("GROUP BY ATT_STD_OID ");

        /*
         * Dismissals
         */
        queryString.append("UNION ");

        queryString.append("SELECT ATT_STD_OID, '" + DISMISSAL_CODE_NAME + "' AS CODE_NAME, COUNT(*) AS TOTAL ");
        queryString.append("FROM STUDENT_ATTENDANCE ");
        queryString.append("WHERE ATT_DATE >= '" + m_startDate + "' ");
        queryString.append("AND ATT_DATE <= '" + m_endDate + "' ");
        queryString.append("AND ATT_DISMISSED_IND = '1' ");

        if (((Boolean) getParameter(ATTRIBUTE_EXCLUDE_EXCUSED_PARAM_DISMISSED)).booleanValue()) {
            queryString.append("AND ATT_EXCUSED_IND = '0' ");
        }

        reason = (String) getParameter(ATTRIBUTE_REASON_PARAM_DISMISSED);
        if (!StringUtils.isEmpty(reason)) {
            queryString.append(" AND ATT_REASON_CODE = '" + reason + "'");
        }

        queryString.append("GROUP BY ATT_STD_OID ");

        /*
         * Codes 1 through 4
         */
        for (int i = 0; i < CODE_COUNT; i++) {
            if (!isEmptyValue(m_codes[i])) {
                queryString.append("UNION ");

                queryString.append("SELECT ATT_STD_OID, '" + m_codes[i] + "' AS CODE_NAME, COUNT(*) AS TOTAL ");
                queryString.append("FROM STUDENT_ATTENDANCE ");
                queryString.append("WHERE ATT_DATE >= '" + m_startDate + "' ");
                queryString.append("AND ATT_DATE <= '" + m_endDate + "' ");
                queryString.append("AND (ATT_OTHER_CODE = '" + m_codes[i] + "' ");
                queryString.append("OR ATT_OTHER_CODE_02 = '" + m_codes[i] + "') ");

                reason = (String) getParameter(CODE_REASON_PARAM_PREFIX + i);
                if (!StringUtils.isEmpty(reason)) {
                    queryString.append(" AND ATT_REASON_CODE = '" + reason + "'");
                }

                queryString.append("GROUP BY ATT_STD_OID ");
            }
        }

        return queryString;
    }

    /**
     * Creates a DataGrid from the passed ResultSet to use as the report data source. The DataGrid
     * flattens the multiple rows for one code/one count to a single row with columns for each
     * count. The DataGrid contains the following columns:
     * <ul>
     * <li>std_oid: The OID of the Student bean
     * <li>student: The actual Student bean
     * <li>total_abs: The total number of absences
     * <li>total_trd: The total number of tardies
     * <li>total_dsm: The total number of dismissals
     * <li>total_01: The total number of occurrences of code 01
     * <li>total_02: The total number of occurrences of code 02
     * <li>total_03: The total number of occurrences of code 03
     * <li>total_04: The total number of occurrences of code 04
     * </ul>
     * This method also adds a report parameter for the total number of students in the grid.
     *
     * @param resultSet ResultSet
     * @param stdToSsaMap Map
     * @return ReportDataGrid
     * @throws SQLException exception
     */
    private ReportDataGrid formatData(ResultSet resultSet, Map stdToSsaMap) throws SQLException {
        ReportDataGrid data = new ReportDataGrid(1000, 9);

        String lastOid = null;
        while (resultSet.next()) {
            String studentOid = resultSet.getString("STD_OID");

            String houseCode = null;
            String teamCode = null;
            StudentScheduleAttributes ssa = (StudentScheduleAttributes) stdToSsaMap.get(studentOid);

            if (ssa != null) {
                houseCode = ssa.getScheduleHouseCode();
                teamCode = ssa.getScheduleTeamCode();
            }

            if (!studentOid.equals(lastOid)) {
                lastOid = studentOid;

                data.append();
                data.set("std_oid", studentOid);
                data.set("student", getBroker().getBeanByOid(SisStudent.class, studentOid));
                data.set("total_abs", Double.valueOf(0));
                data.set("total_trd", Double.valueOf(0));
                data.set("total_dsm", Double.valueOf(0));
                data.set("total_0", Double.valueOf(0));
                data.set("total_1", Double.valueOf(0));
                data.set("total_2", Double.valueOf(0));
                data.set("total_3", Double.valueOf(0));
                data.set("house_code", houseCode);
                data.set("team_code", teamCode);
            }

            String code = resultSet.getString("code_name");
            if (code != null) {
                if (code.equals(ABSENT_CODE_NAME)) {
                    data.set("total_abs", Double.valueOf(resultSet.getDouble("TOTAL")));
                } else if (code.equals(TARDY_CODE_NAME)) {
                    data.set("total_tdy", Double.valueOf(resultSet.getDouble("TOTAL")));
                } else if (code.equals(DISMISSAL_CODE_NAME)) {
                    data.set("total_dsm", Double.valueOf(resultSet.getDouble("TOTAL")));
                } else {
                    for (int i = 0; i < CODE_COUNT; i++) {
                        if (code.equals(m_codes[i])) {
                            data.set("total_" + i, Double.valueOf(resultSet.getDouble("TOTAL")));
                        }
                    }
                }
            }
        }

        return data;
    }

    /**
     * Returns true if the given value is null or the empty string.
     *
     * @param value String
     * @return boolean
     */
    private boolean isEmptyValue(String value) {
        return (value == null || value.length() == 0);
    }
}
