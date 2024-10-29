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

import com.follett.fsc.core.framework.persistence.DatabaseOptimizer;
import com.follett.fsc.core.framework.persistence.DatabaseOptimizerFactory;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.utils.types.PlainDate;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the "Daily Attendance Totals" report. This report lists the daily
 * attendance code totals for a particular date range. The totals are split by excused/unexcused and
 * can be grouped by school and/or YOG. The resulting report looks like this:
 *
 * <pre>
 * +---------+-------+-------+-----+
 * | Code    |  Ex.  | Unex. | Ttl |
 * +=========+=======+=======+=====+
 * | Absent  |  100  |  200  | 300 |
 * +---------+-------+-------+-----+
 * | Tardy   |   52  |   24  |  76 |
 * +---------+-------+-------+-----+
 * | Dismiss |   64  |   23  |  87 |
 * +---------+-------+-------+-----+
 * </pre>
 *
 * @author X2 Development Corporation
 */
public class DailyAttendanceTotalsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "end date" input parameter. This value is a PlainDate object.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Name for the "end YOG" input parameter. This value is an Integer object.
     */
    public static final String END_YOG_PARAM = "endYog";

    /**
     * Name for the "group by school" input parameter. The value is a Boolean.
     */
    public static final String GROUP_BY_SCHOOL_PARAM = "groupBySchool";

    /**
     * Name for the "group by YOG" input parameter. The value is a Boolean.
     */
    public static final String GROUP_BY_YOG_PARAM = "groupByYog";

    /**
     * Parameter name for the map which contains the total number of attendance records in the
     * specified date range for each school in the report. This value is a java.util.Map object with
     * String keys and Long values.
     */
    public static final String SCHOOL_COUNTS_PARAM = "schoolCounts";

    /**
     * Name for the "start date" input parameter. This value is a PlainDate object.
     */
    public static final String START_DATE_PARAM = "startDate";

    /**
     * Name for the "start YOG" input parameter. This value is an Integer object.
     */
    public static final String START_YOG_PARAM = "startYog";

    /**
     * Name for the "sort" input parameter. This value is an Integer object.
     */
    public static final String SORT_ORDER_PARAM = "sort";

    private static final String DUMMY_SCHOOL_OID = "1";

    private PlainDate m_endDate = null;
    private Integer m_endYog = null;
    private String m_excusedColumn = null;
    private boolean m_groupBySchool = false;
    private boolean m_groupByYog = false;
    private PlainDate m_startDate = null;
    private Integer m_startYog = null;
    private String m_unexcusedColumn = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_startDate = (PlainDate) getParameter(START_DATE_PARAM);
        m_endDate = (PlainDate) getParameter(END_DATE_PARAM);

        m_startYog = (Integer) getParameter(START_YOG_PARAM);
        m_endYog = (Integer) getParameter(END_YOG_PARAM);

        DatabaseOptimizer optimizer = DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey());
        m_excusedColumn = optimizer.sqlIf("ATT_EXCUSED_IND = '1'", "1", "0");
        m_unexcusedColumn = optimizer.sqlIf("ATT_EXCUSED_IND = '1'", "0", "1");

        m_groupBySchool = ((Boolean) getParameter(GROUP_BY_SCHOOL_PARAM)).booleanValue();
        m_groupByYog = ((Boolean) getParameter(GROUP_BY_YOG_PARAM)).booleanValue();

        calculateSchoolCounts();

        return createGrid();
    }

    /**
     * Appends the totals query for the given code to the buffer. The query is limited to the report
     * date range and YOG range.
     *
     * @param codeColumn String
     * @param condition String
     * @param buffer this buffer with end with a space character
     * @return StringBuilder
     */
    private StringBuilder appendCodeQuery(String codeColumn, String condition, StringBuilder buffer) {
        buffer.append("SELECT ");

        if (m_groupBySchool) {
            buffer.append("SKL_OID, SKL_SCHOOL_NAME, ");
        } else {
            buffer.append("'" + DUMMY_SCHOOL_OID + "' AS SKL_OID, '' AS SKL_SCHOOL_NAME, ");
        }

        if (m_groupByYog) {
            buffer.append("STD_YOG, ");
        } else {
            buffer.append("1 AS STD_YOG, ");
        }

        buffer.append(codeColumn + " AS CODE, ");
        buffer.append("SUM(" + m_excusedColumn + ") AS EXCUSED, ");
        buffer.append("SUM(" + m_unexcusedColumn + ") AS UNEXCUSED, ");
        buffer.append("COUNT(*) AS TOTAL ");
        buffer.append("FROM STUDENT_ATTENDANCE ");
        buffer.append("INNER JOIN SCHOOL ON SKL_OID = ATT_SKL_OID ");
        buffer.append("INNER JOIN STUDENT ON STD_OID = ATT_STD_OID ");

        buffer.append("WHERE ");
        buffer.append(condition);
        buffer.append(" ");

        buffer.append("AND ATT_DATE  >= '" + m_startDate + "' ");
        buffer.append("AND ATT_DATE <= '" + m_endDate + "' ");

        if (m_startYog != null) {
            buffer.append("AND STD_YOG >= " + m_startYog.intValue() + " ");
        }

        if (m_endYog != null) {
            buffer.append("AND STD_YOG <= " + m_endYog.intValue() + " ");
        }

        if (isSchoolContext()) {
            buffer.append("AND ATT_SKL_OID = '" + getSchool().getOid() + "' ");
        } else {
            int level = getOrganization().getOrganizationDefinition().getLevel();
            buffer.append(" AND STD_ORG_OID_");
            buffer.append(level + 1);
            buffer.append(" = '");
            buffer.append(getOrganization().getOid());
            buffer.append("'");
            buffer.append("AND SKL_INACTIVE_IND = '0' ");
        }

        buffer.append("GROUP BY SKL_OID, SKL_SCHOOL_NAME");

        if (m_groupByYog) {
            buffer.append(", STD_YOG");
        }

        /*
         * The CODE column is either going to be:
         * 'A', 'T', 'D', ATT_OTHER_CODE1 or ATT_OTHER_CODE2.
         * This prevents grouping on those constants (which isn't necessary), while allowing
         * grouping on the "other" code columns.
         *
         * Note: MySQL allows grouping by column alias, but MSSQL does not.
         */
        if (!codeColumn.startsWith("'")) {
            buffer.append(", " + codeColumn);
        }


        return buffer;
    }

    /**
     * Calculates the total number of attendance records within the user-selected date range for
     * each school included in this report. This map is set as a tool input parameter for use on the
     * format.
     * <p>
     * These counts are necessary since the total number of attendance records is not the total of
     * all the individual code counts. For example, a single attendance record could have both the
     * ABSENT and TARDY flags set - two codes, one record.
     */
    private void calculateSchoolCounts() {
        Map schoolCounts = new HashMap(32);

        Criteria criteria = new Criteria();
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_startDate);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_endDate);

        if (m_startYog != null) {
            criteria.addGreaterOrEqualThan(StudentAttendance.REL_STUDENT + "." + SisStudent.COL_YOG, m_startYog);
        }

        if (m_endYog != null) {
            criteria.addLessOrEqualThan(StudentAttendance.REL_STUDENT + "." + SisStudent.COL_YOG, m_endYog);
        }

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(StudentAttendance.class));
            criteria.addEqualTo(StudentAttendance.REL_SCHOOL + "." + SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
        }

        String schoolOidColumn = m_groupBySchool ? StudentAttendance.COL_SCHOOL_OID : "'" + DUMMY_SCHOOL_OID + "'";
        String[] columns = new String[] {schoolOidColumn, "count(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentAttendance.class, columns, criteria);

        if (m_groupBySchool) {
            query.addGroupBy(StudentAttendance.COL_SCHOOL_OID);
        }

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                schoolCounts.put(record[0], record[1]);
            }
        } finally {
            iterator.close();
        }

        addParameter(SCHOOL_COUNTS_PARAM, schoolCounts);
    }

    /**
     * Creates the grid that contains the main data for this report.
     *
     * @return ReportDataGrid this value will be set to "before top"
     */
    private ReportDataGrid createGrid() {
        /*
         * Build the query
         */
        StringBuilder sql = new StringBuilder(3000);

        appendCodeQuery("'A'", "ATT_ABSENT_IND = '1'", sql);
        sql.append(" UNION ");
        appendCodeQuery("'T'", "ATT_TARDY_IND = '1'", sql);
        sql.append(" UNION ");
        appendCodeQuery("'D'", "ATT_DISMISSED_IND = '1'", sql);
        sql.append(" UNION ");

        // Note: The two code columns need to be wrapped in a derived table to combine the counts.
        sql.append(
                "SELECT SKL_OID, SKL_SCHOOL_NAME, STD_YOG, CODE, SUM(EXCUSED) AS EXCUSED, SUM(UNEXCUSED) AS UNEXECUSED, SUM(TOTAL) AS TOTAL FROM (");
        appendCodeQuery("ATT_OTHER_CODE", "ATT_OTHER_CODE IS NOT NULL AND ATT_OTHER_CODE <> ''", sql);
        sql.append(" UNION ");
        appendCodeQuery("ATT_OTHER_CODE_02", "ATT_OTHER_CODE_02 IS NOT NULL AND ATT_OTHER_CODE_02 <> ''", sql);
        sql.append(") A GROUP BY SKL_SCHOOL_NAME, SKL_OID, STD_YOG, CODE ");

        sql.append("ORDER BY SKL_SCHOOL_NAME, SKL_OID, STD_YOG, ");

        if (((Integer) getParameter(SORT_ORDER_PARAM)).intValue() == 0) {
            sql.append("CODE, TOTAL");
        } else {
            sql.append("TOTAL");
        }

        /*
         * Execute the query and load the results in a DataGrid.
         */
        ReportDataGrid grid = new ReportDataGrid(6);

        Connection connection = getBroker().borrowConnection();
        Statement statement = null;
        ResultSet resultSet = null;
        try {
            try {
                statement = connection.createStatement();
                resultSet = statement.executeQuery(sql.toString());

                grid.append(resultSet);
                grid.beforeTop();
            } finally {
                if (resultSet != null) {
                    try {
                        resultSet.close();
                    } catch (SQLException e) {
                        // ignore
                    }
                }

                if (statement != null) {
                    try {
                        statement.close();
                    } catch (SQLException e) {
                        // ignore
                    }
                }
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }

        return grid;
    }
}
