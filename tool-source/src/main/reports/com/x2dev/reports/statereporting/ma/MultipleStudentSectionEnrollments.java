/* ++++ Upgraded by PhoenixUpgradeProcedure ++++ on Tue Jan 17 12:43:49 EST 2012 *//*
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ============
                                                                                    *
                                                                                    * X2 Development
                                                                                    * Corporation
                                                                                    *
                                                                                    * Copyright (c)
                                                                                    * 2002-2008 X2
                                                                                    * Development
                                                                                    * Corporation.
                                                                                    * All rights
                                                                                    * reserved.
                                                                                    *
                                                                                    * Redistribution
                                                                                    * and use in
                                                                                    * source and
                                                                                    * binary forms,
                                                                                    * with or
                                                                                    * without
                                                                                    * modification,
                                                                                    * is not
                                                                                    * permitted
                                                                                    * without
                                                                                    * express
                                                                                    * written
                                                                                    * agreement
                                                                                    * from X2
                                                                                    * Development
                                                                                    * Corporation.
                                                                                    *
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ============
                                                                                    */

package com.x2dev.reports.statereporting.ma;

import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.DataGrid;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Procedure to execute a user-entered query. The procedure handles SELECT, UPDATE, and DELETE
 * queries. It is not intended to be used for INSERT queries.
 * <p>
 * The results of an UPDATE and DELETE query are given in the number of rows effected.
 * <p>
 * The results of a SELECT query are displayed to the user in a grid display on the output.
 *
 * @author X2 Development Corporation
 */
public class MultipleStudentSectionEnrollments extends ProcedureJavaSource {
    /**
     * Only search current year?
     */
    public static final String CURRENT_YEAR_PARAM = "currentYear";

    /*
     * Additional constants
     */
    private static final String SPACE = " ";
    private static final Pattern WHITESPACE_PATTERN = Pattern.compile("\\s+");

    // public enum QueryType { SELECT, UPDATE, DELETE, INSERT }



    // 1. Find all Student-Section Oids that have multiple SSC records (Duplicate enrollment
    // subquery)
    // 2. Join Student and Section Oid back to SSC table to get the associated SSC detail records
    // for just those Student-Sections
    // 3. Join to Student and MasterSchedule to get various columns
    // 4. Optionally join to Schedule then Organization to limit to current year context
    /*
     * SELECT ssc_std_oid, ssc_mst_oid, ssc_oid, ssc_guid, std.STD_NAME_VIEW, mst.MST_DESCRIPTION ,
     * mst.MST_SECTION_NUMBER, std.STD_ENROLLMENT_STATUS
     * FROM student_schedule ssc
     * INNER JOIN
     * ( SELECT ssc_mst_oid inner_mst_oid , SSC_STD_OID inner_std_oid
     * FROM student_schedule
     * GROUP BY ssc_mst_oid, ssc_std_oid
     * HAVING COUNT(*) > 1)
     * AS inner_alias ON inner_mst_oid = ssc_mst_oid AND inner_std_oid = ssc_std_oid
     * INNER JOIN student STD ON std.std_oid = ssc.ssc_std_oid
     * INNER JOIN schedule_master mst ON mst.mst_oid = ssc.ssc_mst_oid
     * INNER JOIN SCHEDULE sch ON sch.sch_oid = ssc.ssc_sch_oid
     * INNER JOIN organization ON sch.sch_ctx_oid = org_ctx_oid_current
     */

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String currentYearOnly = (String) getParameter(CURRENT_YEAR_PARAM);
        StringBuilder sqlBuilder = new StringBuilder();

        sqlBuilder.append(
                "SELECT SSC_STD_OID, SSC_MST_OID, SSC_OID, SSC_GUID, STD_NAME_VIEW, MST_DESCRIPTION, MST_SECTION_NUMBER, STD_ENROLLMENT_STATUS, SCH_START_DATE ");
        sqlBuilder.append("FROM STUDENT_SCHEDULE ssc ");
        sqlBuilder.append("INNER JOIN ");
        sqlBuilder.append("   ( SELECT SSC_MST_OID inner_mst_oid, SSC_STD_OID inner_std_oid ");
        sqlBuilder.append("     FROM STUDENT_SCHEDULE ");
        sqlBuilder.append("     GROUP BY SSC_MST_OID, SSC_STD_OID ");
        sqlBuilder.append("     HAVING COUNT(*) > 1) AS subquery_alias ");
        sqlBuilder.append("ON inner_mst_oid = SSC_MST_OID AND inner_std_oid = SSC_STD_OID ");
        sqlBuilder.append("INNER JOIN STUDENT STD ON STD_OID = SSC_STD_OID  ");
        sqlBuilder.append("INNER JOIN SCHEDULE_MASTER mst ON MST_OID = SSC_MST_OID ");
        sqlBuilder.append("INNER JOIN SCHEDULE sch ON SCH_OID = SSC_SCH_OID ");
        if ("true".equals(currentYearOnly)) {
            sqlBuilder.append("INNER JOIN ORGANIZATION ON SCH_CTX_OID = ORG_CTX_OID_CURRENT ");
        }
        sqlBuilder.append("; ");

        String sql = sqlBuilder.toString();
        sql = WHITESPACE_PATTERN.matcher(sql).replaceAll(SPACE);

        Connection connection = null;
        try {
            DataGrid grid = new DataGrid();

            connection = getBroker().borrowConnection();
            Statement statement = connection.createStatement();
            try {
                ResultSet results = statement.executeQuery(sql);
                try {
                    grid.append(results);
                } finally {
                    results.close();
                }
            } finally {
                statement.close();
            }

            grid.beforeTop();

            /*
             * Go through the grid and set all the null values to have a string value of
             * "NULL". Otherwise, it will be impossible to distinguish between blanks and
             * nulls in the results.
             */
            List<String> columnHeaders = grid.getColumns();
            while (grid.next()) {
                for (String columnHeader : columnHeaders) {
                    if (grid.get(columnHeader) == null) {
                        grid.set(columnHeader, "NULL");
                    }
                }
            }

            grid.beforeTop();

            logMessage(grid.toString());
        } catch (SQLException sqle) {
            logMessage(sqle.toString());
        } finally {
            if (connection != null) {
                getBroker().returnConnection();
            }
        }
    }
}
