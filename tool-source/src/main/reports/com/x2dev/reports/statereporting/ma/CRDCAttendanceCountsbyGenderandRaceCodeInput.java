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
package com.x2dev.reports.statereporting.ma;

import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.DataGrid;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.regex.Pattern;

/**
 * NOTE Quick-and-dirty procedure to run a raw SQL query that produces counts of attendance by
 * gender and race.
 * Based on SqlExecuter.java in reports project
 * June 2015 by Carl Szabo
 */


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
public class CRDCAttendanceCountsbyGenderandRaceCodeInput extends ProcedureJavaSource {
    /**
     * Name for the "query selector" input parameter. This value is a String.
     */
    public static final String QUERY_TYPE_PARAM = "queryType";

    /*
     * Additional constants
     */
    private static final String SPACE = " ";
    private static final Pattern WHITESPACE_PATTERN = Pattern.compile("\\s+");
    private static final String START_DATE_PARAM = "startDate";
    private static final String END_DATE_PARAM = "endDate";

    // public enum QueryType { SELECT, UPDATE, DELETE, INSERT }


    String m_startDateStr = null;
    String m_endDateStr = null;
    SimpleDateFormat m_dateFormat = new SimpleDateFormat("YYYY-MM-dd");

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        m_startDateStr = m_dateFormat.format(getParameter(START_DATE_PARAM));
        m_endDateStr = m_dateFormat.format(getParameter(END_DATE_PARAM));

        String queryType = (String) getParameter(QUERY_TYPE_PARAM);
        StringBuilder sqlBuilder = new StringBuilder();

        if (queryType.equals("byStudent")) {
            sqlBuilder.append("SELECT gender, race");
            sqlBuilder.append(", stdOid, nameview, absences ");
            sqlBuilder.append("FROM ");
            sqlBuilder.append("( ");
            sqlBuilder.append("SELECT ");

            sqlBuilder.append("std.STD_OID stdOid, psn.PSN_OID, std.STD_NAME_VIEW nameview, ");
            sqlBuilder.append("psn.PSN_GENDER_CODE gender, psn.PSN_RACE_VIEW race, COUNT(att.ATT_OID) absences ");
            sqlBuilder.append("FROM STUDENT std ");
            sqlBuilder.append("INNER JOIN PERSON psn ");
            sqlBuilder.append("ON std.STD_PSN_OID = psn.PSN_OID ");
            sqlBuilder.append("INNER JOIN STUDENT_ATTENDANCE att ");
            sqlBuilder.append("ON att.ATT_STD_OID = std.STD_OID ");
            sqlBuilder.append("AND att.ATT_ABSENT_IND = 1 ");
            sqlBuilder.append("AND att.ATT_DATE >= '" + m_startDateStr + "' ");
            sqlBuilder.append("AND att.ATT_DATE <= '" + m_endDateStr + "' ");
            sqlBuilder.append("GROUP BY std.STD_OID, psn.PSN_OID, std.STD_NAME_VIEW, ");
            sqlBuilder.append("psn.PSN_GENDER_CODE, psn.PSN_RACE_VIEW ");
            sqlBuilder.append(") AS studentWithAbsenceCount ");
            sqlBuilder.append("WHERE absences >= 15 ");
            sqlBuilder.append("ORDER BY gender, race, nameview ");
            sqlBuilder.append(";");
        } else if (queryType.equals("byGenderRace")) {
            sqlBuilder.append("SELECT gender, race");
            sqlBuilder.append(", COUNT(stdOid) studentCount ");
            sqlBuilder.append("FROM ");
            sqlBuilder.append("( ");
            sqlBuilder.append("SELECT ");

            sqlBuilder.append("std.STD_OID stdOid, psn.PSN_OID, std.STD_NAME_VIEW nameview, ");
            sqlBuilder.append("psn.PSN_GENDER_CODE gender, psn.PSN_RACE_VIEW race, COUNT(att.ATT_OID) absences ");
            sqlBuilder.append("FROM STUDENT std ");
            sqlBuilder.append("INNER JOIN PERSON psn ");
            sqlBuilder.append("ON std.STD_PSN_OID = psn.PSN_OID ");
            sqlBuilder.append("INNER JOIN STUDENT_ATTENDANCE att ");
            sqlBuilder.append("ON att.ATT_STD_OID = std.STD_OID ");
            sqlBuilder.append("AND att.ATT_ABSENT_IND = 1 ");
            sqlBuilder.append("AND att.ATT_DATE >= '" + m_startDateStr + "' ");
            sqlBuilder.append("AND att.ATT_DATE <= '" + m_endDateStr + "' ");
            sqlBuilder.append("GROUP BY std.STD_OID, psn.PSN_OID, std.STD_NAME_VIEW, ");
            sqlBuilder.append("psn.PSN_GENDER_CODE, psn.PSN_RACE_VIEW ");
            sqlBuilder.append(") AS studentWithAbsenceCount ");
            sqlBuilder.append("WHERE absences >= 15 ");
            sqlBuilder.append("GROUP BY gender, race ");
            sqlBuilder.append("; ");

        }

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
