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
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.StaffPostAttendance;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Data source for the "lunch counts" report. This report displays total lunch counts for a school
 * and date.
 *
 * @author X2 Development Corporation
 */
public class LunchCountsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final String INPUT_PARAM_DATE = "date";

    private static final String COL_LABEL = "label";
    private static final String COL_COUNT = "count";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(5, 2);
        PlainDate date = (PlainDate) getParameter(INPUT_PARAM_DATE);

        String sql = "SELECT SUM(SPA_LUNCH_COUNT_01), " +
                " SUM(SPA_LUNCH_COUNT_02), " +
                " SUM(SPA_LUNCH_COUNT_03), " +
                " SUM(SPA_LUNCH_COUNT_04), " +
                " SUM(SPA_LUNCH_COUNT_05), " +
                " SUM(SPA_LUNCH_COUNT_06), " +
                " SUM(SPA_LUNCH_COUNT_07), " +
                " SUM(SPA_LUNCH_COUNT_08), " +
                " SUM(SPA_LUNCH_COUNT_09), " +
                " SUM(SPA_LUNCH_COUNT_10) " +
                " FROM STAFF_POST_ATTENDANCE " +
                "WHERE SPA_SKL_OID = ? " +
                "  AND SPA_ATTENDANCE_DATE = ? " +
                "  AND SPA_ATTENDANCE_TYPE = ? " +
                "GROUP BY SPA_SKL_OID";

        PreparedStatement statement = null;
        ResultSet results = null;
        Connection connection = getBroker().borrowConnection();
        try {
            statement = connection.prepareStatement(sql);
            statement.setString(1, getSchool().getOid());
            statement.setDate(2, date);
            statement.setInt(3, StaffPostAttendance.DAILY_ATTENDANCE);

            results = statement.executeQuery();

            if (results.next()) {
                int value1 = results.getInt(1);
                int value2 = results.getInt(2);
                int value3 = results.getInt(3);
                int value4 = results.getInt(4);
                int value5 = results.getInt(5);
                int value6 = results.getInt(6);
                int value7 = results.getInt(7);
                int value8 = results.getInt(8);
                int value9 = results.getInt(9);
                int value10 = results.getInt(10);

                String label1 = PreferenceManager.getPreferenceValue(getSchool(),
                        SisPreferenceConstants.ATT_LUNCH_COUNT_01);
                setLunchCount(grid, label1, value1);

                String label2 = PreferenceManager.getPreferenceValue(getSchool(),
                        SisPreferenceConstants.ATT_LUNCH_COUNT_02);
                setLunchCount(grid, label2, value2);

                String label3 = PreferenceManager.getPreferenceValue(getSchool(),
                        SisPreferenceConstants.ATT_LUNCH_COUNT_03);
                setLunchCount(grid, label3, value3);

                String label4 = PreferenceManager.getPreferenceValue(getSchool(),
                        SisPreferenceConstants.ATT_LUNCH_COUNT_04);
                setLunchCount(grid, label4, value4);

                String label5 = PreferenceManager.getPreferenceValue(getSchool(),
                        SisPreferenceConstants.ATT_LUNCH_COUNT_05);
                setLunchCount(grid, label5, value5);

                String label6 = PreferenceManager.getPreferenceValue(getSchool(),
                        SisPreferenceConstants.ATT_LUNCH_COUNT_06);
                setLunchCount(grid, label6, value6);

                String label7 = PreferenceManager.getPreferenceValue(getSchool(),
                        SisPreferenceConstants.ATT_LUNCH_COUNT_07);
                setLunchCount(grid, label7, value7);

                String label8 = PreferenceManager.getPreferenceValue(getSchool(),
                        SisPreferenceConstants.ATT_LUNCH_COUNT_08);
                setLunchCount(grid, label8, value8);

                String label9 = PreferenceManager.getPreferenceValue(getSchool(),
                        SisPreferenceConstants.ATT_LUNCH_COUNT_09);
                setLunchCount(grid, label9, value9);

                String label10 = PreferenceManager.getPreferenceValue(getSchool(),
                        SisPreferenceConstants.ATT_LUNCH_COUNT_10);
                setLunchCount(grid, label10, value10);
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();

            try {
                if (statement != null) {
                    statement.close();
                }
                if (results != null) {
                    results.close();
                }
            } catch (SQLException sqle) {
                AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
            }
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Sets a lunch total on the passed grid. The label parameter may be null or empty, in which
     * case it is not added to the grid.
     *
     * @param grid ReportDataGrid
     * @param label String
     * @param value int
     */
    private void setLunchCount(ReportDataGrid grid, String label, int value) {
        if (!StringUtils.isEmpty(label)) {
            grid.append();
            grid.set(COL_LABEL, label);
            grid.set(COL_COUNT, Integer.valueOf(value));
        }
    }
}
