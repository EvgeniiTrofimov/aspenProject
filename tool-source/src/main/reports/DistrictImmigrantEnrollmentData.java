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

import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigInteger;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Prepares the data for the "Organization Enrollment" report. This report shows the totals for
 * active
 * students across the district with subtotals for schools and YOG.
 *
 * @author X2 Development Corporation
 */
public class DistrictImmigrantEnrollmentData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    DataDictionary m_dictionary = null;
    DataDictionaryField m_field = null;
    String dbFieldName = null;

    // Report fields
    private static final String FIELD_LANGUAGE = "LANGUAGE";
    private static final String FIELD_IMMIGRANT_TOTAL = "IMMIGRANT TOTAL";
    private static final String FIELD_IMMIGRANT_LEP_TOTAL = "IMMIGRANT LEP TOTAL";
    private static final String IMMIGRANT_STATUS_ALIAS = "DOE IMMIGRANT STATUS";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected PlainDate m_reportDate;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid();
        ResultSet resultSet = null;
        try {
            String LEP_CODE = "LEP";
            StringBuilder queryString = new StringBuilder(2000);

            queryString.append(
                    "SELECT A.LANG AS 'LANGUAGE', SUM(IMMTOT) AS 'IMMIGRANT TOTAL', SUM(LEPTOT) AS 'IMMIGRANT LEP TOTAL'");
            queryString.append(" FROM (");
            queryString.append(
                    "SELECT RCD.RCD_CODE, RCD.RCD_DESCRIPTION AS LANG, COUNT(STD.STD_OID) AS IMMTOT, 0 AS LEPTOT");
            queryString.append(" FROM REF_CODE RCD");
            queryString.append(" LEFT JOIN STUDENT STD");
            queryString.append(" ON STD.STD_HOME_LANGUAGE_CODE = RCD.RCD_CODE ");
            queryString.append(" WHERE "
                    + StudentManager.getActiveStudentDirectSQL(getOrganization(), "STD.STD_ENROLLMENT_STATUS"));
            queryString.append(" AND STD." + dbFieldName + " =  " + BooleanAsStringConverter.TRUE);
            queryString.append(" GROUP BY RCD.RCD_CODE, RCD.RCD_DESCRIPTION");
            queryString.append(" UNION");
            queryString.append(
                    " SELECT  RCD.RCD_CODE, RCD.RCD_DESCRIPTION AS Lang, 0 AS IMMTOT, COUNT(PGM.pgm_oid) AS LEPTOT");
            queryString.append(" FROM REF_CODE RCD");
            queryString.append(" LEFT JOIN STUDENT STD");
            queryString.append(" ON STD.STD_HOME_LANGUAGE_CODE = RCD.RCD_CODE");
            queryString.append(" LEFT JOIN STUDENT_PROGRAM_PARTICIPATION PGM");
            queryString.append(" ON STD.STD_OID = PGM.PGM_STD_OID");
            queryString.append(" WHERE "
                    + StudentManager.getActiveStudentDirectSQL(getOrganization(), "STD.STD_ENROLLMENT_STATUS"));
            queryString.append(" AND STD." + dbFieldName + " =  " + BooleanAsStringConverter.TRUE);
            queryString.append(" AND PGM.PGM_PROGRAM_CODE = '" + LEP_CODE + "' ");
            queryString.append(" AND PGM.PGM_START_DATE <= DATE_FORMAT('" + m_reportDate + "','%Y-%m-%d')");
            queryString.append(" AND ((PGM.PGM_END_DATE >= DATE_FORMAT('" + m_reportDate + "','%Y-%m-%d'))");
            queryString.append(" OR (PGM.PGM_END_DATE IS NULL))");
            queryString.append(" GROUP BY RCD.RCD_CODE, RCD.RCD_DESCRIPTION) ");
            queryString.append(" A WHERE A.LANG IS NOT NULL");
            queryString.append(" GROUP BY A.LANG");

            Connection connection = getBroker().borrowConnection();
            Statement statement = connection.createStatement();
            resultSet = statement.executeQuery(queryString.toString());

            while (resultSet.next()) {
                BigInteger immigrantTotal = resultSet.getBigDecimal("IMMIGRANT TOTAL").toBigInteger();
                BigInteger immigrantLepTotal = resultSet.getBigDecimal("IMMIGRANT LEP TOTAL").toBigInteger();
                grid.append();
                grid.set(FIELD_LANGUAGE, resultSet.getString("LANGUAGE"));
                grid.set(FIELD_IMMIGRANT_TOTAL, immigrantTotal.toString());
                grid.set(FIELD_IMMIGRANT_LEP_TOTAL, immigrantLepTotal.toString());

            }
            grid.append(resultSet);
            grid.beforeTop();
            resultSet.close();
            statement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }

        return grid;
    }


    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_field = m_dictionary.findDataDictionaryFieldByAlias(IMMIGRANT_STATUS_ALIAS);
        dbFieldName = m_field.getDatabaseName();
    }

}
