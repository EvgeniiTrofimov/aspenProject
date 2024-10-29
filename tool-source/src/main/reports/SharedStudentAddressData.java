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

import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Data source for the "Shared Student Addresses" report. This report lists the physical addresses
 * for all students in the district (and, optionally, school) ordered by address so as to facilitate
 * address "clean up" - the merging of duplicate records.
 *
 * @author X2 Development Corporation
 */
public class SharedStudentAddressData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "Check link 1" input parameter. This value is a Boolean.
     */
    public static final String LINE1_CHECK = "line1";

    /**
     * Name for the "Check link 2" input parameter. This value is a Boolean.
     */
    public static final String LINE2_CHECK = "line2";

    /**
     * Name for the "Check link 3" input parameter. This value is a Boolean.
     */
    public static final String LINE3_CHECK = "line3";

    private boolean m_checkLine1 = false;
    private boolean m_checkLine2 = false;
    private boolean m_checkLine3 = false;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws X2BaseException {
        m_checkLine1 = ((Boolean) getParameter(LINE1_CHECK)).booleanValue();
        m_checkLine2 = ((Boolean) getParameter(LINE2_CHECK)).booleanValue();
        m_checkLine3 = ((Boolean) getParameter(LINE3_CHECK)).booleanValue();

        ReportDataGrid grid = new ReportDataGrid(8);

        StringBuilder sqlBuilder = new StringBuilder(
                "SELECT STD_YOG, STD_NAME_VIEW, SKL_SCHOOL_NAME, ADR_CITY, ADR_STREET_NAME, ADR_STREET_NUMBER,")
                        .append(" ADR_ADDRESS_LINE_01, ADR_ADDRESS_LINE_02, ADR_ADDRESS_LINE_03")
                        .append(" FROM SCHOOL")
                        .append(" INNER JOIN STUDENT ON STD_SKL_OID = SKL_OID")
                        .append(" INNER JOIN PERSON ON PSN_OID = STD_PSN_OID")
                        .append(" INNER JOIN PERSON_ADDRESS ON ADR_OID = PSN_ADR_OID_PHYSICAL");
        if (isSchoolContext()) {
            sqlBuilder.append(" WHERE SKL_OID = '" + getSchool().getOid() + "'");
        } else {
            int level = getOrganization().getOrganizationDefinition().getLevel();
            sqlBuilder.append(" WHERE STD_ORG_OID_");
            sqlBuilder.append(level + 1);
            sqlBuilder.append(" = '");
            sqlBuilder.append(getOrganization().getOid());
            sqlBuilder.append("'");
        }
        sqlBuilder.append(
                " ORDER BY ADR_CITY, ADR_STREET_NAME, ADR_STREET_NUMBER, ADR_ADDRESS_LINE_01, ADR_ADDRESS_LINE_02, STD_NAME_VIEW, STD_YOG");

        ResultSet results = null;
        Statement statement = null;
        try {
            Connection connection = getBroker().borrowConnection();

            try {
                statement = connection.createStatement();
                results = statement.executeQuery(sqlBuilder.toString());

                String lastKey = null;

                while (results.next()) {
                    HashMap record = new HashMap();

                    record.put("STD_NAME_VIEW", results.getString("STD_NAME_VIEW"));
                    record.put("STD_YOG", String.valueOf(results.getObject("STD_YOG")));
                    record.put("SKL_SCHOOL_NAME", results.getString("SKL_SCHOOL_NAME"));

                    String line1 = results.getString("ADR_ADDRESS_LINE_01");
                    String line2 = results.getString("ADR_ADDRESS_LINE_02");
                    String line3 = results.getString("ADR_ADDRESS_LINE_03");

                    record.put("ADR_ADDRESS_LINE_01", line1);
                    record.put("ADR_ADDRESS_LINE_02", line2);
                    record.put("ADR_ADDRESS_LINE_03", line3);

                    record.put("FIRST_IN_GROUP", Boolean.FALSE);
                    record.put("LAST_IN_GROUP", Boolean.FALSE);

                    String key = getAddressKey(line1, line2, line3);
                    if (!StringUtils.isEmpty(key)) {
                        if (!key.equals(lastKey)) {
                            record.put("FIRST_IN_GROUP", Boolean.TRUE);

                            if (!grid.isEmpty()) {
                                grid.set("LAST_IN_GROUP", Boolean.TRUE);
                            }
                        }

                        grid.append(record);
                    }

                    lastKey = key;
                }

                if (!grid.isEmpty()) {
                    grid.set("LAST_IN_GROUP", Boolean.TRUE);
                }
            } finally {
                if (results != null) {
                    results.close();
                }

                if (statement != null) {
                    statement.close();
                }
            }
        } catch (SQLException sqle) {
            throw new X2BaseException(sqle);
        } finally {
            getBroker().returnConnection();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Returns the key for the given address components based on the input parameters. Any or all of
     * the address components can be null.
     *
     * @param line1 String
     * @param line2 String
     * @param line3 String
     * @return String this value will not be null
     */
    private String getAddressKey(String line1, String line2, String line3) {
        String key = "";

        if (m_checkLine1 && line1 != null) {
            key += line1;
        }

        if (m_checkLine2 && line2 != null) {
            key += line2;
        }

        if (m_checkLine3 && line3 != null) {
            key += line3;
        }

        return key;
    }
}
