/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.sis.model.beans.SisStaff;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Specifies the sub-reports for the Age of Majority Notice report.
 *
 * @author X2 Development Corporation
 */
public class AgeOfMajorityNoticeData extends MultiPageFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static String STAFF_PARAM = "staff";
    private static String INPUT_OAM = "aom";

    private MaSpedAttribHelper m_attribHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        m_attribHelper = new MaSpedAttribHelper(getBroker(), true);

        ReportDataGrid grid = (ReportDataGrid) super.gatherData();

        grid.getColumns();

        String staffOid = (String) getFormStorage().getFieldValueByAlias("aom-From", getDictionary());
        SisStaff staff = (SisStaff) getBroker().getBeanByOid(SisStaff.class, staffOid);
        addParameter(STAFF_PARAM, staff);

        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#preparePage(com.follett.fsc.core.k12.tools.reports.ReportDataGrid)
     */
    @Override
    protected void preparePage(ReportDataGrid grid) {
        JRDataSource dataSource =
                m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        prepareCurrentPage(grid, dataSource);
    }

    /**
     * Gets the format ids.
     *
     * @return String[]
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#getFormatIds()
     */
    @Override
    protected String[] getFormatIds() {

        if (getParameter(INPUT_OAM) != null && getParameter(INPUT_OAM).equals("TPR")) {
            return new String[] {
                    "SYS-SPED-MA-AOM-SUB3",
                    "SYS-SPED-MA-AOM-SUB4",
                    "SYS-SPED-MA-AOM-SUB5"
            };
        } else if (getParameter(INPUT_OAM) != null && getParameter(INPUT_OAM).equals("parentalRights")) {
            return new String[] {
                    "SYS-SPED-MA-AOM-SUB3",
                    "SYS-SPED-MA-AOM-SUB4",
                    "SYS-SPED-MA-AOM-SUB5"
            };
        } else if (getParameter(INPUT_OAM) != null && getParameter(INPUT_OAM).equals("majorityNotice")) {
            return new String[] {
                    "SYS-SPED-MA-AOM-SUB1",
                    "SYS-SPED-MA-AOM-SUB2",
                    "SYS-SPED-MA-AOM-SUB1",
                    "SYS-SPED-MA-AOM-SUB2"
            };
        } else {
            return new String[] {
                    "SYS-SPED-MA-AOM-SUB1",
                    "SYS-SPED-MA-AOM-SUB2",
                    "SYS-SPED-MA-AOM-SUB1",
                    "SYS-SPED-MA-AOM-SUB2",
                    "SYS-SPED-MA-AOM-SUB3",
                    "SYS-SPED-MA-AOM-SUB4",
                    "SYS-SPED-MA-AOM-SUB5"
            };
        }
    }
}
