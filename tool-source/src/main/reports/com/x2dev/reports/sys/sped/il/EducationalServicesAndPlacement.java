/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.il;

import static com.x2dev.reports.sys.sped.il.IlSpedHelper.KEY_DATASOURCE;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.KEY_FORMAT;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.SUB_REPORT_ID_PQESY;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.SUB_REPORT_ID_PQSEC1;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.reports.sys.sped.il.IlSpedHelper.PQSectionType;
import com.x2dev.sis.model.beans.IepData;
import java.io.ByteArrayInputStream;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class EducationalServicesAndPlacement extends BeanReport {


    private IlSpedHelper m_spedHelper = new IlSpedHelper();
    private static final String PARAM_OWNER_OID = "owner.oid";
    private static final String PARAM_OWNER = "owner";
    private static final long serialVersionUID = 1L;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.reports.sys.sped.il.BeanReport#fillGrid(Map, Locale)
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid();
        m_spedHelper.initializeHelper(getBroker(), getDictionary());
        IepData iepData = (IepData) getFormStorage();
        if (iepData != null && iepData.getOid() != null) {
            m_spedHelper.fillSection(iepData, grid, getParameters(), PQSectionType.CURRENT);
            m_spedHelper.fillExtendedYearServiceBlock(iepData, grid, getParameters());
            m_spedHelper.fillSection(iepData, grid, getParameters(), PQSectionType.NEXT);
        } else {
            fillEmptySubReportsIfRunBlank(grid);
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * Fill empty sub reports if run blank.
     *
     * @param grid ReportDataGrid
     */
    private void fillEmptySubReportsIfRunBlank(ReportDataGrid grid) {
        IepData iepOwner = (IepData) getFormOwner();
        JRDataSource subgrid = m_spedHelper.new PQDataSource(iepOwner, iepOwner, getDictionary(),
                m_spedHelper.getDefaultLocale(), PQSectionType.CURRENT);
        fillEmptySubreportByReportId(grid, SUB_REPORT_ID_PQSEC1, subgrid);

        ReportDataGrid subgridESY = new ReportDataGrid();
        for (int i = 0; i < 4; i++) {
            subgridESY.append();
        }
        subgridESY.beforeTop();
        fillEmptySubreportByReportId(grid, SUB_REPORT_ID_PQESY, subgridESY);
    }

    /**
     * Fill empty subreport by report id.
     *
     * @param grid ReportDataGrid
     * @param subReportId String
     * @param subgrid JRDataSource
     */
    private void fillEmptySubreportByReportId(ReportDataGrid grid, String subReportId, JRDataSource subgrid) {
        Report subreport = ReportUtils.getReport(subReportId, getBroker());
        ByteArrayInputStream format = new ByteArrayInputStream(subreport.getCompiledFormat());
        grid.append();
        grid.set(KEY_DATASOURCE, subgrid);
        grid.set(KEY_FORMAT, format);
        Map<String, Object> mapForReport = new HashMap<String, Object>(getParameters());
        // KEY_FORMAT it used key which already exist in getParameters, like result if it dindn't
        // override - can appear exception when print blank report
        // temporary I override key. but in future will more correctly rename key and investigate
        // where it used
        mapForReport.put(KEY_FORMAT, null);
        mapForReport.put(PARAM_OWNER, getFormOwner());
        mapForReport.put(PARAM_OWNER_OID, getFormOwner() != null ? getFormOwner().getOid() : null);
        grid.set(IlSpedHelper.KEY_PARAMETERS_MAP, mapForReport);
    }

}
