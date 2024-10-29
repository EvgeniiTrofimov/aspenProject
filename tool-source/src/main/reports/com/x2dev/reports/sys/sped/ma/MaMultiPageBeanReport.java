/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ma;

import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_DATA_SOURCE;
import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_FORMAT;
import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_PARAMETER_MAP;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import java.util.ArrayList;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class MaMultiPageBeanReport.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class MaMultiPageBeanReport extends MaBeanReport {
    private static final long serialVersionUID = 1L;
    /**
     * Boolean parameter specifying whether or not page numbers should be displayed at the bottom
     * of the report.
     *
     * @see MultiPageFormReportJavaSource.showPageNumbers()
     */
    public static final String PARAM_SHOW_PAGE_NUMBERS = "showPageNumber";

    protected static final String FIELD_PAGE_NUMBER = "pageNumber";

    private int m_currentPageNumber = 1;
    private Map m_subReports = null;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        loadSubReports();

        for (int i = 0; i < getPageCount(); i++) {
            preparePage(grid);
            m_currentPageNumber++;
        }

        grid.beforeTop();

        addParameter(PARAM_SHOW_PAGE_NUMBERS, Boolean.valueOf(showPageNumbers()));

        return grid;
    }

    /**
     * Returns the current page number (1-based).
     *
     * @return int
     */
    protected int getCurrentPageNumber() {
        return m_currentPageNumber;
    }

    /**
     * Returns the number of separate pages on this report.
     *
     * @return int
     */
    protected int getPageCount() {
        return getFormatIds().length;
    }

    /**
     * Returns the IDs of the formats used. This should not include an overflow page format.
     *
     * @return String[]
     */
    protected String[] getFormatIds() {
        return null;
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param formatId String
     * @return byte[]
     */
    protected byte[] getSubreportFormat(String formatId) {
        Report report = (Report) m_subReports.get(formatId);
        return report.getCompiledFormat();
    }

    /**
     * Returns the ID of the overflow format to use. If this method is overridden to return a
     * non-null value, an overflow page will be automatically supported using the provided format.
     *
     * @return String
     */
    protected String getOverflowFormatId() {
        return null;
    }

    /**
     * Prepares the current page of the grid with the passed data source.
     *
     * @param grid ReportDataGrid
     * @param dataSource JRDataSource
     */
    protected final void prepareCurrentPage(ReportDataGrid grid, JRDataSource dataSource) {
        grid.append();
        grid.set(FIELD_DATA_SOURCE, dataSource);
        grid.set(FIELD_FORMAT, getSubreportFormat(getFormatIds()[m_currentPageNumber - 1]));
        grid.set(FIELD_PARAMETER_MAP, getParameters());
        grid.set(FIELD_PAGE_NUMBER, Integer.valueOf(m_currentPageNumber));
    }

    /**
     * Prepares the first page.
     *
     * @param grid ReportDataGrid
     */
    protected void preparePage(ReportDataGrid grid) {
        String overflowFormatId = getOverflowFormatId();

        JRDataSource dataSource = null;
        if (overflowFormatId != null) {
            byte[] overflowFormat = getSubreportFormat(overflowFormatId);
            dataSource = new SimpleFormDataSource(getFormStorage(), getFormOwner(), grid, overflowFormat, null,
                    getDictionary(), getLocale());
        } else {
            dataSource = new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        }

        prepareCurrentPage(grid, dataSource);
    }

    /**
     * Returns true if page numbers should be displayed at the bottom of the report. This method is
     * provided for subclass overriding.
     *
     * @return boolean
     */
    protected boolean showPageNumbers() {
        return false;
    }

    /**
     * Loads each subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        String[] formatIds = getFormatIds();
        ArrayList ids = new ArrayList(formatIds.length + 1);
        for (int i = 0; i < formatIds.length; i++) {
            ids.add(formatIds[i]);
        }

        String overflowFormatId = getOverflowFormatId();
        if (overflowFormatId != null) {
            ids.add(overflowFormatId);
        }

        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, ids);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

}
