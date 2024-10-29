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
package com.x2dev.reports.sys.sped.nh;

import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the New Hampshire transition planning form. This class is responsible for
 * retrieving the related IEP, and passing it to the form (used for rendering the form header).
 *
 * These must be retrieved in the Java source because they are from foreign extended dictionaries.
 * They are passed in as parameters.
 *
 * @author X2 Development Corporation
 */
public class TransitionPlanningFormData_Nh extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * ReportDataGrid Column ID: parameter map
     */
    private static final String COL_PARAMETER_MAP = "parameters";

    /**
     * ReportDataGrid Column ID: datasource
     */
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";

    /**
     * ReportDataGrid Column ID: format
     */
    private static final String COL_SUBREPORT_FORMAT = "format";

    /**
     * Format ID: Page 1 - Current status and Future Vision
     */
    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-NH-IEPT-1";

    /**
     * Format ID: Page 2 - School Year plan/Graduation expectations
     */
    private static final String PAGE_2_FORMAT_ID = "SYS-SPED-NH-IEPT-2";

    /**
     * Format ID: Page 3 - Transition Services and Responsibilities
     */
    private static final String PAGE_3_FORMAT_ID = "SYS-SPED-NH-IEPT-3";

    /**
     * Parameter: Iep (IepData.class)
     */
    private static final String PARAM_IEP = "iep";

    private Map m_subReports = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {

        ReportDataGrid grid = new ReportDataGrid();
        loadSubReports();

        preparePageGeneral(grid, PAGE_1_FORMAT_ID);
        preparePage2(grid, PAGE_2_FORMAT_ID);
        preparePageGeneral(grid, PAGE_3_FORMAT_ID);

        IepData iep = (IepData) getFormOwner();

        addParameter(PARAM_IEP, iep);

        grid.beforeTop();

        return grid;
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     * @return byte[]
     */
    private byte[] getSubreportFormat(String pageId) {
        Report report = (Report) m_subReports.get(pageId);
        return report.getCompiledFormat();
    }

    /**
     * Loads each subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {PAGE_1_FORMAT_ID,
                PAGE_2_FORMAT_ID,
                PAGE_3_FORMAT_ID}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

    /**
     * Prepares a general page.
     *
     * @param grid ReportDataGrid
     * @param formatId String
     */
    private void preparePageGeneral(ReportDataGrid grid, String formatId) {
        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(formatId));
        grid.set(COL_PARAMETER_MAP, getParameters());
    }

    /**
     * Prepares a general page.
     *
     * @param grid ReportDataGrid
     * @param formatId String
     */
    private void preparePage2(ReportDataGrid grid, String formatId) {
        Collection<X2BaseBean> yearPlans;
        if (isBlank()) {
            yearPlans = new ArrayList<X2BaseBean>(5);
        } else {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(GenericFormChildData.COL_GENERIC_FORM_DATA_OID, getFormStorage().getOid());

            QueryByCriteria query = new QueryByCriteria(GenericFormChildData.class, criteria);

            yearPlans = getBroker().getCollectionByQuery(query);
        }

        JRDataSource dataSource = new BeanCollectionDataSource(yearPlans,
                getDictionary(),
                getLocale());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(formatId));
        grid.set(COL_PARAMETER_MAP, getParameters());
    }

}
