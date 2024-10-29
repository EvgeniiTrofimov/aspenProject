/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2022 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.procedures.statereporting.CRDCDataHelper.Dataset;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The Class CRDCExportDataset.
 */
public class CRDCExportDataset extends ExportJavaSource {
    private static final String INPUT_PARAM_FILTER = "filter";
    private static final String INPUT_PARAM_SCHOOL = "schoolOid";
    private static final String INPUT_PARAM_REPORT_DATE = "reportDate";

    private static final String INPUT_PARAM_RESULT_OID_STD = "stdResultOid";
    private static final String INPUT_PARAM_RESULT_OID_SKL = "sklResultOid";
    private static final String INPUT_PARAM_RESULT_OID_MST = "mstResultOid";
    private static final String INPUT_PARAM_RESULT_OID_STD_MST = "stdMstResultOid";
    private static final String INPUT_PARAM_RESULT_OID_STF = "stfResultOid";

    protected CRDCDataHelper m_crdcDataHelper = null;

    private static final List<String> COLUMNS = Arrays.asList("Descr");

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid();

        m_crdcDataHelper = new CRDCDataHelper(getBroker(), getResultsCriteria());
        m_crdcDataHelper.setReportDate((PlainDate) getParameter(INPUT_PARAM_REPORT_DATE));
        SisSchool school =
                (SisSchool) getBroker().getBeanByOid(SisSchool.class, (String) getParameter(INPUT_PARAM_SCHOOL));
        Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
        filtersByFieldName.put(Dataset.SCHOOL, school.getOid());
        List<ExportFormatRow> rows = m_crdcDataHelper.getFilteredRows(Dataset.STUDENT,
                (String) getParameter(INPUT_PARAM_FILTER), filtersByFieldName);

        for (ExportFormatRow item : rows) {
            grid.append();
            grid.set(COLUMNS.get(0), item.getDescription());
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return COLUMNS;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return COLUMNS;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Gets the results criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getResultsCriteria() {
        Collection<String> resultsOids = new ArrayList<String>();

        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_STD));
        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_SKL));
        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_MST));
        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_STD_MST));
        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_STF));

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(X2BaseBean.COL_OID, resultsOids);

        return criteria;
    }
}
