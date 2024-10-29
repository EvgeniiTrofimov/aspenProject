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
package com.x2dev.reports.sys.sped.ct;

import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.utils.StringUtils;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Connecticut IEP form.
 *
 * @author X2 Development Corporation
 */
public class WorksheetData extends BaseFormReportJavaSource {

    // The subreports RPT_ID's
    private static final String PARAM_PAGE_1_FORMAT_ID = "SYS-STD-CT-ED63X-PG1";
    private static final String PARAM_PAGE_2_FORMAT_ID = "SYS-STD-CT-ED63X-PG2";

    // The subreports parameter name
    private static final String PARAM_PAGE1_FORMAT = "page1";
    private static final String PARAM_PAGE2_FORMAT = "page2";

    // Parameters to be passed into the report
    private static final String PARAM_PROGRESS_MONITORING = "progressMonitoring";
    private static final String PARAM_EVIDENCE = "evidence";

    private Map<String, Report> m_subReports = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        loadSubReports();

        preparePage1();
        preparePage2();

        SimpleFormDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        return dataSource;
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     * @return byte[]
     */
    private InputStream getSubreportFormat(String pageId) {
        Report report = m_subReports.get(pageId);
        return new ByteArrayInputStream(report.getCompiledFormat());
    }

    /**
     * Loads each IEP subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {PARAM_PAGE_1_FORMAT_ID,
                PARAM_PAGE_2_FORMAT_ID}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 4);
    }

    /**
     * Prepares page 1.
     */
    private void preparePage1() {
        ReportDataGrid grid = new ReportDataGrid();

        // get dictionary where values should be retrieved from
        ExtendedDataDictionary extendDictionary = ((GenericFormData) getFormStorage()).getExtendedDataDictionary();
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());

        // populate grid with data
        Collection<GenericFormChildData> childDataCollection =
                ((GenericFormData) getFormStorage()).getGenericFormDataChildren();

        for (GenericFormChildData childData : childDataCollection) {
            if (!StringUtils.isEmpty((String) childData.getFieldValueByAlias("ed63016-assessment", dictionary))) {
                grid.append();
                grid.set("assessment", childData.getFieldValueByAlias("ed63016-assessment", dictionary));
                grid.set("skills-targeted", childData.getFieldValueByAlias("ed63016-skills-targeted", dictionary));
                String rawDate = (String) childData.getFieldValueByAlias("ed63016-dates", dictionary);
                if (rawDate != null) {
                    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
                    SimpleDateFormat stringFormat = new SimpleDateFormat("MM/dd/yyyy");
                    Date date;

                    try {
                        date = dateFormat.parse(rawDate);
                        grid.set("date", stringFormat.format(date));
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                }
            }
        }

        // fill in rest of grid with empty rows
        if (grid.rowCount() < 5) {
            for (int i = grid.rowCount(); i < 5; i++) {
                grid.append();
            }
        }

        grid.beforeTop();

        addParameter(PARAM_PAGE1_FORMAT, getSubreportFormat(PARAM_PAGE_1_FORMAT_ID));
        addParameter(PARAM_PROGRESS_MONITORING, grid);

    }

    /**
     * Prepares page 2.
     */
    private void preparePage2() {
        ReportDataGrid grid = new ReportDataGrid();

        // get dictionary where values should be retrieved from
        ExtendedDataDictionary extendDictionary = ((GenericFormData) getFormStorage()).getExtendedDataDictionary();
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());

        // populate grid with data
        Collection<GenericFormChildData> childDataCollection =
                ((GenericFormData) getFormStorage()).getGenericFormDataChildren();

        for (GenericFormChildData childData : childDataCollection) {
            if (!StringUtils.isEmpty((String) childData.getFieldValueByAlias("ed63016-intense-interv", dictionary))) {
                grid.append();
                grid.set("intense-interv", childData.getFieldValueByAlias("ed63016-intense-interv", dictionary));
                grid.set("response", childData.getFieldValueByAlias("ed63016-response", dictionary));
                String rawDate = (String) childData.getFieldValueByAlias("ed63016-dates", dictionary);

                if (rawDate != null) {
                    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
                    SimpleDateFormat stringFormat = new SimpleDateFormat("MM/dd/yyyy");
                    Date date;

                    try {
                        date = dateFormat.parse(rawDate);
                        grid.set("date", stringFormat.format(date));
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                }
            }
        }

        if (grid.rowCount() < 7) {
            for (int i = grid.rowCount(); i < 7; i++) {
                grid.append();
            }
        }

        grid.beforeTop();

        addParameter(PARAM_PAGE2_FORMAT, getSubreportFormat(PARAM_PAGE_2_FORMAT_ID));
        addParameter(PARAM_EVIDENCE, grid);

    }
}
