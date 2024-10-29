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

import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;

/**
 * The Class MaSimpleFormData.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class MaSimpleFormData extends BaseFormReportJavaSource {
    private static final long serialVersionUID = 1L;
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private MaSpedAttribHelper m_attribHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        if (getFormOwner() != null) {
            m_attribHelper = new MaSpedAttribHelper(getBroker(), true);
            return m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        }
        return gatherEmptyReportData();
    }

    /**
     * Returns 'JREmptyDataSource' object accordnig to report's engine version
     *
     * @return
     */
    protected Object gatherEmptyReportData() {
        String engineVersion = ((Report) getJob().getTool()).getEngineVersion();
        if (Report.REPORT_ENGINE_1_RELEASE.equals(engineVersion)) {
            return new net.sf.jasperreports.engine.JREmptyDataSource();
        } else if (Report.REPORT_ENGINE_3_RELEASE.equals(engineVersion)) {
            return new net.sf.jasperreports3.engine.JREmptyDataSource();
        }
        return new net.sf.jasperreports5.engine.JREmptyDataSource();
    }


}
