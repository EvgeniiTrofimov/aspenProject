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
package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import net.sf.jasperreports.engine.JREmptyDataSource;

/**
 * Data source for a Sub Report.
 *
 * @author X2 Development Corporation
 */
public class MaSpedEmptySubreportData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Returns an empty data source. For use with subreports.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected Object gatherData() {
        String engineVersion = ((Report) getJob().getTool()).getEngineVersion();
        if (Report.REPORT_ENGINE_1_RELEASE.equals(engineVersion)) {
            return new JREmptyDataSource();
        } else if (Report.REPORT_ENGINE_3_RELEASE.equals(engineVersion)) {
            return new net.sf.jasperreports3.engine.JREmptyDataSource();
        }
        return new net.sf.jasperreports5.engine.JREmptyDataSource();
    }
}
