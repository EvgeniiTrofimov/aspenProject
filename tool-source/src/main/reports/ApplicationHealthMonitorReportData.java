/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import net.sf.jasperreports5.engine.JREmptyDataSource;

/**
 * Basic report to be used for Application Health Monitor 'monitoring'.
 *
 * Report makes one database query for the root organization name to display on the report output.
 * The report output will also display the report name: "Application Health Monitor Report"
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class ApplicationHealthMonitorReportData extends ReportJavaSourceNet {

    public static final String PARAM_ORG_NAME = "rootOrgName";
    public static final String PARAM_REPORT_NAME = "reportName";
    public static final String REPORT_NAME = "Application Health Monitor Report";

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        addParameter(PARAM_ORG_NAME, OrganizationManager.getRootOrganization(getBroker()).getName());
        addParameter(PARAM_REPORT_NAME, REPORT_NAME);

        return new JREmptyDataSource();
    }

}
