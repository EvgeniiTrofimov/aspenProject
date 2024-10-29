/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.ViewTemplate;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import org.apache.ojb.broker.query.Criteria;

/**
 * The Class checkSpedExitDate.
 */
public class ActivateNewIep extends ProcedureJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String IEP_TEMPLATE_OLD1_OID = "vwtX2IepMA";
    private static final String IEP_TEMPLATE_OLD2_OID = "vwtX2StdIepMA";
    private static final String IEP_TEMPLATE_NEW_OID = "vwtX2IepNMA";
    private static final String IEP_FORM_OID = "fmdMaIep";
    private static final String IEP_REPORT_ID_OLD = "SYS-SPED-MA-IEP";
    private static final String IEP_REPORT_ID_NEW = "SYS-SPED-MA-IEP-N";
    private static final String IEP_REPORT_SELECTION_ID_OLD = "SYS-SPED-MA-IEPSEL";
    private static final String IEP_REPORT_SELECTION_ID_NEW = "SYS-SPED-MA-IEP-LIST";

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        // Change old template name to "OLD".
        ViewTemplate template = getBroker().getBeanByOid(ViewTemplate.class, IEP_TEMPLATE_OLD1_OID);
        if (template != null) {
            template.setName("MA IEP Form OLD");
            getBroker().saveBeanForced(template);
        }

        // Change old template name to "OLD".
        template = getBroker().getBeanByOid(ViewTemplate.class, IEP_TEMPLATE_OLD2_OID);
        if (template != null) {
            template.setName("MA Standard IEP Form OLD");
            getBroker().saveBeanForced(template);
        }

        // Change new template name to "Standard".
        template = getBroker().getBeanByOid(ViewTemplate.class, IEP_TEMPLATE_NEW_OID);
        if (template != null) {
            template.setName("MA Standard IEP Form");
            getBroker().saveBeanForced(template);
            PreferenceManager.setPreferenceValue(getOrganization(), getBroker(), template.getContext(),
                    template.getOid());
        }

        // Change old report name to "OLD".
        Report report = getReport(IEP_REPORT_ID_OLD);
        if (report != null) {
            report.setName("IEP Form OLD");
            getBroker().saveBeanForced(report);
        }

        // Change new report name to "IEP Form".
        report = getReport(IEP_REPORT_ID_NEW);
        if (report != null) {
            report.setName("IEP Form");
            getBroker().saveBeanForced(report);

        }

        // Change old selection report name to "OLD".
        report = getReport(IEP_REPORT_SELECTION_ID_OLD);
        if (report != null) {
            report.setName("IEP Form - Selection OLD");
            getBroker().saveBeanForced(report);
        }

        // Change new report name to "IEP Form - Selection".
        report = getReport(IEP_REPORT_SELECTION_ID_NEW);
        if (report != null) {
            report.setName("IEP Form - Selection");
            getBroker().saveBeanForced(report);

        }

        // Change form to use new template and report.
        FormDefinition form = getBroker().getBeanByOid(FormDefinition.class, IEP_FORM_OID);
        if (form != null) {
            form.setReportId(IEP_REPORT_ID_NEW);
            form.setViewTemplateOid(IEP_TEMPLATE_NEW_OID);
            getBroker().saveBeanForced(form);
        }
    }

    /**
     * Return a Report bean by Report ID;
     *
     * @param reportId
     *
     * @return Report
     */
    private Report getReport(String reportId) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ID, reportId);
        BeanQuery query = new BeanQuery(Report.class, criteria);
        return getBroker().getBeanByQuery(query);
    }
}
