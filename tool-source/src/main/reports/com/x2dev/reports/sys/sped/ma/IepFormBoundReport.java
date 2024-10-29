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

import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.x2dev.sis.model.beans.IepData;
import java.util.List;

/**
 * The Class IepFormBoundReport.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class IepFormBoundReport extends MaBoundReports {
    private static final long serialVersionUID = 1L;
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final String INPUT_PARAMS_IEP_PAGES =
            "vision,plaepA,plaepB,goals,services,schedule,assessment,response";
    private static final String INPUT_PARAMS_IEP =
            "printAsDraft,responseOnly,spedAdministrator,printSettingColumn,transition," + INPUT_PARAMS_IEP_PAGES;
    private static final String RESPONSE_PRINT_ADM1 = "printAdm";
    private static final String RESPONSE_PRINT_RESPONSE_ONLY = "responseOnly";
    private static final String TRANSITION = "transition";

    private transient ReportDataContainer m_admContainer = null;

    /**
     * Adds the psuedo instances.
     *
     * @param container ReportDataContainer
     * @param targetFormInstances List<FormInstance>
     * @throws ToolRunException exception
     * @see com.x2dev.reports.sys.sped.ma.MaBoundReports#addPsuedoInstances(com.x2dev.reports.sys.sped.ma.MaBoundReports.ReportDataContainer,
     *      java.util.List)
     */
    @Override
    protected void addPsuedoInstances(ReportDataContainer container, List<FormInstance> targetFormInstances)
            throws ToolRunException {
        if (container == m_admContainer && targetFormInstances.isEmpty()) {
            IepData iep = (IepData) container.getReportDataContainers().getOwner();
            if (iep != null) {
                FormInstance pseudoInstance = getPsuedoAdmInstance(m_admContainer, iep);
                if (pseudoInstance != null) {
                    targetFormInstances.add(pseudoInstance);
                }
            }
        }
    }

    /**
     * Adds the reports.
     *
     * @param reportsContainer ReportDataContainers
     * @see com.x2dev.reports.sys.sped.ma.MaBoundReports#addReports(com.x2dev.reports.sys.sped.ma.MaBoundReports.ReportDataContainers)
     */
    @Override
    protected void addReports(ReportDataContainers reportsContainer) {
        super.addReports(reportsContainer);
        m_admContainer = reportsContainer.getByToolId("SYS-SPED-MA-ADM1");
    }


    /**
     * @see com.x2dev.reports.sys.sped.ma.MaBoundReports#addReportInfos(com.x2dev.reports.sys.sped.ma.MaBoundReports.ReportDataInfos)
     */
    @Override
    protected void addReportInfos(ReportDataInfos infos) {
        boolean includeADM = ((Boolean) getParameter(RESPONSE_PRINT_ADM1)).booleanValue();
        boolean responseOnly = ((Boolean) getParameter(RESPONSE_PRINT_RESPONSE_ONLY)).booleanValue();
        addParameter(MaBeanReport.PARAM_REPORT_IS_NOT_BLANK, !isBlank());
        if (includeADM) {
            infos.addById("SYS-SPED-MA-ADM1", "ADM1", MaBeanReport.PARAM_REPORT_IS_NOT_BLANK);
        }

        if (isAnyIEPReportSelected()) {
            infos.addById("SYS-SPED-MA-IEP0", "IEP",
                    INPUT_PARAMS_IEP + "," + MaBeanReport.PARAM_REPORT_IS_NOT_BLANK);
        }

        if (!responseOnly && (Boolean) getParameter(TRANSITION)) {
            infos.addById("SYS-SPED-MA-28M9", "TPF", MaBeanReport.PARAM_REPORT_IS_NOT_BLANK);
        }
    }

    /**
     * }
     * Gets the psuedo adm instance.
     *
     * @param container ReportDataContainer
     * @param iep IepData
     * @return Form instance
     */
    private FormInstance getPsuedoAdmInstance(ReportDataContainer container, IepData iep) {
        FormInstance instance = null;
        FormDefinition formDefinition =
                container.getReportDataContainers().getFormDefinitionByFormId(container.getFormId());
        if (formDefinition != null) {
            instance = X2BaseBean.newInstance(FormInstance.class, getBroker().getPersistenceKey());
            instance.setFormDefinitionOid(formDefinition.getOid());
            instance.setOwnerObjectOid(iep.getOid());
            instance.setStorageObjectOid(iep.getStudent().getOid());
        }
        return instance;
    }

    /**
     * Checks if any pages is selected between IEP-1 to IEP-8
     *
     * @return
     */
    private boolean isAnyIEPReportSelected() {
        String[] iepPageName = INPUT_PARAMS_IEP_PAGES.split(",");
        for (int i = 0; i < iepPageName.length; i++) {
            if ((Boolean) getParameter(iepPageName[i])) {
                return true;
            }
        }
        return false;
    }

}
