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
 * The Class IepaFormBoundReport.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class IepaFormBoundReport extends MaBoundReports {
    private static final long serialVersionUID = 1L;
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final String FORMAT_ID_AMD1 = "SYS-SPED-MA-ADM1";
    private static final String FORMAT_ID_IEPA0 = "SYS-SPED-MA-IEPA0";
    private static final String INPUT_SPED_ADMIN = "spedAdministrator";
    private static final String INPUT_PRINT_ADM1 = "printAdm";

    private ReportDataContainer m_admContainer = null;

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
        m_admContainer = reportsContainer.getByToolId(FORMAT_ID_AMD1);
    }

    /**
     * Adds the reports.
     *
     * @param infos ReportDataInfos
     * @see com.x2dev.reports.sys.sped.ma.MaBoundReports#addReports(com.x2dev.reports.sys.sped.ma.MaBoundReports.ReportDataContainers)
     */
    @Override
    protected void addReportInfos(ReportDataInfos infos) {
        addParameter(MaBeanReport.PARAM_REPORT_IS_NOT_BLANK, !isBlank());
        boolean includeADM = ((Boolean) getParameter(INPUT_PRINT_ADM1)).booleanValue();
        if (includeADM) {
            infos.addById(FORMAT_ID_AMD1, "ADM1", MaBeanReport.PARAM_REPORT_IS_NOT_BLANK);
        }
        infos.addById(FORMAT_ID_IEPA0, "IEPA",
                INPUT_SPED_ADMIN + "," + MaBeanReport.PARAM_REPORT_IS_NOT_BLANK);
    }

    /**
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
}
