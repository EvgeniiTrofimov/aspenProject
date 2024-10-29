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
package com.x2dev.procedures.statereporting.on.tool;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.apache.commons.lang3.StringUtils;

/**
 * @author Follett Software Company
 * @copyright 2018
 */
public class RemoveTools extends ProcedureJavaSource {
    /**
     * Input parameters.
     */
    private static final String INPUT_PARAM_EXPORT_IDS = "exportIds";
    private static final String INPUT_PARAM_EXPORT_FORMAT_IDS = "exportFormatIds";
    private static final String INPUT_PARAM_IMPORT_IDS = "importIds";
    private static final String INPUT_PARAM_PROCEDURE_IDS = "procedureIds";
    private static final String INPUT_PARAM_REFTABLE_NAMES = "referenceTableNames";
    private static final String INPUT_PARAM_REPORT_IDS = "reportIds";

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String exportIds = (String) getParameter(INPUT_PARAM_EXPORT_IDS);
        if (!StringUtils.isEmpty(exportIds)) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ImportExportDefinition.COL_TYPE, Integer.valueOf(ImportExportDefinition.TYPE_EXPORT));
            deleteTools(exportIds, criteria, ImportExportDefinition.class, ImportExportDefinition.COL_ID);
        }

        String exportFormatIds = (String) getParameter(INPUT_PARAM_EXPORT_FORMAT_IDS);
        if (!StringUtils.isEmpty(exportFormatIds)) {
            deleteTools(exportFormatIds, new X2Criteria(), ExportFormatDefinition.class,
                    ExportFormatDefinition.COL_PROCEDURE_ID);
        }

        String importIds = (String) getParameter(INPUT_PARAM_IMPORT_IDS);
        if (!StringUtils.isEmpty(importIds)) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ImportExportDefinition.COL_TYPE, Integer.valueOf(ImportExportDefinition.TYPE_IMPORT));
            deleteTools(importIds, criteria, ImportExportDefinition.class, ImportExportDefinition.COL_ID);
        }

        String procedureIds = (String) getParameter(INPUT_PARAM_PROCEDURE_IDS);
        if (!StringUtils.isEmpty(procedureIds)) {
            deleteTools(procedureIds, new X2Criteria(), Procedure.class, Procedure.COL_ID);
        }

        String refTableNames = (String) getParameter(INPUT_PARAM_REFTABLE_NAMES);
        if (!StringUtils.isEmpty(refTableNames)) {
            deleteTools(refTableNames, new X2Criteria(), ReferenceTable.class, ReferenceTable.COL_USER_NAME);
        }

        String reportIds = (String) getParameter(INPUT_PARAM_REPORT_IDS);
        if (!StringUtils.isEmpty(reportIds)) {
            deleteTools(reportIds, new X2Criteria(), Report.class, Report.COL_ID);
        }

    }

    private void deleteTools(String idString, X2Criteria criteria, Class toolClass, String idJavaName) {
        logMessage("Processing " + toolClass.getSimpleName() + " with list " + idString);
        List<String> ids = Arrays.asList(idString.split("\\s*,\\s*"));
        criteria.addIn(idJavaName, ids);
        Collection<X2BaseBean> beans = getBroker().getCollectionByQuery(new BeanQuery(toolClass, criteria));
        for (X2BaseBean bean : beans) {
            logMessage("Deleted " + toolClass.getSimpleName() + " with identifier = "
                    + bean.getFieldValueByBeanPath(idJavaName));
            getBroker().deleteBean(bean);
        }
    }
}
