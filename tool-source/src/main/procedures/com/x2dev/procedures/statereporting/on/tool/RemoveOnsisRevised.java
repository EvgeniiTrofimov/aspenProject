/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import java.util.Collection;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class RemoveOnsisRevised extends ProcedureJavaSource {

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Procedure proc = (Procedure) getJob().getTool();
        
        logMessage("Deleting Procedures ");
        X2Criteria criteriaPRC = new X2Criteria();
        criteriaPRC.addLike(Procedure.COL_ID, "ONSI2-%");
        X2Criteria criteriaPRC2 = new X2Criteria();
        criteriaPRC2.addLike(Procedure.COL_ID, "SR-CMN-%");
        criteriaPRC.addOrCriteria(criteriaPRC2);
        Collection<X2BaseBean> beans = getBroker().getCollectionByQuery(new BeanQuery(Procedure.class, criteriaPRC));
        for (X2BaseBean bean : beans) {
            logMessage("Deleted export format definition with identifier = "
                    + bean.getFieldValueByBeanPath(Procedure.COL_ID));
            getBroker().deleteBean(bean);
        }

        logMessage("Deleting Imports ");
        X2Criteria criteriaIED = new X2Criteria();
        criteriaIED.addLike(Procedure.COL_ID, "ONSI2-%");
        beans = getBroker().getCollectionByQuery(new BeanQuery(ImportExportDefinition.class, criteriaPRC));
        for (X2BaseBean bean : beans) {
            logMessage("Deleted import/export definition with identifier = "
                    + bean.getFieldValueByBeanPath(ImportExportDefinition.COL_ID));
            getBroker().deleteBean(bean);
        }

        logMessage("Deleting export format definitions ");
        X2Criteria criteriaEFD = new X2Criteria();
        criteriaEFD.addLike(ExportFormatDefinition.COL_PROCEDURE_ID, "ONSI2-%");
        beans = getBroker().getCollectionByQuery(new BeanQuery(ExportFormatDefinition.class, criteriaEFD));
        for (X2BaseBean bean : beans) {
            logMessage("Deleted export format definition with identifier = "
                    + bean.getFieldValueByBeanPath(ExportFormatDefinition.COL_PROCEDURE_ID));
            getBroker().deleteBean(bean);
        }

        getBroker().deleteBean(proc);
    }

}
