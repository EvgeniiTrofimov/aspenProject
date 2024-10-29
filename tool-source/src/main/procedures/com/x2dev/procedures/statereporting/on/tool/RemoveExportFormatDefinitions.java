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
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import java.util.Collection;

/**
 * @author Follett Software Company
 * @copyright 2018
 */
public class RemoveExportFormatDefinitions extends ProcedureJavaSource {
    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Procedure proc = (Procedure) getJob().getTool();
        
        logMessage("Deleting export format definitions ");
        X2Criteria criteria = new X2Criteria();
        criteria.addLike(ExportFormatDefinition.COL_PROCEDURE_ID, "ONSIS-%");
        Collection<X2BaseBean> beans = getBroker().getCollectionByQuery(new BeanQuery(ExportFormatDefinition.class, criteria));
        for (X2BaseBean bean : beans) {
            logMessage("Deleted export format definition with identifier = "
                    + bean.getFieldValueByBeanPath(ExportFormatDefinition.COL_PROCEDURE_ID));
            getBroker().deleteBean(bean);
        }

        getBroker().deleteBean(proc);
    }

}
