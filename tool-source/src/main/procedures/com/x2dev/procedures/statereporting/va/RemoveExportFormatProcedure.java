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
package com.x2dev.procedures.statereporting.va;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.tools.exports.Export;
import com.x2dev.utils.X2BaseException;

/**
 * Procedure that remove old export format.
 *
 * @author Follett Software Company
 */
public class RemoveExportFormatProcedure extends ProcedureJavaSource {
    private static final String INPUT_PROCEDURE_ID = "prcdId";

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String prcdId = (String) getParameter(INPUT_PROCEDURE_ID);
        ExportFormatDefinition efd = getEfdByProcedure(prcdId);
        if (efd != null) {
            getBroker().deleteBean(efd);
            logMessage("Export format definition with procedure id = " + prcdId + " was deleted.");
        } else {
            logMessage("There is no export format definition with procedure id = " + prcdId);
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
    }

    /**
     * Returns {@link Export} fromat definition to remove.
     *
     * @return Procedure
     */
    private ExportFormatDefinition getEfdByProcedure(String prcdId) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExportFormatDefinition.COL_PROCEDURE_ID, prcdId);
        BeanQuery beanQuery = new BeanQuery(ExportFormatDefinition.class, criteria);
        return (ExportFormatDefinition) getBroker().getBeanByQuery(beanQuery);
    }
}
