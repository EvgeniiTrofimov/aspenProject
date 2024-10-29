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
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.utils.X2BaseException;

/**
 * The Class OnsisSubmissionCopy.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnsisSubmissionCopy extends ProcedureJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String ALIAS_SUBMISSION_TYPE = "submission-type";
    private static final String PARAM_SUBMISSION_TYPE = "submissionType";

    UserDefinedTableA m_bean;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (m_bean != null) {
            UserDefinedTableA bean = (UserDefinedTableA) m_bean.copyBean();
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(bean.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey());
            if (dictionary != null) {
                DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SUBMISSION_TYPE);
                if (field != null) {
                    bean.setFieldValueByBeanPath(field.getJavaName(), getParameter(PARAM_SUBMISSION_TYPE));
                }
            }
            getBroker().saveBeanForced(bean, dictionary);
            logMessage("Submission Type Copied");
        } else {
            logMessage("A current Submission Type is required");
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_bean = userData.getCurrentRecord(UserDefinedTableA.class);
    }

}
