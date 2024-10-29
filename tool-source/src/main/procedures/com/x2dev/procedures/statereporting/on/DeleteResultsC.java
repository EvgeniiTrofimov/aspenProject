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

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.utils.X2BaseException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.commons.lang3.exception.ExceptionUtils;

/**
 * The Class DeleteResults.
 */
public class DeleteResultsC extends ProcedureJavaSource {
    private static final long serialVersionUID = 1L;

    private UserDataContainer m_userDataContainer;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Collection selectedRecords =
                m_userDataContainer.getListForNode(m_userDataContainer.getCurrentNode().getId()).getPageData();

        List<UserDefinedTableC> results = new ArrayList<>();

        for (Object selectedRecordObject : selectedRecords) {
            UserDefinedTableC result = (UserDefinedTableC) selectedRecordObject;
            results.add(result);
        }

        getBroker().beginTransaction();
        boolean isError = false;
        String message = null;
        try {
            results.stream().forEach(result -> {
                deleteBeans(result.getUserDefinedRecordsA());
                deleteBeans(result.getUserDefinedRecordsB());
                getBroker().deleteBean(result);
            });
        } catch (Exception e) {
            isError = true;
            e.printStackTrace();
            message = ExceptionUtils.getStackTrace(e);
        } finally {
            if (isError) {
                getBroker().rollbackTransaction();
            } else {
                getBroker().commitTransaction();
                message = "Deleted results: " + results.size();
            }

            OutputStream outputStream = getResultHandler().getOutputStream();
            outputStream.write(message.getBytes());
            outputStream.close();
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        //
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        m_userDataContainer = userData;
    }

    /**
     * Delete beans.
     *
     * @param beans Collection<X2BaseBean>
     */
    private void deleteBeans(Collection<? extends X2BaseBean> beans) {
        beans.stream().forEach(bean -> getBroker().deleteBean(bean));
    }
}
