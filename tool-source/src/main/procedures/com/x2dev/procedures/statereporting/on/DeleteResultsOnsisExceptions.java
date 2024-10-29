/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class DeleteResults.
 */
public class DeleteResultsOnsisExceptions extends ProcedureJavaSource {
    private static final long serialVersionUID = 1L;
    private static final String DDX_ONSIS_EXCEPTION = "ddxOnSisExc   ";

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

        List<UserDefinedTableB> results = new ArrayList<>();

        for (Object selectedRecordObject : selectedRecords) {
            UserDefinedTableB result = (UserDefinedTableB) selectedRecordObject;
            results.add(result);
        }

        OutputStream outputStream = getResultHandler().getOutputStream();
        getBroker().beginTransaction();
        boolean isError = false;
        String message = null;
        int count = 0;
        try {
            X2Criteria criteria = new X2Criteria();
            boolean found = false;
            if (results.size() > 0) {
                UserDefinedTableB userDefinedTableB = results.get(0);

                String listDictionaryOid = userDefinedTableB.getExtendedDataDictionaryOid();

                found = !StringUtils.isBlank(listDictionaryOid);
                criteria.addEqualTo(UserDefinedTableB.COL_EXTENDED_DATA_DICTIONARY_OID, listDictionaryOid);
                if (!DDX_ONSIS_EXCEPTION.equals(listDictionaryOid)) {
                    message = "Unexpected DDX: " + listDictionaryOid;
                    throw new RuntimeException(message);
                }
            }

            QueryByCriteria query = new QueryByCriteria(UserDefinedTableB.class, criteria);
            if (found) {
                count = getBroker().deleteByQuery(query);
            } else {
                message = "Could not determine " + UserDefinedTableB.COL_EXTENDED_DATA_DICTIONARY_OID + "\n";
                outputStream.write(message.getBytes());
            }
        } catch (Exception e) {
            isError = true;
            e.printStackTrace();
            message = ExceptionUtils.getStackTrace(e);
        } finally {
            if (isError) {
                getBroker().rollbackTransaction();
            } else {
                getBroker().commitTransaction();
                message = "Deleted results: " + count;
            }

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
