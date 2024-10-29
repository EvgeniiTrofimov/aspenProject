/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.il;

import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure;
import com.x2dev.sis.model.beans.IepData;
import java.lang.reflect.Method;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class PrepopulateForms extends BaseFormReportJavaSource {

    private static final String PARAM_DELETE_NOT_APPROPRIATE_RECORDS = "deleteNotAppropriateRecords";
    private static final String PARAM_PREPOPULATE_FORM = "prepopulateForm";
    private static final String PREPOPULATE_VALUE_ALL = "all";
    private static final long serialVersionUID = 1L;
    private SpedIlWorkflowCommonProcedure m_ilWorkflowHelper = null;
    private ReportDataGrid m_grid;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        m_grid = new ReportDataGrid();
        IepData iepData = (IepData) getFormOwner();
        m_ilWorkflowHelper =
                new SpedIlWorkflowCommonProcedure(null, null, getOrganization(), getUser(), getBroker(), getLocale());
        if (iepData != null) {
            StringBuilder message = new StringBuilder();
            String formForPrepopulate = (String) getParameter(PARAM_PREPOPULATE_FORM);
            Boolean deleteNotApprRecords = (Boolean) getParameter(PARAM_DELETE_NOT_APPROPRIATE_RECORDS);
            m_ilWorkflowHelper.setNeedDeleteNotAppropriateRecords(
                    deleteNotApprRecords == null ? false : deleteNotApprRecords.booleanValue());
            if (formForPrepopulate != null && formForPrepopulate.equals(PREPOPULATE_VALUE_ALL)) {
                message.append(m_ilWorkflowHelper.fill3454I(iepData));
                message.append(m_ilWorkflowHelper.fill3454V(iepData));
                message.append(m_ilWorkflowHelper.fill3457BC(iepData));
                message.append(m_ilWorkflowHelper.fill3457C(iepData));
                message.append(m_ilWorkflowHelper.fillIndicator(iepData));
                message.append(m_ilWorkflowHelper.fillSpAppr(iepData));
                message.append(m_ilWorkflowHelper.fillSumPref(iepData));
            } else if (formForPrepopulate != null) {
                Object object = callFillMethod("fill" + formForPrepopulate, iepData);
                if (object != null && object instanceof StringBuilder) {
                    message.append((StringBuilder) object);
                }

            }
            m_grid.append();
            m_grid.set("message", message.toString());
            m_grid.beforeTop();
        }

        return m_grid;
    }


    /**
     * Call fill method.
     *
     * @param methodName String
     * @param iepData IepData
     * @return Object
     */
    private Object callFillMethod(String methodName, IepData iepData) {
        Method method = null;
        Object returnObject = null;
        try {
            method = m_ilWorkflowHelper.getClass().getDeclaredMethod(methodName,
                    new Class[] {IepData.class});

            if (method != null) {
                returnObject = method.invoke(m_ilWorkflowHelper, new Object[] {iepData});
            }
        } catch (Exception e) {
            // Do nothing;
        }
        return returnObject;
    }
}
