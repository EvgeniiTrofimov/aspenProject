/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tx;

import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tx.TxHelper.TxXmlComposer;
import com.x2dev.utils.X2BaseException;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;

/**
 * This is a export class that performs standardized data export for
 * the TX state export infrastructure.
 * This class will identify a procedure that contains a state report definition.
 * It will use that definition to find all data definitions and produce
 * an export file.
 *
 * @author X2 Development Corporation
 */
public class TxExportFormat extends ToolJavaSource {
    private static final String INITIALIZE_KEY = "label.state.report.initialize";
    private static final String PROCEDURE_ID = "procedureId";

    /**
     * List of errors the export may have
     */
    private Collection<StateReportValidationError> m_initErrors = null;

    /**
     * The export's report data
     */
    private StateReportData m_reportData = null;

    private TxXmlComposer m_xmlComposer = null;

    /**
     * Gathers the data from the export.
     *
     * Passes the xml path to the addToStack() to be parsed and pushed to a stack
     *
     * @return the first element of the tree (the root)
     * @throws Exception exception
     */
    protected String gatherData() throws Exception {
        m_xmlComposer = new TxXmlComposer(m_reportData, m_initErrors, false);

        return m_xmlComposer.getCompleteXml();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        String procedureId = (String) getParameter(PROCEDURE_ID);
        m_initErrors = new ArrayList<StateReportValidationError>();

        // Lookup State report source data procedure
        m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);
        if (m_reportData != null) {
            if (m_initErrors.size() == 0) {
                try {
                    // Initialize the report data object.
                    m_reportData.setBroker(getBroker());
                    m_reportData.setOrganization(getOrganization());
                    m_reportData.setPrivilegeSet(getPrivilegeSet());
                    m_reportData.setSchoolContext(isSchoolContext());
                    m_reportData.setSchool(getSchool());
                    m_reportData.setParameters(getParameters());
                    m_reportData.setUser(getUser());
                    m_reportData.initializeExport();
                } catch (X2BaseException x2be) {
                    String init_msg =
                            LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(INITIALIZE_KEY);
                    m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                    throw x2be;
                }
            } else {
                m_initErrors.addAll(m_reportData.getSetupErrors());
            }
        } else {
            m_initErrors.add(new StateReportValidationError("Procedure not found:", "", procedureId, ""));
        }
    }

    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        outputResult();
    }

    /**
     * Outputs result.
     *
     * @throws Exception exception
     */
    private void outputResult() throws Exception {
        try {
            Writer writer = new OutputStreamWriter(getResultHandler().getOutputStream());
            try {
                if (m_initErrors != null && m_initErrors.size() > 0) {
                    for (StateReportValidationError error : m_initErrors) {
                        writer.write("\n" + error.getEntityName() + " " + error.getFieldName() + " "
                                + error.getErrorId() + " " + error.getErrorMessage());
                    }
                    writer.flush();
                } else {
                    writer.write(gatherData());
                    writer.flush();
                }
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            // Oh well.
        }
    }

}
