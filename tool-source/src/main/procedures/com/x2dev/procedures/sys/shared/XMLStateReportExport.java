/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.shared;

import static com.follett.fsc.core.k12.tools.exports.ExportJavaSource.FORMAT_EOL_WINDOWS;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.tools.stateexports.XMLStateReportData;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.logging.Level;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * This is a report class that performs standardized data export for
 * the state report infrastructure.
 * <p>
 * This class will identify a procedure that contains a state report definition.
 * It will use that definition to find all data definitions and produce
 * an export file.
 * <p>
 * This is enhanced to support XML exports.
 * <p>
 * <strong>LIMITATION:</strong> This format cannot currently save results to the export result row
 * table.
 *
 * @author Follett Software Company
 *
 */
public class XMLStateReportExport extends ToolJavaSource {

    private static final long serialVersionUID = 1L;

    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    public static final String PARAM_CURRENT_BEAN = "currentBean";

    private static final String PARAM_PROCEDURE_ID = "procedureId";

    private String m_validationMode;

    private static final String PARAM_VALIDATION_MODE = "validationMode";

    private static final String DEBUG_OPTION_OFF = "off";

    private static final String DEBUG_OPTION_SUMMARY = "summary";

    /**
     * List of errors the export may have
     */
    private Collection<StateReportValidationError> m_initErrors = null;

    /**
     * The export's report data
     */
    private XMLStateReportData m_reportData = null;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        String procedureId = (String) getParameter(PARAM_PROCEDURE_ID);
        m_initErrors = new ArrayList<StateReportValidationError>();

        m_validationMode = (String) getParameter(PARAM_VALIDATION_MODE);
        if (StringUtils.isEmpty(m_validationMode)) {
            m_validationMode = DEBUG_OPTION_OFF;
        }

        // Lookup State report source data procedure
        m_reportData =
                (XMLStateReportData) StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);
        if (m_reportData != null && m_initErrors.size() == 0) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setCurrentContext(getCurrentContext());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setProcedureId(procedureId);
                m_reportData.setDebug(!DEBUG_OPTION_OFF.equals(m_validationMode));

                m_reportData.initializeExport();
                m_reportData.postProcess();
            } catch (X2BaseException x2be) {
                String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(INITIALIZE_KEY);
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
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
        String name = getJob().getTool().getName();
        String jobId = String.valueOf(getJob().getJobId());

        /*
         * Step 1. Get the root element
         */
        Element root = m_reportData.getRootElement();

        String[] logParameters = new String[] {name, jobId};
        AppGlobals.getLog().log(Level.INFO, LOG_DATA_PREPARED, logParameters);

        ThreadUtils.checkInterrupt();

        /*
         * If there are errors, print them out
         */
        OutputStream outputStream = getResultHandler().getOutputStream();
        if (m_initErrors.size() > 0 || root == null) {
            outputStream.write(("There were some errors initializing" + FORMAT_EOL_WINDOWS).getBytes());
            for (StateReportValidationError error : m_initErrors) {
                String errorId = error.getErrorId();
                String fieldName = error.getFieldName();
                String errorMessage = error.getErrorMessage();
                String message = String.format("%s %s - %s", errorId, fieldName, errorMessage);
                outputStream.write((message + FORMAT_EOL_WINDOWS).getBytes());
            }
        }

        /*
         * If the job hasn't been aborted, then format the results.
         */
        if (getJob().getStatus() != ToolJob.STATUS_ABORT) {
            if (DEBUG_OPTION_SUMMARY.equals(m_validationMode)) {
                ReportDataGrid errorGrid = m_reportData.getErrorGrid();
                outputStream.write(errorGrid.format(true, false, false).getBytes());
            } else if (root != null) {
                Document document = new Document();
                document.setRootElement(root);
                XMLOutputter outputter = new XMLOutputter();
                try {
                    Format format = Format.getPrettyFormat();
                    outputter.setFormat(format);
                    outputter.output(document, outputStream);
                } catch (IOException ioe) {
                    String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(INITIALIZE_KEY);
                    m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, ioe.getMessage()));
                    throw ioe;
                }
            }
        }
    }

    /**
     * Attempt to get the current bean from the user container and
     * add it to a parameter 'currentBean'.
     * <p>
     * An implementing XMLStateReportData may use this for limiting its
     * result to a single bean.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        X2BaseBean bean = userData.getCurrentRecord(X2BaseBean.class);
        addParameter(PARAM_CURRENT_BEAN, bean);
    }
}
