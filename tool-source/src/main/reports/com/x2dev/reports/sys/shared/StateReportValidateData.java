/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.shared;
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

import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.X2BaseException;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * This is a report class that performs standardized error and validation reporting for
 * the state report infrastructure.
 * this class will identify a procedure that contains a state report definition.
 * It will use that definition to final all configuration errors, data load errors
 * and validation errors.
 *
 * @author X2 Development Corporation
 */
public class StateReportValidateData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    /**
     * Keys for resource messages
     */
    private static final String TOTAL_KEY = "label.state.report.total";
    private static final String INITIALIZE_KEY = "label.state.report.initialize";
    private static final String GLOBAL_VALIDATION_KEY = "label.state.report.global";
    private static final String PROBLEM_AREA_KEY = "label.state.report.problemarea";

    /**
     * Format parameter keys.
     */
    private static final String PARAM_REPORT_TITLE = "reportTitle";

    /**
     * Format grid field keys.
     */
    private static final String FIELD_COUNT = "count";
    private static final String FIELD_ENTITY = "entity";
    private static final String FIELD_ERROR = "error";
    private static final String FIELD_ERROR_SECTION = "section";
    private static final String FIELD_FIELD = "field";
    private static final String FIELD_MESSAGE = "message";
    private static final String FIELD_SECTION_TITLE = "secttitle";
    private final static List<String> CSV_COLUMNS =
            Arrays.asList(FIELD_ENTITY, FIELD_FIELD, FIELD_ERROR, FIELD_MESSAGE);
    private final static String CSV_HEADING = "Entity, Field, Error, Message\r\n";

    /**
     * Input parameters for the procedure with the report data object.
     */
    public static String PROCEDURE_ID = "procedureId";
    public static String SHOW_SUMMARY = "summary";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Collection<StateReportValidationError> initErrors = new ArrayList<StateReportValidationError>();
        Map<String, StateReportValidationError> summaryErrors = new TreeMap<String, StateReportValidationError>();

        ReportDataGrid grid = new ReportDataGrid(300, 4);

        String procedureId = (String) getParameter(PROCEDURE_ID);
        Boolean summaryOnly = (Boolean) getParameter(SHOW_SUMMARY);
        if (summaryOnly == null) {
            summaryOnly = Boolean.FALSE;
        }

        // Lookup State report source data procedure
        StateReportData reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), initErrors);
        if (reportData != null) {
            // Set report parameters.
            addParameter(PARAM_REPORT_TITLE, reportData.getExportTitle());

            if (initErrors.size() == 0) {
                try {
                    // Initialize the report data object.
                    reportData.setBroker(getBroker());
                    reportData.setCurrentContext(getCurrentContext());
                    reportData.setOrganization(getOrganization());
                    reportData.setPrivilegeSet(getPrivilegeSet());
                    reportData.setSchoolContext(isSchoolContext());
                    reportData.setSchool(getSchool());
                    reportData.setParameters(getParameters());
                    reportData.setUser(getUser());
                    reportData.initializeExport();
                } catch (X2BaseException x2be) {
                    String initMsg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(INITIALIZE_KEY);
                    initErrors.add(new StateReportValidationError(initMsg, initMsg, initMsg, x2be.getMessage()));
                }

                initErrors.addAll(reportData.getSetupErrors());
                reportData.clearSetupErrors();

                // Only go detail errors if there are no initialization errors.
                if (initErrors.size() == 0) {
                    // Continue if no setup errors occur.

                    String totalLabel = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(TOTAL_KEY);

                    // Iterate over source data, build up errors list.
                    if (reportData.open()) {
                        try {
                            StateReportEntity entity = null;
                            while ((entity = reportData.next()) != null) {
                                entity.preProcess();
                                Collection<StateReportValidationError> fieldErrors = entity.getFieldValidations();

                                for (StateReportValidationError fve : fieldErrors) {
                                    // Accumulate totals for summary area.
                                    String summaryKey = fve.getFieldName() + '\t' + fve.getErrorId();
                                    StateReportValidationError summaryErr = summaryErrors.get(summaryKey);
                                    if (summaryErr == null) {
                                        summaryErr = new StateReportValidationError(totalLabel, fve.getFieldName(),
                                                fve.getErrorId(), null);
                                        summaryErr.increment();
                                        summaryErrors.put(summaryKey, summaryErr);
                                    } else {
                                        summaryErr.increment();
                                    }

                                    // Write the individual error out, if not in summary mode.
                                    if (!summaryOnly.booleanValue()) {
                                        grid.append();
                                        grid.set(FIELD_SECTION_TITLE, reportData.getEntityTitle());
                                        grid.set(FIELD_ERROR_SECTION, reportData.getEntityTitle());
                                        grid.set(FIELD_ENTITY, fve.getEntityName());
                                        grid.set(FIELD_FIELD, fve.getFieldName());
                                        grid.set(FIELD_ERROR, fve.getErrorId());
                                        grid.set(FIELD_MESSAGE, fve.getErrorMessage());
                                    }
                                }
                                entity.postProcess();
                            }
                        } catch (X2BaseException x2be) {
                            reportData.addSetupError("Exception", x2be.getMessage());
                        } finally {
                            reportData.close();
                        }

                        // Now include error summary.
                        for (StateReportValidationError sfve : summaryErrors.values()) {
                            grid.append();
                            grid.set(FIELD_SECTION_TITLE, reportData.getEntityTitle());
                            grid.set(FIELD_ERROR_SECTION, reportData.getEntityTitle());
                            grid.set(FIELD_ENTITY, sfve.getEntityName());
                            grid.set(FIELD_FIELD, sfve.getFieldName());
                            grid.set(FIELD_ERROR, sfve.getErrorId());
                            grid.set(FIELD_COUNT, Integer.valueOf(sfve.getCount()));
                        }
                    }
                }
                /*
                 * postProcess performs total statistical checks on the
                 * entire content of the data.
                 */
                initErrors.addAll(reportData.postProcess());
                /*
                 * Check for processing errors that occured in the entity.
                 */
                initErrors.addAll(reportData.getSetupErrors());
            }
        }
        if (initErrors.size() > 0) {
            // display initialization errors and global errors, if there are any.
            String section = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                    .getMessage(GLOBAL_VALIDATION_KEY);
            String caption = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                    .getMessage(PROBLEM_AREA_KEY);
            for (StateReportValidationError ifve : initErrors) {
                grid.append();
                grid.set(FIELD_SECTION_TITLE, section);
                grid.set(FIELD_ERROR_SECTION, caption);
                grid.set(FIELD_ENTITY, ifve.getEntityName());
                grid.set(FIELD_FIELD, ifve.getFieldName());
                grid.set(FIELD_ERROR, ifve.getErrorId());
                grid.set(FIELD_MESSAGE, ifve.getErrorMessage());
            }
        }
        // If the grid is empty, add one empty row with title and section to populate heading
        // information
        // in the printed report. This avoids the display of the word "null".
        if (grid.getRows().size() == 0) {
            grid.append();
            grid.set(FIELD_SECTION_TITLE, reportData.getEntityTitle());
            grid.set(FIELD_ERROR_SECTION, reportData.getEntityTitle());
            grid.set(FIELD_ENTITY, "");
            grid.set(FIELD_FIELD, "");
            grid.set(FIELD_ERROR, "");
            grid.set(FIELD_MESSAGE, "");
        }
        grid.beforeTop();

        return grid;
    }

    /**
     * Publish results.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#publishResults()
     */
    @Override
    protected void publishResults() throws Exception {
        ToolJob job = this.getJob();
        if (ToolInput.CSV_FORMAT == job.getInput().getFormat()) {
            writeResults();
        } else {
            super.publishResults();
        }
    }

    /**
     * Gets the column names.
     *
     * @return List
     */
    private List<String> getColumnNames() {
        return CSV_COLUMNS;
    }

    /**
     * Gets the header.
     *
     * @return String
     */
    private String getHeader() {
        return CSV_HEADING;
    }

    /**
     * Write results. Similar to ExportJavaSource.WriteResults()
     *
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void writeResults() throws IOException {
        String name = getJob().getTool().getName();
        String jobId = String.valueOf(getJob().getJobId());

        String[] logParameters = new String[] {
                name, jobId
        };
        AppGlobals.getLog().log(Level.INFO, LOG_DATA_PREPARED, logParameters);

        /*
         * If the job has been aborted then don't bother formatting the results.
         */
        if (getJob().getStatus() != ToolJob.STATUS_ABORT) {
            DataGrid dataGrid = null;
            Object data = getDataSource();
            if (data != null && data instanceof DataGrid) {
                dataGrid = (DataGrid) data;
            }

            if (dataGrid != null && dataGrid.rowCount() > 0) {
                // delete total rows
                String totalLabel = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(TOTAL_KEY);
                dataGrid.beforeTop();
                while (dataGrid.next()) {
                    String value = (String) dataGrid.get(FIELD_ENTITY);
                    if (totalLabel.equals(value)) {
                        dataGrid.deleteRow();
                    }
                    // remove html markup
                    for (String col : dataGrid.getColumns()) {
                        if (dataGrid.get(col) instanceof String) {
                            String gridValue = (String) dataGrid.get(col);
                            gridValue = gridValue.replaceAll("<[^>]*>", "");
                            dataGrid.set(col, gridValue);
                        }
                    }
                }

                // output detail rows
                OutputStream outputStream = getResultHandler().getOutputStream();
                ExportJavaSource.writeData(outputStream,
                        dataGrid,
                        getColumnNames(),
                        getColumnNames(),
                        getCharacterEncoding(),
                        "\r\n", // LineSeparator
                        getHeader(),
                        "", // trailer
                        null, // escapeCharacter
                        ',', // valueDelimiter
                        '\"', // valueWrapper
                        false, // includeHeaderRow
                        false, // useEscapes
                        true, // useValueDelimiters
                        true // useValueWrappers
                );
            } else {
                getResultHandler().setEmpty(true);
            }
        }
    }
}
