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
package com.x2dev.reports.statereporting.wa;
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
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
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
public class WAP223BasicSupportExportValidationData extends ReportJavaSourceNet {
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
    private static final String FIELD_SCHOOL_NAME = "schoolName";

    /**
     * Input parameters for the procedure with the report data object.
     */
    public static String PROCEDURE_ID = "procedureId";
    public static String SHOW_SUMMARY = "summary";
    public static String SORT_BY = "sortBy";

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
        Map<String, Map<String, StateReportValidationError>> summaryErrorsBySkl = new TreeMap();

        ReportDataGrid grid = new ReportDataGrid(300, 4);

        String procedureId = (String) getParameter(PROCEDURE_ID);
        Boolean summaryOnly = (Boolean) getParameter(SHOW_SUMMARY);
        String sortBy = (String) getParameter(SORT_BY);

        boolean useSchoolName = isSchoolContext() || "school.name,nameView".equals(sortBy);

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
                                    String sklName =
                                            isSchoolContext() ? getSchool().getName() : entity.getFieldValue(7);

                                    StateReportValidationError summaryErr = summaryErrors.get(summaryKey);
                                    if (summaryErr == null) {
                                        summaryErr = new StateReportValidationError(totalLabel, fve.getFieldName(),
                                                fve.getErrorId(), null);
                                        summaryErr.increment();
                                        summaryErrors.put(summaryKey, summaryErr);
                                    } else {
                                        summaryErr.increment();
                                    }

                                    if (useSchoolName) {
                                        Map<String, StateReportValidationError> sklErrorsMap =
                                                summaryErrorsBySkl.get(sklName);

                                        if (sklErrorsMap == null) {
                                            StateReportValidationError sklError = new StateReportValidationError(
                                                    totalLabel, fve.getFieldName(), fve.getErrorId(), null);
                                            sklError.increment();
                                            sklErrorsMap = new HashMap<String, StateReportValidationError>();
                                            sklErrorsMap.put(summaryKey, sklError);

                                            summaryErrorsBySkl.put(sklName, sklErrorsMap);
                                        } else {
                                            StateReportValidationError sklError = sklErrorsMap.get(summaryKey);

                                            if (sklError == null) {
                                                sklError = new StateReportValidationError(totalLabel,
                                                        fve.getFieldName(), fve.getErrorId(), null);
                                                sklError.increment();
                                                sklErrorsMap.put(summaryKey, sklError);
                                            } else {
                                                sklError.increment();
                                            }
                                        }
                                    }

                                    // Write the individual error out, if not in
                                    // summary mode.
                                    if (!summaryOnly.booleanValue()) {
                                        grid.append();
                                        grid.set(FIELD_SECTION_TITLE, reportData.getEntityTitle());
                                        grid.set(FIELD_ERROR_SECTION, reportData.getEntityTitle());
                                        grid.set(FIELD_ENTITY, fve.getEntityName());
                                        grid.set(FIELD_FIELD, fve.getFieldName());
                                        grid.set(FIELD_ERROR, fve.getErrorId());
                                        grid.set(FIELD_MESSAGE, fve.getErrorMessage());

                                        if (useSchoolName) {
                                            grid.set(FIELD_SCHOOL_NAME, sklName);
                                        }
                                    }
                                }
                                entity.postProcess();
                            }
                        } catch (X2BaseException x2be) {
                            reportData.addSetupError("Exception", x2be.getMessage());
                        } finally {
                            reportData.close();
                        }
                        if (useSchoolName && !summaryOnly.booleanValue()) {
                            // Now include error summary by skl.
                            for (Entry<String, Map<String, StateReportValidationError>> entry : summaryErrorsBySkl
                                    .entrySet()) {
                                Map<String, StateReportValidationError> errorsMap = entry.getValue();

                                for (Entry<String, StateReportValidationError> errorEntry : errorsMap.entrySet()) {
                                    grid.append();
                                    grid.set(FIELD_SECTION_TITLE, reportData.getEntityTitle());
                                    grid.set(FIELD_ERROR_SECTION, reportData.getEntityTitle());
                                    grid.set(FIELD_ENTITY, errorEntry.getValue().getEntityName());
                                    grid.set(FIELD_FIELD, errorEntry.getValue().getFieldName());
                                    grid.set(FIELD_ERROR, errorEntry.getValue().getErrorId());
                                    grid.set(FIELD_COUNT, Integer.valueOf(errorEntry.getValue().getCount()));
                                    grid.set(FIELD_SCHOOL_NAME, entry.getKey());

                                }
                            }
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
                if (!isSchoolContext()) {
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

        if (useSchoolName) {
            grid.sort(FIELD_SCHOOL_NAME, false);
        }

        grid.beforeTop();

        return grid;
    }

}
