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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.ExportResultsComparisonManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.X2BaseException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.logging.Level;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TNComparisonExport.
 */
public class TNComparisonExport extends ExportJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final String COMPARISON_INDICATOR_COLUMN = "compIndicator";
    private static final String FIELD_RECORD_TYPE = "RECORD TYPE";
    private static final String DELETE_RECORD = "D";
    private static final String PARAM_EXPORT_IDS = "exportIds";
    private static final String PARAM_BULK_COMPARISON_INDICATOR = "bulkComparisonIndicator";
    private static final String PARAM_CURRENT_RESULT = "current_";
    private static final String PARAM_DELETED_RECORD_IND = "deleteRecInd";
    private static final String PARAM_FILE_SEQ = "fileSeq";
    private static final String PARAM_INSERTED_RECORD_IND = "insertRecInd";
    private static final String PARAM_MARK_RESULTS = "markResults";
    private static final String PARAM_PREVIOUS_RESULT = "previous_";
    private static final String PARAM_UPDATED_RECORD_IND = "updateRecInd";
    private static final String PARAM_PRINT_ERRORS = "printErrors";
    private static final String PARAM_USE_CSV = "useCsvFormat";
    private static final String TN_COMPARE_MGR_PROC_ID = "TN-COMPARE-MGR";
    private static final String EXPORT_FOR = "Export for";
    private static final String RESTAGING = "Restaging";
    private static final String PROCESSED = "Processed:";

    private static final String[] BUS_REPORTS = {"015", "015_V2"};
    private static final String[] STD_REPORTS = {"040", "040_V5"};

    private Boolean m_bulkComparison;
    private String m_changeIndicatorColumnName;
    private Set<String> m_columnNames;
    private List<String> m_commentList = new ArrayList<String>();
    private ExportResultsComparisonManager m_comparisonManager = null;
    private String m_deletedRecordIndicator;
    private List<String> m_errorList = new ArrayList<String>();
    private List<ExportFormatDefinition> m_exportDefinitionList = new ArrayList<ExportFormatDefinition>();
    private TNHeadingTrailing m_headingTrailing;
    private String m_insertedRecordIndicator;
    private HashMap<String, ExportFormatResult> m_latestResults = new HashMap<String, ExportFormatResult>();
    private Set<String> m_listUniqueValues = new HashSet<String>();
    private Set<String> m_listCurrentKeys;
    private Set<ExportFormatRow> m_listOfRowsWithUniqueValues = new HashSet<ExportFormatRow>();
    private Boolean m_markResultsProcessed;
    private Boolean m_printErrors = null;
    private HashMap<String, ExportFormatResult> m_processedResults = new HashMap<String, ExportFormatResult>();
    private List<String> m_reportParams = new ArrayList<String>();
    private HashMap<String, List<ExportFormatResult>> m_restagingResults =
            new HashMap<String, List<ExportFormatResult>>();
    private String m_schoolYearContext;
    private String m_schoolYearContextOid;
    private String m_updatedRecordIndicator;

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid dataGrid = new DataGrid();
        int stdVersionCounter = 0;
        List<String> stdVersions = Arrays.asList(STD_REPORTS);
        int busVersionCounter = 0;
        List<String> busVersions = Arrays.asList(BUS_REPORTS);

        // Not empty errorList at this point means that we cannot load or
        // initialize required ExportResultComparisonManager and export cannot
        // be produced.
        if (m_errorList.isEmpty()) {
            if (m_bulkComparison != null && m_bulkComparison.booleanValue()) {
                for (ExportFormatDefinition definition : m_exportDefinitionList) {
                    m_listCurrentKeys = new HashSet<String>();

                    ExportFormatResult latestResult = getLatestResult(definition.getOid(), getBroker());
                    Collection<ExportFormatRow> latestEfrRows =
                            latestResult == null ? new ArrayList() : latestResult.getRows();

                    Collection<ExportFormatField> effs = latestResult == null ? new ArrayList()
                            : TNExportResultsHelper.getExportFormatField(latestResult, getBroker());

                    for (ExportFormatRow efw : latestEfrRows) {
                        StringBuilder uniqueRowKey = new StringBuilder();

                        for (ExportFormatField eff : effs) {
                            if (eff.getKeyInd()) {
                                String fieldValue = null;
                                DataFieldConfig config = eff.getDataFieldConfig();
                                if (config != null) {
                                    fieldValue =
                                            (String) efw.getFieldValueByBeanPath(config.getDataField().getJavaName());
                                }
                                fieldValue = ExportFormatManager.doPadding(fieldValue == null ? "" : fieldValue,
                                        (eff.getPaddingDirection() == 0
                                                ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                                : eff.getPaddingDirection()),
                                        eff.getPaddingChar(),
                                        eff.getMaximumLength());
                                uniqueRowKey.append(fieldValue);
                            }
                        }
                        m_listCurrentKeys.add(uniqueRowKey.toString());
                    }

                    List<ExportFormatResult> restagingResults =
                            getLastRestagingResults(definition.getOid(), getBroker());

                    String reportParam = getReportParam(definition);

                    int records = 0;

                    int restagingCount = gatherReportData(dataGrid, restagingResults);

                    Method method = m_comparisonManager.getClass().getDeclaredMethod("setRestagingCounts",
                            new Class[] {ArrayList.class, String.class, Collection.class});
                    if (restagingCount > 0) {
                        ArrayList<ExportFormatRow> allRestagingRows = new ArrayList<ExportFormatRow>();
                        for (ExportFormatResult restagingResult : restagingResults) {
                            allRestagingRows.addAll(restagingResult.getRows());
                        }
                        ArrayList<ExportFormatRow> listOfRestagingRows =
                                new ArrayList<ExportFormatRow>(m_listOfRowsWithUniqueValues);
                        listOfRestagingRows.retainAll(allRestagingRows);

                        Collection<ExportFormatField> exportFormatFields =
                                TNExportResultsHelper.getExportFormatField(restagingResults.get(0), getBroker());

                        method.invoke(m_comparisonManager,
                                listOfRestagingRows,
                                definition.getName().substring(0, 6),
                                exportFormatFields);

                        records += restagingCount;
                    } else {
                        method.invoke(m_comparisonManager,
                                null,
                                definition.getName().substring(0, 6),
                                null);
                    }

                    if (validateInput(definition)) {
                        ExportFormatResult oldResult = m_processedResults.get(definition.getOid());
                        ExportFormatResult newResult = m_latestResults.get(definition.getOid());

                        records += gatherReportData(dataGrid, oldResult, newResult);
                    }

                    m_headingTrailing.setCount(reportParam, records);
                }

                dataGrid.sort(Arrays.asList(new String[] {"Column 10000", "Column 10002"}), true);

                if (m_markResultsProcessed != null && m_markResultsProcessed.booleanValue()) {
                    for (ExportFormatDefinition definition : m_exportDefinitionList) {
                        ExportFormatResult result = m_latestResults.get(definition.getOid());
                        List<ExportFormatResult> restagingResults = m_restagingResults.get(definition.getOid());
                        if (result == null) {
                            result = getLatestResult(definition.getOid(), getBroker());
                        }

                        if (result != null) {
                            TNExportResultsHelper.setResultProcessed(result, getBroker());
                        }
                        if (restagingResults != null && !restagingResults.isEmpty()) {
                            for (ExportFormatResult restagingResult : restagingResults) {
                                TNExportResultsHelper.setResultProcessed(restagingResult, getBroker());
                            }
                        }
                    }
                }
            } else {
                if (m_reportParams.size() >= 0) {
                    for (String reportParam : m_reportParams) {
                        m_listCurrentKeys = new HashSet<String>();
                        ExportFormatResult oldResult = TNExportResultsHelper
                                .getExportFormatResult((String) getParameter(PARAM_PREVIOUS_RESULT +
                                        reportParam),
                                        getBroker());
                        ExportFormatResult newResult =
                                TNExportResultsHelper.getExportFormatResult((String) getParameter(PARAM_CURRENT_RESULT +
                                        reportParam),
                                        getBroker());
                        if (stdVersions.contains(reportParam) && oldResult != null) {
                            stdVersionCounter += 1;
                        }

                        if (busVersions.contains(reportParam) && oldResult != null) {
                            busVersionCounter += 1;
                        }

                        int records = gatherReportData(dataGrid, oldResult, newResult);

                        m_headingTrailing.setCount(reportParam, records);
                    }
                }

                if (stdVersionCounter > 1 || busVersionCounter > 1) {
                    dataGrid.clear();
                    dataGrid.append();
                    m_columnNames.add("Error");
                    dataGrid.set("Error", "Comparing multiple versions of the same export is not allowed");
                    dataGrid.beforeTop();
                    return dataGrid;
                }
            }
        }

        Method method = m_comparisonManager.getClass().getDeclaredMethod("getCommentList", new Class[0]);
        Set<String> comments = (Set<String>) method.invoke(m_comparisonManager, new Object[0]);

        if (!comments.isEmpty()) {
            m_commentList.addAll(comments);
            method = m_comparisonManager.getClass().getDeclaredMethod("getGrandTotal", new Class[0]);
            Integer grandTotal = (Integer) method.invoke(m_comparisonManager, new Object[0]);
            m_commentList.add("Grand Total: " + grandTotal.toString());
        }

        if (!m_errorList.isEmpty() && dataGrid.isEmpty() && m_printErrors.booleanValue()) {
            for (String errText : m_errorList) {
                dataGrid.append();
                dataGrid.set("Error", errText);
            }
        }
        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List columns = new ArrayList(m_columnNames.size());
        if (m_columnNames.size() == 0) {
            columns.add("Error");
        } else {
            columns.addAll(m_columnNames);
        }
        return columns;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return getColumnNames();
    }

    /**
     * This method returns the String, which is essentially a concatenation of all the errors.
     * This will be used by the job that executes the compare procedure.
     *
     * @return String
     */
    @Override
    protected String getComment() {
        String comments = "";
        if (!m_errorList.isEmpty()) {
            for (String error : m_errorList) {
                comments += error + "\n";
            }
        }
        if (!m_commentList.isEmpty()) {
            Collections.sort(m_commentList);
            for (String comment : m_commentList) {
                comments += comment + "\n";
            }
        }
        return comments;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        String header = "";

        if (m_errorList.size() > 0 && m_printErrors.booleanValue()) {
            header += "****** Errors ******\n";
            for (String error : m_errorList) {
                header += error + "\n";
            }
            header += "****** Errors ******\n";
        }
        if (m_headingTrailing != null) {
            header += m_headingTrailing.getHeading();
        }
        return header;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getTrailer()
     */
    @Override
    protected String getTrailer() {
        String trailer = null;
        if (m_headingTrailing != null) {
            trailer = m_headingTrailing.getTrailer();
        }
        return trailer;
    }

    /**
     * This method initializes the export and starts the comparison process.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        // Set exports to use MS/windows end of line character for all exports.
        setLineSeparator(FORMAT_EOL_WINDOWS);
        m_columnNames = new TreeSet<String>();
        Integer fileSeq = (Integer) getParameter(PARAM_FILE_SEQ);
        int iSeq = fileSeq == null ? 1 : fileSeq.intValue();
        m_headingTrailing = new TNHeadingTrailing(getOrganization(), iSeq);
        m_schoolYearContextOid = (String) getParameter(ToolInput.CONTEXT_OID_PARAM);
        m_changeIndicatorColumnName = (String) getParameter(COMPARISON_INDICATOR_COLUMN);
        m_insertedRecordIndicator = (String) getParameter(PARAM_INSERTED_RECORD_IND);
        m_updatedRecordIndicator = (String) getParameter(PARAM_UPDATED_RECORD_IND);
        m_deletedRecordIndicator = (String) getParameter(PARAM_DELETED_RECORD_IND);
        m_bulkComparison = (Boolean) getParameter(PARAM_BULK_COMPARISON_INDICATOR);
        m_markResultsProcessed = (Boolean) getParameter(PARAM_MARK_RESULTS);

        m_printErrors = getParameter(PARAM_PRINT_ERRORS) != null ? (Boolean) getParameter(PARAM_PRINT_ERRORS)
                : Boolean.FALSE;
        SisDistrictSchoolYearContext context = (SisDistrictSchoolYearContext) getBroker()
                .getBeanByOid(SisDistrictSchoolYearContext.class, m_schoolYearContextOid);

        if (context != null) {
            m_schoolYearContext = String.valueOf(context.getSchoolYear());
        }

        for (Object key : getParameters().keySet()) {
            if (key != null && key.toString().startsWith(PARAM_PREVIOUS_RESULT)) {
                m_reportParams.add(key.toString().replace(PARAM_PREVIOUS_RESULT, ""));
            }
        }

        if (getParameter(PARAM_EXPORT_IDS) != null) {
            String[] exportIds = ((String) getParameter(PARAM_EXPORT_IDS)).split(",");

            for (String exportId : exportIds) {
                ExportFormatDefinition exportDefinition =
                        (ExportFormatDefinition) getBroker().getBeanByOid(ExportFormatDefinition.class,
                                exportId);
                m_exportDefinitionList.add(exportDefinition);
            }
        }

        m_comparisonManager = ExportResultsComparisonManager.getComparisonManagerFromProcedure(TN_COMPARE_MGR_PROC_ID,
                getBroker(), getParameters(),
                getLocale(), null);
        if (m_comparisonManager != null) {
            try {
                AppGlobals.getLog().log(Level.INFO,
                        "Comparison Manager Loaded: " + m_comparisonManager.getClass().getName());
                // Reflection must be used because of the class loading for
                // related procedures
                Method method = m_comparisonManager.getClass().getDeclaredMethod("initializeBroker",
                        new Class[] {X2Broker.class});
                method.invoke(m_comparisonManager, getBroker());

                m_errorList.addAll(m_comparisonManager.getErrors());
            } catch (Exception e) {
                // DO nothing
                AppGlobals.getLog().log(Level.SEVERE, "Comparison Manager initialize failure: " + e.toString());
                m_errorList.add("Cannot initialize Comparison Manager");
            }
        } else {
            m_errorList.add("Cannot load Comparison Manager with procedure id " + TN_COMPARE_MGR_PROC_ID);
        }
        setIncludeHeaderRow(false);
        setUseEscapes(false);
        setUseValueDelimiters(false);
        if (getParameter(PARAM_USE_CSV) != null && ((Boolean) getParameter(PARAM_USE_CSV)).booleanValue()) {
            setValueDelimiter(Character.valueOf(','));
            setUseValueDelimiters(true);
        }
        setUseValueWrappers(false);
    }

    /**
     * Populate data grid with compared rows from old result and new results.
     *
     * @param dataGrid DataGrid
     * @param oldResult ExportFormatResult
     * @param newResult ExportFormatResult
     * @return int
     * @throws X2BaseException exception
     */
    private int gatherReportData(DataGrid dataGrid, ExportFormatResult oldResult, ExportFormatResult newResult)
            throws X2BaseException {
        int result;
        if (oldResult == null || newResult == null) {
            result = 0;
        } else {
            Collection<ExportFormatField> exportFormatFields =
                    TNExportResultsHelper.getExportFormatField(newResult, getBroker());

            m_comparisonManager.getErrors().clear();

            List<ExportFormatRow> reportData = m_comparisonManager.compareExportResults(oldResult, newResult,
                    m_changeIndicatorColumnName,
                    m_deletedRecordIndicator,
                    m_insertedRecordIndicator,
                    m_updatedRecordIndicator);

            if (!m_comparisonManager.getErrors().isEmpty()) {
                m_errorList.addAll(m_comparisonManager.getErrors());
                result = 0;
            } else {
                result = gatherReportData(dataGrid, reportData, exportFormatFields, true);
            }
        }
        return result;
    }

    /**
     * Populate data grid with restaged export rows.
     *
     * @param dataGrid DataGrid
     * @param restagingResults List<ExportFormatResult>
     * @return int
     * @throws X2BaseException exception
     */
    private int gatherReportData(DataGrid dataGrid, List<ExportFormatResult> restagingResults)
            throws X2BaseException {
        int result;
        if (restagingResults == null || restagingResults.isEmpty()) {
            result = 0;
        } else {
            Collection<ExportFormatField> exportFormatFields =
                    TNExportResultsHelper.getExportFormatField(restagingResults.get(0), getBroker());
            Collection<ExportFormatRow> reportData = new ArrayList<ExportFormatRow>();
            for (ExportFormatResult restagingResult : restagingResults) {
                reportData.addAll(restagingResult.getRows());
            }
            result = gatherReportData(dataGrid, reportData, exportFormatFields, false);
        }
        return result;
    }

    /**
     * Populate data grid with all possible export rows.
     *
     * @param dataGrid DataGrid
     * @param reportData Collection of ExportFormatRow
     * @param exportFormatFields Collection of ExportFormatField
     * @param indRemoveDeletes boolean
     * @return int
     * @throws X2BaseException exception
     */
    private int gatherReportData(DataGrid dataGrid,
                                 Collection<ExportFormatRow> reportData,
                                 Collection<ExportFormatField> exportFormatFields,
                                 boolean indRemoveDeletes)
            throws X2BaseException {
        int result = 0;
        if (!(reportData == null || reportData.isEmpty())) {
            String rowTypeJava = null;
            for (ExportFormatRow entity : reportData) {
                int columnNumber = 10000;
                Map<String, String> exportFormatRow = new HashMap<String, String>();
                StringBuilder uniqueRowValue = new StringBuilder();
                StringBuilder uniqueKeyValue = new StringBuilder();
                for (ExportFormatField exportFormatField : exportFormatFields) {
                    String fieldValue = null;
                    DataFieldConfig config = exportFormatField.getDataFieldConfig();

                    if (rowTypeJava == null && FIELD_RECORD_TYPE.equalsIgnoreCase(exportFormatField.getName())) {
                        rowTypeJava = exportFormatField.getDataFieldConfig().getDataField().getJavaName();
                    }

                    if (config != null) {
                        fieldValue = (String) entity.getFieldValueByBeanPath(config.getDataField().getJavaName());
                    }

                    /*
                     * If the value requires padding, pad it and trim it to field max length.
                     */
                    fieldValue = ExportFormatManager.doPadding(fieldValue == null ? "" : fieldValue,
                            (exportFormatField.getPaddingDirection() == 0
                                    ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                    : exportFormatField.getPaddingDirection()),
                            exportFormatField.getPaddingChar(),
                            exportFormatField.getMaximumLength());
                    // Set the final value.
                    String fieldName = "Column " + columnNumber;
                    m_columnNames.add(fieldName);
                    exportFormatRow.put(fieldName, fieldValue);

                    uniqueRowValue.append(fieldValue);
                    if (exportFormatField.getKeyInd()) {
                        uniqueKeyValue.append(fieldValue);
                    }
                    ++columnNumber;
                }
                if (!m_listUniqueValues.contains(uniqueRowValue.toString())) {
                    if (indRemoveDeletes && DELETE_RECORD.equals(entity.getFieldValueByBeanPath(rowTypeJava))
                            && m_listCurrentKeys.contains(uniqueKeyValue.toString())) {
                        continue;
                    }
                    m_listOfRowsWithUniqueValues.add(entity);
                    m_listUniqueValues.add(uniqueRowValue.toString());
                    result++;
                    dataGrid.append();
                    for (Map.Entry<String, String> entry : exportFormatRow.entrySet()) {
                        dataGrid.set(entry.getKey(), entry.getValue());
                    }
                }
            }
        }
        return result;
    }

    /**
     * Return latest processed result for the current school year for the given export definition.
     *
     * @param importExportDefinitionOid String
     * @param broker X2Broker
     * @return Export format result
     */
    private ExportFormatResult getLastProcessedResult(String importExportDefinitionOid, X2Broker broker) {
        ExportFormatResult result = null;
        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addEqualTo(ExportFormatResult.COL_DEFINITION_OID, importExportDefinitionOid);
        efrCriteria.addBeginsWith(ExportFormatResult.COL_COMMENT, PROCESSED);
        efrCriteria.addContains(ExportFormatResult.COL_NAME, EXPORT_FOR);
        efrCriteria.addEndsWith(ExportFormatResult.COL_NAME, m_schoolYearContext);
        QueryByCriteria query = new QueryByCriteria(ExportFormatResult.class, efrCriteria);
        query.addOrderByDescending(ExportFormatResult.COL_RUN_DATE);

        QueryIterator results = broker.getIteratorByQuery(query);
        try {
            if (results.hasNext()) {
                result = (ExportFormatResult) results.next();
            }
        } finally {
            if (results != null) {
                results.close();
            }
        }

        return result;
    }

    /**
     * Return latest restaged result for the current school year.
     *
     * @param importExportDefinitionOid String
     * @param broker X2Broker
     * @return List
     */
    private List<ExportFormatResult> getLastRestagingResults(String importExportDefinitionOid, X2Broker broker) {
        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addEqualTo(ExportFormatResult.COL_DEFINITION_OID, importExportDefinitionOid);
        efrCriteria.addBeginsWith(ExportFormatResult.COL_COMMENT, RESTAGING);
        efrCriteria.addEndsWith(ExportFormatResult.COL_NAME, m_schoolYearContext);
        QueryByCriteria query = new QueryByCriteria(ExportFormatResult.class, efrCriteria);
        query.addOrderByDescending(ExportFormatResult.COL_RUN_DATE);

        Map<String, List<ExportFormatResult>> mapToPut =
                broker.getGroupedCollectionByQuery(query, ExportFormatResult.COL_DEFINITION_OID, 1024);
        m_restagingResults.putAll(mapToPut);

        return mapToPut != null ? mapToPut.get(importExportDefinitionOid) : new ArrayList<ExportFormatResult>();
    }

    /**
     * Return latest export's result for the current school year.
     *
     * @param importExportDefinitionOid String
     * @param broker X2Broker
     * @return Export format result
     */
    private ExportFormatResult getLatestResult(String importExportDefinitionOid, X2Broker broker) {
        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addEqualTo(ExportFormatResult.COL_DEFINITION_OID, importExportDefinitionOid);
        efrCriteria.addNotEqualTo(ExportFormatResult.COL_COMMENT, PROCESSED);
        efrCriteria.addContains(ExportFormatResult.COL_NAME, EXPORT_FOR);
        efrCriteria.addEndsWith(ExportFormatResult.COL_NAME, m_schoolYearContext);
        QueryByCriteria query = new QueryByCriteria(ExportFormatResult.class, efrCriteria);
        query.addOrderByDescending(ExportFormatResult.COL_RUN_DATE);

        Collection<ExportFormatResult> results = broker.getCollectionByQuery(query);

        return !results.isEmpty() ? (ExportFormatResult) results.toArray()[0] : null;
    }

    /**
     * Return definition name.
     *
     * @param definition ExportFormatDefinition
     * @return String
     */
    private String getReportParam(ExportFormatDefinition definition) {
        String[] split = definition.getName().split(" ");
        return split.length > 1 ? split[1] : "";
    }

    /**
     * Validate input. There are have to be at least 2 export results.
     *
     * @param definition ExportFormatDefinition
     * @return true, if successful
     */
    private boolean validateInput(ExportFormatDefinition definition) {
        boolean value = true;
        ExportFormatResult prior = getLastProcessedResult(definition.getOid(), getBroker());
        ExportFormatResult next = getLatestResult(definition.getOid(), getBroker());
        List<ExportFormatResult> restagingResults = getLastRestagingResults(definition.getOid(), getBroker());

        if (prior == null && CollectionUtils.isEmpty(restagingResults)) {
            m_errorList.add("Processed result not found for " + definition.getId());
            value = false;
        }
        if (next == null && CollectionUtils.isEmpty(restagingResults)) {
            m_errorList.add("Latest result not found for " + definition.getId());
            value = false;
        }
        if (prior != null && next != null) {
            if (prior.getOid().equals(next.getOid()) && CollectionUtils.isEmpty(restagingResults)) {
                m_errorList.add("Latest result matches processed result for " + definition.getId());
                value = false;
            } else if (prior.getRunDate() > next.getRunDate()) {
                m_errorList.add("Latest result is earlier than processed result for " + definition.getId());
                value = false;
            } else {
                m_processedResults.put(definition.getOid(), prior);
                m_latestResults.put(definition.getOid(), next);
            }
        }
        return value;
    }
}
