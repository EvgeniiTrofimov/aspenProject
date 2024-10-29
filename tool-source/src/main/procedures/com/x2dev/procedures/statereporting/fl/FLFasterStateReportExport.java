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
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.JobResult;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.sys.shared.StateReportExport;
import com.x2dev.utils.DataExporter;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;

/**
 * The Class FLFasterStateReportExport.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class FLFasterStateReportExport extends StateReportExport {

    /**
     * The Class FasterDataExporter.
     */
    private class FasterDataExporter extends DataExporter {

        /**
         * Instantiates a new faster data exporter.
         *
         * @param outputStream OutputStream
         * @param initialCapacity int
         * @param characterEncoding String
         */
        public FasterDataExporter(OutputStream outputStream, int initialCapacity, String characterEncoding) {
            super(outputStream, initialCapacity, characterEncoding);
        }

        /**
         * Write data.
         *
         * @param dataGrid DataGrid
         * @param columnNames List
         * @see com.x2dev.utils.DataExporter#writeData(com.x2dev.utils.DataGrid, java.util.List)
         */
        @Override
        public void writeData(DataGrid dataGrid, List columnNames) {
            int originalRow = dataGrid.currentRowNumber();

            dataGrid.beforeTop();
            while (dataGrid.next()) {
                startLine();
                String errorId = (String) dataGrid.get(COLUMN_NAME_ERROR_ID);
                if (errorId != null) {
                    String entityName = (String) dataGrid.get(COLUMN_NAME_ENTITY_NAME);
                    String errorMessage = (String) dataGrid.get(COLUMN_NAME_ERROR_MESSAGE);
                    String fieldName = (String) dataGrid.get(COLUMN_NAME_FIELD_NAME);
                    writeValue(COLUMN_NAME_ENTITY_NAME + "\t", false);
                    writeValue(COLUMN_NAME_FIELD_NAME + "\t", false);
                    writeValue(COLUMN_NAME_ERROR_ID + "\t", false);
                    writeValue(COLUMN_NAME_ERROR_MESSAGE + "\t", true);
                    endLine();
                    startLine();
                    writeValue(entityName + "\t", false);
                    writeValue(fieldName + "\t", false);
                    writeValue(errorId + "\t", false);
                    writeValue(errorMessage + "\t", true);
                    endLine();
                } else {
                    String recordType = (String) dataGrid.get(COLUMN_NAME_RECORD_TYPE);
                    recordType = recordType.substring(1);
                    if (recordType.equals("99")) {
                        String nationalRecordType = (String) dataGrid.get(COLUMN_NAME_NATIONAL_RECORD_TYPE);
                        recordType = recordType + (nationalRecordType == null ? "" : nationalRecordType.trim());
                    }
                    Iterator columns = m_helper.getColumnNamesByRecordType(recordType).iterator();
                    int lineLength = 0;
                    while (columns.hasNext()) {
                        String columnName = (String) columns.next();
                        String value = (String) dataGrid.get(columnName);
                        writeValue(value, !columns.hasNext());
                        lineLength += value.length();
                        if (lineLength >= 1020) {
                            break;
                        }
                    }
                }

                endLine();
            }

            dataGrid.gotoRow(originalRow);
        }
    }

    public static final String COLUMN_NAME_ERROR_ID = "Error ID";
    public static final String COLUMN_NAME_ERROR_MESSAGE = "Error message";
    public static final String COLUMN_NAME_ENTITY_NAME = "Entity name";
    public static final String COLUMN_NAME_FIELD_NAME = "Field Name";
    public static final String COLUMN_NAME_NATIONAL_RECORD_TYPE = "National Record Type";
    public static final String COLUMN_NAME_RECORD_TYPE = "Record Type";

    private static final String INITIALIZE_KEY = "label.state.report.initialize";
    private static final String PROCEDURE_ID = "procedureId";

    private List<String> m_columnNames = null;
    private Collection<StateReportValidationError> m_initErrors = null;
    private StateReportData m_reportData = null;
    private Map<ExportFormatResult, List<ExportFormatRow>> m_resultRows = new HashMap<>();
    private FLFasterExportConfiguration m_helper = null;

    public FLFasterExportConfiguration getHelper() {
        return m_helper;
    }

    /**
     * Gets the result rows.
     *
     * @return Map
     */
    public Map<ExportFormatResult, List<ExportFormatRow>> getResultRows() {
        return m_resultRows;
    }

    /**
     * Writes the passed data grid to the passed output stream.
     *
     * @param outputStream OutputStream
     * @param grid DataGrid
     * @param columnNames List<String>
     * @param columnUserNames List<String>
     * @param characterEncoding String
     * @param lineSeparator String
     * @param header String
     * @param trailer String
     * @param escapeCharacter Character
     * @param valueDelimiter Character
     * @param valueWrapper Character
     * @param includeHeaderRow boolean
     * @param useEscapes boolean
     * @param useValueDelimiters boolean
     * @param useValueWrappers boolean
     */
    public void writeFasterData(OutputStream outputStream,
                                DataGrid grid,
                                List<String> columnNames,
                                List<String> columnUserNames,
                                String characterEncoding,
                                String lineSeparator,
                                String header,
                                String trailer,
                                Character escapeCharacter,
                                Character valueDelimiter,
                                Character valueWrapper,
                                boolean includeHeaderRow,
                                boolean useEscapes,
                                boolean useValueDelimiters,
                                boolean useValueWrappers) {
        int initialSize = grid.rowCount() * columnNames.size() * 100;

        if (initialSize <= 0 || initialSize > 32000000) // Limit buffer size to 32M.
        {
            initialSize = 32000000;
        }

        DataExporter exporter = new FasterDataExporter(outputStream, initialSize, characterEncoding);

        try {
            exporter.setLineSeparator(lineSeparator);
            exporter.setOutputType(DataExporter.OUTPUT_TEXT);

            if (escapeCharacter != null) {
                exporter.setEscapeCharacter(escapeCharacter.charValue());
            }

            if (valueDelimiter != null) {
                exporter.setValueDelimiter(valueDelimiter.charValue());
            }

            if (valueWrapper != null) {
                exporter.setValueWrapper(valueWrapper.charValue());
            }

            exporter.setUseEscapes(useEscapes);
            exporter.setUseValueDelimiters(useValueDelimiters);
            exporter.setUseValueWrappers(useValueWrappers);

            /*
             * Header data block.
             */
            if (!StringUtils.isEmpty(header)) {
                exporter.write(header);
            }

            /*
             * Start table.
             */
            exporter.startTable();

            /*
             * Header row
             */
            if (includeHeaderRow) {
                exporter.setHtmlStyle("toolExportHeader");
                exporter.startLine();

                Iterator columns = columnUserNames.iterator();
                while (columns.hasNext()) {
                    exporter.writeValue((String) columns.next(), !columns.hasNext());
                }

                exporter.endLine();
                exporter.setHtmlStyle(null);
            }

            /*
             * Data rows
             */
            exporter.writeData(grid, columnNames);

            /*
             * Final formatting
             */
            exporter.endTable();

            /*
             * Trailer data block.
             */
            if (!StringUtils.isEmpty(trailer)) {
                exporter.write(trailer);
            }
        } finally {
            exporter.close();
        }
    }

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        long rowNumber = 0;
        int rowcount = 1;
        if (m_reportData != null) {
            rowcount = m_reportData.getFieldCount();
        }
        DataGrid dataGrid = new DataGrid(rowcount);
        if (m_initErrors.size() == 0) {
            ExportFormatResult saveResult =
                    X2BaseBean.newInstance(ExportFormatResult.class, getBroker().getPersistenceKey());
            OrganizationManager.setOrganizationOids(saveResult, m_reportData.getOrganization());
            saveResult.setRunDate(System.currentTimeMillis());
            saveResult.setName(m_reportData.getExportTitle());
            saveResult.setDefinitionOid(m_reportData.getEfdOid());
            getBroker().saveBeanForced(saveResult);

            if (m_reportData.open()) {
                try {
                    ArrayList<ExportFormatRow> rows = new ArrayList<>();
                    m_resultRows.put(saveResult, rows);
                    StateReportEntity entity = null;
                    while ((entity = m_reportData.next()) != null) {
                        StateReportValidationError err = entity.filterEntity();
                        dataGrid.append();
                        if (err == null) {
                            entity.preProcess();
                            entity.setScriptManager(m_reportData.getScriptManager());

                            rowNumber++;
                            ExportFormatRow saveRow =
                                    X2BaseBean.newInstance(ExportFormatRow.class, getBroker().getPersistenceKey());
                            if (!StringUtils.isEmpty(entity.getSchoolOid())) {
                                saveRow.setSchoolOid(entity.getSchoolOid());
                            }
                            saveRow.setResultOid(saveResult.getOid());
                            saveRow.setDefinitionOid(m_reportData.getEfdOid());
                            saveRow.setSortOrder(new BigDecimal(rowNumber));
                            saveRow.setSourceOid(entity.getBean().getOid());
                            String rowName = entity.getEntityName();
                            if (rowName != null && rowName.length() > 50) {
                                rowName = rowName.substring(0, 50);
                            }
                            saveRow.setDescription(rowName);

                            /*
                             * Add all fields
                             */
                            for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                                FieldDefinition field = m_reportData.getFieldDefinition(pos);
                                String fieldValue = entity.getFieldValue(pos);

                                /*
                                 * If the value requires padding, pad it and trim it to field max
                                 * length.
                                 */
                                fieldValue = ExportFormatManager.doPadding(fieldValue,
                                        (field.getResizeMode() == null
                                                ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                                : field.getResizeMode().ordinal()),
                                        field.getPaddingChar(),
                                        field.getExportLength());

                                /*
                                 * Save field value into CustomExportRow before
                                 * padding/trimming.
                                 */
                                String saveField = field.getSaveBeanPath();
                                if (!StringUtils.isEmpty(saveField)) {
                                    try {
                                        WebUtils.setProperty(saveRow, saveField, fieldValue);
                                    } catch (RuntimeException re) {
                                        // Ignore: the value was not saved, probably an invalid
                                        // field name.
                                    }
                                }

                                dataGrid.set(field.getFieldId(), fieldValue);
                            }
                            entity.postProcess();

                            rows.add(saveRow);
                        } else {
                            m_initErrors.add(err);
                        }
                    }
                } finally {
                    m_reportData.close();
                }

                for (Entry<ExportFormatResult, List<ExportFormatRow>> entry : m_resultRows.entrySet()) {
                    List<ExportFormatRow> rows = entry.getValue();
                    Collections.sort(rows, new Comparator<ExportFormatRow>() {
                        @Override
                        public int compare(ExportFormatRow row1, ExportFormatRow row2) {
                            int compResult = row1.getDescription().compareTo(row2.getDescription());
                            if (compResult == 0) {
                                String row1SklYearWithCentPath = m_helper.getFieldJavaName(row1.getDefinitionOid(),
                                        FLFasterExportConfiguration.FIELD_SKL_YEAR_WITH_CENT);
                                String row2SklYearWithCentPath = m_helper.getFieldJavaName(row2.getDefinitionOid(),
                                        FLFasterExportConfiguration.FIELD_SKL_YEAR_WITH_CENT);

                                if (row1SklYearWithCentPath != null && row2SklYearWithCentPath != null) {
                                    String row1SchoolYearCent =
                                            (String) row1.getFieldValueByBeanPath(row1SklYearWithCentPath);
                                    String row2SchoolYearCent =
                                            (String) row2.getFieldValueByBeanPath(row2SklYearWithCentPath);
                                    compResult = row1SchoolYearCent.compareTo(row2SchoolYearCent);
                                }

                                if (compResult == 0) {
                                    String row1RecordTypePath = m_helper.getFieldJavaName(row1.getDefinitionOid(),
                                            FLFasterExportConfiguration.FIELD_RECORD_TYPE);
                                    String row2RecordTypePath = m_helper.getFieldJavaName(row2.getDefinitionOid(),
                                            FLFasterExportConfiguration.FIELD_RECORD_TYPE);

                                    String row1RecordType = (String) row1.getFieldValueByBeanPath(row1RecordTypePath);
                                    String row2RecordType = (String) row2.getFieldValueByBeanPath(row2RecordTypePath);

                                    if (!row1RecordType.startsWith("G") && row2RecordType.startsWith("G")) {
                                        compResult = -1;
                                    } else if (row1RecordType.startsWith("G") && !row2RecordType.startsWith("G")) {
                                        compResult = 1;
                                    } else {
                                        compResult = row1RecordType.substring(1, 3)
                                                .compareTo(row2RecordType.substring(1, 3));
                                    }
                                }
                            }
                            return compResult;
                        }
                    });

                    DataGrid sortedDataGrid = new DataGrid();

                    for (int i = 0; i < rows.size(); i++) {
                        ExportFormatRow row = rows.get(i);
                        int currentOrder = row.getSortOrder().intValue();
                        row.setSortOrder(new BigDecimal(i + 1));
                        getBroker().saveBean(row);
                        sortedDataGrid.insertRow(i, dataGrid.getRows().get(currentOrder - 1));
                    }

                    dataGrid = sortedDataGrid;
                }
            }

            // If the report has a heading or trailer, save it to the parent record.
            if ((!StringUtils.isEmpty(m_reportData.getHeading())
                    || !StringUtils.isEmpty(m_reportData.getTrailer()))) {
                saveResult.setHeading(m_reportData.getHeading());
                saveResult.setTrailer(m_reportData.getTrailer());
                getBroker().saveBeanForced(saveResult);
            }

        } else {
            for (StateReportValidationError error : m_initErrors) {
                dataGrid.append();
                dataGrid.set("Entity name", error.getEntityName());
                dataGrid.set("Error ID", error.getErrorId());
                dataGrid.set("Field Name", error.getFieldName());
                dataGrid.set("Error message", error.getErrorMessage());
            }
        }
        dataGrid.beforeTop();

        m_columnNames = dataGrid.getColumns();

        return dataGrid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.procedures.sys.shared.StateReportExport#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List<String> columnNames = super.getColumnNames();
        if (columnNames.size() == 4
                && columnNames.containsAll(Arrays.asList("Entity name", "Error ID", "Field Name", "Error message"))) {
            return columnNames;
        }
        return m_columnNames;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.sys.shared.StateReportExport#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        String procedureId = (String) getParameter(PROCEDURE_ID);
        m_initErrors = new ArrayList<StateReportValidationError>();

        // Lookup State report source data procedure
        m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);
        if (m_reportData != null && m_initErrors.size() == 0) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setCurrentContext(getCurrentContext());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setPrivilegeSet(getPrivilegeSet());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setUser(getUser());
                m_reportData.initializeExport();

                // Set export parameters from the report data object.
                setEscapeCharacter(m_reportData.getEscapeCharacter());
                setIncludeHeaderRow(m_reportData.getIncludeHeaderRow());
                setUseEscapes(m_reportData.getUseEscapes());
                setUseValueDelimiters(m_reportData.getUseValueDelimiters());
                setUseValueWrappers(m_reportData.getUseValueWrappers());
                setValueDelimiter(m_reportData.getValueDelimiter());
                setValueWrapper(m_reportData.getValueWrapper());
            } catch (X2BaseException x2be) {
                String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(INITIALIZE_KEY);
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }

        m_helper = new FLFasterExportConfiguration(getCurrentContext(), null, getBroker());
    }

    /**
     * Write the results of this export.
     *
     * @param dataGrid the results of {@link #gatherData()}
     * @throws Exception exception
     */
    @Override
    protected void writeResults(DataGrid dataGrid) throws Exception {

        String name = getJob().getTool().getName();
        String jobId = String.valueOf(getJob().getJobId());

        String[] logParameters = new String[] {
                name, jobId
        };
        AppGlobals.getLog().log(Level.INFO, LOG_DATA_PREPARED, logParameters);

        /*
         * Set comment into job result.
         */
        String resultOid = getJob().getJobResultOid();
        if (resultOid != null && resultOid.length() > 0) {
            X2Broker broker = getBroker();
            JobResult result = (JobResult) broker.getBeanByOid(JobResult.class, resultOid);
            if (result != null) {
                if (this.getComment() != null) {
                    result.setComments(this.getComment());
                }
                broker.saveBeanForced(result);
            }
        }

        /*
         * If the job has been aborted then don't bother formatting the results.
         */
        if (getJob().getStatus() != ToolJob.STATUS_ABORT) {
            if (dataGrid.rowCount() > 0) {
                OutputStream outputStream = getResultHandler().getOutputStream();
                writeFasterData(outputStream,
                        dataGrid,
                        getColumnNames(),
                        getColumnUserNames(),
                        getCharacterEncoding(),
                        getLineSeparator(),
                        getHeader(),
                        getTrailer(),
                        null,
                        null,
                        null,
                        false,
                        false,
                        false,
                        false);
            } else {
                getResultHandler().setEmpty(true);
            }
        }
    }
}
