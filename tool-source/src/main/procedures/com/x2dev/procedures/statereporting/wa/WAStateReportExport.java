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
package com.x2dev.procedures.statereporting.wa;

import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.JobResult;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.utils.DataExporter;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;

/**
 * This class is created by combining ExportJavaSource and StateReportExport. This was required to
 * generate export files when no rows are returned.
 *
 * @author Follett Software Company
 */
public class WAStateReportExport extends ToolJavaSource {

    private static final String FORMAT_EOL_WINDOWS = "\r\n";

    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    private static final String PROCEDURE_ID = "procedureId";

    private static final String SAVE_RESULTS = "saveResults";

    private Collection<StateReportValidationError> m_initErrors = null;
    private StateReportData m_reportData = null;

    /**
     * A parameter that indicates the export results should be saved in the CustomExportResults
     * table.
     */
    private boolean m_saveResults = false;

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
    public static void writeData(OutputStream outputStream,
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
        int initialSize = grid.rowCount() == 0 ? 10000 : grid.rowCount() * columnNames.size() * 100;
        DataExporter exporter = new DataExporter(outputStream, initialSize, characterEncoding);

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
     * Gathers the data for the export.
     *
     * @return DataGrid all values in this grid must be converted to String equivalents, values may
     *         be null
     *
     * @throws Exception exception
     */
    protected DataGrid gatherData() throws Exception {
        ExportFormatResult saveResult = null;
        ExportFormatRow saveRow = null;
        long rowNumber = 0;
        int rowcount = 1;
        if (m_reportData != null) {
            rowcount = m_reportData.getFieldCount();
        }

        DataGrid dataGrid = new DataGrid(rowcount);
        if (m_initErrors.size() == 0) {
            if (m_saveResults) {
                saveResult = X2BaseBean.newInstance(ExportFormatResult.class, getBroker().getPersistenceKey());
                saveResult.setOrganization1Oid(m_reportData.getOrganization().getOid());
                saveResult.setRunDate(System.currentTimeMillis());
                saveResult.setName(m_reportData.getExportTitle());
                saveResult.setDefinitionOid(m_reportData.getEfdOid());
                getBroker().saveBeanForced(saveResult);
            }

            if (m_reportData.open()) {
                try {
                    StateReportEntity entity = null;
                    while ((entity = m_reportData.next()) != null) {
                        StateReportValidationError err = entity.filterEntity();
                        if (err == null) {
                            entity.preProcess();
                            entity.setScriptManager(m_reportData.getScriptManager());
                            dataGrid.append();

                            if (m_saveResults) {
                                rowNumber++;
                                saveRow =
                                        X2BaseBean.newInstance(ExportFormatRow.class, getBroker().getPersistenceKey());
                                saveRow.setResultOid(saveResult.getOid());
                                saveRow.setDefinitionOid(m_reportData.getEfdOid());
                                saveRow.setSortOrder(new BigDecimal(rowNumber));
                                saveRow.setSourceOid(entity.getBean().getOid());
                                String rowName = entity.getEntityName();
                                if (rowName != null && rowName.length() > 50) {
                                    rowName = rowName.substring(0, 50);
                                }
                                saveRow.setDescription(rowName);
                            }

                            /*
                             * Add all fields
                             */
                            for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                                FieldDefinition field = m_reportData.getFieldDefinition(pos);
                                String fieldValue = entity.getFieldValue(pos);

                                if (m_saveResults) {
                                    /*
                                     * If a value has a specified maximum length, then the field
                                     * that it is
                                     * being saved into also has the specified maximum length,
                                     * So we must trim the value to that maximum length before
                                     * saving.
                                     *
                                     * Ex: Middle name is specified as 10 chars and is assigned to a
                                     * FieldA.
                                     * The value is 12 chars.
                                     * Must trim to 10 prior to saving so it will fit into the
                                     * field.
                                     *
                                     * The case that this might lose data would be in a CSV where
                                     * the length is not
                                     * absolute as it would be in a column width report. The export
                                     * might still
                                     * contain the excessive length but the saved value would not.
                                     * 
                                     * In those cases, the field would generate a validation error
                                     * anyway.
                                     *
                                     * Save happens before padding so pad values do not also get
                                     * saved.
                                     */
                                    String saveFieldValue = ExportFormatManager.doPadding(fieldValue,
                                            ExportFormatField.PaddingDirectionCode.TRUNCATE_ONLY.ordinal(), null,
                                            field.getExportLength());

                                    /*
                                     * Save field value into CustomExportRow before
                                     * padding/trimming.
                                     */
                                    String saveField = field.getSaveBeanPath();
                                    if (!StringUtils.isEmpty(saveField)) {
                                        try {
                                            WebUtils.setProperty(saveRow, saveField, saveFieldValue);
                                        } catch (RuntimeException re) {
                                            // Ignore: the value was not saved, probably an invalid
                                            // field name.
                                        }
                                    }
                                }

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

                                // Set the final value.
                                dataGrid.set(field.getFieldId(), fieldValue);

                            }
                            entity.postProcess();

                            if (m_saveResults) {
                                getBroker().saveBean(saveRow);
                            }

                        } else {
                            m_initErrors.add(err);
                        }
                    }
                } finally {
                    m_reportData.close();
                }
            }

            // If the report has a heading or trailer, save it to the parent record.
            if (m_saveResults && (!StringUtils.isEmpty(m_reportData.getHeading())
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
        return dataGrid;
    }

    /**
     * Returns the list of column names for the export. These are the actual values used for
     * retrieving data from the grid. This list corresponds to the list returned by
     * <code>getColumnUserNames()</code>.
     *
     * @return A List of String objects
     */
    protected List getColumnNames() {
        List fields = null;
        if (m_reportData != null) {
            fields = new ArrayList(m_reportData.getFieldCount());
            for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                fields.add(m_reportData.getFieldDefinition(pos) == null ? ""
                        : m_reportData.getFieldDefinition(pos).getFieldId());
            }

        }
        return fields;
    }

    /**
     * Returns the list of column user names for the export. These are the user-friendly values that
     * describe the data contained in each column of the grid. This list corresponds to the list
     * returned by <code>getColumnNames()</code>.
     *
     * @return A List of String objects
     */
    protected List getColumnUserNames() {
        return getColumnNames();
    }

    /**
     * Returns any comments from the data gather. This method may return null.
     *
     * @return String
     */
    protected String getComment() {
        StringBuilder comment = new StringBuilder();
        String lastName = "";
        if (m_initErrors != null && m_initErrors.size() > 0) {
            for (StateReportValidationError err : m_initErrors) {
                String thisName = err.getEntityName();
                if (!lastName.equals(thisName)) {
                    comment.append(err.getEntityName());
                    comment.append("\n");
                    lastName = thisName;
                }
                comment.append("    ");
                comment.append(err.getFieldName());
                comment.append("   ");
                comment.append(err.getErrorId());
                comment.append("   ");
                comment.append(err.getErrorMessage());
                comment.append("\n");
            }
        }
        return comment.toString();
    }

    /**
     * Returns the file header for this export. This value will be written at the beginning of the
     * file before any data values are written. This method may return null. If it is not null then
     * it should end with the EOL string.
     *
     * @return String
     */
    protected String getHeader() {
        String header = null;
        if (m_reportData != null) {
            header = m_reportData.getHeading();
        }
        return header;
    }

    /**
     * Returns the file trailer for this export. This value will be written at the end of the
     * file after all data values are written. This method may return null. If it is not null then
     * it should end with the EOL string.
     *
     * @return String
     */
    protected String getTrailer() {
        String trailer = null;
        if (m_reportData != null) {
            trailer = m_reportData.getTrailer();
        }
        return trailer;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        // Determine if the results should be saved in the StateReport results tables.
        Boolean saveResults = (Boolean) getParameter(SAVE_RESULTS);
        if (saveResults != null) {
            m_saveResults = saveResults.booleanValue();
        }

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
         * Step 1: Prepare the data
         */
        DataGrid dataGrid = gatherData();

        String[] logParameters = new String[] {name, jobId};
        AppGlobals.getLog().log(Level.INFO, LOG_DATA_PREPARED, logParameters);

        ThreadUtils.checkInterrupt();

        /*
         * Step 2: set comment into job result.
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
            OutputStream outputStream = getResultHandler().getOutputStream();

            writeData(outputStream,
                    dataGrid,
                    getColumnNames(),
                    getColumnUserNames(),
                    getCharacterEncoding(),
                    FORMAT_EOL_WINDOWS,
                    getHeader(),
                    getTrailer(),
                    m_reportData.getEscapeCharacter(),
                    m_reportData.getValueDelimiter(),
                    m_reportData.getValueWrapper(),
                    m_reportData.getIncludeHeaderRow(),
                    m_reportData.getUseEscapes(),
                    m_reportData.getUseValueDelimiters(),
                    m_reportData.getUseValueWrappers());
        }

        cleanup();
    }
}
