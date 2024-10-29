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
package com.x2dev.procedures.sys.shared;

import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ExportFormatDataGrid;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * This is a report class that performs standardized data export for
 * the state report infrastructure.
 * This class will identify a procedure that contains a state report definition.
 * It will use that definition to find all data definitions and produce
 * an export file.
 *
 * @author X2 Development Corporation
 */
public class ExportFormatExport extends ExportJavaSource {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

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
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid dataGrid = null;
        if (m_reportData != null && m_initErrors.size() == 0) {
            dataGrid = new ExportFormatDataGrid(m_reportData, this, m_saveResults);
        } else {
            dataGrid = new DataGrid(1, 1);
        }
        return dataGrid;
    }

    /**
     * Returns a list of export field names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List fields = null;
        if (m_reportData != null) {
            fields = new ArrayList(m_reportData.getFieldCount());
            for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                fields.add(m_reportData.getFieldDefinition(pos).getFieldId());
            }

        }
        return fields;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return getColumnNames();
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
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
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        String header = null;
        if (m_reportData != null) {
            header = m_reportData.getHeading();
        }
        return header;
    }

    /**
     * Gets the trailer.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getTrailer()
     */
    @Override
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
        // Set exports to use MS/windows end of line character for all exports.
        setLineSeparator(FORMAT_EOL_WINDOWS);

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

                // Set export parameters from the report data object.
                setEscapeCharacter(m_reportData.getEscapeCharacter());
                setIncludeHeaderRow(m_reportData.getIncludeHeaderRow());
                setUseEscapes(m_reportData.getUseEscapes());
                setUseValueDelimiters(m_reportData.getUseValueDelimiters());
                setUseValueWrappers(m_reportData.getUseValueWrappers());
                setValueDelimiter(m_reportData.getValueDelimiter());
                setValueWrapper(m_reportData.getValueWrapper());
            } catch (X2BaseException x2be) {
                String init_msg =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(INITIALIZE_KEY);
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }
    }
}
