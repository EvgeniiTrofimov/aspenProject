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
package com.x2dev.procedures.statereporting.pa;

import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

/**
 * This is a report class that performs standardized data export for
 * the state report infrastructure.
 * This class will identify a procedure that contains a state report definition.
 * It will use that definition to find all data definitions and produce
 * an export file.
 *
 * @author X2 Development Corporation
 */
public class PimsStateReportExport extends ExportJavaSource {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    private static final String PARAM_USER_DATA = "userDataContainer";

    private static final String PROCEDURE_ID = "procedureId";

    private static final String SAVE_RESULTS = "saveResults";

    private static final Map<String, String> TARGET_TABLES;
    static {
        Map<String, String> targTables = new HashMap<String, String>();
        targTables.put("EXP-PA-ACT", "Incident_Offndr_Dsplnry_Action");
        targTables.put("EXP-PA-ACTP", "Incident_Offndr_Parent_Involve");
        targTables.put("EXP-PA-CND", "Incident");
        targTables.put("EXP-PA-CNDO", "Incident_Offndr");
        targTables.put("EXP-PA-CNDV", "Incident_Victim");
        targTables.put("EXP-PA-CNO", "Incident_Offndr_Infraction");
        targTables.put("EXP-PA-CSD", "Pims_School_Calendar");
        targTables.put("EXP-PA-CSK", "Course");
        targTables.put("EXP-PA-CTE", "CTE_Student_Fact");
        targTables.put("EXP-PA-DIS", "District_Fact");
        targTables.put("EXP-PA-ENR", "School_Enroll");
        targTables.put("EXP-PA-LOC", "Location_Fact");
        targTables.put("EXP-PA-MTC", "Crse_Instruct");
        targTables.put("EXP-PA-ORG", "District_Year");
        targTables.put("EXP-PA-PGM", "Programs_Fact");
        targTables.put("EXP-PA-PSN", "Person");
        targTables.put("EXP-PA-SCB", "Student_Fact");
        targTables.put("EXP-PA-SCE", "Course_Enroll");
        targTables.put("EXP-PA-SFP", "Staff_Assignment");
        targTables.put("EXP-PA-SSC", "Staff_Student_Course");
        targTables.put("EXP-PA-STD", "Student");
        targTables.put("EXP-PA-STDC", "Pims_Student_Calendar_Fact");
        targTables.put("EXP-PA-STF", "Staff");
        targTables.put("EXP-PA-SSS", "Staff_Student_Subtest");
        targTables.put("EXP-PA-SIC", "CTE_Student_Credential");

        TARGET_TABLES = Collections.unmodifiableMap(targTables);
    }

    private Collection<StateReportValidationError> m_initErrors = null;

    private StateReportData m_reportData = null;

    /**
     * A parameter that indicates the export results should be saved in the CustomExportResults
     * table.
     */
    private boolean m_saveResults = false;



    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        runOnApplicationServer();
        addParameter(PARAM_USER_DATA, userData);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        StringBuilder filename = new StringBuilder();

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMddHHmm");

        String toolNamePrefix = "PIMS";
        String toolNamePrefixPa = "PA PIMS";

        String toolName = getJob().getTool().getName();
        String toolId = getJob().getTool().getId().trim();

        if (toolName.startsWith(toolNamePrefix) || toolName.startsWith(toolNamePrefixPa)) {
            // try
            // {
            String pimsReportName = toolName.substring(toolName.indexOf(toolNamePrefix) + toolNamePrefix.length() + 1);

            String targetTable = TARGET_TABLES.get(toolId);

            Organization organization = OrganizationManager.getRootOrganization(getBroker());

            filename.append(organization.getId());
            filename.append("_");
            if (targetTable != null) {
                filename.append(targetTable);
            } else {
                filename.append(pimsReportName.trim());
            }
            filename.append("_");

            long utcTime = System.currentTimeMillis() - TimeZone.getDefault().getRawOffset();
            long convertedTime = utcTime + OrganizationManager.getTimeZone(organization).getRawOffset();

            // Calendar calendar = Calendar.getInstance(TimeZone.getDefault());
            // calendar.setTimeInMillis(System.currentTimeMillis());
            // calendar.setTimeZone(OrganizationManager.getTimeZone(getOrganization()));
            //
            filename.append(dateFormat.format(new Date(convertedTime)));
            filename.append(".tab");
            // }
            // catch (StringIndexOutOfBoundsException sioobe)
            // {
            // return super.getCustomFileName();
            // }
        }

        return filename.toString();
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
                fields.add(m_reportData.getFieldDefinition(pos) == null ? ""
                        : m_reportData.getFieldDefinition(pos).getFieldId());
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
    }
}
