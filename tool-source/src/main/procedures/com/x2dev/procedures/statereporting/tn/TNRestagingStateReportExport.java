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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

/**
 * Differences from parent are:
 * 1)overridden gatherData() method to add "Restaging" comment and "TN xxx vv Restaging for yyyy"
 * name
 * to exports` result.
 * 2)additional D records are added to top of result`s records.
 *
 * Used in TNRestagingProcedure.java.
 *
 * @author X2 Development Corporation
 */
public class TNRestagingStateReportExport extends TNStateReportExport {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    private static final String FIELD_RECORD_TYPE = "RECORD TYPE";
    private static final String PARAM_DELETE_ROWS_OIDS = "deleteRowsOids";
    private static final String PARAM_DELIMITER = ";";
    private static final String SAVE_RESULTS = "saveResults";

    private Collection<String> m_deleteRows = null;

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
     * @see com.x2dev.procedures.sys.shared.StateReportExport#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        ExportFormatResult saveResult = null;
        boolean saveResultsContainsRows = false;
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
                saveResult.setComment("Restaging");
                saveResult.setOrganization1Oid(m_reportData.getOrganization().getOid());
                saveResult.setRunDate(System.currentTimeMillis());
                saveResult.setDefinitionOid(m_reportData.getEfdOid());

                // Determine names of RECORD ID and RECORD VERSION fields .
                String recordId = null;
                String recordVersionDefaulValue = null;

                Collection<ExportFormatField> fields = saveResult.getDefinition().getFields();
                for (ExportFormatField field : fields) {
                    if (field.getName().equalsIgnoreCase("RECORD ID")) {
                        recordId = field.getName();
                    } else if (field.getName().equalsIgnoreCase("RECORD VERSION")) {
                        recordVersionDefaulValue = field.getDefaultValue();
                    }
                }

                saveResult.setName("TN " + m_reportData.getFieldDefinition(recordId).getDefaultValue() +
                        " " + recordVersionDefaulValue +
                        " Restaging for " + getCurrentContext().getSchoolYear());

                getBroker().saveBeanForced(saveResult);
            }
            // Save delete rows.
            if (m_saveResults) {
                for (String rowOid : m_deleteRows) {
                    rowNumber++;
                    ExportFormatRow row = (ExportFormatRow) getBroker().getBeanByOid(ExportFormatRow.class, rowOid);

                    ExportFormatDefinition definition = row.getDefinition();
                    ExportFormatResult result = row.getResult();

                    ArrayList<ExportFormatField> fields = new ArrayList<ExportFormatField>();
                    if (result != null && (definition = result.getDefinition()) != null) {
                        fields.addAll(definition.getFields(getBroker()));
                    }

                    Map<String, ArrayList<String>> fieldsMap = getMapOfKeyIndexes(fields);

                    String recordTypeBeanPath = (String) fieldsMap.keySet().toArray()[0];

                    saveRow = (ExportFormatRow) row.copyBean();
                    saveRow.setFieldValueByBeanPath(recordTypeBeanPath, "D");
                    saveRow.setResultOid(saveResult.getOid());
                    saveRow.setSortOrder(new BigDecimal(rowNumber));
                    getBroker().saveBeanForced(saveRow);
                    saveResultsContainsRows = true;
                }
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
                                saveResultsContainsRows = true;
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
            if (m_saveResults && !saveResultsContainsRows) {
                getBroker().deleteBean(saveResult);
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
     * @see com.x2dev.procedures.statereporting.tn.TNStateReportExport#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        // Get delete rows.
        String deleteRowsParam = (String) getParameter(PARAM_DELETE_ROWS_OIDS);

        m_deleteRows = new ArrayList<String>();
        if (!StringUtils.isEmpty(deleteRowsParam)) {
            m_deleteRows.addAll(Arrays.asList(deleteRowsParam.split(PARAM_DELIMITER)));
        }
        // Determine if the results should be saved in the StateReport results tables.
        Boolean saveResults = (Boolean) getParameter(SAVE_RESULTS);
        if (saveResults != null) {
            m_saveResults = saveResults.booleanValue();
        }
    }

    /**
     * Return Map of java names of ExportFormat fields with effKeyInd = 1 keyed on java name of
     * RECORD TYPE field.
     *
     * @param fields ArrayList<ExportFormatField>
     * @return Map
     */
    private Map<String, ArrayList<String>> getMapOfKeyIndexes(ArrayList<ExportFormatField> fields) {
        String javaName = null;
        DataFieldConfig fdd = null;
        Map<String, ArrayList<String>> fieldsMap = new HashMap<String, ArrayList<String>>();
        ArrayList<String> keyInds = new ArrayList<String>();
        if (fields != null) {
            Collections.sort(fields, new Comparator<ExportFormatField>() {
                @Override
                public int compare(ExportFormatField eff1, ExportFormatField eff2) {

                    return eff1.getPosition() - eff2.getPosition();
                }
            });

            for (ExportFormatField field : fields) {
                if (field.getKeyInd()) {
                    keyInds.add(field.getDataFieldConfig().getDataField().getJavaName());
                }

                if (FIELD_RECORD_TYPE.equals(field.getName())) {
                    fdd = field.getDataFieldConfig();
                }
            }
        }

        if (fdd != null && fdd.getDataField() != null) {
            javaName = fdd.getDataField().getJavaName();
            fieldsMap.put(javaName, keyInds);
        }

        return fieldsMap;
    }
}
