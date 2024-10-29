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
package com.x2dev.procedures.statereporting;
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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ExportResultsComparisonManager;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class generates an export text file after comparing two export format results that have the
 * same underlying export format definition.
 *
 * @author X2 Development Corporation
 */
public class ExportResultsComparisonExport extends ExportJavaSource {

    private static final long serialVersionUID = 1L;

    private static final String PROCEDURE_ID = "procedureId";

    private static final String SAVE_RESULTS = "saveResults";

    private Collection<ExportFormatRow> m_reportData = null;

    private ExportResultsComparisonManager m_comparisonManager = null;

    /**
     * A parameter that indicates the export results should be saved in the CustomExportResults
     * table.
     */
    private boolean m_saveResults = false;

    private String m_changeIndicatorColumnName;

    private String m_insertedRecordIndicator;

    private String m_deletedRecordIndicator;

    private String m_updatedRecordIndicator;

    private static final String RESULT1 = "result1";

    private static final String RESULT2 = "result2";

    private static final String COMPARISON_INDICATOR_COLUMN = "compIndicator";

    private static final String INSERTED_RECORD_IND = "insertRecInd";

    private static final String DELETED_RECORD_IND = "deleteRecInd";

    private static final String UPDATED_RECORD_IND = "updateRecInd";


    private ExportFormatDefinition m_exportFormatDefinition = null;

    private ExportFormatResult m_oldResult = null;

    private ExportFormatResult m_newResult = null;

    private String m_delimiter = null;

    private String m_escapeChar = null;

    private String m_wrapper = null;

    protected Collection<ExportFormatField> m_exportFormatFields = null;

    private List<String> m_errorList = new ArrayList<String>();

    /**
     * This method builds the export grid after the compare is done.
     *
     * @return DataGrid
     * @throws Exception exception
     */
    @Override
    protected DataGrid gatherData() throws Exception {

        ExportFormatResult saveResult = null;
        ExportFormatRow saveRow = null;
        long rowNumber = 0;
        int rowCount = 1;
        if (m_exportFormatFields != null) {
            rowCount = m_exportFormatFields.size();
        }

        // Initializing the grid.
        DataGrid dataGrid = new DataGrid(rowCount);
        if (m_saveResults) {
            saveResult = X2BaseBean.newInstance(ExportFormatResult.class, getBroker().getPersistenceKey());
            saveResult.setOrganization1Oid(getOrganization().getOid());
            saveResult.setRunDate(System.currentTimeMillis());
            if (m_newResult != null) {
                saveResult.setName(m_newResult.getName() + " - Diff");
            }
            saveResult.setDefinitionOid(m_exportFormatDefinition.getOid());
            getBroker().saveBeanForced(saveResult);
        }
        for (ExportFormatRow entity : m_reportData) {
            dataGrid.append();
            if (m_saveResults) {
                rowNumber++;
                saveRow = (ExportFormatRow) entity.copyBean();
                saveRow.setResultOid(saveResult.getOid());
                saveRow.setSortOrder(new BigDecimal(rowNumber));
            }
            /*
             * Add all fields
             */
            for (ExportFormatField exportFormatField : m_exportFormatFields) {
                String fieldValue = null;
                DataFieldConfig config = exportFormatField.getDataFieldConfig();
                if (config != null) {
                    fieldValue = (String) entity.getFieldValueByBeanPath(config.getDataField().getJavaName());
                }

                if (m_saveResults) {
                    /*
                     * If a value has a specified maximum length, then the field that it is
                     * being saved into also has the specified maximum length,
                     * So we must trim the value to that maximum length before saving.
                     * 
                     * Ex: Middle name is specified as 10 chars and is assigned to a FieldA.
                     * The value is 12 chars.
                     * Must trim to 10 prior to saving so it will fit into the field.
                     * 
                     * The case that this might lose data would be in a CSV where the length is not
                     * absolute as it would be in a column width report. The export might still
                     * contain the excessive length but the saved value would not.
                     * 
                     * In those cases, the field would generate a validation error anyway.
                     * 
                     * Save happens before padding so pad values do not also get saved.
                     */
                    String saveFieldValue = ExportFormatManager.doPadding(fieldValue,
                            ExportFormatField.PaddingDirectionCode.TRUNCATE_ONLY.ordinal(), null,
                            exportFormatField.getMaximumLength());

                    /*
                     * Save field value into CustomExportRow before padding/trimming.
                     */
                    String saveField = null;
                    if (config != null) {
                        saveField = config.getDataField().getJavaName();
                    }
                    if (!StringUtils.isEmpty(saveField)) {
                        try {
                            WebUtils.setProperty(saveRow, saveField, saveFieldValue);
                        } catch (RuntimeException re) {
                            // Ignore: the value was not saved, probably an invalid field name.
                        }
                    }
                }

                /*
                 * If the value requires padding, pad it and trim it to field max length.
                 */
                fieldValue = ExportFormatManager.doPadding(fieldValue,
                        (exportFormatField.getPaddingDirection() == 0
                                ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                : exportFormatField.getPaddingDirection()),
                        exportFormatField.getPaddingChar(),
                        exportFormatField.getMaximumLength());

                // Set the final value.
                dataGrid.set(exportFormatField.getName(), fieldValue);

            }

            if (m_saveResults) {
                getBroker().saveBean(saveRow);
            }
        }

        // If the report has a heading or trailer, save it to the parent record.
        if (m_saveResults && (!StringUtils.isEmpty(getHeader()) || !StringUtils.isEmpty(getTrailer()))) {
            saveResult.setHeading(getHeader());
            saveResult.setTrailer(getTrailer());
            getBroker().saveBeanForced(saveResult);
        }
        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Returns a list of export column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List<String> fields = new ArrayList<String>();
        if (m_exportFormatFields != null && m_exportFormatFields.size() != 0) {
            for (ExportFormatField exportFormatField : m_exportFormatFields) {
                fields.add(exportFormatField.getName());
            }
        }
        return fields;
    }

    /**
     * Returns a list of export column usernames.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return getColumnNames();
    }

    /**
     * Returns the header for the export.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        String header = null;
        if (m_comparisonManager != null) {
            header = m_comparisonManager.getHeader();
        } else {
            if (m_exportFormatDefinition != null) {
                header = m_exportFormatDefinition.getHeading();
            }
        }
        return header;
    }

    /**
     * Returns the trailer for the export.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getTrailer()
     */
    @Override
    protected String getTrailer() {
        String trailer = null;
        if (m_comparisonManager != null) {
            trailer = m_comparisonManager.getTrailer();
        } else {
            if (m_exportFormatDefinition != null) {
                trailer = m_exportFormatDefinition.getTrailer();
            }
        }
        return trailer;
    }

    /**
     * This method returns the String, which is essentially a concatenation of all the errors.
     * This will be used by the job that executes the compare procedure.
     *
     * @return String
     */
    @Override
    protected String getComment() {
        String errors = "";
        if (m_errorList.isEmpty()) {
            for (String error : m_errorList) {
                errors = errors.concat(error);
            }
        }
        return errors;
    }

    /**
     * This method initializes the export and starts the comparison process.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        initializeFields();
        // Set exports to use MS/windows end of line character for all exports.
        setLineSeparator(FORMAT_EOL_WINDOWS);
        String procedureId = (String) getParameter(PROCEDURE_ID);
        m_comparisonManager = ExportResultsComparisonManager.getComparisonManagerFromProcedure(procedureId, getBroker(),
                getParameters(), getLocale(), null);
        m_reportData = m_comparisonManager.compareExportResults(m_oldResult, m_newResult, m_changeIndicatorColumnName,
                m_deletedRecordIndicator, m_insertedRecordIndicator, m_updatedRecordIndicator);

        if (!m_comparisonManager.getErrors().isEmpty()) {
            m_errorList.addAll(m_comparisonManager.getErrors());
        } else {
            if (m_reportData != null && m_reportData.size() != 0) {
                // Set export parameters from the report data object.
                setUseEscapes(true);
                setUseValueDelimiters(true);
                setUseValueWrappers(true);
                setIncludeHeaderRow(true);
                if (m_escapeChar != null) {
                    setEscapeCharacter(Character.valueOf(m_escapeChar.charAt(0)));
                }
                if (m_delimiter != null) {
                    setValueDelimiter(Character.valueOf(m_delimiter.charAt(0)));
                }
                if (m_wrapper != null) {
                    setValueWrapper(Character.valueOf(m_wrapper.charAt(0)));
                }
            }
        }

        if (m_newResult == null || m_oldResult == null) {
            throw new X2BaseException((String) null,
                    "The Export compare manager requires 2 exports to compare but 2 exports were not found.");

        }


    }

    /**
     * This method initializes all the fields that are needed for the export.
     */
    private void initializeFields() {
        String oldResultOid = (String) getParameter(RESULT1);
        String newResultOid = (String) getParameter(RESULT2);
        m_changeIndicatorColumnName = (String) getParameter(COMPARISON_INDICATOR_COLUMN);
        m_insertedRecordIndicator = (String) getParameter(INSERTED_RECORD_IND);
        m_updatedRecordIndicator = (String) getParameter(UPDATED_RECORD_IND);
        m_deletedRecordIndicator = (String) getParameter(DELETED_RECORD_IND);

        if (!StringUtils.isEmpty(oldResultOid) && !StringUtils.isEmpty(newResultOid)) {
            m_oldResult = getExportFormatResult(oldResultOid);
            m_newResult = getExportFormatResult(newResultOid);

        } else {

            BeanQuery query = new BeanQuery(ExportFormatResult.class);
            query.addOrderBy(ExportFormatResult.COL_RUN_DATE, false);

            QueryIterator iterator = getBroker().getIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    ExportFormatResult result = (ExportFormatResult) iterator.next();
                    if (!result.getName().toLowerCase().contains("diff")) {
                        if (m_newResult == null) {
                            m_newResult = result;
                        } else {
                            m_oldResult = result;
                            break;
                        }
                    }
                }
            } finally {
                iterator.close();
            }
        }

        // Determine if the results should be saved in the StateReport results tables.
        Boolean saveResults = (Boolean) getParameter(SAVE_RESULTS);
        if (saveResults != null) {
            m_saveResults = saveResults.booleanValue();
        }
        if (m_newResult != null) {
            m_exportFormatDefinition = m_newResult.getDefinition();
            m_delimiter = m_exportFormatDefinition.getDelimiterChar();
            m_wrapper = m_exportFormatDefinition.getWrapperChar();
            m_escapeChar = m_exportFormatDefinition.getEscapeChar();
            m_exportFormatFields = getExportFormatField(m_newResult);
        }
    }

    /**
     * This method returns the export format result for the selected result's oid.
     *
     * @param resultOid String
     * @return Export format result
     */
    private ExportFormatResult getExportFormatResult(String resultOid) {
        ExportFormatResult result = null;
        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addEqualTo(X2BaseBean.COL_OID, resultOid);
        QueryByCriteria query = new QueryByCriteria(ExportFormatResult.class, efrCriteria);
        Collection efrCollection = getBroker().getCollectionByQuery(query);
        if (!efrCollection.isEmpty()) {
            result = (ExportFormatResult) (efrCollection.toArray()[0]);
        }
        return result;
    }

    /**
     * This method returns the collection of all the export format fields for the given export
     * format result.
     *
     * @param result ExportFormatResult
     * @return Collection
     */
    private Collection<ExportFormatField> getExportFormatField(ExportFormatResult result) {
        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addEqualTo(X2BaseBean.COL_OID, result.getOid());
        SubQuery efrSubQuery =
                new SubQuery(ExportFormatResult.class, ExportFormatResult.COL_DEFINITION_OID, efrCriteria);

        X2Criteria ukiCriteria = new X2Criteria();
        ukiCriteria.addIn(ExportFormatResult.COL_DEFINITION_OID, efrSubQuery);

        QueryByCriteria query = new QueryByCriteria(ExportFormatField.class, ukiCriteria);
        query.addOrderByAscending(ExportFormatField.COL_POSITION);

        Collection<ExportFormatField> exportFields = getBroker().getCollectionByQuery(query);
        return exportFields;
    }
}
