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
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.k12.beans.DataRequest;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This export identifies all DataRequest records that are incoming requests
 * that are flagged as type FASTER and have a status of identified and ready
 * to send.
 * It gathers the necessary data for the request type and formats and adds the
 * necessary records to the output file.
 * It also marks the rDataRequest records as sent.
 *
 * @author X2 Development Corporation
 */
public class FASTERReply extends ExportJavaSource {
    // private static final String PARAM_UPDATE = "updateRequest";

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    // private static final String CALC_DEMO = "olddemo";
    private static final String GRID_COL_NAME = "gridrow";

    /**
     * m_update indicates the DataRequest records status should be updated to closed
     * after the send.
     */
    // private boolean m_update = false;

    protected DataDictionary m_dictionary = null;

    /**
     * A map of custom export field lists.
     * Each entry in the map represents a record format to be used in the export.
     * A definition and field list will exist for each record type in the export.
     * The definition ID will be called on for field definitions.
     */
    protected Map<String, List<ExportFormatField>> m_definitionsMap = new HashMap<String, List<ExportFormatField>>();

    /**
     * Once a definitionsMap entry is selected, it is processed into
     * a list of field definitions for actual use.
     */
    protected Map<String, List<FieldDefinition>> m_fieldsMap = new HashMap<String, List<FieldDefinition>>();

    /**
     * This method gathers the main data for output.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        /*
         * retrieve user parameter values.
         */
        // Boolean updateFlag = (Boolean) getParameter(PARAM_UPDATE);
        // m_update = (updateFlag != null && updateFlag.booleanValue());

        /*
         * Lookup aliases and fields.
         */

        /*
         * Initialize data grid and processing query.
         */
        DataGrid grid = new DataGrid(1);
        QueryByCriteria query = getQuery();

        /*
         * Generate output.
         */
        generateData(query, grid);

        return grid;
    }

    /**
     * This method processes the query, iterating through all data requests
     * and determines what type of response is appropriate.
     * It then calls the necessary record generators to provide the data.
     *
     * @param query QueryByCriteria
     * @param grid DataGrid
     * @throws X2BaseException exception
     */
    private void generateData(QueryByCriteria query, DataGrid grid) throws X2BaseException {
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                DataRequest request = (DataRequest) iterator.next();
                Student student = request.getStudent();
                String requestRecord = request.getRequestRecord();
                if (student != null && requestRecord != null) {
                    String requestType = requestRecord.substring(0, 3);
                    if (requestType.equals("I00")) {
                        generateHeader(request, requestRecord, grid);
                    }
                    if (requestType.equals("S00")) {
                        generateHeader(request, requestRecord, grid);
                    }
                    if (requestType.equals("T00")) {
                        generateHeader(request, requestRecord, grid);
                    }
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * This method generates a header record from the request record.
     *
     * @param request DataRequest
     * @param requestRecord String
     * @param grid DataGrid
     * @throws X2BaseException exception
     */
    private void generateHeader(DataRequest request, String requestRecord, DataGrid grid) throws X2BaseException {
        SisStudent student = (SisStudent) request.getStudent();
        StringBuilder responseRecord = new StringBuilder(1024);

        String oldRequestDemographic = requestRecord.substring(3, 256);

        List<FieldDefinition> fields = findFieldDefinitions("EXPDATA-FL-FASTER-" + requestRecord.substring(0, 3));
        if (fields == null) {
            fields = findFieldDefinitions("EXPDATA-FL-FASTER-00");
        }
        if (fields != null) {
            grid.append();

            for (FieldDefinition field : fields) {
                Object baseValue = null;
                if (!StringUtils.isEmpty(field.getCalcId())) {
                    if ("olddemo".equals(field.getCalcId())) {
                        baseValue = oldRequestDemographic;
                    }
                } else if (field.getBeanPath() != null) {
                    // Look up the value through the bean.
                    try {
                        baseValue = PropertyUtils.getProperty(student, field.getBeanPath());
                    } catch (Exception x2be) {
                        // Exception can happen if the path is invalid.
                    }
                }
                // Check default value.
                if (baseValue == null || ((baseValue instanceof String) && ((String) baseValue).length() == 0)) {
                    baseValue = field.getDefaultValue();
                } else {
                    // See if the value needs to be mapped to a different code.
                    if (field.getMappedLookup() > 0 && baseValue instanceof String) {
                        baseValue = lookupStateValue(SisStudent.class, field.getBeanPath(), (String) baseValue);
                    }

                    // See if the value needs a formatter for display.
                    if (field.getFormatter() != null) {
                        if (baseValue instanceof String && field.getConverter() != null) {
                            baseValue = field.getConverter().parseSystemString((String) baseValue);
                        }
                        baseValue = field.getFormatter().format(baseValue);
                    }
                }
                String fieldValue = "";
                if (baseValue != null) {
                    fieldValue = baseValue.toString();
                }

                fieldValue = ExportFormatManager.doPadding(fieldValue, field.getResizeMode().ordinal(),
                        field.getPaddingChar(), field.getExportLength());
                responseRecord.append(fieldValue);
            }

            grid.set(GRID_COL_NAME, responseRecord.toString());
        }
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List<String> names = new ArrayList<String>();
        names.add(GRID_COL_NAME);
        return names;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        List<String> names = new ArrayList<String>();
        names.add(GRID_COL_NAME);
        return names;
    }

    /**
     *
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize dictionary and alias names.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        loadFieldDefinitions();
        setIncludeHeaderRow(false);
        setUseValueDelimiters(false);
        setUseValueWrappers(false);
        setUseEscapes(false);
    }

    /**
     * This method generates a query of DataRequest records that
     * are ready to send,.
     *
     * @return QueryByCriteria
     */
    private QueryByCriteria getQuery() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(DataRequest.COL_TYPE, "FASTER");
        criteria.addEqualTo(DataRequest.COL_STATUS, Integer.valueOf(DataRequest.RequestStatusCode.IDENTIFIED.ordinal()));
        criteria.addNotNull(DataRequest.COL_STUDENT_OID);

        QueryByCriteria query = new QueryByCriteria(DataRequest.class, criteria);
        query.addOrderBy(DataRequest.COL_LAST_NAME, true);

        return query;
    }

    /**
     * Find field definitions.
     *
     * @param procedureId String
     * @return List
     * @throws X2BaseException exception
     */
    private List<FieldDefinition> findFieldDefinitions(String procedureId) throws X2BaseException {
        List<FieldDefinition> fields = m_fieldsMap.get(procedureId);
        if (fields == null) {
            List<ExportFormatField> efFields = m_definitionsMap.get(procedureId);
            if (efFields != null) {
                fields = new ArrayList<FieldDefinition>(efFields.size());
                for (ExportFormatField field : efFields) {
                    FieldDefinition newField = null;

                    newField = new FieldDefinition(field, SisStudent.class, getBroker());

                    fields.add(newField);
                }
                m_fieldsMap.put(procedureId, fields);
            }
        }
        return fields;
    }

    /**
     * This method loads the export format field definitions that are related to the FASTER exports.
     */
    private void loadFieldDefinitions() {
        Criteria criteria = new Criteria();
        criteria.addLike(ExportFormatField.REL_DEFINITION + ModelProperty.PATH_DELIMITER
                + ExportFormatDefinition.COL_PROCEDURE_ID, "EXPDATA-FL-FASTER-%");
        QueryByCriteria query = new QueryByCriteria(ExportFormatField.class, criteria);
        query.addOrderBy(ExportFormatField.REL_DEFINITION + ModelProperty.PATH_DELIMITER
                + ExportFormatDefinition.COL_PROCEDURE_ID, true);
        query.addOrderBy(ExportFormatField.COL_POSITION, true);

        m_definitionsMap = getBroker().getGroupedCollectionByQuery(query, ExportFormatField.REL_DEFINITION
                + ModelProperty.PATH_DELIMITER + ExportFormatDefinition.COL_PROCEDURE_ID, 20);
    }

    /**
     * Returns the state lookup code for field value.
     * Look up based on bean path.
     *
     * @param beanClass the class of the bean to find the reference table.
     * @param beanPath String
     * @param value String
     * @return String state code for input value.
     */
    private String lookupStateValue(Class beanClass, String beanPath, String value) {
        String stateValue = null;
        DataDictionary districtDictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop = new ModelProperty(beanClass, beanPath, getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField = districtDictionary.findDataDictionaryField(prop.getFieldId());
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            /*
             * The district dictionary is used here to look up the state equivalent
             * reference code. This is safe since all state equivalents are defined at the
             * district level. If a dictionary was needed for other purposes that required
             * extended or school-level information, this instance could not be used.
             */
            stateValue = districtDictionary.findStateReferenceCode(dictionaryField.getReferenceTableOid(), value);
        }
        return stateValue;
    }
}
