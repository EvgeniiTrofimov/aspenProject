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

import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataTable;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisDataFieldConfig;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class DataDictionaryAliasExporter.
 */
public class DataDictionaryAliasExporter extends StateReportData {

    private static final String PARAM_ALIAS_LIST = "aliasList";

    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class DataDictionaryEntity extends StateReportEntity {

        private ArrayList<HashMap<String, String>> m_columnData = new ArrayList<HashMap<String, String>>();

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            SisDataFieldConfig dataField = (SisDataFieldConfig) bean;

            HashMap<String, String> fieldData = new HashMap<String, String>();
            fieldData.put("A", "F");
            fieldData.put("B", dataField.getAlias());
            fieldData.put("C", dataField.getDataField().getDataTable().getDatabaseName());
            String databaseName = dataField.getDataField().getDatabaseName();
            fieldData.put("D", isUDF(databaseName) ? "UDF" : databaseName);
            fieldData.put("E", dataField.getUserLongName());
            fieldData.put("F", dataField.getUserShortName());
            // A, B, C - character, D is text
            String userType = "Character";
            if (dataField.getUserLength() > 50) {
                userType = "Text";
            }
            fieldData.put("G", StringUtils.isEmpty(dataField.getUserType()) ? userType : dataField.getUserType());
            fieldData.put("H", "" + dataField.getUserLength());
            fieldData.put("I", "" + dataField.getUserDecimal());

            String referenceTableOid = dataField.getReferenceTableOid();
            if (!StringUtils.isEmpty(referenceTableOid)) {
                ReferenceTable refTable = dataField.getReferenceTable();
                if (refTable != null) {
                    fieldData.put("J", refTable.getUserName());
                    if ("0".equals(fieldData.get("H"))) {
                        fieldData.put("H", "" + refTable.getCodeLength());
                    }
                    m_columnData.add(fieldData);

                    Map<String, ReferenceCode> codeMap = refTable.getCodeMap();
                    for (String key : codeMap.keySet()) {
                        ReferenceCode code = codeMap.get(key);
                        HashMap<String, String> codeData = new HashMap<String, String>();
                        codeData.put("A", "C");
                        codeData.put("B", code.getCode());
                        codeData.put("C", code.getDescription());
                        codeData.put("D", code.getStateCode());
                        codeData.put("E", "");
                        codeData.put("F", "");
                        codeData.put("G", "");
                        codeData.put("H", "");
                        codeData.put("I", "");
                        codeData.put("J", "");
                        codeData.put("K", "");
                        m_columnData.add(codeData);
                    }

                    setRowCount(m_columnData.size());
                } else {
                    fieldData.put("J", "");
                    m_columnData.add(fieldData);
                }
                fieldData.put("K", dataField.getFieldDescription());
            } else {
                fieldData.put("J", "");
                m_columnData.add(fieldData);
            }
        }

        /**
         * Checks if is udf.
         *
         * @param fieldName String
         * @return true, if is udf
         */
        private boolean isUDF(String fieldName) {
            if (fieldName.contains("FIELDA") ||
                    fieldName.contains("FIELDB") ||
                    fieldName.contains("FIELDC") ||
                    fieldName.contains("FIELDD")) {
                return true;
            }
            return false;
        }

        /**
         * Gets the row data.
         *
         * @return Hash map
         */
        public HashMap<String, String> getRowData() {
            return m_columnData.get(getCurrentRow());
        }
    }

    /**
     * The Class RetrieveValues.
     */
    protected static class RetrieveValues implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            DataDictionaryEntity ddeEntity = (DataDictionaryEntity) entity;
            return ddeEntity.getRowData().get(field.getParameter());
        }

    }

    /**
     * Initialize the data module for DD Alias Export.
     */
    @Override
    public void initialize() {
        if (getSetupErrors().size() == 0) {
            String aliasList = (String) getParameter(PARAM_ALIAS_LIST);

            Criteria aliasCriteria = new Criteria();
            if (StringUtils.isEmpty(aliasList)) {
                aliasCriteria.addNotNull(SisDataFieldConfig.COL_ALIAS);
                aliasCriteria.addNotEqualTo(SisDataFieldConfig.COL_ALIAS, "");
            } else {
                String[] aliases = aliasList.split(",");
                for (String alias : aliases) {
                    Criteria aliasListCriteria = new Criteria();
                    aliasListCriteria.addEqualTo(SisDataFieldConfig.COL_ALIAS, alias.trim());
                    aliasCriteria.addOrCriteria(aliasListCriteria);
                }
            }
            // aliasCriteria.addEqualTo(SisDataFieldConfig.COL_ENABLED_INDICATOR, Boolean.TRUE);

            QueryByCriteria aliasQuery = new QueryByCriteria(SisDataFieldConfig.class, aliasCriteria);
            aliasQuery.addOrderByAscending(SisDataFieldConfig.REL_DATA_FIELD + "." + DataField.REL_DATA_TABLE + "."
                    + DataTable.COL_DATABASE_NAME);
            aliasQuery.addOrderByAscending(SisDataFieldConfig.COL_ALIAS);

            // Set the query to be used for alias selection.
            setQuery(aliasQuery);
            setEntityClass(DataDictionaryEntity.class);

            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("DD-VALUES", new RetrieveValues());
            super.addCalcs(calcs);
        }
    }
}
