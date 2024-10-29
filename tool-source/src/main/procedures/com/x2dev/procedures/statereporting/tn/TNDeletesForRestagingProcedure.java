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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import org.apache.commons.lang.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TNDeletesForRestagingProcedure.
 */
public class TNDeletesForRestagingProcedure extends ProcedureJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * Class members
     */
    private Map<String, LinkedList<ExportFormatResult>> m_efrMap;
    private ArrayList<String> m_efrNameByParams;
    private ArrayList<String> m_resultsOids;

    /**
     * Other constants
     */
    private static final String REGEX_EFR_NAME = "TN %%% %%% Uploads for %%%%";

    /**
     * Fields used in export operations.
     */
    public enum EXPORT_RESULTS {
        ROW_010("EXP-TN-CTX", "TN 010"), ROW_011("EXP-TN-CAL", "TN 011"), ROW_015("EXP-TN-BUS", "TN 015"),
        //
        ROW_020("EXP-TN-CAS", "TN 020"), ROW_021("EXP-TN-CASP", "TN 021"), ROW_022("EXP-TN-CSD", "TN 022"),
        //
        ROW_030("EXP-TN-MST", "TN 030"), ROW_031("EXP-TN-MSTS", "TN 031"), ROW_040("EXP-TN-STD", "TN 040"),
        //
        ROW_041("EXP-TN-ENR", "TN 041"), ROW_042("EXP-TN-PGMI", "TN 042"), ROW_043("EXP-TN-STDG", "TN 043"),
        //
        ROW_044("EXP-TN-PGMC", "TN 044"), ROW_045("EXP-TN-STR", "TN 045"), ROW_046("EXP-TN-CND", "TN 046"),
        //
        ROW_047("EXP-TN-STDD", "TN 047"), ROW_048("EXP-TN-SSC", "TN 048"), ROW_049("EXP-TN-ATT", "TN 049"),
        //
        ROW_050("EXP-TN-ENRW", "TN 050"), ROW_051("EXP-TN-STDE", "TN 051"), ROW_052("EXP-TN-PGMM", "TN 052"),
        //
        ROW_080("EXP-TN-TRN", "TN 080"), ROW_081("EXP-TN-TAI", "TN 081"), ROW_082("EXP-TN-TAS", "TN 082"),
        //
        ROW_060("EXP-TN-STF", "TN 060"), ROW_062("EXP-TN-SFP", "TN 062"), ROW_063("EXP-TN-MTC", "TN 063");

        private String m_param;
        private String m_efrName;

        /**
         * Gets the param.
         *
         * @return String
         */
        public String getParam() {
            return m_param;
        }

        /**
         * Gets the efr name.
         *
         * @return String
         */
        public String getEfrName() {
            return m_efrName;
        }

        /**
         * Instantiates a new export results.
         *
         * @param fieldId String
         * @param efrName String
         */
        private EXPORT_RESULTS(String fieldId, String efrName) {
            m_param = fieldId;
            m_efrName = efrName;
        }

    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (m_resultsOids == null) {
            populateFinalResultOids();
        }

        for (String resultOid : m_resultsOids) {
            ArrayList<ExportFormatRow> rows = getEFRowsResultOid(resultOid);
            ExportFormatResult result =
                    (ExportFormatResult) getBroker().getBeanByOid(ExportFormatResult.class, resultOid);
            ExportFormatResult newResult = null;
            ExportFormatDefinition definition = null;
            ArrayList<ExportFormatField> fields = new ArrayList<ExportFormatField>();
            ArrayList<ExportFormatRow> deletesRows = new ArrayList<ExportFormatRow>();
            if (result != null && (definition = result.getDefinition()) != null) {
                fields.addAll(definition.getFields(getBroker()));
            }

            Map<String, ArrayList<String>> fieldsMap = getMapOfKeyIndexes(fields);

            if (!fieldsMap.isEmpty()) {
                String recordTypeBeanPath = (String) fieldsMap.keySet().toArray()[0];
                ArrayList<String> keyValues = new ArrayList<String>();
                for (ExportFormatRow row : rows) {
                    String recordType = (String) row.getFieldValueByBeanPath(recordTypeBeanPath);
                    ArrayList<String> keyValuesPaths = fieldsMap.get(recordTypeBeanPath);

                    if ("N".equals(recordType)) {
                        String keyValue = "";
                        for (String beanPath : keyValuesPaths) {
                            keyValue = keyValue.concat((String) row.getFieldValueByBeanPath(beanPath));
                        }

                        if (!keyValues.contains(keyValue)) {
                            ExportFormatRow rowToAdd = row.clone();
                            rowToAdd.setFieldValueByBeanPath(recordTypeBeanPath, "D");
                            getBroker().saveBeanForced(rowToAdd);
                            deletesRows.add(rowToAdd);
                            keyValues.add(keyValue);
                        }
                    }
                }

                if (!deletesRows.isEmpty()) {
                    newResult = result.clone();
                    newResult.setComment("Restaging");
                    getBroker().saveBeanForced(newResult);

                    for (ExportFormatRow row : deletesRows) {
                        row.setResultOid(newResult.getOid());
                        getBroker().saveBeanForced(row);
                        logMessage("Row with type \"D\" with ID = " + row.getOid()
                                + " was cloned of row with type \"N\".");
                    }
                }
            }
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        populateFinalResultOids();
    }

    /**
     * Get all ExportFromatRow of the given ExportFromatResult oid.
     *
     * @param resultOid String
     * @return list of ExportFormatRow
     */
    private ArrayList<ExportFormatRow> getEFRowsResultOid(String resultOid) {
        ArrayList<ExportFormatRow> rowsByEFR = new ArrayList<ExportFormatRow>();
        if (!StringUtils.isEmpty(resultOid)) {
            X2Criteria efwCriteria = new X2Criteria();
            efwCriteria.addEqualTo(ExportFormatRow.COL_RESULT_OID, resultOid);
            QueryByCriteria efwQuery = new QueryByCriteria(ExportFormatRow.class, efwCriteria);
            rowsByEFR.addAll(getBroker().getCollectionByQuery(efwQuery));
        }

        return rowsByEFR;
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

                if ("RECORD TYPE".equals(field.getName())) {
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

    /**
     * Get rows which are needed to be checked for the creating deletes from input map.
     */
    private void populateEfrNamesByParams() {
        m_efrNameByParams = new ArrayList<String>();

        for (EXPORT_RESULTS result : EXPORT_RESULTS.values()) {
            if ((Boolean) getParameter(result.getParam()) == Boolean.TRUE) {
                m_efrNameByParams.add(result.getEfrName());
            }
        }
    }

    /**
     * Populate ExportFormatResult map keyed on name.
     */
    private void populateEFResultsMap() {
        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addLikeIgnoreCase(ExportFormatResult.COL_NAME, REGEX_EFR_NAME);

        QueryByCriteria efrQuery = new QueryByCriteria(ExportFormatResult.class, efrCriteria);
        m_efrMap = getBroker().getGroupedCollectionByQuery(efrQuery, ExportFormatResult.COL_NAME, 1024);
    }

    /**
     * Populate list of ExportFromatReslt oids which match input selection.
     */
    private void populateFinalResultOids() {
        m_resultsOids = new ArrayList<String>();
        if (m_efrMap == null) {
            populateEFResultsMap();
        }

        if (m_efrNameByParams == null) {
            populateEfrNamesByParams();
        }

        for (String efrName : m_efrNameByParams) {
            for (Map.Entry<String, LinkedList<ExportFormatResult>> entry : m_efrMap.entrySet()) {
                if (entry.getKey().contains(efrName)) {
                    for (ExportFormatResult result : entry.getValue()) {
                        m_resultsOids.add(result.getOid());
                    }
                }
            }
        }
    }
}
