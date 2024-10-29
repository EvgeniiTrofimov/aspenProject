/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.CRDCDataHelper.CRDCFinalData;
import com.x2dev.procedures.statereporting.CRDCDataHelper.CRDCFinalEntity;
import com.x2dev.procedures.statereporting.CRDCDataHelper.Dataset;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data Module for CRDC LEA Part 1 Export.
 *
 * @author X2 Development Corporation
 */
public class CRDCLeaData extends CRDCFinalData {

    /**
     * The Class LeaCRDCEntity.
     */
    public static class LeaCRDCEntity extends CRDCFinalEntity {

        /**
         * Instantiates a new lea CRDC entity.
         */
        public LeaCRDCEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        List<String> m_leaIds = new ArrayList();

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = getLeaId();

            return name;
        }

        /**
         * Gets the lea filter.
         *
         * @return String
         */
        public String getLeaFilter() {
            return "LeaNCESCode=" + getLeaId() + ",";
        }

        /**
         * Initialize the entity for the student bean provided.
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

            Set<String> leaIds = new HashSet();
            CRDCLeaData crdcData = (CRDCLeaData) data;
            CRDCDataHelper.CalcParameter calcParam = CRDCDataHelper.CalcParameter.LEA_NCES_CODE;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            String beanPath =
                    crdcData.m_crdcDataHelper.getBeanPath(Dataset.SCHOOL.getProcedureId(), calcParam.getFieldName());
            List<ExportFormatRow> rows =
                    crdcData.m_crdcDataHelper.getFilteredRows(Dataset.SCHOOL, "", filtersByFieldName);
            for (ExportFormatRow row : rows) {
                String value = calcParam.getCalculatedValue(crdcData.m_crdcDataHelper, beanPath, row);
                if (!StringUtils.isEmpty(value)) {
                    leaIds.add(value);
                }
            }
            m_leaIds.addAll(leaIds);
            this.setRowCount(m_leaIds.size());
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * return the current lea id.
         *
         * @return String
         */
        String getLeaId() {
            return m_leaIds.get(getCurrentRow());
        }
    }

    /**
     * The Class RetrieverLeaId.
     */
    public class RetrieverLeaId implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return ((LeaCRDCEntity) entity).getLeaId();
        }

    }

    /**
     * The Class RetrieverSklCount.
     */
    public class RetrieverSklCount implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Integer value = Integer.valueOf(0);

            CRDCLeaData crdcData = (CRDCLeaData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            String leaFilter = ((LeaCRDCEntity) entity).getLeaFilter();
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.SCHOOL,
                    leaFilter + (String) field.getParameter(), filtersByFieldName);

            if (!rows.isEmpty()) {
                value = Integer.valueOf(rows.size());
            }

            return value;
        }

    }

    /**
     * The Class RetrieverStdCount.
     */
    public class RetrieverStdCount implements FieldRetriever {
        static final String DEFAULT_FILTER = "ActivePart1=Y,";

        private String m_defaultFilter = "";

        /**
         * Instantiates a new retriever std count.
         */
        public RetrieverStdCount() {
            super();
        }

        /**
         * Instantiates a new retriever std count.
         *
         * @param defaultFilter String
         */
        public RetrieverStdCount(String defaultFilter) {
            super();
            m_defaultFilter = defaultFilter;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Integer value = Integer.valueOf(0);

            CRDCLeaData crdcData = (CRDCLeaData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            String leaFilter = ((LeaCRDCEntity) entity).getLeaFilter();
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STUDENT,
                    leaFilter + m_defaultFilter + (String) field.getParameter(), filtersByFieldName);

            if (!rows.isEmpty()) {
                value = Integer.valueOf(rows.size());
            }

            return value;
        }

    }

    /**
     * The Class RetrieverStdExists.
     */
    public class RetrieverStdExists implements FieldRetriever {

        static final String DEFAULT_FILTER = "ActivePart1=Y,";

        private String m_defaultFilter = "";

        /**
         * Instantiates a new retriever std exists.
         */
        public RetrieverStdExists() {}

        /**
         * Instantiates a new retriever std exists.
         *
         * @param defaultFilter String
         */
        public RetrieverStdExists(String defaultFilter) {
            m_defaultFilter = defaultFilter;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            CRDCLeaData crdcData = (CRDCLeaData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            String leaFilter = ((LeaCRDCEntity) entity).getLeaFilter();
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STUDENT,
                    leaFilter + m_defaultFilter + (String) field.getParameter(), filtersByFieldName);

            if (!rows.isEmpty()) {
                value = true;
            }

            return Boolean.valueOf(value);
        }

    }

    private static final String INPUT_PARAM_REPORT_DATE = "reportDate";

    private static final String INPUT_PARAM_RESULT_OID_STD = "stdResultOid";
    private static final String INPUT_PARAM_RESULT_OID_SKL = "sklResultOid";

    protected CRDCDataHelper m_crdcDataHelper = null;

    /**
     * @throws X2BaseException
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        m_crdcDataHelper = new CRDCDataHelper(this, getResultsCriteria());
        m_crdcDataHelper.setReportDate((PlainDate) getParameter(INPUT_PARAM_REPORT_DATE));

        X2Criteria leaCriteria = new X2Criteria();
        leaCriteria.addEqualTo(X2BaseBean.COL_OID, getOrganization().getOid());

        QueryByCriteria leaQuery = new QueryByCriteria(Organization.class, leaCriteria);

        setQuery(leaQuery);
        setEntityClass(LeaCRDCEntity.class);

        Map<String, FieldRetriever> calcMap = new HashMap<String, FieldRetriever>();
        calcMap.put("LEA_ID", new RetrieverLeaId());
        calcMap.put("LEA_P1_SKL_COUNT", new RetrieverSklCount());
        calcMap.put("LEA_P1_STD_COUNT", new RetrieverStdCount(RetrieverStdCount.DEFAULT_FILTER));
        calcMap.put("LEA_P1_STD_EXISTS", new RetrieverStdExists(RetrieverStdExists.DEFAULT_FILTER));
        calcMap.put("LEA_P2_STD_COUNT", new RetrieverStdCount());
        calcMap.put("LEA_P2_STD_EXISTS", new RetrieverStdExists());

        addCalcs(calcMap);
    }

    /**
     * Gets the results criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getResultsCriteria() {
        Collection<String> resultsOids = new ArrayList<String>();

        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_STD));
        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_SKL));

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(X2BaseBean.COL_OID, resultsOids);

        return criteria;
    }
}
