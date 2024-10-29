/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2013 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.pa;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Location Fact Export data module.
 */
public class PADistrictFact extends StateReportData {
    private static final String CATEGORY_1 = "SECURITY";
    private static final String STATE_CODE_VALUE = "Security";

    /**
     * The Class PADistrictFactEntity.
     */
    public static class PADistrictFactEntity extends StateReportEntity {
        PADistrictFact m_exportData;

        /**
         * Instantiates a new PA district fact entity.
         */
        public PADistrictFactEntity() {}

        /**
         * Gets the row.
         *
         * @return Counter result
         */
        public CounterResult getRow() {
            return m_exportData.m_counterResultList.get(getCurrentRow());
        }

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
            m_exportData = (PADistrictFact) data;
            super.intitialize(data, bean);
            setRowCount(m_exportData.m_counterResultList.size());
        }
    }

    /**
     * CounterAggResult is a list of CounterResult objects
     * each CounterResult represent counter of group of values.
     */

    /**
     * CounterResult used to count groups of values similar to SQL count(*) with GROUP BY
     * It's overridden equals() method does not include 'count' field to compare
     */
    class CounterResult {
        private final String category1;
        private final String category2;
        Integer count;
        private final String districtCode;

        /**
         * Instantiates a new counter result.
         *
         * @param districtCode String
         * @param category1 String
         * @param category2 String
         * @param count int
         */
        CounterResult(String districtCode, String category1, String category2, int count) {
            this.districtCode = districtCode;
            this.category1 = category1;
            this.category2 = category2;
            this.count = Integer.valueOf(count);
        }

        /**
         * Gets the count.
         *
         * @return the count
         */
        public Integer getCount() {
            return count;
        }

        /**
         * Sets the count.
         *
         * @param count the count to set
         */
        public void setCount(Integer count) {
            this.count = count;
        }

        /**
         * Gets the category 1.
         *
         * @return the category1
         */
        public String getCategory1() {
            return category1;
        }

        /**
         * Gets the category 2.
         *
         * @return the category2
         */
        public String getCategory2() {
            return category2;
        }

        /**
         * Gets the district code.
         *
         * @return the districtCode
         */
        public String getDistrictCode() {
            return districtCode;
        }

    }

    /**
     * Field retriever for all fields not derived from organization.
     */
    class DefaultFieldRetriever implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity exportEntity, FieldDefinition field)
                throws X2BaseException {
            PADistrictFactEntity entity = (PADistrictFactEntity) exportEntity;

            CounterResult result = entity.getRow();
            Object value = "##empty";

            String calcParam = (String) field.getParameter();

            if (CALC_PARAM_CATEGORY_1.equals(calcParam)) {
                value = result.getCategory1();
            } else if (CALC_PARAM_CATEGORY_2.equals(calcParam)) {
                value = result.getCategory2();
            } else if (CALC_PARAM_COUNT.equals(calcParam)) {
                value = result.getCount();
            }

            return value;
        }
    }

    /**
     * The Class RetrieveMeasureType.
     */
    class RetrieveMeasureType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return "COUNT";
        }
    }

    static final String CALC_ID_CND_CALC_DEFAULT = "LOC_CALC_DEFAULT";
    static final String CALC_ID_MEASURE_TYPE = "MEASURE TYPE";
    static final String CALC_PARAM_CATEGORY_1 = "CATEGORY 1";
    static final String CALC_PARAM_CATEGORY_2 = "CATEGORY 2";
    static final String CALC_PARAM_COUNT = "COUNT";

    List<CounterResult> m_counterResultList = new ArrayList<CounterResult>();

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        initializeFields();

        if (getSetupErrors().size() > 0) {
            return;
        }
        X2Criteria districtCriteria = new X2Criteria();
        QueryByCriteria query = new QueryByCriteria(Organization.class, districtCriteria);
        setQuery(query);
        setEntityClass(PADistrictFactEntity.class);
        initCounterResult(query);
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_CND_CALC_DEFAULT, new DefaultFieldRetriever());
        calcs.put(CALC_ID_MEASURE_TYPE, new RetrieveMeasureType());
        addCalcs(calcs);
    }

    /**
     * Inits the counter result.
     *
     * @param query QueryByCriteria
     */
    private void initCounterResult(QueryByCriteria query) {
        Collection<String> staffTypeCodesWithStateValueSecurity = new ArrayList<String>(0);
        Criteria codesCriteria = ReferenceManager.getCodesCriteria(
                getDataDictionaryField(Staff.class, Staff.COL_STAFF_TYPE).getReferenceTableOid(),
                null, true, false, false, getBroker().getPersistenceKey());
        codesCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, STATE_CODE_VALUE);
        SubQuery subQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, codesCriteria);
        staffTypeCodesWithStateValueSecurity = getBroker().getSubQueryCollectionByQuery(subQuery);

        Collection<Organization> collection = getBroker().getCollectionByQuery(query);
        Iterator<Organization> it = collection.iterator();
        while (it.hasNext()) {
            Organization organization = it.next();
            int count = 0;
            if (!staffTypeCodesWithStateValueSecurity.isEmpty()) {
                X2Criteria staffCriteria = new X2Criteria();
                staffCriteria.addIn(Staff.COL_STAFF_TYPE, staffTypeCodesWithStateValueSecurity);
                staffCriteria.addEqualTo(Staff.COL_ORGANIZATION1_OID, organization.getOid());
                QueryByCriteria staffQuery = new QueryByCriteria(Staff.class, staffCriteria);
                count = getBroker().getCount(staffQuery);
            }
            CounterResult counterResult = new CounterResult(organization.getId(), CATEGORY_1, null, count);
            m_counterResultList.add(counterResult);
        }
    }
}
