/*
 * ==================================================================== X2 Development Corporation
 * Copyright (c)
 * 2002-2015 X2 Development Corporation. All rights reserved. Redistribution and use in source and
 * binary forms, with or
 * without modification, is not permitted without express written agreement from X2 Development
 * Corporation.
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tx;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.OrganizationAttributes;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This is a class for TxLocalEducationAgencyExtension Complex Type export (Education Organization
 * Interchange).
 *
 * @author X2 Development Corporation
 */
public class TxLocalEducationAgencyExtension extends TxCoreReportData {

    /**
     * The Class TxLocalEducationAgencyExtensionEntity.
     *
     * @author X2 Development Corporation
     */
    public static class TxLocalEducationAgencyExtensionEntity extends StateReportEntity {
        List<OrganizationAttributes> m_attributes;

        /**
         * Instantiates a new tx local education agency extension entity.
         */
        /*
         * Public no argument constructor for dynamic instantiation.
         */
        public TxLocalEducationAgencyExtensionEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the organization attributes.
         *
         * @return Organization attributes
         */
        public OrganizationAttributes getOrganizationAttributes() {
            return m_attributes.get(this.getCurrentRow());
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            TxLocalEducationAgencyExtension txData = (TxLocalEducationAgencyExtension) data;
            m_attributes = txData.m_mapAttributes.get(bean.getOid());
            setRowCount(m_attributes.size());
        }
    }

    /**
     * The Class ExtendedDictionaryAttributeRetriever.
     */
    public class ExtendedDictionaryAttributeRetriever implements FieldRetriever {
        public static final String CALC_ID = "ORA-TX-SR-ATTR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String alias = (String) field.getParameter();
            TxLocalEducationAgencyExtension txData = (TxLocalEducationAgencyExtension) data;
            TxLocalEducationAgencyExtensionEntity txEntity = (TxLocalEducationAgencyExtensionEntity) entity;
            OrganizationAttributes ora = txEntity.getOrganizationAttributes();
            return ora.getFieldValueByAliasExtended(alias, txData.getAttributesDictionary());
        }
    }

    /**
     * The Class ExtendedDictionaryRatingsRetriever.
     */
    public class ExtendedDictionaryRatingsRetriever implements FieldRetriever {
        public static final String CALC_ID = "TX_CALC_RATINGS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String alias = (String) field.getParameter();
            TxLocalEducationAgencyExtension txData = (TxLocalEducationAgencyExtension) data;
            return entity.getBean().getFieldValueByAliasExtended(alias, txData.getRaingsDictionary());
        }
    }

    private static final String ALIAS_FIELD_CATEGORY = "all-org-Category";
    private static final String DICT_ORA_RATINGS = "ORA-TX-SR-RATINGS";
    private static final String DICT_ORA_ATTRIBUTES = "ORA-TX-SR-ATTR";
    private static final String LEA_CATEGORY = "0123456789";

    protected Map<String, List<OrganizationAttributes>> m_mapAttributes;

    private String m_fieldCategory;
    private DataDictionary m_oraRaingsDictionary;
    private DataDictionary m_oraAttributesDictionary;

    /**
     * Gets the attributes dictionary.
     *
     * @return Data dictionary
     */
    protected DataDictionary getAttributesDictionary() {
        return m_oraAttributesDictionary;
    }

    /**
     * Gets the raings dictionary.
     *
     * @return Data dictionary
     */
    protected DataDictionary getRaingsDictionary() {
        return m_oraRaingsDictionary;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        initFieldRetrievers();

        // If no errors so far, continue with query.
        if (getSetupErrors().size() == 0) {
            m_mapAttributes = getOrganizationAttributes(DICT_ORA_ATTRIBUTES);
            X2Criteria orgCriteria = new X2Criteria();
            orgCriteria.addEqualTo(m_fieldCategory, LEA_CATEGORY);
            QueryByCriteria orgQuery = new QueryByCriteria(Organization.class, orgCriteria);

            setQuery(orgQuery);
            setEntityClass(TxLocalEducationAgencyExtensionEntity.class);
        }
    }

    /**
     * Gets the organization attributes.
     *
     * @param extendedDict String
     * @return Map
     */
    private Map<String, List<OrganizationAttributes>> getOrganizationAttributes(String extendedDict) {
        X2Criteria crit = new X2Criteria();
        crit.addEqualTo(OrganizationAttributes.REL_EXTENDED_DATA_DICTIONARY + "." + ExtendedDataDictionary.COL_ID,
                extendedDict);
        QueryByCriteria query = new QueryByCriteria(OrganizationAttributes.class, crit);

        return getBroker().getGroupedCollectionByQuery(query, OrganizationAttributes.COL_ORGANIZATION_OID, 32);
    }

    /**
     * Inits the field retrievers.
     */
    private void initFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(ExtendedDictionaryRatingsRetriever.CALC_ID, new ExtendedDictionaryRatingsRetriever());
        calcs.put(ExtendedDictionaryAttributeRetriever.CALC_ID, new ExtendedDictionaryAttributeRetriever());
        super.addCalcs(calcs);
    }

    /**
     * Initialize fields.
     *
     * @throws X2BaseException exception
     */
    private void initializeFields() throws X2BaseException {
        m_fieldCategory = translateAliasToJavaName(ALIAS_FIELD_CATEGORY, true);
        setRaingsDictionary(getExtendedDataDictionary(DICT_ORA_RATINGS));
        setAttributesDictionary(getExtendedDataDictionary(DICT_ORA_ATTRIBUTES));
    }

    /**
     * Sets the attributes dictionary.
     *
     * @param oraAttributesDictionary void
     */
    private void setAttributesDictionary(DataDictionary oraAttributesDictionary) {
        this.m_oraAttributesDictionary = oraAttributesDictionary;
    }

    /**
     * Sets the raings dictionary.
     *
     * @param oraRaingsDictionary void
     */
    private void setRaingsDictionary(DataDictionary oraRaingsDictionary) {
        this.m_oraRaingsDictionary = oraRaingsDictionary;
    }
}
