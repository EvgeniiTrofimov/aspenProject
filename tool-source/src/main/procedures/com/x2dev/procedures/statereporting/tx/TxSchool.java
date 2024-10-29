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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.SchoolAttribute;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
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
public class TxSchool extends TxCoreReportData {

    /**
     * The Class TxSchoolEntity.
     *
     * @author X2 Development Corporation
     */
    public static class TxSchoolEntity extends StateReportEntity {

        /**
         * Instantiates a new tx school entity.
         */
        /*
         * Public no argument constructor for dynamic instantiation.
         */
        public TxSchoolEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
        }
    }

    /**
     * The Class AttributeRetriever.
     */
    public class AttributeRetriever implements FieldRetriever {
        public static final String CALC_ID = "SKA-TX-ATTR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = "";
            String alias = (String) field.getParameter();
            TxSchool txData = (TxSchool) data;
            String sklOid = entity.getBean().getOid();
            SchoolAttribute ska = txData.getSklAttribute(sklOid);

            if (ska != null) {
                value = (String) ska.getFieldValueByAlias(alias);
            }

            return value;
        }
    }

    /**
     * The Class RatingsRetriever.
     */
    public class RatingsRetriever implements FieldRetriever {
        public static final String CALC_ID = "TX_CALC_RATINGS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return "";
        }
    }

    private static final String ALIAS_FIELD_CATEGORY = "all-org-Category";
    private static final String SKL_CATEGORY = "School";
    private static final String CONTEXT_OID = "schoolYear";
    private static final String CODE_ATTRIBUTE = "ATTR";
    private static final String CODE_RATINGS = "RATINGS";

    protected Map<String, List<SchoolAttribute>> m_mapAttributes;

    private String m_fieldCategory;
    private Map<String, Collection<SchoolAttribute>> m_sklRatings;
    private Map<String, Collection<SchoolAttribute>> m_sklAttributes;
    private String m_contexOid;

    /**
     * Gets the skl attribute.
     *
     * @param sklOid String
     * @return School attribute
     */
    protected SchoolAttribute getSklAttribute(String sklOid) {
        Collection<SchoolAttribute> skas = m_sklAttributes.get(sklOid);
        SchoolAttribute ska = null;

        if (skas != null && skas.size() > 0) {
            ska = (SchoolAttribute) skas.toArray()[0];
        }

        return ska;
    }

    /**
     * Gets the ratings.
     *
     * @return Map
     */
    protected Map<String, Collection<SchoolAttribute>> getRatings() {
        return m_sklRatings;
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

        m_contexOid = (String) getParameter(CONTEXT_OID);

        initializeFields();

        initFieldRetrievers();

        // If no errors so far, continue with query.
        if (getSetupErrors().size() == 0) {
            X2Criteria sklCriteria = new X2Criteria();
            sklCriteria.addEqualTo(SisSchool.REL_ORGANIZATION1 + "." + m_fieldCategory, SKL_CATEGORY);

            SubQuery sklSubQuery = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, sklCriteria);

            preloadAttributesMap(sklSubQuery);
            preloadRatingsMap(sklSubQuery);

            QueryByCriteria sklQuery = new QueryByCriteria(SisSchool.class, sklCriteria);
            getBroker().getCollectionByQuery(sklQuery);
            setQuery(sklQuery);
            setEntityClass(TxSchoolEntity.class);
        }
    }

    /**
     * Preload ratings map.
     *
     * @param sklSubQuery SubQuery
     */
    private void preloadRatingsMap(SubQuery sklSubQuery) {
        X2Criteria skaCriteria = new X2Criteria();
        skaCriteria.addEqualTo(SchoolAttribute.COL_CONTEXT_OID, m_contexOid);
        skaCriteria.addEqualTo(SchoolAttribute.COL_FIELD_D001, CODE_RATINGS);

        skaCriteria.addIn(SchoolAttribute.COL_SCHOOL_OID, sklSubQuery);

        QueryByCriteria skaQuery = new QueryByCriteria(SchoolAttribute.class, skaCriteria);

        m_sklRatings = getBroker().getGroupedCollectionByQuery(skaQuery, SchoolAttribute.COL_SCHOOL_OID, 1024);

    }

    /**
     * Preload attributes map.
     *
     * @param sklSubQuery SubQuery
     */
    private void preloadAttributesMap(SubQuery sklSubQuery) {
        X2Criteria skaCriteria = new X2Criteria();

        skaCriteria.addIn(SchoolAttribute.COL_SCHOOL_OID, sklSubQuery);
        skaCriteria.addEqualTo(SchoolAttribute.COL_CONTEXT_OID, m_contexOid);
        skaCriteria.addEqualTo(SchoolAttribute.COL_FIELD_D001, CODE_ATTRIBUTE);

        QueryByCriteria skaQuery = new QueryByCriteria(SchoolAttribute.class, skaCriteria);

        m_sklAttributes = getBroker().getGroupedCollectionByQuery(skaQuery, SchoolAttribute.COL_SCHOOL_OID, 1024);

    }

    /**
     * Inits the field retrievers.
     */
    private void initFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RatingsRetriever.CALC_ID, new RatingsRetriever());
        calcs.put(AttributeRetriever.CALC_ID, new AttributeRetriever());
        super.addCalcs(calcs);
    }

    /**
     * Initialize fields.
     *
     * @throws X2BaseException exception
     */
    private void initializeFields() throws X2BaseException {
        m_fieldCategory = translateAliasToJavaName(ALIAS_FIELD_CATEGORY, true);
    }
}
