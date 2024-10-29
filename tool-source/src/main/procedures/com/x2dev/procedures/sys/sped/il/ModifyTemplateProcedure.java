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
package com.x2dev.procedures.sys.sped.il;

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryRelationship;
import com.follett.fsc.core.k12.business.dictionary.ExtendedFieldAttributes;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.web.workflow.SisOutcomeDetail;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class ModifyTemplateProcedure.
 */
public class ModifyTemplateProcedure {

    private static final String EMPTY = "";
    private ModelProperty m_fieldProperty;
    private Strategy m_strategy;
    private Object m_strategyValue;
    private X2Broker m_broker = null;
    private UserDataContainer m_userData;
    private Map<String, Object> m_filter = new HashMap<String, Object>();
    private List<String> m_codes = new ArrayList<String>();
    private boolean m_initialize = false;

    /**
     * The Enum Strategy.
     */
    public enum Strategy {
        STRATEGY_COUNT("count"), STRATEGY_FILTER_RTB("filterRtb"), STRATEGY_RTB("rtb");

        private String m_strategy = null;

        /**
         * Instantiates a new strategy.
         *
         * @param value String
         */
        Strategy(String value) {
            m_strategy = value;
        }

        /**
         * Gets the strategy.
         *
         * @return String
         */
        public String getStrategy() {
            return m_strategy;
        }

        /**
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            // TODO Auto-generated method stub
            return m_strategy;
        }
    }

    /**
     * Instantiates a new modify template procedure.
     *
     * @param userData UserDataContainer
     * @param strategy Strategy
     * @param strategyValue Object
     * @param filter Map<String,Object>
     * @param field String
     * @param dictionary DataDictionary
     */
    public ModifyTemplateProcedure(UserDataContainer userData, Strategy strategy, Object strategyValue,
            Map<String, Object> filter, String field, DataDictionary dictionary) {
        m_strategy = strategy;
        m_filter = filter;
        m_strategyValue = strategyValue;
        m_userData = userData;
        m_broker = getBroker();
        m_fieldProperty = getPath(field, null, dictionary);

    }


    /**
     * Find embedded list by name.
     *
     * @param detail GenericDetail
     * @param embeddedListName String
     * @return EmbeddedListDetailSet
     */
    public static EmbeddedListDetailSet findEmbeddedListByName(GenericDetail detail, String embeddedListName) {
        GenericDetail formdetail = null;
        EmbeddedListDetailSet embeddedDetail = null;
        if (detail instanceof SisOutcomeDetail) {
            formdetail = ((SisOutcomeDetail) detail).getCurrentFormDetail();
        } else {
            formdetail = detail;
        }

        for (String childDetailSetId : formdetail.getChildDetailSetIds()) {
            ChildDetailSet childDetailSet = detail.getChildDetailSet(childDetailSetId);

            if (childDetailSetId.equals(embeddedListName)) {
                if (childDetailSet instanceof EmbeddedListDetailSet) {
                    embeddedDetail = (EmbeddedListDetailSet) childDetailSet;
                    break;
                }
            }
        }


        return embeddedDetail;
    }


    /**
     * Creates the new embadded child.
     *
     * @param embeddedDetail EmbeddedListDetailSet
     * @throws X2BaseException exception
     */
    public void createNewEmbaddedChild(EmbeddedListDetailSet embeddedDetail) throws X2BaseException {
        if (!m_initialize) {
            initialize();
        }
        List<String> codesForCreating = getCodesForCreatingChild(embeddedDetail);

        Class beanClass = m_fieldProperty.getRootClass();
        for (String code : codesForCreating) {
            Map<String, Object> properties = new HashMap<String, Object>();
            properties.put(m_fieldProperty.getBeanPath(), code);
            properties.putAll(m_filter);
            X2BaseBean baseBean = getNewFillingBean(beanClass, properties);
            embeddedDetail.addChild(baseBean, m_userData, (ModelBroker) getBroker());
        }
    }


    /**
     * Check properties.
     *
     * @param properties Map<String,Object>
     * @param filter Map<String,Object>
     * @return true, if successful
     */
    private boolean checkProperties(Map<String, Object> properties, Map<String, Object> filter) {
        boolean returnValue = filter.isEmpty();
        if (!returnValue) {
            for (Entry<String, Object> enty : filter.entrySet()) {
                Object propValue = properties.get(enty.getKey());
                Object filterValue = filter.get(enty.getKey());
                if (propValue != null && filterValue != null && propValue.equals(filterValue)) {
                    returnValue = true;
                } else {
                    returnValue = false;
                    break;
                }
            }
        }
        return returnValue;

    }


    /**
     * Delete matches.
     *
     * @param from List<String>
     * @param whatDelete List<String>
     */
    private void deleteMatches(List<String> from, List<String> whatDelete) {
        Iterator<String> iterator = from.iterator();
        while (iterator.hasNext()) {
            String code = iterator.next();
            if (whatDelete.contains(code)) {
                iterator.remove();
            }
        }
    }


    /**
     * Gets the broker.
     *
     * @return X 2 broker
     */
    private X2Broker getBroker() {
        if (m_broker == null) {
            m_broker = new ModelBroker(m_userData.getPrivilegeSet());
        }
        return m_broker;
    }


    /**
     * Gets the codes for creating child.
     *
     * @param embeddedDetail EmbeddedListDetailSet
     * @return List
     */
    private List<String> getCodesForCreatingChild(EmbeddedListDetailSet embeddedDetail) {
        List<String> returnList = new ArrayList<String>();
        returnList.addAll(m_codes);
        List<String> existigCodes = getExisting(embeddedDetail, m_fieldProperty, m_filter);

        if (m_strategy.equals(Strategy.STRATEGY_COUNT)) {
            int countForAdd = Integer.parseInt(m_strategyValue.toString());
            int existingCount = existigCodes.size();
            for (int i = 0; i < countForAdd - existingCount; i++) {
                returnList.add(EMPTY);
            }
        } else {
            deleteMatches(returnList, existigCodes);
        }
        return returnList;
    }


    /**
     * Gets the codes by rtb oid.
     *
     * @param refTableOid String
     * @return List
     */
    private List<String> getCodesByRtbOid(String refTableOid) {
        Criteria refTableCriteria = new Criteria();
        refTableCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, refTableOid);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, refTableCriteria);
        Collection<ReferenceCode> refCodes = getBroker().getCollectionByQuery(query);

        List<String> codes = new ArrayList<String>();
        for (ReferenceCode referenceCode : refCodes) {
            String code = referenceCode.getCode();
            codes.add(code);
        }
        return codes;
    }


    /**
     * Gets the existing.
     *
     * @param targetEmbadded EmbeddedListDetailSet
     * @param modelProperty ModelProperty
     * @param filter Map<String,Object>
     * @return List
     */
    private List<String> getExisting(EmbeddedListDetailSet targetEmbadded,
                                     ModelProperty modelProperty,
                                     Map<String, Object> filter) {
        List<String> existCodes = new ArrayList<String>();
        for (GenericDetail genericDetail : targetEmbadded.getChildDetails()) {
            Map<String, Object> properties = genericDetail.getPropertyValues();
            if (checkProperties(properties, filter)) {
                String value = (String) properties.get(modelProperty.getDictionaryPath());
                existCodes.add(value == null ? EMPTY : value);
            }
        }

        return existCodes;
    }


    /**
     * Gets the matches.
     *
     * @param first List<String>
     * @param second List<String>
     * @return List
     */
    private List<String> getMatches(List<String> first, List<String> second) {
        List<String> matches = new ArrayList<String>();

        Iterator<String> iterator = first.iterator();
        while (iterator.hasNext()) {
            String code = iterator.next();
            if (second.contains(code)) {
                matches.add(code);
            }
        }
        return matches;
    }


    /**
     * Gets the new filling bean.
     *
     * @param beanClass Class
     * @param properties Map<String,Object>
     * @return X 2 base bean
     */
    private X2BaseBean getNewFillingBean(Class beanClass, Map<String, Object> properties) {
        X2BaseBean baseBean = X2BaseBean.newInstance(beanClass, getBroker().getPersistenceKey());
        setValuesToBean(baseBean, properties);
        return baseBean;
    }


    /**
     * Gets the path.
     *
     * @param path String
     * @param bean X2BaseBean
     * @param dataDictionary DataDictionary
     * @return Model property
     */
    private ModelProperty getPath(String path, X2BaseBean bean, DataDictionary dataDictionary) {
        // WebUtils.getProperty(bean, property)


        ModelProperty returnModel = null;
        DataDictionaryField field = dataDictionary.findDataDictionaryFieldByAlias(path);
        DataDictionaryRelationship ddrelation = null;
        if (field == null) {
            field = dataDictionary.findDataDictionaryField(path);
            ddrelation = dataDictionary.findDataDictionaryRelationship(path);

        }
        if (field != null) {
            returnModel = new ModelProperty(field, dataDictionary);

        }
        if (ddrelation != null) {
            String dPath = ddrelation.getSystemDataRelationship().getPrimaryDataIndex().getFieldList();
            returnModel = new ModelProperty(dPath, getBroker().getPersistenceKey());
        }

        if (returnModel == null && bean != null) {
            returnModel = new ModelProperty(bean.getClass(), path, dataDictionary);
        }

        return returnModel;
    }


    /**
     * Gets the ref table oid.
     *
     * @param strategyValue Object
     * @param property ModelProperty
     * @return String
     */
    private String getRefTableOid(Object strategyValue, ModelProperty property) {
        String rtbOid = null;
        if (strategyValue != null && strategyValue instanceof String) {
            rtbOid = (String) strategyValue;
        }

        if (rtbOid == null || rtbOid.isEmpty()) {
            ExtendedFieldAttributes extField = property.getField().getExtendedDataField();
            if (extField != null) {
                rtbOid = extField.getReferenceTableOid();
            } else {
                rtbOid = property.getField().getReferenceTableOid();
            }
        }
        return rtbOid;
    }

    /**
     * Initialize.
     */
    private void initialize() {
        if (m_strategy.equals(Strategy.STRATEGY_RTB)) {
            String rtbOid = getRefTableOid(m_strategyValue, m_fieldProperty);
            m_codes = getCodesByRtbOid(rtbOid);

        } else if (m_strategy.equals(Strategy.STRATEGY_FILTER_RTB)) {
            String rtbOid = getRefTableOid(m_strategyValue, m_fieldProperty);
            m_codes = getCodesByRtbOid(rtbOid);
            m_codes = getMatches(m_codes, (List<String>) m_strategyValue);
        }

        m_initialize = true;
    }


    /**
     * Sets the values to bean.
     *
     * @param bean X2BaseBean
     * @param properties Map<String,Object>
     */
    private void setValuesToBean(X2BaseBean bean, Map<String, Object> properties) {
        for (Entry<String, Object> entry : properties.entrySet()) {
            bean.setFieldValueByBeanPath(entry.getKey(), entry.getValue());
        }

    }
}
