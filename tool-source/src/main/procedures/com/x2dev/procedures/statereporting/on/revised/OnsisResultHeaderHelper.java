/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.utils.X2RuntimeException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class OnsisResultsHelper.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnsisResultHeaderHelper {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private Filterable<UserDefinedTableC> m_resultsHelper;

    /**
     * The Class OnsisResultHeader.
     */
    public static class OnsisResultHeader {
        public static final String ALIAS_RECORDS_DDX_ID = "records-ddx-id";

        public static final String DDX_ID = "ON-SIS-RESULT-HEADER";

        private static final String ALIAS_CREATION_DATE = "creation-date";
        private static final String ALIAS_DESCRIPTION = "description";
        private static final String ALIAS_FILENAME = "filename";
        private static final String ALIAS_SCHOOLS_SELECTED = "schools-selected";
        private static final String ALIAS_SCHOOLS_OIDS = "schools-oids";

        private static final String ALIAS_USERNAME = "username";

        private final static SimpleDateFormat s_timestampDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        private UserDefinedTableC m_bean;
        private final X2Broker m_broker;
        private Map<String, DataDictionary> m_dictionariesById = new HashMap<>();

        private OnsisResultHeader(X2Broker broker, UserDefinedTableC bean) {
            m_broker = broker;
            m_bean = bean;
        }

        /**
         * Creates the result.
         *
         * @param organization
         *
         * @param broker X2Broker
         * @param description String
         * @param elementsHelper
         * @param resultClass
         * @return OnsisResult
         */
        public static OnsisResultHeader createResult(Organization organization,
                                                     X2Broker broker) {
            OnsisResultHeader newResult = null;
            newResult = new OnsisResultHeader(broker, new UserDefinedTableC(broker.getPersistenceKey()));
            newResult.m_bean.setFieldValueByAlias(ALIAS_CREATION_DATE, getCurrentDate(),
                    newResult.getDictionary(DDX_ID));
            newResult.m_bean
                    .setExtendedDataDictionaryOid(newResult.getDictionary(DDX_ID).getExtendedDictionaryOid());
            newResult.m_bean.setOrganization1Oid(organization.getOid());

            return newResult;
        }

        /**
         * Gets the bean.
         *
         * @return User defined table A
         */
        public X2BaseBean getBean() {
            return m_bean;
        }

        /**
         * Gets the broker.
         *
         * @return X 2 broker
         */
        public X2Broker getBroker() {
            return m_broker;
        }

        /**
         * Gets the current date.
         *
         * @return String
         */
        public static String getCurrentDate() {
            return s_timestampDateFormat.format(new Date());
        }

        /**
         * Gets the dictionary.
         *
         * @param ddxId String
         * @return Data dictionary
         */
        public DataDictionary getDictionary(String ddxId) {
            DataDictionary dictionary = m_dictionariesById.get(ddxId);
            if (dictionary == null) {
                if (ddxId == null) {
                    dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
                    m_dictionariesById.put(null, dictionary);
                } else {
                    ExtendedDictionaryAttributes extDictAttributes = null;

                    X2Criteria ddxCriteria = new X2Criteria();
                    ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);
                    QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
                    ExtendedDataDictionary ddx = m_broker.getBeanByQuery(ddxQuery);
                    if (ddx == null) {
                        X2Criteria asdCriteria = new X2Criteria();
                        asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, ddxId);
                        QueryByCriteria asdQuery = new QueryByCriteria(AssessmentDefinition.class, asdCriteria);
                        AssessmentDefinition asd = m_broker.getBeanByQuery(asdQuery);
                        if (asd == null) {
                            throw new X2RuntimeException();
                        }
                        extDictAttributes = asd;
                    } else {
                        extDictAttributes = ddx;
                    }
                    dictionary =
                            DataDictionary.getDistrictDictionary(extDictAttributes, m_broker.getPersistenceKey());
                    m_dictionariesById.put(ddxId, dictionary);
                }
            }
            return dictionary;
        }

        public Object getFieldValueByAlias(String alias) {
            return m_bean.getFieldValueByAlias(alias, getDictionary(DDX_ID));
        }

        public String getRecordsDdxId() {
            return (String) getFieldValueByAlias(ALIAS_RECORDS_DDX_ID);
        }

        public void setDescription(String description) {
            setFieldValueByAlias(ALIAS_DESCRIPTION, description);
        }

        public void setFilename(String filename) {
            setFieldValueByAlias(ALIAS_FILENAME, filename);
        }

        public void setRecordsDdxId(String ddxId) {
            setFieldValueByAlias(ALIAS_RECORDS_DDX_ID, ddxId);
        }

        public void setSchools(List<OnSchool> schools) {
            if (schools != null && !schools.isEmpty()) {
                List<String> selectedSchoolsBsids = new ArrayList<>();
                List<String> selectedSchoolOids = new ArrayList<>();
                for (OnSchool school : schools) {
                    String schoolString = school.getBsid();
                    if (StringUtils.isEmpty(schoolString)) {
                        schoolString = school.getName();
                    }
                    selectedSchoolsBsids.add(schoolString);
                    selectedSchoolOids.add(school.getOid());
                }
                setFieldValueByAlias(ALIAS_SCHOOLS_SELECTED, String.join(", ", selectedSchoolsBsids));
                setFieldValueByAlias(ALIAS_SCHOOLS_OIDS, String.join(", ", selectedSchoolOids));
            }
        }

        public void setUsername(String username) {
            setFieldValueByAlias(ALIAS_USERNAME, username);
        }

        private void setFieldValueByAlias(String alias, Object value) {
            m_bean.setFieldValueByAlias(alias, value, getDictionary(DDX_ID));
        }
    }

    public OnsisResultHeaderHelper(X2Broker broker) {
        X2Criteria resultsCriteria = new X2Criteria();
        resultsCriteria.addEqualTo(UserDefinedTableC.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER
                + ExtendedDataDictionary.COL_ID, OnsisResultHeader.DDX_ID);
        m_resultsHelper = FilterableFactory.create(broker, UserDefinedTableC.class, resultsCriteria);
    }

    /**
     * Save result.
     *
     * @param broker X2Broker
     * @param resultOid
     */
    public static void deleteResult(X2Broker broker, String resultOid) {
        X2Criteria recordsCriteria = new X2Criteria();
        recordsCriteria.addEqualTo(UserDefinedTableB.COL_USER_DEFINED_TABLE_C_OID, resultOid);
        broker.deleteBeanByOid(UserDefinedTableC.class, resultOid);
        broker.deleteByQuery(new QueryByCriteria(UserDefinedTableB.class, recordsCriteria));
    }

    public String findResultBeanOid(final X2Broker broker, final Filter<OnsisResultHeader> filter) {
        UserDefinedTableC resultBean = m_resultsHelper.extractFirst(new Filter<UserDefinedTableC>() {

            @Override
            public boolean isFiltered(UserDefinedTableC toFilter) {
                OnsisResultHeader result = new OnsisResultHeader(broker, toFilter);
                return filter.isFiltered(result);
            }

        });

        return resultBean == null ? null : resultBean.getOid();
    }

    /**
     * Save result.
     *
     * @param broker X2Broker
     * @param result OnsisResult
     */
    public static void saveResult(X2Broker broker, OnsisResultHeader result) {
        saveResultForced(broker, result);
    }

    /**
     * Save result.
     *
     * @param broker X2Broker
     * @param result OnsisResult
     */
    public static void saveResultForced(X2Broker broker, OnsisResultHeader result) {
        broker.saveBean(result.getBean());
    }
}
