/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetProcedureSchTerm.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class SetProcedureSchTerm extends ProcedureJavaSource {

    /**
     * The Enum TERM_CODE.
     */
    enum TERM_CODE {
        ANNUAL("FY", "3"), Q1("Q1", "6"), Q2("Q2", "7"), Q3("Q3", "8"), Q4("Q4", "9"), SEMESTER_1("S1", "1"), SEMESTER_2("S2",
                "2"), SUMMER("SS", "S");

        /**
         * Instantiates a new term code.
         *
         * @param oldCode String
         * @param flCode String
         */
        TERM_CODE(String oldCode, String flCode) {
            this.oldCode = oldCode;
            this.flCode = flCode;
        }

        String oldCode = null;
        String flCode = null;

        /**
         * Gets the fl fl code.
         *
         * @param codeToFind String
         * @return String
         */
        static public String getFlFlCode(String codeToFind) {
            for (TERM_CODE code : TERM_CODE.values()) {
                if (code.oldCode.equals(codeToFind)) {
                    return code.flCode;
                }
            }
            return null;
        }
    }

    private static final String REF_TABLE_OID_FL_SCH_TERMS = "rtbFlSchTerms";
    private static final String REF_TABLE_OID_SCH_TERMS = "rtbSchTermCode";

    Collection<DataFieldConfig> m_dataFieldConfigs = null;
    Collection<String> m_fieldsDBNames = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        ReferenceTable flTermCodesRefTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, REF_TABLE_OID_FL_SCH_TERMS);
        if (flTermCodesRefTable != null && flTermCodesRefTable.getReferenceCodes().size() > 0) {
            adjustFields();
            adjustValues();
            DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
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

        X2Criteria withTermsCriteria = new X2Criteria();
        withTermsCriteria.addEqualTo(DataFieldConfig.COL_REFERENCE_TABLE_OID, REF_TABLE_OID_SCH_TERMS);

        SubQuery withTermsSubQuery = new SubQuery(DataFieldConfig.class,
                DataFieldConfig.REL_DATA_FIELD + ModelProperty.PATH_DELIMITER + DataField.COL_DATABASE_NAME, withTermsCriteria);
        m_fieldsDBNames = getBroker().getSubQueryCollectionByQuery(withTermsSubQuery);

        QueryByCriteria withTermsQuery = new QueryByCriteria(DataFieldConfig.class, withTermsCriteria);
        m_dataFieldConfigs = getBroker().getCollectionByQuery(withTermsQuery);


    }

    /**
     * Adjust fields.
     */
    private void adjustFields() {
        for (DataFieldConfig config : m_dataFieldConfigs) {
            System.out.println("Change data field config for " + config.getDataFieldOid());
            config.setReferenceTableOid(REF_TABLE_OID_FL_SCH_TERMS);
            getBroker().saveBeanForced(config);
        }
    }

    /**
     * Adjust values.
     */
    private void adjustValues() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        for (String dbName : m_fieldsDBNames) {
            DataDictionaryField field = dictionary.findDataDictionaryFieldByDatabaseName(dbName);
            Class dataClass = field.getDataTable().getDataClass();
            String javaName = field.getJavaName();

            X2Criteria criteria = new X2Criteria();
            criteria.addNotEmpty(javaName, getBroker().getPersistenceKey());
            QueryByCriteria query = new QueryByCriteria(dataClass, criteria);
            Collection<X2BaseBean> beans = getBroker().getCollectionByQuery(query);
            for (X2BaseBean bean : beans) {
                System.out.println("Change " + bean.getFieldValueByBeanPath(javaName) + " to "
                        + TERM_CODE.getFlFlCode((String) bean.getFieldValueByBeanPath(javaName)));
                String newValue = TERM_CODE.getFlFlCode((String) bean.getFieldValueByBeanPath(javaName));
                if (!StringUtils.isEmpty(newValue)) {
                    bean.setFieldValueByBeanPath(javaName, TERM_CODE.getFlFlCode((String) bean.getFieldValueByBeanPath(javaName)));
                    getBroker().saveBeanForced(bean);
                }
            }
        }
    }
}
