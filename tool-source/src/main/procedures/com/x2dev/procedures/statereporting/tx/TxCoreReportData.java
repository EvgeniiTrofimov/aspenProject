/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tx;

import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * TX Core Report Data.
 *
 * @author X2 Development Corporation
 */
public class TxCoreReportData extends StateReportData {
    /**
     * Retriever is used to get EdFi Enumeration value for field.
     *
     * @author Follett Software Company
     */
    public class EdFiEnumerationRetriever implements FieldRetriever {
        public static final String CALC_TYPE_ED_FI_ENUMERATION = "Ed-Fi Enumeration";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return ((TxCoreReportData) data).getEdFiEnumeration(entity.getBean(), field.getBeanPath());
        }
    }

    /*
     * Aliases
     */
    protected static final String ALIAS_EDFI_CODE = "all-rcd-EdFiEnumeration";

    /*
     * Internal variables
     */
    private String m_fieldEdFiCode;

    /**
     * Gets the extended data dictionary.
     *
     * @param dictionaryId String
     * @return Data dictionary
     * @throws X2BaseException exception
     */
    public DataDictionary getExtendedDataDictionary(String dictionaryId) throws X2BaseException {
        DataDictionary result = null;
        if (StringUtils.isEmpty(dictionaryId)) {
            throw new X2BaseException(new Exception("dictionaryId parameter must not be null"));
        }

        Criteria ddxCriteria = new Criteria();
        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, dictionaryId);
        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        ExtendedDataDictionary ddxIep = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        if (ddxIep == null) {
            addSetupError("Dictionary with specified ID is not found", dictionaryId);
        } else {
            result = DataDictionary.getDistrictDictionary(ddxIep, getBroker().getPersistenceKey());
        }

        return result;
    }

    private XMLOutputter m_outputter;

    /**
     * Gets the XML outputter.
     *
     * @return XML outputter
     */
    public XMLOutputter getXMLOutputter() {
        if (m_outputter == null) {
            m_outputter = new XMLOutputter();
            Format format = Format.getPrettyFormat();
            m_outputter.setFormat(format);
        }
        return m_outputter;
    }

    /**
     * Returns codes of reference codes with enumerationValue from reference table of field with
     * alias.
     *
     * @param alias String
     * @param enumerationValue String
     * @return Set<String>
     */
    protected Set<String> codesFromEdFiEnumerationByAlias(String alias, String enumerationValue) {
        DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(alias);
        return codesFromEdFiEnumerationByField(field, enumerationValue);
    }

    /**
     * Returns codes of reference codes with enumerationValue from reference table of field by
     * beanClass and beanPath.
     *
     * @param beanClass Class
     * @param beanPath String
     * @param enumerationValue String
     * @return Set<String>
     */
    protected Set<String> codesFromEdFiEnumerationByBeanPath(Class beanClass,
                                                             String beanPath,
                                                             String enumerationValue) {
        DataDictionaryField field = getDataDictionary().findDataDictionaryField(beanClass.getName(), beanPath);
        return codesFromEdFiEnumerationByField(field, enumerationValue);
    }

    /**
     * Returns EdFi Enumeration value for bean by javaName.
     *
     * @param bean X2BaseBean
     * @param javaName String
     * @return Object
     * @throws X2BaseException exception
     */
    protected Object getEdFiEnumeration(X2BaseBean bean, String javaName) throws X2BaseException {
        String value = null;
        String code = (String) getProperty(bean, javaName);
        if (!StringUtils.isEmpty(m_fieldEdFiCode)) {
            if (!StringUtils.isEmpty(code)) {
                DataDictionaryField dictionaryField = getDataDictionaryField(bean.getClass(), javaName);
                if (dictionaryField != null) {
                    if (dictionaryField.hasReferenceTable()) {
                        Map<String, ReferenceCode> refCodes = getReferenceCodes(dictionaryField.getReferenceTableOid());
                        ReferenceCode rcd = refCodes.get(code);
                        if (rcd != null) {
                            value = (String) rcd.getFieldValueByBeanPath(m_fieldEdFiCode);
                            if (StringUtils.isEmpty(value)) {
                                value = rcd.getFederalCode();
                            }
                            if (StringUtils.isEmpty(value)) {
                                value = rcd.getStateCode();
                            }
                        }
                    }
                }
            }
        }
        return value;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        setEntityClass(StateReportEntity.class);

        Map<String, FieldRetriever> core = new HashMap<String, FieldRetriever>();
        core.put(EdFiEnumerationRetriever.CALC_TYPE_ED_FI_ENUMERATION, new EdFiEnumerationRetriever());
        addCalcs(core);
    }

    /**
     * Returns codes of reference codes with enumerationValue from reference table of field.
     *
     * @param field DataDictionaryField
     * @param enumerationValue String
     * @return Set<String>
     */
    private Set<String> codesFromEdFiEnumerationByField(DataDictionaryField field, String enumerationValue) {
        Set<String> codes = new HashSet<String>();
        if (field != null) {
            ReferenceTable refTable = field.getReferenceTable();
            if (refTable != null) {
                Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

                for (ReferenceCode refCode : refCodes) {
                    String code = (String) refCode.getFieldValueByBeanPath(m_fieldEdFiCode);
                    if (!StringUtils.isEmpty(code) && code.equals(enumerationValue)) {
                        codes.add(code);
                    }
                }
            }
        }

        return codes;
    }

    /**
     * Initialize fields that are used in the class.
     */
    private void initializeFields() {
        m_fieldEdFiCode = translateAliasToJavaName(ALIAS_EDFI_CODE, true);
    }
}
