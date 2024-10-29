/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.common;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanColumn;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.TranscriptDefinition;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class DictionaryExtractor.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class DictionaryExtractor implements Serializable {
    /**
     * The Class NoDdxException.
     */
    public static class NoDdxException extends RuntimeException {

        /**
         * Instantiates a new no export exception.
         *
         * @param message String
         */
        public NoDdxException(String message) {
            super(message);
        }
    }

    private X2Broker m_broker;
    private Map<String, List<ReferenceCode>> m_codesWithStateValues = new HashMap<>();
    private Map<String, Map<String, List<String>>> m_codesValuesByStateValue = new HashMap<>();
    private HashMap<DataDictionaryField, Converter> m_converterMap;
    private Map<String, DataDictionary> m_dictionariesById = new HashMap<>();
    private Map<String, Map<String, DataDictionaryField>> m_fieldsByAlias = new HashMap<>();

    /**
     * A map of maps of reference code.
     * The outer map is indexed by the reference table OID. It contains maps of reference codes.
     * The inner map is indexed by the reference code. It contains the RefrenceCode bean for the
     * code.
     */
    private Map<String, Map<String, ReferenceCode>> m_refTableCodesMap = null;
    private Map<String, ReferenceTable> m_refTableMap = null;

    /**
     * Instantiates a new dictionary extractor.
     *
     * @param broker X2Broker
     */
    public DictionaryExtractor(X2Broker broker) {
        m_broker = broker;
    }

    /**
     * Gets the alias as java type.
     *
     * @param bean ToolBean
     * @param column ToolBeanColumn
     * @return Object
     */
    public Object getAliasAsJavaType(ToolBean bean, ToolBeanColumn column) {
        Object value = null;
        DataDictionaryField field = column.getField(this);
        if (field != null) {
            value = bean.getValue(column);

            if (value instanceof String) {
                // Get a SystemStringConverter for the field and convert the value from a string
                // value to java type.
                SystemStringConverter converter = getStringConverter(field);
                if (converter != null) {
                    value = converter.parseSystemString((String) value);
                }
            }
        }
        return value;
    }

    /**
     * Gets the alias as java type.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return Object
     * @throws X2BaseException exception
     */
    public Object getAliasAsJavaType(X2BaseBean bean, String alias) throws X2BaseException {
        return getAliasAsJavaType(bean, alias, null);
    }

    /**
     * Gets the alias as java type.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param ddxId String
     * @return Object
     * @throws X2BaseException exception
     */
    public Object getAliasAsJavaType(X2BaseBean bean, String alias, String ddxId) throws X2BaseException {
        Object value = null;
        boolean required = false;
        DataDictionaryField field = getFieldByAlias(alias, ddxId, required);
        if (field != null) {
            value = WebUtils.getProperty(bean, field.getJavaName());

            if (value instanceof String) {
                // Get a SystemStringConverter for the field and convert the value from a string
                // value to java type.
                SystemStringConverter converter = getStringConverter(field);
                if (converter != null) {
                    value = converter.parseSystemString((String) value);
                }
            }

        }

        return value;
    }

    /**
     * Gets the alias as string.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param ddxId String
     * @return String
     * @throws X2BaseException exception
     */
    public String getAliasAsString(X2BaseBean bean, String alias, String ddxId) throws X2BaseException {
        String stringValue = null;
        boolean required = false;
        DataDictionaryField field = getFieldByAlias(alias, ddxId, required);
        if (field != null) {
            Object value = WebUtils.getProperty(bean, field.getJavaName());

            Converter converter = getConverter(field);
            if (converter != null) {
                stringValue = converter.javaToString(value);
            } else if (value != null) {
                stringValue = value.toString();
            }
        }

        return stringValue;
    }

    /**
     * Gets the alias as string value.
     *
     * @param alias String
     * @param value Object
     * @param ddxId String
     * @return String
     * @throws X2BaseException exception
     */
    public String getAliasAsStringValue(String alias, Object value, String ddxId) throws X2BaseException {
        String stringValue = null;
        boolean required = false;
        DataDictionaryField field = getFieldByAlias(alias, ddxId, required);
        if (field != null) {
            Converter converter = getConverter(field);
            if (converter != null) {
                stringValue = converter.javaToString(value);
            } else if (value != null) {
                stringValue = value.toString();
            }
        }

        return stringValue;
    }

    /**
     * Gets the alias as system value.
     *
     * @param alias String
     * @param value Object
     * @return Object
     * @throws X2BaseException exception
     */
    public Object getAliasAsSystemValue(String alias, Object value) throws X2BaseException {
        return getAliasAsSystemValue(alias, value, null);
    }

    /**
     * Gets the alias as system value.
     *
     * @param alias String
     * @param value Object
     * @param ddxId String
     * @return Object
     * @throws X2BaseException exception
     */
    public Object getAliasAsSystemValue(String alias, Object value, String ddxId) throws X2BaseException {
        boolean required = false;
        DataDictionaryField field = getFieldByAlias(alias, ddxId, required);
        if (field != null) {
            if (field.isString()) {
                SystemStringConverter converter = getStringConverter(field);
                if (converter != null) {
                    value = converter.getSystemString(value);
                }
            }
        }

        return value;
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
     * Gets the dictionary.
     *
     * @return Data dictionary
     */
    public DataDictionary getDictionary() {
        return getDictionary(null);
    }

    /**
     * Gets the dictionary.
     * The ddxId can be any of these properties {asdID, ddxID, asdOID, ddxOID, efdOID, gtdOID}
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
                if (ddxId.toUpperCase().startsWith("ASD")) {
                    extDictAttributes = m_broker.getBeanByOid(AssessmentDefinition.class, ddxId);
                } else if (ddxId.toUpperCase().startsWith("DDX")) {
                    extDictAttributes = m_broker.getBeanByOid(ExtendedDataDictionary.class, ddxId);
                } else if (ddxId.toUpperCase().startsWith("EFD")) {
                    extDictAttributes = m_broker.getBeanByOid(ExportFormatDefinition.class, ddxId);
                } else if (ddxId.toUpperCase().startsWith("GTD")) {
                    extDictAttributes = m_broker.getBeanByOid(TranscriptDefinition.class, ddxId);
                } else {
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
                            throw new NoDdxException("Extended Dictionary Attributes not found for id [" + ddxId + "]");
                        }
                        extDictAttributes = asd;
                    } else {
                        extDictAttributes = ddx;
                    }
                }
                if (extDictAttributes == null) {
                    throw new RuntimeException("Invalid extended data dictionary id - " + ddxId);
                }
                dictionary =
                        DataDictionary.getDistrictDictionary(extDictAttributes, m_broker.getPersistenceKey());
                m_dictionariesById.put(ddxId, dictionary);
            }
        }
        return dictionary;
    }

    /**
     * @param dictionaryField
     * @return
     */
    public String getDictionaryId(DataDictionaryField dictionaryField) {
        String dictionaryId = null;

        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            ReferenceTable refTable = getReferenceTable(dictionaryField.getReferenceTableOid());
            if (refTable.getExtendedDataDictionary() != null) {
                dictionaryId = refTable.getExtendedDataDictionary().getId();
            }
        }
        return dictionaryId;
    }

    /**
     * Lookup and return a DataDictionaryField based on a root bean and bean path.
     * This allows multi-hop paths in the bean path.
     *
     *
     * @param beanClass Class
     * @param path String
     * @return DataDictionaryField
     */
    public DataDictionaryField getDataDictionaryField(Class beanClass, String path) {
        beanClass = getX2BeanClass(beanClass);
        ModelProperty prop = new ModelProperty(beanClass, path, m_broker.getPersistenceKey());
        DataDictionaryField dictionaryField = getDictionary().findDataDictionaryField(prop.getFieldId());

        return dictionaryField;
    }

    /**
     * Lookup and return a DataDictionaryField based on a root bean and bean path.
     * This allows multi-hop paths in the bean path.
     *
     *
     * @param bean X2BaseBean
     * @param path String
     * @return DataDictionaryField
     */
    public DataDictionaryField getDataDictionaryField(X2BaseBean bean, String path) {
        ModelProperty prop = new ModelProperty(bean.getClass(), path, m_broker.getPersistenceKey());
        DataDictionaryField dictionaryField = getDictionary().findDataDictionaryField(prop.getFieldId());

        return dictionaryField;
    }

    /**
     * Gets the field by alias.
     *
     * @param alias String
     * @return Data dictionary field
     */
    public DataDictionaryField getFieldByAlias(String alias) {
        boolean required = true;
        return getFieldByAlias(alias, null, required);
    }

    /**
     * Gets the field by alias.
     *
     * @param alias String
     * @param required boolean
     * @return Data dictionary field
     */
    public DataDictionaryField getFieldByAlias(String alias, boolean required) {
        return getFieldByAlias(alias, null, required);
    }

    /**
     * Gets the field by alias.
     *
     * @param alias String
     * @param ddxId String
     * @param required boolean
     * @return Data dictionary field
     */
    public DataDictionaryField getFieldByAlias(String alias, String ddxId, boolean required) {
        Map<String, DataDictionaryField> dictionaryFields = m_fieldsByAlias.get(ddxId);
        if (dictionaryFields == null) {
            dictionaryFields = new HashMap<>();
            m_fieldsByAlias.put(ddxId, dictionaryFields);
        }
        DataDictionaryField field = dictionaryFields.get(alias);
        if (field == null) {
            DataDictionary dictionary = null;
            try {
                dictionary = getDictionary(ddxId);
            } catch (Exception e) {
                if (required) {
                    throw e;
                }
                return null;
            }
            field = dictionary.findDataDictionaryFieldByAlias(alias);
            if (field == null && required) {
                throw new X2RuntimeException(
                        new IllegalStateException("The alias [" + alias + "] could not be found."));
            }
            if (ddxId == null) {
                dictionaryFields.put(alias, field);
            }
        }
        return field;
    }

    /**
     * Returns the value for the specified property on the entity bean. This method will log
     * missing values.
     * Some bean paths as specified in the FieldDefinition may not be valid. These will be marked by
     * '$'.
     * In those cases, the bean path itself is a description and not a field.
     *
     * If the path is a typed custom field, use a system string converter to convert it to a usable
     * java type.
     *
     * @param bean X2BaseBean
     * @param beanPath String
     * @return Object
     * @throws X2BaseException exception
     */
    public Object getPropertyAsJavaType(X2BaseBean bean, String beanPath) throws X2BaseException {
        Object value = null;
        value = WebUtils.getProperty(bean, beanPath);

        if (value instanceof String) {
            // Get a SystemStringConverter for the field and convert the value from a string
            // value to java type.
            DataDictionaryField field = getDataDictionaryField(bean, beanPath);

            if (value instanceof String) {
                // Get a SystemStringConverter for the field and convert the value from a string
                // value to java type.
                SystemStringConverter converter = getStringConverter(field);
                if (converter != null) {
                    value = converter.parseSystemString((String) value);
                }
            }
        }

        return value;
    }

    public Object getPropertyAsJavaType(ToolBean bean, String beanPath) throws X2BaseException {
        Object value = null;
        value = WebUtils.getProperty(bean, beanPath);

        if (value instanceof String) {
            // Get a SystemStringConverter for the field and convert the value from a string
            // value to java type.
            DataDictionaryField field = getDataDictionaryField(bean.getClass(), beanPath);

            if (value instanceof String) {
                // Get a SystemStringConverter for the field and convert the value from a string
                // value to java type.
                SystemStringConverter converter = getStringConverter(field);
                if (converter != null) {
                    value = converter.parseSystemString((String) value);
                }
            }
        }

        return value;
    }

    /**
     * Gets the ref code by alias.
     *
     * @param bean X2BaseBean
     * @param column the column
     * @return Reference code
     */
    public ReferenceCode getRefCodeByAlias(ToolBean bean, ToolBeanColumn column) {
        ReferenceCode code = null;
        DataDictionaryField field = column.getField(this);
        if (field != null && field.hasReferenceTable()) {
            String value = bean.getValueString(column);
            if (!StringUtils.isEmpty(value)) {
                code = getReferenceCodes(field.getReferenceTableOid()).get(value);
            }
        }
        return code;
    }

    /**
     * Gets the ref code by alias.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param ddxId String
     * @return Reference code
     */
    public ReferenceCode getRefCodeByAlias(X2BaseBean bean, String alias, String ddxId) {
        ReferenceCode code = null;
        DataDictionaryField field = getFieldByAlias(alias, ddxId, false);
        if (field != null && field.hasReferenceTable()) {
            String value = (String) bean.getFieldValueByBeanPath(field.getJavaName());
            if (!StringUtils.isEmpty(value)) {
                code = getReferenceCodes(field.getReferenceTableOid()).get(value);
            }
        }
        return code;
    }

    /**
     * Gets the ref code values by state value.
     *
     * @param field the field
     * @return the ref code values by state value
     */
    public Map<String, List<String>> getRefCodeValuesByStateValue(DataDictionaryField field) {
        String key = field.getDatabaseName();
        Map<String, List<String>> codesWithStateValue = m_codesValuesByStateValue.get(key);
        if (codesWithStateValue == null) {
            codesWithStateValue = new HashMap();
            m_codesValuesByStateValue.put(key, codesWithStateValue);
            String refTableOid = field.getReferenceTableOid();
            if (!StringUtils.isEmpty(refTableOid)) {
                Map<String, ReferenceCode> refCodes = getReferenceCodes(refTableOid);
                for (ReferenceCode rcd : refCodes.values()) {
                    List<String> codes = codesWithStateValue.get(rcd.getStateCode());
                    if (codes == null) {
                        codes = new ArrayList();
                        codesWithStateValue.put(rcd.getStateCode(), codes);
                    }
                    codes.add(rcd.getCode());
                }
            }
        }
        return codesWithStateValue;
    }

    /**
     * Gets the ref codes with state value.
     *
     * @param field DataDictionaryField
     * @return List
     */
    public List<ReferenceCode> getRefCodesWithStateValue(DataDictionaryField field) {
        return getRefCodesWithStateValue(field, null);
    }

    /**
     * Gets the ref codes with state value.
     *
     * @param field DataDictionaryField
     * @param includedCodes Collection<String>
     * @return List
     */
    public List<ReferenceCode> getRefCodesWithStateValue(DataDictionaryField field,
                                                         Collection<String> includedCodes) {
        String key = field.getDatabaseName() + (includedCodes == null ? "" : includedCodes.toString());
        List<ReferenceCode> codesWithStateValue = m_codesWithStateValues.get(key);
        if (codesWithStateValue == null) {
            codesWithStateValue = new ArrayList<ReferenceCode>();
            m_codesWithStateValues.put(key, codesWithStateValue);
            String refTableOid = field.getReferenceTableOid();
            if (!StringUtils.isEmpty(refTableOid)) {
                Map<String, ReferenceCode> refCodes = getReferenceCodes(refTableOid);
                for (Entry<String, ReferenceCode> refCodeEntry : refCodes.entrySet()) {
                    if (!StringUtils.isEmpty(refCodeEntry.getValue().getStateCode()) &&
                            (includedCodes == null
                                    || includedCodes.contains(refCodeEntry.getValue().getStateCode()))) {
                        codesWithStateValue.add(refCodeEntry.getValue());
                    }
                }
            }
        }
        return codesWithStateValue;
    }

    /**
     * Lookup a map of reference codes for a reference table oid.
     * Cache the results for later use.
     *
     * @param referenceTableOid String
     * @return Map<String, ReferenceCode>
     */
    public Map<String, ReferenceCode> getReferenceCodes(String referenceTableOid) {
        Map<String, ReferenceCode> codeMap = null;
        if (m_refTableCodesMap == null) {
            m_refTableCodesMap = new HashMap<String, Map<String, ReferenceCode>>();
        }

        if (m_refTableCodesMap.containsKey(referenceTableOid)) {
            codeMap = m_refTableCodesMap.get(referenceTableOid);
        } else {
            Map<String, ReferenceCode> finalMap = new HashMap<String, ReferenceCode>();
            ReferenceTable refTable = getReferenceTable(referenceTableOid);
            if (refTable != null) {
                Map<String, ReferenceCode> rcdMap = refTable.getCodeMap(getBroker());
                finalMap.putAll(rcdMap);
                for (ReferenceCode rcd : rcdMap.values()) {
                    String key = LocalizationUtils.generateKey(rcd.getOid(),
                            SisBeanPaths.REF_CODE.code().getColumnOid());
                    LocalizationCache.getLocalizedResources(getBroker().getPersistenceKey(), rcd.getOid()).stream()
                            .filter(resource -> key.equals(resource.getKey()))
                            .map(resource -> resource.getValue())
                            .filter(alternateKey -> !StringUtils.isEmpty(alternateKey))
                            .forEach(alternateKey -> finalMap.putIfAbsent(alternateKey, rcd));
                }
                codeMap = finalMap;
            }
            m_refTableCodesMap.put(referenceTableOid, codeMap);
        }

        return codeMap;
    }

    /**
     * Gets the reference table.
     *
     * @param referenceTableOid the reference table oid
     * @return the reference table
     */
    public ReferenceTable getReferenceTable(String referenceTableOid) {
        ReferenceTable table = null;
        if (m_refTableMap == null) {
            m_refTableMap = new HashMap();
        }

        if (m_refTableMap.containsKey(referenceTableOid)) {
            table = m_refTableMap.get(referenceTableOid);
        } else {
            table = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, referenceTableOid);
            m_refTableMap.put(referenceTableOid, table);
        }
        return table;
    }

    /**
     * Gets the SIF value.
     *
     * @param bean ToolBean
     * @param column ToolBeanColumn
     * @return String
     */
    public String getSifValue(ToolBean bean, ToolBeanColumn column) {
        DataDictionaryField dictionaryField = column.getField(this);
        String value = bean.getValueString(column);
        return dictionaryField.hasReferenceTable()
                ? lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value,
                        ExportFormatField.ReferenceMapTypeCode.SIF.ordinal())
                : null;
    }

    /**
     * Gets the SIF value by alias.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return String
     */
    public String getSifValueByAlias(X2BaseBean bean, String alias) {
        String value = (String) bean.getFieldValueByAlias(alias);
        return lookupSifValue(bean.getClass(), getFieldByAlias(alias).getJavaName(), value);
    }

    /**
     * Gets the state value.
     *
     * @param bean ToolBean
     * @param column ToolBeanColumn
     * @return String
     */
    public String getStateValue(ToolBean bean, ToolBeanColumn column) {
        DataDictionaryField dictionaryField = column.getField(this);
        String value = bean.getValueString(column);
        return dictionaryField.hasReferenceTable()
                ? lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal())
                : null;
    }

    /**
     * Gets the state values.
     *
     * @param bean ToolBean
     * @param column ToolBeanColumn
     * @return List
     */
    public List<String> getStateValues(ToolBean bean, ToolBeanColumn column) {
        DataDictionaryField dictionaryField = column.getField(this);
        String csv = bean.getValueString(column);
        if (csv == null) {
            return null;
        }
        return dictionaryField.hasReferenceTable()
                ? StringUtils.convertDelimitedStringToList(csv, ',', true).stream()
                        .map(str -> str.trim())
                        .map(str -> lookupStateValue(column, str))
                        .filter(str -> !StringUtils.isBlank(str))
                        .distinct()
                        .collect(Collectors.toList())
                : null;
    }

    /**
     * Gets the state value by alias.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return String
     */
    public String getStateValueByAlias(X2BaseBean bean, String alias) {
        String value = (String) bean.getFieldValueByAlias(alias);
        return lookupStateValue(bean.getClass(), getFieldByAlias(alias).getJavaName(), value);
    }


    /**
     * Gets the state value by alias.
     *
     * @param beans Collection<X2BaseBean>
     * @param aliases Collection<String>
     * @return String
     */
    public String getStateValueByAlias(Collection<X2BaseBean> beans, Collection<String> aliases) {
        Iterator<X2BaseBean> beanIterator = beans.iterator();
        Iterator<String> aliasIterator = aliases.iterator();
        String value = null;
        while (beanIterator.hasNext() && aliasIterator.hasNext()) {
            X2BaseBean bean = beanIterator.next();
            String alias = aliasIterator.next();
            if (bean != null) {
                String beanValue = (String) bean.getFieldValueByAlias(alias);
                if (beanValue != null) {
                    value = lookupStateValue(bean.getClass(), getFieldByAlias(alias).getJavaName(), beanValue);
                }
            }
            if (!StringUtils.isEmpty(value)) {
                break;
            }
        }
        return value;
    }

    /**
     * Gets the state values by alias.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return List
     */
    public List<String> getStateValuesByAlias(X2BaseBean bean, String alias) {
        String csv = (String) bean.getFieldValueByAlias(alias);
        if (csv == null) {
            return null;
        }

        List<String> values = StringUtils.convertDelimitedStringToList(csv, ',', true);
        List<String> stateCodes = new ArrayList<>();

        for (String value : values) {
            value = value.trim();
            String stateCode = lookupStateValue(bean.getClass(), getFieldByAlias(alias).getJavaName(), value);

            if (!StringUtils.isBlank(stateCode)
                    && !stateCodes.contains(stateCode)) {
                stateCodes.add(stateCode);
            }
        }

        return stateCodes;
    }

    /**
     * Gets the state value by alias.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param ddxId String
     * @return String
     */
    public String getStateValueByAlias(X2BaseBean bean, String alias, String ddxId) {
        boolean required = true;
        DataDictionaryField field = getFieldByAlias(alias, ddxId, required);
        String value = (String) bean.getFieldValueByBeanPath(field.getJavaName());
        return lookupReferenceCodeByRefTbl(field.getReferenceTableOid(), value,
                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
    }

    /**
     * Gets the state value by bean path.
     *
     * @param bean X2BaseBean
     * @param beanPath String
     * @return String
     */
    public String getStateValueByBeanPath(X2BaseBean bean, String beanPath) {
        String value = (String) bean.getFieldValueByBeanPath(beanPath);
        return lookupStateValue(bean.getClass(), beanPath, value);
    }

    /**
     * Returns the federal lookup code for field value.
     * Look up based on bean path.
     *
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed table to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     *
     * @return String - state code for input value.
     */
    public String lookupFederalValue(Class beanClass, String beanPath, String value) {
        beanClass = getX2BeanClass(beanClass);
        String federalValue = lookupReferenceCodeByBeanPath(beanClass, beanPath, value,
                ExportFormatField.ReferenceMapTypeCode.FEDERAL.ordinal());
        return federalValue;
    }

    /**
     * Lookup federal value.
     *
     * @param column ToolBeanColumn
     * @param value String
     * @return String
     */
    public String lookupFederalValue(ToolBeanColumn column, String value) {
        return lookupReferenceCodeByColumn(column, value,
                ExportFormatField.ReferenceMapTypeCode.FEDERAL.ordinal());
    }

    /**
     * Returns the state lookup code for field value.
     * Look up based on bean path.
     *
     * @param alias String
     * @param value - the value to lookup and translate in the lookup table.
     * @param referenceMap - reference map type
     *        (ExportFormatField.ReferenceMapTypeCode.*.ordinal())
     *        of the lookup.
     * @return String - state code for input value
     */
    public String lookupReferenceCodeByAlias(String alias, String value, int referenceMap) {
        String stateValue = null;
        DataDictionaryField dictionaryField = getDictionary().findDataDictionaryFieldByAlias(alias);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            stateValue = lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value, referenceMap);
        }

        return stateValue;
    }

    /**
     * Returns the state lookup code for field value.
     * Look up based on bean path.
     *
     * @param column ToolBeanColumn
     * @param value - the value to lookup and translate in the lookup table.
     * @param referenceMap - reference map type
     *        (ExportFormatField.ReferenceMapTypeCode.*.ordinal())
     *        of the lookup.
     * @return String - state code for input value
     */
    public String lookupReferenceCodeByColumn(ToolBeanColumn column, String value, int referenceMap) {
        String stateValue = null;
        DataDictionaryField dictionaryField = column.getField(this);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            stateValue = lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value, referenceMap);
        }
        return stateValue;
    }

    /**
     * Returns the lookup code value for field value.
     * Look up based on the reference table.
     *
     * @param referenceTableOid String
     * @param value - the value to lookup and translate in the lookup table.
     * @param referenceMap - the reference map type
     *        (ExportFormatField.ReferenceMapTypeCode.*.ordinal()) of the lookup.
     * @return String - reference code lookup value for input value.
     */
    public String lookupReferenceCodeByRefTbl(String referenceTableOid, String value, int referenceMap) {
        String returnValue = null;
        Map<String, ReferenceCode> refCodes = getReferenceCodes(referenceTableOid);
        ReferenceCode code = refCodes.get(value);
        if (code != null) {
            if (referenceMap == ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()) {
                returnValue = code.getStateCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.FEDERAL.ordinal()) {
                returnValue = code.getFederalCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal()) {
                returnValue = code.getLocalCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.SYSTEM.ordinal()) {
                returnValue = code.getSystemCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.EDFI.ordinal()) {
                returnValue = code.getEdfiCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.SIF.ordinal()) {
                returnValue = code.getSifCode();
            }
        }

        return returnValue;
    }

    /**
     * Returns the state lookup code for field value.
     * Look up based on bean path.
     *
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed table to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     * @param referenceMap - reference map type
     *        (ExportFormatField.ReferenceMapTypeCode.*.ordinal())
     *        of the lookup.
     *
     * @return String - state code for input value.
     */
    public String lookupReferenceCodeByBeanPath(Class beanClass, String beanPath, String value, int referenceMap) {
        beanClass = getX2BeanClass(beanClass);
        String stateValue = null;
        DataDictionaryField dictionaryField = getDataDictionaryField(beanClass, beanPath);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            stateValue = lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value, referenceMap);
        }

        return stateValue;
    }

    /**
     * Returns the SIF lookup code for field value.
     * Look up based on bean path.
     *
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed table to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     *
     * @return String - state code for input value.
     */
    public String lookupSifValue(Class beanClass, String beanPath, String value) {
        String stateValue = lookupReferenceCodeByBeanPath(beanClass, beanPath, value,
                ExportFormatField.ReferenceMapTypeCode.SIF.ordinal());
        return stateValue;
    }

    /**
     * Returns the state lookup code for field value.
     * Look up based on bean path.
     *
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed tabl to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     *
     * @return String - state code for input value.
     */
    public String lookupStateValue(Class beanClass, String beanPath, String value) {
        beanClass = getX2BeanClass(beanClass);
        String stateValue = lookupReferenceCodeByBeanPath(beanClass, beanPath, value,
                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
        return stateValue;
    }

    /**
     * Lookup state value.
     *
     * @param column ToolBeanColumn
     * @param value String
     * @return String
     */
    public String lookupStateValue(ToolBeanColumn column, String value) {
        return lookupReferenceCodeByColumn(column, value,
                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
    }

    /**
     * Returns the user lookup code for a state reference code value.
     * Look up based on bean path.
     *
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed table to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     *
     * @return String - state code for input value.
     */
    public String lookupUserValue(Class beanClass, String beanPath, String value) {
        beanClass = getX2BeanClass(beanClass);
        String userValue = null;
        DataDictionaryField dictionaryField = getDataDictionaryField(beanClass, beanPath);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            Map<String, ReferenceCode> refcodes = getReferenceCodes(dictionaryField.getReferenceTableOid());
            for (ReferenceCode code : refcodes.values()) {
                if (!StringUtils.isEmpty(code.getStateCode()) && code.getStateCode().equals(value)) {
                    userValue = code.getCode();
                    break;
                }
            }
        }

        return userValue;
    }

    /**
     * Sets the alias as java type.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param value Object
     * @throws X2BaseException exception
     */
    public void setAliasAsJavaType(X2BaseBean bean, String alias, Object value) throws X2BaseException {
        setAliasAsJavaType(bean, alias, value, null);
    }

    /**
     * Sets the alias as java type.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param value Object
     * @param ddxId String
     * @throws X2BaseException exception
     */
    public void setAliasAsJavaType(X2BaseBean bean, String alias, Object value, String ddxId)
            throws X2BaseException {
        boolean required = false;
        DataDictionaryField field = getFieldByAlias(alias, ddxId, required);
        if (field != null) {
            if (field.isString()) {
                SystemStringConverter converter = getStringConverter(field);
                if (converter != null && value != null) {
                    value = converter.getSystemString(value);
                }
            }
            bean.setFieldValueByBeanPath(field.getJavaName(), value);
        }
    }

    /**
     * Translate alias to java name.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    public String translateAliasToJavaName(String alias, boolean required) {
        String javaName = null;

        DataDictionaryField field = getFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        } else if (required) {
            throw new X2RuntimeException(
                    new IllegalStateException("The alias [" + alias + "] could not be found."));
        }

        return javaName;
    }

    /**
     * Gets the converter.
     *
     * @param field DataDictionaryField
     * @return Converter
     */
    private Converter getConverter(DataDictionaryField field) {
        Converter converter = null;
        if (m_converterMap == null) {
            m_converterMap = new HashMap();
        }
        if (m_converterMap.keySet().contains(field)) {
            converter = m_converterMap.get(field);
        } else {
            converter = ConverterFactory.getConverterForClass(
                    field.getEffectiveJavaType(),
                    LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                    field.isString());
            m_converterMap.put(field, converter);
        }

        return converter;
    }


    /**
     * For a DataDictionaryField, find a SystemStringConverter appropriate for the field.
     * If no converter is appropriate, return null.
     * Use a map to maintain a cache of converters.
     *
     *
     * @param field DataDictionaryField
     * @return SystemStringConverter
     */
    private SystemStringConverter getStringConverter(DataDictionaryField field) {
        SystemStringConverter stringConverter = null;
        if (field.isString()) {
            Converter converter = getConverter(field);
            if (converter instanceof SystemStringConverter) {
                stringConverter = ((SystemStringConverter) converter);
            }
        }
        return stringConverter;
    }

    /**
     * Gets the x 2 bean class.
     *
     * @param beanClass Class
     * @return Class
     */
    private Class getX2BeanClass(Class beanClass) {
        Class result = beanClass;
        if (ToolBean.class.isAssignableFrom(beanClass)) {
            Class<ToolBean> toolBeanClass = beanClass;
            result = ToolBean.getX2BaseClassForClass(toolBeanClass);
        }
        if (result == null) {
            throw new IllegalStateException(
                    "Class " + beanClass.getName() + " cannot be converted to X2BaseBean class");
        }
        if (!X2BaseBean.class.isAssignableFrom(result)) {
            throw new IllegalStateException("Class " + beanClass.getName() + " is converted to " + result.getName()
                    + " which is not an X2BaseBean class");
        }
        return result;
    }

}
