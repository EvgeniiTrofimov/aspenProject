/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.tool;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationResource;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.TranscriptDefinition;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * The Class PushOnsisCodes.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class PushOnsisCodes extends ProcedureJavaSource {

    /**
     * The Class Pair.
     *
     * @param <L> the generic type
     * @param <R> the generic type
     */
    public static class Pair<L, R> {
        /**
         * Of.
         *
         * @param <L> the generic type
         * @param <R> the generic type
         * @param left L
         * @param right R
         * @return Pair
         */
        public static <L, R> Pair<L, R> of(L left, R right) {
            return new Pair(left, right);
        }

        private final L m_left;
        private final R m_right;

        /**
         * Instantiates a new pair.
         *
         * @param left L
         * @param right R
         */
        private Pair(L left, R right) {
            m_left = left;
            m_right = right;
        }

        /**
         * Gets the left.
         *
         * @return l
         */
        public L getLeft() {
            return m_left;
        }

        /**
         * Gets the right.
         *
         * @return r
         */
        public R getRight() {
            return m_right;
        }
    }

    /**
     * The Class FieldLookup.
     */
    public class FieldLookup {
        String alias;
        String ddxId;
        DataDictionary dictionary;
        DataDictionaryField field;
        String fieldId;
        DataDictionaryTable table;
        String tableName;
        boolean valid = false;

        /**
         * Instantiates a new field lookup.
         *
         * @param element the element
         */
        public FieldLookup(Element element) {
            tableName = element.getAttributeValue("table-name");
            ddxId = element.getAttributeValue("ddx-id");
            alias = element.getAttributeValue("alias");
            fieldId = element.getAttributeValue("fieldId");
            init(element);
        }

        /**
         * Gets the ref table.
         *
         * @return the ref table
         */
        public ReferenceTable getRefTable() {
            return valid ? field.getReferenceTable() : null;
        }

        /**
         * To string.
         *
         * @return the string
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "tableName: " + tableName + ", ddxId: " + ddxId + ", alias: " + alias + ", fieldId: " + fieldId;
        }

        /**
         * Inits the.
         *
         * @param element the element
         */
        private void init(Element element) {
            dictionary = getDictionary(ddxId, this, element);
            if (dictionary != null) {
                table = dictionary.findDataDictionaryTableByDatabaseName(tableName);
                if (!StringUtils.isEmpty(alias)) {
                    field = dictionary.findDataDictionaryFieldByAlias(alias);
                } else {
                    field = dictionary.findDataDictionaryField(fieldId);
                }
                if (table == null) {
                    addFieldLookupWarning("Data table not found for ", this, (Element) element.getParent());
                } else {
                    if (field == null) {
                        addFieldLookupWarning("Data field not found for ", this, (Element) element.getParent());
                    } else {
                        if (!field.getTable().equals(table)) {
                            addFieldLookupWarning("Field associated with wring table ", this,
                                    (Element) element.getParent());
                        } else {
                            if (!field.hasReferenceTable()) {
                                addFieldLookupWarning("Reference table not found for ", this,
                                        (Element) element.getParent());
                            } else {
                                valid = true;
                            }
                        }
                    }
                }
            }
        }
    }

    private static final String ALIAS_RCD_DO_NOT_SYNC = "all-rcd-DoNotSync";
    private static final String ALIAS_RCD_END_DATE = "all-rcd-EndDate";
    private static final String ALIAS_RCD_MINISTRY_TABLE_NAME = "all-rcd-MinistryTableName";
    private static final String ALIAS_RCD_START_DATE = "all-rcd-StartDate";

    private static final String INPUT_PARAM_DICTIONARY_ONLY = "dictionaryOnly";
    private static final String INPUT_PARAM_UPDATE_CODES = "updateCodes";

    private static final String LOCALE_FRANCE = Locale.FRANCE.toString();

    private static final String REFERENCE_CODE_SEQUENCE_NUMBER_ATTR = "sequence-number";
    private static final String REFERENCE_CODE_DEPENDENCY_ATTR = "dependency-code";
    private static final String REFERENCE_CODE_DISABLED_ATTR = "disabled";
    private static final String REFERENCE_CODE_LOCAL_CODE_ATTR = "local-code";
    private static final String REFERENCE_CODE_STATE_CODE_ATTR = "state-code";
    private static final String REFERENCE_CODE_FEDERAL_CODE_ATTR = "federal-code";
    private static final String REFERENCE_CODE_SYSTEM_CODE_ATTR = "system-code";
    private static final String REFERENCE_CODE_EDFI_CODE_ATTR = "edfi-code";
    private static final String REFERENCE_CODE_SIF_CODE_ATTR = "sif-code";
    private static final String REFERENCE_CODE_EXTENDED_DICTIONARY_ID_ATTR = "ext-dictionary-id";
    private static final String REFERENCE_CODE_TEMPLATE_CONTEXT_ATTR = "template-context";
    private static final String REFERENCE_CODE_CUSTOM_FIELD_ALIAS_ATTR = "field-alias";

    Map<String, Map<String, List<ReferenceCode>>> m_rtbToCodesMap = new HashMap();

    /**
     * Adds the field lookup warning.
     *
     * @param error the error
     * @param fieldLookup the field lookup
     * @param rtbElement the rtb element
     */
    public void addFieldLookupWarning(String error, FieldLookup fieldLookup, Element rtbElement) {
        logWarningMessage(
                error + fieldLookup.toString() + " for reference table " + rtbElement.getAttributeValue("table-name"));
    }

    /**
     * Gets the dictionary.
     *
     * @param ddxId the ddx id
     * @return the dictionary
     */
    public DataDictionary getDictionary(String ddxId, FieldLookup fieldLookup, Element element) {
        DataDictionary dictionary = null;
        if (ddxId == null) {
            dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        } else {
            ExtendedDictionaryAttributes extDictAttributes = null;
            if (ddxId.toUpperCase().startsWith("ASD")) {
                extDictAttributes = getBroker().getBeanByOid(AssessmentDefinition.class, ddxId);
            } else if (ddxId.toUpperCase().startsWith("DDX")) {
                extDictAttributes = getBroker().getBeanByOid(ExtendedDataDictionary.class, ddxId);
            } else if (ddxId.toUpperCase().startsWith("EFD")) {
                extDictAttributes = getBroker().getBeanByOid(ExportFormatDefinition.class, ddxId);
            } else if (ddxId.toUpperCase().startsWith("GTD")) {
                extDictAttributes = getBroker().getBeanByOid(TranscriptDefinition.class, ddxId);
            } else {
                X2Criteria ddxCriteria = new X2Criteria();
                ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);
                QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
                ExtendedDataDictionary ddx = getBroker().getBeanByQuery(ddxQuery);
                if (ddx == null) {
                    X2Criteria asdCriteria = new X2Criteria();
                    asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, ddxId);
                    QueryByCriteria asdQuery = new QueryByCriteria(AssessmentDefinition.class, asdCriteria);
                    AssessmentDefinition asd = getBroker().getBeanByQuery(asdQuery);
                    if (asd == null) {
                        addFieldLookupWarning("Extended Dictionary Attributes not found for ", fieldLookup,
                                (Element) element.getParent());
                        return null;
                    }
                    extDictAttributes = asd;
                } else {
                    extDictAttributes = ddx;
                }
            }
            if (extDictAttributes == null) {
                addFieldLookupWarning("Invalid extended data dictionary id ", fieldLookup,
                        (Element) element.getParent());
                return null;
            }
            dictionary =
                    DataDictionary.getDistrictDictionary(extDictAttributes, getBroker().getPersistenceKey());
        }
        return dictionary;
    }

    /**
     * Gets the extended data dictionary by id.
     *
     * @param extendedDataDictionaryId the extended data dictionary id
     * @return the extended data dictionary
     */
    public ExtendedDataDictionary getExtendedDataDictionary(String extendedDataDictionaryId) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, extendedDataDictionaryId);

        return getBroker().getBeanByQuery(new BeanQuery(ExtendedDataDictionary.class, criteria));
    }

    /**
     * Gets the root element from an XML string (input definition, view template,
     * etc.)
     *
     * @param xmlString the xml string
     * @return Element
     * @throws JDOMException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    public Element getXmlRoot(String xmlString) throws JDOMException, IOException {
        Element xmlRoot = null;

        if (xmlString != null) {
            SAXBuilder builder = new SAXBuilder();
            Document xmlDocument = builder.build(new ByteArrayInputStream(xmlString.getBytes()));
            xmlRoot = xmlDocument.getRootElement();
        } else {
            throw new RuntimeException("No XML root in string");
        }
        return xmlRoot;
    }

    /**
     * Logs message and sets m_errors so procedure won't be deleted.
     *
     * @param message the message
     */
    public void logErrorMessage(String message) {
        logMessage("ERROR: " + message);
    }

    /**
     * Logs message and sets m_errors so procedure won't be deleted.
     *
     * @param message the message
     */
    public void logInfoMessage(String message) {
        logMessage("INFO: " + message);
    }

    /**
     * Log warning message.
     *
     * @param message the message
     */
    public void logWarningMessage(String message) {
        logMessage("WARNING: " + message);
    }

    /**
     * Execute.
     *
     * @throws Exception the exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Procedure procedure = (Procedure) this.getJob().getTool();
        if (procedure != null) {
            String inputDefinition = procedure.getFullInputDefinition();
            Element root = getXmlRoot(inputDefinition);
            Element parent = getParentDocument(root, "reference-tables");
            if (parent != null) {
                boolean dictionaryOnlyIndicator = false; // will be used as default when deployed by
                // SAPPY
                if (getParameter(INPUT_PARAM_DICTIONARY_ONLY) != null
                        && getParameter(INPUT_PARAM_DICTIONARY_ONLY) instanceof Boolean) {
                    dictionaryOnlyIndicator = ((Boolean) getParameter(INPUT_PARAM_DICTIONARY_ONLY)).booleanValue();
                }
                boolean updateCodesIndicator = true; // will be used as default when deployed by
                // SAPPY
                if (getParameter(INPUT_PARAM_UPDATE_CODES) != null
                        && getParameter(INPUT_PARAM_UPDATE_CODES) instanceof Boolean) {
                    updateCodesIndicator = ((Boolean) getParameter(INPUT_PARAM_UPDATE_CODES)).booleanValue();
                }

                List referenceTables = parent.getChildren("reference-table");
                for (Object item : referenceTables) {
                    Element rtbElement = (Element) item;
                    updateReferenceTable(rtbElement, dictionaryOnlyIndicator, updateCodesIndicator);
                }
            }
        } else {
            logErrorMessage("Not able to find XML root in input definition");
        }
        getBroker().deleteBean(procedure);
    }

    /**
     * Gets the document from the parent.
     *
     * @param parent Element
     * @param rootName String
     * @return Element
     * @throws JDOMException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    protected Element getParentDocument(Element parent, String rootName) throws JDOMException, IOException {
        Element document = null;
        if (parent.getChildren() != null && parent.getChildren().size() > 0) {
            Iterator<Element> children = parent.getChildren().iterator();
            while (children.hasNext()) {
                Element element = children.next();
                String name = element.getName();
                if (rootName.equals(name)) {
                    document = element;
                    break;
                }
            }
        }
        return document;
    }

    /**
     * Adds the reference code.
     *
     * @param referenceTable the reference table
     * @param childElement the child element
     * @param updateCodes the update codes
     */
    private void addReferenceCode(ReferenceTable referenceTable, Element childElement, boolean updateCodes) {

        List<String> messages = new ArrayList<>();

        String code = childElement.getAttributeValue(ReferenceCode.COL_CODE);

        if (!StringUtils.isBlank(code)) {
            ReferenceCode bean = getUpdateBean(code, referenceTable, updateCodes, messages);
            if (bean == null) {
                logErrorMessage(
                        "reference code " + code + " in " + referenceTable.getUserName() + " cannot be created");
                return;
            }

            Boolean cancelSave = false;
            Iterator customFieldsIterator = (childElement.getChildren()).iterator();
            while (customFieldsIterator.hasNext()) {
                Element customFieldsElement = (Element) customFieldsIterator.next();
                Iterator customFieldIterator = (customFieldsElement.getChildren()).iterator();
                while (customFieldIterator.hasNext()) {
                    Element customFieldElement = (Element) customFieldIterator.next();
                    String alias = customFieldElement
                            .getAttributeValue(REFERENCE_CODE_CUSTOM_FIELD_ALIAS_ATTR);
                    if (ALIAS_RCD_END_DATE.equals(alias)) {
                        cancelSave = true;
                    }
                }
            }

            if (!cancelSave) {
                messages.stream().forEach(this::logInfoMessage);
            }
            if (updateCodes && !cancelSave) {
                setValueByBeanPath(ReferenceCode.COL_CATEGORY, bean, childElement, ReferenceCode.COL_CATEGORY);
                setValueByBeanPath(ReferenceCode.COL_DEPENDENCY_CODE, bean, childElement,
                        REFERENCE_CODE_DEPENDENCY_ATTR);
                setValueByBeanPath(ReferenceCode.COL_DESCRIPTION, bean, childElement,
                        ReferenceCode.COL_DESCRIPTION);

                // Set the disabled indicator to false because the XML only includes this
                // attribute when true
                bean.setDisabledIndicator(Boolean.FALSE);
                setBooleanValueByBeanPath(bean, childElement, REFERENCE_CODE_DISABLED_ATTR,
                        ReferenceCode.COL_DISABLED_INDICATOR);
                setValueByBeanPath(ReferenceCode.COL_EDFI_CODE, bean, childElement,
                        REFERENCE_CODE_EDFI_CODE_ATTR);
                // Check if code has an extended dictionary ID
                String codeExtendedDictionaryId = childElement
                        .getAttributeValue(REFERENCE_CODE_EXTENDED_DICTIONARY_ID_ATTR);
                if (!StringUtils.isBlank(codeExtendedDictionaryId)) {
                    if (codeExtendedDictionaryId.equals(referenceTable.getExtendedDataDictionaryOid())) {
                        // If ID is same as reference table, don't query, just use reference table's
                        // oid
                        bean.setExtendedDataDictionaryOid(referenceTable.getExtendedDataDictionaryOid());
                    } else {
                        // Query for the extended dictionary if the IDs are different (which should
                        // never be the case)
                        ExtendedDataDictionary extendedDataDictionary = getExtendedDataDictionary(
                                codeExtendedDictionaryId);
                        if (extendedDataDictionary != null) {
                            bean.setExtendedDataDictionaryOid(extendedDataDictionary.getOid());
                        }
                    }
                }
                setValueByBeanPath(ReferenceCode.COL_FEDERAL_CODE, bean, childElement,
                        REFERENCE_CODE_FEDERAL_CODE_ATTR);
                setValueByBeanPath(ReferenceCode.COL_LOCAL_CODE, bean, childElement,
                        REFERENCE_CODE_LOCAL_CODE_ATTR);
                setValueByBeanPath(ReferenceCode.COL_SIF_CODE, bean, childElement,
                        REFERENCE_CODE_SIF_CODE_ATTR);
                setValueByBeanPath(ReferenceCode.COL_STATE_CODE, bean, childElement,
                        REFERENCE_CODE_STATE_CODE_ATTR);
                setValueByBeanPath(ReferenceCode.COL_SYSTEM_CODE, bean, childElement,
                        REFERENCE_CODE_SYSTEM_CODE_ATTR);
                setValueByBeanPath(ReferenceCode.COL_TEMPLATE_CONTEXT, bean, childElement,
                        REFERENCE_CODE_TEMPLATE_CONTEXT_ATTR);
                if (childElement.getAttributeValue(REFERENCE_CODE_SEQUENCE_NUMBER_ATTR) != null) {
                    bean.setSequenceNumber(Integer
                            .parseInt(childElement.getAttributeValue(REFERENCE_CODE_SEQUENCE_NUMBER_ATTR)));
                }

                // Import extended fields
                customFieldsIterator = (childElement.getChildren()).iterator();
                while (customFieldsIterator.hasNext()) {
                    DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                            bean.getExtendedDataDictionary(), getBroker().getPersistenceKey());
                    if (dictionary == null) {
                        dictionary = DataDictionary.getDistrictDictionary(
                                referenceTable.getExtendedDataDictionary(), getBroker().getPersistenceKey());
                    }
                    Element customFieldsElement = (Element) customFieldsIterator.next();
                    Iterator customFieldIterator = (customFieldsElement.getChildren()).iterator();
                    while (customFieldIterator.hasNext()) {
                        Element customFieldElement = (Element) customFieldIterator.next();
                        String alias = customFieldElement
                                .getAttributeValue(REFERENCE_CODE_CUSTOM_FIELD_ALIAS_ATTR);
                        String value = customFieldElement.getValue();
                        if (!StringUtils.isBlank(alias) && !StringUtils.isBlank(value)) {
                            bean.setFieldValueByAlias(alias, value, dictionary);
                        }
                    }
                }

                getBroker().saveBeanForced(bean);

                if (childElement.getAttributeValue("fr-code") != null) {
                    saveResource(bean, SisBeanPaths.REF_CODE.code().getColumnOid(),
                            childElement.getAttributeValue("fr-code"));
                }
                if (childElement.getAttributeValue("fr-description") != null) {
                    saveResource(bean, SisBeanPaths.REF_CODE.description().getColumnOid(),
                            childElement.getAttributeValue("fr-description"));
                }
            }
        }
    }

    /**
     * Discover reference table.
     *
     * @param rtbElement the rtb element
     * @return the reference table
     */
    private ReferenceTable discoverReferenceTable(Element rtbElement) {
        ReferenceTable result = null;
        String userName = rtbElement.getAttributeValue("user-name");
        for (Object item : rtbElement.getChildren("field-lookup")) {
            FieldLookup lookup = new FieldLookup((Element) item);
            if (lookup.getRefTable() != null) {
                if (result == null) {
                    result = lookup.getRefTable();
                } else if (!lookup.getRefTable().getOid().equals(result.getOid())) {
                    logErrorMessage(
                            "Ambiguous lookup for reference table " + userName);
                    return null;
                }
            }
        }
        if (result == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisBeanPaths.REF_TABLE.userName().getPath(), userName);
            result = getBroker().getBeanByQuery(new BeanQuery(SisBeanPaths.REF_TABLE.getBeanType(), criteria));
            if (result == null) {
                logErrorMessage("No valid lookup for reference table " + userName);
            }
            logWarningMessage("Reference table " + userName + " matched by name only.  No field match found");
        }
        return result;
    }

    /**
     * Gets the codes by state.
     *
     * @param rtb the rtb
     * @param stateCode the state code
     * @return the codes by state
     */
    private List<ReferenceCode> getCodesByState(ReferenceTable rtb, String stateCode) {
        Map<String, List<ReferenceCode>> codesMap = m_rtbToCodesMap.get(rtb.getOid());
        if (codesMap == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisBeanPaths.REF_CODE.referenceTableOid().getPath(), rtb.getOid());
            BeanQuery query = new BeanQuery(SisBeanPaths.REF_CODE.getBeanType(), criteria);

            codesMap = getBroker().getGroupedCollectionByQuery(query, SisBeanPaths.REF_CODE.stateCode().getPath(), 100);

            m_rtbToCodesMap.put(stateCode, codesMap);
        }
        return codesMap.get(stateCode) == null ? Collections.EMPTY_LIST : codesMap.get(stateCode);
    }

    /**
     * Gets the custom field.
     *
     * @param element the element
     * @param alias the alias
     * @return the custom field
     */
    private String getCustomField(Element element, String alias) {
        String result = null;
        Element customFields = element.getChild("custom-fields");
        if (customFields == null) {
            return null;
        }
        Element customField = (Element) customFields.getChildren("custom-field").stream()
                .filter(item -> {
                    Element field = (Element) item;
                    String aliasValue = field.getAttributeValue("field-alias");
                    return aliasValue != null && aliasValue.equals(alias);
                }).findAny().orElse(null);
        return customField == null ? null : customField.getValue();
    }

    /**
     * Gets the reference code.
     *
     * @param referenceTableOid the reference table oid
     * @param code the code
     * @return the reference code
     */
    private ReferenceCode getReferenceCode(String referenceTableOid, String code) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
        criteria.addEqualTo(ReferenceCode.COL_CODE, code);
        BeanQuery query = new BeanQuery(ReferenceCode.class, criteria);

        return getBroker().getBeanByQuery(query);
    }

    /**
     * Gets the update bean.
     *
     * @param code the code
     * @param referenceTable the reference table
     * @param updateCodes
     * @param messages info output
     * @return the update bean
     */
    private ReferenceCode getUpdateBean(String code,
                                        ReferenceTable referenceTable,
                                        boolean updateCodes,
                                        List<String> messages) {
        for (String newCode = code; newCode.length() <= referenceTable.getCodeLength(); newCode = newCode + "+") {
            ReferenceCode bean = getReferenceCode(referenceTable.getOid(), newCode);
            if (bean == null) {
                bean = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
                bean.setReferenceTableOid(referenceTable.getOid());
                bean.setOwnerOid(referenceTable.getOwnerOid());
                bean.setOwnerType(referenceTable.getOwnerType());
                bean.setCode(newCode);
                messages.add("reference code " + newCode + " in " + referenceTable.getUserName() + " created");
                return bean;
            } else if (StringUtils.isEmpty(bean.getStateCode())) {
                if (updateCodes) {
                    bean.setCode(newCode);
                }
                messages.add("reference code " + newCode + " in " + referenceTable.getUserName() + " updated");
                return bean;
            }
        }
        return null;
    }

    /**
     * Save bean.
     *
     * @param bean X2BaseBean
     * @return true, if successful
     */
    private boolean saveBean(X2BaseBean bean) {
        boolean ok = true;
        List<ValidationError> errors = getBroker().saveBean(bean);
        if (errors != null && !errors.isEmpty()) {
            for (ValidationError error : errors) {
                logErrorMessage("Validation error: " + error.toString());
            }
            ok = false;
        }
        return ok;
    }

    /**
     * Save localization resource for created/updated reference code.
     *
     * @param refCode the ref code
     * @param beanPath the bean path
     * @param value the value
     * @return success flag
     */
    private boolean saveResource(ReferenceCode refCode, String beanPath, String value) {

        String key =
                LocalizationUtils.generateKey(refCode.getOid(), beanPath);
        LocalizationResource resource = new LocalizationResource(LOCALE_FRANCE,
                key, refCode.getOid(),
                value);
        LocalizationCache.setLocalizedResource(getBroker(), resource);
        return true;
    }

    /**
     * Sets a boolean value by the bean path.
     *
     * @param bean the bean
     * @param element the element
     * @param attribute the attribute
     * @param beanPath the bean path
     */
    private void setBooleanValueByBeanPath(X2BaseBean bean, Element element, String attribute, String beanPath) {
        String value = element.getAttributeValue(attribute);
        if (!StringUtils.isBlank(value)) {
            bean.setFieldValueByBeanPath(beanPath, Boolean.parseBoolean(value));
        }
    }

    /**
     * Sets the value on the bean by the bean path.
     *
     * @param beanPath the bean path
     * @param bean the bean
     * @param element the element
     * @param attribute the attribute
     */
    private void setValueByBeanPath(String beanPath, X2BaseBean bean, Element element, String attribute) {
        String value = element.getAttributeValue(attribute);
        if (value != null) {
            bean.setFieldValueByBeanPath(beanPath, value);
        }
    }

    /**
     * Update reference code.
     *
     * @param code the code
     * @param rcdElement the rcd element
     * @param updateCodes the update codes
     */
    private void updateReferenceCode(ReferenceCode code, Element rcdElement, boolean updateCodes) {
        Map<String, Pair<String, String>> map = new HashMap();

        String doNotSyncString = (String) code.getFieldValueByAlias(ALIAS_RCD_DO_NOT_SYNC);
        boolean doNotSync = BooleanAsStringConverter.TRUE.equals(doNotSyncString);

        boolean disabledElement =
                rcdElement.getAttribute("disabled") != null && rcdElement.getAttributeValue("disabled").equals("true");
        if (!doNotSync && disabledElement && !code.getDisabledIndicator()) {
            map.put(ReferenceCode.COL_DISABLED_INDICATOR, Pair.of("false", "true"));
            if (updateCodes) {
                code.setDisabledIndicator(true);
            }
        } else if (!doNotSync && !disabledElement && code.getDisabledIndicator()) {
            map.put(ReferenceCode.COL_DISABLED_INDICATOR, Pair.of("true", "false"));
            if (updateCodes) {
                code.setDisabledIndicator(false);
            }
        }

        String sifElement = rcdElement.getAttributeValue("sif-code");
        if (sifElement != null && !sifElement.equals(code.getSifCode())) {
            map.put(ReferenceCode.COL_SIF_CODE, Pair.of(StringUtils.unNullify(code.getSifCode()), sifElement));
            if (updateCodes) {
                code.setSifCode(sifElement);
            }
        } else if (sifElement == null && !StringUtils.isEmpty(code.getSifCode())) {
            map.put(ReferenceCode.COL_SIF_CODE, Pair.of(code.getSifCode(), ""));
            if (updateCodes) {
                code.setSifCode(null);
            }
        }

        String startDateElement = getCustomField(rcdElement, ALIAS_RCD_START_DATE);
        String endDateElement = getCustomField(rcdElement, ALIAS_RCD_END_DATE);
        String startDate = (String) code.getFieldValueByAlias(ALIAS_RCD_START_DATE);
        String endDate = (String) code.getFieldValueByAlias(ALIAS_RCD_END_DATE);

        if ((startDateElement != null && !startDateElement.equals(startDate))
                || (startDateElement == null && startDate != null)) {
            map.put(ALIAS_RCD_START_DATE, Pair.of(startDate, startDateElement));
            if (updateCodes) {
                code.setFieldValueByAlias(ALIAS_RCD_START_DATE, startDateElement);
            }
        }
        if ((endDateElement != null && !endDateElement.equals(endDate))
                || (endDateElement == null && endDate != null)) {
            map.put(ALIAS_RCD_END_DATE, Pair.of(endDate, endDateElement));
            if (updateCodes) {
                code.setFieldValueByAlias(ALIAS_RCD_END_DATE, endDateElement);
            }
        }


        String tableNameElement = getCustomField(rcdElement, ALIAS_RCD_MINISTRY_TABLE_NAME);
        String tableName = (String) code.getFieldValueByAlias(ALIAS_RCD_MINISTRY_TABLE_NAME);

        if (!StringUtils.isEmpty(tableNameElement) && !tableNameElement.equals(tableName)) {
            map.put(ALIAS_RCD_MINISTRY_TABLE_NAME, Pair.of(tableName, tableNameElement));
            if (updateCodes) {
                code.setFieldValueByAlias(ALIAS_RCD_MINISTRY_TABLE_NAME, tableNameElement);
            }
        }
        if (!map.isEmpty()) {
            logInfoMessage("Reference code " + code.getCode() + " with ONSIS code " + code.getStateCode()
                    + " in reference table " + code.getReferenceTable().getUserName() + " is updated "
                    + map.entrySet().stream()
                            .map(entry -> entry.getKey() + " from [" + entry.getValue().getLeft() + "] to ["
                                    + entry.getValue().getRight() + "]")
                            .collect(Collectors.joining(", ", "{", "}")));
            if (updateCodes) {
                saveBean(code);
            }
        }
    }

    /**
     * Update reference table.
     *
     * @param rtbElement the rtb element
     * @param dictionaryOnly the dictionary only
     * @param updateCodes the update codes
     */
    private void updateReferenceTable(Element rtbElement, boolean dictionaryOnly, boolean updateCodes) {
        Set<String> processedStateCodes = new HashSet();

        ReferenceTable rtb = discoverReferenceTable(rtbElement);

        if (rtb != null && !dictionaryOnly) {
            // process enabled source reference codes first
            rtbElement.getChildren("reference-code").stream()
                    .filter(rcdItem -> {
                        Attribute disabled = ((Element) rcdItem).getAttribute("disabled");
                        return disabled == null || !disabled.getValue().equals("true");
                    }).forEach(item -> {
                        Element rcdElement = (Element) item;
                        String stateCode = rcdElement.getAttributeValue("state-code");
                        if (!processedStateCodes.contains(stateCode)) {
                            List<ReferenceCode> enabledCodes = getCodesByState(rtb, stateCode).stream()
                                    .filter(rcd -> !rcd.getDisabledIndicator())
                                    .collect(Collectors.toList());
                            // Update all enabled codes found with matching state code
                            if (enabledCodes.size() > 0) {
                                for (ReferenceCode code : enabledCodes) {
                                    updateReferenceCode(code, rcdElement, updateCodes);
                                }
                                processedStateCodes.add(stateCode);
                            }
                        }
                        if (!processedStateCodes.contains(stateCode)) {
                            // Since no enabled code was found, update all disabled codes
                            List<ReferenceCode> disabledCodes = getCodesByState(rtb, stateCode).stream()
                                    .filter(rcd -> rcd.getDisabledIndicator())
                                    .collect(Collectors.toList());
                            if (disabledCodes.size() > 0) {
                                for (ReferenceCode code : disabledCodes) {
                                    updateReferenceCode(code, rcdElement, updateCodes);
                                }
                                processedStateCodes.add(stateCode);
                            }
                        }
                        // Remaining codes not processed are adds.
                        if (!processedStateCodes.contains(stateCode)) {
                            addReferenceCode(rtb, rcdElement, updateCodes);
                        }
                    });

            // Now process disabled codes
            rtbElement.getChildren("reference-code").stream()
                    .filter(rcdItem -> {
                        Attribute disabled = ((Element) rcdItem).getAttribute("disabled");
                        return disabled != null && disabled.getValue().equals("true");
                    }).forEach(item -> {
                        Element rcdElement = (Element) item;
                        String stateCode = rcdElement.getAttributeValue("state-code");
                        if (!processedStateCodes.contains(stateCode)) {
                            // Update enabled and disabled codes
                            List<ReferenceCode> allCodes = getCodesByState(rtb, stateCode);
                            // Update all enabled codes found with matching state code
                            if (allCodes.size() > 0) {
                                for (ReferenceCode code : allCodes) {
                                    updateReferenceCode(code, rcdElement, updateCodes);
                                }
                                processedStateCodes.add(stateCode);
                            }
                        }
                        // Remaining codes not processed are adds.
                        if (!processedStateCodes.contains(stateCode)) {
                            addReferenceCode(rtb, rcdElement, updateCodes);
                        }
                    });
        }
    }


}
