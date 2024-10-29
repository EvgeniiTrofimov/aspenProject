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
package com.x2dev.procedures.sys.sped;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowPhase;
import com.follett.fsc.core.k12.beans.WorkflowPhaseOutcome;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryIndex;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryRelationship;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.workflow.WorkflowManager;
import com.follett.fsc.core.k12.tools.imports.ImportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.k12.web.workflow.SessionAccessManager;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.SisExtendedDataDictionary;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.CryptographyUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.converters.TimestampAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Type;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.sql.Timestamp;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class IepDataImport.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class IepDataImport extends ImportJavaSource {

    private static final String ALIAS_TIMSTAMP = "imported-timestamp";

    private static class DataRow {
        private String alias;
        private String id;
        private Integer index;
        private String relationship;
        private String value;
        private String rcdStateCode;
    }

    /**
     * The Class BeanData.
     */
    private class BeanData {
        private List<BeanField> m_fields = new ArrayList<>();
        private Map<String, List<BeanData>> m_relationships = new HashMap<>();

        /**
         * Append bean data.
         *
         * @param relationship String
         */
        public void appendBeanData(String relationship) {
            String[] relationships = relationship.split("\\.");
            String currentRelationship = relationships[0];
            if (!m_relationships.containsKey(currentRelationship)) {
                m_relationships.put(currentRelationship, new ArrayList<>());
            }
            if (relationships.length == 1) {
                BeanData beanData = new BeanData();
                m_relationships.get(currentRelationship).add(beanData);
            } else {
                List<BeanData> beans = m_relationships.get(currentRelationship);
                BeanData last = beans.get(beans.size() - 1);
                last.appendBeanData(shiftRelationship(relationship));
            }
        }

        /**
         * Append bean field.
         *
         * @param id String
         * @param alias String
         * @param value String
         * @param stateCode String
         */
        public void appendBeanField(String id, String alias, String value, String stateCode) {
            BeanField beanField = new BeanField();
            beanField.m_id = id;
            beanField.m_alias = alias;
            beanField.m_value = value;
            beanField.m_stateCode = stateCode;
            m_fields.add(beanField);
        }

        /**
         * Apply to.
         *
         * @param bean X2BaseBean
         */
        public void applyTo(X2BaseBean bean) {
            logBean(bean);

            List<DataDictionaryField> fields = m_dictionary.getFieldsForContext(bean.getClass().getName());
            m_fields.forEach(beanField -> {
                Optional<DataDictionaryField> field =
                        fields.stream().filter(
                                f -> !StringUtils.isEmpty(f.getAlias()) && f.getAlias().equals(beanField.m_alias))
                                .findFirst();
                if (!field.isPresent()) {
                    field = fields.stream().filter(f -> f.getId().equals(beanField.m_id)).findFirst();
                }
                if (field.isPresent()) {
                    bean.setFieldValueByBeanPath(field.get().getJavaName(),
                            string2Object(field.get(), beanField.m_value, beanField.m_stateCode));
                }
            });
            // set student relationship
            m_dictionary.getRelationshipsForContext(bean.getClass().getName()).stream()
                    .filter(relationship -> "stdOID".equals(m_dictionary
                            .findDataDictionaryField(relationship.getRelatedDataIndex().getFieldList()).getId()))
                    .forEach(relationship -> setRelationship(bean, m_student, relationship));

            // set additional iep relationship
            m_dictionary.getRelationshipsForContext(bean.getClass().getName()).stream()
                    .filter(relationship -> "iepOID".equals(m_dictionary
                            .findDataDictionaryField(relationship.getRelatedDataIndex().getFieldList()).getId()))
                    .forEach(relationship -> setRelationship(bean, m_iep, relationship));

            getBroker().saveBeanForced(bean);
            m_relationships.entrySet().forEach(relatedBeans -> {
                DataDictionaryRelationship relationship =
                        m_dictionary.findDataDictionaryRelationship(relatedBeans.getKey());
                relatedBeans.getValue().forEach(beanData -> {
                    X2BaseBean child = X2BaseBean.newInstance(relationship.getRelatedDataTable().getDataClass(),
                            getBroker().getPersistenceKey());
                    setRelationship(child, bean, relationship);
                    beanData.applyTo(child);
                });
            });

            writeTimestamp(bean, fields);
        }

        /**
         * Find bean data.
         *
         * @param relationship String
         * @param index int
         * @return BeanData
         */
        public BeanData findBeanData(String relationship, int index) {
            if (relationship.isEmpty()) {
                return this;
            }
            String[] relationships = relationship.split("\\.");
            List<BeanData> beans = m_relationships.get(relationships[0]);
            if (relationships.length == 1) {
                return beans.get(index);
            }
            BeanData last = beans.get(beans.size() - 1);
            return last.findBeanData(shiftRelationship(relationship), index);
        }

        private Object convertNonStringFieldValue(DataDictionaryField field, String stringValue) {
            Object convertedValue = stringValue;
            String format = WebUtils.generateFormat(field,
                    LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()));
            Converter baseConverter = ConverterFactory.getConverterForClass(
                    field.getEffectiveJavaType(),
                    LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                    true, format);

            if (baseConverter instanceof SystemStringConverter) {
                SystemStringConverter converter = ((SystemStringConverter) baseConverter);
                if (converter != null && !StringUtils.isEmpty(stringValue)) {
                    convertedValue = converter.parseSystemString(stringValue);
                    if (convertedValue instanceof Long && "int".equals(field.getJavaType())) {
                        convertedValue = Integer.valueOf(((Long) convertedValue).intValue());
                    }
                }
            }
            return convertedValue;
        }

        private Object convertReferenceCodeField(DataDictionaryField field, String code, String stateCode) {
            if (StringUtils.isEmpty(code)) {
                return code;
            }

            Collection<ReferenceCode> codes = field.getReferenceTable().getReferenceCodes();

            boolean rcdExists = codes.stream().anyMatch(rcd -> rcd.getCode().equals(code));
            if (rcdExists) {
                return code;
            }

            if (!StringUtils.isEmpty(stateCode)) {
                ReferenceCode rcdByStateCode = codes.stream()
                        .filter(rcd -> rcd.getStateCode() != null && rcd.getStateCode().equals(stateCode))
                        .findFirst()
                        .orElse(null);

                if (rcdByStateCode != null) {
                    writeLog(
                            "The value for {0} {1} was changed from {2} to {3} based on state code {4}.",
                            field.toString(), getAliasLogToken(field.getAlias()), code, rcdByStateCode.getCode(),
                            stateCode);
                    return rcdByStateCode.getCode();
                }
            }

            if (field.getValidReferenceOnlyIndicator()) {
                writeLog(
                        "The value for {0} {1} and value {2} was not imported because the value is not included in the required reference table.",
                        field.toString(), getAliasLogToken(field.getAlias()), code);
            }

            return field.getValidReferenceOnlyIndicator() ? null : code;
        }

        /**
         * @param alias
         * @return
         */
        private Object getAliasLogToken(String alias) {
            return StringUtils.isEmpty(alias) ? "with no alias" : "with alias " + alias;
        }

        /**
         * Sets the relationship.
         *
         * @param child X2BaseBean
         * @param parent X2BaseBean
         * @param relationship DataDictionaryRelationship
         */
        private void setRelationship(X2BaseBean child,
                                     X2BaseBean parent,
                                     DataDictionaryRelationship relationship) {
            DataDictionaryIndex parentIndex;
            DataDictionaryIndex childIndex;
            if (relationship.getRelatedRelationType().equals(DataDictionaryRelationship.ONE_TYPE_CODE)) {
                parentIndex = relationship.getRelatedDataIndex();
                childIndex = relationship.getPrimaryDataIndex();
            } else {
                childIndex = relationship.getRelatedDataIndex();
                parentIndex = relationship.getPrimaryDataIndex();
            }

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField parentField =
                    dictionary.findDataDictionaryField(parentIndex.getFieldList());
            DataDictionaryField childField =
                    dictionary.findDataDictionaryField(childIndex.getFieldList());

            try {
                String oid = (String) PropertyUtils.getProperty(parent, parentField.getJavaName());
                PropertyUtils.setProperty(child, childField.getJavaName(), oid);
            } catch (IllegalAccessException iae) {
                throw new X2RuntimeException(iae);
            } catch (InvocationTargetException ite) {
                throw new X2RuntimeException(ite);
            } catch (NoSuchMethodException nsme) {
                throw new X2RuntimeException(nsme);
            }
        }

        /**
         * Shift relationship.
         *
         * @param relationship String
         * @return String
         */
        private String shiftRelationship(String relationship) {
            String[] relationships = relationship.split("\\.");
            return String.join(".", ArrayUtils.remove(relationships, 0));
        }

        /**
         * String 2 object.
         *
         * @param field DataDictionaryField
         * @param stringValue String
         * @return Object
         */
        private Object string2Object(DataDictionaryField field, String stringValue, String stateCode) {
            Object convertedValue = stringValue;
            if (field.hasReferenceTable()) {
                convertedValue = convertReferenceCodeField(field,
                        stringValue == null ? stringValue : stringValue.trim(),
                        stateCode == null ? stateCode : stateCode.trim());
            } else if (!field.isString()) {
                convertedValue = convertNonStringFieldValue(field, stringValue);
            } else if (!StringUtils.isEmpty(stringValue)) {
                if (field.getLength() > 0 && stringValue.length() > field.getLength()) {
                    convertedValue = stringValue.substring(0, field.getLength());
                }
            }
            return convertedValue;
        }

        private void writeLog(String pattern, Object... arguments) {
            String logMessage = MessageFormat.format(pattern, arguments);
            m_refCodeConversionLog.add(logMessage);
        }

        private void writeTimestamp(X2BaseBean bean, List<DataDictionaryField> fields) {
            DataDictionaryField timestampDataField = fields.stream()
                    .filter(field -> ALIAS_TIMSTAMP.equals(field.getAlias()))
                    .findFirst()
                    .orElse(null);
            if (timestampDataField != null) {
                try {
                    String timeStamp = m_timestampConverter.getSystemString(new Timestamp(System.currentTimeMillis()));
                    BeanUtils.setProperty(bean, timestampDataField.getJavaName(), timeStamp);
                } catch (IllegalAccessException | InvocationTargetException e) {
                    logToolMessage(java.util.logging.Level.SEVERE, e.getMessage(), false);
                }
            }
        }
    }

    /**
     * The Class BeanField.
     */
    private static class BeanField {
        private String m_alias;
        private String m_id;
        private String m_value;
        private String m_stateCode;

        /**
         * Gets the alias.
         *
         * @return String
         */
        public String getAlias() {
            return m_alias;
        }

        /**
         * Gets the value.
         *
         * @return Object
         */
        public Object getValue() {
            return m_value;
        }
    }

    private static final String ENCRYPTION_EXT = ".enc";
    private static final String ENCRYPTION_KEY = "KviFiVfQbf5GMkw9QEOKaxCFSh8OfM4p";

    private static final String FORM_DEFINITION_TRANSFER = "SPED-XFER";

    private static final String PARAM_STAFF = "staff";
    private static final String PARAM_STUDENT = "student";

    private static final String WORKFLOW_DEFINITION_ID = "SYS-SPED-TRANS";

    private ModelBroker m_broker;
    private BeanData m_beanData;
    private Map<String, String> m_classNamesMap = new HashMap<>();
    private Map<String, Integer> m_classNamesStat = new HashMap<>();
    private int m_currentIndex = 0;
    private String m_currentRelationship = "";
    private DataDictionary m_dictionary;
    private IepData m_iep;
    private List<String> m_infoLog = new LinkedList();
    private List<String> m_refCodeConversionLog = new LinkedList();
    private SisStudent m_student;
    private TimestampAsStringConverter m_timestampConverter;
    private UserDataContainer m_userData;
    private List<String> m_validationErrors = new LinkedList();


    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.x2dev.sis.tools.ToolJavaSource#getBroker()
     */
    @Override
    public ModelBroker getBroker() {
        if (m_broker == null) {
            m_broker = new ModelBroker(getPrivilegeSet());
        }
        return m_broker;
    }

    /**
     * Gets the import statistics.
     *
     * @return String builder
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#getImportStatistics()
     */
    @Override
    protected StringBuilder getImportStatistics() {
        StringBuilder buffer = new StringBuilder();
        if (!m_validationErrors.isEmpty()) {
            buffer.append("Validation Errors");
            buffer.append('\n');
            m_validationErrors.stream().forEach(error -> buffer.append(error + "\n"));
        } else {
            buffer.append("Records imported\n\n");
            m_classNamesStat.keySet().stream().sorted().forEach(className -> {
                buffer.append(className);
                buffer.append(": ");
                buffer.append(m_classNamesStat.get(className));
                buffer.append('\n');
            });
            buffer.append('\n');
            if (!m_refCodeConversionLog.isEmpty()) {
                buffer.append("Reference codes conversion\n\n");
                m_refCodeConversionLog.forEach(message -> {
                    buffer.append(message);
                    buffer.append('\n');
                });
            }
        }
        return buffer;
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        m_beanData = new BeanData();
        SisExtendedDataDictionary extendedDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        m_dictionary =
                DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());

        WorkflowDefinition wfd = loadWorkflowDefinition();
        if (wfd == null) {
            m_validationErrors.add("Transfer workflow definition id could not be found");
        }
        m_student = getBroker().getBeanByOid(SisStudent.class, (String) getParameter(PARAM_STUDENT));
        if (m_student == null) {
            m_validationErrors.add("Student could not be located");
        }
        if (wfd != null && m_student != null) {
            String path = sourceFile.getPath();
            String decrypted = null;
            if (path.endsWith(ENCRYPTION_EXT)) {
                String encrypted = Files.readAllLines(sourceFile.toPath()).stream()
                        .reduce((first, second) -> first + second).orElse("");
                decrypted = CryptographyUtils.decrypt(encrypted, ENCRYPTION_KEY);
            }
            try (InputStream inputStream =
                    decrypted == null ? new FileInputStream(path) : new ByteArrayInputStream(decrypted.getBytes())) {
                Type listType = new TypeToken<ArrayList<DataRow>>() {/**/}.getType();
                Reader json = new InputStreamReader(inputStream, StandardCharsets.UTF_8.displayName());
                List<DataRow> lines = new Gson().fromJson(json, listType);
                lines.forEach(this::importLine);
                processBeanData(wfd);
            }
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_timestampConverter = (TimestampAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.TIMESTAMP_CONVERTER, userData.getLocale(), true);
        this.m_userData = userData;
    }

    /**
     * Creates the form.
     *
     * @param formDefinitionId String
     * @param ownerObject X2BaseBean
     * @param strorageObject X2BaseBean
     * @return FormInstance
     */
    private FormInstance createForm(String formDefinitionId, X2BaseBean ownerObject, X2BaseBean strorageObject) {
        FormDefinition formDefinition = getFormDefinition(formDefinitionId, getBroker());
        FormInstance form = X2BaseBean.newInstance(FormInstance.class, getBroker().getPersistenceKey());
        form.setFormDefinitionOid(formDefinition.getOid());
        form.setCreatedTime(System.currentTimeMillis());
        if (ownerObject != null) {
            form.setOwnerObjectOid(ownerObject.getOid());
        }
        form.setStorageObjectOid(strorageObject.getOid());
        getBroker().saveBeanForced(form);
        return form;
    }

    /**
     * Creates the transfer form.
     *
     * @return FormInstance
     */
    private FormInstance createTransferForm() {
        GenericFormData formData = X2BaseBean.newInstance(GenericFormData.class, getBroker().getPersistenceKey());
        getBroker().saveBeanForced(formData);
        FormInstance fmi = createForm(FORM_DEFINITION_TRANSFER, null, formData);
        return fmi;
    }

    /**
     * Gets the form definition.
     *
     * @param id String
     * @param broker X2Broker
     * @return Form definition
     */
    private FormDefinition getFormDefinition(String id, X2Broker broker) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(FormDefinition.COL_ID, id);

        QueryByCriteria query = new QueryByCriteria(FormDefinition.class, criteria);

        return (FormDefinition) broker.getBeanByQuery(query);
    }

    /**
     * Import line.
     *
     * @param line String[]
     */
    private void importLine(DataRow line) {
        String relationship = line.relationship;
        int index = line.index;
        if (index != m_currentIndex || !m_currentRelationship.equals(relationship)) {
            m_beanData.appendBeanData(relationship);
            m_currentIndex = index;
            m_currentRelationship = relationship;
        }
        BeanData beanData = m_beanData.findBeanData(relationship, index);
        beanData.appendBeanField(line.id, line.alias, line.value, line.rcdStateCode);
        incrementMatchCount();
    }

    /**
     * Load workflow definition.
     *
     * @return WorkflowDefinition
     */
    private WorkflowDefinition loadWorkflowDefinition() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(WorkflowDefinition.COL_ID, WORKFLOW_DEFINITION_ID);
        BeanQuery query = new BeanQuery(WorkflowDefinition.class, criteria);
        return getBroker().getBeanByQuery(query);
    }

    /**
     * Log bean.
     *
     * @param bean X2BaseBean
     */
    private void logBean(X2BaseBean bean) {
        String javaClassName = bean.getClass().getName();
        if (!m_classNamesMap.containsKey(javaClassName)) {
            DataDictionaryTable ddt = m_dictionary.findDataDictionaryTableByClass(bean.getClass().getName());
            m_classNamesMap.put(javaClassName, ddt.getUserName());
        }
        String className = m_classNamesMap.get(javaClassName);
        if (!m_classNamesStat.containsKey(className)) {
            m_classNamesStat.put(className, Integer.valueOf(1));
        } else {
            m_classNamesStat.put(className, 1 + m_classNamesStat.get(className));
        }
    }

    /**
     * Process bean data.
     *
     * @param wfd WorkflowDefinition
     * @throws X2BaseException exception
     */
    private void processBeanData(WorkflowDefinition wfd) throws X2BaseException {
        WorkflowManager workflowManager = new WorkflowManager(getBroker());
        SessionAccessManager accessManager = new SessionAccessManager(m_userData, getBroker());
        WorkflowPhase firstPhase =
                workflowManager.getInitialWorkflowPhase(wfd, m_userData.getUser(), accessManager);
        if (firstPhase == null) {
            firstPhase = wfd.getFirstWorkflowPhase();
        }
        WorkflowPhaseOutcome standardOutcome = ((firstPhase != null) ? firstPhase.getStandardPhaseOutcome() : null);

        boolean interrupted = false;
        getBroker().beginTransaction();
        try {
            Map<String, FormInstance> formInstances = new HashMap();
            formInstances.put(FORM_DEFINITION_TRANSFER, createTransferForm());

            Workflow workflow = workflowManager.initiateWorkflow(m_student.getOid(),
                    standardOutcome,
                    formInstances,
                    m_userData,
                    new PlainDate(),
                    false,
                    getLocale(),
                    accessManager);
            if (!workflowManager.getValidationErrors().isEmpty()) {
                interrupted = true;
                workflowManager.getValidationErrors().stream()
                        .forEach(error -> m_validationErrors.add(error.toString()));
            } else {
                m_iep = (IepData) workflow.getOwner();
                m_beanData.applyTo(m_iep);
                m_iep.setStaffOid((String) getParameter(PARAM_STAFF));
                m_iep.setStatusCodeEnum(StatusCode.DRAFT);
                getBroker().saveBeanForced(m_iep);
            }
        } catch (RuntimeException re) {
            interrupted = true;
            throw re;
        } finally {
            if (interrupted) {
                getBroker().rollbackTransaction();
            } else {
                getBroker().commitTransaction();
            }
        }
    }

}
