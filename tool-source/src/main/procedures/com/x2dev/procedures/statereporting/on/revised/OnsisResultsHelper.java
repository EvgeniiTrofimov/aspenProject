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
package com.x2dev.procedures.statereporting.on.revised;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap;
import com.x2dev.procedures.statereporting.on.ResultException;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisRecord.HeaderErrors;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisStateReportEntity;
import com.x2dev.procedures.statereporting.on.revised.OnsisValidations.OnsisElement;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2RuntimeException;
import java.io.StringReader;
import java.lang.reflect.InvocationTargetException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

/**
 * The Class OnsisResultsHelper.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnsisResultsHelper {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Interface ElementsHelper.
     */
    public interface ElementsHelper {

        /**
         * Gets the keys by path.
         *
         * @param path List<String>
         * @return Linked hash map
         */
        public LinkedHashMap<String, String> getKeysByPath(List<String> path);

        /**
         * Gets the root name.
         *
         * @return String
         */
        public String getRootName();
    }

    // TODO: refactor this to be private with a public isRoot test if possible
    public static final List<String> s_submissionsTypes =
            Arrays.asList("SCHOOL_SUBMISSION", "BOARD_SUBMISSION", "OEN_VALIDATION_FILE",
                    "OEN_VALIDATION_FILE/OEN_BATCH_MULTIPLES");

    public static final MultiLevelMap.ValueByKeyResolver ONSIS_XML_RESULT_NODE_RESOLVER =
            new MultiLevelMap.ValueByKeyResolver() {

                /**
                 * @see com.x2dev.procedures.statereporting.on.FilterableFactory.MultiLevelMap.ValueByKeyResolver#getValue(java.lang.String,
                 *      java.lang.Object)
                 */
                @Override
                public Object getValue(String key, Object entity) {
                    return OnsisResultsHelper.getValueByElementName((Node) entity, key);
                }

            };

    final static String ELEMENTS_DELIMITER = "/";

    private final static String ELEMENT_NAME_ACTION = "ACTION";
    private static final String ENCODING_UTF_8 = "UTF-8";
    private final static SimpleDateFormat s_timestampDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    /**
     * The Enum IDENTIFYING_FIELD.
     */
    public enum IDENTIFYING_FIELD {
        SCHOOL_NUMBER(SisSchool.class),
        //
        MEN(SisStaff.class),
        //
        OEN(SisStudent.class) {
            @Override
            public X2Criteria getCriteria(String fieldName,
                                          String value) {
                X2Criteria criteria = new X2Criteria();
                String withoutDashes = value.replaceAll("-", "");
                List<String> withDashesList = new ArrayList<>(Arrays.asList(withoutDashes.split("")));
                withDashesList.add(3, "-");
                withDashesList.add(7, "-");
                criteria.addIn(fieldName, Arrays.asList(String.join("", withDashesList), withoutDashes));
                return criteria;
            }
        },
        //
        CLASS_CODE(MasterSchedule.class) {
            @Override
            public X2Criteria getCriteria(String fieldName,
                                          String value) {
                if (!StringUtils.isEmpty(fieldName)) {
                    return super.getCriteria(fieldName, value);
                }
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(MasterSchedule.COL_COURSE_VIEW, value);
                X2Criteria classCriteria = new X2Criteria();
                classCriteria.addEqualTo(
                        MasterSchedule.REL_SCHEDULE_CLASSES + ModelProperty.PATH_DELIMITER + ScheduleClass.COL_ID,
                        value);
                criteria.addOrCriteria(classCriteria);
                return criteria;
            }
        };

        private Class m_class;

        /**
         * Instantiates a new identifying field.
         *
         * @param clazz Class
         */
        private IDENTIFYING_FIELD(Class clazz) {
            m_class = clazz;
        }

        /**
         * Gets the clazz.
         *
         * @return Class
         */
        public Class getClazz() {
            return m_class;
        }

        /**
         * Gets the criteria.
         *
         * @param fieldName String
         * @param value String
         * @return X 2 criteria
         */
        public X2Criteria getCriteria(String fieldName, String value) {
            if (StringUtils.isEmpty(fieldName)) {
                return null;
            }
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(fieldName, value);
            return criteria;
        }

        /**
         * Gets the by class.
         *
         * @param clazz Class
         * @return identifying field
         */
        public static IDENTIFYING_FIELD getByClass(Class clazz) {
            for (IDENTIFYING_FIELD field : values()) {
                if (field.getClazz().equals(clazz)) {
                    return field;
                }
            }
            return null;
        }
    }

    /**
     * The Interface NodeHandler.
     */
    public interface NodeHandler {

        /**
         * Handle node.
         *
         * @param node Node
         */
        public void handleNode(Node node);
    }

    /**
     * The Class OnsisError.
     */
    static class OnsisError {

        /**
         * The Enum ErrorField.
         */
        enum ErrorField {
            E_MESSAGE, F_MESSAGE, FIELD_NAME, FIELD_VALUE, MESSAGE_CODE;
        }

        private final static String ALIAS_RECORD_OID = "record-oid";
        private final static String DDX_ID = "ON-SIS-ERROR";

        private final UserDefinedTableA m_bean;
        private final OnsisRecord m_record;

        private String m_stringRepresention;

        /**
         * Instantiates a new onsis error.
         *
         * @param broker X2Broker
         * @param dictionary DataDictionary
         * @param headerErrors Node
         * @param errorParser ErrorParser
         * @param record OnsisRecord
         */
        private OnsisError(X2Broker broker, DataDictionary dictionary, HeaderErrors headerErrors,
                OnsisResult.ErrorParser errorParser,
                OnsisRecord record) {
            m_record = record;
            m_bean = new UserDefinedTableA(broker.getPersistenceKey());
            errorParser.parseToBean(m_bean, headerErrors, dictionary);
            m_bean.setFieldValueByAlias(ALIAS_RECORD_OID, m_record.getBean().getOid(), dictionary);

            m_bean.setFieldValueByBeanPath(UserDefinedTableA.COL_EXTENDED_DATA_DICTIONARY_OID,
                    dictionary.getExtendedDictionaryOid());
            m_bean.setOrganization1Oid(record.getBean().getOrganization1Oid());
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (!(obj instanceof OnsisError)) {
                return false;
            }
            return obj.toString().equals(toString());
        }

        /**
         * Gets the field value by alias.
         *
         * @param alias String
         * @return Object
         */
        public Object getFieldValueByAlias(String alias) {
            return m_bean.getFieldValueByAlias(alias, m_record.m_result.getDictionary(DDX_ID));
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return getDescription().hashCode();
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            if (m_stringRepresention == null) {
                StringBuilder builder = new StringBuilder();
                builder.append(m_record.toString());
                builder.append(", ");
                builder.append(getDescription());
                m_stringRepresention = builder.toString();
            }
            return m_stringRepresention;
        }

        /**
         * Gets the description.
         *
         * @return String
         */
        private String getDescription() {
            StringBuilder builder = new StringBuilder();
            builder.append("Message Code: ");
            builder.append(getFieldValueByAlias(ErrorField.MESSAGE_CODE.toString()));
            builder.append(", Message: ");
            builder.append(getFieldValueByAlias(ErrorField.E_MESSAGE.toString()));
            return builder.toString();
        }
    }

    /**
     * The Class OnsisException.
     */
    public static class OnsisException {
        public static final String DDX_ID = "ON-SIS-EXC";

        private static final String ALIAS_CREATION_DATE = "creation-date";
        private static final String ALIAS_EXCEPTION_DESCRIPTION = "exception-description";
        private static final String ALIAS_RESULT_OID = "result-oid";

        private final UserDefinedTableB m_bean;

        /**
         * Instantiates a new onsis exception.
         *
         * @param broker X2Broker
         * @param organization Organization
         * @param exceptionDescription String
         * @param ddxExtractor DictionaryExtractor
         */
        public OnsisException(X2Broker broker, Organization organization, String exceptionDescription,
                DictionaryExtractor ddxExtractor) {
            m_bean = new UserDefinedTableB(broker.getPersistenceKey());

            m_bean.setFieldValueByAlias(ALIAS_CREATION_DATE, getCurrentDate(), ddxExtractor.getDictionary(DDX_ID));
            m_bean.setFieldValueByAlias(ALIAS_EXCEPTION_DESCRIPTION, exceptionDescription,
                    ddxExtractor.getDictionary(DDX_ID));
            m_bean.setExtendedDataDictionaryOid(ddxExtractor.getDictionary(DDX_ID).getExtendedDictionaryOid());
            m_bean.setOrganization1Oid(organization.getOid());
        }

        /**
         * Instantiates a new onsis exception.
         *
         * @param broker X2Broker
         * @param organization Organization
         * @param exceptionDescription String
         * @param ddxExtractor DictionaryExtractor
         */
        public OnsisException(X2Broker broker, Organization organization, String schoolOid, String exceptionDescription,
                DictionaryExtractor ddxExtractor) {
            m_bean = new UserDefinedTableB(broker.getPersistenceKey());

            m_bean.setFieldValueByAlias(ALIAS_CREATION_DATE, getCurrentDate(), ddxExtractor.getDictionary(DDX_ID));
            m_bean.setFieldValueByAlias(ALIAS_EXCEPTION_DESCRIPTION, exceptionDescription,
                    ddxExtractor.getDictionary(DDX_ID));
            m_bean.setExtendedDataDictionaryOid(ddxExtractor.getDictionary(DDX_ID).getExtendedDictionaryOid());
            m_bean.setSchoolOid(schoolOid);
            m_bean.setOrganization1Oid(organization.getOid());
        }

        /**
         * Gets the bean.
         *
         * @return User defined table B
         */
        public UserDefinedTableB getBean() {
            return m_bean;
        }
    }

    /**
     * The Class OnsisRecord.
     */
    public static class OnsisRecord {
        interface HeaderErrors {
            public String getAction();

            public String getDescription(ElementsHelper helper);

            public String getKeyFields(ElementsHelper helper);

            public String getKeyValues(ElementsHelper helper);

            public String getPathToElement();

            public String getValueByElementName(String elementName);
        }

        public static class EntityHeaderErrors implements HeaderErrors {
            final private OnsisStateReportEntity m_entity;

            EntityHeaderErrors(OnsisStateReportEntity entity) {
                m_entity = entity;
            }

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisResultsHelper.OnsisRecord.HeaderErrors#getAction()
             */
            @Override
            public String getAction() {
                return m_entity.deepGetFieldValueByElementName(ELEMENT_NAME_ACTION);
            }

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisResultsHelper.OnsisRecord.HeaderErrors#getDescription()
             */
            @Override
            public String getDescription(ElementsHelper helper) {
                List<String> descriptionItems = new ArrayList<>();
                int i = 0;
                LinkedHashMap<String, String> keys = helper.getKeysByPath(getPathToRoot());
                List<String> values = new ArrayList<>();
                for (String keyFieldName : keys.keySet()) {
                    values.add(m_entity.deepGetFieldValueByElementName(keyFieldName));
                }
                for (String keyFieldName : keys.keySet()) {
                    String value = values.get(i);

                    descriptionItems.add(keyFieldName + " = " + value);

                    i++;
                }
                return String.join(", ", descriptionItems);
            }

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisResultsHelper.OnsisRecord.HeaderErrors#getKeyFields()
             */
            @Override
            public String getKeyFields(ElementsHelper helper) {
                LinkedHashMap<String, String> keys = helper.getKeysByPath(getPathToRoot());
                return String.join(", ", keys.keySet());
            }

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisResultsHelper.OnsisRecord.HeaderErrors#getKeyValues()
             */
            @Override
            public String getKeyValues(ElementsHelper helper) {
                LinkedHashMap<String, String> keys = helper.getKeysByPath(getPathToRoot());
                List<String> values = new ArrayList<>();
                for (String keyFieldName : keys.keySet()) {
                    values.add(m_entity.deepGetFieldValueByElementName(keyFieldName));
                }
                return String.join(", ", values);
            }

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisResultsHelper.OnsisRecord.HeaderErrors#getPathToElement()
             */
            @Override
            public String getPathToElement() {
                List<String> reversedPath = new ArrayList<>(getPathToRoot());
                Collections.reverse(reversedPath);
                return String.join(ELEMENTS_DELIMITER, reversedPath);
            }

            @Override
            public String getValueByElementName(String elementName) {
                return m_entity.deepGetFieldValueByElementName(elementName);
            }

            /**
             * Gets the path to root.
             *
             * @return List
             */
            private List<String> getPathToRoot() {
                List<String> pathToRoot = new ArrayList<>();
                {
                    OnsisStateReportEntity currentEntity = m_entity;
                    while (currentEntity.getReportData().getParentEntity() != null) {
                        if (s_submissionsTypes.contains(currentEntity.getElementName())) {
                            break;
                        }
                        pathToRoot.add(currentEntity.getElementName());
                        currentEntity = currentEntity.getReportData().getParentEntity();
                    }
                }
                return pathToRoot;
            }
        }

        public static class NodeHeaderErrors implements HeaderErrors {
            final private Node m_node;

            NodeHeaderErrors(Node node) {
                m_node = node;
            }

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisResultsHelper.OnsisRecord.HeaderErrors#getAction()
             */
            @Override
            public String getAction() {
                return OnsisResultsHelper.getValueByElementName(m_node, ELEMENT_NAME_ACTION);
            }

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisResultsHelper.OnsisRecord.HeaderErrors#getDescription()
             */
            @Override
            public String getDescription(ElementsHelper helper) {
                List<String> descriptionItems = new ArrayList<>();
                int i = 0;
                LinkedHashMap<String, String> keys = helper.getKeysByPath(getPathToRoot());
                List<String> values = new ArrayList<>();
                for (String keyFieldName : keys.keySet()) {
                    values.add(OnsisResultsHelper.getValueByElementName(m_node, keyFieldName));
                }
                for (String keyFieldName : keys.keySet()) {
                    String value = values.get(i);

                    descriptionItems.add(keyFieldName + " = " + value);

                    i++;
                }

                String description = String.join(", ", descriptionItems);

                description += "\n\n" + m_node.getNodeName() + "\n";
                descriptionItems.clear();

                NodeList childNodes = m_node.getChildNodes();
                for (int n = 0; n < childNodes.getLength(); ++n) {
                    Node childNode = childNodes.item(n);

                    String nodeName = childNode.getNodeName();
                    String nodeValue = OnsisResultsHelper.getValueByElementName(childNode, nodeName);
                    if (!StringUtils.isBlank(nodeValue)) {
                        descriptionItems.add(nodeName + " = " + nodeValue);
                    }
                }

                description += String.join(", ", descriptionItems);

                return description;
            }

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisResultsHelper.OnsisRecord.HeaderErrors#getKeyFields()
             */
            @Override
            public String getKeyFields(ElementsHelper helper) {
                LinkedHashMap<String, String> keys = helper.getKeysByPath(getPathToRoot());

                if (keys == null) {
                    String message = "KeysByPath not found. "
                            + "May indicate a tag that should be a child export. PathToRoot: " + getPathToRoot();
                    throw new RuntimeException(message);
                }

                return String.join(", ", keys.keySet());
            }

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisResultsHelper.OnsisRecord.HeaderErrors#getKeyValues()
             */
            @Override
            public String getKeyValues(ElementsHelper helper) {
                LinkedHashMap<String, String> keys = helper.getKeysByPath(getPathToRoot());
                List<String> values = new ArrayList<>();
                for (String keyFieldName : keys.keySet()) {
                    values.add(OnsisResultsHelper.getValueByElementName(m_node, keyFieldName));
                }
                return String.join(", ", values);
            }

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisResultsHelper.OnsisRecord.HeaderErrors#getPathToElement()
             */
            @Override
            public String getPathToElement() {
                List<String> reversedPath = new ArrayList<>(getPathToRoot());
                Collections.reverse(reversedPath);
                return String.join(ELEMENTS_DELIMITER, reversedPath);
            }

            @Override
            public String getValueByElementName(String elementName) {
                return OnsisResultsHelper.getValueByElementName(m_node, elementName);
            }

            private List<String> getPathToRoot() {
                List<String> pathToRoot = new ArrayList<>();
                {
                    Node currentNode = m_node;
                    while (currentNode.getParentNode() != null) {
                        if (s_submissionsTypes.contains(currentNode.getNodeName())) {
                            break;
                        }
                        pathToRoot.add(currentNode.getNodeName());
                        currentNode = currentNode.getParentNode();
                    }
                }
                return pathToRoot;
            }
        }
        /**
         * The Enum CLASS_TO_OID.
         */
        private enum CLASS_TO_OID {
            SCHOOL(SisSchool.class, UserDefinedTableB.COL_SCHOOL_OID),
            //
            STAFF(SisStaff.class, UserDefinedTableB.COL_STAFF_OID),
            //
            STUDENT(SisStudent.class, UserDefinedTableB.COL_STUDENT_OID),
            //
            MASTER_SCHEDULE(MasterSchedule.class, UserDefinedTableB.COL_MASTER_SCHEDULE_OID);

            private Class m_class;
            private String m_fieldName;

            /**
             * Instantiates a new class to oid.
             *
             * @param clazz Class
             * @param fieldName String
             */
            private CLASS_TO_OID(Class clazz, String fieldName) {
                m_class = clazz;
                m_fieldName = fieldName;
            }

            /**
             * Gets the field name.
             *
             * @param clazz Class
             * @return String
             */
            public static String getFieldName(Class clazz) {
                for (CLASS_TO_OID classToOid : values()) {
                    if (classToOid.m_class.equals(clazz)) {
                        return classToOid.m_fieldName;
                    }
                }
                return null;
            }
        }

        private static List<Class> s_classesToSetOid =
                Arrays.<Class>asList(SisSchool.class, SisStaff.class, SisStudent.class, MasterSchedule.class);

        public static final String DDX_ID = "ON-SIS-RECORD";

        private static final String ALIAS_ACTION = "action";
        private static final String ALIAS_CREATION_DATE = "creation-date";
        private static final String ALIAS_DESCRIPTION = "description";
        private static final String ALIAS_KEY_FIELDS = "key-fields";
        private static final String ALIAS_KEY_FIELDS_VALUES = "key-fields-values";
        private static final String ALIAS_PATH_TO_ELEMENT = "path-to-element";
        private static final String ALIAS_STATUS = "status";
        private static final String ALIAS_SUMBMISSION_DESC = "submission-description";

        private final UserDefinedTableB m_bean;
        private final OnsisResult m_result;
        private String m_stringRepresention;

        /**
         * Instantiates a new onsis record.
         *
         * @param headerErrors
         *
         * @param result OnsisResult
         */
        public OnsisRecord(HeaderErrors headerErrors, OnsisResult result) {
            m_bean = new UserDefinedTableB(result.getBroker().getPersistenceKey());

            m_result = result;

            m_bean.setFieldValueByAlias(ALIAS_ACTION, headerErrors.getAction(),
                    result.getDictionary(DDX_ID));

            m_bean.setFieldValueByAlias(ALIAS_PATH_TO_ELEMENT, headerErrors.getPathToElement(),
                    result.getDictionary(DDX_ID));

            m_bean.setFieldValueByAlias(ALIAS_KEY_FIELDS, headerErrors.getKeyFields(m_result.getElementsHelper()),
                    result.getDictionary(DDX_ID));

            m_bean.setFieldValueByAlias(ALIAS_KEY_FIELDS_VALUES,
                    headerErrors.getKeyValues(m_result.getElementsHelper()),
                    result.getDictionary(DDX_ID));

            m_bean.setFieldValueByAlias(ALIAS_DESCRIPTION, headerErrors.getDescription(m_result.getElementsHelper()),
                    result.getDictionary(DDX_ID));

            m_bean.setFieldValueByAlias(ALIAS_CREATION_DATE,
                    result.getFieldValueByAlias(OnsisResult.ALIAS_CREATION_DATE),
                    result.getDictionary(DDX_ID));
            m_bean.setFieldValueByAlias(ALIAS_SUMBMISSION_DESC,
                    result.getFieldValueByAlias(OnsisResult.ALIAS_DESCRIPTION),
                    result.getDictionary(DDX_ID));

            m_bean.setFieldValueByBeanPath(UserDefinedTableB.COL_EXTENDED_DATA_DICTIONARY_OID,
                    result.getDictionary(DDX_ID).getExtendedDictionaryOid());

            m_bean.setOrganization1Oid(result.getBean().getOrganization1Oid());

            setOidsIfPossible(headerErrors);

            m_result.setSchoolOid(headerErrors);
        }

        /**
         * Instantiates a new onsis record.
         *
         * @param result OnsisResult
         * @param bean UserDefinedTableB
         */
        public OnsisRecord(OnsisResult result, UserDefinedTableB bean) {
            m_result = result;
            m_bean = bean;
        }

        /**
         * Gets the bean.
         *
         * @return User defined table B
         */
        public UserDefinedTableB getBean() {
            return m_bean;
        }

        /**
         * Gets the field value by alias.
         *
         * @param alias String
         * @return Object
         */
        public Object getFieldValueByAlias(String alias) {
            return m_bean.getFieldValueByAlias(alias, m_result.getDictionary(DDX_ID));
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            if (m_stringRepresention == null) {
                StringBuilder builder = new StringBuilder();
                builder.append(getFieldValueByAlias(ALIAS_PATH_TO_ELEMENT));
                builder.append(", ");
                builder.append(getFieldValueByAlias(ALIAS_DESCRIPTION));
                m_stringRepresention = builder.toString();
            }
            return m_stringRepresention;
        }

        /**
         * Update status.
         *
         * @param status STATUS
         */
        public void updateStatus(OnsisResult.STATUS status) {
            m_bean.setFieldValueByAlias(ALIAS_STATUS, status.toString(), m_result.getDictionary(DDX_ID));
            m_result.getBroker().saveBean(m_bean);
        }

        /**
         * Before save.
         *
         * @see com.follett.fsc.core.k12.beans.X2BaseBean#beforeSave(com.follett.fsc.core.k12.beans.BeanBroker)
         */
        protected void beforeSave() {
            m_bean.setFieldValueByBeanPath(UserDefinedTableB.COL_USER_DEFINED_TABLE_A_OID, m_result.getBean().getOid());
        }

        /**
         * Adds the error.
         *
         * @param broker X2Broker
         * @param dictionary DataDictionary
         * @param headerErrors headerErrors
         * @param errorParser ErrorParser
         * @return OnsisError
         */
        private OnsisError addError(X2Broker broker,
                                    DataDictionary dictionary,
                                    HeaderErrors headerErrors,
                                    OnsisResult.ErrorParser errorParser) {
            OnsisError error = createError(broker, dictionary, headerErrors, errorParser);
            broker.saveBean(createError(broker, dictionary, headerErrors, errorParser).m_bean);
            return error;
        }

        /**
         * Adds the error.
         *
         * @param broker X2Broker
         * @param error OnsisError
         * @return OnsisError
         */
        private OnsisError addError(X2Broker broker, OnsisError error) {
            broker.saveBean(error.m_bean);
            return error;
        }

        /**
         * Creates the error.
         *
         * @param broker X2Broker
         * @param dictionary DataDictionary
         * @param headerErrors HeaderErrors
         * @param errorParser ErrorParser
         * @return OnsisError
         */
        private OnsisError createError(X2Broker broker,
                                       DataDictionary dictionary,
                                       HeaderErrors headerErrors,
                                       OnsisResult.ErrorParser errorParser) {
            return new OnsisError(broker, dictionary, headerErrors, errorParser, this);
        }

        /**
         * Sets the oids if possible.
         *
         * @param node void
         */
        private void setOidsIfPossible(HeaderErrors headerErrors) {
            for (Class clazz : s_classesToSetOid) {
                String oid = m_result.getGroupOid(clazz, headerErrors);
                if (oid != null) {
                    m_bean.setFieldValueByBeanPath(CLASS_TO_OID.getFieldName(clazz), oid);
                }
            }
        }
    }

    /**
     * The Class OnsisResult.
     */
    public static class OnsisResult {

        /**
         * The Interface ErrorParser.
         */
        public interface ErrorParser {

            /**
             * Parses the to bean.
             *
             * @param bean X2BaseBean
             * @param headerErrors
             * @param dictionary DataDictionary
             */
            public void parseToBean(X2BaseBean bean, HeaderErrors headerErrors, DataDictionary dictionary);
        }

        public static final String DDX_ID = "ON-SIS-RESULT";

        /**
         * The Enum STATUS.
         */
        public enum STATUS {
            NEW, ERROR, SUCCESS;
        }

        static final String ALIAS_CREATION_DATE = "creation-date";
        static final String ALIAS_DESCRIPTION = "description";
        static final String ALIAS_SECTION = "section";
        static final String ALIAS_STATUS = "status";

        private UserDefinedTableA m_bean;
        private X2Broker m_broker;
        private Map<String, DataDictionary> m_dictionariesById = new HashMap<>();
        private ElementsHelper m_elementsHelper;
        private Filterable<ExportFormatField> m_fieldsHelper;
        private Map<String, Filterable<X2BaseBean>> m_filterables;
        private List<OnsisRecord> m_records = new ArrayList<>();
        private String m_schoolOid;

        /**
         * Instantiates a new onsis result.
         */
        protected OnsisResult() {
            // forbid OnsisResult direct creation
        }

        /**
         * Adds the error.
         *
         * @param record OnsisRecord
         * @param headerErrors HeaderErrors
         * @param errorParser ErrorParser
         * @return OnsisError
         */
        public OnsisError addError(OnsisRecord record, HeaderErrors headerErrors, ErrorParser errorParser) {
            return record.addError(m_broker, getDictionary(OnsisError.DDX_ID), headerErrors, errorParser);
        }

        /**
         * Adds the error.
         *
         * @param record OnsisRecord
         * @param error OnsisError
         * @return OnsisError
         */
        public OnsisError addError(OnsisRecord record, OnsisError error) {
            return record.addError(m_broker, error);
        }

        /**
         * Adds the record.
         *
         * @param header
         *
         * @return OnsisRecord
         */
        public OnsisRecord addRecord(HeaderErrors header) {
            OnsisRecord newRecord = new OnsisRecord(header, this);
            m_records.add(newRecord);
            return newRecord;
        }

        /**
         * Adds the record.
         *
         * @param entity
         *
         * @return OnsisRecord
         */
        public OnsisRecord addRecord(OnsisStateReportEntity entity) {
            OnsisRecord newRecord = new OnsisRecord(new OnsisRecord.EntityHeaderErrors(entity), this);
            m_records.add(newRecord);
            return newRecord;
        }

        /**
         * Before save.
         */
        public void beforeSave() {
            if (m_schoolOid != null) {
                m_bean.setFieldValueByBeanPath(UserDefinedTableA.COL_SCHOOL_OID, m_schoolOid);
            }
        }

        /**
         * Creates the result.
         *
         * @param organization Organization
         * @param broker X2Broker
         * @param description String
         * @param elementsHelper ElementsHelper
         * @return OnsisResult
         */
        public static OnsisResult createResult(Organization organization,
                                               X2Broker broker,
                                               String description,
                                               ElementsHelper elementsHelper) {
            return createResult(organization, broker, description, elementsHelper, OnsisResult.class);
        }

        /**
         * Adds the error.
         *
         * @param record OnsisRecord
         * @param headerErrors HeaderErrors
         * @param errorParser ErrorParser
         * @return OnsisError
         */
        public OnsisError createError(OnsisRecord record, HeaderErrors headerErrors, ErrorParser errorParser) {
            return record.createError(m_broker, getDictionary(OnsisError.DDX_ID), headerErrors, errorParser);
        }

        /**
         * Creates the result.
         *
         * @param organization Organization
         * @param broker X2Broker
         * @param description String
         * @param elementsHelper ElementsHelper
         * @param resultClass Class
         * @return OnsisResult
         */
        public static OnsisResult createResult(Organization organization,
                                               X2Broker broker,
                                               String description,
                                               ElementsHelper elementsHelper,
                                               Class resultClass) {
            OnsisResult newResult = null;
            try {
                newResult = (OnsisResult) resultClass.getDeclaredConstructor().newInstance();
                newResult.m_broker = broker;
                newResult.m_bean = new UserDefinedTableA(broker.getPersistenceKey());

                newResult.m_bean.setFieldValueByAlias(ALIAS_CREATION_DATE, getCurrentDate(),
                        newResult.getDictionary(DDX_ID));
                newResult.m_bean.setFieldValueByAlias(ALIAS_DESCRIPTION, description, newResult.getDictionary(DDX_ID));
                newResult.updateStatus(STATUS.NEW);
                newResult.m_bean
                        .setExtendedDataDictionaryOid(newResult.getDictionary(DDX_ID).getExtendedDictionaryOid());

                newResult.m_bean.setOrganization1Oid(organization.getOid());

                newResult.m_elementsHelper = elementsHelper;
            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
                    | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                throw new X2RuntimeException(e);
            }

            return newResult;
        }

        /**
         * Find result by oid.
         *
         * @param broker X2Broker
         * @param resultOid String
         * @param elementsHelper ElementsHelper
         * @return OnsisResult
         */
        public static OnsisResult findResultByOid(X2Broker broker, String resultOid, ElementsHelper elementsHelper) {
            OnsisResult onsisResult = new OnsisResult();
            onsisResult.m_broker = broker;
            onsisResult.m_bean = broker.getBeanByOid(UserDefinedTableA.class, resultOid);

            X2Criteria recordsCriteria = new X2Criteria();
            recordsCriteria.addEqualTo(UserDefinedTableB.REL_USER_DEFINED_TABLE_A, resultOid);

            Collection<UserDefinedTableB> recordBeans = broker
                    .getCollectionByQuery(new QueryByCriteria(UserDefinedTableB.class, recordsCriteria));
            List<OnsisRecord> onsisRecords = new ArrayList<>();
            for (UserDefinedTableB recordBean : recordBeans) {
                onsisRecords.add(new OnsisRecord(onsisResult, recordBean));
            }
            onsisResult.m_records = onsisRecords;

            onsisResult.m_elementsHelper = elementsHelper;

            return onsisResult;
        }

        /**
         * Gets the bean.
         *
         * @return User defined table A
         */
        public UserDefinedTableA getBean() {
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

        /**
         * Gets the elements helper.
         *
         * @return Exsms elements helper
         */
        public ElementsHelper getElementsHelper() {
            return m_elementsHelper;
        }


        /**
         * Gets the field value by alias.
         *
         * @param alias String
         * @return Object
         */
        public Object getFieldValueByAlias(String alias) {
            return m_bean.getFieldValueByAlias(alias, getDictionary(DDX_ID));
        }

        /**
         * Gets the records.
         *
         * @return List
         */
        public List<OnsisRecord> getRecords() {
            return m_records;
        }

        /**
         * Sets the school oid.
         *
         * @param header
         *
         */
        public void setSchoolOid(HeaderErrors header) {
            if (m_schoolOid == null) {
                m_schoolOid = getGroupOid(SisSchool.class, header);
            }
        }

        /**
         * Sets the school oid.
         *
         * @param oid void
         */
        public void setSchoolOid(String oid) {
            m_schoolOid = oid;
        }

        /**
         * Update status.
         *
         * @param status STATUS
         */
        public void updateStatus(STATUS status) {
            m_bean.setFieldValueByAlias(ALIAS_STATUS, status.toString(),
                    getDictionary(DDX_ID));
            if (STATUS.SUCCESS.equals(status)) {
                for (OnsisRecord record : getRecords()) {
                    record.updateStatus(status);
                }
            }
            m_broker.saveBean(m_bean);
        }

        /**
         * Gets the fields helper.
         *
         * @return Filterable
         */
        private Filterable<ExportFormatField> getFieldsHelper() {
            if (m_fieldsHelper == null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addBeginsWith(ExportFormatField.REL_DEFINITION + ModelProperty.PATH_DELIMITER
                        + ExportFormatDefinition.COL_PROCEDURE_ID, OnsisConstants.FORMAT_PREFIX_ONSIS);
                m_fieldsHelper = FilterableFactory.create(getBroker(), ExportFormatField.class, criteria);
            }
            return m_fieldsHelper;
        }

        /**
         * Gets the filterable.
         *
         * @param clazz Class
         * @param criteria X2Criteria
         * @return Filterable
         */
        private Filterable<X2BaseBean> getFilterable(Class clazz, X2Criteria criteria) {
            if (m_filterables == null) {
                m_filterables = new HashMap<>();
            }
            String filterableKey = clazz.getName() + criteria.toString();
            Filterable filterable = m_filterables.get(filterableKey);
            if (filterable == null) {
                filterable = FilterableFactory.create(getBroker(), clazz, criteria);
                m_filterables.put(filterableKey, filterable);
            }
            return filterable;
        }

        /**
         * Gets the group oid.
         *
         * @param clazz Class
         * @param node Node
         * @return String
         */
        protected String getGroupOid(Class clazz, HeaderErrors header) {
            IDENTIFYING_FIELD identifyingField = IDENTIFYING_FIELD.getByClass(clazz);
            String idElementValue = header.getValueByElementName(identifyingField.toString());
            if (StringUtils.isEmpty(idElementValue)) {
                // cannot be grouped for this class
                return null;
            }

            Collection<ExportFormatField> fields =
                    getFieldsHelper().filter(ExportFormatField.COL_SIF_PATH, identifyingField.toString()).extract();
            String fieldName = null;
            for (ExportFormatField field : fields) {
                ExportFormatManager.validateField(field, true, false, getBroker());
                String fieldPath = field.getValFieldPath();
                if (fieldPath != null) {
                    fieldName =
                            fieldPath.substring(fieldPath.lastIndexOf(ModelProperty.PATH_DELIMITER) + 1,
                                    fieldPath.length());
                    if (!StringUtils.isEmpty(fieldName)) {
                        break;
                    }
                }
            }
            X2Criteria criteria = identifyingField.getCriteria(fieldName, idElementValue);
            if (criteria == null) {
                return null;
            }
            X2BaseBean entityWithId = getFilterable(clazz, criteria).extractFirst();
            return entityWithId == null ? null : entityWithId.getOid();
        }
    }

    /**
     * The Class UdtHelper.
     */
    public static class UdtHelper {
        private static final List<Class> s_classes = Arrays.asList(UserDefinedTableA.class, UserDefinedTableB.class,
                UserDefinedTableC.class, UserDefinedTableD.class);

        /**
         * Delete cascade A.
         *
         * @param broker X2Broker
         * @param udtA UserDefinedTableA
         */
        public static void deleteCascadeA(X2Broker broker, UserDefinedTableA udtA) {
            deleteCascade(broker, udtA);
        }

        /**
         * Delete cascade B.
         *
         * @param broker X2Broker
         * @param udtB UserDefinedTableB
         */
        public static void deleteCascadeB(X2Broker broker, UserDefinedTableB udtB) {
            deleteCascade(broker, udtB);
        }

        /**
         * Delete cascade C.
         *
         * @param broker X2Broker
         * @param udtC UserDefinedTableC
         */
        public static void deleteCascadeC(X2Broker broker, UserDefinedTableC udtC) {
            deleteCascade(broker, udtC);
        }

        /**
         * Delete cascade D.
         *
         * @param broker X2Broker
         * @param udtD UserDefinedTableD
         */
        public static void deleteCascadeD(X2Broker broker, UserDefinedTableD udtD) {
            deleteCascade(broker, udtD);
        }

        /**
         * Delete cascade.
         *
         * @param broker X2Broker
         * @param udt X2BaseBean
         */
        private static void deleteCascade(X2Broker broker, X2BaseBean udt) {
            String pathPrefix = getPathPrefix(udt);
            List<Class> classes = getClasses(udt.getClass());

            broker.deleteBean(udt);

            getQueriesToDelete(classes, pathPrefix, udt.getOid()).stream()
                    .forEach(query -> broker.deleteByQuery(query));
        }

        /**
         * Gets the classes.
         *
         * @param clazzToEclude Class
         * @return List
         */
        private static List<Class> getClasses(Class clazzToEclude) {
            return s_classes.stream().filter(clazz -> !clazz.equals(clazzToEclude)).collect(Collectors.toList());
        }

        /**
         * Gets the path prefix.
         *
         * @param udt X2BaseBean
         * @return String
         */
        private static String getPathPrefix(X2BaseBean udt) {
            String pathPrefix = null;
            if (udt instanceof UserDefinedTableA) {
                pathPrefix = UserDefinedTableB.REL_USER_DEFINED_TABLE_A;
            }
            if (udt instanceof UserDefinedTableB) {
                pathPrefix = UserDefinedTableA.REL_USER_DEFINED_TABLE_B;
            }
            if (udt instanceof UserDefinedTableC) {
                pathPrefix = UserDefinedTableA.REL_USER_DEFINED_TABLE_C;
            }
            if (udt instanceof UserDefinedTableD) {
                pathPrefix = UserDefinedTableA.REL_USER_DEFINED_TABLE_D;
            }
            return pathPrefix;
        }

        /**
         * Gets the queries to delete.
         *
         * @param classes List<Class>
         * @param pathPrefix String
         * @param parentOid String
         * @return List
         */
        private static List<QueryByCriteria> getQueriesToDelete(List<Class> classes,
                                                                String pathPrefix,
                                                                String parentOid) {
            return classes.stream().map(clazz -> {
                X2Criteria classCriteria = new X2Criteria();
                classCriteria.addEqualTo(pathPrefix + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, parentOid);
                return new QueryByCriteria(clazz, classCriteria);
            }).collect(Collectors.toList());
        }
    }

    /**
     * Dump node.
     *
     * @param node Node
     * @return String
     */
    public static String dumpNode(Node node) {
        String fullPathOfNode = OnsisResultsHelper.getFullPathOfNode(node);
        String message = fullPathOfNode;

        message += "\n" + toString(node);

        return message;
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
     * Gets the full path of node.
     *
     * @param node Node
     * @return String
     */
    public static String getFullPathOfNode(Node node) {
        return getFullPathOfNode(node, null);
    }

    /**
     * Gets the full path of node.
     *
     * @param node Node
     * @param rootNodeName String
     * @return String
     */
    public static String getFullPathOfNode(Node node, String rootNodeName) {
        List<String> path = new ArrayList<>();
        path.add(node.getNodeName());
        Node currentNode = node;
        while (currentNode.getParentNode() != null) {
            currentNode = currentNode.getParentNode();
            if (currentNode.getNodeType() == Node.ELEMENT_NODE) {
                String nodeName = currentNode.getNodeName();
                path.add(nodeName);
                if (rootNodeName != null && rootNodeName.equals(nodeName)) {
                    break;
                }
            }
        }
        Collections.reverse(path);
        return String.join(ELEMENTS_DELIMITER, path);
    }

    /**
     * Gets the nodes by name.
     *
     * @param xmlString String
     * @param nodeName String
     * @return List
     */
    public static List<Node> getNodesByName(String xmlString, final String nodeName) {
        final List<Node> oenDetailsNodes = new ArrayList<>();
        Element validationFileOenOutputXml = OnsisResultsHelper.stringToElement(xmlString);
        OnsisResultsHelper.traverseNodeWithHandler(validationFileOenOutputXml, new NodeHandler() {
            @Override
            public void handleNode(Node node) {
                if (nodeName.equals(node.getNodeName())) {
                    oenDetailsNodes.add(node);
                }
            }
        });
        return getNodesByName(xmlString, nodeName, ENCODING_UTF_8);
    }

    /**
     * @param xmlString
     * @param nodeName
     * @param encoding
     * @param string
     * @param elementNameOen
     * @param encodingWindows1252
     * @return
     */
    public static List<Node> getNodesByName(String xmlString, String nodeName, String encoding) {
        final List<Node> oenDetailsNodes = new ArrayList<>();
        Element validationFileOenOutputXml = OnsisResultsHelper.stringToElement(xmlString, encoding);
        OnsisResultsHelper.traverseNodeWithHandler(validationFileOenOutputXml, new NodeHandler() {
            @Override
            public void handleNode(Node node) {
                if (nodeName.equals(node.getNodeName())) {
                    oenDetailsNodes.add(node);
                }
            }
        });
        return oenDetailsNodes;
    }

    /**
     * Gets the nodes by path.
     *
     * @param element Element
     * @param pathEndsTo String
     * @return List
     */
    public static List<Node> getNodesByPath(Element element, String pathEndsTo) {
        final List<Node> nodesWithPath = new ArrayList<>();
        OnsisResultsHelper.traverseNodeWithHandler(element, new NodeHandler() {
            @Override
            public void handleNode(Node node) {
                if (node.getNodeType() == Node.ELEMENT_NODE) {
                    String fullPath = OnsisResultsHelper.getFullPathOfNode(node);
                    try {
                        if (fullPath.endsWith(pathEndsTo)) {
                            nodesWithPath.add(node);
                        }
                    } catch (Exception e) {
                        System.out.println();
                    }

                }
            }
        });
        return nodesWithPath;
    }

    /**
     * Gets the outer node.
     *
     * @param node Node
     * @param field OnsisElement
     * @return Node
     */
    public static Node getOuterNode(Node node, OnsisElement field) {
        Node parentNode = node.getParentNode();
        while (parentNode != null) {
            if (field.toString().equals(parentNode.getNodeName())) {
                return parentNode;
            }
            parentNode = parentNode.getParentNode();
        }
        throw new RuntimeException(
                "Outer node with name " + field.toString() + " for node " + node.getNodeName() + " is not found");
    }

    /**
     * Gets the outer node.
     *
     * @param node Node
     * @param elementName String
     * @return Optional
     */
    public static Optional<Node> getOuterNode(Node node, String elementName) {
        Node parentNode = node.getParentNode();
        while (parentNode != null) {
            if (elementName.equals(parentNode.getNodeName())) {
                return Optional.of(parentNode);
            }
            parentNode = parentNode.getParentNode();
        }
        return Optional.empty();
    }

    /**
     * Gets the value by element name.
     *
     * @param node Node
     * @param elementName String
     * @return String
     */
    public static String getValueByElementName(Node node, String elementName) {
        if (elementName.equals(node.getNodeName())) {
            NodeList innerChildren = node.getChildNodes();
            if (innerChildren.getLength() > 0) {
                return innerChildren.item(0).getNodeValue();
            }
            return node.getNodeValue();
        }

        Node currentNode = node;
        while (currentNode.getParentNode() != null) {
            for (int i = 0; i < currentNode.getChildNodes().getLength(); i++) {
                Node currentNodeValue = currentNode.getChildNodes().item(i);
                if (currentNodeValue.getNodeName().equals(elementName)) {
                    NodeList innerChildren = currentNodeValue.getChildNodes();
                    if (innerChildren.getLength() > 0) {
                        return innerChildren.item(0).getNodeValue();
                    }
                    return currentNodeValue.getNodeValue();
                }
            }
            currentNode = currentNode.getParentNode();
        }
        return null;
    }

    /**
     * Gets the value by element name.
     *
     * @param node Node
     * @param field OnsisElement
     * @return String
     */
    public static String getValueByElementName(Node node, OnsisElement field) {
        return getValueByElementName(node, field.toString());
    }

    public static String getValueByElementPath(Node node, String path) {
        if (OnsisResultsHelper.getFullPathOfNode(node).endsWith(path)) {
            NodeList innerChildren = node.getChildNodes();
            if (innerChildren.getLength() > 0) {
                return innerChildren.item(0).getNodeValue();
            }
            return node.getNodeValue();
        }

        Node currentNode = node;
        while (currentNode.getParentNode() != null) {
            for (int i = 0; i < currentNode.getChildNodes().getLength(); i++) {
                Node currentNodeValue = currentNode.getChildNodes().item(i);
                if (OnsisResultsHelper.getFullPathOfNode(currentNodeValue).endsWith(path)) {
                    NodeList innerChildren = currentNodeValue.getChildNodes();
                    if (innerChildren.getLength() > 0) {
                        return innerChildren.item(0).getNodeValue();
                    }
                    return currentNodeValue.getNodeValue();
                }
            }
            currentNode = currentNode.getParentNode();
        }
        return null;
    }

    /**
     * Save result.
     *
     * @param broker X2Broker
     * @param result OnsisResult
     */
    public static void saveResult(X2Broker broker, OnsisResult result) {
        result.beforeSave();
        if (result.getRecords() != null && !result.getRecords().isEmpty()) {
            broker.saveBean(result.getBean());
            for (OnsisRecord record : result.getRecords()) {
                record.beforeSave();
                broker.saveBean(record.getBean());
            }
        }
    }

    /**
     * Save result.
     *
     * @param organization Organization
     * @param broker X2Broker
     * @param description String
     * @param exceptions Map<String,Set<String>>
     * @param extractor DictionaryExtractor
     * @return OnsisResult
     */
    public static OnsisResult saveResult(Organization organization,
                                         X2Broker broker,
                                         String description,
                                         Map<String, Set<ResultException>> exceptions,
                                         DictionaryExtractor extractor) {
        boolean isError = false;
        OnsisResult result = null;
        try {
            broker.beginTransaction();
            Filterable<ExportFormatDefinition> formats =
                    FilterableFactory.create(broker, ExportFormatDefinition.class);
            ElementsHelper elementsHelper =
                    new ExsmsElementsHelper(broker, new ArrayList<ExportFormatDefinition>(formats.extract()));
            result = OnsisResult.createResult(organization, broker, description, elementsHelper);
            saveResult(broker, result);
            storeExceptions(broker, organization, exceptions, result, extractor);
        } catch (Exception e) {
            isError = true;
            throw e;
        } finally {
            if (isError) {
                broker.rollbackTransaction();
            } else {
                broker.commitTransaction();
            }
        }
        return result;
    }

    /**
     * Store exceptions.
     *
     * @param broker X2Broker
     * @param organization Organization
     * @param resultExceptions Map<String,Set<String>>
     * @param result OnsisResult
     * @param extractor DictionaryExtractor
     */
    public static void storeExceptions(X2Broker broker,
                                       Organization organization,
                                       Map<String, Set<ResultException>> resultExceptions,
                                       OnsisResult result,
                                       DictionaryExtractor extractor) {
        List<OnsisException> exceptions = new ArrayList<>();
        for (Entry<String, Set<ResultException>> exceptionsByGroup : resultExceptions.entrySet()) {
            Set<ResultException> exceptionsItself = exceptionsByGroup.getValue();
            for (ResultException oneOfExceptions : exceptionsItself) {
                OnsisException exception = new OnsisException(broker, organization, oneOfExceptions.getSchoolOid(),
                        exceptionsByGroup.getKey() + "\n\n" + oneOfExceptions.getMsg(), extractor);
                exception.getBean().setUserDefinedTableAOid(result.getBean().getOid());
                exceptions.add(exception);
            }
        }
        for (OnsisException exception : exceptions) {
            result.getBroker().saveBean(exception.getBean());
        }
    }

    /**
     * String to element.
     *
     * @param stringRepresentation String
     * @return Element
     */
    public static Element stringToElement(String stringRepresentation) {
        return stringToElement(stringRepresentation, ENCODING_UTF_8);
    }

    /**
     * String to element.
     *
     * @param stringRepresentation String
     * @param encoding String
     * @return Element
     */
    public static Element stringToElement(String stringRepresentation, String encoding) {
        try {
            InputSource is = new InputSource(new StringReader(stringRepresentation));
            is.setEncoding(encoding);
            return getNormalizedElement(DocumentBuilderFactory
                    .newInstance()
                    .newDocumentBuilder()
                    .parse(is));
        } catch (Exception e) {
            throw new X2RuntimeException(e);
        }
    }

    /**
     * To string.
     *
     * @param node Node
     * @return String
     */
    // public static String toString(Node node) {
    // StringWriter writer = new StringWriter();
    // try {
    // TransformerFactory tf = TransformerFactory.newInstance();
    // Transformer transformer = tf.newTransformer();
    // transformer.setOutputProperty(OutputKeys.INDENT, "yes");
    //
    // transformer.transform(new DOMSource(node), new StreamResult(writer));
    // } catch (TransformerException e) {
    // writer.append(LoggerUtils.convertThrowableToString(e));
    // }
    // return writer.toString();
    // }


    /**
     * Pretty print.
     *
     * @param node Node
     * @return String
     */
    public static String toString(Node node) {
        return toString(node, "");
    }

    /**
     * Pretty print.
     *
     * @param node Node
     * @param indent String
     * @return String
     */
    public static String toString(Node node, String indent) {
        final String incrIndent = "   ";

        if (node == null) {
            return null;
        }

        String output = "";
        indent = StringUtils.emptyIfNull(indent);

        if (node.getNodeType() == Node.TEXT_NODE) {
            output += node.getNodeValue();
        } else if (node.getNodeType() == Node.ELEMENT_NODE) {
            /*
             * Open tag
             */
            output += indent;
            output += "<" + node.getNodeName() + ">";
            NodeList childNodes = node.getChildNodes();
            boolean wroteChildren = false;

            /*
             * Children / text content
             */
            for (int i = 0; i < childNodes.getLength(); i++) {
                Node childNode = childNodes.item(i);

                if (childNode.getNodeType() == Node.ELEMENT_NODE) {
                    /*
                     * Element children are written indented and recursively
                     */
                    output += "\n";
                    output += toString(childNode, indent + incrIndent);
                    wroteChildren = true;
                } else {
                    /*
                     * Text children are written inline
                     */
                    output += childNode.getNodeValue();
                }
            }

            /*
             * Close tag
             */
            if (wroteChildren) {
                output += "\n";
                output += indent;
            }
            output += "</" + node.getNodeName() + ">";
        }

        return output;
    }

    /**
     * Traverse node with handler.
     *
     * @param node Node
     * @param nodeHandler NodeHandler
     */
    public static void traverseNodeWithHandler(Node node, NodeHandler nodeHandler) {
        nodeHandler.handleNode(node);
        if (node.getChildNodes().getLength() > 0) {
            for (int i = 0; i < node.getChildNodes().getLength(); i++) {
                traverseNodeWithHandler(node.getChildNodes().item(i), nodeHandler);
            }
        }
    }

    /**
     * Gets the normalized element.
     *
     * @param doc Document
     * @return Element
     * @throws XPathExpressionException exception
     */
    private static Element getNormalizedElement(Document doc) throws XPathExpressionException {
        XPath xPath = XPathFactory.newInstance().newXPath();

        removeEmptyTextNodes(xPath, doc);
        Element element = doc.getDocumentElement();
        element.normalize();
        return element;

    }

    /**
     * Remove all empty text nodes.
     *
     * @param xPath XPath
     * @param document Document
     * @throws XPathExpressionException exception
     */
    private static void removeEmptyTextNodes(XPath xPath, Document document) throws XPathExpressionException {
        String expression = "//text()[normalize-space(.)='']";
        NodeList emptyTextNodes = (NodeList) xPath.compile(expression).evaluate(document, XPathConstants.NODESET);
        for (int i = 0; i < emptyTextNodes.getLength(); i++) {
            Node emptyTextNode = emptyTextNodes.item(i);
            emptyTextNode.getParentNode().removeChild(emptyTextNode);
        }
    }
}
