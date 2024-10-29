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
package com.x2dev.procedures.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.portability.ObjectImporter;
import com.follett.fsc.core.k12.business.portability.Portable;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.template.TemplateConstants;
import com.x2dev.utils.StringUtils;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * The Class IepConfigurationSetupProcedure.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class IepConfigurationSetupProcedure extends ProcedureJavaSource {
    private static final String ALIAS_REPORT_FIELD_SCRIPTS = "ora-ma-rpt-field-scripts";
    private static final String PORTABLE_DOCUMENT = "PortableDocument";
    private static final String PROCEDURE_ID = "procedureId";
    private static final String REPORT_FIELD = "report-field";
    private static final String REPORT_FIELD_NAME_ATTRIB = "name";
    private static final String REPORT_FIELDS = "report-fields";
    private static final String VIEW_TEMPLATE = "view-template";

    private static final String CALCULATED_FIELD = "calculated-field";
    private static final String CALCULATED_FIELD_NAME = "name";
    private static final String CALCULATED_FIELD_DESCRIPTION = "description";
    private static final String CALCULATED_FIELD_CALCULATED_EXPRESSION = "calculated-expression";
    private static final String CALCULATED_FIELD_PROCEDURE_ID = "procedure-id";
    private static final String CALCULATED_FIELD_FIELD_OID = "field-oid";
    private static final String CALCULATED_FIELD_FIELD_ALIAS = "field-alias";

    private static final String DATA_FIELD_CONFIG = "data-field-config";
    private static final String DATA_FIELD_CONFIG_TABLE_ID = "id";
    private static final String DATA_FIELD_CONFIG_TABLE_ATTR = "table";
    private static final String DATA_FIELD_CONFIG_FIELD_ATTR = "field";
    private static final String DATA_FIELD_CONFIG_LONG_NAME_ATTR = "long-name";
    private static final String DATA_FIELD_CONFIG_SHORT_NAME_ATTR = "short-name";
    private static final String DATA_FIELD_CONFIG_LENGTH_ATTR = "length";
    private static final String DATA_FIELD_CONFIG_SEQUENCE_NUMBER_ATTR = "sequence-number";
    private static final String DATA_FIELD_CONFIG_ENABLED_ATTR = "enabled";
    private static final String DATA_FIELD_CONFIG_LIST_EDIT_ATTR = "list-edit";
    private static final String DATA_FIELD_CONFIG_READ_ONLY_ATTR = "read-only";
    private static final String DATA_FIELD_CONFIG_REQUIRED_ATTR = "required";
    private static final String DATA_FIELD_CONFIG_UPDATE_ATTR = "update";
    private static final String DATA_FIELD_CONFIG_ALIAS_ATTR = "alias";
    private static final String DATA_FIELD_CONFIG_TYPE_ATTR = "type";

    private static final String WORK_FLOW_PHASE = "workflow-phase";
    private static final String WORK_FLOW_PHASE_UPDATE = "update-phase";
    private static final String WORK_FLOW_PHASE_ACTION = "action";

    private static final String FIELD_AUDIT = "field-audit";
    private static final String FIELD_AUDIT_TYPE = "type";
    private static final String FIELD_AUDIT_FIELD_OID = "field-oid";

    private LinkedHashMap<String, DataFieldConfig> m_existingConfigs;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Map<String, String> NAME_TO_ID = new HashMap();
        NAME_TO_ID.put("assessment-definition", "tblAssessDef");
        NAME_TO_ID.put("extended-data-dictionary", "tblDataDictExt");

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        Element root = getXmlRoot();
        Element parent = getParentDocument(root, PORTABLE_DOCUMENT);
        Iterator<Element> children = parent.getChildren().iterator();
        while (children.hasNext()) {
            Element element = children.next();
            String name = element.getName();
            String tableId = NAME_TO_ID.get(name);
            DataDictionaryTable table = dictionary.findDataDictionaryTableById(tableId);
            if (table != null) {
                logMessage("Finding table[" + tableId + "] for [" + name + "] - result = " + table);
                ObjectImporter importer = new ObjectImporter(getBroker(), true);
                // First pass performs analyze only.
                importer.start(table.getBeanClass(), getOrganization().getOid(), element);
                // second pass with analyze off. This will perform the import.
                importer.setAnalyze(false);
                Portable portable = importer.start(table.getBeanClass(), getOrganization().getOid(), element);
                logMessage("Imported portable object " + tableId + " oid = " + portable.getOid());
            } else if (VIEW_TEMPLATE.equals(name)) {
                // Must handle view template separately because portable is faulty
                importViewTemplate(element);
            } else if (DATA_FIELD_CONFIG.equals(name)) {
                importDataFieldConfig(element);
            } else if (WORK_FLOW_PHASE.equals(name)) {
                updateWorkflowPhase(element);
            } else if (CALCULATED_FIELD.equals(name)) {
                importCalculatedField(element, dictionary);
            } else if (FIELD_AUDIT.equals(name)) {
                importDataFieldAudit(element);
            }
        }

        // clear cache and reload dictionary
        try {
            (new ModelBroker(getPrivilegeSet())).clearCache();
        } catch (Exception e) {
            // ignore any clear cache exception
        }
        DataDictionaryCache.clearDictionaries(getUser().getPersistenceKey(), true);

        // Create default if necessary
        OrganizationAttributes ora = MaSpedWorkflowProcedure.getSpedConfig(getBroker());
        if (ora != null) {
            insertReportFields(ora, getParentDocument(root, REPORT_FIELDS));
        }
    }

    /**
     * Converts Element to XML string.
     *
     * @param root Element
     * @return String
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private String elementToString(Element root) throws IOException {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        XMLOutputter outputter = new XMLOutputter(Format.getPrettyFormat());
        outputter.output(root, outputStream);
        return outputStream.toString();
    }

    /**
     * Gets the attribute string.
     *
     * @param element Element
     * @param colContext String
     * @return String
     */
    private String getAttributeString(Element element, String colContext) {
        Attribute attribute = element.getAttribute(colContext);
        return attribute == null ? null : attribute.getValue();
    }

    private DataFieldConfig getDataFieldConfig(String oid) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DataFieldConfig.COL_DATA_FIELD_OID, oid);
        return getBroker().getBeanByQuery(new BeanQuery(DataFieldConfig.class, criteria));
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
    private Element getParentDocument(Element parent, String rootName) throws JDOMException, IOException {
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
     * Get the procedure bean based on the id from the input definition.
     * This procedure's input definition is the source of the SQLDocument.
     *
     * @return Procedure
     */
    private Procedure getProcedure() {
        Procedure bean = null;
        String procId = (String) getParameter(PROCEDURE_ID);
        if (!StringUtils.isEmpty(procId)) {
            X2Criteria crit = new X2Criteria();
            crit.addEqualTo(Procedure.COL_ID, procId);
            bean = (Procedure) getBroker().getBeanByQuery(new QueryByCriteria(Procedure.class, crit));
        }
        return bean;
    }

    private CalculatedField getCalculatedField(String name) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(CalculatedField.COL_NAME, name);
        CalculatedField field = getBroker().getBeanByQuery(new BeanQuery(CalculatedField.class, criteria));
        if (field == null) {
            field = X2BaseBean.newInstance(CalculatedField.class, getBroker().getPersistenceKey());
            field.setOrganization1Oid(this.getOrganization().getOid());
            field.setName(name);
            logMessage("Calculated field: " + name + " created");
        }
        return field;
    }

    /**
     * Gets the data field config.
     *
     * @param element Element
     * @return DataFieldConfig bean
     */
    private DataFieldConfig getDataFieldConfig(Element element) {
        String dataTableOid =
                element.getChild(DATA_FIELD_CONFIG_TABLE_ATTR).getAttributeValue(DATA_FIELD_CONFIG_TABLE_ID);
        String alias = element.getChild(DATA_FIELD_CONFIG_TABLE_ATTR).getChild(DATA_FIELD_CONFIG_FIELD_ATTR)
                .getAttributeValue(DATA_FIELD_CONFIG_ALIAS_ATTR);
        String lengthString = element.getChild(DATA_FIELD_CONFIG_TABLE_ATTR).getChild(DATA_FIELD_CONFIG_FIELD_ATTR)
                .getAttributeValue(DATA_FIELD_CONFIG_LENGTH_ATTR);
        int length = Integer.parseInt(lengthString);
        DataFieldConfig dataFieldConfig;
        X2Criteria criteria = new X2Criteria();

        criteria.addEqualTo(DataFieldConfig.REL_DATA_FIELD + "." +
                DataField.COL_DATA_TABLE_OID, dataTableOid);
        criteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.TRUE);
        criteria.addEqualTo(DataFieldConfig.COL_ALIAS, alias);

        dataFieldConfig = (DataFieldConfig) getBroker().getBeanByQuery(new BeanQuery(DataFieldConfig.class, criteria));

        if (dataFieldConfig == null) {
            criteria = new X2Criteria();

            criteria.addEqualTo(DataFieldConfig.REL_DATA_FIELD + "." +
                    DataField.COL_DATA_TABLE_OID, dataTableOid);
            criteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.FALSE);

            BeanQuery query = new BeanQuery(DataFieldConfig.class, criteria);
            m_existingConfigs =
                    (LinkedHashMap<String, DataFieldConfig>) getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 1024);

            if (m_existingConfigs != null) {
                if (length <= 10) {
                    for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
                        if (entry.getValue().getDataFieldOid().contains("FieldA")) {
                            dataFieldConfig = entry.getValue();
                            break;
                        }
                    }
                }
                if (dataFieldConfig == null && length <= 25) {
                    for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
                        if (entry.getValue().getDataFieldOid().contains("FieldB")) {
                            dataFieldConfig = entry.getValue();
                            break;
                        }
                    }
                }
                if (dataFieldConfig == null && length <= 50) {
                    for (Map.Entry<String, DataFieldConfig> entry : m_existingConfigs.entrySet()) {
                        if (entry.getValue().getDataFieldOid().contains("FieldC")) {
                            dataFieldConfig = entry.getValue();
                            break;
                        }
                    }
                }
            }
        }

        return dataFieldConfig;
    }

    /**
     * Gets the workflow phase.
     *
     * @return WorkflowPhase collection
     */
    private Collection getWorkflowPhaseCollection() {
        X2Criteria criteria = new X2Criteria();

        criteria.addIsNull(WorkflowPhase.COL_ID);

        return getBroker().getCollectionByQuery(new BeanQuery(WorkflowPhase.class, criteria));
    }

    /**
     * Gets the workflow phase.
     *
     * @return WorkflowPhase collection
     */
    private Collection getWorkflowPhaseOutcomeCollection() {
        X2Criteria criteria = new X2Criteria();

        criteria.addIsNull(WorkflowPhaseOutcome.COL_ID);

        return getBroker().getCollectionByQuery(new BeanQuery(WorkflowPhaseOutcome.class, criteria));
    }

    /**
     * Gets the view template.
     *
     * @param context String
     * @param name String
     * @return View template
     */
    private ViewTemplate getViewTemplate(String context, String name) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ViewTemplate.COL_CONTEXT, context);
        criteria.addEqualTo(ViewTemplate.COL_NAME, name);
        return (ViewTemplate) getBroker().getBeanByQuery(new BeanQuery(ViewTemplate.class, criteria));
    }

    /**
     * Gets the root element from input definition document.
     *
     * @return Element
     * @throws JDOMException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private Element getXmlRoot() throws JDOMException, IOException {
        Procedure proc = getProcedure();
        SAXBuilder builder = new SAXBuilder();
        Document xmlDocument = builder.build(new ByteArrayInputStream(proc.getFullInputDefinition().getBytes()));
        return xmlDocument.getRootElement();
    }

    /**
     * @param element
     * @param dictionary
     */
    private void importCalculatedField(Element element, DataDictionary dictionary) {
        String name = element.getAttributeValue(CALCULATED_FIELD_NAME);
        if (!StringUtils.isEmpty(name)) {
            CalculatedField field = getCalculatedField(name);
            String value = element.getAttributeValue(CALCULATED_FIELD_DESCRIPTION);
            if (!StringUtils.isEmpty(value)) {
                field.setDescription(value);
            }
            value = element.getAttributeValue(CALCULATED_FIELD_CALCULATED_EXPRESSION);
            if (!StringUtils.isEmpty(value)) {
                field.setCalculatedExpression(value);
            }
            value = element.getAttributeValue(CALCULATED_FIELD_PROCEDURE_ID);
            if (!StringUtils.isEmpty(value)) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(Procedure.COL_ID, value);
                Procedure procedure = getBroker().getBeanByQuery(new BeanQuery(Procedure.class, criteria));
                if (procedure != null) {
                    field.setProcedureOid(procedure.getOid());
                }
            }
            getBroker().saveBeanForced(field);

            DataFieldConfig dataField = null;
            value = element.getAttributeValue(CALCULATED_FIELD_FIELD_OID);
            if (!StringUtils.isEmpty(value)) {
                dataField = getDataFieldConfig(value);
            } else {
                value = element.getAttributeValue(CALCULATED_FIELD_FIELD_ALIAS);
                DataDictionaryField dictionaryField = dictionary.findDataDictionaryFieldByAlias(value);
                if (dictionaryField != null) {
                    dataField = dictionaryField.getDataFieldConfig();
                }
            }
            if (dataField != null) {
                dataField.setCalculatedFieldOid(field.getOid());
                getBroker().saveBeanForced(dataField);
                logMessage("Calculated field: " + name + " updated");
            } else {
                logMessage("Calculated field: " + name + " error - data field not found");
            }
        }
    }

    /**
     * Import field config.
     *
     * @param element Element
     */
    private void importDataFieldConfig(Element element) {
        String table = element.getChild(DATA_FIELD_CONFIG_TABLE_ATTR).getAttributeValue(DATA_FIELD_CONFIG_TABLE_ID);
        DataFieldConfig bean = getDataFieldConfig(element);
        if (bean != null) {

            Element childElement =
                    (Element) ((Element) element.getChild(DATA_FIELD_CONFIG_TABLE_ATTR)
                            .getChild(DATA_FIELD_CONFIG_FIELD_ATTR).clone()).detach();
            if (childElement != null) {

                bean.setOrganization1Oid(getOrganization().getOid());

                bean.setUserLongName(childElement.getAttributeValue(DATA_FIELD_CONFIG_LONG_NAME_ATTR));
                bean.setUserShortName(childElement.getAttributeValue(DATA_FIELD_CONFIG_SHORT_NAME_ATTR));
                bean.setUserLength(Integer.parseInt(childElement.getAttributeValue(DATA_FIELD_CONFIG_LENGTH_ATTR)));
                bean.setEnabledIndicator(
                        Boolean.parseBoolean(childElement.getAttributeValue(DATA_FIELD_CONFIG_ENABLED_ATTR)));
                bean.setListEditIndicator(
                        Boolean.parseBoolean(childElement.getAttributeValue(DATA_FIELD_CONFIG_LIST_EDIT_ATTR)));
                bean.setReadOnlyIndicator(
                        Boolean.parseBoolean(childElement.getAttributeValue(DATA_FIELD_CONFIG_READ_ONLY_ATTR)));
                bean.setRequiredIndicator(
                        Boolean.parseBoolean(childElement.getAttributeValue(DATA_FIELD_CONFIG_REQUIRED_ATTR)));
                bean.setUpdateIndicator(
                        Boolean.parseBoolean(childElement.getAttributeValue(DATA_FIELD_CONFIG_UPDATE_ATTR)));
                bean.setAlias(childElement.getAttributeValue(DATA_FIELD_CONFIG_ALIAS_ATTR));
                bean.setUserType(childElement.getAttributeValue(DATA_FIELD_CONFIG_TYPE_ATTR));
            }
            getBroker().saveBeanForced(bean);

            logMessage("Imported field config into the table:" + table + " alias = " + bean.getAlias() + ", oid = "
                    + bean.getOid());
        } else {
            String alias = element.getChild(DATA_FIELD_CONFIG_TABLE_ATTR).getChild(DATA_FIELD_CONFIG_FIELD_ATTR)
                    .getAttributeValue(DATA_FIELD_CONFIG_ALIAS_ATTR);
            logMessage("FAILED importing field config into the table:" + table + " alias = " + alias);
        }
    }

    private void importDataFieldAudit(Element element) {
        String oid = element.getAttributeValue(FIELD_AUDIT_FIELD_OID);
        DataFieldConfig field = getDataFieldConfig(oid);
        if (field != null) {
            String type = element.getAttributeValue(FIELD_AUDIT_TYPE);
            if (!StringUtils.isEmpty(type)) {
                int value = Integer.parseInt(type);
                field.setFieldAuditType(value);
                getBroker().saveBeanForced(field);
            }
        }
    }

    /**
     * Import view template.
     *
     * @param element Element
     */
    private void importViewTemplate(Element element) {
        String context = getAttributeString(element, ViewTemplate.COL_CONTEXT);
        String name = getAttributeString(element, ViewTemplate.COL_NAME);
        if (!StringUtils.isEmpty(context) && !StringUtils.isEmpty(name)) {
            ViewTemplate bean = getViewTemplate(context, name);
            if (bean == null) {
                bean = X2BaseBean.newInstance(ViewTemplate.class, getBroker().getPersistenceKey());
            }
            bean.setContext(context);
            bean.setName(name);
            String procedureId = getAttributeString(element, ViewTemplate.COL_PROCEDURE_ID);
            if (procedureId != null) {
                bean.setProcedureId(procedureId);
            }
            String ddxOid = getAttributeString(element, ViewTemplate.COL_EXTENDED_DATA_DICTIONARY_OID);
            if (ddxOid != null) {
                bean.setExtendedDataDictionaryOid(ddxOid);
            }
            bean.setOwnerType(Ownable.OWNER_TYPE_ORG1);
            bean.setOwnerOid(OrganizationManager.ROOT_ORGANIZATION);

            Element childElement =
                    (Element) ((Element) element.getChild(TemplateConstants.TEMPLATE_ROOT_ELEMENT).clone()).detach();
            if (childElement != null) {
                XMLOutputter xmlOut = new XMLOutputter();
                org.jdom.Document document = new org.jdom.Document(childElement);

                bean.setViewDefinition(xmlOut.outputString(document));
            }
            getBroker().saveBeanForced(bean);
            logMessage("Imported view template " + context + "-" + name + " oid = " + bean.getOid());
        }
    }

    /**
     * Insert report fields.
     *
     * @param ora OrganizationAttributes
     * @param root Element
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void insertReportFields(OrganizationAttributes ora, Element root) throws IOException {
        DataDictionaryField dictionaryField = getReportFieldsDictionaryField();
        if (dictionaryField != null) {
            String xml = (String) ora.getFieldValueByBeanPath(dictionaryField.getJavaName());
            Element rootCurrent = null;
            Set<String> currentFields = new HashSet();
            if (!StringUtils.isEmpty(xml)) {
                try {
                    SAXBuilder builder = new SAXBuilder();
                    Document xmlDocument = builder.build(new ByteArrayInputStream(xml.getBytes()));
                    rootCurrent = xmlDocument.getRootElement();
                    if (REPORT_FIELDS.equals(rootCurrent.getName())) {
                        Iterator<Element> children = rootCurrent.getChildren().iterator();
                        while (children.hasNext()) {
                            Element element = children.next();
                            String name = element.getName();
                            if (REPORT_FIELD.equals(name)) {
                                String fieldName = element.getAttributeValue(REPORT_FIELD_NAME_ATTRIB);
                                currentFields.add(fieldName);
                            }
                        }
                    }
                } catch (JDOMException | IOException e) {
                    logMessage(ALIAS_REPORT_FIELD_SCRIPTS + " does not parse as xml document");
                }
            }
            if (currentFields.isEmpty()) {
                logMessage(ALIAS_REPORT_FIELD_SCRIPTS + " populating all report-fields");
                ora.setFieldValueByBeanPath(dictionaryField.getJavaName(), elementToString(root));
                getBroker().saveBeanForced(ora);
            } else {
                List<Element> elementsToAdd = new LinkedList();
                Iterator<Element> children = root.getChildren().iterator();
                while (children.hasNext()) {
                    Element element = children.next();
                    String name = element.getName();
                    if (REPORT_FIELD.equals(name)) {
                        String fieldName = element.getAttributeValue(REPORT_FIELD_NAME_ATTRIB);
                        if (!currentFields.contains(fieldName)) {
                            elementsToAdd.add(element);
                        }
                    }
                }
                if (!elementsToAdd.isEmpty()) {
                    for (Element child : elementsToAdd) {
                        logMessage(ALIAS_REPORT_FIELD_SCRIPTS + " populating report-field "
                                + child.getAttributeValue(REPORT_FIELD_NAME_ATTRIB));
                        child.detach();
                        rootCurrent.getChildren().add(child);
                    }
                    ora.setFieldValueByBeanPath(dictionaryField.getJavaName(), elementToString(rootCurrent));
                    getBroker().saveBeanForced(ora);
                }
            }
        } else {
            logMessage("Extended data dictionary field for alias " + ALIAS_REPORT_FIELD_SCRIPTS
                    + " cannot be found");
        }
    }

    /**
     * Gets the report fields dictionary field.
     *
     * @return Data dictionary field
     */
    private DataDictionaryField getReportFieldsDictionaryField() {
        DataDictionaryField dictionaryField = null;
        ExtendedDataDictionary ddx = MaSpedWorkflowProcedure.getSpedConfigDictionary(getBroker());
        if (ddx != null) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
            if (dictionary != null) {
                dictionaryField = dictionary.findDataDictionaryFieldByAlias(ALIAS_REPORT_FIELD_SCRIPTS);
            } else {
                logMessage("Dictionary cannot be found");
            }
        } else {
            logMessage("Extended data dictionary cannot be found");
        }
        return dictionaryField;
    }

    /**
     * Update workflow phase.
     *
     * @param element Element
     */
    private void updateWorkflowPhase(Element element) {
        String wph_action = element.getChild(WORK_FLOW_PHASE_UPDATE).getAttributeValue(WORK_FLOW_PHASE_ACTION);
        if (wph_action.equals("true")) {
            Collection beans = getWorkflowPhaseCollection();

            if (beans.size() != 0) {
                Iterator workflowObjectsIterator = beans.iterator();
                while (workflowObjectsIterator.hasNext()) {
                    WorkflowPhase workflowObject = (WorkflowPhase) workflowObjectsIterator.next();

                    if (StringUtils.isEmpty(workflowObject.getId())) {
                        workflowObject.setId(workflowObject.getOid());
                        getBroker().saveBeanForced(workflowObject);
                        logMessage("Workflow phase id updated: oid = " + workflowObject.getOid());
                    }
                }
            } else {
                logMessage("Workflow phase - nothing to update");
            }

            updateWorkflowPhaseOutcome();
        }
    }

    /**
     * Update workflow phase outcome.
     *
     * @param element Element
     */
    private void updateWorkflowPhaseOutcome() {
        Collection beans = getWorkflowPhaseOutcomeCollection();

        if (beans.size() != 0) {
            Iterator workflowObjectsIterator = beans.iterator();
            while (workflowObjectsIterator.hasNext()) {
                WorkflowPhaseOutcome workflowObject = (WorkflowPhaseOutcome) workflowObjectsIterator.next();

                if (StringUtils.isEmpty(workflowObject.getId())) {
                    workflowObject.setId(workflowObject.getOid());
                    getBroker().saveBeanForced(workflowObject);
                    logMessage("Workflow phase outcome id updated: oid = " + workflowObject.getOid());
                }
            }
        } else {
            logMessage("Workflow phase outcome - nothing to update");
        }
    }

}
