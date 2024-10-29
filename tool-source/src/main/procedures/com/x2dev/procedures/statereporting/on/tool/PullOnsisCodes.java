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
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.TranscriptDefinition;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class PullOnsisCodes extends ProcedureJavaSource {
    private static final String LOCALE_FRANCE = Locale.FRANCE.toString();

    public class OnsisField {
        String alias;
        String ddxId;
        DataDictionary dictionary;
        DataDictionaryField field;
        String fieldId;
        DataDictionaryTable table;
        String tableName;

        public OnsisField(Element element) {
            tableName = element.getAttributeValue("table");
            ddxId = element.getAttributeValue("ddxId");
            alias = element.getAttributeValue("alias");
            fieldId = element.getAttributeValue("fieldId");
            init();
        }

        public Element getElement() {
            Element element = new Element("field-lookup");

            element.setAttribute("table-name", tableName);
            if (!StringUtils.isEmpty(ddxId)) {
                element.setAttribute("ddx-id", ddxId);
            }
            if (!StringUtils.isEmpty(alias)) {
                element.setAttribute("alias", alias);
            }
            if (!StringUtils.isEmpty(fieldId)) {
                element.setAttribute("fieldId", fieldId);
            }

            return element;
        }

        public ReferenceTable getRefTable() {
            return field.getReferenceTable();
        }

        private void init() {
            dictionary = getDictionary(ddxId);
            table = dictionary.findDataDictionaryTableByDatabaseName(tableName);
            if (table == null) {
                throw new IllegalStateException("Database table not found for table:" + tableName + ", ddx:" + ddxId
                        + ", alias:" + alias + ", field:" + fieldId);
            }
            if (!StringUtils.isEmpty(alias)) {
                field = dictionary.findDataDictionaryFieldByAlias(alias);
            } else {
                field = dictionary.findDataDictionaryField(fieldId);
            }
            if (field == null) {
                throw new IllegalStateException("Field not found for table:" + tableName + ", ddx:" + ddxId + ", alias:"
                        + alias + ", field:" + fieldId);
            }
            if (!field.getTable().equals(table)) {
                throw new IllegalStateException("Field not associated with table:" + tableName + ", ddx:" + ddxId
                        + ", alias:" + alias + ", field:" + fieldId);
            }
            if (!field.hasReferenceTable()) {
                throw new IllegalStateException(
                        "Reference table not associated with table:" + tableName + ", ddx:" + ddxId
                                + ", alias:" + alias + ", field:" + fieldId);
            }
        }
    }


    StringBuilder xml = new StringBuilder();

    /**
     * Converts element to XML string.
     *
     * @param root Element
     * @return String
     * @throws IOException Signals that an I/O exception has occurred.
     */
    public String elementToString(Element root) throws IOException {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        Format format = Format.getPrettyFormat().setEncoding("UTF-8");
        XMLOutputter outputter = new XMLOutputter(format);
        outputter.output(root, outputStream);
        return outputStream.toString();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyyMMdd_hhmmss");
        String dateStr = dateFormatter.format(new Date());

        String customName = "OnsisCodes";
        String fileNamePrefix = customName;
        String extension = ".xml";

        if (getMessages().size() > 0) {
            customName = "OnsisCodesErrors";
            extension = ".txt";
        }

        return fileNamePrefix.replaceAll("\\s", "_") + "_" + dateStr + extension;
    }

    public DataDictionary getDictionary(String ddxId) {
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
                        throw new RuntimeException("Extended Dictionary Attributes not found for id [" + ddxId + "]");
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
                    DataDictionary.getDistrictDictionary(extDictAttributes, getBroker().getPersistenceKey());
        }
        return dictionary;
    }

    /**
     * Logs message and sets m_errors so procedure won't be deleted
     *
     * @param message
     */
    public void logErrorMessage(String message) {
        logMessage("ERROR: " + message);
    }

    /**
     * Gets the root element from an XML string (input definition, view template,
     * etc.)
     *
     * @param xmlString
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
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws JDOMException, IOException {
        Procedure procedure = (Procedure) this.getJob().getTool();
        if (procedure != null) {
            String inputDefinition = procedure.getFullInputDefinition();
            Element root = getXmlRoot(inputDefinition);
            Element parent = getParentDocument(root, "OnsisFields");
            if (parent != null) {
                xml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
                xml.append("<tool-input refresh=\"0\">\n");
                xml.append(
                        "<input name=\"dictionaryOnly\" data-type=\"boolean\" display-type=\"checkbox\" display-name=\"Dictionary check only\" default-value=\"false\" required=\"false\" /> \n");
                xml.append(
                        "<input name=\"updateCodes\" data-type=\"boolean\" display-type=\"checkbox\" display-name=\"Update codes\" default-value=\"true\" required=\"false\" /> \n");
                xml.append("<reference-tables>\n");
                List<OnsisField> fields = (List<OnsisField>) parent.getChildren().stream()
                        .map(element -> {
                            OnsisField field = null;
                            try {
                                field = new OnsisField((Element) element);
                            } catch (Exception e) {
                                logErrorMessage(e.getMessage());
                            }
                            return field;
                        })
                        .filter(Objects::nonNull)
                        .collect(Collectors.toList());
                Map<ReferenceTable, List<OnsisField>> refTableMap =
                        fields.stream().collect(Collectors.groupingBy(field -> field.getRefTable()));
                List<ReferenceTable> rtbList = refTableMap.keySet().stream()
                        .sorted((rtb1, rtb2) -> rtb1.getUserName().compareTo(rtb2.getUserName()))
                        .collect(Collectors.toList());

                for (ReferenceTable rtb : rtbList) {
                    Map<String, ReferenceCode> codeMap = getCodeMap(rtb);
                    Element rtbElement = rtb.marshal();
                    rtbElement.setAttribute(ReferenceTable.COL_DATA_ENTRY_VIA_VIEW_FIELDS_IND,
                            String.valueOf(rtb.getDataEntryViaViewFieldsInd()));
                    rtbElement.setAttribute(ReferenceTable.COL_DATA_TABLE_OID, rtb.getDataTableOid());
                    rtbElement.setAttribute(X2BaseBean.COL_OID, rtb.getOid());

                    List<Element> childrenToRemove = new ArrayList();
                    Set<String> stateCodesProcessed = new HashSet();
                    boolean hasValues = false;
                    Iterator<Element> children = rtbElement.getChildren().stream()
                            .sorted(new Comparator() {
                                @Override
                                public int compare(Object obj1, Object obj2) {
                                    Element elem1 = (Element) obj1;
                                    Attribute attrib1 = elem1.getAttribute("disabled");
                                    boolean disabled1 = attrib1 != null && attrib1.getValue().equals("true");

                                    Element elem2 = (Element) obj2;
                                    Attribute attrib2 = elem2.getAttribute("disabled");
                                    boolean disabled2 = attrib2 != null && attrib2.getValue().equals("true");
                                    if (disabled1 && disabled2) {
                                        return 0;
                                    } else if (disabled1) {
                                        return 1;
                                    } else if (disabled2) {
                                        return -1;
                                    }
                                    return 0;
                                }
                            }).iterator();
                    while (children.hasNext()) {
                        Element rcdElement = children.next();
                        if (rcdElement.getName().equals("reference-code")) {
                            Attribute stateCode = rcdElement.getAttribute("state-code");
                            if (stateCode == null || StringUtils.isEmpty(stateCode.getValue())) {
                                childrenToRemove.add(rcdElement);
                            } else if (stateCodesProcessed.contains(stateCode.getValue())) {
                                Attribute attrib = rcdElement.getAttribute("disabled");
                                boolean disabled = attrib != null && attrib.getValue().equals("true");
                                if (disabled) {
                                    childrenToRemove.add(rcdElement);
                                } else {
                                    logErrorMessage(
                                            "Duplicated state code " + stateCode.getValue() + " in reference table "
                                                    + rtb.getUserName());
                                }
                            } else {
                                Attribute code = rcdElement.getAttribute("code");
                                if (code == null) {
                                    throw new IllegalStateException("code attribute not found");
                                }
                                ReferenceCode rcd = codeMap.get(code.getValue());
                                if (rcd == null) {
                                    throw new IllegalStateException("reference code not found");
                                }
                                String frDescription =
                                        getFrench(rcd.getOid(), SisBeanPaths.REF_CODE.description().getColumnOid());
                                if (!StringUtils.isEmpty(frDescription)) {
                                    rcdElement.setAttribute("fr-description", frDescription);
                                }
                                String frCode = getFrench(rcd.getOid(), SisBeanPaths.REF_CODE.code().getColumnOid());
                                if (!StringUtils.isEmpty(frCode)) {
                                    rcdElement.setAttribute("fr-code", frCode);
                                }
                                hasValues = true;
                                stateCodesProcessed.add(stateCode.getValue());
                            }
                        }
                    }
                    if (hasValues) {
                        for (Element element : childrenToRemove) {
                            element.getParent().removeContent(element);
                        }
                        for (OnsisField field : refTableMap.get(rtb)) {
                            rtbElement.addContent(field.getElement());
                        }
                        xml.append(elementToString(rtbElement) + "\n\n");
                    }
                }

                xml.append("</reference-tables>\n");
                xml.append("</tool-input>\n");
            }
        } else {
            logErrorMessage("Not able to find XML root in input definition");
        }
    }

    /**
     * Export results.
     *
     * @throws X2BaseException the x 2 base exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        if (getMessages().size() == 0) {
            try {
                try (InputStream in = new ByteArrayInputStream(xml.toString().getBytes(StandardCharsets.UTF_8));
                        OutputStream out = getResultHandler().getOutputStream()) {
                    StreamUtils.copyStream(in, out);
                }
            } catch (IOException e) {
                throw new X2BaseException(e);
            }
        } else {
            super.exportResults();
        }

    }

    /**
     * Gets the document from the parent
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
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        this.runOnApplicationServer();
    }

    /**
     * @param rtb
     * @return
     */
    private Map<String, ReferenceCode> getCodeMap(ReferenceTable rtb) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisBeanPaths.REF_CODE.referenceTableOid().getPath(), rtb.getOid());
        BeanQuery query = new BeanQuery(SisBeanPaths.REF_CODE.getBeanType(), criteria);

        return getBroker().getMapByQuery(query, SisBeanPaths.REF_CODE.code().getPath(), 100);
    }

    /**
     * @param oid
     * @return
     */
    private String getFrench(String oid, String beanPath) {
        String key = LocalizationUtils.generateKey(oid, beanPath);
        return LocalizationCache.getLocalizedResources(getBroker().getPersistenceKey(), oid).stream()
                .filter(resource -> key.equals(resource.getKey()) && LOCALE_FRANCE.equals(resource.getLocale()))
                .map(resource -> resource.getValue())
                .findAny().orElse(null);
    }

}
