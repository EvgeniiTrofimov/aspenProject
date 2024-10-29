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

import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationResource;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * The Class SetToolNames.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class SetToolNames extends ProcedureJavaSource {

    /**
     * The Class ToolName.
     */
    private static class ToolName {
        private String prefix;
        private String id;
        private String nameEn;
        private String nameFr;
        private String descrEn;
        private String descrFr;

        /**
         * Create from XML element
         *
         * @param element the element
         * @return the tool name
         */
        public static ToolName of(Element element) {
            Function<String, String> attr = attrName -> Optional.ofNullable(element.getAttribute(attrName))
                    .map(Attribute::getValue)
                    .orElse("");
            ToolName instance = new ToolName();
            instance.prefix = attr.apply("prefix");
            instance.id = attr.apply("id");
            instance.nameEn = attr.apply("name-en");
            instance.nameFr = attr.apply("name-fr");
            instance.descrEn = attr.apply("description-en");
            instance.descrFr = attr.apply("description-fr");
            return instance;
        }

        /**
         * Checks if is valid.
         *
         * @return true, if is valid
         */
        public boolean isValid() {
            return !prefix.isEmpty() && !id.isEmpty() && !nameEn.isEmpty() && !nameFr.isEmpty();
        }

        /**
         * To string.
         *
         * @return the string
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "prefix: " + prefix + ", id: " + id + ", nameEn: " + nameEn + ", nameFr: " + nameFr + "descrEn:"
                    + descrEn + "descrFr:" + descrFr;
        }

    }

    private static final Map<String, String> DESCRIPTION_PATH_MAP = new HashMap<>();
    static {
        DESCRIPTION_PATH_MAP.put("IED", SisBeanPaths.IMPORT_EXPORT_DEFINITION.description().getColumnOid());
        DESCRIPTION_PATH_MAP.put("RPT", SisBeanPaths.REPORT.description().getColumnOid());
        DESCRIPTION_PATH_MAP.put("PRC", SisBeanPaths.PROCEDURE_INFO.description().getColumnOid());
    }

    private static final Map<String, String> NAME_PATH_MAP = new HashMap<>();
    static {
        NAME_PATH_MAP.put("IED", SisBeanPaths.IMPORT_EXPORT_DEFINITION.name().getColumnOid());
        NAME_PATH_MAP.put("RPT", SisBeanPaths.REPORT.name().getColumnOid());
        NAME_PATH_MAP.put("PRC", SisBeanPaths.PROCEDURE_INFO.name().getColumnOid());
    }

    private static final String LOCALE_EN_US = Locale.US.toString();
    private static final String LOCALE_EN_FR = Locale.CANADA.toString();
    private static final String LOCALE_FRANCE = Locale.FRANCE.toString();

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
     * Execute.
     *
     * @throws Exception the exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Procedure procedure = (Procedure) this.getJob().getTool();
        String inputDefinition = procedure.getFullInputDefinition();
        Element root = getXmlRoot(inputDefinition);
        int count = 0;
        if (root != null) {
            Element parent = getParentDocument(root, "tool-names");
            if (parent != null) {
                List toolNames = parent.getChildren("tool-name");
                if (toolNames.isEmpty()) {
                    logErrorMessage("No tool name records found");
                } else {
                    for (Object item : toolNames) {
                        ToolName toolName = ToolName.of((Element) item);
                        if (toolName.isValid()) {
                            if (setToolName(toolName)) {
                                count++;
                            }
                        } else {
                            logErrorMessage("Invalid tool name record: " + toolName.toString());
                        }
                    }
                }
            } else {
                logErrorMessage("No tool names element found");
            }
        } else {
            logErrorMessage("Not able to find XML root in input definition");
        }
        logInfoMessage("Localization resources created for " + count + " beans");
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
     * Find bean.
     *
     * @param toolName the tool name
     * @return the optional
     */
    private Optional<X2BaseBean> findBean(ToolName toolName) {
        Criteria criteria = new Criteria();
        Class cls;
        if (toolName.prefix.equals(SisBeanPaths.IMPORT_EXPORT_DEFINITION.getObjectPrefix())) {
            criteria.addEqualTo(ImportExportDefinition.COL_ID, toolName.id);
            cls = ImportExportDefinition.class;
        } else if (toolName.prefix.equals(SisBeanPaths.REPORT.getObjectPrefix())) {
            criteria.addEqualTo(Report.COL_ID, toolName.id);
            cls = Report.class;
        } else if (toolName.prefix.equals(SisBeanPaths.PROCEDURE_INFO.getObjectPrefix())) {
            criteria.addEqualTo(Procedure.COL_ID, toolName.id);
            cls = Procedure.class;
        } else {
            logErrorMessage("Unexpected bean class prefix: " + toolName.prefix);
            return Optional.empty();
        }
        QueryByCriteria query = new QueryByCriteria(cls, criteria);
        return Optional.ofNullable(getBroker().getBeanByQuery(query));
    }

    /**
     * Save resource.
     *
     * @param locale the locale
     * @param beanOid the bean oid
     * @param beanPath the bean path
     * @param value the value
     * @return true, if successful
     */
    private boolean saveResource(String locale, String beanOid, String beanPath, String value) {
        String key = LocalizationUtils.generateKey(beanOid, beanPath);
        LocalizationResource resource = new LocalizationResource(locale, key, beanOid, value);
        LocalizationCache.setLocalizedResource(getBroker(), resource);
        return true;
    }

    /**
     * Sets the tool name.
     *
     * @param toolName the tool name
     * @return true, if successful
     */
    private boolean setToolName(ToolName toolName) {
        Optional<X2BaseBean> beanOpt = findBean(toolName);
        if (beanOpt.isPresent()) {
            X2BaseBean bean = beanOpt.get();
            String beanPathName = NAME_PATH_MAP.get(toolName.prefix);
            String beanPathDescr = DESCRIPTION_PATH_MAP.get(toolName.prefix);
            saveResource(LOCALE_EN_US, bean.getOid(), beanPathName, toolName.nameEn);
            saveResource(LOCALE_EN_FR, bean.getOid(), beanPathName, toolName.nameEn);
            saveResource(LOCALE_FRANCE, bean.getOid(), beanPathName, toolName.nameFr);
            saveResource(LOCALE_EN_US, bean.getOid(), beanPathDescr, toolName.descrEn);
            saveResource(LOCALE_EN_FR, bean.getOid(), beanPathDescr, toolName.descrEn);
            saveResource(LOCALE_FRANCE, bean.getOid(), beanPathDescr, toolName.descrFr);

            return true;
        }
        logErrorMessage("can't find bean for tool: " + toolName.toString());
        return false;
    }
}
