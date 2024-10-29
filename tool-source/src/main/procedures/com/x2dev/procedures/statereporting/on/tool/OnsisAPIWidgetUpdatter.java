/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2023 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.tool;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.WidgetDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * The Class OnsisAPIBuilder.
 *
 * @author Follett Software Company
 * @copyright 2023
 */
public class OnsisAPIWidgetUpdatter extends ProcedureJavaSource {
    private static final String ALIAS_ORGANIZATION_LANGUAGE = "all-org-BoardLanguage";

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

        WidgetDefinition widget = getWidgetDefinition(getElementValue(root, WidgetDefinition.COL_ID));
        widget.setTitle(getElementValue(root, WidgetDefinition.COL_TITLE));
        widget.setCustomIndicator(true);
        widget.setPosition("top");
        String source = updateSource(getElementValue(root, WidgetDefinition.COL_SOURCE));
        widget.setSource(source);

        getBroker().saveBeanForced(widget);
    }

    /**
     * Save state.
     *
     * @param userData the user data
     * @throws X2BaseException the x 2 base exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        this.runOnApplicationServer();
        super.saveState(userData);
    }

    /**
     * Gets the document from the parent.
     *
     * @param parent Element
     * @param name the name
     * @return Element
     * @throws JDOMException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private String getElementValue(Element parent, String name) throws JDOMException, IOException {
        Element document = null;
        if (parent.getChildren() != null && parent.getChildren().size() > 0) {
            Iterator<Element> children = parent.getChildren().iterator();
            while (children.hasNext()) {
                Element element = children.next();
                if (name.equals(element.getName())) {
                    document = element;
                    break;
                }
            }
        }
        return document.getText();
    }

    /**
     * Get the procedure bean based on the id from the input definition.
     * This procedure's input definition is the source of the SQLDocument.
     *
     * @param importId the import id
     * @return Procedure
     */
    private ImportExportDefinition getImport(String importId) {
        ImportExportDefinition bean = null;
        if (!StringUtils.isEmpty(importId)) {
            X2Criteria crit = new X2Criteria();
            crit.addEqualTo(ImportExportDefinition.COL_ID, importId);
            bean = (ImportExportDefinition) getBroker()
                    .getBeanByQuery(new QueryByCriteria(ImportExportDefinition.class, crit));
        }
        return bean;
    }

    /**
     * Gets the widget definition.
     *
     * @param id the id
     * @return the widget definition
     */
    private WidgetDefinition getWidgetDefinition(String id) {
        X2Criteria crit = new X2Criteria();
        crit.addEqualTo(WidgetDefinition.COL_ID, id);
        WidgetDefinition bean =
                (WidgetDefinition) getBroker().getBeanByQuery(new QueryByCriteria(WidgetDefinition.class, crit));
        if (bean == null) {
            bean = X2BaseBean.newInstance(WidgetDefinition.class, getBroker().getPersistenceKey());
            OrganizationManager.setOrganizationOids(bean, getOrganization());
            bean.setId(id);
        }
        return bean;
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
    private Element getXmlRoot(String xmlString) throws JDOMException, IOException {
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
     * Determine if the board is a french language board.
     *
     * @return boolean
     */
    private boolean isFrenchBoard() {
        boolean isFrench = false;
        Organization org = getOrganization();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_ORGANIZATION_LANGUAGE);
        if (field != null) {
            String lang = (String) org.getFieldValueByBeanPath(field.getJavaName());
            if (!StringUtils.isEmpty(lang)) {
                ReferenceTable refTable = field.getReferenceTable();
                if (refTable != null) {
                    Map<String, ReferenceCode> codes = refTable.getCodeMap(getBroker());
                    ReferenceCode code = codes.get(lang);
                    if (code != null) {
                        isFrench = ("F".equals(code.getStateCode()));
                    }
                }
            }
        }
        return isFrench;
    }

    /**
     * Update source.
     *
     * @param source the source
     * @return the string
     */
    private String updateSource(String source) {
        source = source.replace("{{ONSIS-ERR-IMPORT}}", getImport("ONSIS-ERR-IMPORT").getOid());
        source = source.replace("{{ONSIS-EXP-ALL}}", getImport("ONSIS-EXP-ALL").getOid());
        source = source.replace("{{deploymentId}}", getBroker().getPersistenceKey().getDeploymentId());
        source = source.replace("{{isFrench}}", isFrenchBoard() ? "true" : "false");
        return source;
    }

}
