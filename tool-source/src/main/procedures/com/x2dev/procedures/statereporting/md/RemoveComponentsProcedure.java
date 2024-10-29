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
package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * Procedure that remove old export format.
 *
 * @author Follett Software Company
 */
public class RemoveComponentsProcedure extends ProcedureJavaSource {

    /**
     * Input parameters.
     */
    private static final String INPUT_PREVIEW = "preview";
    private static final String INPUT_EXP_IDS = "exportIds";
    private static final String INPUT_REMOVE = "remove";

    /**
     * Class members
     */
    private boolean m_previewOnly;
    private boolean m_removeComponents;
    private Collection<Report> m_reports;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Collection<ImportExportDefinition> ieds = getSelectedIeds();
        if (ieds != null && !ieds.isEmpty()) {
            if (m_previewOnly) {
                logMessage("Preview Mode");
                logMessage("============");
                logMessage(System.lineSeparator());
                for (ImportExportDefinition ied : ieds) {
                    String iedInput = ied.getFullInputDefinition();
                    if (!StringUtils.isEmpty(iedInput)) {
                        try {
                            SAXBuilder builder = new SAXBuilder();
                            Document xmlDocument = builder.build(new ByteArrayInputStream(iedInput.getBytes()));
                            Element parent = xmlDocument.getRootElement();
                            String prcdId = null;
                            if (parent.getChildren() != null && parent.getChildren().size() > 0) {
                                Iterator<Element> children = parent.getChildren().iterator();
                                while (children.hasNext()) {
                                    Element element = children.next();
                                    String name = element.getName();
                                    if ("input".equals(name)
                                            && "procedureId".equals(element.getAttributeValue("name"))) {
                                        prcdId = element.getAttributeValue("default-value");
                                        break;
                                    }
                                }
                            }
                            if (!StringUtils.isEmpty(prcdId)) {
                                Procedure prc = getPrcByPrcdId(prcdId);
                                if (prc != null) {
                                    logMessage("Procedure with ID = " + prc.getId()
                                            + " will be removed in \"Delete Mode\"");
                                } else {
                                    logMessage(
                                            "There was NOT found procedure concerning selected Export/Import with id = "
                                                    + ied.getId()
                                                    + ". No any Procedure will be removed in \"Delete Mode\"");
                                }
                                Report rpt = getRptByPrcdId(prcdId);
                                if (rpt != null) {
                                    logMessage("Report with ID = " + rpt.getId()
                                            + " will be removed in \"Delete Mode\"");
                                } else {
                                    logMessage("There was NOT found report concerning selected Export/Import with id = "
                                            + ied.getId() + ". No any Report will be removed in \"Delete Mode\"");
                                }
                                ExportFormatDefinition efd = getEfdByPrcdId(prcdId);
                                if (efd != null) {
                                    logMessage("Format with ID = " + efd.getId()
                                            + " will be removed in \"Delete Mode\"");
                                } else {
                                    logMessage("There was NOT found report concerning selected Export/Import with id = "
                                            + ied.getId() + ". No any Format will be removed in \"Delete Mode\"");
                                }
                            } else {
                                logMessage(
                                        "There was nothing (formats/reports/procedures) found concerning selected Export/Import with id = "
                                                + ied.getId()
                                                + ". Default procedure ID was not indentified in input definition.");
                                logMessage("Export/Import with ID = " + ied.getId()
                                        + " will be removed in \"Delete Mode\"");
                            }
                        } catch (JDOMException jde) {
                            // Ignore. The tool input template is bad somehow. We can't get the
                            // input
                            // defaults from it.
                        } catch (IOException ioe) {
                            // Ignore. The tool input template is bad somehow. We can't get the
                            // input
                            // defaults from it.
                        }
                    }
                    logMessage("Import/Export with ID = " + ied.getId() + " will be removed in \"Delete Mode\"");
                    logMessage(System.lineSeparator());
                }
            } else if (m_removeComponents) {
                logMessage("Remove Mode");
                logMessage("============");
                logMessage(System.lineSeparator());
                for (ImportExportDefinition ied : ieds) {
                    String iedInput = ied.getFullInputDefinition();
                    if (!StringUtils.isEmpty(iedInput)) {
                        try {
                            SAXBuilder builder = new SAXBuilder();
                            Document xmlDocument = builder.build(new ByteArrayInputStream(iedInput.getBytes()));
                            Element parent = xmlDocument.getRootElement();
                            String prcdId = null;
                            if (parent.getChildren() != null && parent.getChildren().size() > 0) {
                                Iterator<Element> children = parent.getChildren().iterator();
                                while (children.hasNext()) {
                                    Element element = children.next();
                                    String name = element.getName();
                                    if ("input".equals(name)
                                            && "procedureId".equals(element.getAttributeValue("name"))) {
                                        prcdId = element.getAttributeValue("default-value");
                                        break;
                                    }
                                }
                            }
                            if (!StringUtils.isEmpty(prcdId)) {
                                Procedure prc = getPrcByPrcdId(prcdId);
                                if (prc != null) {
                                    getBroker().deleteBean(prc);
                                    logMessage("Procedure with ID = " + prc.getId()
                                            + " was removed.");
                                } else {
                                    logMessage(
                                            "There was NOT found procedure concerning selected Export/Import with id = "
                                                    + ied.getId()
                                                    + ". Nothing to delete");
                                }
                                Report rpt = getRptByPrcdId(prcdId);
                                if (rpt != null) {
                                    logMessage("Report with ID = " + rpt.getId()
                                            + " was removed.");
                                } else {
                                    logMessage("There was NOT found report concerning selected Export/Import with id = "
                                            + ied.getId() + ". Nothing to delete");
                                }
                                ExportFormatDefinition efd = getEfdByPrcdId(prcdId);
                                if (efd != null) {
                                    getBroker().deleteBean(efd);
                                    logMessage("Format with ID = " + efd.getId()
                                            + " was removed.");
                                } else {
                                    logMessage("There was NOT found report concerning selected Export/Import with id = "
                                            + ied.getId() + ". Nothing to delete");
                                }
                            } else {
                                logMessage(
                                        "There was nothing (formats/reports/procedures) found concerning selected Export/Import with id = "
                                                + ied.getId()
                                                + ". Default procedure ID was not indentified in input definition.");
                                logMessage("Export/Import with ID = " + ied.getId()
                                        + " will be removed in \"Delete Mode\"");
                            }
                        } catch (JDOMException jde) {
                            // Ignore. The tool input template is bad somehow. We can't get the
                            // input
                            // defaults from it.
                        } catch (IOException ioe) {
                            // Ignore. The tool input template is bad somehow. We can't get the
                            // input
                            // defaults from it.
                        }
                    }
                    getBroker().deleteBean(ied);
                    logMessage("Import/Export with ID = " + ied.getId() + " was removed");
                    logMessage(System.lineSeparator());
                }
            }
        } else {
            logMessage(
                    "No imports/exports were found/selected. Please be sure you selected at least one import/export to remove");
        }
    }

    /**
     * This method is provided as a convenient way for subclasses to initialize member variables
     * before the <code>run()</code> method is called. The default implementation does nothing.
     *
     * @throws X2BaseException exception
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_previewOnly = ((Boolean) getParameter(INPUT_PREVIEW)).booleanValue();
        m_removeComponents = ((Boolean) getParameter(INPUT_REMOVE)).booleanValue();
        m_reports = getBroker().getCollectionByQuery(new QueryByCriteria(Report.class, new X2Criteria()));
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
    }

    /**
     * Returns Export Format definition to remove.
     *
     * @return ExportFormatDefinition
     */
    private ExportFormatDefinition getEfdByPrcdId(String prcdId) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExportFormatDefinition.COL_PROCEDURE_ID, prcdId);
        BeanQuery beanQuery = new BeanQuery(ExportFormatDefinition.class, criteria);
        return (ExportFormatDefinition) getBroker().getBeanByQuery(beanQuery);
    }

    /**
     * Returns Procedure to remove.
     *
     * @return Procedure
     */
    private Procedure getPrcByPrcdId(String prcdId) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Procedure.COL_ID, prcdId);
        BeanQuery beanQuery = new BeanQuery(Procedure.class, criteria);
        return (Procedure) getBroker().getBeanByQuery(beanQuery);
    }

    /**
     * Returns Report to remove.
     *
     * @return Report
     */
    private Report getRptByPrcdId(String prcdId) {
        Report reportToReturn = null;
        for (Report rpt : m_reports) {
            String input = rpt.getFullInputDefinition();
            if (!StringUtils.isEmpty(input)) {
                try {
                    SAXBuilder builder = new SAXBuilder();
                    Document xmlDocument = builder.build(new ByteArrayInputStream(input.getBytes()));
                    Element parent = xmlDocument.getRootElement();
                    if (parent.getChildren() != null && parent.getChildren().size() > 0) {
                        Iterator<Element> children = parent.getChildren().iterator();
                        while (children.hasNext()) {
                            Element element = children.next();
                            String name = element.getName();
                            if ("input".equals(name)
                                    && "procedureId".equals(element.getAttributeValue("name"))
                                    && element.getAttributeValue("default-value") != null
                                    && element.getAttributeValue("default-value").equals(prcdId)) {
                                return rpt;
                            }
                        }
                    }
                } catch (JDOMException jde) {
                    // Ignore. The tool input template is bad somehow. We can't get the
                    // input
                    // defaults from it.
                } catch (IOException ioe) {
                    // Ignore. The tool input template is bad somehow. We can't get the
                    // input
                    // defaults from it.
                }
            }
        }
        return reportToReturn;
    }

    /**
     * Returns Exports/Imports to remove.
     *
     * @return Report
     */
    private Collection<ImportExportDefinition> getSelectedIeds() {
        Collection<ImportExportDefinition> ieds = null;
        if (getParameter(INPUT_EXP_IDS) != null) {
            String[] exportOids = ((String) getParameter(INPUT_EXP_IDS)).split(",");
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(X2BaseBean.COL_OID, Arrays.asList(exportOids));
            ieds = getBroker().getCollectionByQuery(new QueryByCriteria(ImportExportDefinition.class, criteria));
        }
        return ieds;
    }
}
