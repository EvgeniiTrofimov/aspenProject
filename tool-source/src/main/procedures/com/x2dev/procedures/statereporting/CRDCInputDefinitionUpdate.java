/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.ToolSourceCode;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import java.io.ByteArrayInputStream;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

/**
 * The Class CRDCInputDefinitionUpdate is a procedure that is used to update the input definitions
 * for the CRDC Final Exports with the OIDs of the export format definitions for the export format
 * result sets. This is an important performance enhancement for MA districts which have millions of
 * rows in export format results.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class CRDCInputDefinitionUpdate extends ProcedureJavaSource {
    private static final List<String> EXPORT_IDS =
            Arrays.asList("EXP-CRDC-SCH-P1", "EXP-CRDC-SCH-P2", "EXP-CRDC-LEA-P1", "EXP-CRDC-LEA-P2");
    private static final List<String> REPORT_IDS =
            Arrays.asList("EXP-CRDC-SCH-P1-VAL", "EXP-CRDC-SCH-P2-VAL", "EXP-CRDC-LEA-P1-VAL", "EXP-CRDC-LEA-P2-VAL");
    private static final String XPATH_TO_EVALUATE = "//filter[@field='relEfrEfdOid.efdPrcId']";

    /**
     * Select the reports and exports to update
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        // Process Imports
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(Report.COL_ID, REPORT_IDS);
        Collection<Report> reports = getBroker().getCollectionByQuery(
                new BeanQuery(Report.class, criteria));
        for (Report report : reports) {
            if (updateSourceCode(report.getSourceCode())) {
                logMessage("Report " + report.getId() + " was updated.");
            }
        }

        // Process Exports
        criteria = new X2Criteria();
        criteria.addIn(ImportExportDefinition.COL_ID, EXPORT_IDS);
        Collection<ImportExportDefinition> exports = getBroker().getCollectionByQuery(
                new BeanQuery(ImportExportDefinition.class, criteria));
        for (ImportExportDefinition export : exports) {
            if (updateSourceCode(export.getSourceCode())) {
                logMessage("Export " + export.getId() + " was updated.");
            }
        }
    }

    /**
     * Gets the export format definition for a particular OID
     *
     * @param efdId String
     * @return Export format definition
     */
    private ExportFormatDefinition getExportFormatDefinition(String efdId) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExportFormatDefinition.COL_PROCEDURE_ID, efdId);
        return (ExportFormatDefinition) getBroker().getBeanByQuery(
                new BeanQuery(ExportFormatDefinition.class, criteria));
    }

    /**
     * Update the filter node to restrict values based on EFR OID rather than using the related ID
     *
     * @param node Node
     */
    private void updateNode(Node node) {
        Element item = (Element) node;
        String efdId = item.getAttribute("value");
        ExportFormatDefinition efd = getExportFormatDefinition(efdId);
        if (efd != null) {
            item.setAttribute("value", efd.getOid());
            item.setAttribute("field", "efrEfdOID");
        }
    }

    /**
     * Evaluate the input definition for the provided ToolSourceCode and update any filters
     * referring to export format results queried by export format definition ID to query directly
     * based on export format definition OID.
     *
     * @param source ToolSourceCode
     * @return true, if successful
     * @throws Exception exception
     */
    private boolean updateSourceCode(ToolSourceCode source) throws Exception {
        boolean value = false;
        String inputDefinition = source.getInputDefinition();
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        Document docInputDefinition =
                dBuilder.parse(new InputSource(new ByteArrayInputStream(inputDefinition.getBytes("utf-8"))));
        XPathFactory xPathfactory = XPathFactory.newInstance();
        XPath xpath = xPathfactory.newXPath();
        XPathExpression expr = xpath.compile(XPATH_TO_EVALUATE);
        NodeList nodeList = (NodeList) expr.evaluate(docInputDefinition, XPathConstants.NODESET);

        if (nodeList.getLength() > 0) {
            for (int count = 0; count < nodeList.getLength(); count++) {
                updateNode(nodeList.item(count));
            }
            DOMSource inputDefSource = new DOMSource(docInputDefinition);
            StringWriter output = new StringWriter();
            StreamResult inputDef = new StreamResult(output);

            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer;
            transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");

            transformer.transform(inputDefSource, inputDef);
            source.setInputDefinition(output.toString());
            getBroker().saveBeanForced(source);
            value = true;
        }
        return value;
    }

}
