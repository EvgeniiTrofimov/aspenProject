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
package com.follett.fsc.bundle;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class BundleDefinitionNodeParser {

    private final static Logger logger = Logger.getLogger(BundleDefinitionNodeParser.class);

    //@formatter:off
    public enum NodeName {
        EXPORT_DEF("export-definition"), IMPORT_DEF("import-definition"), PROCEDURE_DEF("procedure-definition"),
        REPORT_DEF("report-definition"), PORTABLE_DEF("portable-object-definition"), PRCDS("procedures"),
        REPORTS("reports"), IMPORTS("imports"), EXPORTS("exports"), PORT_OBJECTS("portable-objects"),
        EXTERNAl_SRCS("external-sources"), EXTERNAl_SRC("external-source");

        private String nodeName;

        /**
         * @param string
         */
        NodeName(String string) {
            this.nodeName = string;
        }
    }

    public enum NodeAttribute {
        INPUT_FILE("input-file"), JAVA_FILE("javasource-file"), DEF_FILE("definition-file"), REP_DESIGN_FILE("design-file"),
        PACKAGE("package");

        private String nodeName;

        /**
         * @param string
         */
        NodeAttribute(String string) {
            this.nodeName = string;
        }
    }
    //@formatter:on

    public static List<String> parseExportNodeSources(Document bundleDefinition) throws DOMException, BundleVerificationException {
        return parseNodeSources(bundleDefinition, NodeName.EXPORT_DEF, NodeAttribute.JAVA_FILE,
                NodeAttribute.INPUT_FILE);
    }

    public static List<String> parseProcedureNodeSources(Document bundleDefinition) throws DOMException, BundleVerificationException {
        return parseNodeSources(bundleDefinition, NodeName.PROCEDURE_DEF, NodeAttribute.JAVA_FILE,
                NodeAttribute.INPUT_FILE);
    }

    public static List<String> parseImportNodeSources(Document bundleDefinition) throws DOMException, BundleVerificationException {
        return parseNodeSources(bundleDefinition, NodeName.IMPORT_DEF, NodeAttribute.JAVA_FILE,
                NodeAttribute.INPUT_FILE, NodeAttribute.DEF_FILE);
    }

    public static List<String> parsePortableNodeSources(Document bundleDefinition) throws DOMException, BundleVerificationException {
        return parseNodeSources(bundleDefinition, NodeName.PORTABLE_DEF, NodeAttribute.JAVA_FILE,
                NodeAttribute.INPUT_FILE, NodeAttribute.DEF_FILE);
    }

    public static List<String> parseReportNodeSources(Document bundleDefinition) throws DOMException, BundleVerificationException {
        return parseNodeSources(bundleDefinition, NodeName.REPORT_DEF, NodeAttribute.JAVA_FILE,
                NodeAttribute.INPUT_FILE, NodeAttribute.REP_DESIGN_FILE);
    }

    public static Set<String> getReportPaths(Document bundleDefinition) {
        return parseNodePackagePath(bundleDefinition, NodeName.REPORTS);
    }

    public static Set<String> getExportPaths(Document bundleDefinition) {
        return parseNodePackagePath(bundleDefinition, NodeName.EXPORTS);
    }

    public static Set<String> getImportPaths(Document bundleDefinition) {
        return parseNodePackagePath(bundleDefinition, NodeName.IMPORTS);
    }

    public static Set<String> getProcedurePaths(Document bundleDefinition) {
        return parseNodePackagePath(bundleDefinition, NodeName.PRCDS);
    }

    public static Set<String> getPortableObjectPaths(Document bundleDefinition) {
        return parseNodePackagePath(bundleDefinition, NodeName.PORT_OBJECTS);
    }

    public static NodeList getNodeListByXPath(Document document, String xPathExpression)
            throws XPathExpressionException {
        XPath xPath = XPathFactory.newInstance().newXPath();
        return (NodeList) xPath.compile(xPathExpression).evaluate(document, XPathConstants.NODESET);
    }

    public static List<String> parseExternalSourcePaths(Node node){
        List<String> sources = new ArrayList<String>();

        NodeList children = node.getChildNodes();
        if (children == null) {
            return sources;
        }

        for (int count = 0; count < children.getLength(); count++) {
            Node childNode = children.item(count);
            if(childNode == null) {
                continue;
            }
            if (NodeName.EXTERNAl_SRCS.nodeName.equals(childNode.getNodeName()) && childNode.hasChildNodes()) {
                NodeList externalSourceChildren = childNode.getChildNodes();

                for (int extSrcCount = 0; extSrcCount < externalSourceChildren.getLength(); extSrcCount++) {
                    Node externalSource = externalSourceChildren.item(extSrcCount);
                    if (externalSource != null
                            && NodeName.EXTERNAl_SRC.nodeName.equals(externalSource.getNodeName())) {
                        sources.add("//*[@id=\"" +
                                externalSource.getAttributes().getNamedItem("id").getNodeValue() +
                                "\"]");
                    }
                }
            }
        }
        return sources;
    }

    private static Set<String> parseNodePackagePath(Document bundleDefinition, NodeName nodeName) {
        Set<String> paths = new HashSet<String>();

        NodeList nodes = bundleDefinition.getElementsByTagName(nodeName.nodeName);
        if (nodes == null) {
            return paths;
        }

        for (int count = 0; count < nodes.getLength(); count++) {
            Node node = nodes.item(count);
            Node namedItem = node.getAttributes().getNamedItem(NodeAttribute.PACKAGE.nodeName);
            if (namedItem != null) {
                paths.add(namedItem.getNodeValue().replaceAll("\\.", "/"));
            }
        }
        return paths;

    }

    private static List<String> parseNodeSources(Document bundleDefinition,
                                                 NodeName nodeName,
                                                 NodeAttribute... attributes) throws DOMException, BundleVerificationException {
        List<String> sources = new ArrayList<String>();

        NodeList nodes = bundleDefinition.getElementsByTagName(nodeName.nodeName);

        if (nodes == null) {
            return sources;
        }

        for (int count = 0; count < nodes.getLength(); count++) {
            Node node = nodes.item(count);
            for (NodeAttribute attribute : attributes) {
                Node namedItem = node.getAttributes().getNamedItem(attribute.nodeName);
                if (namedItem != null) {
                    if(StringUtils.isBlank(namedItem.getNodeValue())) {
                        Node nodeId = node.getAttributes().getNamedItem("id");
                        throw new BundleVerificationException("Blank item for: "+namedItem.getNodeName()+" "+node.getNodeName()+": id="+nodeId.getNodeValue());
                    }
                    sources.add(namedItem.getNodeValue());
                }
            }
        }
        return sources;
    }
}
