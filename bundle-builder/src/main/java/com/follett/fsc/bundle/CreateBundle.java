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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.apache.log4j.Logger;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class CreateBundle {

    private static final String NODE_EXTERNAl_SRCS = "external-sources";
    private static final String NODE_TOOL_BUNDLE = "tool-bundle";

    private static final String ATTR_ID = "id";
    private static final String ATTR_PACKAGE = "package";

    private final BundleProperties bundleProperties;
    private final static Logger logger = Logger.getLogger(CreateBundle.class);

    /**
     * @param properties
     */
    protected CreateBundle(BundleProperties properties) throws Exception {
        this.bundleProperties = properties;
        createBundle();
    }

    public static void main(String[] args) throws Exception {

        Properties builderProperties = new Properties();
        String filePath = args[0];
        Path fullFilePath = new File(filePath).toPath();
        try {
            if (!Files.exists(fullFilePath)) {
                logger.error(
                        "PLEASE make sure you have a bundle.properties file that lives in <fullpath>/bundle-builder/bundle.properties\n\n");
                System.exit(1);
            }
            builderProperties.load(new FileInputStream(fullFilePath.toFile()));
        } catch (IOException e) {
            logger.error(
                    "usage: mvn install\n\nPLEASE make sure you have a bundle.properties file that lives in <fullpath>/bundle-builder/bundle.properties\n\n");
            System.exit(1);
        }
        logger.debug("\nBegin bundle-builder");
        BundleProperties properties = new BundleProperties(builderProperties, fullFilePath.getParent());
        new CreateBundle(properties);
    }


    /**
     * Parse the original bundle definition, create a new one, and create a zip
     *
     * @throws Exception
     */
    private void createBundle() throws Exception {
        logger.debug("Parse Bundle Definition [" + bundleProperties.getBundlePath() + "]");
        final List<String> externalXPaths = new ArrayList<String>();
        if(!Files.exists(Paths.get(bundleProperties.getBundlePath()))){
            throw new MissingFileException(bundleProperties.getState(), bundleProperties.getExportDirectory(), bundleProperties.getBundlePath());
        }
        try (InputStream inputBundleDef = new FileInputStream(bundleProperties.getBundlePath())) {

            DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();

            // The clean default bundle-definition that is committed in source-control
            Document originalBundleDefinitionDocument = dBuilder.parse(inputBundleDef);
            // The new generated bundle-definition we are creating.
            Document customBundleDefinitionDocument = dBuilder.newDocument();

            Node root = getCustomBundleRootNode(originalBundleDefinitionDocument, customBundleDefinitionDocument);

            XPathFactory xPathfactory = XPathFactory.newInstance();
            XPath xpath = xPathfactory.newXPath();
            String[] xPaths = bundleProperties.getXPaths();
            for (String xPathToEvaluate : xPaths) {
                XPathExpression expr = xpath.compile(xPathToEvaluate);
                NodeList nodeList = (NodeList) expr.evaluate(originalBundleDefinitionDocument, XPathConstants.NODESET);

                for (int count = 0; count < nodeList.getLength(); count++) {
                    Node xpathNode = nodeList.item(count);
                    // adding nodes according xpaths.
                    addNodeToCustom(customBundleDefinitionDocument, root, xpathNode);
                    externalXPaths.addAll(BundleDefinitionNodeParser.parseExternalSourcePaths(xpathNode));
                }
            }
            logger.debug("Xpaths: "+externalXPaths);
            customBundleDefinitionDocument.appendChild(root);
            addExternalSources(originalBundleDefinitionDocument, customBundleDefinitionDocument, externalXPaths);

            boolean versioning = bundleProperties.isVersioning();
            if (versioning) {
                addGitCommitNumberAsComment(customBundleDefinitionDocument);
            }
            File bundleDefinitionFile = writeCustomBundleDefinitionDocument(customBundleDefinitionDocument);

            createZipStaging(customBundleDefinitionDocument, bundleDefinitionFile);
        }
        logger.debug("Completed ["+bundleProperties.getZipFileName()+"]\n");
    }

    private File writeCustomBundleDefinitionDocument(Document customBundleDefinitionDocument) {

        // write the content into xml file
        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        Transformer transformer;
        try {
            transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");

            Path targetPath = bundleProperties.getBundleDir();
            DOMSource bundleDefSource = new DOMSource(customBundleDefinitionDocument);
            String bundleDefPath = bundleProperties.getDirectoryPaths(targetPath, "bundle-definition.xml");
            File bundleDefinitionFile = new File(bundleDefPath);
            StreamResult bundleDef = new StreamResult(bundleDefinitionFile);

            try {
                transformer.transform(bundleDefSource, bundleDef);
            } catch (TransformerException e) {
                e.printStackTrace();
            }
            return bundleDefinitionFile;
        } catch (TransformerConfigurationException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Adds all external sources as procedures.
     */
    private void addExternalSources(Document originalBundleDefinitionDocument,
                                    Document customBundleDefinitionDocument,
                                    List<String> externalXpaths)
                                            throws XPathExpressionException {

        // adding external sources if exist
        boolean skipExternalFlag = bundleProperties.isSkipExternal();
        List<String> skipExternalIdsList = bundleProperties.getSkipExternalIds();
        List<String> externalSourcesIds = new ArrayList<String>();

        if (customBundleDefinitionDocument == null || customBundleDefinitionDocument.getDocumentElement() == null) {
            return;
        }
        if (originalBundleDefinitionDocument == null || externalXpaths.isEmpty()) {
            return;
        }

        for (String xPath : externalXpaths) {
            XPath xpath = XPathFactory.newInstance().newXPath();
            XPathExpression expr = xpath.compile(xPath);
            Object exprResult = expr.evaluate(originalBundleDefinitionDocument, XPathConstants.NODESET);
            NodeList externalSrcList = (NodeList) exprResult;

            if (externalSrcList == null) {
                continue;
            }

            for (int count = 0; count < externalSrcList.getLength(); count++) {
                Node externalSrc = externalSrcList.item(count);
                if (externalSrc != null && externalSrc.getParentNode() != null) {
                    String nodeName = externalSrc.getParentNode().getNodeName();

                    if (!nodeName.equals(NODE_EXTERNAl_SRCS) &&
                            customBundleDefinitionDocument.getElementsByTagName(nodeName) != null) {
                        Element parent = customBundleDefinitionDocument.createElement(nodeName);

                        Node packageNamedItem = externalSrc.getParentNode().getAttributes().getNamedItem(ATTR_PACKAGE);
                        if (packageNamedItem != null) {
                            parent.setAttribute(ATTR_PACKAGE, packageNamedItem.getNodeValue());
                        }

                        String externalSourceId = externalSrc.getAttributes().getNamedItem(ATTR_ID).getNodeValue();
                        if (!skipExternalFlag && !skipExternalIdsList.contains(externalSourceId)
                                && !externalSourcesIds.contains(externalSourceId)) {
                            parent.appendChild(customBundleDefinitionDocument.adoptNode(externalSrc.cloneNode(true)));
                            customBundleDefinitionDocument.getDocumentElement().appendChild(parent);
                            externalSourcesIds.add(externalSourceId);
                        }
                    }
                }
            }
        }
    }

    /**
     * Adds the git commit number as comment.
     *
     * @param bundleDocument Document
     * @throws XPathExpressionException
     * @throws IOException
     */
    private void addGitCommitNumberAsComment(Document bundleDocument)
            throws Exception {
        String commitNumber = getGitVersionNumber();
        NodeList nodeList = BundleDefinitionNodeParser.getNodeListByXPath(bundleDocument,
                "//report-definition|//export-definition|//import-definition|//procedure-definition");
        for (int i = 0; i < nodeList.getLength(); i++) {
            ((Element) nodeList.item(i)).setAttribute("comment", commitNumber);
        }
    }

    /**
     * Parses given node and add it to the custom bundle definition.</br>
     * Also fills collection of external xpaths if any exist in given node.
     *
     */
    private void addNodeToCustom(Document customBundleDefinitionDocument,
                                 Node customBundleDefinitionRoot,
                                 Node xpathNode)
                                         throws XPathExpressionException {
        if (customBundleDefinitionDocument == null || xpathNode == null || xpathNode.getParentNode() == null) {
            return;
        }

        Node xpathParentNode = xpathNode.getParentNode();
        Element parent = customBundleDefinitionDocument.createElement(xpathParentNode.getNodeName());

        // Set the package value on the parent-element
        String packageValue = null;
        if (xpathParentNode.hasAttributes()) {
            Node namedItem = xpathParentNode.getAttributes().getNamedItem("package");
            if (namedItem != null) {
                packageValue = namedItem.getNodeValue();
            }
        }
        if (packageValue != null) {
            parent.setAttribute(ATTR_PACKAGE, packageValue);
        }

        parent.appendChild(customBundleDefinitionDocument.adoptNode(xpathNode.cloneNode(true)));
        customBundleDefinitionRoot.appendChild(parent);
    }

    /**
     * Gets the revision list and print a number stating how many commits would have been listed
     *
     * @return
     * @throws IOException
     */
    private String getGitVersionNumber() throws IOException {
        List<String> commandsList = new ArrayList<String>();
        String projectDir = bundleProperties.getStateSourcePath().toString();
        commandsList.addAll(Arrays.asList(new String[] {"git", "-C", projectDir, "rev-list", "head", "--count"}));


        ProcessBuilder processBuilder = new ProcessBuilder(commandsList.toArray(new String[commandsList.size()]));
        processBuilder.redirectErrorStream(true);
        Process process = processBuilder.start();
        BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
        process.getOutputStream().flush();
        process.getOutputStream().close();
        StringBuilder output = new StringBuilder();
        while (true) {
            String line = reader.readLine();
            if (line == null) {
                break;
            }
            output.append(line);
        }
        return output.toString();
    }

    /**
     * Creates the actual zip file
     *
     * @param bundleDefinition
     * @param bundleDefinitionFile
     * @throws IOException
     */
    private void createZipStaging(Document bundleDefinition, File bundleDefinitionFile) throws Exception {
        logger.debug("Creating zip file: "
                + Paths.get(bundleProperties.getTargetDir().toString(), bundleProperties.getZipFileName()).toString());
        Set<String> packagePaths = getPackagesPaths(bundleDefinition);

        Set<String> commonSources = new HashSet<String>();
        commonSources.addAll(getCommonExportSources(bundleDefinition));
        commonSources.addAll(getCommonReportSources(bundleDefinition));

        logger.debug("packages: ");
        logger.debug("commonSources SOURCE: "+commonSources);


        new BundleZipBuilder(bundleProperties, packagePaths, commonSources, bundleDefinitionFile).createZipFile();
    }

    private Set<String> getPackagesPaths(Document bundleDefinition) {
        Set<String> packages = new HashSet<String>();
        packages.addAll(BundleDefinitionNodeParser.getReportPaths(bundleDefinition));
        packages.addAll(BundleDefinitionNodeParser.getExportPaths(bundleDefinition));
        packages.addAll(BundleDefinitionNodeParser.getImportPaths(bundleDefinition));
        packages.addAll(BundleDefinitionNodeParser.getProcedurePaths(bundleDefinition));
        packages.addAll(BundleDefinitionNodeParser.getPortableObjectPaths(bundleDefinition));


        logger.debug("Packages: " + Arrays.toString(packages.toArray(new String[0])));
        return packages;
    }

    private List<String> getCommonReportSources(Document bundleDefinition) throws DOMException, BundleVerificationException {
        List<String> reportSources = new ArrayList<String>();
        reportSources.addAll(BundleDefinitionNodeParser.parseReportNodeSources(bundleDefinition));
        logger.debug("CommonReportSources: " + reportSources.toString());
        return reportSources;
    }

    private List<String> getCommonExportSources(Document bundleDefinition) throws DOMException, BundleVerificationException {
        List<String> exportSources = new ArrayList<String>();
        exportSources.addAll(BundleDefinitionNodeParser.parseExportNodeSources(bundleDefinition));
        exportSources.addAll(BundleDefinitionNodeParser.parseProcedureNodeSources(bundleDefinition));
        exportSources.addAll(BundleDefinitionNodeParser.parseImportNodeSources(bundleDefinition));
        exportSources.addAll(BundleDefinitionNodeParser.parsePortableNodeSources(bundleDefinition));
        logger.debug("CommonExportSources: " + exportSources.toString());
        return exportSources;
    }

    private Node getCustomBundleRootNode(Document originalBundleDefinitionDocument,
                                         Document customBundleDefinitionDocument) {
        NodeList toolBundleListOrig = originalBundleDefinitionDocument.getElementsByTagName(NODE_TOOL_BUNDLE);

        if (toolBundleListOrig.getLength() > 0) {
            Node toolNode = toolBundleListOrig.item(0);
            return customBundleDefinitionDocument.adoptNode(toolNode.cloneNode(false));
        }
        return customBundleDefinitionDocument.createElement(NODE_TOOL_BUNDLE);
    }
}
