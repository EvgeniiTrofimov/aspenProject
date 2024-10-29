/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.imports.ImportJavaSource;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.ElementsHelper;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.NodeHandler;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisError;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisResult;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisResult.ErrorParser;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisResult.STATUS;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileReader;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.w3c.dom.Node;

/**
 * The Class OnsisErrorsImport.
 */
public class OnsisErrorsImport extends ImportJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private enum OnsisErrorParser implements ErrorParser {
        INSTANCE;

        @Override
        public void parseToBean(X2BaseBean bean, OnsisRecord.HeaderErrors headerErrors, DataDictionary dictionary) {
            for (OnsisError.ErrorField field : OnsisError.ErrorField.values()) {
                String errorFieldValue = headerErrors.getValueByElementName(field.toString());
                if (errorFieldValue != null) {
                    bean.setFieldValueByAlias(field.toString(),
                            errorFieldValue,
                            dictionary);
                }
            }
        }
    }

    private static final String NODE_NAME_DATA_ERROR_DETAILS = "DATA_ERROR_DETAILS";
    private static final String NODE_NAME_ERROR = "ERROR";
    private static final String PARAM_RESULT_NAME = "resultName";
    private static final String PARAM_RESULT_OID = "resultOid";

    /**
     * Export results.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        //
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        String resultName = (String) getParameter(PARAM_RESULT_NAME);

        SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyyMMdd_hhmmss");
        String dateStr = dateFormatter.format(new Date());

        String prefix = "OnsisImportErrors";

        String fileNamePrefix = StringUtils.isEmpty(resultName) ? prefix : resultName;
        String extension = ".txt";

        return fileNamePrefix.replaceAll("\\s", "_") + "_" + dateStr + extension;
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
        try (BufferedReader reader = new BufferedReader(new FileReader(sourceFile))) {
            String resultOid = (String) getParameter(PARAM_RESULT_OID);

            StringBuilder sourceFileStringBuilder = new StringBuilder();
            String line = null;
            // process first line separately and remove byte order mark
            line = reader.readLine();
            if (line != null) {
                if (line.indexOf("<") > 0) {
                    line = line.substring(line.indexOf("<"));
                }
                sourceFileStringBuilder.append(line);
                while ((line = reader.readLine()) != null) {
                    sourceFileStringBuilder.append(line);
                }
            }

            List<Node> errorDetailsNodes = getErrorDetailsNodes(sourceFileStringBuilder.toString());
            Filterable<ExportFormatDefinition> formats =
                    FilterableFactory.create(getBroker(), ExportFormatDefinition.class);
            ElementsHelper elementsHelper =
                    new ExsmsElementsHelper(getBroker(), new ArrayList<ExportFormatDefinition>(formats.extract()));
            OnsisResult result = null;
            if (StringUtils.isEmpty(resultOid)) {
                result = OnsisResult.createResult(getOrganization(), getBroker(),
                        (String) getParameter(PARAM_RESULT_NAME), elementsHelper);
                getBroker().saveBean(result.getBean());
            } else {
                result = OnsisResult.findResultByOid(getBroker(), resultOid, elementsHelper);
            }

            boolean isException = false;
            getBroker().beginTransaction();

            int errorsCounter = 0;
            Node errorDetailsNode = null;

            try {
                boolean isError = false;
                Iterator<Node> iter = errorDetailsNodes.iterator();
                while (iter.hasNext()) {
                    errorDetailsNode = iter.next();
                    for (int i = 0; i < errorDetailsNode.getChildNodes().getLength(); i++) {
                        Node childNode = errorDetailsNode.getChildNodes().item(i);
                        if (NODE_NAME_ERROR.equals(childNode.getNodeName())) {
                            Node parentNode = errorDetailsNode.getParentNode();
                            OnsisRecord.HeaderErrors header = new OnsisRecord.NodeHeaderErrors(parentNode);
                            OnsisRecord record = result.addRecord(header);
                            record.beforeSave();
                            getBroker().saveBeanForced(record.getBean(), true, false);
                            OnsisRecord.HeaderErrors headerErrors = new OnsisRecord.NodeHeaderErrors(childNode);
                            result.addError(record, headerErrors, OnsisErrorParser.INSTANCE);
                            isError = true;
                            errorsCounter++;
                        }
                    }
                }
                if (isError) {
                    result.updateStatus(STATUS.ERROR);
                } else {
                    result.updateStatus(STATUS.SUCCESS);
                }
            } catch (Exception e) {
                isException = true;

                /*
                 * Log the exception
                 */
                OutputStream out = getResultHandler().getOutputStream();
                String message = LoggerUtils.convertThrowableToString(e) + "\n";
                try (InputStream in = new ByteArrayInputStream(message.getBytes(StandardCharsets.UTF_8))) {
                    StreamUtils.copyStream(in, out);
                }

                /*
                 * Log the error details node
                 */
                if (errorDetailsNode != null) {
                    Node parent = errorDetailsNode.getParentNode();
                    Node nodeToPrint = (parent == null)
                            ? errorDetailsNode
                            : parent;
                    message = OnsisResultsHelper.dumpNode(nodeToPrint) + "\n";
                    try (InputStream in = new ByteArrayInputStream(message.getBytes(StandardCharsets.UTF_8))) {
                        StreamUtils.copyStream(in, out);
                    }
                }
                // throw e;
            } finally {
                String message = "Number of errors: " + errorsCounter;
                if (isException) {
                    getBroker().rollbackTransaction();
                    message = "Exception!";
                } else {
                    getBroker().commitTransaction();
                }
                try (InputStream in = new ByteArrayInputStream(message.getBytes(StandardCharsets.UTF_8));
                        OutputStream out = getResultHandler().getOutputStream()) {
                    StreamUtils.copyStream(in, out);
                }
            }

        }
    }

    /**
     * Gets the error details nodes.
     *
     * @param xmlString String
     * @return List
     */
    private List<Node> getErrorDetailsNodes(String xmlString) {
        final List<Node> errorDetailsNodes = new ArrayList<>();
        Node embeddedXmlString = OnsisResultsHelper.stringToElement(xmlString);
        OnsisResultsHelper.traverseNodeWithHandler(embeddedXmlString, new NodeHandler() {
            @Override
            public void handleNode(Node node) {
                for (int i = 0; i < node.getChildNodes().getLength(); i++) {
                    Node childNode = node.getChildNodes().item(i);
                    if (NODE_NAME_DATA_ERROR_DETAILS.equals(childNode.getNodeName())) {
                        errorDetailsNodes.add(childNode);
                    }
                }
            }
        });
        return errorDetailsNodes;
    }
}
