/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.shared;

import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Stack;
import java.util.Vector;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.jdom.Attribute;
import org.jdom.Comment;
import org.jdom.Content;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.Text;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * This is a report class that performs standardized data export for
 * the state report infrastructure.
 *
 * This class will identify a procedure that contains a state report definition.
 * It will use that definition to find all data definitions and produce
 * an export file.
 *
 * This is enhanced to support XML exports.
 *
 * LIMITATION: This format cannot currently save results to the export result row table.
 *
 * @author Follett Software Company
 *
 */
public class XMLExportFormatExport extends ToolJavaSource {

    private static final long serialVersionUID = 1L;

    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    private static final String PROCEDURE_ID = "procedureId";

    /**
     * The attribute to indicate a text.
     */
    private static final String FIELD_VALUE_ATTRIBUTE = "*text";

    /**
     * The flag to indicate an element is required.
     *
     * i.e, regardless if element is empty or not, print it
     */
    private static final String REQUIRED_FLAG = "^";

    /**
     * The flag to create a comment
     */
    private static final String COMMENT_FLAG = "!";

    /**
     * The action to force an element to be created
     */
    private static final String ACTION_ADD_FLAG = "+";

    /**
     * The name of the attribute on every element.
     *
     * Its name should be complex to avoid conflicts
     */
    private static final String POST_PROCESS_ATTRIBUTE = "fscPostProcessingAttribute";

    /**
     * List of errors the export may have
     */
    private Collection<StateReportValidationError> m_initErrors = null;

    /**
     * Stack of nodes currently being processed.
     *
     * Note: Export assumes incoming fields have the same root node name
     */
    private Stack<Content> m_stack = new Stack<Content>();

    /**
     * The export's report data
     */
    private StateReportData m_reportData = null;

    /**
     * Given an xml path, parse it and attempt to add it to the stack. The stack
     * may re-adjust itself.
     *
     * @param xmlPath the incoming path
     * @param value the value to be stored in that path
     */
    protected void addToStack(String xmlPath, String value) {

        boolean required = xmlPath.endsWith(REQUIRED_FLAG);

        /*
         * Search within the xml path for /foo@bar or /foo+ or /!
         * where "foo" or "!" is required and ("@bar" OR "+" is optional)
         *
         * "foo" - is an element
         * "!" - is a comment
         * "@bar" - is an attribute ("bar") to an element
         *
         * the variable "attribute" is used until the end of this method
         */
        Pattern pattern = Pattern.compile("/([A-Za-z][-A-Za-z0-9]*|!)(([@+])(\\w*))?");
        Matcher matcher = pattern.matcher(xmlPath);
        List<Content> contents = new ArrayList<Content>(4);
        List<String> actions = new ArrayList<String>(4);
        String attribute = "";
        while (matcher.find()) {
            String tagName = matcher.group(1);
            String action = matcher.group(3);
            attribute = matcher.group(4);

            actions.add(action);
            if (tagName.equals(COMMENT_FLAG)) {
                contents.add(new Comment(value));
            } else {
                Element e = new Element(tagName);
                e.setAttribute(POST_PROCESS_ATTRIBUTE, "");
                contents.add(e);
            }
        }

        /*
         * Example:
         *
         * current stack incoming path (/root/student/dob)
         * 
         * +---------+
         * | address |x
         * |---------| +---------+
         * | person |x | dob |+
         * |---------| |---------|
         * | student | | student |
         * |---------| |---------|
         * | root | | root |
         * +---------+ +---------+
         *
         * Iterate through both stacks starting from index 0 ... n.
         *
         * Let "parent" = the current content on the stack
         * Let "temp" = the incoming content at index i
         *
         * If
         * 1. the stack's size is less than or equal to the index OR
         * 2. the current stack's content at index i is a comment OR
         * 3. the incoming content at index i is a comment OR
         * 4. the current stack's element name at index i and the incoming element name at index i
         * are not equal OR
         * 5. the current stack's element name at index i and the incoming element name at index i
         * are equal but the action contains "+"
         * Then
         * Remove all contents from index i .. end, and then
         * set temp = incoming content at index i's, and then
         * If
         * - parent = null
         * Then
         * parent = temp
         * Else
         * parent -> temp
         * parent = temp
         * Else
         * We assume that the tree's structure still holds and that
         * parent = current stack's content at index i, increment i
         * 
         * 
         * At the end, the parent becomes the last content of the incoming path (in this example,
         * it's "dob")
         *
         */
        Content parent = null;
        for (int i = 0; i < contents.size(); i++) {

            boolean stackLessThanIndex = m_stack.size() <= i;
            boolean bothElements =
                    (!stackLessThanIndex && contents.get(i) instanceof Element && m_stack.get(i) instanceof Element);
            boolean bothElementNotEqual = (bothElements && !StringUtils.isEqual(((Element) contents.get(i)).getName(),
                    ((Element) m_stack.get(i)).getName()));
            boolean bothEqualNotAction = (!bothElementNotEqual && StringUtils.isEqual(actions.get(i), ACTION_ADD_FLAG));

            if (stackLessThanIndex || // 1
                    m_stack.get(i) instanceof Comment || // 2
                    contents.get(i) instanceof Comment || // 3
                    bothElementNotEqual || // 4
                    bothEqualNotAction) // 5
            {

                // remove contents i .. n from stack
                for (int rm = m_stack.size(); rm > i; rm--) {
                    m_stack.pop();
                }

                // set the parent
                Content incomingContent = contents.get(i);
                if (parent == null) {
                    parent = incomingContent;
                } else {
                    ((Element) parent).addContent(incomingContent);
                    parent = incomingContent;
                }

                m_stack.push(parent);
            } else {
                parent = m_stack.get(i);
            }
        }

        /*
         * if the stack contains more contents than the incoming contents, remove the additional
         * ones
         */
        while (m_stack.size() > contents.size()) {
            m_stack.pop();
        }

        /*
         * finally, edit the last element (if it is) in the stack (which should be the last content
         * in the incoming xml path)
         * 
         * in addition, add flags to the element to be later evaluated
         */
        Content element = m_stack.peek();
        if (element instanceof Element) {
            String flags = ((Element) element).getAttributeValue(POST_PROCESS_ATTRIBUTE);

            if (!StringUtils.isEmpty(attribute)) {
                Attribute attr = new Attribute(attribute, value);
                ((Element) element).setAttribute(attr);
                flags = flags.concat(attribute + "=" + (!required) + "&");
            } else {
                ((Element) element).addContent(new Text(value));
                flags = flags.concat(FIELD_VALUE_ATTRIBUTE + "=" + (!required) + "&");
            }

            ((Element) element).setAttribute(POST_PROCESS_ATTRIBUTE, flags);
        }
    }

    /**
     * Gathers the data from the export.
     *
     * Passes the xml path to the addToStack() to be parsed and pushed to a stack
     *
     * @return the first element of the tree (the root)
     * @throws Exception exception
     */
    protected Element gatherData() throws Exception {

        // add any initial elements to the stack from the header
        // must be in <xmlpath>,<content> format
        String header = getHeader();
        if (!StringUtils.isEmpty(header)) {
            String[] lines = header.split(System.getProperty("line.separator"));
            for (String line : lines) {
                String[] data = line.split(",");
                String xmlPath = data.length > 0 ? data[0] : null;
                String value = data.length > 1 ? data[1] : null;

                if (!StringUtils.isEmpty(xmlPath) && !StringUtils.isEmpty(value)) {
                    addToStack(xmlPath, value);
                }

            }
        }

        if (m_initErrors.size() == 0) {
            if (m_reportData.open()) {
                try {
                    StateReportEntity entity = null;
                    while ((entity = m_reportData.next()) != null) {
                        StateReportValidationError err = entity.filterEntity();
                        if (err == null) {
                            entity.preProcess();

                            /*
                             * Add all fields
                             */
                            for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                                FieldDefinition field = m_reportData.getFieldDefinition(pos);
                                String xmlPath = field.getSifPath();
                                String fieldValue = entity.getFieldValue(pos);

                                // If the value requires padding, pad it and trim it to field max
                                // length.
                                fieldValue = ExportFormatManager.doPadding(fieldValue,
                                        (field.getResizeMode() == null
                                                ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                                : field.getResizeMode().ordinal()),
                                        field.getPaddingChar(),
                                        field.getExportLength());

                                fieldValue = fieldValue.replace(ExportJavaSource.FORMAT_EOL_WINDOWS,
                                        ExportJavaSource.FORMAT_EOL_UNIX);
                                addToStack(xmlPath, fieldValue);
                            }

                            entity.postProcess();
                        } else {
                            m_initErrors.add(err);
                        }

                    }
                } finally {
                    m_reportData.close();
                }
            }
        }

        // add any initial elements to the stack from the trailer
        // must be in <xmlpath>,<content> format
        String trailer = getTrailer();
        if (!StringUtils.isEmpty(trailer)) {
            String[] lines = trailer.split(System.getProperty("line.separator"));
            for (String line : lines) {
                String[] data = line.split(",");
                String xmlPath = data.length > 0 ? data[0] : null;
                String value = data.length > 1 ? data[1] : null;

                if (!StringUtils.isEmpty(xmlPath) && !StringUtils.isEmpty(value)) {
                    addToStack(xmlPath, value);
                }

            }
        }

        return (Element) m_stack.firstElement();
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    protected String getHeader() {
        String header = null;
        if (m_reportData != null) {
            header = m_reportData.getHeading();
        }
        return header;
    }

    /**
     * Gets the trailer.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getTrailer()
     */
    protected String getTrailer() {
        String trailer = null;
        if (m_reportData != null) {
            trailer = m_reportData.getTrailer();
        }
        return trailer;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        String procedureId = (String) getParameter(PROCEDURE_ID);
        m_initErrors = new ArrayList<StateReportValidationError>();

        // Lookup State report source data procedure
        m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);
        if (m_reportData != null && m_initErrors.size() == 0) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setCurrentContext(getCurrentContext());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setPrivilegeSet(getPrivilegeSet());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setUser(getUser());
                m_reportData.initializeExport();
            } catch (X2BaseException x2be) {
                String init_msg =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(INITIALIZE_KEY);
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }
    }

    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        String name = getJob().getTool().getName();
        String jobId = String.valueOf(getJob().getJobId());

        /*
         * Step 1: Prepare the data
         */
        Element root = gatherData();

        String[] logParameters = new String[] {name, jobId};
        AppGlobals.getLog().log(Level.INFO, LOG_DATA_PREPARED, logParameters);

        ThreadUtils.checkInterrupt();

        /*
         * If the job has been aborted then don't bother formatting the results.
         */
        if (getJob().getStatus() != ToolJob.STATUS_ABORT) {
            Document document = new Document();
            cleanNode(root);
            document.setRootElement(root);
            XMLOutputter outputter = new XMLOutputter();
            try {
                Format format = Format.getPrettyFormat();
                outputter.setFormat(format);
                outputter.output(document, getResultHandler().getOutputStream());
            } catch (IOException ioe) {
                String init_msg =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(INITIALIZE_KEY);
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, ioe.getMessage()));

                throw ioe;
            }
        }
    }

    /**
     * Given a node, recursively navigate through every child and check the flag requirements.
     *
     * @param node the node to check
     */
    private void cleanNode(Element node) {
        String flagsAsString = node.getAttributeValue(POST_PROCESS_ATTRIBUTE);
        node.removeAttribute(POST_PROCESS_ATTRIBUTE);

        Vector<Element> children = new Vector(node.getChildren());
        for (Element e : children) {
            cleanNode(e);
        }

        // remove any attributes flagged as required and are empty
        if (!StringUtils.isEmpty(flagsAsString)) {
            String[] requirements = flagsAsString.split("&");
            for (String requirement : requirements) {
                String[] data = requirement.split("=");
                String attr = data.length > 0 ? data[0] : "";
                boolean removeIfEmpty = data.length > 1 ? Boolean.parseBoolean(data[1]) : false;

                if (removeIfEmpty && attr.equals(FIELD_VALUE_ATTRIBUTE) && StringUtils.isEmpty(node.getText())) {
                    if (node.getParentElement() != null) {
                        node.getParentElement().removeContent(node);
                    }
                } else if (removeIfEmpty && StringUtils.isEmpty(node.getAttributeValue(attr))) {
                    node.removeAttribute(attr);
                }
            }
        }

        // finally, if the current node is empty and has no children nodes, remove it
        if (node.getChildren().size() == 0 && node.getAttributes().size() == 0 && node.getContentSize() == 0) {
            if (node.getParentElement() != null) {
                node.getParentElement().removeContent(node);
            }
        }
    }
}
