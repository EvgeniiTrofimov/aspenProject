/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tx;

import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.Vector;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.xml.XMLConstants;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import org.jdom.Attribute;
import org.jdom.Comment;
import org.jdom.Content;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.Text;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;
import org.xml.sax.SAXException;

/**
 * The Class TxHelper.
 */
public class TxHelper {
    /**
     * This class provides path to xsd by interchange type or resource name.
     *
     * @author Follett Software Company
     */
    public static class TxDictionary {

        /**
         * The Enum TX_DICTIONARY.
         */
        protected enum TX_DICTIONARY {
            EDUCATION_SERVICE_CENTER(
                    "EducationOrganization",
                    "EducationServiceCenter",
                    "Xsd/tx/InterchangeEducationOrganizationExtension.xsd");

            private String m_interchangeType;
            private String m_resourceName;
            private String m_xsdPath;

            /**
             * Instantiates a new tx dictionary.
             *
             * @param interchangeType String
             * @param resourceName String
             * @param xsdPath String
             */
            TX_DICTIONARY(String interchangeType, String resourceName, String xsdPath) {
                m_interchangeType = interchangeType;
                m_resourceName = resourceName;
                m_xsdPath = xsdPath;
            }

            /**
             * Gets the interchange type.
             *
             * @return String
             */
            public String getInterchangeType() {
                return m_interchangeType;
            }

            /**
             * Gets the resource name.
             *
             * @return String
             */
            public String getResourceName() {
                return m_resourceName;
            }

            /**
             * Gets the xsd path.
             *
             * @return String
             */
            public String getXsdPath() {
                return m_xsdPath;
            }
        }

        private static Map<String, TX_DICTIONARY> m_resourceToDictionaryIndex;
        private static Map<String, TX_DICTIONARY> m_interchangeTypeIndex;

        /*
         * Static initialization index maps
         */
        static {
            m_interchangeTypeIndex = new HashMap<String, TX_DICTIONARY>();
            m_resourceToDictionaryIndex = new HashMap<String, TX_DICTIONARY>();
            for (TX_DICTIONARY value : TX_DICTIONARY.values()) {
                m_resourceToDictionaryIndex.put(value.getResourceName(), value);
                m_interchangeTypeIndex.put(value.getInterchangeType(), value);
            }
        }

        /**
         * Return Xsd path by resource name from static map.
         *
         * @param resourceName String
         * @return String
         */
        public static String getXsdPath(String resourceName) {
            String xsdPath = null;

            if (m_resourceToDictionaryIndex.containsKey(resourceName)) {
                xsdPath = m_resourceToDictionaryIndex.get(resourceName).getXsdPath();
            }

            return xsdPath;
        }

        /**
         * Return Xsd path by interchange type from static map.
         *
         * @param interchangeType String
         * @return String
         */
        public static String getXsdPathByInterchangeType(String interchangeType) {
            String xsdPath = null;

            if (m_interchangeTypeIndex.containsKey(interchangeType)) {
                xsdPath = m_interchangeTypeIndex.get(interchangeType).getXsdPath();
            }

            return xsdPath;
        }
    }

    /**
     * Class responsible for building xml representation for StateReportData.
     *
     * @author X2 Development Corporation
     */
    public static class TxXmlComposer {
        /**
         * A container class for returning information about a single generated Element
         * from an iteration of generated Elements.
         *
         * @author Follett Software Company
         */
        public static class ElementContainer {
            Element m_element;
            boolean m_valid;
            SAXException m_validationException;

            /**
             * Constructor.
             *
             * @param element Element
             * @param valid boolean
             * @param validationException SAXException
             */
            ElementContainer(Element element, boolean valid, SAXException validationException) {
                m_element = element;
                m_valid = valid;
                m_validationException = validationException;
            }

            /**
             * Returns the Element.
             *
             * @return Element
             */
            public Element getElement() {
                return m_element;
            }

            /**
             * Return the XML validation exception object for this Element.
             *
             * @return SAXException
             */
            public SAXException getValidationException() {
                return m_validationException;
            }

            /**
             * Returns the valid indicator for the element.
             *
             * @return boolean
             */
            public boolean isValid() {
                return m_valid;
            }
        }

        /**
         * The action to force an element to be created
         */
        private static final String ACTION_ADD_FLAG = "+";

        private static final String AND = "&";

        /**
         * The flag to create a comment
         */
        private static final String COMMENT_FLAG = "!";
        private static final String EMPTY_STRING = "";

        private static final String EQUAL = "=";

        /**
         * The attribute to indicate a text.
         */
        private static final String FIELD_VALUE_ATTRIBUTE = "*text";

        private static final String INITIALIZE_KEY = "label.state.report.initialize";

        private static final String KEYWORD_INTERCHANGE = "Interchange";

        private static final String LESS = "<";

        /**
         * The name of the attribute on every element.
         *
         * Its name should be complex to avoid conflicts
         */
        private static final String POST_PROCESS_ATTRIBUTE = "fscPostProcessingAttribute";
        private static final String R_N = "\\r\\n";

        private static final String REG_EXP_PATTERN = "/([A-Za-z][-A-Za-z0-9]*|!)(([@])([A-Za-z][-A-Za-z0-9]*))?";

        /**
         * The flag to indicate an element is required.
         *
         * i.e, regardless if element is empty or not, print it
         */
        private static final String REQUIRED_FLAG = "^";

        /**
         * Parser used to process values as xml strings
         */
        private SAXBuilder m_builder = null;

        private Collection<Element> m_elements = new ArrayList<Element>();

        private String m_interchangeType = null;

        private Map<Element, SAXException> m_invalidElements = new HashMap<Element, SAXException>();

        private SAXException m_lastValidationException = null;

        private XMLOutputter m_outputter = null;

        private Schema m_schema = null;

        private SchemaFactory m_schemaFactory = null;

        /**
         * Stack of nodes currently being processed.
         *
         * Note: Export assumes incoming fields have the same root node name
         */
        private Stack<Content> m_stack = new Stack<Content>();

        /**
         * List of errors the export may have
         */
        private Collection<StateReportValidationError> m_stateReportErrors = null;

        /**
         * The export's report data
         */
        private StateReportData m_reportData = null;

        private Validator m_validator = null;

        private boolean m_isValid = false;

        /**
         * List of xml elements
         */
        private Collection<Element> m_validElements = new ArrayList<Element>();

        private String m_xmlHeader = null;

        private String m_xmlFooter = null;

        private String m_xsdName = null;

        private String m_xsdPath = null;

        /**
         * Constructor.
         *
         * @param stateReportData StateReportData
         * @throws X2BaseException exception
         */
        public TxXmlComposer(StateReportData stateReportData) throws X2BaseException {
            this(stateReportData, null, false);
        }

        /**
         * Constructor.
         *
         * @param stateReportData StateReportData
         * @param initializationErrors Collection<StateReportValidationError>
         * @param asIterator boolean
         * @throws X2BaseException exception
         */
        public TxXmlComposer(StateReportData stateReportData,
                Collection<StateReportValidationError> initializationErrors, boolean asIterator)
                throws X2BaseException {
            m_stateReportErrors = new ArrayList<StateReportValidationError>();
            if (initializationErrors != null) {
                m_stateReportErrors.addAll(initializationErrors);
            }
            m_reportData = stateReportData;

            setXsdPath(TxDictionary.getXsdPathByInterchangeType(getResourceName()));

            if (!asIterator) {
                processData();
            }
        }

        /**
         * Constructor.
         *
         * @param xml String
         * @throws ParserConfigurationException exception
         * @throws SAXException exception
         * @throws IOException Signals that an I/O exception has occurred.
         * @throws JDOMException exception
         */
        public TxXmlComposer(String xml) throws ParserConfigurationException, SAXException, IOException, JDOMException {
            String[] strings = xml.split(R_N);
            List<String> list = Arrays.asList(strings);
            StringBuilder builder = new StringBuilder();
            for (int i = 1; i < list.size() - 1; i++) {
                String s = list.get(i);
                if (!s.equals(EMPTY_STRING)) {
                    builder.append(s);
                }
            }
            String body = builder.toString();
            setXmlHeader(list.get(0));
            setXmlFooter(list.get(list.size() - 1));

            // create element from xml
            SAXBuilder sb = new SAXBuilder();
            Document doc = sb.build(new StringReader(body));
            Element element = doc.getRootElement();

            setXsdPath(TxDictionary.getXsdPath(getResourceName(element)));
            addElement(element);
        }

        /**
         * Constructor.
         *
         * @param xsdName String
         * @param broker X2Broker
         */
        public TxXmlComposer(String xsdName, X2Broker broker) {
            initializeXsdSchema(xsdName);
        }

        /**
         * Get complete xml (which contains header, set of repeatable elements and trailer).
         *
         * @return String
         * @throws IOException Signals that an I/O exception has occurred.
         */
        public String getCompleteXml() throws IOException {
            ByteArrayOutputStream memOut = new ByteArrayOutputStream();
            PrintStream printStream = new PrintStream(memOut);

            printStream.print(m_xmlHeader);
            printStream.print(ExportJavaSource.FORMAT_EOL_WINDOWS);
            for (Element element : getValidElements()) {
                m_outputter.output(element, memOut);
                printStream.print(ExportJavaSource.FORMAT_EOL_WINDOWS);
            }
            printStream.print(m_xmlFooter);

            return memOut.toString();
        }

        /**
         * Get elements.
         * 
         * @return elements
         */
        public Collection<Element> getElements() {
            return m_elements;
        }

        /**
         * Get element xml.
         *
         * @param element Element
         * @return String
         * @throws IOException Signals that an I/O exception has occurred.
         */
        public String getElementXml(Element element) throws IOException {
            ByteArrayOutputStream memOut = new ByteArrayOutputStream();
            PrintStream printStream = new PrintStream(memOut);

            printStream.print(getHeader());
            printStream.print(ExportJavaSource.FORMAT_EOL_WINDOWS);

            getXMLOutputter().output(element, memOut);
            printStream.print(ExportJavaSource.FORMAT_EOL_WINDOWS);

            printStream.print(getTrailer());

            return memOut.toString();
        }

        /**
         * Gets the element xml reduced.
         *
         * @param element Element
         * @return String
         * @throws IOException Signals that an I/O exception has occurred.
         */
        public String getElementXmlReduced(Element element) throws IOException {
            ByteArrayOutputStream memOut = new ByteArrayOutputStream();
            // PrintStream printStream = new PrintStream(memOut); //Not Use

            getXMLOutputter().output(element, memOut);

            return memOut.toString();
        }

        /**
         * Getting interchange type from xml root element (header).
         *
         * @return String
         */
        public String getInterchangeType() {
            if (m_interchangeType == null && m_reportData != null) {
                m_interchangeType = m_reportData.getHeading().substring(1, m_reportData.getHeading().indexOf(" "));
            }

            else if (m_interchangeType == null && m_reportData == null) {
                m_interchangeType = m_xmlHeader.substring(1, m_xmlHeader.indexOf(" "));
            }

            return m_interchangeType;
        }

        /**
         * Gets the invalid elements.
         *
         * @return invalid elements that appear during processing
         */
        public Map<Element, SAXException> getInvalidElements() {
            return m_invalidElements;
        }

        /**
         * Get resource name (no argument).
         * 
         * @return String
         */
        public String getResourceName() {
            return getInterchangeType().substring(KEYWORD_INTERCHANGE.length());
        }

        /**
         * Get resource name by element.
         *
         * @param element Element
         * @return String
         */
        public String getResourceName(Element element) {
            return element.getName();
        }

        /**
         * Get valid elements.
         * 
         * @return elements
         */
        public Collection<Element> getValidElements() {
            return m_validElements;
        }

        /**
         * Get xml outputter.
         * 
         * @return XMLOutputter
         */
        public XMLOutputter getXMLOutputter() {
            if (m_outputter == null) {
                m_outputter = new XMLOutputter();
                Format format = Format.getPrettyFormat();
                m_outputter.setFormat(format);
            }
            return m_outputter;
        }

        /**
         * Get xsd path.
         * 
         * @return String
         */
        public String getXsdPath() {
            return m_xsdPath;
        }

        /**
         * Returns an indicator if the last element returned from "next()" is valid.
         * This indicator is equivalent to the element being in the getValidElements() list or the
         * getInvalidElements() list.
         * true indicates the last element was a valid element.
         *
         * @return boolean
         */
        public boolean isValid() {
            return m_isValid;
        }

        /**
         * Returns the next generated object from the statReportData.
         * Must be called after openIterator().
         *
         * @return ElementContainer
         * @throws X2BaseException exception
         */
        public ElementContainer next() throws X2BaseException {
            if (m_reportData == null) {
                // Can only use this mode if using a stateReportData.
                throw new X2RuntimeException();
            }

            ElementContainer container = null;
            // loop until a usable record is returned. Possibly skipped due to entity.filterEntity()
            // or completely empty content.
            while (container == null) {
                StateReportEntity entity = m_reportData.next();
                if (entity != null) {
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

                        if (!m_stack.isEmpty()) {
                            Element root = (Element) m_stack.firstElement();
                            Document document = new Document();
                            cleanNode(root);
                            document.setRootElement(root);
                            try {
                                boolean valid = false;
                                if (validateXmlElement(root)) {
                                    valid = true;
                                }
                                container = new ElementContainer(root, valid, m_lastValidationException);
                                addElement(root);
                            } catch (IOException ioe) {
                                String init_msg =
                                        LocalizationCache.getMessages(m_reportData.getBroker().getPersistenceKey())
                                                .getMessage(INITIALIZE_KEY);
                                m_stateReportErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg,
                                        ioe.getMessage()));
                            }
                        }

                        m_stack.clear();

                    }
                } else {
                    // if reportData returned null, this is the real end of the list. Exit the loop
                    // and return a real null.
                    m_reportData.close();
                    break;
                }
            }

            return container;
        }

        /**
         * Open the stateReportData for iteration.
         * This allows individual record result iteration through the method "next()".
         * if the return value is false, the iterator is not open and should not be used.
         *
         * @return boolean
         */
        public boolean openIterator() {
            if (m_reportData == null) {
                // Can only use this mode if using a stateReportData.
                throw new X2RuntimeException();
            }
            return m_reportData.open();
        }

        /**
         * Set xsd path.
         *
         * @param xsdPath void
         */
        public void setXsdPath(String xsdPath) {
            m_xsdPath = xsdPath;
            initializeXsdSchema();
        }

        /**
         * Validate passed xml across xsd.
         *
         * @param xml String
         * @return true, if successful
         * @throws IOException Signals that an I/O exception has occurred.
         */
        public boolean validateXml(String xml) throws IOException {
            ByteArrayInputStream memIn = new ByteArrayInputStream(xml.getBytes());
            Source xmlFile = new StreamSource(memIn);
            boolean isXmlValid = false;

            try {
                m_validator.validate(xmlFile);
                m_lastValidationException = null;
                isXmlValid = true;

            } catch (SAXException e) {
                m_lastValidationException = e;
                isXmlValid = false;
            }

            return isXmlValid;
        }

        /**
         * Validate passed element across xsd.
         *
         * @param element Element
         * @return true, if successful
         * @throws IOException Signals that an I/O exception has occurred.
         */
        public boolean validateXmlElement(Element element) throws IOException {
            ByteArrayOutputStream memOut = new ByteArrayOutputStream();
            PrintStream printStream = new PrintStream(memOut);

            printStream.print(getHeader());
            if (!element.getContent().isEmpty()) {
                printStream.print(ExportJavaSource.FORMAT_EOL_WINDOWS);
                getXMLOutputter().output(element, memOut);
                printStream.print(ExportJavaSource.FORMAT_EOL_WINDOWS);
            }
            printStream.print(getTrailer());

            return validateXml(memOut.toString());
        }

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
            Pattern pattern = Pattern.compile(REG_EXP_PATTERN);
            Matcher matcher = pattern.matcher(xmlPath);
            List<Content> contents = new ArrayList<Content>(4);
            List<String> actions = new ArrayList<String>(4);
            String attribute = EMPTY_STRING;
            while (matcher.find()) {
                String tagName = matcher.group(1);
                String action = matcher.group(3);
                attribute = matcher.group(4);

                actions.add(action);
                if (tagName.equals(COMMENT_FLAG)) {
                    contents.add(new Comment(value));
                } else {
                    Element e = new Element(tagName);
                    e.setAttribute(POST_PROCESS_ATTRIBUTE, EMPTY_STRING);
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
             * 4. the current stack's element name at index i and the incoming element name at index
             * i are not equal OR
             * 5. the current stack's element name at index i and the incoming element name at index
             * i are equal but the action contains "+"
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
             * At the end, the parent becomes the last content of the incoming path (in this
             * example, it's "dob")
             *
             */
            Content parent = null;
            for (int i = 0; i < contents.size(); i++) {

                boolean stackLessThanIndex = m_stack.size() <= i;
                boolean bothElements = (!stackLessThanIndex && contents.get(i) instanceof Element
                        && m_stack.get(i) instanceof Element);
                boolean bothElementNotEqual =
                        (bothElements && !StringUtils.isEqual(((Element) contents.get(i)).getName(),
                                ((Element) m_stack.get(i)).getName()));
                boolean bothEqualNotAction =
                        (!bothElementNotEqual && StringUtils.isEqual(actions.get(i), ACTION_ADD_FLAG));

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
             * finally, edit the last element (if it is) in the stack (which should be the last
             * content in the incoming xml path)
             * 
             * in addition, add flags to the element to be later evaluated
             */
            Content element = m_stack.peek();
            if (element instanceof Element) {
                String flags = ((Element) element).getAttributeValue(POST_PROCESS_ATTRIBUTE);

                if (!StringUtils.isEmpty(attribute)) {
                    Attribute attr = new Attribute(attribute, value);
                    ((Element) element).setAttribute(attr);
                    flags = flags.concat(attribute + EQUAL + (!required) + AND);
                } else {
                    boolean addAttribute = true;
                    Element e = (Element) element;
                    String name = e.getName();
                    if (value.startsWith(LESS + name)) {
                        try {
                            if (m_builder == null) {
                                m_builder = new SAXBuilder();
                            }
                            boolean detachElement = false;
                            Element eParent = e.getParentElement();
                            org.jdom.Document document = m_builder.build(new ByteArrayInputStream(value.getBytes()));
                            Element root = document.getRootElement();
                            if (root.getName().equals(name)) {
                                Iterator itr = (root.getChildren()).iterator();
                                while (itr.hasNext()) {
                                    Element child = (Element) itr.next();
                                    Element newChild = (Element) child.clone();
                                    String childName = child.getName();
                                    newChild.detach();
                                    if (childName.equals(name)) {
                                        detachElement = true;
                                        eParent.addContent(newChild);
                                    } else {
                                        e.addContent(newChild);
                                        addAttribute = false;
                                    }
                                }
                                if (detachElement) {
                                    e.detach();
                                }
                            } else {
                                e.addContent(new Text(value));
                            }
                        } catch (Exception e1) {
                            e.addContent(new Text(value));
                        }
                    } else {
                        e.addContent(new Text(value));
                    }
                    if (addAttribute) {
                        flags = flags.concat(FIELD_VALUE_ATTRIBUTE + EQUAL + (!required) + AND);
                    }
                }

                ((Element) element).setAttribute(POST_PROCESS_ATTRIBUTE, flags);
            }
        }

        /**
         * Return header element for xml.
         *
         * @return String
         */
        protected String getHeader() {
            if (m_xmlHeader == null && m_reportData != null) {
                m_xmlHeader = m_reportData.getHeading();
            }

            return m_xmlHeader;
        }

        /**
         * Return trailer element for xml.
         *
         * @return String
         */
        protected String getTrailer() {
            if (m_xmlFooter == null && m_reportData != null) {
                m_xmlFooter = m_reportData.getTrailer();
            }
            return m_xmlFooter;
        }

        /**
         * Process data.
         *
         * @throws X2BaseException exception
         */
        protected void processData() throws X2BaseException {
            if (m_stateReportErrors.size() == 0) {
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

                                    // If the value requires padding, pad it and trim it to field
                                    // max length.
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

                                if (!m_stack.isEmpty()) {
                                    Element root = (Element) m_stack.firstElement();
                                    Document document = new Document();
                                    cleanNode(root);
                                    document.setRootElement(root);
                                    try {
                                        addElement(root);
                                    } catch (IOException ioe) {
                                        String init_msg = LocalizationCache
                                                .getMessages(m_reportData.getBroker().getPersistenceKey())
                                                .getMessage(INITIALIZE_KEY);
                                        m_stateReportErrors
                                                .add(new StateReportValidationError(init_msg, init_msg, init_msg,
                                                        ioe.getMessage()));
                                    }
                                }

                                m_stack.clear();

                            } else {
                                m_stateReportErrors.add(err);
                            }

                        }
                    } finally {
                        m_reportData.close();
                    }
                }
            }
        }

        /**
         * Add new element. Method validate passed element across xsd, and put it in list of
         * valid/invalid elements
         *
         * @param element Element
         * @return true, if successful
         * @throws IOException Signals that an I/O exception has occurred.
         */
        private boolean addElement(Element element) throws IOException {
            boolean isAdded = false;
            m_elements.add(element);
            if (validateXmlElement(element)) {
                m_validElements.add(element);
                isAdded = true;
            } else {
                m_invalidElements.put(element, m_lastValidationException);
            }

            return isAdded;
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
                String[] requirements = flagsAsString.split(AND);
                for (String requirement : requirements) {
                    String[] data = requirement.split(EQUAL);
                    String attr = data.length > 0 ? data[0] : EMPTY_STRING;
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

        /**
         * Initialize xsd schema.
         */
        private void initializeXsdSchema() {
            m_schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            try {
                m_schema = m_schemaFactory.newSchema(getClass().getClassLoader().getResource(m_xsdPath));
            } catch (SAXException exc) {
                AppGlobals.getLog().log(Level.SEVERE, "TXXmlComposer: XSD schema " + m_xsdPath + " cannot be loaded.");
            }

            m_validator = m_schema.newValidator();
        }

        /**
         * Initialize xsd schema.
         *
         * @param xsdName String
         */
        private void initializeXsdSchema(String xsdName) {
            m_xsdName = xsdName;
            m_schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            try {
                m_schema = m_schemaFactory.newSchema(getClass().getClassLoader().getResource(m_xsdName));
            } catch (SAXException exc) {
                AppGlobals.getLog().log(Level.SEVERE, "TXXmlComposer: XSD schema " + m_xsdName + " cannot be loaded.");
            }
        }

        /**
         * Sets the xml footer.
         *
         * @param footer void
         */
        private void setXmlFooter(String footer) {
            m_xmlFooter = footer;

        }

        /**
         * Sets the xml header.
         *
         * @param header void
         */
        private void setXmlHeader(String header) {
            m_xmlHeader = header;
        }
    }

}
