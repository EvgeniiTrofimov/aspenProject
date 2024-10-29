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
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */


import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.DataTableConfig.OrganizationAccess;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.OrganizationChild;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryRelationship;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebConstants;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.k12.web.nav.Filter;
import com.x2dev.sis.model.beans.AssessmentColumnDefinition;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Serializable;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.Format;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * Java source for imports that are based solely on an XML definition file.
 *
 * @author X2 Development Corporation
 */
public class PARCCXmlDefinitionImport extends TextImportJavaSource {
    /**
     * Class that reads and processes XML import definitions. This class is capable of both parsing
     * the XML and performing
     * the actual import of data from a Map of values. Methods are provided to monitor the results
     * of the import (e.g.
     * number of records inserted, matched, updated, and skipped).
     *
     * This class was enhanced to include the use of aliases on extended dictionary fields for
     * assessments
     *
     * @author X2 Development Corporation
     */
    public class PARCCXmlDefinitionManager implements Serializable {
        /**
         * Mode constant indicating that records should be both inserted and updated.
         */
        public static final String MODE_BOTH = "both";

        /**
         * Mode constant indicating that records should be inserted only.
         */
        public static final String MODE_INSERT = "insert";

        /**
         * Mode constant indicating that records should be updated only.
         */
        public static final String MODE_UPDATE = "update";

        /**
         * Constant for bean-save validation errors.
         */
        public static final int ERROR_VALIDATION = 100;

        /**
         * Constant for invalid foreign key errors.
         */
        public static final int ERROR_UNRESOLVED_FOREIGN_KEY = 200;

        /**
         * Constant for multiply resolved match key errors.
         */
        public static final int ERROR_MULTIPLY_RESOLVED_MATCH_KEY = 250;

        /**
         * Constant for imports happening at a level below root which have no organization path.
         */
        public static final int ERROR_NO_ORGANIZATION_PATH = 300;

        // Constants for XML parsing
        protected static final String DIRECT_FIELD_ELEMENT = "direct-field";
        protected static final String DIRECT_FIELDS_ELEMENT = "direct-fields";

        protected static final String FIELD_ID_ATTRIBUTE = "id";
        protected static final String FIELD_LENGTH_ATTRIBUTE = "length";
        protected static final String FIELD_FORMAT_ATTRIBUTE = "format";
        protected static final String FIELD_MATCH_ATTRIBUTE = "match";
        protected static final String FIELD_REPEAT_ATTRIBUTE = "repeat";
        protected static final String FIELD_SOURCE_ID_ATTRIBUTE = "source-id";
        protected static final String FIELD_SOURCE_TYPE_ATTRIBUTE = "source-type";
        protected static final String FIELD_TYPE_ATTRIBUTE = "type";
        protected static final String FIELD_TYPE_ALIAS = "alias";

        protected static final String FORMATTER_DECIMAL = "decimal";
        protected static final String PRESET_FIELD_ELEMENT = "preset-field";
        protected static final String PRESET_FIELD_SOURCE_ATTRIBUTE = "source";
        protected static final String PRESET_FIELD_VALUE_ATTRIBUTE = "value";
        protected static final String PRESET_FIELDS_ELEMENT = "preset-fields";

        protected static final String REQUIREMENT_ELEMENT = "requirement";
        protected static final String REQUIREMENT_OPERATOR_ATTRIBUTE = "operator";
        protected static final String REQUIREMENT_SOURCE_ATTRIBUTE = "source";
        protected static final String REQUIREMENT_VALUE_ATTRIBUTE = "value";

        protected static final String ROOT_ALLOW_MULTIPLE_MATCH_ATTRIBUTE = "allow-multi-match";
        protected static final String ROOT_ESCAPE_CHARACTER_ATTRIBUTE = "escape-character";
        protected static final String ROOT_FORCED_ATTRIBUTE = "force";
        protected static final String ROOT_MODE_ATTRIBUTE = "mode";
        protected static final String ROOT_STRICT_LOOKUP_ATTRIBUTE = "strict-lookup";
        protected static final String ROOT_SKIP_FIRST_ROW_ATTRIBUTE = "skip-first-row";
        protected static final String ROOT_TABLE_ID_ATTRIBUTE = "table-id";
        protected static final String ROOT_VALUE_DELIMITER_ATTRIBUTE = "value-delimiter";
        protected static final String ROOT_VALUE_WRAPPER_ATTRIBUTE = "value-wrapper";
        protected static final String ROOT_VALUE_WRAPPING_MODE = "value-wrapping-mode";
        protected static final String VALUE_WRAPPING_MODE_OPTIONAL = "optional";
        protected static final String VALUE_WRAPPING_MODE_REQUIRED = "required";

        /**
         *
         */
        private static final long serialVersionUID = 1L;

        private List<KeyValuePair<String, Class>> m_affectedBeanOids;
        private boolean m_allowMultipleMatches;
        private Class m_beanClass;
        private ArrayList<ModelProperty> m_directFields; // An ArrayList is used because it may
                                                         // contain null elements
        private HashMap<ModelProperty, ModelProperty> m_directFieldRepeats;
        private ArrayList m_formats;
        private LinkedList<KeyValuePair<Integer, String>> m_errors;
        private char m_escapeCharacter = '\\';
        private int[] m_fieldLengths;
        private boolean m_force;
        private Map m_inputParameters;
        private Locale m_locale;
        private List m_matchFields;
        private String m_matchKey;
        private String m_mode;
        private Map m_presetFields;
        private Map m_requirements;
        private boolean m_strictLookup = true;
        private String m_uniqueKey;
        private boolean m_useEscapes = true;
        private boolean m_useValueDelimiters = true;
        private boolean m_useValueWrappers = true;
        private char m_valueDelimiter = ',';
        private char m_valueWrapper = '"';
        private VALUE_WRAPPING_MODE m_valueWrappingMode;
        private Organization m_organization;
        private int m_organizationLevel;

        private int m_inserted = 0;
        private int m_matched = 0;
        private int m_skipped = 0;
        private int m_updated = 0;

        // These transients are reset on deserialization by XmlDefinitionImport
        private transient X2Broker m_broker;
        private transient DataDictionary m_dictionary;

        private Map<String, AssessmentColumnDefinition> m_acdMapByAlias;
        private Map<ModelProperty, String> m_headings;
        private Map<ModelProperty, Format> m_formatters;

        /**
         * Constructs a new XmlDefinitionManager.
         *
         * @param importElement Element
         * @param inputParameters Map<String,Object>
         * @param userData UserDataContainer
         * @param broker X2Broker
         * @param dictionary DataDictionary
         * @param locale Locale
         * @param organization Organization
         * @throws X2BaseException exception
         */
        public PARCCXmlDefinitionManager(Element importElement, Map<String, Object> inputParameters,
                UserDataContainer userData,
                X2Broker broker, DataDictionary dictionary, Locale locale, Organization organization)
                throws X2BaseException {
            initialize(importElement, inputParameters, userData, broker, dictionary, locale, organization);
        }

        /**
         * Constructs a new XmlDefinitionManager.
         *
         * @param xmlDefinition byte[]
         * @param inputParameters Map<String,Object>
         * @param userData UserDataContainer
         * @param broker X2Broker
         * @param dictionary DataDictionary
         * @param locale Locale
         * @param organization Organization
         * @throws X2BaseException exception
         */
        public PARCCXmlDefinitionManager(byte[] xmlDefinition, Map<String, Object> inputParameters,
                UserDataContainer userData,
                X2Broker broker, DataDictionary dictionary, Locale locale, Organization organization)
                throws X2BaseException {
            try {
                ByteArrayInputStream stream = new ByteArrayInputStream(xmlDefinition);

                SAXBuilder builder = new SAXBuilder();
                Document document = builder.build(stream);
                Element root = document.getRootElement();

                initialize(root, inputParameters, userData, broker, dictionary, locale, organization);
            } catch (IOException ioe) {
                throw new X2BaseException(ioe);
            } catch (JDOMException jde) {
                throw new X2BaseException(jde);
            }
        }

        /**
         * Constructs a new XmlDefinitionManager. Note: this constructor does not initialize the
         * object. It should not be
         * used in client code.
         */
        protected PARCCXmlDefinitionManager() {
            // Empty constructor
        }

        /**
         * Imports a single "row" of data based on the XML definition provided at construction. The
         * values to import are contained the passed map.
         * <p>
         * The results of this method will be reflected immediately after completion in the skipped,
         * inserted, matched, and updated counts. Any errors that occur can be retrieved with a call
         * to
         * <code>getErrors()</code>.
         *
         * @param directValuesMap Map<ModelProperty,Object>
         */
        public void doImport(Map<ModelProperty, Object> directValuesMap) {
            m_inserted = 0;
            m_matched = 0;
            m_skipped = 0;
            m_updated = 0;

            m_errors.clear();

            if (validateRequirements(directValuesMap)) {
                QueryIterator beans = getMatchingBeans(directValuesMap);
                try {
                    if (beans == null || !beans.hasNext()) {
                        if (getMode().equals(PARCCXmlDefinitionManager.MODE_UPDATE)) {
                            m_skipped++;
                        } else {
                            X2BaseBean bean = X2BaseBean.newInstance(m_beanClass, m_broker.getPersistenceKey());

                            if (bean instanceof OrganizationChild) {
                                OrganizationManager.setOrganizationOids((OrganizationChild) bean, m_organization);
                            }

                            updateBean(bean, directValuesMap);
                        }
                    } else {
                        if (getMode().equals(PARCCXmlDefinitionManager.MODE_INSERT)) {
                            m_skipped++;
                        } else {
                            X2BaseBean bean = (X2BaseBean) beans.next();

                            if (!beans.hasNext() || isAllowMultipleMatches()) {
                                // We must update the first bean outside the while loop
                                m_matched++;
                                updateBean(bean, directValuesMap);

                                while (beans.hasNext()) {
                                    bean = (X2BaseBean) beans.next();

                                    m_matched++;
                                    updateBean(bean, directValuesMap);
                                }
                            } else {
                                m_errors.add(new KeyValuePair(
                                        Integer.valueOf(PARCCXmlDefinitionManager.ERROR_MULTIPLY_RESOLVED_MATCH_KEY), ""));
                                m_skipped++;
                            }
                        }
                    }
                } finally {
                    // This check for null is necessary since this isn't typical use of a
                    // QueryIterator
                    if (beans != null) {
                        beans.close();
                    }
                }
            } else {
                m_skipped++;
            }
        }

        /**
         * Gets the acd map by alias.
         *
         * @return the m_acdMapByAlias
         */
        public Map<String, AssessmentColumnDefinition> getAcdMapByAlias() {
            return m_acdMapByAlias;
        }

        /**
         * Returns the class of the bean.
         *
         * @return beanClass
         */
        public Class getBeanClass() {
            return m_beanClass;
        }

        /**
         * Returns the direct field repeats map. The key of this map is a non-repeat ModelProperty.
         * The value
         * is a ModelProperty whose value should be the same as that of the key.
         *
         * @return HashMap<ModelProperty,ModelProperty>
         */
        public HashMap<ModelProperty, ModelProperty> getDirectFieldRepeats() {
            return m_directFieldRepeats;
        }

        /**
         * Returns the direct fields.
         *
         * @return An ArrayList of ModelProperty objects, some elements may be null
         */
        public ArrayList<ModelProperty> getDirectFields() {
            return m_directFields;
        }

        /**
         * Returns the errors that occurred during the last call to <code>doImport</code>.
         *
         * @return LinkedList<Integer>
         */
        public LinkedList<KeyValuePair<Integer, String>> getErrors() {
            return m_errors;
        }

        /**
         * Returns the escape character.
         *
         * @return char
         */
        public char getEscapeCharacter() {
            return m_escapeCharacter;
        }

        /**
         * Returns the field lengths.
         *
         * @return int[]
         */
        public int[] getFieldLengths() {
            return m_fieldLengths;
        }

        /**
         * Returns the format attributes.
         *
         * @return List<String>
         */
        public List<String> getFormats() {
            return m_formats;
        }


        /**
         * Returns the formatter.
         *
         * @param property
         *
         * @return Format
         */
        public Format getFormatter(ModelProperty property) {
            return m_formatters.get(property);
        }

        /**
         * Gets the headings.
         *
         * @return the m_headings
         */
        public Map<ModelProperty, String> getHeadings() {
            return m_headings;
        }

        /**
         * Returns the inserted.
         *
         * @return int
         */
        public int getInserted() {
            return m_inserted;
        }

        /**
         * Returns the matched.
         *
         * @return int
         */
        public int getMatched() {
            return m_matched;
        }

        /**
         * return the match key.
         *
         * @return String
         */
        public String getMatchKey() {
            return m_matchKey;
        }

        /**
         * Returns the mode - either <code>MODE_INSERT</code>, <code>MODE_UPDATE</code>, or
         * <code>MODE_BOTH</code>.
         *
         * @return String
         */
        public String getMode() {
            return m_mode;
        }

        /**
         * Returns the organization.
         *
         * @return the m_organization
         */
        public Organization getOrganization() {
            return m_organization;
        }

        /**
         * Returns the preset fields.
         *
         * @return Map
         */
        public Map<ModelProperty, Object> getPresetFields() {
            return m_presetFields;
        }

        /**
         * Returns the skipped.
         *
         * @return int
         */
        public int getSkipped() {
            return m_skipped;
        }

        /**
         * Returns the unique key.
         *
         * @return String
         */
        public String getUniqueKey() {
            return m_uniqueKey;
        }

        /**
         * Returns the updated.
         *
         * @return int
         */
        public int getUpdated() {
            return m_updated;
        }

        /**
         * Returns the value delimiter.
         *
         * @return char
         */
        public char getValueDelimiter() {
            return m_valueDelimiter;
        }

        /**
         * Returns the value wrapper.
         *
         * @return char
         */
        public char getValueWrapper() {
            return m_valueWrapper;
        }

        /**
         * Returns the value wrapping mode.
         *
         * @return char
         */
        public VALUE_WRAPPING_MODE getWrappingMode() {
            VALUE_WRAPPING_MODE mode = VALUE_WRAPPING_MODE.NONE;
            if (isUseValueWrappers()) {
                mode = m_valueWrappingMode == null ? VALUE_WRAPPING_MODE.REQUIRED : m_valueWrappingMode;
            }
            return mode;
        }

        /**
         * Returns true if multiple matches are allowed.
         *
         * @return boolean
         */
        public boolean isAllowMultipleMatches() {
            return m_allowMultipleMatches;
        }

        /**
         * Returns the force.
         *
         * @return boolean
         */
        public boolean isForce() {
            return m_force;
        }

        /**
         * Returns true if strict lookup is enforced. Strict lookup means that lookups for a related
         * record (with a "rel" prefix) should result in a single match. If multiple matches are
         * found then the corresponding OID field is not updated.
         *
         * @return boolean
         */
        public boolean isStrictLookup() {
            return m_strictLookup;
        }

        /**
         * Returns true if escapes are used.
         *
         * @return boolean
         */
        public boolean isUseEscapes() {
            return m_useEscapes;
        }

        /**
         * Returns true if value delimiters are used.
         *
         * @return boolean
         */
        public boolean isUseValueDelimiters() {
            return m_useValueDelimiters;
        }

        /**
         * Returns true if value wrappers are used.
         *
         * @return boolean
         */
        public boolean isUseValueWrappers() {
            return m_useValueWrappers;
        }

        /**
         * Sets the broker.
         *
         * @param broker void
         */
        public void setBroker(X2Broker broker) {
            m_broker = broker;
        }

        /**
         * Sets the dictionary.
         *
         * @param dictionary void
         */
        public void setDictionary(DataDictionary dictionary) {
            m_dictionary = dictionary;
        }

        /**
         * Parses and loads the import definition.
         *
         * @param importElement Element
         * @param inputParameters Map<String,Object>
         * @param userData UserDataContainer
         * @param broker X2Broker
         * @param dictionary DataDictionary
         * @param locale Locale
         * @param organization Organization
         * @throws X2BaseException exception
         */
        protected void initialize(Element importElement,
                                  Map<String, Object> inputParameters,
                                  UserDataContainer userData,
                                  X2Broker broker,
                                  DataDictionary dictionary,
                                  Locale locale,
                                  Organization organization)
                throws X2BaseException {
            m_affectedBeanOids = new LinkedList<KeyValuePair<String, Class>>();
            m_broker = broker;
            m_dictionary = dictionary;
            m_errors = new LinkedList();
            m_inputParameters = inputParameters;
            m_locale = locale;
            m_organization = organization;
            m_organizationLevel = OrganizationManager.getOrganizationLevel(organization, m_broker.getPersistenceKey());

            /*
             * Parse the root <import> element.
             */
            String tableId = importElement.getAttributeValue(ROOT_TABLE_ID_ATTRIBUTE);
            DataDictionaryTable rootTable = m_dictionary.findDataDictionaryTableById(tableId);
            m_beanClass = rootTable.getBeanClass();

            m_allowMultipleMatches = Boolean
                    .valueOf(importElement.getAttributeValue(ROOT_ALLOW_MULTIPLE_MATCH_ATTRIBUTE)).booleanValue();
            m_force = Boolean.valueOf(importElement.getAttributeValue(ROOT_FORCED_ATTRIBUTE)).booleanValue();

            m_mode = importElement.getAttributeValue(ROOT_MODE_ATTRIBUTE);
            if (StringUtils.isEmpty(m_mode)) {
                m_mode = MODE_BOTH;
            }

            String escapeCharacter = importElement.getAttributeValue(ROOT_ESCAPE_CHARACTER_ATTRIBUTE);
            if (!StringUtils.isEmpty(escapeCharacter)) {
                m_escapeCharacter = escapeCharacter.charAt(0);
            } else if (escapeCharacter != null && escapeCharacter.length() == 0) {
                m_useEscapes = false;
            }

            String valueDelimiter = importElement.getAttributeValue(ROOT_VALUE_DELIMITER_ATTRIBUTE);
            if (!StringUtils.isEmpty(valueDelimiter)) {
                m_valueDelimiter = valueDelimiter.charAt(0);
            } else if (valueDelimiter != null && valueDelimiter.length() == 0) {
                m_useValueDelimiters = false;
            }

            String valueWrapper = importElement.getAttributeValue(ROOT_VALUE_WRAPPER_ATTRIBUTE);
            if (!StringUtils.isEmpty(valueWrapper)) {
                m_valueWrapper = valueWrapper.charAt(0);
                m_useValueWrappers = true;
            } else if (valueWrapper != null && valueWrapper.length() == 0) {
                m_useValueWrappers = false;
            }

            String valueWrappingMode = importElement.getAttributeValue(ROOT_VALUE_WRAPPING_MODE);
            if (!StringUtils.isEmpty(valueWrappingMode)) {
                if (VALUE_WRAPPING_MODE_OPTIONAL.equals(valueWrappingMode)) {
                    m_valueWrappingMode = VALUE_WRAPPING_MODE.OPTIONAL;
                } else if (VALUE_WRAPPING_MODE_REQUIRED.equals(valueWrappingMode)) {
                    m_valueWrappingMode = VALUE_WRAPPING_MODE.REQUIRED;
                }
            }


            String strictLookup = importElement.getAttributeValue(ROOT_STRICT_LOOKUP_ATTRIBUTE);
            m_strictLookup = StringUtils.isEmpty(strictLookup) || Boolean.parseBoolean(strictLookup);

            /*
             * Initialize the "match fields" list. This member holds a List of ModelProperty
             * objects.
             */
            m_matchFields = new LinkedList();

            /*
             * Initialize the "field requirements" map. This member is structured as follows:
             *
             * Key: ModelProperty Value: A List of requirements (KeyValuePair objects) Key:
             * Comparison operator (String
             * object) Value: Value to compare (Object object)
             */
            m_requirements = new HashMap();

            /*
             * Parse the children of <preset-fields> into a Map of ModelProperty keys to preset
             * values (Object objects).
             */
            m_presetFields = new HashMap();

            m_headings = new HashMap();
            Element presetFields = importElement.getChild(PRESET_FIELDS_ELEMENT);
            if (presetFields != null) {
                Iterator children = presetFields.getChildren(PRESET_FIELD_ELEMENT).iterator();
                while (children.hasNext()) {
                    Element child = (Element) children.next();

                    ModelProperty property = parseField(child);

                    String source = child.getAttributeValue(PRESET_FIELD_SOURCE_ATTRIBUTE);
                    String value = child.getAttributeValue(PRESET_FIELD_VALUE_ATTRIBUTE);

                    m_presetFields.put(property,
                            resolvePresetValue(source, value, property, inputParameters, userData));
                }
            }

            populateAcdMap();

            /*
             * Parse the children of <direct-fields> into an ArrayList of ModelProperty objects. The
             * list will contain null
             * elements where ever a <skip> element is located. Parse the requirements (if any) for
             * each ModelProperty.
             */
            Element directFieldsElement = importElement.getChild(DIRECT_FIELDS_ELEMENT);

            if (directFieldsElement != null) {
                Collection directFields = directFieldsElement.getChildren();
                m_directFields = new ArrayList(directFields.size());
                m_directFieldRepeats = new HashMap(directFields.size());
                m_fieldLengths = new int[directFields.size()];
                m_formats = new ArrayList(directFields.size());
                m_formatters = new HashMap<ModelProperty, Format>();

                ModelProperty lastNonRepeatProperty = null;

                int i = 0;
                Iterator children = directFields.iterator();

                while (children.hasNext()) {
                    Element child = (Element) children.next();

                    boolean repeat = Boolean.parseBoolean(child.getAttributeValue(FIELD_REPEAT_ATTRIBUTE));
                    if (repeat) {
                        ModelProperty property = parseField(child);

                        if (lastNonRepeatProperty != null) {
                            m_directFieldRepeats.put(property, lastNonRepeatProperty);
                        }
                    } else {
                        ModelProperty property = null;
                        String sourceHeading = null;
                        String formatter = null;
                        String format = child.getAttributeValue(FIELD_FORMAT_ATTRIBUTE);
                        m_formats.add(format);
                        String length = child.getAttributeValue(FIELD_LENGTH_ATTRIBUTE);
                        if (StringUtils.isInteger(length)) {
                            m_fieldLengths[i] = Integer.parseInt(length);
                        } else {
                            m_fieldLengths[i] = 0;
                        }

                        if (child.getName().equals(DIRECT_FIELD_ELEMENT)) {
                            property = parseField(child);
                            sourceHeading = child.getAttributeValue(FIELD_SOURCE_ID_ATTRIBUTE);
                            formatter = child.getAttributeValue("formatter");

                            parseRequirements(property, child.getChildren(REQUIREMENT_ELEMENT), userData);

                            lastNonRepeatProperty = property;

                            if (!StringUtils.isEmpty(sourceHeading)) {
                                m_headings.put(property, sourceHeading);
                            }

                            if (!StringUtils.isEmpty(formatter)) {
                                if (formatter.equals(FORMATTER_DECIMAL) && !StringUtils.isEmpty(format)) {
                                    m_formatters.put(property, new DecimalFormat(format));
                                }
                            }
                        }

                        m_directFields.add(property);


                        i++;
                    }
                }
            }
        }

        /**
         * Parses the core field element into a ModelProperty. If the "match" attribute is true then
         * the ModelProperty will
         * be added to the "match fields" list.
         *
         * @param element
         *        either a &lt;preset-field&gt; or a &lt;direct-field&gt; element
         *
         * @return ModelProperty
         */
        protected ModelProperty parseField(Element element) {
            String id = element.getAttributeValue(FIELD_ID_ATTRIBUTE);
            String type = element.getAttributeValue(FIELD_TYPE_ATTRIBUTE);
            ModelProperty property = null;

            if (m_acdMapByAlias != null && m_acdMapByAlias.containsKey(id)) {
                AssessmentColumnDefinition acd = m_acdMapByAlias.get(id);
                id = acd.getDataFieldConfig().getDataFieldOid();

                property = getModelProperty(id, FIELD_TYPE_ATTRIBUTE);

                if (Boolean.valueOf(element.getAttributeValue(FIELD_MATCH_ATTRIBUTE)).booleanValue()) {
                    m_matchFields.add(property);
                }
            } else {
                property = getModelProperty(id, type);

                if (Boolean.valueOf(element.getAttributeValue(FIELD_MATCH_ATTRIBUTE)).booleanValue()) {
                    m_matchFields.add(property);
                }
            }
            if (property == null) {
                throw new RuntimeException("Field cannot be parsed: " + id + "[" + type + "]");
            }
            return property;
        }

        /**
         * Returns the beans matching the given property values.
         *
         * @param propertyValues a Map of ModelProperty keys to Object values; only the properties
         *        designated as match fields are considered
         *
         * @return QueryIterator this value may be null
         */
        private QueryIterator getMatchingBeans(Map<ModelProperty, Object> propertyValues) {
            QueryIterator iterator = null;

            if (!m_matchFields.isEmpty()) {
                Criteria criteria = new Criteria();

                Iterator matchingFields = m_matchFields.iterator();
                while (matchingFields.hasNext()) {
                    ModelProperty matchingField = (ModelProperty) matchingFields.next();

                    Object value = null;
                    if (propertyValues.containsKey(matchingField)) {
                        value = propertyValues.get(matchingField);
                    } else {
                        value = m_presetFields.get(matchingField);
                    }

                    criteria.addEqualTo(matchingField.getBeanPath(), value);
                }

                m_matchKey = criteria.toString();

                /**
                 * Adds criteria for the root table's organization path for limiting matching beans
                 * to those that are viewable
                 * within the organization hierarchy.
                 */
                X2Criteria organizationCriteria = new X2Criteria();
                Collection<ModelProperty> organizationPaths =
                        OrganizationManager.getOrganizationPaths(m_beanClass, m_dictionary, m_organizationLevel);

                for (ModelProperty modelProperty : organizationPaths) {
                    X2Criteria orgPathCriteria = OrganizationManager.getOrganizationAccessCriteria(m_organization,
                            modelProperty, OrganizationAccess.READ_WRITE, OrganizationAccess.READ_WRITE);
                    organizationCriteria.addAndCriteria(orgPathCriteria);
                }

                criteria.addAndCriteria(organizationCriteria);

                QueryByCriteria query = new BeanQuery(m_beanClass, criteria);
                iterator = m_broker.getIteratorByQuery(query);
            }

            return iterator;
        }

        /**
         * Returns the model property for the passed ID.
         *
         * @param id either a dictionary path or a field alias, depending on the
         *        <code>type</code> parameter
         * @param type either <code>FIELD_TYPE_ALIAS</code> or <code>FIELD_TYPE_ATTRIBUTE</code>
         *
         * @return ModelProperty
         */
        private ModelProperty getModelProperty(String id, String type) {
            ModelProperty property = null;

            if (FIELD_TYPE_ALIAS.equals(type)) {
                DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(id);
                if (field != null) {
                    property = new ModelProperty(field.getId(), m_dictionary);
                }
            } else if (StringUtils.isEmpty(type) || FIELD_TYPE_ATTRIBUTE.equals(type)) {
                property = new ModelProperty(id, m_dictionary);
            }

            return property;
        }

        /**
         * Parses the requirements (if any) for the given ModelProperty. If there are requirements
         * then
         * an entry will be added to the "requirements" map. This method should not be called more
         * than
         * once for the same ModelProperty. Requirements are ignored for BLOB fields.
         *
         * @param property ModelProperty
         * @param collection a Collection of &lt;requirement&gt; elements, this value may be empty
         *        but
         *        cannot be null
         * @param userData UserDataContainer
         * @throws X2BaseException exception
         */
        private void parseRequirements(ModelProperty property,
                                       Collection collection,
                                       UserDataContainer userData)
                throws X2BaseException {
            DataDictionaryField field = property.getField();
            if (!field.getDatabaseType().equals(DataField.BINARY_DATABASE_TYPE) && !collection.isEmpty()) {
                List requirements = new ArrayList(collection.size());

                String javaType = field.getJavaType();
                boolean isString = field.isString();

                Iterator elements = collection.iterator();
                while (elements.hasNext()) {
                    Element element = (Element) elements.next();

                    String operator = element.getAttributeValue(REQUIREMENT_OPERATOR_ATTRIBUTE);
                    String source = element.getAttributeValue(REQUIREMENT_SOURCE_ATTRIBUTE);
                    String value = element.getAttributeValue(REQUIREMENT_VALUE_ATTRIBUTE);

                    // Some operators (like "is empty") don't have a compare value
                    Object compareValue = null;
                    if (source != null) {
                        if (source.equals(WebConstants.VALUE_SOURCE_INPUT)) {
                            compareValue = m_inputParameters.get(value);
                        } else {
                            compareValue =
                                    WebUtils.interpretValue(value, source, javaType, userData, m_locale, isString);
                        }
                    }

                    requirements.add(new KeyValuePair(operator, compareValue));
                }

                m_requirements.put(property, requirements);
            }
        }

        /**
         * Populate map keyed on acdAlias and valued on AssessmentColumnDefinition.
         */
        private void populateAcdMap() {
            X2Criteria acdCriteria = new X2Criteria();

            if (m_presetFields != null && !m_presetFields.isEmpty()) {
                acdCriteria.addIn(AssessmentColumnDefinition.COL_ASSESSMENT_DEFINITION_OID, m_presetFields.values());
            } else {
                acdCriteria.addEqualTo(AssessmentColumnDefinition.COL_ASSESSMENT_DEFINITION_OID, "__DummyOid___");
            }

            m_acdMapByAlias = m_broker.getMapByQuery(new QueryByCriteria(AssessmentColumnDefinition.class, acdCriteria),
                    AssessmentColumnDefinition.COL_ALIAS, 1024);
        }

        /**
         * Populates the bean with values from the given map.
         *
         * @param bean X2BaseBean
         * @param propertyValues a Map of ModelProperty keys to Object values
         * @return true if the bean was populated successfully, false if it was not (i.e., a related
         *         property could not be resolved)
         */
        private boolean populateBean(X2BaseBean bean, Map<ModelProperty, Object> propertyValues) {
            boolean success = true;

            HashSet<DataDictionaryRelationship> relationships = new HashSet<DataDictionaryRelationship>();

            for (ModelProperty property : propertyValues.keySet()) {
                if (property.isComplex()) {
                    DataDictionaryRelationship relationship = property.getRelationships().get(0);
                    relationships.add(relationship);
                } else {
                    String beanPath = property.getBeanPath();
                    Object value = propertyValues.get(property);

                    bean.setFieldValueByBeanPath(beanPath, value);
                }
            }


            for (DataDictionaryRelationship relationship : relationships) {
                Criteria relationshipCriteria = new Criteria();

                for (ModelProperty property : propertyValues.keySet()) {
                    if (property.isComplex() && property.getDictionaryPath().startsWith(relationship.getId())) {
                        String newPath = property.getDictionaryPath().replace(relationship.getId() + ".", "");
                        ModelProperty relatedProperty = new ModelProperty(newPath, property.getDictionary());

                        relationshipCriteria.addEqualTo(relatedProperty.getBeanPath(), propertyValues.get(property));
                    }
                }

                m_uniqueKey = relationshipCriteria.toString();

                X2BaseBean relatedObject = null;

                /**
                 * Add organization scoping to only see relationships which are viewable within the
                 * current organization.
                 */
                Collection<ModelProperty> organizationPaths = OrganizationManager.getOrganizationPaths(
                        relationship.getRelatedDataTable().getDataClass(), m_dictionary, m_organizationLevel);
                for (ModelProperty modelProperty : organizationPaths) {
                    X2Criteria orgPathCriteria = OrganizationManager.getOrganizationAccessCriteria(m_organization,
                            modelProperty, OrganizationAccess.READ_WRITE, OrganizationAccess.READ_WRITE);
                    relationshipCriteria.addAndCriteria(orgPathCriteria);
                }

                BeanQuery query =
                        new BeanQuery(relationship.getRelatedDataTable().getDataClass(), relationshipCriteria);
                QueryIterator results = m_broker.getIteratorByQuery(query);
                try {
                    if (results.hasNext()) {
                        relatedObject = (X2BaseBean) results.next();
                    } else {
                        success = false;
                    }

                    if (results.hasNext() && isStrictLookup()) {
                        relatedObject = null;
                        success = false;
                    }
                } finally {
                    results.close();
                }

                if (relatedObject != null) {
                    String foreignKey = relationship.getPrimaryDataIndex().getFieldList();
                    String beanPath = m_dictionary.findDataDictionaryField(foreignKey).getJavaName();

                    bean.setFieldValueByBeanPath(beanPath, relatedObject.getOid());
                }
            }

            return success;
        }

        /**
         * Resolves a preset value.
         *
         * @param source String
         * @param value String
         * @param targetProperty ModelProperty
         * @param inputParameters Map<String,Object>
         * @param userData UserDataContainer
         * @return Object
         * @throws X2BaseException exception
         */
        private Object resolvePresetValue(String source,
                                          String value,
                                          ModelProperty targetProperty,
                                          Map<String, Object> inputParameters,
                                          UserDataContainer userData)
                throws X2BaseException {
            Object presetValue;
            if (source.equals(WebConstants.VALUE_SOURCE_INPUT)) {
                presetValue = inputParameters.get(value);
            } else {
                presetValue = WebUtils.interpretValue(value, source,
                        targetProperty.getField().getJavaType(), userData, m_locale,
                        targetProperty.getField().isString());
            }

            return presetValue;
        }

        /**
         * Populates the bean with direct and preset values and saves it.
         *
         * @param bean X2BaseBean
         * @param directValuesMap Map<ModelProperty,Object>
         * @return true if the update was successful, false otherwise
         */
        private boolean updateBean(X2BaseBean bean, Map<ModelProperty, Object> directValuesMap) {
            boolean success = true;

            /*
             * Clone the bean to prevent "dirty" records in the cache in case of validation errors.
             */
            bean = bean.cloneBean();

            HashMap<ModelProperty, Object> valueMap = new HashMap<ModelProperty, Object>(directValuesMap.size() * 2);
            valueMap.putAll(directValuesMap);
            valueMap.putAll(getPresetFields());

            if (populateBean(bean, valueMap)) {
                /**
                 * Ensure the organizationPath specified for the table is within the current
                 * organization.
                 */
                boolean orgPathForBeanWithinScope = OrganizationManager.hasReadWriteAccess(bean, getOrganization());

                if (OrganizationManager.getOrganizationLevel(m_organization, m_broker.getPersistenceKey()) == 0
                        || orgPathForBeanWithinScope) {
                    if (bean.isDirty()) {
                        boolean wasNew = bean.isNew();

                        if (isForce()) {
                            m_broker.saveBeanForced(bean);
                        } else {
                            List errors = m_broker.saveBean(bean);
                            if (!errors.isEmpty()) {
                                success = false;
                                m_errors.add(new KeyValuePair(Integer.valueOf(ERROR_VALIDATION), errors.toString()));
                            }
                        }

                        if (success) {
                            m_affectedBeanOids.add(new KeyValuePair(bean.getOid(), bean.getClass()));

                            if (wasNew) {
                                m_inserted++;
                            } else {
                                m_updated++;
                            }
                        }
                    }
                } else {
                    success = false;
                    m_skipped++;
                    m_errors.add(new KeyValuePair(Integer.valueOf(ERROR_NO_ORGANIZATION_PATH), ""));
                }
            } else {
                success = false;
                m_skipped++;
                m_errors.add(new KeyValuePair(Integer.valueOf(ERROR_UNRESOLVED_FOREIGN_KEY), ""));
            }

            return success;
        }

        /**
         * Validates that the values meet all the requirements specified for the fields.
         *
         * @param propertyValues a Map of ModelProperty keys to Object values
         *
         * @return true if the values meet the requirements, false otherwise
         */
        private boolean validateRequirements(Map<ModelProperty, Object> propertyValues) {
            boolean valid = true;

            for (ModelProperty property : propertyValues.keySet()) {
                Object value = propertyValues.get(property);

                List requirementList = ((List) m_requirements.get(property));
                if (requirementList != null) {
                    Iterator requirements = requirementList.iterator();
                    while (requirements.hasNext()) {
                        KeyValuePair requirement = (KeyValuePair) requirements.next();

                        String operator = (String) requirement.getKey();
                        Object compareValue = requirement.getValue();

                        // These cases are listed in the same order as in the DTD
                        if (operator.equals(Filter.OPERATOR_EQUALS)) {
                            if (property.getField().isString() && StringUtils.isEmpty((String) value)) {
                                valid = StringUtils.isEmpty((String) compareValue);
                            } else if (value == null) {
                                valid = (compareValue == null);
                            } else {
                                valid = value.equals(compareValue);
                            }
                        } else if (operator.equals(Filter.OPERATOR_EMPTY)) {
                            if (property.getField().isString()) {
                                valid = StringUtils.isEmpty((String) value);
                            } else {
                                valid = (value == null);
                            }
                        } else if (operator.equals(Filter.OPERATOR_GREATER_THAN) ||
                                operator.equals(Filter.OPERATOR_AFTER)) {
                            if (value == null) {
                                valid = false;
                            } else {
                                valid = ((Comparable) value).compareTo(compareValue) > 0;
                            }
                        } else if (operator.equals(Filter.OPERATOR_GREATER_OR_EQUAL) ||
                                operator.equals(Filter.OPERATOR_ON_OR_AFTER)) {
                            if (value == null) {
                                valid = (compareValue == null);
                            } else {
                                valid = ((Comparable) value).compareTo(compareValue) >= 0;
                            }
                        } else if (operator.equals(Filter.OPERATOR_LESS_THAN) ||
                                operator.equals(Filter.OPERATOR_BEFORE)) {
                            if (value == null) {
                                valid = (compareValue != null);
                            } else {
                                valid = ((Comparable) value).compareTo(compareValue) < 0;
                            }
                        } else if (operator.equals(Filter.OPERATOR_LESS_OR_EQUAL) ||
                                operator.equals(Filter.OPERATOR_ON_OR_BEFORE)) {
                            if (value != null) // A null value is always valid for this operator
                            {
                                valid = ((Comparable) value).compareTo(compareValue) <= 0;
                            }
                        } else if (operator.equals(Filter.OPERATOR_NOT_EMPTY)) {
                            if (property.getField().isString()) {
                                valid = !StringUtils.isEmpty((String) value);
                            } else {
                                valid = (value != null);
                            }
                        } else if (operator.equals(Filter.OPERATOR_NOT_EQUAL)) {
                            if (property.getField().isString() && StringUtils.isEmpty((String) value)) {
                                valid = !StringUtils.isEmpty((String) compareValue);
                            }
                            if (value == null) {
                                valid = (compareValue != null);
                            } else {
                                valid = !value.equals(compareValue);
                            }
                        }

                        if (!valid) {
                            break;
                        }
                    }
                }

                if (!valid) {
                    break;
                }
            }

            return valid;
        }

    }

    protected static final String ALIAS_IMPORT_FORMAT = "all-acd-ImportFormat";
    protected static final String ALIAS_IMPORT_HEADING = "all-acd-ImportHeading";
    protected static final String ALIAS_PERIOD_FIELD = "PARCCTSTPERIOD";
    protected static final String ALIAS_SCHOOL_YEAR_FIELD = "PARCCTSTYEAR";
    protected static final String ALIAS_TEST_CODE_FIELD = "PARCCTSTCODE";

    protected static final String PARAM_ASD_ID = "asdId";
    protected static final String PARAM_ASD_OID = "asdOid";
    protected static final String PARAM_ALIAS_PREFIX = "aliasPrefix";
    protected static final String PARAM_DEBUG = "debug";
    protected static final String PARAM_FIELD_COUNT = "fieldCount";
    protected static final String PARAM_PERIOD_FIELD = "periodField";
    protected static final String PARAM_SASID_FIELD = "sasidField";
    protected static final String PARAM_SCHOOL_YEAR_FIELD = "schoolYearField";
    protected static final String PARAM_TEST_CODE_FIELD = "testCodeField";
    protected static final String PARAM_TEST_DATE_FIELD = "testDateField";
    protected static final String PARAM_TEST_DATE_FORMAT = "testDateFormat";
    protected static final String PARAM_TEST_SCHOOL_FIELD = "testSchoolField";
    protected static final String PARAM_TEST_SCHOOL_MATCHING_FIELD = "testSchoolMatchingField";

    private static final long serialVersionUID = 1L;

    protected List<String> m_initErrors = new LinkedList();
    protected boolean m_debug = false;
    protected List<String> m_initMessages = new LinkedList();
    private AssessmentDefinition m_asd;
    private int m_fieldCount = 0;
    private int[] m_fieldLengthArr = null;
    private Map<String, Integer> m_headingPositions;
    private boolean m_invalidDestinationTable = false;
    private UserDataContainer m_userData;
    private PARCCXmlDefinitionManager m_xmlManager = null;

    /**
     * @param record
     * @param dictionary
     * @param acdMap
     * @return String
     */
    protected String addAdditionalImportFields(DataDictionary dictionary,
                                               List<String> record,
                                               Map<String, Object> acdMap) {
        StringBuilder xml = new StringBuilder();
        DataDictionaryField importFormatField = dictionary.findDataDictionaryFieldByAlias(ALIAS_IMPORT_FORMAT);
        DataDictionaryField importHeadingField = dictionary.findDataDictionaryFieldByAlias(ALIAS_IMPORT_HEADING);

        for (Entry<String, Object> entry : acdMap.entrySet()) {
            String heading = entry.getKey();
            if (entry.getValue() instanceof AssessmentColumnDefinition && importHeadingField != null
                    && !StringUtils.isEmpty(importHeadingField.getJavaName())) {
                String headingCandidate = (String) ((AssessmentColumnDefinition) entry.getValue())
                        .getFieldValueByBeanPath(importHeadingField.getJavaName());
                if (!StringUtils.isEmpty(headingCandidate)) {
                    heading = headingCandidate;
                }
            }

            if (record.contains(heading)) {
                String columnValue = getAliasField(acdMap, entry.getKey());
                m_initMessages.add("Input field " + entry.getKey() + " is mapped to student assessment column "
                        + columnValue);
                xml.append("<direct-field id=\"");
                xml.append(columnValue);
                xml.append("\" source-id=\"");
                xml.append(entry.getKey());
                xml.append("\"");
                if (entry.getValue() instanceof AssessmentColumnDefinition && importFormatField != null
                        && !StringUtils.isEmpty(importFormatField.getJavaName())) {
                    String format = (String) ((AssessmentColumnDefinition) entry.getValue())
                            .getFieldValueByBeanPath(importFormatField.getJavaName());
                    if (!StringUtils.isEmpty(format)) {
                        xml.append(" format=\"");
                        xml.append(format);
                        xml.append("\"");
                    }
                }
                xml.append(" />");
            }
        }
        return xml.toString();
    }

    /**
     * Builds a map of fields to their values for the current record.
     *
     * @param record a List of String objects that is the same length as m_directFields
     *
     * @return A Map of ModelProperty keys to Object values
     */
    protected Map<ModelProperty, Object> buildDirectValuesMap(List record) {
        HashMap directValues = new HashMap((int) (m_xmlManager.getDirectFields().size() * 1.5));

        Iterator properties = m_xmlManager.getDirectFields().iterator();
        Iterator formats = m_xmlManager.getFormats().iterator();

        while (properties.hasNext()) {
            ModelProperty property = (ModelProperty) properties.next();
            Format formatter = m_xmlManager.getFormatter(property);
            String heading = m_xmlManager.getHeadings().get(property);
            String formatSet = (String) formats.next();
            Integer valuePosition = heading != null ? m_headingPositions.get(heading) : null;

            if (valuePosition != null) {

                Object value = null;

                DataDictionaryField field = property.getField();
                String stringValue = (String) record.get(valuePosition.intValue());
                if ("java.sql.Timestamp".equals(field.getEffectiveJavaType()) && field.isString()
                        && !StringUtils.isEmpty(formatSet)) {
                    for (String format : Arrays.asList(formatSet.split("\\|"))) {
                        try {
                            SimpleDateFormat inbound = new SimpleDateFormat(format);
                            java.util.Date utilDate = inbound.parse(stringValue);
                            SimpleDateFormat outbound = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                            value = outbound.format(utilDate);
                            break;
                        } catch (ParseException e) {
                            // default to stringValue
                        }
                    }
                    value = stringValue;

                } else if (formatter != null) {
                    if (formatter instanceof DecimalFormat && StringUtils.isNumeric(stringValue)) {
                        stringValue = stringValue.trim();
                        BigDecimal decimal = new BigDecimal(stringValue);
                        value = formatter.format(decimal);
                    } else {
                        AppGlobals.getLog().log(
                                Level.WARNING,
                                "unknown formatter " + formatter.getClass() + " or value is not numeric + "
                                        + stringValue);
                    }
                }
                if (value == null) {
                    if (!StringUtils.isEmpty(formatSet)) {
                        for (String format : Arrays.asList(formatSet.split("\\|"))) {
                            Converter converter =
                                    ConverterFactory.getConverterForClass(field.getEffectiveJavaType(), getLocale(),
                                            field.isString(), format);
                            if (converter != null) {
                                value = converter.stringToJava(stringValue.trim());
                                if (value != null) {
                                    break;
                                }
                            }
                        }
                    } else {
                        Converter converter =
                                ConverterFactory.getConverterForClass(field.getEffectiveJavaType(), getLocale(),
                                        field.isString());
                        if (converter != null) {
                            // Remove whitespace before converting string value
                            value = converter.stringToJava(stringValue.trim());
                        }
                    }
                    if (value == null && !StringUtils.isEmpty(stringValue)) {
                        value = stringValue;
                    }
                }

                directValues.put(property, value);

                for (ModelProperty repeatProperty : m_xmlManager.getDirectFieldRepeats().keySet()) {
                    ModelProperty dependentProperty = m_xmlManager.getDirectFieldRepeats().get(repeatProperty);
                    if (dependentProperty.equals(property)) {
                        directValues.put(repeatProperty, value);
                    }
                }
            }
        }

        if (m_debug) {
            m_initMessages.add(directValues.toString());
        }

        return directValues;
    }

    /**
     * Display only init errors if exit. Otherwise, add init messages to default messages.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        if (m_initErrors.isEmpty()) {
            if (!m_initMessages.isEmpty()) {
                m_initMessages.add("\n");
                exportList(m_initMessages);
            }
            super.exportResults();
        } else {
            exportList(m_initErrors);
        }
    }

    /**
     * Determines the field for the specified alias.
     *
     * @param map Map<String,Object>
     * @param key String
     * @return String
     */
    protected String getAliasField(Map<String, Object> map, String key) {
        Object value = map.get(key);
        String columnValue = null;

        if (value instanceof String) {
            columnValue = (String) value;
        } else {
            AssessmentColumnDefinition acd = (AssessmentColumnDefinition) value;
            columnValue = acd.getDataFieldConfig().getDataFieldOid();
        }
        return columnValue;
    }

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        if (m_fieldCount == 0) {
            return m_xmlManager.getDirectFields().size();
        }
        return m_fieldCount;
    }

    /**
     * Gets the field lengths.
     *
     * @return int[]
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldLengths()
     */
    @Override
    protected int[] getFieldLengths() {
        if (m_fieldLengthArr == null) {
            return m_xmlManager.getFieldLengths();
        }
        return m_fieldLengthArr;
    }

    /**
     * Only import file if there are no initialization errors.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        if (m_initErrors.isEmpty()) {
            super.importData(sourceFile);
        }
    }

    /**
     * Retrieves and updates the bean according to the given record.
     *
     * @param record List
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List record, int lineNumber) throws Exception {
        if (!m_invalidDestinationTable) {
            if (lineNumber == 1) {
                populateHeadingPositions(record);
            } else {
                m_xmlManager.doImport(buildDirectValuesMap(record));

                incrementInsertCount(m_xmlManager.getInserted());
                incrementMatchCount(m_xmlManager.getMatched());
                incrementSkipCount(m_xmlManager.getSkipped());
                incrementUpdateCount(m_xmlManager.getUpdated());

                for (KeyValuePair<Integer, String> error : m_xmlManager.getErrors()) {
                    switch (error.getKey().intValue()) {
                        case PARCCXmlDefinitionManager.ERROR_UNRESOLVED_FOREIGN_KEY:
                            logInvalidRecord(lineNumber,
                                    "Unique foreign key not found - " + m_xmlManager.getUniqueKey());
                            break;

                        case PARCCXmlDefinitionManager.ERROR_VALIDATION:
                            logInvalidRecord(lineNumber,
                                    "Validation errors found while saving record - " + error.getValue());
                            break;

                        case PARCCXmlDefinitionManager.ERROR_NO_ORGANIZATION_PATH:
                            logInvalidRecord(lineNumber, "This table is not available for import/export");
                            break;

                        case PARCCXmlDefinitionManager.ERROR_MULTIPLY_RESOLVED_MATCH_KEY:
                            logInvalidRecord(lineNumber, "Multiply resolved match key - " + m_xmlManager.getMatchKey());
                            break;
                    }
                }
            }
        }
    }

    /**
     * Initialize the XML definition from the assessment definition and input file header.
     *
     * @param dictionary DataDictionary
     * @return String
     */
    protected String initFromHeader(DataDictionary dictionary) {
        m_initErrors.add(
                "Method PARCCXmlDefinitionImport.initFromHeader(DataDictionary dictionary) should be overriden in the child classes!!!");
        return null;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        ImportExportDefinition bean = (ImportExportDefinition) getJob().getTool();
        String xmlDefinition = null;

        String asdOid = (String) getParameter(PARAM_ASD_OID);
        m_asd = (AssessmentDefinition) getBroker().getBeanByOid(AssessmentDefinition.class, asdOid);
        Boolean debug = (Boolean) getParameter(PARAM_DEBUG);
        m_debug = debug != null ? debug.booleanValue() : false;

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_asd, bean.getPersistenceKey());

        if (StringUtils.isEmpty(bean.getDefinition())) {
            xmlDefinition = initFromHeader(dictionary);
        } else {
            Integer fieldCount = (Integer) getParameter(PARAM_FIELD_COUNT);
            if (fieldCount != null) {
                m_fieldCount = fieldCount.intValue();
                m_fieldLengthArr = new int[m_fieldCount];
            }
            xmlDefinition = bean.getDefinition();
        }
        if (xmlDefinition != null) {
            xmlDefinition = xmlDefinition.replaceAll("&([^;]+(?!(?:\\w|;)))", "&amp;$1");
        }
        if (m_initErrors.isEmpty()) {
            m_xmlManager = new PARCCXmlDefinitionManager(xmlDefinition.getBytes(),
                    getParameters(),
                    m_userData,
                    getBroker(),
                    dictionary,
                    getLocale(),
                    getOrganization());

            /*
             * This will make sure that if the current organization is not root that it has an
             * organization path.
             */
            int level = OrganizationManager.getOrganizationLevel(getOrganization(), getBroker().getPersistenceKey());
            Collection<ModelProperty> organizationPaths =
                    OrganizationManager.getOrganizationPaths(m_xmlManager.getBeanClass(),
                            dictionary, level);
            if (level > 0 && CollectionUtils.isEmpty(organizationPaths)) {
                logInvalidRecord(1, "error.import.text.invalidOrganizationPath");
                m_invalidDestinationTable = true;
            }

            setUseEscapes(m_xmlManager.isUseEscapes());
            setUseValueDelimiters(m_xmlManager.isUseValueDelimiters());
            setValueWrappingMode(m_xmlManager.getWrappingMode());

            setEscapeCharacter(m_xmlManager.getEscapeCharacter());
            setValueDelimiter(m_xmlManager.getValueDelimiter());
            setValueWrapper(m_xmlManager.getValueWrapper());

            m_xmlManager.setBroker(getBroker());
            m_xmlManager.setDictionary(dictionary);
        }
    }

    /**
     * This map is used to associate the column headings in the import file with the field where the
     * data in that column in the input file is placed in the student assessment record.
     * <br>
     * In this map, the key is the column heading name in the input data and the value is either a
     * Student Assessment data field oid or an Assessment column definition.
     * <br>
     * These mappings are generated using two different mechanisms:
     * <br>
     * 1) An alias value is added to the Student Assessment field or to the Assessment Column
     * Definition that consists of the prefix and the column heading name. This is the only method
     * available to map a Student Assessment field without using an Assessment column definition.
     * <br>
     * 2) The alias all-acd-ImportHeading is added to the assessment column definition. This field
     * is then populated with the heading from the input file which maps to the field defined.
     * <br>
     * If an Assessment column definition field requires a custom format, the value is placed in am
     * assessment column definition field with alias all-acd-ImportFormat
     *
     * @param dictionary DataDictionary
     * @param aliasPrefix String
     * @return Map
     */
    protected Map<String, Object> loadAliasColumns(DataDictionary dictionary, String aliasPrefix) {
        Map<String, Object> map = new HashMap();

        DataDictionaryTable table = dictionary.findDataDictionaryTableByClass(StudentAssessment.class.getName());
        for (DataField field : table.getDataFields()) {
            for (DataFieldConfig config : field.getDataFieldConfigs()) {
                if (!StringUtils.isEmpty(config.getAlias())) {
                    String[] aliases = config.getAlias().split(",");
                    for (String alias : aliases) {
                        addAliasColumn(aliasPrefix, map, alias.trim(), field.getOid());
                    }
                }
            }
        }

        if (m_asd != null) {
            DataDictionaryField importHeadingField = dictionary.findDataDictionaryFieldByAlias(ALIAS_IMPORT_HEADING);
            for (AssessmentColumnDefinition col : m_asd.getAssessmentColumnDefinitions()) {
                if (importHeadingField != null) {
                    String heading = (String) col.getFieldValueByBeanPath(importHeadingField.getJavaName());
                    if (!StringUtils.isEmpty(heading)) {
                        map.put(heading, col);
                        continue;
                    }
                }
                String alias = col.getAlias();
                addAliasColumn(aliasPrefix, map, alias, col);
            }
        } else {
            m_initErrors.add("Assessment definition could not be found");
        }
        return map;
    }

    /**
     * Read a list of column headings from the input file.
     *
     * @return List
     */
    protected List<String> readInputHeading() {
        // Set temporary field count - needed to use splitLine()
        m_fieldCount = 100;
        m_fieldLengthArr = new int[m_fieldCount];

        File sourceFile = (File) getParameter(FILE_KEY);
        List<String> record = new ArrayList();
        setValueWrappingMode(VALUE_WRAPPING_MODE.OPTIONAL);
        setUseValueDelimiters(true);
        setValueWrapper('"');

        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(sourceFile), 4096);
            String line = reader.readLine();
            record = splitLine(line, 1);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    // Do nothing
                }
            }
        }
        if (record != null) {
            m_fieldCount = record.size();
        }
        m_fieldLengthArr = new int[m_fieldCount];
        return record;
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
        m_userData = userData;
    }

    /**
     * Verify that the alias exists in the map and generate an initialization error if it is
     * missing.
     *
     * @param acdMap Map<String,Object>
     * @param alias String
     */
    protected void verifyRequiredAlias(Map<String, Object> acdMap, String alias) {
        if (!acdMap.containsKey(alias)) {
            m_initErrors.add("An assessment definition column with required alias " + alias
                    + " cannot be found in the selected assessment definition");
        }
    }

    /**
     * Verify that the required field exists and generate an initialization error if field is
     * missing.
     *
     * @param record List<String>
     * @param field String
     * @param parameter String
     */
    protected void verifyRequiredField(List<String> record, String field, String parameter) {
        if (StringUtils.isEmpty(field)) {
            m_initErrors.add("No field name found for parameter " + parameter);
        } else if (!record.contains(field)) {
            m_initErrors.add("Field name " + field + " for parameter " + parameter + " not found in input file");
        }
    }

    /**
     * Used to add an alias to the map.
     *
     * @param aliasPrefix String
     * @param map Map<String,Object>
     * @param alias String
     * @param obj Object
     */
    private void addAliasColumn(String aliasPrefix, Map<String, Object> map, String alias, Object obj) {
        if (!StringUtils.isEmpty(alias) && alias.startsWith(aliasPrefix)) {
            alias = alias.substring(aliasPrefix.length());
        } else if (!ALIAS_PERIOD_FIELD.equals(alias) && !ALIAS_SCHOOL_YEAR_FIELD.equals(alias)
                && !ALIAS_TEST_CODE_FIELD.equals(alias)) {
            alias = "";
        }
        if (!StringUtils.isEmpty(alias)) {
            map.put(alias, obj);
        }
    }

    /**
     * Write a list of strings to the results handler.
     *
     * @param list List<String>
     * @throws X2BaseException exception
     */
    private void exportList(List<String> list) throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        for (String err : list) {
            buffer.append(err);
            buffer.append('\n');
        }
        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Populate positions of headings from the file.
     *
     * @param record List
     */
    private void populateHeadingPositions(List record) {
        m_headingPositions = new HashMap();
        Iterator stringHeadings = record.iterator();
        int position = 0;
        Collection<String> headings = m_xmlManager.getHeadings().values();
        while (stringHeadings.hasNext()) {
            String stringHeading = (String) stringHeadings.next();

            if (headings.contains(stringHeading)) {
                m_headingPositions.put(stringHeading, Integer.valueOf(position));
            }
            ++position;
        }
    }

}
