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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataTableConfig;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ExtendedDataField;
import com.follett.fsc.core.k12.beans.ExtendedDataTable;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.InvalidDictionaryIdException;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DictionaryHelper;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.presentation.SecondaryDetailFormatter;
import com.follett.fsc.core.k12.web.template.Property;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.sis.model.beans.StudentCase;
import com.x2dev.sis.model.beans.StudentCaseChild;
import com.x2dev.sis.model.beans.StudentCasePerson;
import com.x2dev.sis.model.beans.StudentSchoolChoice;
import com.x2dev.sis.tools.procedures.StudentCaseSearchProcedure;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import org.jdom.Element;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * Procedure for determining student case search results.
 *
 * @author X2 Development Corporation
 */
public class StandardStudentCaseSearchProcedure extends StudentCaseSearchProcedure {
    // Template constants
    private static final String TEMPLATE_CONTEXT_STUDENT = "student.std.list.detail";
    private static final String TEMPLATE_CONTEXT_CASE = "district.stc.list.detail";

    // XML constants
    private static final String ATTRIBUTE_COUNT = "count";
    private static final String ATTRIBUTE_FIELD_ID = "field-id";
    private static final String ATTRIBUTE_OID = "oid";
    private static final String ATTRIBUTE_VALUE = "value";
    private static final String ELEMENT_BEAN = "bean";
    private static final String ELEMENT_CASES = "cases";
    private static final String ELEMENT_FIELD = "field";
    private static final String ELEMENT_RESPONSE = "response";
    private static final String ELEMENT_STUDENTS = "students";

    // Delimiter constants
    private static final char PROPERTY_DELIMITER = '|';

    private X2Criteria m_caseCriteria;
    private List<ModelProperty> m_caseProperties;
    private DictionaryHelper m_dictionaryHelper;
    private Map<String, Collection<ModelProperty>> m_properties;
    private X2Criteria m_studentCriteria;
    private List<ModelProperty> m_studentProperties;

    /**
     * Constructs a new StandardStudentCaseSearchProcedure object.
     *
     * @param organization Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public StandardStudentCaseSearchProcedure(Organization organization, User user, X2Broker broker, Locale locale) {
        super(organization, user, broker, locale);

        initialize();
    }

    /**
     * Destroy.
     *
     * @see com.follett.fsc.core.k12.web.SessionObject#destroy()
     */
    @Override
    public void destroy() {
        m_properties.clear();
        m_properties = null;

        m_studentProperties.clear();
        m_studentProperties = null;

        m_caseProperties.clear();
        m_caseProperties = null;
    }

    /**
     * Gets the max properties per column.
     *
     * @return int
     * @see com.x2dev.sis.tools.procedures.StudentCaseSearchProcedure#getMaxPropertiesPerColumn()
     */
    @Override
    public int getMaxPropertiesPerColumn() {
        return 6;
    }

    /**
     * Gets the max search results.
     *
     * @return int
     * @see com.x2dev.sis.tools.procedures.StudentCaseSearchProcedure#getMaxSearchResults()
     */
    @Override
    public int getMaxSearchResults() {
        return 15;
    }

    /**
     * Gets the student case properties.
     *
     * @return Collection
     * @see com.x2dev.sis.tools.procedures.StudentCaseSearchProcedure#getStudentCaseProperties()
     */
    @Override
    public Collection<ModelProperty> getStudentCaseProperties() {
        return m_caseProperties;
    }

    /**
     * Gets the student properties.
     *
     * @return Collection
     * @see com.x2dev.sis.tools.procedures.StudentCaseSearchProcedure#getStudentProperties()
     */
    @Override
    public Collection<ModelProperty> getStudentProperties() {
        return m_studentProperties;
    }

    /**
     * Search.
     *
     * @param searchParameters Map<String,String>
     * @return String
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.StudentCaseSearchProcedure#search(java.util.Map)
     */
    @Override
    public String search(Map<String, String> searchParameters) throws Exception {
        String xmlResults = null;

        /*
         * Build criteria.
         */
        buildCriteria(searchParameters);

        /*
         * Execute queries.
         */
        Element root = new Element(ELEMENT_RESPONSE);
        root.addContent(buildXmlResponse(ELEMENT_STUDENTS, buildStudentQuery(), getStudentProperties()));
        root.addContent(buildXmlResponse(ELEMENT_CASES, buildCaseQuery(), getStudentCaseProperties()));

        /*
         * Generate the XML.
         */
        org.jdom.Document document = new org.jdom.Document(root);
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        XMLOutputter outputter = new XMLOutputter(Format.getCompactFormat());
        outputter.output(document, outputStream);

        xmlResults = new String(outputStream.toByteArray());

        return xmlResults;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_dictionaryHelper = new DictionaryHelper(getBroker(), getLocale());

        m_properties = new HashMap<String, Collection<ModelProperty>>();

        loadStudentProperties();
        loadStudentCaseProperties();
    }

    /**
     * Adds the criteria for the passed field ID and value. If multiple model properties are found
     * for the passed ID, the criteria is or'd together.
     *
     * @param criteria X2Criteria
     * @param id String
     * @param valueAsString String
     * @param extendedDictionaryColumn String
     */
    private void addCriteria(X2Criteria criteria, String id, String valueAsString, String extendedDictionaryColumn) {
        Collection<ModelProperty> properties = getModelProperties(id);

        if (!CollectionUtils.isEmpty(properties)) {
            X2Criteria andCriteria = new X2Criteria();

            for (ModelProperty modelProperty : properties) {
                DataDictionaryField field = modelProperty.getField();

                Object value = m_dictionaryHelper.getValueAsObject(field, valueAsString);
                String extendedDictionaryOid = modelProperty.getDictionary().getExtendedDictionaryOid();

                X2Criteria orCriteria = new X2Criteria();

                if (field.isString()) {
                    orCriteria.addEndsWithIgnoreCase(modelProperty.getBeanPath(), value);
                } else {
                    orCriteria.addEqualTo(modelProperty.getBeanPath(), value);
                }

                if (!StringUtils.isEmpty(extendedDictionaryColumn) && !StringUtils.isEmpty(extendedDictionaryOid)) {
                    orCriteria.addEqualTo(extendedDictionaryColumn, extendedDictionaryOid);
                }

                andCriteria.addOrCriteria(orCriteria);
            }

            if (!andCriteria.isEmpty()) {
                criteria.addAndCriteria(andCriteria);
            }
        }
    }

    /**
     * Returns the report query by criteria used to find the student case beans.
     *
     * @return ReportQueryByCriteria
     */
    private ReportQueryByCriteria buildCaseQuery() {
        Collection<ModelProperty> displayProperties = getStudentCaseProperties();

        String[] columns = new String[displayProperties.size() + 1];
        getQueryColumns(displayProperties).toArray(columns);

        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentCase.class, columns, m_caseCriteria);
        query.addOrderByAscending(StudentCase.COL_NAME_VIEW);

        return query;
    }

    /**
     * Builds the <code>m_studentCriteria</code> and <code>m_caseCriteria</code> collections based
     * on
     * the passed map of search parameters.
     * <p>
     * The map will always be in the following format:
     * <p>
     * <code>&lt;model-property&gt|&lt;value&gt;</code>
     * <p>
     * <code>relStdPsnOid.psnNameFirst|John</code>
     *
     * @param searchParameters Map<String,String>
     */
    private void buildCriteria(Map<String, String> searchParameters) {
        m_studentCriteria = new X2Criteria();
        m_caseCriteria = new X2Criteria();

        Template template = getTemplate();

        for (String dictionaryPath : searchParameters.keySet()) {
            String searchValueAsString = searchParameters.get(dictionaryPath);

            if (!StringUtils.isEmpty(searchValueAsString)) {
                ModelProperty modelProperty = null;

                try {
                    modelProperty = new ModelProperty(dictionaryPath, getBroker().getPersistenceKey());
                } catch (InvalidDictionaryIdException idide) {
                    AppGlobals.getLog().severe(idide.getMessage());
                }

                Property templateProperty = null;

                Set<Property> templateProperties = template.getProperty(modelProperty);
                if (!CollectionUtils.isEmpty(templateProperties)) {
                    templateProperty = templateProperties.iterator().next();
                }

                if (templateProperty != null && !StringUtils.isEmpty(templateProperty.getSearchParameter())) {
                    String searchParameter = templateProperty.getSearchParameter();
                    List<String> searchFieldIds =
                            StringUtils.convertDelimitedStringToList(searchParameter, PROPERTY_DELIMITER, true);

                    /*
                     * Add the student property criteria.
                     */
                    addCriteria(m_studentCriteria, searchFieldIds.get(0), searchValueAsString, null);

                    /*
                     * Add the case property criteria.
                     */
                    addCriteria(m_caseCriteria, searchFieldIds.get(1), searchValueAsString,
                            StudentCase.COL_EXTENDED_DATA_DICTIONARY_OID);
                }
            }
        }
    }

    /**
     * Returns the report query by criteria used to find the student beans.
     *
     * @return ReportQueryByCriteria
     */
    private ReportQueryByCriteria buildStudentQuery() {
        Collection<ModelProperty> displayProperties = getStudentProperties();

        String[] columns = new String[displayProperties.size() + 1];
        getQueryColumns(displayProperties).toArray(columns);

        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, m_studentCriteria);
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        return query;
    }

    /**
     * Returns the XML element for the passed query and collection of display properties.
     *
     * @param elementName String
     * @param query ReportQueryByCriteria
     * @param displayProperties Collection<ModelProperty>
     * @return Element
     */
    private Element buildXmlResponse(String elementName,
                                     ReportQueryByCriteria query,
                                     Collection<ModelProperty> displayProperties) {
        Element root = new Element(elementName);

        int count = getBroker().getCount(query);
        if (count > 0) {
            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                int resultCount = 0;
                while (iterator.hasNext() && resultCount < getMaxSearchResults()) {
                    Object[] record = (Object[]) iterator.next();
                    String oid = (String) record[displayProperties.size()];

                    Element bean = new Element(ELEMENT_BEAN);
                    bean.setAttribute(ATTRIBUTE_OID, oid);

                    int propertyCount = 0;
                    for (ModelProperty modelProperty : displayProperties) {
                        String fieldId = modelProperty.getDictionaryPath();
                        Object valueAsObject = record[propertyCount];

                        String valueAsString =
                                m_dictionaryHelper.getValueAsString(modelProperty.getField(), valueAsObject);
                        if (valueAsString == null) {
                            valueAsString = "";
                        }

                        if (propertyCount == 0 && !StringUtils.isEmpty(valueAsString)) {
                            String templateContext = null;
                            String objectPrefix = null;

                            if (oid.toUpperCase().startsWith(SisStudent.OBJECT_PREFIX)) {
                                templateContext = TEMPLATE_CONTEXT_STUDENT;
                                objectPrefix = SisStudent.OBJECT_PREFIX;
                            } else if (oid.toUpperCase().startsWith(StudentCase.OBJECT_PREFIX)) {
                                templateContext = TEMPLATE_CONTEXT_CASE;
                                objectPrefix = StudentCase.OBJECT_PREFIX;
                            }

                            valueAsString = SecondaryDetailFormatter.getHTMLOutput(oid,
                                    templateContext, objectPrefix, valueAsString, getHttpServletRequest());
                        }

                        Element field = new Element(ELEMENT_FIELD);
                        field.setAttribute(ATTRIBUTE_FIELD_ID, fieldId);
                        field.setAttribute(ATTRIBUTE_VALUE, valueAsString);

                        bean.addContent(field);

                        propertyCount++;
                    }

                    root.addContent(bean);
                    resultCount++;
                }
            } finally {
                iterator.close();
            }
        }

        root.setAttribute(ATTRIBUTE_COUNT, String.valueOf(count));

        return root;
    }

    /**
     * Returns a collection of model properties for the passed ID. The ID could be a dictionary
     * path,
     * alias, or combination of both.
     *
     * @param id String
     * @return Collection<ModelProperty>
     */
    private Collection<ModelProperty> getModelProperties(String id) {
        Collection<ModelProperty> modelProperties = m_properties.get(id);

        if (CollectionUtils.isEmpty(modelProperties) && !StringUtils.isEmpty(id)) {
            modelProperties = new ArrayList<ModelProperty>();

            if (id.contains(ALIAS_PREFIX)) {
                Criteria criteria = new Criteria();

                String alias = id.substring(id.indexOf(ALIAS_PREFIX) + ALIAS_PREFIX.length());
                criteria.addEqualTo(ExtendedDataDictionary.REL_EXTENDED_DATA_TABLES + PATH_DELIMITER +
                        ExtendedDataTable.REL_EXTENDED_DATA_FIELDS + PATH_DELIMITER +
                        ExtendedDataField.COL_ALIAS, alias);

                List<String> studentCaseDictionaryIds = Arrays.asList(StudentCase.DICTIONARY_ID,
                        StudentCaseChild.DICTIONARY_ID,
                        StudentCasePerson.DICTIONARY_ID,
                        StudentSchoolChoice.DICTIONARY_ID);

                criteria.addIn(ExtendedDataDictionary.REL_EXTENDED_DATA_TABLES + PATH_DELIMITER +
                        ExtendedDataTable.REL_DATA_TABLE_CONFIG + PATH_DELIMITER +
                        DataTableConfig.COL_DATA_TABLE_OID, studentCaseDictionaryIds);

                QueryByCriteria query = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
                QueryIterator iterator = getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        ExtendedDataDictionary extendedDictionary = (ExtendedDataDictionary) iterator.next();
                        DataDictionary dictionary = DataDictionary.getDistrictDictionary(extendedDictionary,
                                getBroker().getPersistenceKey());

                        String modelPath = DictionaryHelper.translateAlias(id, dictionary);
                        modelProperties.add(new ModelProperty(modelPath, dictionary));
                    }
                } finally {
                    iterator.close();
                }
            } else {
                modelProperties.add(new ModelProperty(id, getBroker().getPersistenceKey()));
            }

            m_properties.put(id, modelProperties);
        }

        return modelProperties;
    }

    /**
     * Returns the query columns for the collection of model properties. The OID column is always
     * added to the collection.
     *
     * @param displayProperties Collection<ModelProperty>
     * @return List<String>
     */
    private List<String> getQueryColumns(Collection<ModelProperty> displayProperties) {
        List<String> queryColumns = new ArrayList<String>();

        if (!CollectionUtils.isEmpty(displayProperties)) {
            for (ModelProperty modelProperty : displayProperties) {
                queryColumns.add(modelProperty.getBeanPath());
            }
        }

        queryColumns.add(X2BaseBean.COL_OID);

        return queryColumns;
    }

    /**
     * Loads the collection of student case display properties.
     */
    private void loadStudentCaseProperties() {
        m_caseProperties = new ArrayList<ModelProperty>();
        m_caseProperties
                .add(new ModelProperty(StudentCase.class, StudentCase.COL_CASE_ID, getBroker().getPersistenceKey()));
        m_caseProperties
                .add(new ModelProperty(StudentCase.class, StudentCase.COL_NAME_VIEW, getBroker().getPersistenceKey()));
        m_caseProperties.add(new ModelProperty(StudentCase.class,
                StudentCase.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER + ExtendedDataDictionary.COL_NAME,
                getBroker().getPersistenceKey()));
        m_caseProperties.add(
                new ModelProperty(StudentCase.class, StudentCase.COL_CREATED_TIME, getBroker().getPersistenceKey()));
        m_caseProperties.add(
                new ModelProperty(StudentCase.class, StudentCase.COL_WORKFLOW_PHASE, getBroker().getPersistenceKey()));
        m_caseProperties.add(new ModelProperty(StudentCase.class,
                StudentCase.REL_USER + PATH_DELIMITER + SisUser.COL_NAME_VIEW, getBroker().getPersistenceKey()));
    }

    /**
     * Loads the collection of student display properties.
     */
    private void loadStudentProperties() {
        m_studentProperties = new ArrayList<ModelProperty>();
        m_studentProperties
                .add(new ModelProperty(SisStudent.class, SisStudent.COL_LOCAL_ID, getBroker().getPersistenceKey()));
        m_studentProperties
                .add(new ModelProperty(SisStudent.class, SisStudent.COL_NAME_VIEW, getBroker().getPersistenceKey()));
        m_studentProperties
                .add(new ModelProperty(SisStudent.class, SisStudent.COL_ADDRESS_VIEW, getBroker().getPersistenceKey()));
        m_studentProperties
                .add(new ModelProperty(SisStudent.class, SisStudent.COL_YOG, getBroker().getPersistenceKey()));
        m_studentProperties.add(new ModelProperty(SisStudent.class,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_DOB, getBroker().getPersistenceKey()));
        m_studentProperties.add(
                new ModelProperty(SisStudent.class, SisStudent.COL_ENROLLMENT_STATUS, getBroker().getPersistenceKey()));
        m_studentProperties.add(new ModelProperty(SisStudent.class,
                SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME, getBroker().getPersistenceKey()));
    }
}
