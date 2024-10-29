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
package com.x2dev.procedures.sys.sped.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.OrganizationAttributes;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.tools.script.ScriptManager;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.script.ScriptException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * The Class MaSpedAttribHelper.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class MaSpedAttribHelper {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Class ContactParameters.
     */
    public static class ContactParameters {
        private String m_name;
        private String m_address1;
        private String m_address2;

        /**
         * Instantiates a new contact parameters.
         *
         * @param name String
         * @param address1 String
         * @param address2 String
         */
        public ContactParameters(String name, String address1, String address2) {
            super();
            this.m_name = name;
            this.m_address1 = address1;
            this.m_address2 = address2;
        }

        /**
         * Gets the name.
         *
         * @return the name
         */
        public String getName() {
            return m_name;
        }

        /**
         * Gets the address 1.
         *
         * @return the address1
         */
        public String getAddress1() {
            return m_address1;
        }

        /**
         * Gets the address 2.
         *
         * @return the address2
         */
        public String getAddress2() {
            return m_address2;
        }

    }

    private static final String ALIAS_REPORT_FIELD_SCRIPTS = "ora-ma-rpt-field-scripts";
    private static final String ALIAS_SPED_18_DECISION = "sped-decision";
    private static final String ATTRIBUTE_NAME = "name";
    private static final String DECISTION_STATUS_DELEGATED = "Delegated";
    private static final String DECISTION_STATUS_OWN_BEHALF = "Own Behalf";
    private static final String DECISTION_STATUS_SHARED = "Shared";
    private static final String OWNER_PREFIX = "owner";
    private static final String PATH_SEPARATOR = ".";
    private static final String PATTERN_REPORT_FIELD = "^masped:([^:]+)(:(\\S+))?$";
    private static final String RESULT_NO_SCRIPT_FOUND = "NO SCRIPT";
    private static final String RESULT_SCRIPT_FAILED = "SCRIPT FAILED";
    private static final String SCRIPT_PARAMETERS = "bean";
    private static final String XPATH_REPORT_FIELDS = "//report-field";


    /**
     * Gets the mailing address.
     *
     * @param person Person
     * @return Address
     */
    public static Address getMailingAddress(Person person) {
        Address addr = person.getMailingAddress();
        if (addr == null) {
            addr = person.getPhysicalAddress();
        }
        String address = MaSpedAttribHelper.getStudentContactAddress(addr);
        return StringUtils.isEmpty(address) ? person.getPhysicalAddress() : addr;
    }

    /**
     * Gets the address.
     *
     * @param address Address
     * @return String
     */
    public static String getStudentContactAddress(Address address) {
        String streetLine = getStudentContactStreet(address);
        String cityAndZip = getStudentContactCityAndZip(address);
        return streetLine + (streetLine.isEmpty() ? "" : ", ") + cityAndZip;
    }

    /**
     * Gets the city and zip.
     *
     * @param address Address
     * @return String
     */
    public static String getStudentContactCityAndZip(Address address) {
        String cityAndZip = "";
        if (address != null) {
            cityAndZip = (StringUtils.isEmpty(address.getCity()) ? "" : address.getCity() + ", ")
                    + StringUtils.unNullify(address.getState()) + " "
                    + StringUtils.unNullify(address.getPostalCode());
        }
        return cityAndZip;
    }

    /**
     * Gets the list of student contact parameters.
     *
     * @param iep IepData
     * @param broker X2Broker
     * @return List
     */
    public static List<ContactParameters> getStudentContactParameters(IepData iep, X2Broker broker) {
        List<ContactParameters> list = new ArrayList();
        if (iep != null) {
            boolean includeParents = true;
            boolean studentOnTeam = false;
            for (IepTeamMember member : iep.getTeamMembers()) {
                if (iep.getStudent().getPersonOid().equals(member.getPersonOid())) {
                    studentOnTeam = true;
                    break;
                }
            }
            if (studentOnTeam && isStudentOwnBehalf(iep.getStudent())) {
                includeParents = false;
            }
            if (includeParents) {
                List<StudentContact> contacts = MaSpedAttribHelper.getStudentContacts(iep, 2, broker);
                if (!contacts.isEmpty()) {
                    String name = contacts.get(0).getContact().getNameView();
                    Address adr = MaSpedAttribHelper.getMailingAddress(contacts.get(0).getPerson());
                    String address = MaSpedAttribHelper.getStudentContactAddress(adr);
                    String address1 = MaSpedAttribHelper.getStudentContactStreet(adr);
                    String address2 = MaSpedAttribHelper.getStudentContactCityAndZip(adr);
                    if (contacts.size() > 1) {
                        Address adr2 = MaSpedAttribHelper.getMailingAddress(contacts.get(1).getPerson());

                        String secondAddress = MaSpedAttribHelper.getStudentContactAddress(adr2);
                        if (address.equalsIgnoreCase(secondAddress)) {
                            name += " and " + contacts.get(1).getContact().getNameView();
                        } else {
                            list.add(new ContactParameters(contacts.get(1).getContact().getNameView(),
                                    MaSpedAttribHelper.getStudentContactStreet(adr2),
                                    MaSpedAttribHelper.getStudentContactCityAndZip(adr2)));
                        }
                    }
                    // Add first parent in position 0
                    list.add(0, new ContactParameters(name, address1, address2));
                }
            }
            if (studentOnTeam && isStudentDecistionStatus(iep.getStudent(),
                    Arrays.asList(DECISTION_STATUS_DELEGATED, DECISTION_STATUS_OWN_BEHALF, DECISTION_STATUS_SHARED))) {
                SisStudent student = iep.getStudent();
                Address adr = MaSpedAttribHelper.getMailingAddress(student.getPerson());
                list.add(new ContactParameters(student.getNameView(),
                        MaSpedAttribHelper.getStudentContactStreet(adr),
                        MaSpedAttribHelper.getStudentContactCityAndZip(adr)));
            }
        }
        return list;
    }

    /**
     * Gets the street.
     *
     * @param address Address
     * @return String
     */
    public static String getStudentContactStreet(Address address) {
        String streetLine = "";
        if (address != null) {
            streetLine = StringUtils.unNullify(address.getAddressLine01())
                    + (StringUtils.isEmpty(address.getAddressLine02()) ? "" : ", " + address.getAddressLine02());
        }

        return streetLine;
    }

    /**
     * Return student contacts which added like iep team members.
     * method created instead SpedUtils.getStudentContacts, because SpedUtils working with all
     * contacts
     * Contact sorted by COL_FORM_PRIORITY and limited by maxContacts param
     *
     * @param iep IepData
     * @param maxContacts int
     * @param broker X2Broker
     * @return List
     */
    public static List<StudentContact> getStudentContacts(IepData iep, int maxContacts, X2Broker broker) {
        List<StudentContact> contacts = new ArrayList<StudentContact>();
        /*
         * Load the student contact team members with form priority ordered by form priority
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentContact.COL_STUDENT_OID, iep.getStudentOid());
        criteria.addEqualTo(StudentContact.REL_CONTACT + PATH_DELIMITER +
                Contact.REL_PERSON + PATH_DELIMITER +
                SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                IepTeamMember.COL_IEP_DATA_OID, iep.getOid());
        criteria.addNotEmpty(StudentContact.REL_CONTACT + PATH_DELIMITER +
                Contact.REL_PERSON + PATH_DELIMITER +
                SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                IepTeamMember.COL_FORM_PRIORITY, broker.getPersistenceKey());

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);
        query.addOrderByAscending(StudentContact.REL_CONTACT + PATH_DELIMITER +
                Contact.REL_PERSON + PATH_DELIMITER +
                SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                IepTeamMember.COL_FORM_PRIORITY);
        contacts.addAll(broker.getCollectionByQuery(query));

        if (contacts.isEmpty()) {
            /*
             * Load additional student contact team members without form priority ordered by
             * emergency priority
             */
            criteria = new X2Criteria();
            criteria.addEqualTo(StudentContact.COL_STUDENT_OID, iep.getStudentOid());
            criteria.addEqualTo(StudentContact.REL_CONTACT + PATH_DELIMITER +
                    Contact.REL_PERSON + PATH_DELIMITER +
                    SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                    IepTeamMember.COL_IEP_DATA_OID, iep.getOid());
            criteria.addEmpty(StudentContact.REL_CONTACT + PATH_DELIMITER +
                    Contact.REL_PERSON + PATH_DELIMITER +
                    SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                    IepTeamMember.COL_FORM_PRIORITY, broker.getPersistenceKey());
            query = new QueryByCriteria(StudentContact.class, criteria);

            query.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);
            contacts.addAll(broker.getCollectionByQuery(query));
        }

        if (maxContacts != 0 && contacts.size() > maxContacts) {
            contacts = contacts.subList(0, maxContacts);
        }

        return contacts;
    }

    /**
     * Checks if is student own behalf.
     *
     * @param student SisStudent
     * @param statuses List<String>
     * @return true, if is student own behalf
     */
    public static boolean isStudentDecistionStatus(SisStudent student, List<String> statuses) {
        boolean result = false;
        int age = student.getPerson().getAge();
        if (age >= 18) {
            String decision = (String) student.getFieldValueByAlias(ALIAS_SPED_18_DECISION);
            if (!StringUtils.isEmpty(decision) && statuses.contains(decision)) {
                result = true;
            }
        }
        return result;
    }

    /**
     * Checks if is student own behalf.
     *
     * @param student SisStudent
     * @return true, if is student own behalf
     */
    public static boolean isStudentOwnBehalf(SisStudent student) {
        return isStudentDecistionStatus(student, Arrays.asList(DECISTION_STATUS_OWN_BEHALF));
    }

    private boolean m_abortOnError;
    private X2Broker m_broker;
    private DataDictionary m_dictionary;
    private Map<String, KeyValuePair<String, String>> m_mapJavaScripts = null;
    private OrganizationAttributes m_ora;
    private Pattern m_pattern = Pattern.compile(PATTERN_REPORT_FIELD);
    private ScriptManager m_scriptManager = null;

    /**
     * The Class MaSpedCollectionDataSource.
     */
    public class MaSpedCollectionDataSource extends BeanCollectionDataSource {
        private X2BaseBean m_formOwner;

        /**
         * Instantiates a new ma sped collection data source.
         *
         * @param formOwner X2BaseBean
         * @param beanCollection Collection<? extends X2BaseBean>
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        public MaSpedCollectionDataSource(X2BaseBean formOwner, Collection<? extends X2BaseBean> beanCollection,
                DataDictionary dictionary,
                Locale locale) {
            super(beanCollection, dictionary, locale);
            m_formOwner = formOwner;
        }

        /**
         * Gets the field value.
         *
         * @param fieldName String
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.reports.SimpleFormDataSource#getFieldValue(java.lang.String)
         */
        @Override
        protected Object getFieldValue(String fieldName) throws X2BaseException {
            Object fieldValue = getSpedAttribValue(fieldName, getCurrentBean(), m_formOwner);
            if (fieldValue == null) {
                fieldValue = super.getFieldValue(fieldName);
            }
            return fieldValue;
        }


    }

    /**
     * The Class MaSpedDataSource.
     */
    public class MaSpedDataSource extends SimpleFormDataSource {
        private X2BaseBean m_formOwner;

        /**
         * Instantiates a new ma sped data source.
         *
         * @param formStorage X2BaseBean
         * @param formOwner X2BaseBean
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        public MaSpedDataSource(X2BaseBean formStorage, X2BaseBean formOwner, DataDictionary dictionary,
                Locale locale) {
            super(formStorage, formOwner, dictionary, locale);
            m_formOwner = formOwner;
        }

        /**
         * Instantiates a new ma sped data source.
         *
         * @param formStorage X2BaseBean
         * @param formOwner X2BaseBean
         * @param multiPageGrid ReportDataGrid
         * @param overflowFormat byte[]
         * @param overflowFormatFields Map<String,Object>
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        public MaSpedDataSource(X2BaseBean formStorage, X2BaseBean formOwner, ReportDataGrid multiPageGrid,
                byte[] overflowFormat, Map<String, Object> overflowFormatFields, DataDictionary dictionary,
                Locale locale) {
            super(formStorage, formOwner, multiPageGrid, overflowFormat, overflowFormatFields, dictionary, locale);
            m_formOwner = formOwner;
        }

        /**
         * Gets the field value.
         *
         * @param fieldName String
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.reports.SimpleFormDataSource#getFieldValue(java.lang.String)
         */
        @Override
        protected Object getFieldValue(String fieldName) throws X2BaseException {
            Object fieldValue = getSpedAttribValue(fieldName, getCurrentBean(), m_formOwner);
            if (fieldValue == null) {
                fieldValue = super.getFieldValue(fieldName);
            }
            return fieldValue;
        }

    }

    /**
     * Instantiates a new ma sped attrib helper.
     *
     * @param broker X2Broker
     * @param abortOnError boolean
     */
    public MaSpedAttribHelper(X2Broker broker, boolean abortOnError) {
        m_broker = broker;
        m_abortOnError = abortOnError;
    }

    /**
     * Gets the ma sped collection data source.
     *
     * @param formOwner X2BaseBean
     * @param beanCollection Collection<? extends X2BaseBean>
     * @param dictionary DataDictionary
     * @param locale Locale
     * @return Ma sped collection data source
     */
    public MaSpedCollectionDataSource getMaSpedCollectionDataSource(X2BaseBean formOwner,
                                                                    Collection<? extends X2BaseBean> beanCollection,
                                                                    DataDictionary dictionary,
                                                                    Locale locale) {
        return new MaSpedCollectionDataSource(formOwner, beanCollection, dictionary, locale);
    }

    /**
     * Gets the ma sped data source.
     *
     * @param storage X2BaseBean
     * @param formOwner X2BaseBean
     * @param dictionary DataDictionary
     * @param locale Locale
     * @return JR data source
     */
    public MaSpedDataSource getMaSpedDataSource(X2BaseBean storage,
                                                X2BaseBean formOwner,
                                                DataDictionary dictionary,
                                                Locale locale) {
        return new MaSpedDataSource(storage, formOwner, dictionary, locale);
    }

    /**
     * Gets the ma sped data source.
     *
     * @param storage X2BaseBean
     * @param formOwner X2BaseBean
     * @param multiPageGrid ReportDataGrid
     * @param overflowFormat byte[]
     * @param overflowFormatFields Map<String,Object>
     * @param dictionary DataDictionary
     * @param locale Locale
     * @return JR data source
     */
    public MaSpedDataSource getMaSpedDataSource(X2BaseBean storage,
                                                X2BaseBean formOwner,
                                                ReportDataGrid multiPageGrid,
                                                byte[] overflowFormat,
                                                Map<String, Object> overflowFormatFields,
                                                DataDictionary dictionary,
                                                Locale locale) {
        return new MaSpedDataSource(storage, formOwner, multiPageGrid, overflowFormat, overflowFormatFields, dictionary,
                locale);
    }

    /**
     * Gets the sped attrib value.
     *
     * @param fieldName String
     * @param bean X2BaseBean
     * @param formOwner X2BaseBean
     * @return Object
     */
    public Object getSpedAttribValue(String fieldName, X2BaseBean bean, X2BaseBean formOwner) {
        Object fieldValue = null;
        Matcher matcher = m_pattern.matcher(fieldName);
        if (matcher.matches()) {
            String aliasName = matcher.group(1);
            String beanPath = matcher.group(3);
            try {
                if (!StringUtils.isEmpty(beanPath)) {
                    if (beanPath.startsWith(OWNER_PREFIX)) {
                        bean = formOwner;
                        beanPath = beanPath.substring(OWNER_PREFIX.length());
                    }
                    if (beanPath.startsWith(PATH_SEPARATOR)) {
                        bean = formOwner;
                        beanPath = beanPath.substring(PATH_SEPARATOR.length());
                    }
                    if (!StringUtils.isEmpty(beanPath)) {
                        Object value = WebUtils.getProperty(bean, beanPath);
                        if (X2BaseBean.class.isAssignableFrom(value.getClass())) {
                            bean = (X2BaseBean) value;
                        }
                    }
                }
                String functionName = getScriptFunctionName(aliasName);
                if (StringUtils.isEmpty(functionName)) {
                    fieldValue = RESULT_NO_SCRIPT_FOUND;
                } else if (bean != null && !bean.isNew()) {
                    try {
                        fieldValue = getScriptManager().invokeFunction(getScriptFunctionName(aliasName),
                                new Object[] {bean});
                    } catch (ScriptException e) {
                        /*
                         * There are special cases when a report is migrated from Iep to Student.
                         * In this case, the same methods are required but the bean here is Student
                         * instead of IepData.
                         * Since these methods mainly use student info and there is a desire to
                         * maintain only a single method, the invoke is retried with an IEP bean
                         */
                        if (bean instanceof SisStudent) {
                            IepData iep = X2BaseBean.newInstance(IepData.class, getBroker().getPersistenceKey());
                            iep.setStudentOid(bean.getOid());
                            fieldValue = getScriptManager().invokeFunction(getScriptFunctionName(aliasName),
                                    new Object[] {iep});
                        } else {
                            throw (e);
                        }
                    }
                }
            } catch (NoSuchMethodException | ScriptException | X2BaseException e) {
                fieldValue = RESULT_SCRIPT_FAILED;
                if (m_abortOnError) {
                    throw new X2RuntimeException(e);
                }
            }
        }
        return fieldValue;
    }

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     */
    private X2Broker getBroker() {
        return m_broker;
    }

    /**
     * Gets the dictionary.
     *
     * @return Data dictionary
     */
    private DataDictionary getDictionary() {
        if (m_dictionary == null) {
            m_dictionary = DataDictionary.getDistrictDictionary(
                    MaSpedWorkflowProcedure.getSpedConfigDictionary(getBroker()), getBroker().getPersistenceKey());
        }
        return m_dictionary;
    }

    /**
     * Gets the organization attribute.
     *
     * @param javaName String
     * @return String
     */
    private String getOrganizationAttribute(String javaName) {
        if (m_ora == null) {
            m_ora = MaSpedWorkflowProcedure.getSpedConfig(getBroker());
        }
        return (String) m_ora.getFieldValueByBeanPath(javaName);
    }

    /**
     * Gets the script function name.
     *
     * @param aliasName String
     * @return String
     * @throws X2BaseException exception
     */
    private String getScriptFunctionName(String aliasName) throws X2BaseException {
        KeyValuePair<String, String> pair = getScriptPair(aliasName);
        return pair == null ? null : pair.getKey();
    }


    /**
     * Gets the script manager.
     *
     * @return Script manager
     * @throws X2BaseException exception
     */
    private ScriptManager getScriptManager() throws X2BaseException {
        if (m_scriptManager == null) {
            try {
                m_scriptManager = new ScriptManager(ScriptManager.ENGINE_JAVASCRIPT);
            } catch (ScriptException e) {
                throw new X2BaseException(e);
            }
        }
        return m_scriptManager;
    }

    /**
     * Gets the script pair.
     *
     * @param aliasName String
     * @return Key value pair
     * @throws X2BaseException exception
     */
    private KeyValuePair<String, String> getScriptPair(String aliasName) throws X2BaseException {
        KeyValuePair<String, String> pair = null;
        if (m_mapJavaScripts == null) {
            m_mapJavaScripts = new HashMap();
            DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_REPORT_FIELD_SCRIPTS);
            if (field != null) {
                String xmlString = getOrganizationAttribute(field.getJavaName());
                if (!StringUtils.isEmpty(xmlString)) {
                    NodeList nodeList = null;
                    try {
                        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
                        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
                        Document docInputDefinition =
                                dBuilder.parse(new InputSource(new ByteArrayInputStream(xmlString.getBytes())));

                        XPathFactory xPathfactory = XPathFactory.newInstance();
                        XPath xpath = xPathfactory.newXPath();
                        XPathExpression expr = xpath.compile(XPATH_REPORT_FIELDS);
                        nodeList = (NodeList) expr.evaluate(docInputDefinition, XPathConstants.NODESET);
                    } catch (XPathExpressionException | ParserConfigurationException | SAXException | IOException e) {
                        throw new X2BaseException(e);
                    }
                    if (nodeList.getLength() > 0) {
                        for (int count = 0; count < nodeList.getLength(); count++) {
                            Element item = (Element) nodeList.item(count);
                            String name = item.getAttribute(ATTRIBUTE_NAME);
                            String script = item.getTextContent();
                            if (!StringUtils.isEmpty(name) && !StringUtils.isEmpty(script)) {
                                StringBuilder javascript = new StringBuilder();

                                javascript.append(ScriptManager.FUNCTION_CONSTANT);

                                StringBuilder functionName = new StringBuilder();
                                functionName.append(name.replace("-", ScriptManager.FUNCTION_DELIMETER_CONSTANT));

                                javascript.append(functionName.toString());
                                javascript.append(ScriptManager.LEFT_PARENTHESIS);
                                javascript.append(SCRIPT_PARAMETERS);
                                javascript.append(ScriptManager.RIGHT_PARENTHESIS);
                                javascript.append(ScriptManager.LEFT_BRACKET);
                                javascript.append(script);
                                javascript.append(ScriptManager.RIGHT_BRACKET);
                                pair = new KeyValuePair(functionName.toString(), javascript.toString());
                                m_mapJavaScripts.put(name, pair);

                                getScriptManager().initializeScript(javascript.toString());
                            }
                        }
                    }
                }
            }
        }
        pair = m_mapJavaScripts.get(aliasName);
        return pair;
    }
}
