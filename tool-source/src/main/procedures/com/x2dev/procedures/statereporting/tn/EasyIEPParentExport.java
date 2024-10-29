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
package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 *
 * Exports the EasyIEP Parent data.
 *
 * @author X2 Development Corporation
 */
public class EasyIEPParentExport extends ExportJavaSource {
    private static final String CODE_NO = "N";
    private static final String CODE_YES = "Y";
    private static final String DISABLE_BEAN_PATH = "-9999";
    private static final int FIELD_COUNT = 14;

    // table oids
    private static final String LANGUAGE_RTB_OID = "rtbLanguage";
    private static final String PERSON_RELATIONSHIP_RTB_OID = "rtbPsnRelate";

    // aliases
    private static final String ALIAS_CONTACT_LANGUAGE = "all-cnt-ContactLanguage";
    private static final String ALIAS_PERSON_WORK_PHONE_EXT = "all-psn-WorkPhone1Ext";
    private static final String ALIAS_PERSON_WORK_PHONE2 = "all-psn-WorkPhone2";
    private static final String ALIAS_PERSON_WORK_PHONE2_EXT = "all-psn-WorkPhone2Ext";

    // bean paths
    private static final String BEAN_PATH_HOME_PHONE = "phone01";
    private static final String BEAN_PATH_CELL_PHONE = "phone02";
    private static final String BEAN_PATH_WORK_PHONE = "phone03";
    private static final String BEAN_PATH_EMAIL = "email01";
    private static final String BEAN_PATH_EMAIL2 = "email02";

    // default values
    private static final String PERSON_HOME_FAX = "-9999";
    private static final String PERSON_WORK_FAX = "-9999";
    private static final String PERSON_CELL_PHONE2 = "-9999";

    // other constants
    private static final String DELIMITER = ",";
    private static final String INPUT_PARAM_INCLUDE_ADDR_2_ = "includeAddr2";
    private static final String PARAM_EMERGENCY_PRIORITY_FILTER = "emergencyPriorityFilter";
    private static final String PARAM_EMERGENCY_PRIORITY_INDICATOR = "emergencyPriorityIndicator";
    private static final String RACE_CODE_HISPANIC = "H";
    private static final String RELATIONSHIP_CODE_GUARDIAN = "Guardian";
    private static final long serialVersionUID = 1L;
    private static final String STRIP_CHARACTER_ZERO = "0";

    // Grid fields
    private static final String FIELD_ADDRESS = "Address";
    private static final String FIELD_CELL_PHONE = "CellPhone";
    private static final String FIELD_CELL_PHONE2 = "CellPhone2";
    private static final String FIELD_CITY = "City";
    private static final String FIELD_DISTRICT_CODE = "DistrictCode";
    private static final String FIELD_EMAIL = "Email";
    private static final String FIELD_EMAIL2 = "Email2";
    private static final String FIELD_FIRST_NAME = "FirstName";
    private static final String FIELD_GUARDIAN = "Guardian";
    private static final String FIELD_HOME_FAX = "HomeFax";
    private static final String FIELD_HOME_PHONE = "HomePhone";
    private static final String FIELD_LANGUAGE = "Language";
    private static final String FIELD_LAST_NAME = "LastName";
    private static final String FIELD_MIDDLE_NAME = "MiddleName";
    private static final String FIELD_PARENT_TYPE = "ParentType";
    private static final String FIELD_STATE = "State";
    private static final String FIELD_STATE_CODE = "StateCode";
    private static final String FIELD_STUDENT_CODE = "StudentCode";
    private static final String FIELD_STUDENT_LIVES_HERE = "StudentLivesHere";
    private static final String FIELD_SUFFIX = "Suffix";
    private static final String FIELD_WORK_FAX = "WorkFax";
    private static final String FIELD_WORK_PHONE = "WorkPhone";
    private static final String FIELD_WORK_PHONE2 = "WorkPhone2";
    private static final String FIELD_ZIP_CODE = "ZipCode";

    /**
     * Class members.
     */
    private String m_beanPathStudentEisStateCode;
    private List<String> m_columns;
    private List<String> m_emergencyPriorities;
    private Boolean m_emergencyPriorityIndicator;
    private Map<String, String> m_hispanics;
    private Boolean m_includeAddr2;
    private Map<String, String> m_languageCodes;
    private Map<String, String> m_parentTypeCodes;
    private Map<String, String> m_personRaces;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid();
        Criteria criteria = buildCriteria();
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                Collection<StudentContact> studentContacts = student.getContacts();

                for (StudentContact studentContact : studentContacts) {
                    if ((!m_emergencyPriorityIndicator
                            || m_emergencyPriorities.contains(studentContact.getEmergencyPriority() + ""))
                            && !StringUtils.isEmpty(studentContact.getRelationshipCode())
                            && m_parentTypeCodes.containsKey(studentContact.getRelationshipCode())
                            && !StringUtils.isEmpty(m_parentTypeCodes.get(studentContact.getRelationshipCode()))) {
                        List<String> gridData = new ArrayList<String>(24);
                        boolean livesWith = studentContact.getLivesWithIndicator();
                        Person personContact = studentContact.getPerson();
                        Person personStudent = student.getPerson();
                        String workPhone = getFieldValue(personContact, BEAN_PATH_WORK_PHONE);
                        String workPhoneExt = getFieldValueByAlias(personContact, ALIAS_PERSON_WORK_PHONE_EXT);
                        String workPhone2 = getFieldValueByAlias(personContact, ALIAS_PERSON_WORK_PHONE2);
                        String workPhone2Ext = getFieldValueByAlias(personContact, ALIAS_PERSON_WORK_PHONE2_EXT);
                        if (personContact != null) {
                            grid.append();
                            gridData.add(personContact.getFirstName());
                            gridData.add(personContact.getLastName());
                            gridData.add(personContact.getMiddleName());
                            gridData.add(personContact.getNameSuffixCode());
                            gridData.add(m_parentTypeCodes.get(studentContact.getRelationshipCode()));
                            gridData.add(student.getLocalId());
                            gridData.add(StringUtils.stripStart(getFieldValue(student, m_beanPathStudentEisStateCode),
                                    STRIP_CHARACTER_ZERO));

                            String cntAddrLine01 = null;
                            String cntCity = null;
                            String cntState = null;
                            String cntPostalCode = null;
                            String addrLine02 = null;
                            Address cntAddr = personContact.getPhysicalAddress();
                            if (cntAddr != null) {
                                cntAddrLine01 = cntAddr.getAddressLine01();
                                if (m_includeAddr2) {
                                    addrLine02 = cntAddr.getAddressLine02();
                                    if (!StringUtils.isEmpty(addrLine02)) {
                                        addrLine02 = ", " + addrLine02;
                                    }
                                }
                                cntCity = cntAddr.getCity();
                                cntState = cntAddr.getState();
                                cntPostalCode = cntAddr.getPostalCode();

                            }
                            if (student != null && personStudent != null
                                    && student.getPerson().getPhysicalAddress() != null) {
                                if (m_includeAddr2 && (cntAddr == null || StringUtils.isEmpty(cntAddrLine01))) {
                                    addrLine02 = student.getPerson().getPhysicalAddress().getAddressLine02();
                                    if (!StringUtils.isEmpty(addrLine02)) {
                                        addrLine02 = ", " + addrLine02;
                                    }
                                }
                                if (addrLine02 == null) {
                                    addrLine02 = "";
                                }
                                gridData.add(!StringUtils.isEmpty(cntAddrLine01) ? cntAddrLine01 + addrLine02
                                        : personStudent.getPhysicalAddress().getAddressLine01() + addrLine02);
                                gridData.add(!StringUtils.isEmpty(cntCity) ? cntCity
                                        : personStudent.getPhysicalAddress().getCity());
                                gridData.add(!StringUtils.isEmpty(cntState) ? cntState
                                        : personStudent.getPhysicalAddress().getState());
                                gridData.add(!StringUtils.isEmpty(cntPostalCode) ? cntPostalCode
                                        : personStudent.getPhysicalAddress().getPostalCode());
                            }

                            gridData.add(formatPhoneNumber(getFieldValue(personContact, BEAN_PATH_HOME_PHONE)));
                            gridData.add(formatPhoneNumber(getFieldValue(personContact, PERSON_HOME_FAX)));
                            gridData.add(formatPhoneNumber(getWorkPhone(workPhone, workPhoneExt)));
                            gridData.add(formatPhoneNumber(getFieldValue(personContact, PERSON_WORK_FAX)));
                            gridData.add(formatPhoneNumber(getWorkPhone(workPhone2, workPhone2Ext)));
                            gridData.add(getFieldValue(personContact, BEAN_PATH_EMAIL));
                            gridData.add(getFieldValue(personContact, BEAN_PATH_EMAIL2));
                            gridData.add(formatPhoneNumber(getFieldValue(personContact, BEAN_PATH_CELL_PHONE)));
                            gridData.add(formatPhoneNumber(getFieldValue(personContact, PERSON_CELL_PHONE2)));
                            gridData.add(m_languageCodes.get(getContactBeanValue(studentContact.getContact(),
                                    ALIAS_CONTACT_LANGUAGE)));
                            gridData.add(getOrganization().getId());
                            gridData.add(livesWith ? String.valueOf(CODE_YES) : String.valueOf(CODE_NO));
                            gridData.add(studentContact.getRelationshipCode() != null &&
                                    studentContact.getRelationshipCode().equals(RELATIONSHIP_CODE_GUARDIAN)
                                            ? String.valueOf(CODE_YES)
                                            : String.valueOf(CODE_NO));

                            // Fill grid row with appropriate data
                            int gridSize = gridData.size();
                            for (int i = 0; i < gridSize; i++) {
                                String data = (gridData.get(i) != null ? gridData.get(i) : "");
                                grid.set(m_columns.get(i), data);
                            }
                        }
                    }
                }
            }
        } finally {
            students.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_beanPathStudentEisStateCode = translateAliasToJavaName("DOE EIS STATE ID");
        m_hispanics = new HashMap<String, String>();
        m_languageCodes = new HashMap<String, String>();
        m_parentTypeCodes = new HashMap<String, String>();
        m_personRaces = new HashMap<String, String>();
        Object priorityInd = getParameter(PARAM_EMERGENCY_PRIORITY_INDICATOR);
        m_emergencyPriorityIndicator =
                priorityInd != null && priorityInd instanceof Boolean && ((Boolean) priorityInd).booleanValue() ? true
                        : false;
        m_includeAddr2 =
                getParameter(INPUT_PARAM_INCLUDE_ADDR_2_) != null
                        && getParameter(INPUT_PARAM_INCLUDE_ADDR_2_) instanceof Boolean
                        && ((Boolean) getParameter(INPUT_PARAM_INCLUDE_ADDR_2_)).booleanValue() ? true
                                : false;
        m_emergencyPriorities = loadEmergencyPriorities();

        setIncludeHeaderRow(true);
        setLineSeparator("\r\n");
        setValueDelimiter(Character.valueOf('\t'));
        setUseValueWrappers(false);

        m_columns = new ArrayList<String>(FIELD_COUNT);

        m_columns.add(FIELD_FIRST_NAME);
        m_columns.add(FIELD_LAST_NAME);
        m_columns.add(FIELD_MIDDLE_NAME);
        m_columns.add(FIELD_SUFFIX);
        m_columns.add(FIELD_PARENT_TYPE);
        m_columns.add(FIELD_STUDENT_CODE);
        m_columns.add(FIELD_STATE_CODE);
        m_columns.add(FIELD_ADDRESS);
        m_columns.add(FIELD_CITY);
        m_columns.add(FIELD_STATE);
        m_columns.add(FIELD_ZIP_CODE);
        m_columns.add(FIELD_HOME_PHONE);
        m_columns.add(FIELD_HOME_FAX);
        m_columns.add(FIELD_WORK_PHONE);
        m_columns.add(FIELD_WORK_FAX);
        m_columns.add(FIELD_WORK_PHONE2);
        m_columns.add(FIELD_EMAIL);
        m_columns.add(FIELD_EMAIL2);
        m_columns.add(FIELD_CELL_PHONE);
        m_columns.add(FIELD_CELL_PHONE2);
        m_columns.add(FIELD_LANGUAGE);
        m_columns.add(FIELD_DISTRICT_CODE);
        m_columns.add(FIELD_STUDENT_LIVES_HERE);
        m_columns.add(FIELD_GUARDIAN);

        loadHispanicStudents();
        loadLanguageCodes();
        loadParentTypeCodes();
        loadStudentRaces();
    }

    /**
     * Builds the criteria to include only active students.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        String activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Student.COL_ENROLLMENT_STATUS, activeStatus);

        if (isSchoolContext()) {
            criteria.addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
        }

        return criteria;
    }

    /**
     * Formats phone numbers removes leading 1 if it exists and returns left most 10 digits so phone
     * extensions
     * will get removed.
     *
     * @param phone String
     * @return String
     */
    private static String formatPhoneNumber(String phone) {
        String strPhone = phone;

        if (strPhone != null) {
            // Strip out everything except numbers
            strPhone = strPhone.replaceAll("[^0-9]", "");

            // Check to see if number begins with 1
            if (strPhone.indexOf("1") == 0) {
                if (strPhone.length() >= 11) {
                    strPhone = strPhone.substring(1, 11);
                } else {
                    strPhone = strPhone.substring(1, strPhone.length());
                }
            }

            // final pass gets just the first 10 digits
            if (strPhone.length() >= 10) {
                strPhone = strPhone.substring(0, 10);
            }
        } else {
            strPhone = "";
        }

        return strPhone;
    }

    /**
     * Returns the value of the field associated with the passed bean path for the passed Contact
     * object.
     *
     * @param contact Contact
     * @param alias String
     * @return String
     */
    private String getContactBeanValue(Contact contact, String alias) {
        String value = "";

        if (alias != null) {
            value = (String) contact.getFieldValueByAlias(alias);
        }

        return value;
    }

    /**
     * Gets the selected field from the passed bean.
     *
     * @param bean X2BaseBean
     * @param path String
     * @return String
     */
    private String getFieldValue(X2BaseBean bean, String path) {
        String value = "";
        if (bean != null && path != null && !path.equals(DISABLE_BEAN_PATH)) {
            value = (String) bean.getFieldValueByBeanPath(path);
        }

        return value;
    }

    /**
     * Gets the selected field from the passed bean.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return String
     */
    private String getFieldValueByAlias(X2BaseBean bean, String alias) {
        String value = "";
        if (bean != null) {
            value = (String) bean.getFieldValueByAlias(alias);
        }

        return value;
    }

    /**
     * Returns the work phone number.
     *
     * @param workPhone String
     * @param workPhoneExt String
     * @return String
     */
    private String getWorkPhone(String workPhone, String workPhoneExt) {
        String phoneNumber = "";

        if (workPhoneExt != null) {
            phoneNumber = workPhone + " x" + workPhoneExt;
        } else if (workPhone != null) {
            phoneNumber = workPhone;
        }

        return phoneNumber;
    }

    /**
     * Loads a list of emergency priorities for filtering.
     */
    private List<String> loadEmergencyPriorities() {
        List<String> values = new ArrayList<String>();
        String epParam = (String) getParameter(PARAM_EMERGENCY_PRIORITY_FILTER);
        if (!StringUtils.isEmpty(epParam)) {
            for (String priority : epParam.split(DELIMITER)) {
                values.add(priority.trim());
            }
        }
        return values;
    }

    /**
     * Loads a list of Hispanic Students to a map keyed on personOid.
     */
    private void loadHispanicStudents() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Race.REL_PERSON + PATH_DELIMITER + Person.COL_HISPANIC_LATINO_INDICATOR, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(Race.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                Race race = (Race) iterator.next();
                m_hispanics.put(race.getPersonOid(), RACE_CODE_HISPANIC);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a map of Language codes keyed on code mapped to the local code (Easy IEP Language
     * Code).
     */
    private void loadLanguageCodes() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, LANGUAGE_RTB_OID);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) iterator.next();
                m_languageCodes.put(code.getCode(), code.getLocalCode());
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a map of person relationship codes keyed on code mapped to the local code (Easy IEP
     * Parent Type Code).
     */
    private void loadParentTypeCodes() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, PERSON_RELATIONSHIP_RTB_OID);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) iterator.next();
                if (!StringUtils.isEmpty(code.getLocalCode())) {
                    m_parentTypeCodes.put(code.getCode(), code.getLocalCode());
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads races to m_personRaces ensuring that the Hispanic code always comes first in the list
     * of race codes.
     */
    private void loadStudentRaces() {
        // Adds m_hispanics to m_personRaces
        m_personRaces.putAll(m_hispanics);

        QueryByCriteria query = new QueryByCriteria(Race.class);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                Race race = (Race) iterator.next();
                String personOid = race.getPersonOid();
                String races = m_personRaces.get(personOid);

                if (races == null) {
                    races = race.getRaceCode();
                } else {
                    races += ", " + race.getRaceCode();
                }

                m_personRaces.put(personOid, races);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    public String translateAliasToJavaName(String alias) {
        String javaName = null;
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        if (dictionary != null) {
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
            if (field != null) {
                javaName = field.getJavaName();
            }
        }
        return javaName;
    }

}
