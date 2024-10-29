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
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;

/**
 * The Class DfEManager.
 */
public class DfEManager {
    public static final String DEFAULT_ORGANIZATION = "*dst";
    public static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
    public static final String REPORT_TYPE_ATF = "ATF";
    public static final String REPORT_TYPE_CTF = "CTF";

    public static final String PARAM_STUDENT_OIDS = "student-oids";
    public static final String PARAM_DESTINATION_LEA = "dest-lea";
    public static final String PARAM_DESTINATION_ESTAB = "dest-estab";

    public static final String PARAM_CORE = "core";
    public static final String PARAM_BASIC_DETAILS = "basic-details";
    public static final String PARAM_LOOKED_AFTER = "looked-after";
    public static final String PARAM_SEN_HISTORY = "sen-history";
    public static final String PARAM_ADMISSIONS = "admissions";
    public static final String PARAM_ADDRESS_PHONE_EMAIL = "address-phone-email";
    public static final String PARAM_CONTACTS = "contacts";
    public static final String PARAM_ATTENDANCE = "attendance";
    public static final String PARAM_ATTENDANCE_SESSIONS = "attendance-sessions";
    public static final String PARAM_ASSESSMENTS = "assessments";
    public static final String PARAM_SCHOOL_HISTORY = "school-history";
    public static final String PARAM_NAW_DETAILS = "naw-details";
    public static final String PARAM_FSM_HISTORY = "fsm-history";

    public static final String PARAM_UPDATE_EXISTING = "update-existing";

    public static final String STRING_TRUE = "true";
    public static final String STRING_FALSE = "false";
    public static final String STRING_EMPTY = "";
    public static final String DB_TRUE = "1";
    public static final String DB_FALSE = "0";
    public final Boolean BOOLEAN_TRUE = Boolean.valueOf(true);
    public final Boolean BOOLEAN_FALSE = Boolean.valueOf(false);
    public static final int DB_FIELD_A_MAX_LENGTH = 10;
    public static final int DB_FIELD_B_MAX_LENGTH = 25;
    public static final int DB_FIELD_C_MAX_LENGTH = 50;
    public static final int STRING_10_MAX_LENGTH = 10;
    public static final int STRING_20_MAX_LENGTH = 20;
    public static final int STRING_32_MAX_LENGTH = 32;
    public static final int STRING_40_MAX_LENGTH = 40;
    public static final int STRING_50_MAX_LENGTH = 50;
    public static final int STRING_100_MAX_LENGTH = 100;

    public static final String DFE_DATA_TIME_DELIMITER = "T";
    public static final String SCHOOL_ID_CTF_SCHOOL = "CTF School";
    public static final String ASSESSMENT_DEFINITION_SAT = "SAT";

    public static final String ALIAS_NAME_LEA = "DFE LEA ID";
    public static final String ALIAS_NAME_ESTAB = "DFE ESTABLISHMENT ID";

    public static final String ALIAS_NAME_APPLICATION_REFERENCE = "DFE APPLICATION REF";
    public static final String ALIAS_NAME_UPN = "DFE UPN";
    public static final String ALIAS_NAME_FORMER_UPN = "DFE FORMER UPN";
    public static final String ALIAS_NAME_PREFERRED_SURNAME = "DFE PREFERRED SURNAME";
    public static final String ALIAS_NAME_PREFERRED_FORENAME = "DFE PREFERRED FORENAME";
    public static final String ALIAS_NAME_FORMER_SURNAME = "DFE FORMER SURNAME";
    public static final String ALIAS_NAME_FORMER_FORENAME = "DFE FORMER FORENAME";
    public static final String ALIAS_NAME_ETHNICITY = "DFE ETHNICITY";
    public static final String ALIAS_NAME_ETHNICITY_SOURCE = "DFE ETHNICITY SOURCE";
    public static final String ALIAS_NAME_MEDICAL_FLAG = "DFE MEDICAL FLAG";
    public static final String ALIAS_NAME_ORDER = "DFE ORDER";
    public static final String ALIAS_NAME_RESPONSIBLE = "DFE RESPONSIBLE";
    public static final String ALIAS_NAME_ULN = "DFE ULN";
    public static final String ALIAS_NAME_UCI = "DFE UCI";
    public static final String ALIAS_NAME_IN_CARE = "DFE IN CARE";
    public static final String ALIAS_NAME_CARE_AUTHORITY = "DFE CARE AUTHORITY";
    public static final String ALIAS_NAME_TELEPHONE_TYPE_1 = "DFE TELEPHONE TYPE 1";
    public static final String ALIAS_NAME_TELEPHONE_TYPE_2 = "DFE TELEPHONE TYPE 2";
    public static final String ALIAS_NAME_TELEPHONE_TYPE_3 = "DFE TELEPHONE TYPE 3";
    public static final String ALIAS_NAME_FSM_REVIEW_DATE = "DFE FSM REVIEW DATE";
    public static final String ALIAS_NAME_FSM_UK_COUNTRY = "DFE FSM UK COUNTRY";
    public static final String ALIAS_NAME_SEN_PUPIL_PROVISION = "DFE PUPIL SEN PROVISION";
    public static final String ALIAS_NAME_SEN_PROVISION = "DFE SEN PROVISION";
    public static final String ALIAS_NAME_SEN_TYPE = "DFE SEN TYPE";
    public static final String ALIAS_NAME_SEN_RANK = "DFE SEN RANK";
    public static final String ALIAS_NAME_SPEAK_WELSH = "DFE SPEAK WELSH";
    public static final String ALIAS_NAME_HOME_WELSH = "DFE HOME WELSH";
    public static final String ALIAS_NAME_NATIONAL_IDENTITY = "DFE NATIONAL IDENTITY";
    public static final String ALIAS_NAME_WELSH_SOURCE = "DFE WELSH SOURCE";
    public static final String ALIAS_NAME_EAL_ACQUISITION = "DFE EAL ACQUISITION";
    public static final String ALIAS_NAME_LANGUAGE_SOURCE = "DFE LANGUAGE SOURCE";
    public static final String ALIAS_NAME_SEN_CURR_TEACH_METHOD = "DFE SEN CURR TEACH METHOD";
    public static final String ALIAS_NAME_SEN_GROUPING_SUPPORT = "DFE SEN GROUPING SUPPORT";
    public static final String ALIAS_NAME_SEN_SPEC_RESOURCES = "DFE SEN SPEC RESOURCES";
    public static final String ALIAS_NAME_SEN_ADVICE_ASSESSMENT = "DFE SEN ADVICE ASSESSMENT";

    public static final String ALIAS_NAME_SAON = "DFE ADDRESS SAON";
    public static final String ALIAS_NAME_LOCALITY = "DFE ADDRESS LOCALITY";
    public static final String ALIAS_NAME_ADMIN_AREA = "DFE ADDRESS ADMIN AREA";
    public static final String ALIAS_NAME_POST_TOWN = "DFE ADDRESS POST TOWN";
    public static final String ALIAS_NAME_UNIQUE_PROP_REF_NUM = "DFE UNIQUE PROP REF NUM";
    public static final String ALIAS_NAME_EASTING = "DFE ADDRESS EASTING";
    public static final String ALIAS_NAME_NORTHING = "DFE ADDRESS NORTHING";
    public static final String ALIAS_NAME_ADDRESS_LINE_4 = "DFE ADDRESS LINE 4";
    public static final String ALIAS_NAME_ADDRESS_LINE_5 = "DFE ADDRESS LINE 5";

    public static final String ALIAS_NAME_ATTENDANCE_XML = "DFE ATTENDANCE XML";
    public static final String ALIAS_NAME_ASSESSMENTS_XML = "DFE ASSESSMENTS XML";
    public static final String ALIAS_NAME_SCHOOL_HISTORY_XML = "DFE SCHOOL HISTORY XML";

    public static final String ALIAS_NAME_ATTEND_YEAR = "DFE ATTEND YEAR";
    public static final String ALIAS_NAME_ATTEND_LEA = "DFE ATTEND LEA";
    public static final String ALIAS_NAME_ATTEND_ESTAB = "DFE ATTEND ESTAB";
    public static final String ALIAS_NAME_ATTEND_SCHOOL_NAME = "DFE ATTEND SCHOOL NAME";
    public static final String ALIAS_NAME_ATTEND_SESS_POSSIBLE = "DFE ATTEND SESS POSSIBLE";
    public static final String ALIAS_NAME_ATTEND_SESS_AUTHORIZED = "DFE ATTEND SESS AUTHORIZED";
    public static final String ALIAS_NAME_ATTEND_SESS_ATTENDED = "DFE ATTEND SESS ATTENDED";
    public static final String ALIAS_NAME_ATTEND_SESS_UNAUTHORIZED = "DFE ATTEND SESS UNAUTHORIZED";
    public static final String ALIAS_NAME_ATTEND_START_DATE = "DFE ATTEND START DATE";
    public static final String ALIAS_NAME_ATTEND_MARKS = "DFE ATTEND MARKS";

    public static final String ALIAS_NAME_STAGE = "DFE ASSESS STAGE";
    public static final String ALIAS_NAME_LOCALE = "DFE ASSESS LOCALE";
    public static final String ALIAS_NAME_YEAR_TAKEN = "DFE ASSESS YEAR TAKEN";
    public static final String ALIAS_NAME_SUBJECT = "DFE ASSESS SUBJECT";
    public static final String ALIAS_NAME_METHOD = "DFE ASSESS METHOD";
    public static final String ALIAS_NAME_COMPONENT = "DFE ASSESS COMPONENT";
    public static final String ALIAS_NAME_RESULT_STATUS = "DFE ASSESS RESULT STATUS";
    public static final String ALIAS_NAME_RESULT_QUALIFIER = "DFE ASSESS RESULT QUALIFIER";
    public static final String ALIAS_NAME_RESULT = "DFE ASSESS RESULT";
    public static final String ALIAS_NAME_RESULT_DATE = "DFE ASSESS RESULT DATE";

    public static final String ALIAS_NAME_OUT_OF_LEA_ID = "DFE OUT OF LEA ID";
    public static final String ALIAS_NAME_OUT_OF_LEA_ESTAB = "DFE OUT OF LEA ESTAB";
    public static final String ALIAS_NAME_OUT_OF_LEA_ESTAB_NAME = "DFE OUT OF LEA ESTAB NAME";

    /**
     * A local copy of the data dictionary for use by various lookup utilities.
     */
    private DataDictionary m_dictionary;

    /**
     * A local copy of the X2 Broker.
     */
    protected X2Broker m_broker = null;

    /**
     * A local copy of the locale.
     */
    protected Locale m_locale = null;

    /**
     * Default SAT Assessment Definition.
     */
    protected AssessmentDefinition m_sATAssessmentDefinition = null;

    /**
     * Default CTF School.
     */
    protected School m_cTFSchool = null;

    /**
     * Date and Time formats.
     */
    public SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");
    public SimpleDateFormat m_timeFormat = new SimpleDateFormat("hh:mm:ss");

    /**
     * A map of maps of reference code.
     * The outer map is indexed by the reference table OID. It contains maps of reference codes.
     * The inner map is indexed by the reference code. It contains the RefrenceCode bean for the
     * code.
     */
    private Map<String, Map<String, ReferenceCode>> m_refTableMap = null;

    /**
     * Constructor for DfEManager.
     *
     * @param broker X2Broker
     * @param locale Locale
     */
    public DfEManager(X2Broker broker, Locale locale) {
        m_broker = broker;
        m_locale = locale;

        m_cTFSchool = getCTFSchool();
        m_sATAssessmentDefinition = getSATAssessmentDefinition();
    }

    /**
     * Convert a Java File to byte Array.
     *
     * @param file File
     * @return byte[]
     * @throws IOException Signals that an I/O exception has occurred.
     */
    public byte[] getBytesFromFile(File file) throws IOException {
        // Get the size of the file
        long length = file.length();

        // You cannot create an array using a long type.
        // It needs to be an int type.
        // Before converting to an int type, check
        // to ensure that file is not larger than Integer.MAX_VALUE.
        if (length > Integer.MAX_VALUE) {
            // File is too large
        }

        // Create the byte array to hold the data
        byte[] bytes = new byte[(int) length];

        InputStream is = new FileInputStream(file);
        try {
            // Read in the bytes
            int offset = 0;
            int numRead = 0;
            while (offset < bytes.length && (numRead = is.read(bytes, offset, bytes.length - offset)) >= 0) {
                offset += numRead;
            }

            // Ensure all the bytes have been read in
            if (offset < bytes.length) {
                throw new IOException("Could not completely read file " + file.getName());
            }
        } finally {
            // Close the input stream and return bytes
            is.close();
        }

        return bytes;
    }

    /**
     * Get the default CTF School
     *
     * This is used for imported out-of-LEA SchoolHistory and Student Attendance.
     *
     * @return School
     */
    public School getCTFSchool() {
        School cTFSchool = null;

        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(School.COL_SCHOOL_ID, SCHOOL_ID_CTF_SCHOOL);
        BeanQuery schoolQuery = new BeanQuery(School.class, schoolCriteria);
        Collection<School> schools = m_broker.getCollectionByQuery(schoolQuery);

        if (schools.size() > 0) {
            for (School school : schools) {
                cTFSchool = school;
                break;
            }
        } else {
            // If not found create it
            cTFSchool = X2BaseBean.newInstance(School.class, m_broker.getPersistenceKey());
            cTFSchool.setOrganization1Oid(DEFAULT_ORGANIZATION);
            cTFSchool.setName(SCHOOL_ID_CTF_SCHOOL);
            cTFSchool.setSchoolId(SCHOOL_ID_CTF_SCHOOL);
            cTFSchool.setInactiveIndicator(true);

            m_broker.saveBeanForced(cTFSchool);
        }

        return cTFSchool;
    }

    /**
     * Returns a local instance of a district data dictionary.
     *
     * @return DataDictionary.
     */
    public DataDictionary getDataDictionary() {
        if (m_dictionary == null) {
            m_dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
        }
        return m_dictionary;
    }

    /**
     * Lookup and return a DataDictionaryField based on a root bean and bean path.
     * This allows multi-hop paths in the bean path.
     *
     *
     * @param beanClass Class
     * @param path String
     * @return DataDictionaryField
     */
    public DataDictionaryField getDataDictionaryField(Class beanClass, String path) {
        ModelProperty prop = new ModelProperty(beanClass, path, m_broker.getPersistenceKey());
        DataDictionaryField dictionaryField = getDataDictionary().findDataDictionaryField(prop.getFieldId());
        return dictionaryField;
    }

    /**
     * Lookup and return a DataDictionaryField based on a root bean and bean path.
     * This allows multi-hop paths in the bean path.
     *
     *
     * @param bean X2BaseBean
     * @param path String
     * @return DataDictionaryField
     */
    public DataDictionaryField getDataDictionaryField(X2BaseBean bean, String path) {
        ModelProperty prop = new ModelProperty(bean.getClass(), path, m_broker.getPersistenceKey());
        DataDictionaryField dictionaryField = getDataDictionary().findDataDictionaryField(prop.getFieldId());
        return dictionaryField;
    }

    /**
     * Get a UK Establishment by UK School .
     *
     * @param school School
     * @return String
     */
    public String getEstabBySchool(School school) {
        String estab = null;

        if (school != null) {
            estab = (String) school.getFieldValueByAlias(ALIAS_NAME_ESTAB, getDataDictionary());
        }

        return estab;
    }

    /**
     * Get a UK LEA by UK School .
     *
     * @param school School
     * @return String
     */
    public String getLEABySchool(School school) {
        String lEA = null;

        Organization organization = school.getOrganization1();
        if (organization != null) {
            lEA = (String) organization.getFieldValueByAlias(ALIAS_NAME_LEA, getDataDictionary());
        }

        return lEA;
    }

    /**
     * Get the District's Organization by UK LEA .
     *
     * @param lEA String
     * @return Organization
     */
    public Organization getOrganizationByLea(String lEA) {
        Organization existingOrganization = null;

        String lEAAliasName = translateAliasToJavaName(ALIAS_NAME_LEA, true);

        // If not found error DFE LEA ID not properly defined.
        if (lEAAliasName == null) {
            String message = "The server's Data Dictionary was not configurated with the proper field Aliases!";
            AppGlobals.getLog().log(Level.SEVERE, message);
        } else {
            X2Criteria organizationCriteria = new X2Criteria();
            organizationCriteria.addEqualTo(lEAAliasName, lEA);

            BeanQuery organizationQuery = new BeanQuery(Organization.class, organizationCriteria);
            Collection<Organization> organizations = m_broker.getCollectionByQuery(organizationQuery);

            if (organizations.size() > 0) {
                for (Organization organization : organizations) {
                    existingOrganization = organization;
                    break;
                }
            }
        }

        return existingOrganization;
    }

    /**
     * Lookup a map of reference codes for a reference table oid.
     * Cache the results for later use.
     *
     * @param referenceTableOid String
     * @return Map<String, ReferenceCode>
     */
    public Map<String, ReferenceCode> getReferenceCodes(String referenceTableOid) {
        Map<String, ReferenceCode> codeMap = null;
        if (m_refTableMap == null) {
            m_refTableMap = new HashMap<String, Map<String, ReferenceCode>>();
        }

        if (m_refTableMap.containsKey(referenceTableOid)) {
            codeMap = m_refTableMap.get(referenceTableOid);
        } else {
            codeMap = new HashMap<String, ReferenceCode>();
            ReferenceTable refTable = (ReferenceTable) m_broker.getBeanByOid(ReferenceTable.class, referenceTableOid);
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes(m_broker);
                for (ReferenceCode code : codes) {
                    codeMap.put(code.getCode(), code);
                }
            }
            m_refTableMap.put(referenceTableOid, codeMap);
        }
        return codeMap;
    }

    /**
     * Get a UK SAT Assessment Definition .
     *
     * @return AssessmentDefinition
     */
    public AssessmentDefinition getSATAssessmentDefinition() {
        AssessmentDefinition assessmentDefinition = null;

        X2Criteria assessmentDefinitionCriteria = new X2Criteria();
        assessmentDefinitionCriteria.addEqualTo(AssessmentDefinition.COL_ID, ASSESSMENT_DEFINITION_SAT);

        BeanQuery assessmentDefinitionQuery = new BeanQuery(AssessmentDefinition.class, assessmentDefinitionCriteria);
        assessmentDefinition = (AssessmentDefinition) m_broker.getBeanByQuery(assessmentDefinitionQuery);

        // If it doesn't exist then produce an error message.
        if (assessmentDefinition == null || assessmentDefinition.getAssessmentColumnDefinitions().size() == 0) {
            String message = "ERROR CTF Import: SAT Assessment Definition is not configured.";
            AppGlobals.getLog().log(Level.WARNING, message);
        }

        return assessmentDefinition;
    }

    /**
     * Get a UK School by UK Estab .
     *
     * @param estab String
     * @return School
     */
    public School getSchoolByEstab(String estab) {
        School existingSchool = null;

        String estabAliasName = translateAliasToJavaName(ALIAS_NAME_ESTAB, true);

        // If not found error DFE Estab ID not properly defined.
        if (estabAliasName == null) {
            String message = "The server's Data Dictionary was not configurated with the proper field Aliases!";
            AppGlobals.getLog().log(Level.SEVERE, message);
        } else {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addEqualTo(estabAliasName, estab);

            BeanQuery schoolQuery = new BeanQuery(School.class, schoolCriteria);
            Collection<School> schools = m_broker.getCollectionByQuery(schoolQuery);

            if (schools.size() > 0) {
                for (School school : schools) {
                    existingSchool = school;
                    break;
                }
            }
        }

        return existingSchool;
    }

    /**
     * Get the Student by their UPN (Unique Pupil Number)
     *
     * A student existence is checked by searching for their UPN.
     *
     * @param dfEPupil DfEPupil
     * @return Student
     */
    public Student getStudentByUPN(DfEPupil dfEPupil) {
        Student existingStudent = null;

        String studentUPNAliasName = translateAliasToJavaName(ALIAS_NAME_UPN, true);

        // If not found error DFE UPN not properly defined in Data Dictionary.
        if (studentUPNAliasName == null) {
            String message = "The server's Data Dictionary was not configurated with the proper field Aliases!";
            AppGlobals.getLog().log(Level.SEVERE, message);
        } else {
            // Check Student UPN [DFE UPN] and Former UPN [DFE FORMER UPN]
            X2Criteria studentCriteria = new X2Criteria();
            studentCriteria.addEqualTo(studentUPNAliasName, dfEPupil.getUniquePupilNumber());

            BeanQuery studentQuery = new BeanQuery(Student.class, studentCriteria);
            Collection<Student> students = m_broker.getCollectionByQuery(studentQuery);

            if (!students.isEmpty()) {
                for (Student student : students) {
                    existingStudent = student;
                    break;
                }
            }
        }

        return existingStudent;
    }

    /**
     * Check is a Section is Active .
     *
     * @param activeSections HashMap<String,Boolean>
     * @param sectionName String
     * @return boolean
     */
    public boolean isSectionActive(HashMap<String, Boolean> activeSections, String sectionName) {
        boolean isActive = false;
        if (activeSections.get(sectionName) != null && activeSections.get(sectionName).booleanValue()) {
            isActive = true;
        }

        return isActive;
    }

    /**
     * Returns the state lookup code for field value.
     * Look up based on bean path.
     *
     * @param beanClass - the class of the bean to find the reference table.
     * @param beanPath - the bean path of the bean to find the reference table.
     * @param value - the value to lookup and translate in the lookup table.
     * @param referenceMap - the reference map type
     *        (ExportFormatField.ReferenceMapTypeCode.*.ordinal()) of the lookup.
     *
     * @return String - state code for input value.
     */
    public String lookupReferenceCodeByBeanPath(Class beanClass, String beanPath, String value, int referenceMap) {
        String stateValue = null;
        DataDictionaryField dictionaryField = getDataDictionaryField(beanClass, beanPath);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            stateValue = lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value, referenceMap);
        }
        return stateValue;
    }

    /**
     * Returns the lookup code value for field value.
     * Look up based on the reference table.
     *
     * @param referenceTableOid - the reference table OID of the reference table.
     * @param value - the value to lookup and translate in the lookup table.
     * @param referenceMap - the reference map type
     *        (ExportFormatField.ReferenceMapTypeCode.*.ordinal()) of the lookup.
     *
     * @return String - reference code lookup value for input value.
     */
    public String lookupReferenceCodeByRefTbl(String referenceTableOid, String value, int referenceMap) {
        String returnValue = null;
        Map<String, ReferenceCode> refCodes = getReferenceCodes(referenceTableOid);
        ReferenceCode code = refCodes.get(value);
        if (code != null) {
            if (referenceMap == ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()) {
                returnValue = code.getStateCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.FEDERAL.ordinal()) {
                returnValue = code.getFederalCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal()) {
                returnValue = code.getLocalCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.SYSTEM.ordinal()) {
                returnValue = code.getSystemCode();
            }
        }

        return returnValue;
    }

    /**
     * Returns the lookup code value for Local Code.
     * Look up based on the reference table.
     *
     * @param beanClass - the class of the bean to find the reference table.
     * @param beanPath - the bean path of the bean to find the reference table.
     * @param lookupLocalCode - the value to lookup and translate in the lookup table.
     *
     * @return String - reference code lookup value for local code.
     */
    public String lookupRefCodeByLocalCode(Class beanClass, String beanPath, String lookupLocalCode) {
        String refCode = null;
        Map<String, ReferenceCode> m_referenceCodeMap = new HashMap<String, ReferenceCode>();

        DataDictionaryField dataDictionaryField = getDataDictionaryField(beanClass, beanPath);
        ReferenceTable referenceTable = dataDictionaryField.getReferenceTable();
        m_referenceCodeMap = referenceTable.getCodeMap(m_broker);
        for (ReferenceCode referenceCode : m_referenceCodeMap.values()) {
            String localCode = referenceCode.getLocalCode();
            if (localCode != null && localCode.equals(lookupLocalCode)) {
                refCode = referenceCode.getCode();
                return refCode; // break
            }
        }

        return refCode;
    }

    /**
     * Returns the lookup code value for Local Code Alias.
     * Look up based on the reference table.
     *
     * @param beanClass - the class of the bean to find the reference table.
     * @param beanPath - the bean path of the bean to find the reference table.
     * @param lookupLocalCode - the value to lookup and translate in the lookup table.
     *
     * @return String - reference code lookup value for local code.
     */
    public String lookupRefCodeByLocalCodeAlias(Class beanClass, String beanPath, String lookupLocalCode) {
        String refCode = null;
        Map<String, ReferenceCode> m_referenceCodeMap = new HashMap<String, ReferenceCode>();
        DataDictionaryField dataDictionaryField = getDataDictionaryField(beanClass, beanPath);
        ReferenceTable referenceTable = dataDictionaryField.getReferenceTable();
        m_referenceCodeMap = referenceTable.getCodeMap(m_broker);
        for (ReferenceCode referenceCode : m_referenceCodeMap.values()) {
            String localCode = referenceCode.getLocalCode();
            if (localCode != null && localCode.equals(lookupLocalCode)) {
                refCode = referenceCode.getCode();
                return refCode; // break
            }
        }

        return refCode;
    }

    /**
     * Returns the lookup code value for State Code.
     * Look up based on the reference table.
     *
     * @param beanClass - the class of the bean to find the reference table.
     * @param beanPath - the bean path of the bean to find the reference table.
     * @param lookupStateCode - the value to lookup and translate in the lookup table.
     *
     * @return String - reference code lookup value for state code.
     */
    public String lookupRefCodeByStateCode(Class beanClass, String beanPath, String lookupStateCode) {
        String refCode = null;
        Map<String, ReferenceCode> m_referenceCodeMap = new HashMap<String, ReferenceCode>();

        DataDictionaryField dataDictionaryField = getDataDictionaryField(beanClass, beanPath);
        ReferenceTable referenceTable = dataDictionaryField.getReferenceTable();
        m_referenceCodeMap = referenceTable.getCodeMap(m_broker);
        for (ReferenceCode referenceCode : m_referenceCodeMap.values()) {
            String stateCode = referenceCode.getStateCode();
            if (stateCode != null && stateCode.equals(lookupStateCode)) {
                refCode = referenceCode.getCode();
                return refCode; // break
            }
        }

        return refCode;
    }

    /**
     * Returns the state lookup code for field value.
     * Look up based on bean path.
     *
     * @param beanClass - the class of the bean to find the reference table.
     * @param beanPath - the bean path of the bean to find the reference table.
     * @param refCode - the refCode to lookup and translate in the lookup table.
     *
     * @return String - state code for input refCode.
     */
    public String lookupStateValueByRefCode(Class beanClass, String beanPath, String refCode) {
        String stateValue = lookupReferenceCodeByBeanPath(beanClass, beanPath, refCode,
                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
        return stateValue;
    }

    /**
     * Set the data dictionary to be used on this export.
     * If not set, a district data dictionary will be used.
     *
     * @param dictionary void
     */
    public void setDataDictionary(DataDictionary dictionary) {
        m_dictionary = dictionary;
    }

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    public String translateAliasToJavaName(String alias, boolean required) {
        String javaName = null;

        DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        } else if (required) {
            String message = LocalizationCache.getMessages(m_broker.getPersistenceKey(), m_locale)
                    .getMessage(ERROR_ALIAS_LOOKUP);
            AppGlobals.getLog().log(Level.SEVERE, message);
        }

        return javaName;
    }

}
