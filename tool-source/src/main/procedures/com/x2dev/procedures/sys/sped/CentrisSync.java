/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.BeanRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.XMLStateReportData;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * State report data module for the Centris Sync export. Centris Sync is an XML-based export that is
 * used
 * to send SIS data to the Centris IEP Direct special education system. The XML produced is defined
 * by the
 * CentrisSyncTemplate.xml template file.
 * <p>
 * This procedure populates two collections used to generate the XML:
 * <ul>
 * <li>StudentRecords: all active students with a non-empty local ID
 * <li>StudentContacts: a grouped collection containing the StudentContacts for each student in the
 * StudentRecords
 * collection, sorted by emergency priority
 * </ul>
 * Three custom calculators are also defined:
 * <ul>
 * <li>centris-GradeLevel: returns a student's numeric grade level
 * <li>centris-ContactPriority: returns the student's emergency priority, prefixed with the letter
 * 'E' as required
 * by the XML spec
 * <li>centris-ContactRelationship: returns the local code equivalent of the contact relationship
 * code
 * </ul>
 *
 * @author mmastrangelo
 */
public class CentrisSync extends XMLStateReportData {
    protected HashMap<String, Integer> m_numericGradeLevelMap;
    protected Map<String, ReferenceCode> m_relationshipCodeMap;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_numericGradeLevelMap = StudentManager.buildNumericGradeLevelMap(getBroker());

        loadCollections();
        addCustomCalcs();
    }

    /**
     * Loads the StudentRecords and StudentContacts collections for use in generating the XML.
     */
    protected void loadCollections() {
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(Student.COL_LOCAL_ID, getBroker().getPersistenceKey());

        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
        BeanQuery beanQuery = new BeanQuery(Student.class, criteria);

        setCollectionToQuery("StudentRecords", getBroker().getCollectionByQuery(beanQuery));

        X2Criteria contactCriteria = new X2Criteria();
        contactCriteria.addIn(StudentContact.COL_STUDENT_OID,
                new SubQuery(Student.class, X2BaseBean.COL_OID, criteria));

        BeanQuery contactQuery = new BeanQuery(StudentContact.class, contactCriteria);
        contactQuery.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);

        setGroupedCollectionToQuery("StudentContacts",
                getBroker().getGroupedCollectionByQuery(contactQuery, StudentContact.COL_STUDENT_OID, 512));

        X2Criteria raceCriteria = new X2Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, new SubQuery(Student.class, Student.COL_PERSON_OID, criteria));

        BeanQuery raceQuery = new BeanQuery(Race.class, raceCriteria);

        Map<String, Collection<Race>> raceMap =
                getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 512);

        BeanQuery studentQuery = new BeanQuery(Student.class, criteria);
        Map<String, Student> studentMap = getBroker().getMapByQuery(studentQuery, Student.COL_PERSON_OID, 1024);

        Map<String, Collection<Race>> raceMapByStudent = new HashMap<>(1024);
        for (String personOid : raceMap.keySet()) {
            Collection<Race> racesForPerson = raceMap.get(personOid);
            Student student = studentMap.get(personOid);

            if (student != null) {
                raceMapByStudent.put(student.getOid(), racesForPerson);
            }
        }
        setGroupedCollectionToQuery("Races", raceMapByStudent);
    }

    /**
     * Add custom field retrievers.
     */
    private void addCustomCalcs() {
        HashMap<String, FieldRetriever> calcRetrievers = new HashMap<String, FieldRetriever>();

        DataDictionaryField relationshipField = getDataDictionary()
                .findDataDictionaryField(StudentContact.class.getName(), StudentContact.COL_RELATIONSHIP_CODE);
        m_relationshipCodeMap = relationshipField.getReferenceTable().getCodeMap(getBroker());

        // calcRetrievers.put("centris-GradeLevel", new GradeLevelRetriever()); // Use a
        // StateCodeLookupRetriever
        calcRetrievers.put("centris-GradeLevel", new LocalCodeLookupRetriever(
                new ModelProperty(Student.class, Student.COL_GRADE_LEVEL, getBroker().getPersistenceKey())));
        calcRetrievers.put("centris-ContactPriority", new ContactPriorityRetriever());
        calcRetrievers.put("centris-ContactRelationship", new RelationshipCodeRetriever());
        calcRetrievers.put("centris-GenderCode", new GenderRetriever());
        calcRetrievers.put("centris-StateCode",
                new LocalCodeLookupRetriever(new ModelProperty(Student.class,
                        Student.REL_PERSON + "." + Person.REL_PHYSICAL_ADDRESS + "." + Address.COL_STATE,
                        getBroker().getPersistenceKey()))); // Use a StateCodeLookupRetriever
        calcRetrievers.put("centris-ContactStateCode",
                new LocalCodeLookupRetriever(new ModelProperty(Contact.class,
                        Contact.REL_PERSON + "." + Person.REL_PHYSICAL_ADDRESS + "." + Address.COL_STATE,
                        getBroker().getPersistenceKey())));
        calcRetrievers.put("centris-RaceCode", new LocalCodeLookupRetriever(
                new ModelProperty(Race.class, Race.COL_RACE_CODE, getBroker().getPersistenceKey())));
        calcRetrievers.put("centris-HomeLanguage", new LocalCodeLookupRetriever(
                new ModelProperty(Student.class, Student.COL_HOME_LANGUAGE_CODE, getBroker().getPersistenceKey())));
        calcRetrievers.put("centris-EthnicityCode", new EthnicityCodeRetriever());
        calcRetrievers.put("centris-ContactMailingIndicator", new ContactMailingIndicatorRetriever());
        calcRetrievers.put("centris-EnglishProficiency", new EnglishProficiencyCodeRetriever());

        addCalcs(calcRetrievers);
    }

    // /**
    // * Retriever for centris-GradeLevel
    // *
    // * @author mmastrangelo
    // */
    // protected class GradeLevelRetriever extends BeanRetriever
    // {
    // /**
    // * @see
    // com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
    // com.follett.fsc.core.k12.beans.X2BaseBean,
    // com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
    // */
    // @Override
    // public Object getFieldValue(StateReportData data, X2BaseBean bean,
    // FieldDefinition field)
    // {
    // Student student = (Student) bean;
    //
    // DataDictionaryField gradeLevelField =
    // getDataDictionary().findDataDictionaryField(Student.class.getName(),
    // Student.COL_GRADE_LEVEL);
    //
    // return getDataDictionary().findStateReferenceCode(gradeLevelField.getReferenceTableOid(),
    // student.getGradeLevel());
    //// return m_numericGradeLevelMap.get(student.getGradeLevel());
    // }
    // }

    /**
     * Retriever for centris-ContactPriority.
     *
     * @author mmastrangelo
     */
    protected class ContactPriorityRetriever extends BeanRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            StudentContact contact = (StudentContact) bean;
            return "E" + contact.getEmergencyPriority();
        }
    }

    /**
     * TODO: Describe class.
     *
     * @author mmastrangelo
     */
    protected class LocalCodeLookupRetriever extends BeanRetriever {
        private Map<String, ReferenceCode> m_codeLookupMap;
        private ModelProperty m_property;

        /**
         * Instantiates a new local code lookup retriever.
         *
         * @param property ModelProperty
         */
        public LocalCodeLookupRetriever(ModelProperty property) {
            super();

            m_codeLookupMap = property.getField().getReferenceTable().getCodeMap(null, null, null, true);
            m_property = property;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            String localCodeValue = null;

            String propertyValue = (String) bean.getFieldValueByProperty(m_property);
            if (!StringUtils.isEmpty(propertyValue)) {
                ReferenceCode code = m_codeLookupMap.get(propertyValue);

                if (code != null) {
                    localCodeValue = code.getLocalCode();
                }
            }

            return localCodeValue;
        }
    }

    /**
     * The Class RelationshipCodeRetriever.
     */
    protected class RelationshipCodeRetriever extends LocalCodeLookupRetriever {

        /**
         * Instantiates a new relationship code retriever.
         */
        public RelationshipCodeRetriever() {
            super(new ModelProperty(StudentContact.class, StudentContact.COL_RELATIONSHIP_CODE,
                    getBroker().getPersistenceKey()));
        }

        /**
         * @see com.x2dev.procedures.sys.sped.CentrisSync.LocalCodeLookupRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, X2BaseBean bean, FieldDefinition field) {
            Object fieldValue = super.getFieldValue(data, bean, field);

            if (StringUtils.isEmpty((String) fieldValue)) {
                fieldValue = "21";
            }

            return fieldValue;
        }
    }

    /**
     * The Class GenderRetriever.
     *
     * @author mmastrangelo
     */
    protected class GenderRetriever extends BeanRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            Student student = (Student) bean;
            String genderCode = student.getPerson().getGenderCode();

            return ("M".equalsIgnoreCase(genderCode) || "Male".equalsIgnoreCase(genderCode)) ? "M"
                    : (("F".equalsIgnoreCase(genderCode) || "Female".equalsIgnoreCase(genderCode)) ? "F" : genderCode);
        }
    }

    /**
     * The Class EthnicityCodeRetriever.
     *
     * @author mmastrangelo
     */
    protected class EthnicityCodeRetriever extends BeanRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            String hispanicLatino = "N";

            Student student = (Student) bean;
            Person person = student.getPerson();

            if (person != null && person.getHispanicLatinoIndicator()) {
                hispanicLatino = "Y";
            }

            return hispanicLatino;
        }
    }

    /**
     * The Class ContactMailingIndicatorRetriever.
     *
     * @author mmastrangelo
     */
    protected class ContactMailingIndicatorRetriever extends BeanRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            StudentContact studentContact = (StudentContact) bean;

            return studentContact.getOtherMailingIndicator() ? "Y" : "N";
        }
    }

    /**
     * The Class EnglishProficiencyCodeRetriever.
     */
    protected class EnglishProficiencyCodeRetriever extends BeanRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            Student student = (Student) bean;

            String value = "01"; // Native English speaker

            String lep = (String) student.getFieldValueByAlias("limited-english");
            if (!StringUtils.isEmpty(lep)) {
                lep = lep.toUpperCase();
                if (lep.startsWith("Y") || "1".equalsIgnoreCase(lep)) {
                    value = "03";
                }
            }

            return value;
        }
    }
}
