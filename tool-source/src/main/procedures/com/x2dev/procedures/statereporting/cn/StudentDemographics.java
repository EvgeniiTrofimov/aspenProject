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
package com.x2dev.procedures.statereporting.cn;

import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;

/**
 * This class is used by a district in China for Student Demographics.
 * 
 * @author Follett Software Company
 *
 */
public class StudentDemographics extends StateReportData {
    /**
     * This class has the entity for the Student Demographics to be used by the district in China.
     * 
     * @author Follett Software Company
     *
     */

    public static class StudentDemographicsEntity extends StateReportEntity {

        /**
         * This is the public no argument default constructor for dynamic instantiation.
         */
        public StudentDemographicsEntity() {
            // Default constructor
        }

        /**
         * This is the initialize method. This method loads the student record for a particular
         * student.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public SisStudent getStudent() {
            return (SisStudent) getBean();
        }
    }

    /**
     * This retriever class is used to get the address of the student.
     * 
     * @author Follett Software Company
     *
     */
    public class AddressRetriever implements FieldRetriever {
        private static final String COMMA_SEPARATOR = ",";

        /**
         * This method retrieves the address of the student.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Object address = null;
            SisStudent student = ((StudentDemographicsEntity) entity).getStudent();
            Address physicalAddress = student.getPerson().getPhysicalAddress();
            address = new StringBuilder(physicalAddress.getAddressLine01())
                    .append(COMMA_SEPARATOR)
                    .append(physicalAddress.getAddressLine02())
                    .append(COMMA_SEPARATOR)
                    .append(physicalAddress.getAddressLine03());
            return address;
        }
    }

    /**
     * This retriever is used to get the details of the emergency contact.
     * 
     * @author Follett Software Company
     *
     */
    public class ContactRetriever implements FieldRetriever {
        /**
         * 
         * Some Constants for emergency contact's employer, mobile phone number, name, nationality,
         * primary email,
         * and relationship with the student.
         * 
         */
        private static final String PARAM_CONTACT_EMPLOYER = "CONTACT_EMPLOYER";

        private static final String PARAM_CONTACT_MOBILE_PHONE = "CONTACT_MOBILE_PHONE";

        private static final String PARAM_CONTACT_NAME = "CONTACT_NAME";

        private static final String PARAM_CONTACT_NATIONALITY = "CONTACT_NATIONALITY";

        private static final String PARAM_CONTACT_PRIMARY_EMAIL = "CONTACT_PRIMARY_EMAIL";

        private static final String PARAM_CONTACT_RELATIONSHIP = "CONTACT_RELATIONSHIP";

        /**
         * This method retrieves emergency contact's employer, mobile phone number, name,
         * nationality, primary email,
         * and relationship with the student.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            SisStudent student = ((StudentDemographicsEntity) entity).getStudent();
            StudentContact emergencyContact = null;
            if (student != null) {
                Collection<StudentContact> contacts = student.getContacts();
                if (contacts != null && !contacts.isEmpty()) {
                    for (StudentContact contact : contacts) {
                        if (emergencyContact == null) {
                            emergencyContact = contact;
                        }
                        if (emergencyContact != null
                                && (contact.getEmergencyPriority() < emergencyContact.getEmergencyPriority())) {
                            emergencyContact = contact;
                        }
                    }
                }
            }
            String param = (String) field.getParameter();
            if (param != null && PARAM_CONTACT_EMPLOYER.equals(param.trim())) {
                value = emergencyContact.getContact().getFieldValueByBeanPath(m_aliasEmergencyContactEmployer);
            } else if (param != null && PARAM_CONTACT_MOBILE_PHONE.equals(param.trim())) {
                value = emergencyContact.getPerson().getPhone01();
            } else if (param != null && PARAM_CONTACT_NAME.equals(param.trim())) {
                value = emergencyContact.getPerson().getNameView();
            } else if (param != null && PARAM_CONTACT_NATIONALITY.equals(param.trim())) {
                value = emergencyContact.getFieldValueByBeanPath(m_aliasEmergencyContactNationality);
            } else if (param != null && PARAM_CONTACT_PRIMARY_EMAIL.equals(param.trim())) {
                value = emergencyContact.getPerson().getEmail01();
            } else if (param != null && PARAM_CONTACT_RELATIONSHIP.equals(param.trim())) {
                value = emergencyContact.getRelationshipCode();
            }
            return value;
        }
    }

    /**
     * Alias for the emergency contact's employer.
     */
    private static final String ALIAS_CONTACT_EMPLOYER = "DOE EMPLOYER";

    /**
     * Alias for the emergency contact's nationality.
     */
    private static final String ALIAS_CONTACT_NATIONALITY = "DOE NATIONALITY";

    /**
     * This field holds the java name for the alias of emergency contact's employer.
     */
    protected String m_aliasEmergencyContactEmployer;

    /**
     * This field holds the java name for the alias of emergency contact's nationality.
     */
    protected String m_aliasEmergencyContactNationality;

    /**
     * This is the end of school date for the district. It is retrieved using the current context of
     * the
     * current organization.
     */
    protected PlainDate m_districtSchoolEndDate;

    /**
     * StudentHistoryHelper class instance.
     */
    protected StudentHistoryHelper m_helper;

    /**
     * This method is the main initialization method for the entire export.
     *
     * @throws X2BaseException exception
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_districtSchoolEndDate = getCurrentContext().getEndDate();
        m_aliasEmergencyContactEmployer = translateAliasToJavaName(ALIAS_CONTACT_EMPLOYER, true);
        m_aliasEmergencyContactNationality = translateAliasToJavaName(ALIAS_CONTACT_NATIONALITY, true);

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, getCurrentContext().getStartDate());
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_districtSchoolEndDate);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(StudentDemographicsEntity.class);

            // Build a map of calculations
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STD-CONTACT-RTRVE", new ContactRetriever());
            calcs.put("STD-ADDRESS-RTRVE", new AddressRetriever());
            super.addCalcs(calcs);
        }
    }
}
