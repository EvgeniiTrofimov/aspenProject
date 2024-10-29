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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class NYContact.
 */
public class NYContact extends StateReportData {
    /**
     * Entity class for Contact export.
     *
     * @author X2 Development Corporation
     */

    public static class ContactEntity extends StateReportEntity {
        /**
         * Local variables for reporting information.
         */
        protected StudentContact m_studentContact;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ContactEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_studentContact = (StudentContact) bean;
        }

        /**
         * Returns attendance code.
         *
         * @return Student contact
         */
        public StudentContact getStudentContact() {
            return m_studentContact;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            if (m_studentContact.getPerson() != null) {
                return m_studentContact.getPerson().getNameView();
            }
            return "No person for oid " + super.getEntityName() + "!";
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePhoneNumber implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String phoneNumber = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (phoneNumber != null) {
                Matcher matcher = m_illegalPhoneCharacters.matcher(phoneNumber);
                cleanValue = matcher.replaceAll("");
                if (cleanValue.length() != 10) {
                    cleanValue = "";
                } else {
                    cleanValue = cleanValue.substring(0, 3) + "-" + cleanValue.substring(3, 6) + "-"
                            + cleanValue.substring(6);
                }
            } else {
                cleanValue = "";
            }

            return cleanValue;
        }

    }


    /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAddress implements FieldRetriever {

        private String PARAM_MAIL_ADD_LINE01 = "MAIL_ADD_LINE01";
        private String PARAM_MAIL_ADD_LINE02 = "MAIL_ADD_LINE02";
        private String PARAM_MAIL_CITY = "MAIL_ADD_CITY";
        private String PARAM_MAIL_STATE = "MAIL_ADD_STATE";
        private String PARAM_MAIL_POSTAL_CODE = "MAIL_ADD_POSTAL_CODE";
        private String PARAM_MAIL_COUNTY = "MAIL_ADD_COUNTY";
        private String PARAM_MAIL_COUNTRY = "MAIL_ADD_COUNTRY";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            StudentContact student = (StudentContact) entity.getBean();
            Person person = student.getPerson();
            String parameter = (String) field.getParameter();
            Address mailingAddress = person.getMailingAddress();
            Address physicalAddress = person.getPhysicalAddress();
            Object value = null;
            if (physicalAddress != null) {
                if (PARAM_MAIL_ADD_LINE01.equals(parameter)) {
                    if (mailingAddress != null && mailingAddress.getAddressLine01() != null) {
                        value = mailingAddress.getAddressLine01();
                    } else if (physicalAddress.getAddressLine01() != null) {
                        value = physicalAddress.getAddressLine01();
                    }
                }
                if (PARAM_MAIL_ADD_LINE02.equals(parameter)) {
                    if (mailingAddress != null && mailingAddress.getAddressLine02() != null) {
                        value = mailingAddress.getAddressLine02();
                    } else if (physicalAddress.getAddressLine02() != null) {
                        value = physicalAddress.getAddressLine02();
                    }

                }
                // not displaying Address Line 3 as Contact Address line 3 was not displayed in the
                // current export.
                if (PARAM_MAIL_CITY.equals(parameter)) {
                    if (mailingAddress != null && mailingAddress.getCity() != null) {
                        value = mailingAddress.getCity();
                    } else if (physicalAddress.getCity() != null) {
                        value = physicalAddress.getCity();
                    }

                }
                if (PARAM_MAIL_COUNTRY.equals(parameter)) {
                    if (mailingAddress != null && mailingAddress.getCountry() != null) {
                        value = mailingAddress.getCountry();
                    } else if (physicalAddress.getCountry() != null) {
                        value = physicalAddress.getCountry();
                    }

                }
                if (PARAM_MAIL_STATE.equals(parameter)) {
                    if (mailingAddress != null && mailingAddress.getState() != null) {
                        value = mailingAddress.getState();
                    } else if (physicalAddress.getState() != null) {
                        value = physicalAddress.getState();
                    }
                }
                if (PARAM_MAIL_POSTAL_CODE.equals(parameter)) {
                    if (mailingAddress != null && mailingAddress.getPostalCode() != null) {
                        value = mailingAddress.getPostalCode();
                    } else if (physicalAddress.getPostalCode() != null) {
                        value = physicalAddress.getPostalCode();
                    }
                }
                if (PARAM_MAIL_COUNTY.equals(parameter)) {
                    if (mailingAddress != null && mailingAddress.getCounty() != null) {
                        value = mailingAddress.getCounty();
                    } else if (physicalAddress.getCounty() != null) {
                        value = physicalAddress.getCounty();
                    }
                }
            }
            return value;
        }
    }

    public static final String PARAM_REMOVE_HEADER = "removeHeader";
    public static final String PARAM_REPORT_DATE = "reportDate";

    protected static final String ILLEGAL_PHONENUMBER_CHARACTERS = "[_\\W]";


    /**
     * Instance variables
     */
    protected Pattern m_illegalPhoneCharacters = Pattern.compile(ILLEGAL_PHONENUMBER_CHARACTERS);
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;


    /**
     * Overrided getHeading Method. The reason is so that the user can decide if the header is
     * included or not at runtime.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        if ((Boolean) getParameter(PARAM_REMOVE_HEADER) == Boolean.TRUE) {
            return null;
        }
        return super.getHeading();
    }

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        Criteria studentCriteria = m_helper.getStudentCriteria();
        SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        Criteria studentContactCriteria = getStudentContactCriteria();
        studentContactCriteria.addIn(StudentContact.REL_STUDENT + "." + X2BaseBean.COL_OID, studentSubQuery);

        // Create StudentContact Query
        QueryByCriteria query = new QueryByCriteria(StudentContact.class, studentContactCriteria);
        applyInputSort(query, null);
        setQuery(query);

        setEntityClass(ContactEntity.class);

        HashMap calcs = new HashMap<String, FieldRetriever>();
        HashMap validators = new HashMap<String, FieldRetriever>();

        calcs.put("PHONE", new RetrievePhoneNumber());
        calcs.put("ADDRESS", new RetrieveAddress());

        super.addCalcs(calcs);
        super.addValidators(validators);
    }

    /**
     * Gets the student contact criteria.
     *
     * @return Criteria
     */
    private Criteria getStudentContactCriteria() {
        X2Criteria studentContactCriteria = new X2Criteria();
        applyInputCriteria(studentContactCriteria, false, StudentContact.REL_STUDENT);
        // Look up school or organization level enrollment records.
        if (isSchoolContext()) {
            studentContactCriteria.addEqualTo(StudentContact.REL_STUDENT + "." + Student.COL_SCHOOL_OID,
                    getSchool().getOid());
        }
        return studentContactCriteria;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
    }
}
