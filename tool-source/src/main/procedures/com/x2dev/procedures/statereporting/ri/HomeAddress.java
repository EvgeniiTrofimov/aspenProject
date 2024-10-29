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
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * RI state report for Home Address export. This class implements the data
 * export for the RI Home Address export.
 *
 * @author X2 Development Corporation
 */
public class HomeAddress extends RIStateReportData {
    /**
     * Implementation of Home Address Entity to be used by the Home Address export. This
     * must be a public static inner class with a public no argument constructor
     * so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class HomeAddressEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        HomeAddress m_homeAddress;
        SisStudent m_student;
        Address m_studentMailingAddress;
        Address m_studentPhysicalAddress;
        Address m_primaryContactMailingAddress;
        Address m_primaryContactPhysicalAddress;
        Address m_secondaryContactMailingAddress;
        Address m_secondaryContactPhysicalAddress;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public HomeAddressEntity() {
            // Empty constructor for dynamic instantiation.
        }

        /**
         * Returns the mailing address. if this is null or empty, return the physical address
         *
         * @param addressee String
         * @return Address
         */
        public Address getMailingOrPhysicalAddress(String addressee) {
            Address address = null;
            if (addressee.equals(PARAM_STUDENT_ADDRESSEE)) {
                address = m_studentMailingAddress;
                if (address == null ||
                        (StringUtils.isEmpty(address.getAddressLine01())
                                && StringUtils.isEmpty(address.getAddressLine03()))) {
                    address = m_studentPhysicalAddress;
                }

            }

            else if (addressee.equals(PARAM_PRIMARY_CONTACT_ADDRESSEE)) {
                address = m_primaryContactMailingAddress;
                if (address == null ||
                        (StringUtils.isEmpty(address.getAddressLine01())
                                && StringUtils.isEmpty(address.getAddressLine03()))) {
                    address = m_primaryContactPhysicalAddress;
                }

                if (address == null ||
                        (StringUtils.isEmpty(address.getAddressLine01())
                                && StringUtils.isEmpty(address.getAddressLine03()))) {
                    address = m_studentPhysicalAddress;
                }
            }

            else if (addressee.equals(PARAM_SECONDARY_CONTACT_ADDRESSEE)) {
                address = m_secondaryContactMailingAddress;
                if (address == null ||
                        (StringUtils.isEmpty(address.getAddressLine01())
                                && StringUtils.isEmpty(address.getAddressLine03()))) {
                    address = m_secondaryContactPhysicalAddress;
                }

                if (address == null ||
                        (StringUtils.isEmpty(address.getAddressLine01())
                                && StringUtils.isEmpty(address.getAddressLine03()))) {
                    address = m_studentPhysicalAddress;
                }
            }
            return address;
        }

        /**
         * Returns the physical address zip code field, which contains the 5 digit zip code and the
         * +4 zip code.
         *
         * @return Address
         */
        public Address getPhysicalAddress() {
            return m_studentPhysicalAddress;
        }

        /**
         * Include address.
         *
         * @param addressee String
         * @return true, if successful
         */
        public boolean includeAddress(String addressee) {
            return (addressee != null) && (addressee.equals(PARAM_PRIMARY_CONTACT_ADDRESSEE)
                    || (addressee.equals(PARAM_SECONDARY_CONTACT_ADDRESSEE)
                            && !m_homeAddress.m_primaryContactOnly.booleanValue())
                            && (m_student.getSecondaryContact() != null))
                    || (!addressee.equals(PARAM_SECONDARY_CONTACT_ADDRESSEE)
                            && !addressee.equals(PARAM_PRIMARY_CONTACT_ADDRESSEE));
        }

        /**
         * Initialize the entity for the student bean provided. This method
         * finds the student schedule and student schedule change records for
         * the student and generates a list of reportable schedule items. The
         * entity can produce multiple rows from these results.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData,
         *      com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);

            m_homeAddress = (HomeAddress) data;
            m_student = (SisStudent) bean;
            HomeAddress haData = (HomeAddress) data;
            m_student.getLocalId();
            Collection<StudentEnrollmentSpan> spans = haData.m_studentHelper.getStudentEnrollmentSpans(m_student, true);

            if (!includeStudent(spans, haData.m_endDate, haData.m_statusToInclude)) {
                setRowCount(0);
            } else {
                Person studentPerson = m_student.getPerson();
                if (studentPerson != null) {
                    m_studentMailingAddress = studentPerson.getMailingAddress();
                    m_studentPhysicalAddress = studentPerson.getPhysicalAddress();
                }

                Person primaryContact = null;
                if (m_student.getPrimaryContact() != null &&
                        m_student.getPrimaryContact().getPerson() != null) {
                    primaryContact = m_student.getPrimaryContact().getPerson();
                }

                if (primaryContact != null) {
                    m_primaryContactMailingAddress = primaryContact.getMailingAddress();
                    m_primaryContactPhysicalAddress = primaryContact.getPhysicalAddress();
                }

                Person secondaryContact = null;
                if (m_student.getSecondaryContact() != null &&
                        m_student.getSecondaryContact().getPerson() != null) {
                    secondaryContact = m_student.getSecondaryContact().getPerson();
                }

                if (secondaryContact != null) {
                    m_secondaryContactMailingAddress = secondaryContact.getMailingAddress();
                    m_secondaryContactPhysicalAddress = secondaryContact.getPhysicalAddress();
                }
            }
        }

        /**
         * Returns the display name of the represented entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StringBuilder buffer = new StringBuilder();
            SisStudent student = (SisStudent) getBean();

            buffer.append(student.getNameView());
            buffer.append(" [LASID=");
            buffer.append(student.getLocalId());
            buffer.append(", SASID=");
            buffer.append(student.getStateId());
            buffer.append("]");

            return buffer.toString();
        }

        /**
         * Include student.
         *
         * @param spans Collection<StudentEnrollmentSpan>
         * @param date PlainDate
         * @param status String
         * @return true, if successful
         */
        private boolean includeStudent(Collection<StudentEnrollmentSpan> spans, PlainDate date, String status) {
            boolean include = false;
            if (spans != null && !spans.isEmpty()) {
                boolean active = false;
                for (StudentEnrollmentSpan span : spans) {
                    if (!date.before(span.getFirstActiveDate())
                            && (span.getLastActiveDate() == null || !date.after(span.getLastActiveDate()))) {
                        active = true;
                    }
                }
                if ((active && !STATUS_INACTIVE.equals(status)) || (!active && !STATUS_ACTIVE.equals(status))) {
                    include = true;
                }
            }
            return include;
        }
    }

    public static final String PARAM_PRIMARY_CONTACT_ADDRESSEE = "primaryContact";
    public static final String PARAM_PERSON_ADDRESSEE = "person";
    public static final String PARAM_STUDENT_ADDRESSEE = "student";
    public static final String PARAM_SECONDARY_CONTACT_ADDRESSEE = "secondaryContact";

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Input parameter: Select organization level.
     */
    public static final String ORGANIZATION_PARAM = "orgOid";

    /**
     * Name for the primaryContactOnly students only parameter. The value is a boolean.
     */
    public static final String PRIMARY_CONTACT_ONLY_PARAM = "primaryContactOnly";

    /**
     * Name for the startDate parameter. The value is a Date.
     */
    public static final String START_DATE_PARAM = "startDate";

    /**
     * Name for the endDate parameter. The value is a Date.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Name for the sasid students only parameter. The value is a boolean.
     */
    public static final String SASID_STUDENTS_ONLY_PARAM = "sasidStudentsOnly";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the enumerated "selection" parameter. The value is an String.
     */
    public static final String STATUS_PARAM = "status";

    /**
     * Options of status selection param
     */
    public static final String STATUS_ACTIVE = "active";
    public static final String STATUS_BOTH = "both";
    public static final String STATUS_INACTIVE = "inactive";
    public static final String RELATIONSHIP_CODE_JAVA_NAME = "relationshipCode";
    private static final String ALIAS_CTJ_PRIM_LANG = "all-ctj-ContactPrimaryLanguage";
    private static final String ALIAS_PSN_CELL_PHONE = "all-psn-CellPhone";
    private static final String ALIAS_PSN_HOME_PHONE = "all-psn-HomePhone";
    private static final String ALIAS_PSN_WORK_PHONE = "all-psn-WorkPhone";
    private static final String EXCLUDE_ALIAS_SKL = "all-skl-ExcludefromReporting";
    private static final String EXCLUDE_ALIAS_STD = "DOE EXCLUDE STD";

    protected Map<String, String> m_contactRelationshipCodesMap;
    protected PlainDate m_districtFirstSessionDate;
    protected String m_excludeSklField;
    protected String m_excludeStdField;
    protected String m_fieldCtjPrimLang;
    protected String m_fieldPsnCellPhone;
    protected String m_fieldPsnHomePhone;
    protected String m_fieldPsnWorkPhone;
    protected PlainDate m_startDate;
    protected PlainDate m_endDate;
    protected String m_statusToInclude;
    protected StudentHistoryHelper m_studentHelper;
    protected Boolean m_primaryContactOnly;

    /**
     * Retrieve the mailing address line 1.
     */
    protected class RetrieveMailingLine1 implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String addressee = (String) field.getParameter();

            HomeAddressEntity homeAddressEntity = ((HomeAddressEntity) entity);
            String addressLine1 = null;

            if (homeAddressEntity.includeAddress(addressee)) {
                if (homeAddressEntity.getMailingOrPhysicalAddress(addressee) != null) {
                    addressLine1 = homeAddressEntity.getMailingOrPhysicalAddress(addressee).getAddressLine01();
                }
            }

            return addressLine1;
        }
    }

    /**
     * Retrieve the mailing address line 2.
     */
    protected class RetrieveMailingLine2 implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String addressee = (String) field.getParameter();

            HomeAddressEntity homeAddressEntity = ((HomeAddressEntity) entity);
            String addressLine2 = null;

            if (homeAddressEntity.includeAddress(addressee)) {
                if (homeAddressEntity.getMailingOrPhysicalAddress(addressee) != null) {
                    addressLine2 = homeAddressEntity.getMailingOrPhysicalAddress(addressee).getAddressLine02();
                }
            }

            return addressLine2;
        }
    }

    /**
     * Retrieve the mailing address city.
     */
    protected class RetrieveMailingCity implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {

            String addressee = (String) field.getParameter();

            HomeAddressEntity homeAddressEntity = ((HomeAddressEntity) entity);
            String city = null;

            if (homeAddressEntity.includeAddress(addressee)) {
                if (homeAddressEntity.getMailingOrPhysicalAddress(addressee) != null) {
                    city = homeAddressEntity.getMailingOrPhysicalAddress(addressee).getCity();
                }
            }

            return city;
        }
    }

    /**
     * Retrieve the mailing address state.
     */
    protected class RetrieveMailingState implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String addressee = (String) field.getParameter();

            HomeAddressEntity homeAddressEntity = ((HomeAddressEntity) entity);
            String state = null;
            if (homeAddressEntity.includeAddress(addressee)) {
                if (homeAddressEntity.getMailingOrPhysicalAddress(addressee) != null) {
                    state = homeAddressEntity.getMailingOrPhysicalAddress(addressee).getState();
                }
            }
            return state;
        }
    }

    /**
     * Retrieve the mailing zip code, 5 digits only.
     */
    protected class RetrieveMailingZip implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String addressee = (String) field.getParameter();

            HomeAddressEntity homeAddressEntity = ((HomeAddressEntity) entity);
            String mailingZipCode = null;

            if (homeAddressEntity.includeAddress(addressee)) {
                if (homeAddressEntity.getMailingOrPhysicalAddress(addressee) != null) {
                    mailingZipCode = homeAddressEntity.getMailingOrPhysicalAddress(addressee).getPostalCode();
                }
            }

            String mailingZipCode5chars = "";
            if (mailingZipCode != null && mailingZipCode.length() >= 5) {
                mailingZipCode5chars = mailingZipCode.substring(0, 5);
            }

            return mailingZipCode5chars;
        }
    }

    /**
     * Retrieve the physical zip code, 5 digits only.
     */
    protected class RetrievePhysicalZip implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            HomeAddressEntity homeAddressEntity = ((HomeAddressEntity) entity);
            String physicalZipCode = null;
            if (homeAddressEntity.getPhysicalAddress() != null) {
                physicalZipCode = homeAddressEntity.getPhysicalAddress().getPostalCode();
            }
            String physicalZipCode5chars = "";
            if (physicalZipCode != null && physicalZipCode.length() >= 5) {
                physicalZipCode5chars = physicalZipCode.substring(0, 5);
            }

            return physicalZipCode5chars;
        }
    }

    /**
     * Retrieve the mailing zip code +4.
     *
     */
    protected class RetrieveMailingZip4 implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String addressee = (String) field.getParameter();

            HomeAddressEntity homeAddressEntity = ((HomeAddressEntity) entity);
            String mailingZipCode = null;

            if (homeAddressEntity.includeAddress(addressee)) {
                if (homeAddressEntity.getMailingOrPhysicalAddress(addressee) != null) {
                    mailingZipCode = homeAddressEntity.getMailingOrPhysicalAddress(addressee).getPostalCode();
                }
            }

            String mailingZipCode4chars = "";
            if (mailingZipCode != null && mailingZipCode.length() >= 9) {
                mailingZipCode4chars = mailingZipCode.substring(6, 10);
            }

            return mailingZipCode4chars;
        }
    }

    /**
     * Retrieve personal data about a contact.
     */
    protected class RetrievePersonalData implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String[] parameters = ((String) field.getParameter()).split(",");
            String addressee = parameters[0];
            String requestedParameter = parameters[1];
            String requestedData = null;

            HomeAddressEntity homeAddressEntity = ((HomeAddressEntity) entity);
            StudentContact contact = null;
            if (homeAddressEntity.m_student != null && addressee.equals(PARAM_PRIMARY_CONTACT_ADDRESSEE)) {
                contact = homeAddressEntity.m_student.getPrimaryContact();
            } else if (homeAddressEntity.m_student != null && addressee.equals(PARAM_SECONDARY_CONTACT_ADDRESSEE)
                    && !m_primaryContactOnly.booleanValue()) {
                contact = homeAddressEntity.m_student.getSecondaryContact();
            }
            if (contact != null) {
                if (requestedParameter.equals("firstName") &&
                        contact.getPerson() != null) {
                    requestedData = contact.getPerson().getFirstName();
                } else if (requestedParameter.equals("lastName") &&
                        contact.getPerson() != null) {
                    requestedData = contact.getPerson().getLastName();
                } else if (requestedParameter.equals("relation")) {
                    requestedData = contact.getRelationshipCode();

                    if (!m_contactRelationshipCodesMap.isEmpty()) {
                        String stateCode = m_contactRelationshipCodesMap.get(requestedData);
                        if (!StringUtils.isEmpty(stateCode)) {
                            requestedData = stateCode;
                        } else {
                            requestedData = null;
                        }
                    } else {
                        requestedData = null;
                    }
                } else if (requestedParameter.equals("email") &&
                        contact.getPerson() != null) {
                    requestedData = contact.getPerson().getEmail01();
                } else if (requestedParameter.equals("phoneHome") &&
                        contact.getPerson() != null) {
                    if (m_fieldPsnHomePhone != null
                            && contact.getPerson().getFieldValueByBeanPath(m_fieldPsnHomePhone) != null) {
                        requestedData = ((String) contact.getPerson().getFieldValueByBeanPath(m_fieldPsnHomePhone))
                                .replaceAll("[^0-9]", "");
                    } else if (contact.getPerson().getPhone01() != null) {
                        requestedData = contact.getPerson().getPhone01().replaceAll("[^0-9]", "");
                    }
                } else if (requestedParameter.equals("phoneWork") &&
                        contact.getPerson() != null) {
                    if (m_fieldPsnWorkPhone != null
                            && contact.getPerson().getFieldValueByBeanPath(m_fieldPsnWorkPhone) != null) {
                        requestedData = ((String) contact.getPerson().getFieldValueByBeanPath(m_fieldPsnWorkPhone))
                                .replaceAll("[^0-9]", "");
                    } else if (contact.getPerson().getPhone02() != null) {
                        requestedData = contact.getPerson().getPhone02().replaceAll("[^0-9]", "");
                    }
                } else if (requestedParameter.equals("phoneCell") &&
                        contact.getPerson() != null) {
                    if (m_fieldPsnCellPhone != null
                            && contact.getPerson().getFieldValueByBeanPath(m_fieldPsnCellPhone) != null) {
                        requestedData = ((String) contact.getPerson().getFieldValueByBeanPath(m_fieldPsnCellPhone))
                                .replaceAll("[^0-9]", "");
                    } else if (contact.getPerson().getPhone03() != null) {
                        requestedData = contact.getPerson().getPhone03().replaceAll("[^0-9]", "");
                    }
                } else if (requestedParameter.equals("language")) {
                    if (contact.getFieldValueByBeanPath(m_fieldCtjPrimLang) != null) {
                        requestedData = data.lookupStateValue(StudentContact.class, m_fieldCtjPrimLang,
                                (String) contact.getFieldValueByBeanPath(m_fieldCtjPrimLang));
                    }
                    if (StringUtils.isEmpty(requestedData) && contact.getStudent() != null) {
                        requestedData = contact.getStudent().getHomeLanguageCode();
                        if (!StringUtils.isEmpty(requestedData)) {
                            requestedData = data.lookupStateValue(SisStudent.class, SisStudent.COL_HOME_LANGUAGE_CODE,
                                    requestedData);
                        }
                    }
                }
                // TODO : get address 1, 2, city and state -- wherever
                // "Phone-EC" is used as placeholder in export format.

            } else if (addressee.equals(PARAM_PERSON_ADDRESSEE) && requestedParameter.equals("phoneHome")) {
                SisPerson psn = homeAddressEntity.m_student.getPerson();
                if (m_fieldPsnHomePhone != null && psn.getFieldValueByBeanPath(m_fieldPsnCellPhone) != null) {
                    requestedData =
                            ((String) psn.getFieldValueByBeanPath(m_fieldPsnCellPhone)).replaceAll("[^0-9]", "");
                } else if (psn.getPhone01() != null) {
                    requestedData = psn.getPhone01().replaceAll("[^0-9]", "");
                }
            }
            return requestedData;
        }

    }

    /**
     * Retrieve the physical zip code +4.
     *
     */
    protected class RetrievePhysicalZip4 implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            HomeAddressEntity homeAddressEntity = ((HomeAddressEntity) entity);

            String physicalZipCode = null;
            if (homeAddressEntity.getPhysicalAddress() != null) {
                physicalZipCode = homeAddressEntity.getPhysicalAddress().getPostalCode();
            }

            String physicalZipCode4chars = "";
            if (physicalZipCode != null && physicalZipCode.length() >= 9) {
                physicalZipCode4chars = physicalZipCode.substring(6, 10);
            }

            return physicalZipCode4chars;
        }
    }

    /**
     * retrieve the emergency contact's phone number.
     */
    protected class RetrievePhoneEmergencyContact implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            HomeAddressEntity homeAddressEntity = ((HomeAddressEntity) entity);
            SisStudent student = (SisStudent) homeAddressEntity.getBean();

            StudentContact studentContact = student.getPrimaryContact(data.getBroker());
            String emergencyPhone = "";
            if (studentContact != null &&
                    studentContact.getContact() != null &&
                    studentContact.getContact().getPerson() != null) {
                emergencyPhone = studentContact.getContact().getPerson().getPhone01();
            }
            return emergencyPhone;
        }
    }

    /**
     * Retrieve the secondary contact's phone number.
     */
    protected class RetrievePhoneSecondaryContact implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            HomeAddressEntity homeAddressEntity = ((HomeAddressEntity) entity);
            SisStudent student = (SisStudent) homeAddressEntity.getBean();

            StudentContact studentContact = student.getSecondaryContact(data.getBroker());
            String secondaryPhone = "";
            if (studentContact != null &&
                    studentContact.getContact() != null &&
                    studentContact.getContact().getPerson() != null) {
                secondaryPhone = studentContact.getContact().getPerson().getPhone01();
            }
            return secondaryPhone;
        }
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        Organization district = OrganizationManager.getRootOrganization(getBroker());
        DistrictCalendar districtCalender = CalendarManager.getDistrictInSessionStartEndDate(district,
                getCurrentContext(), true,
                getBroker());
        m_districtFirstSessionDate = districtCalender.getDate();
        m_excludeSklField = translateAliasToJavaName(EXCLUDE_ALIAS_SKL, false);
        m_excludeStdField = translateAliasToJavaName(EXCLUDE_ALIAS_STD, false);
        m_fieldCtjPrimLang = translateAliasToJavaName(ALIAS_CTJ_PRIM_LANG, true);
        m_fieldPsnCellPhone = translateAliasToJavaName(ALIAS_PSN_CELL_PHONE, false);
        m_fieldPsnHomePhone = translateAliasToJavaName(ALIAS_PSN_HOME_PHONE, false);
        m_fieldPsnWorkPhone = translateAliasToJavaName(ALIAS_PSN_WORK_PHONE, false);
        Boolean sasidStudentOnly = (Boolean) getParameter(SASID_STUDENTS_ONLY_PARAM);

        m_primaryContactOnly = (Boolean) getParameter(PRIMARY_CONTACT_ONLY_PARAM);

        m_startDate = (PlainDate) getParameter(START_DATE_PARAM);
        m_endDate = (PlainDate) getParameter(END_DATE_PARAM);
        if (m_endDate == null) {
            m_endDate = new PlainDate(getTimeZone());
        }

        // initialize the map for the relationship code to lookup the state code
        // if different from the local code
        m_contactRelationshipCodesMap = new HashMap<String, String>();
        DataDictionaryField field = getDataDictionaryField(StudentContact.class,
                RELATIONSHIP_CODE_JAVA_NAME);
        if (field != null) {
            ReferenceTable refTable = field.getReferenceTable();
            if (refTable != null) {
                for (ReferenceCode code : refTable.getReferenceCodes()) {
                    if (!StringUtils.isEmpty(code.getStateCode())) {
                        m_contactRelationshipCodesMap.put(code.getCode(), code.getStateCode());
                    }
                }
            }
        }

        m_statusToInclude = (String) getParameter(STATUS_PARAM);

        m_studentHelper = new StudentHistoryHelper(this);
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        if (isSchoolContext()) {
            m_studentHelper.getStudentCriteria().addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        }
        if (sasidStudentOnly.booleanValue()) {
            m_studentHelper.getStudentCriteria().addNotEmpty(SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        if (m_excludeSklField != null) {
            m_studentHelper.getStudentCriteria().addNotEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_excludeSklField,
                    BooleanAsStringConverter.TRUE);
        }

        // Set the query to be used for student selection.
        setQuery(m_studentHelper.getStudentQuery(true));
        setEntityClass(HomeAddressEntity.class);

        // Build maps of retriever functions
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();

        calcs.put("HA-MailingZip", new RetrieveMailingZip());
        calcs.put("HA-MailingZip4", new RetrieveMailingZip4());
        calcs.put("HA-DwellingZip", new RetrievePhysicalZip());
        calcs.put("HA-DwellingZip4", new RetrievePhysicalZip4());
        calcs.put("HA-PhoneEC", new RetrievePhoneEmergencyContact());
        // using the same retriever
        calcs.put("HA-PhonePC", new RetrievePhoneEmergencyContact());
        calcs.put("HA-PhoneSC", new RetrievePhoneSecondaryContact());
        calcs.put("HA-MailLine1", new RetrieveMailingLine1());
        calcs.put("HA-MailLine2", new RetrieveMailingLine2());
        calcs.put("HA-MailCity", new RetrieveMailingCity());
        calcs.put("HA-MailState", new RetrieveMailingState());
        calcs.put("HA-PersonalData", new RetrievePersonalData());

        super.addCalcs(calcs);
    }

}
