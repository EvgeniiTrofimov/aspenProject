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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.framework.persistence.adjusters.DistinctAdjuster;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * This class is used by a district in China for Student Demographics. This class does not use
 * StudentHistoryHelper
 * 
 * @author Follett Software Company
 *
 */
public class StudentDemographicsNSH extends StateReportData {
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
     * Student identification mode MODE_STUDENT_ACTIVE_ANY_TIME.
     * <br>
     * When operating in this mode, the student selection and filtering will identify
     * students who were active at any time within the selected date range. Their enrollment
     * information and events within that time will be available.
     * <ul>
     * <li>PROPERTY_BEGIN_DATE Specify the active range begin date.</li>
     * <li>PROPERTY_END_DATE Specify the active range end date.</li>
     * <li>PROPERTY_WITHDRAWN_AFTER_DATE Specify to include students who are withdrawn after a given
     * date, generally a date before the Begin date.</li>
     * <li>PROPERTY_INCLUDE_SECONDARY Specify that secondary students should be included when a
     * specific school is selected.</li>
     * </ul>
     */
    private static final String MODE_STUDENT_ACTIVE_ANY_TIME = "MODE_STUDENT_ACTIVE_ANY_TIME";

    /**
     * Parameter for use with setStudentSelectionProperties() to indicate if the standard
     * tool input selection criteria and sort should be applied to the student selection.
     * <p>
     * The related property should be a Boolean.
     * <br>
     * If not set, the default will be TRUE.
     */
    private static final String PROPERTY_APPLY_INPUT = "PROPERTY_APPLY_INPUT";

    /**
     * Parameter for use with setStudentSelectionProperties() to indicate if the standard
     * tool input school selection should be applied to the student selection.
     * <p>
     * The related property should be a Boolean.
     * <br>
     * If not set, the default will be TRUE.
     */
    private static final String PROPERTY_APPLY_SCHOOL = "PROPERTY_APPLY_SCHOOL";

    /**
     * Parameter for use with setStudentSelectionProperties() to set the beginning date of the
     * date range for identifying an active student.
     * <p>
     * The related property should be a PlainDate.
     * <br>
     * If not set, the default will be the beginning of the district school year.
     */
    private static final String PROPERTY_BEGIN_DATE = "PROPERTY_BEGIN_DATE";

    /**
     * Parameter for use with setStudentSelectionProperties() to set the ending date of the
     * date range for identifying an active student.
     * <p>
     * The related property should be a PlainDate.
     * <br>
     * If not set, the default will be the current date.
     */
    private static final String PROPERTY_END_DATE = "PROPERTY_END_DATE";

    /**
     * Parameter for use with setStudentSelectionProperties() to set an additional
     * date for selecting students to report. If set, students who have a Withdrawal
     * StudentEnrollment record after this date will also be included. This will typically be
     * used to include summer withdrawal students as part of a student demographic report.
     * <p>
     * The related property should be a PlainDate.
     * <br>
     * If not set, the withdrawn after date will not be used, and only begin date will be used.
     */
    private static final String PROPERTY_WITHDRAWN_AFTER_DATE = "PROPERTY_WITHDRAWN_AFTER_DATE";

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

    private X2Criteria m_studentCriteria;

    private String m_studentSelectionMode;

    private Map<String, Object> m_studentSelectionProperties = new HashMap<String, Object>();

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


        m_studentSelectionMode = MODE_STUDENT_ACTIVE_ANY_TIME;
        m_studentSelectionProperties.put(PROPERTY_BEGIN_DATE, getCurrentContext().getStartDate());
        m_studentSelectionProperties.put(PROPERTY_END_DATE, m_districtSchoolEndDate);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(buildStudentQuery(false));
            setEntityClass(StudentDemographicsEntity.class);

            // Build a map of calculations
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STD-CONTACT-RTRVE", new ContactRetriever());
            calcs.put("STD-ADDRESS-RTRVE", new AddressRetriever());
            super.addCalcs(calcs);
        }
    }

    /**
     * Build the student selection query. This is based on the student selection criteria.
     *
     * @param distinct boolean
     * @return QueryByCriteria
     */
    private BeanQuery buildStudentQuery(boolean distinct) {
        // Find the alias fields and preferences.

        Boolean applyInput = (Boolean) getStudentSelectionProperty(PROPERTY_APPLY_INPUT, Boolean.class, Boolean.TRUE);

        // Identify parameters for student selection.
        if (m_studentCriteria == null) {
            m_studentCriteria = buildStudentCriteria();
        }

        // Build the query.
        BeanQuery query = new BeanQuery(SisStudent.class, m_studentCriteria);

        // Apply user input criteria.
        if (applyInput.booleanValue()) {
            this.applyInputSort(query, null);
        }

        // If the calling process requests the query to return a distinct value (due to modified
        // criteria),
        // apply distinct and a distinct adjuster to the query.
        if (distinct) {
            query.setDistinct(distinct);
            DataDictionaryTable table =
                    this.getDataDictionary().findDataDictionaryTableByClass(this.getBeanClass().getName());
            DistinctAdjuster adjuster =
                    new DistinctAdjuster(table.getPrimaryKeyColumn(), this.getBroker().getPersistenceKey());
            query.addQueryAdjuster(adjuster);
        }
        return query;
    }

    /**
     * Return the user value entered for a parameter. Verify that the value matches a specified
     * type.
     *
     * @param selectKey The key of the property to retrieve.
     * @param expectedClass The class type expected for the value.
     * @param defaultValue The value to return if the property is not present or is null.
     *
     * @return Object
     */
    private Object getStudentSelectionProperty(String selectKey, Class expectedClass, Object defaultValue) {
        Object value = m_studentSelectionProperties.get(selectKey);
        if (value != null) {
            if (!expectedClass.isInstance(value)) {
                throw new ClassCastException("getStudentSeletionProperty(" + selectKey + "): Expected "
                        + expectedClass.getName() + ", found " + value.getClass().getName());
            }
        } else {
            value = defaultValue;
        }
        return value;
    }

    /**
     * Build the student selection criteria to load students reportable within a school year.
     * <br>
     * Student criteria is built based on mode.
     * <p>
     * When the mode is a student selection mode,
     * MODE_STUDENT_ACTIVE_ANY_TIME or MODE_STUDENT_ACTIVE_SNAPSHOT, the query is based on students
     * who are active or who have enrollment activity during the year.
     * <p>
     * When the mode is a schedule mode, MODE_SCHEDULE_ACTIVE or MODE_SCHEDULE_SPANS, the query
     * is based on students who have a student schedule, schedule change or transcript that matches
     * the appropriate criteria for those records. <b>As a result, criteria for those should be
     * built
     * first before getting student criteria or query.</b>
     * <p>
     * Further criteria are applied as required by property values.
     *
     * @return X2Criteria
     */
    private X2Criteria buildStudentCriteria() {
        // Verify that we have enough information to start.
        if (m_studentSelectionMode == null) {
            throw new X2RuntimeException();
        }

        X2Criteria criteria = new X2Criteria();

        if (MODE_STUDENT_ACTIVE_ANY_TIME.equals(m_studentSelectionMode)) {
            // Identify parameters for student selection.
            Boolean applyInput =
                    (Boolean) getStudentSelectionProperty(PROPERTY_APPLY_INPUT, Boolean.class, Boolean.TRUE);
            Boolean applySchool =
                    (Boolean) getStudentSelectionProperty(PROPERTY_APPLY_SCHOOL, Boolean.class, Boolean.TRUE);
            PlainDate beginDate = (PlainDate) getStudentSelectionProperty(PROPERTY_BEGIN_DATE, PlainDate.class,
                    getCurrentContext().getStartDate());
            PlainDate waDate =
                    (PlainDate) getStudentSelectionProperty(PROPERTY_WITHDRAWN_AFTER_DATE, PlainDate.class, null);

            // Build the subquery for student enrollments within the current year.
            X2Criteria enrollmentCriteria = new X2Criteria();

            if (waDate != null) {
                enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, waDate);
            } else {
                enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, beginDate);
            }

            // Apply school selection criteria.
            if (applySchool.booleanValue() && this.isSchoolContext()) {
                enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, this.getSchool().getOid());
            } else {
                enrollmentCriteria.addNotEqualTo(
                        StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                enrollmentCriteria.addNotEqualTo(
                        StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }
            SubQuery enrollmentSubQuery =
                    new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);
            X2Criteria enrCriteria = new X2Criteria();
            enrCriteria.addIn(X2BaseBean.COL_OID, enrollmentSubQuery);

            // Select students who are active.
            X2Criteria activeCriteria = new X2Criteria();
            activeCriteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
            // Apply school selection criteria.
            if (applySchool.booleanValue() && this.isSchoolContext()) {
                activeCriteria.addEqualTo(Student.COL_SCHOOL_OID, this.getSchool().getOid());
            } else {
                activeCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                activeCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }

            // join the two criteria in an OR.
            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addOrCriteria(enrCriteria);
            orCriteria.addOrCriteria(activeCriteria);

            // Build the final student criteria, including user criteria and exclude criteria.
            criteria.addAndCriteria(orCriteria);

            // Apply user input criteria.
            if (applyInput.booleanValue()) {
                this.applyInputCriteria(criteria, false, null);
            }

        }
        return criteria;
    }
}
