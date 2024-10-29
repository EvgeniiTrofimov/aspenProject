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

package com.x2dev.procedures.statereporting.ca;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export Data Module for CA Student Information.
 *
 * @author X2 Development Corporation
 */

public class CAStudentInformation extends StateReportData {

    /**
     * Entity class for CA Student Information export.
     *
     */
    public static class CAStudentInformationEntity extends StateReportEntity {
        CAStudentInformation m_data;
        /**
         * Entity instance variables.
         */
        List<StudentEnrollmentSpan> m_enrollmentSpans;
        StudentContact m_firstContact;
        List<Race> m_races = new ArrayList<Race>();
        StudentContact m_secondContact;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CAStudentInformationEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the StudentEnrollmentSpan record for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public StudentEnrollmentSpan getEnrollmentSpan() {
            return m_enrollmentSpans.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";
            StudentEnrollmentSpan span = getEnrollmentSpan();
            if (span != null) {
                name += span.getSchool().getName();
            }

            return name;
        }

        /**
         * Returns the First Student Contact record for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public StudentContact getFirstContact() {
            return m_firstContact;
        }

        /**
         * Returns the List of races for the current student.
         *
         * @return List<Race>
         */
        public List<Race> getRaces() {
            return m_races;
        }

        /**
         * Returns the Second Student Contact record for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public StudentContact getSecondContact() {
            return m_secondContact;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (CAStudentInformation) data;
            m_enrollmentSpans = new ArrayList();
            for (StudentEnrollmentSpan span : m_data.m_helper.getStudentEnrollmentSpans((SisStudent) bean, true)) {
                String excludeSchool = BooleanAsStringConverter.TRUE;
                if (span.getSchool() != null) {
                    excludeSchool = (String) span.getSchool()
                            .getFieldValueByBeanPath(((CAStudentInformation) data).m_fieldExcludeSchool);
                }
                if (!BooleanAsStringConverter.TRUE.equals(excludeSchool)) {
                    m_enrollmentSpans.add(span);
                }
            }

            if (!m_enrollmentSpans.isEmpty()) {
                Collection races = m_data.m_helper.getRaces((SisStudent) bean);
                if (races != null) {
                    m_races.addAll(races);
                    Collections.sort(m_races, new Comparator<Race>() {

                        @Override
                        public int compare(Race o1, Race o2) {
                            return m_data.getRacePriority(o1) - m_data.getRacePriority(o2);
                        }

                    });
                }

                Collection<StudentContact> contacts = m_data.getStudentContacts((SisStudent) bean);
                m_firstContact = null;
                m_secondContact = null;
                if (contacts != null) {
                    Iterator contact = contacts.iterator();
                    while (contact.hasNext()) {
                        StudentContact cont = (StudentContact) contact.next();
                        if (cont.getEmergencyPriority() == 1) {
                            m_firstContact = cont;
                        } else if (cont.getEmergencyPriority() == 2) {
                            m_secondContact = cont;
                        }
                    }
                }
            }

            setRowCount(m_enrollmentSpans.size());
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }
    }

    /**
     * Retrieve data from the student contacts collection based on field parameter
     * <p>
     * GUARDIAN1_FNAME - Guardian 1 First Name<br>
     * GUARDIAN1_LNAME - Guardian 1 Last Name<br>
     * GUARDIAN2_FNAME - Guardian 2 First Name<br>
     * GUARDIAN2_LNAME - Guardian 2 Last Name<br>
     * .
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveContacts implements FieldRetriever {
        private final String PARAM_GUARDIAN1_FNAME = "GUARDIAN1_FNAME";
        private final String PARAM_GUARDIAN1_LNAME = "GUARDIAN1_LNAME";
        private final String PARAM_GUARDIAN2_FNAME = "GUARDIAN2_FNAME";
        private final String PARAM_GUARDIAN2_LNAME = "GUARDIAN2_LNAME";
        private final String PARAM_HIGH_ED_LEVEL = "HIGH_ED_LEVEL";
        private final String PARAM_HIGH_ED_LEVEL_2 = "HIGH_ED_LEVEL_2";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String parameter = (String) field.getParameter();
            StudentContact firstContact = ((CAStudentInformationEntity) entity).getFirstContact();
            StudentContact secondContact = ((CAStudentInformationEntity) entity).getSecondContact();
            String value = "";

            if (PARAM_GUARDIAN1_FNAME.equals(parameter)) {
                value = getContactName(firstContact, true);
            } else if (PARAM_GUARDIAN1_LNAME.equals(parameter)) {
                value = getContactName(firstContact, false);
            } else if (PARAM_GUARDIAN2_FNAME.equals(parameter)) {
                value = getContactName(secondContact, true);
            } else if (PARAM_GUARDIAN2_LNAME.equals(parameter)) {
                value = getContactName(secondContact, false);
            } else if (PARAM_HIGH_ED_LEVEL.equals(parameter)) {
                if (firstContact != null) {
                    String highLevel = (String) firstContact.getContact().getFieldValueByBeanPath(m_fieldHighEdLevel);
                    value = data.lookupStateValue(Contact.class, m_fieldHighEdLevel, highLevel);
                }
            } else if (PARAM_HIGH_ED_LEVEL_2.equals(parameter)) {
                if (secondContact != null) {
                    String highLevel2 = (String) secondContact.getContact().getFieldValueByBeanPath(m_fieldHighEdLevel);
                    value = data.lookupStateValue(Contact.class, m_fieldHighEdLevel, highLevel2);
                }
            }
            return value;
        }

        /**
         * Method for retrieving first or last name of a person in a studentContact object.
         *
         * @param contact StudentContact class instance.
         * @param isFirstName boolean flag indicating which name to retrieve. True for first name,
         *        false for last name
         * @return String
         */
        private String getContactName(StudentContact contact, boolean isFirstName) {
            if (contact == null) {
                return null;
            }

            Person person = contact.getPerson();
            if (person == null) {
                return null;
            }

            return isFirstName
                    ? person.getFirstName()
                    : person.getLastName();
        }
    }

    /**
     * Retrieve data from the student enrollment record based on field parameter:
     *
     * <LI>DISTRICT_HOME - District of Geographic Residence
     * <LI>END_DATE - Effective End Date
     * <LI>INTERIDSTRICT_HOME - Interdistrict Transfer Code
     * <LI>OID - Local Record ID
     * <LI>START_DATE - Effective Start Date.
     *
     * @author X2 Development Corporation
     */

    protected class RetrieveEnrollment implements FieldRetriever {
        private final String PARAM_DISTRICT_HOME = "DISTRICT_HOME";
        private final String PARAM_END_DATE = "END_DATE";
        private final String PARAM_OID = "OID";
        private final String PARAM_START_DATE = "START_DATE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String parameter = (String) field.getParameter();
            StudentEnrollmentSpan span = ((CAStudentInformationEntity) entity).getEnrollmentSpan();
            Object value = null;

            if (span != null) {
                if (PARAM_START_DATE.equals(parameter)) {
                    StudentEnrollment enrollment = span.getFirstActiveEnrollment();
                    if (enrollment != null) {
                        value = enrollment.getEnrollmentDate();
                    }
                } else if (PARAM_END_DATE.equals(parameter)) {
                    StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                    if (enrollment != null) {
                        value = enrollment.getEnrollmentDate();
                    }
                } else if (PARAM_DISTRICT_HOME.equals(parameter)) {
                    StudentEnrollment enrollment = span.getFirstActiveEnrollment();
                    if (enrollment != null) {
                        value = enrollment.getFieldValueByBeanPath(m_fieldDistrictHome);
                    }
                } else if (PARAM_OID.equals(parameter)) {
                    StudentEnrollment enrollment = span.getFirstActiveEnrollment();
                    if (enrollment != null) {
                        value = enrollment.getOid();
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve race codes from the student races collection based on field parameter
     * (RACE_1..RACE_5)
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRaces implements FieldRetriever {
        private final String PARAM_RACE_1 = "RACE_1";
        private final String PARAM_RACE_2 = "RACE_2";
        private final String PARAM_RACE_3 = "RACE_3";
        private final String PARAM_RACE_4 = "RACE_4";
        private final String PARAM_RACE_5 = "RACE_5";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String parameter = (String) field.getParameter();
            List<Race> races = ((CAStudentInformationEntity) entity).getRaces();
            Object value = null;

            if (races != null) {
                if (PARAM_RACE_1.equals(parameter)) {
                    if (races.size() >= 1) {
                        String raceCode = races.get(0).getRaceCode();
                        value = lookupReferenceCodeByBeanPath(Race.class, Race.COL_RACE_CODE, raceCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                } else if (PARAM_RACE_2.equals(parameter)) {
                    if (races.size() >= 2) {
                        String raceCode = races.get(1).getRaceCode();
                        value = lookupReferenceCodeByBeanPath(Race.class, Race.COL_RACE_CODE, raceCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                } else if (PARAM_RACE_3.equals(parameter)) {
                    if (races.size() >= 3) {
                        String raceCode = races.get(2).getRaceCode();
                        value = lookupReferenceCodeByBeanPath(Race.class, Race.COL_RACE_CODE, raceCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                } else if (PARAM_RACE_4.equals(parameter)) {
                    if (races.size() >= 4) {
                        String raceCode = races.get(3).getRaceCode();
                        value = lookupReferenceCodeByBeanPath(Race.class, Race.COL_RACE_CODE, raceCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                } else if (PARAM_RACE_5.equals(parameter)) {
                    if (races.size() >= 5) {
                        String raceCode = races.get(4).getRaceCode();
                        value = lookupReferenceCodeByBeanPath(Race.class, Race.COL_RACE_CODE, raceCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                }
            }

            return value;
        }
    }

    /**
     * Retrieve Record Type Code from the input parameter.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTYCInd implements FieldRetriever {

        private DateAsStringConverter m_converter;

        /**
         * Instantiates a new retrieve TYC ind.
         */
        public RetrieveTYCInd() {
            m_converter = (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                    Locale.getDefault(),
                    true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = BooleanAsStringConverter.FALSE;
            SisStudent student = (SisStudent) entity.getBean();

            String birthCountry = (String) student.getPerson().getFieldValueByBeanPath(m_fieldBirthCountry);
            birthCountry = data.lookupReferenceCodeByBeanPath(SisPerson.class, m_fieldBirthCountry, birthCountry,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            if (birthCountry == null || !birthCountry.matches(MATCH_BIRTH_COUNTRY_CODE_USPR)) {
                String date = (String) student.getFieldValueByBeanPath(m_fieldInitUSDate);
                if (date != null) {
                    PlainDate reportDate = m_reportDate;
                    try {
                        PlainDate initUSDate = (PlainDate) m_converter.parseSystemString(date);
                        PlainDate initUSDatePlusThreeYears = DateUtils.add(initUSDate, Calendar.YEAR, 3);

                        if (!initUSDatePlusThreeYears.before(reportDate)) {
                            value = BooleanAsStringConverter.TRUE;
                        }
                    } catch (Exception e) {
                        // return false
                    }
                }
            }

            return value;
        }
    }

    /**
     * FieldValidator for:
     *
     * Residential Address Line 1,
     * Residential Address City Name,
     * Residential Address State Province Code,
     * Residential Address Zip Code
     *
     * <LI>If Primary Residence Category Code not = 100, 110, 120, 130 (Homeless), or 310 then this
     * field is required.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateAddress implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();

            String primResCat = (String) student.getFieldValueByBeanPath(m_fieldPrimaryResCat);

            if ((primResCat == null || !primResCat.matches(MATCH_PRIMARY_RES_CAT)) && value == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Primary Residence Category Code not = 100, 110, 120, 130 (Homeless), or 310 then this field is required",
                        "Primary Residence Category Code = " + STYLE_BOLD + primResCat +
                                STYLE_END +
                                " This field value = " + STYLE_BOLD + "NULL" + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * FieldValidator for:
     *
     * Student Birth Date
     *
     * <LI>If Grade Level Code is equal to Adult (AD) Then student age must be greater than or
     * equal to 16 and less than 80
     * <LI>Else Student age must be greater than 0 and less than or equal 22.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateBirthDate implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();
            if (student.getGradeLevel() == null) {
                return errors;
            }

            if (student.getPerson() == null) {
                return errors;
            }
            String gradeLevel = student.getGradeLevel();
            gradeLevel = data.lookupReferenceCodeByBeanPath(SisStudent.class,
                    SisStudent.COL_GRADE_LEVEL,
                    gradeLevel,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            if (StringUtils.isEmpty(gradeLevel)) {
                errors.add(new StateReportValidationError(entity, field, "Grade state code missing",
                        "State code is missing for gradeLevel"
                                + STYLE_BOLD + gradeLevel + STYLE_END));
                return errors;
            }

            int age = student.getPerson().getAge();

            if (gradeLevel.matches(MATCH_GRADE_LEVEL_CODE_AD)) {
                if (age < 16 || age >= 80) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If Grade Level Code is equal to Adult (AD) Then student age must be greater than or equal to 16 and less than 80",
                            "Grade Level Code = " + STYLE_BOLD + gradeLevel +
                                    STYLE_END +
                                    " Student age = " + STYLE_BOLD + age + STYLE_END));
                    return errors;
                }
            } else {
                if (age <= 0 || age > 22) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If Grade Level Code is not equal to Adult (AD) Then student age must be greater than 0 and less than or equal 22",
                            "Grade Level Code = " + STYLE_BOLD + gradeLevel +
                                    STYLE_END +
                                    " Student age = " + STYLE_BOLD + age + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * FieldValidator for:
     *
     * Student Proficient or Advanced for ELA Code
     * <LI>If English Language Acquisition Status State Code = RFEP then this field is required.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateELATest implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();

            String elaStatusCode = (String) student.getFieldValueByBeanPath(m_fieldELAStatusCode);

            if (elaStatusCode != null) {
                elaStatusCode =
                        data.lookupReferenceCodeByBeanPath(SisStudent.class, m_fieldELAStatusCode, elaStatusCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                boolean isCorrectEla = StringUtils.isEmpty(elaStatusCode)
                        ? false
                        : elaStatusCode.matches(MATCH_ELA_STATUS_CODE_RFEP);

                if (isCorrectEla && value == null) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If English Language Acquisition Status State Code = RFEP then this field is required",
                            "ELA Status Code = " + STYLE_BOLD + elaStatusCode + STYLE_END +
                                    " Student Proficient or Advanced for ELA Code = " +
                                    STYLE_BOLD + "NULL" + STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidateEmptyValue.
     */
    protected class ValidateEmptyValue implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
                return errors;
            }
            return errors;
        }
    }

    /**
     * FieldValidator for:
     *
     * Effective End Date
     * <LI>The value must be greater or equal to Effective Start Date.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateEndDate implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            StudentEnrollmentSpan span = ((CAStudentInformationEntity) entity).getEnrollmentSpan();

            if (span != null) {
                if (span.getFirstInactiveEnrollment() != null) {
                    PlainDate startDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                    PlainDate endDate = span.getFirstInactiveEnrollment().getEnrollmentDate();

                    if (startDate.after(endDate)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "The value must be greater or equal to Effective Start Date",
                                "Effective Start Date = " + STYLE_BOLD + startDate +
                                        STYLE_END +
                                        " Effective End Date = " + STYLE_BOLD + endDate +
                                        STYLE_END));
                    }
                }
            }
            return errors;
        }
    }

    /**
     * FieldValidator for:
     *
     * Student Hispanic Ethnicity Indicator,
     * Student Ethnicity Missing Indicator
     * <LI>If Student Hispanic Ethnicity Indicator is populated then Student Ethnicity Missing
     * Indicator must be equal to N or blank.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateEthnicity implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();

            boolean hispEthn = student.getPerson().getHispanicLatinoIndicator();

            if ((BooleanAsStringConverter.TRUE.equals(value) && hispEthn == true) ||
                    (BooleanAsStringConverter.FALSE.equals(value) && hispEthn == false)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Student Hispanic Ethnicity Indicator is populated then" +
                                " Student Ethnicity Missing Indicator must be" +
                                " equal to N or blank",
                        "Student Ethnicity Missing Indicator = " + STYLE_BOLD + value +
                                STYLE_END +
                                " Student Hispanic Ethnicity Indicator = " + STYLE_BOLD +
                                hispEthn + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * FieldValidator for:
     *
     * Student Initial US School Enrollment Date
     * <LI>If (Student Grade Level Code = PS thru 12, UE, or US) AND If Student Birth Country Code
     * not equal to US or PR or English Language Acquisition Status = EL then this field is required
     * <LI>Must be greater than Student Birth Date
     * <LI>Must be less than or equal to current date plus six months.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateInitialUSDate implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();

            PlainDate birthDate = student.getPerson().getDob();
            PlainDate currentDate = m_reportDate;
            PlainDate currentDatePlusSixMonth = DateUtils.add(currentDate, Calendar.MONTH, 6);

            String date = (String) student.getFieldValueByBeanPath(m_fieldInitUSDate);
            DateAsStringConverter m_converter =
                    (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                            Locale.getDefault(),
                            true);
            PlainDate initUSDate = (PlainDate) m_converter.parseSystemString(date);

            String elaStatusCode = (String) student.getFieldValueByBeanPath(m_fieldELAStatusCode);
            if (elaStatusCode != null) {
                elaStatusCode =
                        data.lookupReferenceCodeByBeanPath(SisStudent.class, m_fieldELAStatusCode, elaStatusCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            String birthCountry = (String) student.getPerson().getFieldValueByBeanPath(m_fieldBirthCountry);
            if (birthCountry != null) {
                birthCountry = data.lookupReferenceCodeByBeanPath(SisPerson.class, m_fieldBirthCountry, birthCountry,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            String gradeLevel = student.getGradeLevel();
            if (gradeLevel != null) {
                gradeLevel =
                        data.lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            if (!StringUtils.isEmpty(gradeLevel) && gradeLevel.matches(MATCH_GRADE_LEVEL_CODE_UEUS)) {

                boolean isCorrectEla = StringUtils.isEmpty(elaStatusCode)
                        ? false
                        : elaStatusCode.matches(MATCH_ELA_STATUS_CODE_EL);

                boolean isCorrectBirthCountry = StringUtils.isEmpty(birthCountry)
                        ? false
                        : !birthCountry.matches(MATCH_BIRTH_COUNTRY_CODE_USPR);

                if ((isCorrectBirthCountry || isCorrectEla) && initUSDate == null) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If (Student Grade Level Code = PS thru 12, UE, or US)"
                                    + " AND If Student Birth Country Code not equal to US or"
                                    + " PR or English Language Acquisition Status = EL then"
                                    + " this field is required",
                            "ELA Status State Code = " + STYLE_BOLD + elaStatusCode
                                    + STYLE_END + " Birth Country Code = "
                                    + STYLE_BOLD + birthDate + STYLE_END
                                    + " Student Grade Level Code = "
                                    + STYLE_BOLD + gradeLevel + STYLE_END));
                }
            }
            if (initUSDate != null) {
                if (!initUSDate.after(birthDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Must be greater than Student Birth Date",
                            "Student Birth Date = "
                                    + STYLE_BOLD + birthDate + STYLE_END
                                    + " Student Initial US School Enrollment Date = "
                                    + STYLE_BOLD + initUSDate + STYLE_END));
                }
                if (initUSDate.after(currentDatePlusSixMonth)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Must be less than or equal to current date plus six months",
                            "Current System Date plus six months = "
                                    + STYLE_BOLD + currentDatePlusSixMonth + STYLE_END
                                    + " Student Initial US School Enrollment Date = "
                                    + STYLE_BOLD + initUSDate + STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * FieldValidator for:
     *
     * Student Initial Ninth Grade Entry Year
     * <LI>If Grade Level = 09, 10, 11, or 12 then this field is required.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateNGEYear implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();

            String gradeLevel = student.getGradeLevel();
            if (gradeLevel != null) {
                gradeLevel =
                        data.lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            boolean isCorrectGradeLevel = StringUtils.isEmpty(gradeLevel)
                    ? false
                    : gradeLevel.matches(MATCH_GRADE_LEVEL_CODE_912);

            if (isCorrectGradeLevel && value == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Grade Level = 09, 10, 11, or 12 then this field is required",
                        "This field value = " + STYLE_BOLD + "NULL" + STYLE_END +
                                " Birth Country Code = " + STYLE_BOLD + gradeLevel +
                                STYLE_END));
            }
            return errors;
        }
    }

    /**
     * FieldValidator for:
     *
     * Student Race 1 Code,
     * Student Race 2 Code,
     * Student Race 3 Code,
     * Student Race 4 Code,
     * Student Race 5 Code,
     * Student Race Missing Indicator
     * <LI>If one or more of the Student Race Codes are populated then Student Race Missing
     * Indicator must be equal to N or blank.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateRace implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();

            if ((m_helper.getRaces(student) != null && BooleanAsStringConverter.TRUE.equals(value)) ||
                    (m_helper.getRaces(student) == null && BooleanAsStringConverter.FALSE.equals(value))) {
                errors.add(new StateReportValidationError(entity, field,
                        "If one or more of the Student Race Codes are populated then Student Race Missing Indicator must be equal to N or blank",
                        "Student Race Codes are populated. Student Race Missing Indicator = " +
                                STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * FieldValidator for:
     *
     * Enrolled in US School less than Three Cumulative Years Indicator
     * <LI>If (Student Grade Level Code = PS thru 12, UE, or US) AND If Student Birth Country Code
     * not equal to US or PR then this field is required.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateTYCIndicator implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();

            String birthCountry = (String) student.getPerson().getFieldValueByBeanPath(m_fieldBirthCountry);
            if (birthCountry != null) {
                birthCountry = data.lookupReferenceCodeByBeanPath(SisPerson.class, m_fieldBirthCountry, birthCountry,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            String gradeLevel = student.getGradeLevel();
            if (gradeLevel != null) {
                gradeLevel =
                        data.lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            boolean isCorrectGradeLevel = StringUtils.isEmpty(gradeLevel)
                    ? false
                    : gradeLevel.matches(MATCH_GRADE_LEVEL_CODE_UEUS);

            if (isCorrectGradeLevel) {
                if (!birthCountry.matches(MATCH_BIRTH_COUNTRY_CODE_USPR) && value == null) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If (Student Grade Level Code = PS thru 12, UE, or US) AND If Student "
                                    +
                                    "Birth Country Code not equal to US or PR then this field is required",
                            "This field value = " + STYLE_BOLD + "NULL" + STYLE_END +
                                    " Birth Country Code = " + STYLE_BOLD + birthCountry +
                                    STYLE_END +
                                    " Grade Level Code = " + STYLE_BOLD + gradeLevel +
                                    STYLE_END));
                }
            }

            return errors;
        }
    }

    protected static final String ALIAS_BIRTH_COUNTRY = "DOE BIRTH COUNTRY";
    protected static final String ALIAS_DISTRICT_HOME = "DOE DISTRICT HOME";
    protected static final String ALIAS_ELA_START_DATE = "DOE ELA START DATE";
    protected static final String ALIAS_ELA_STATUS_CODE = "DOE ELA STATUS CODE";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_HIGH_ED_LEVEL = "DOE HIGH ED CD";
    protected static final String ALIAS_INITIAL_US_DATE = "DOE INITIAL US DATE";
    protected static final String ALIAS_MISSING_ETHNICITY_IND = "DOE MISSING ETHNICITY IND";
    protected static final String ALIAS_PRIMARY_LANGUAGE = "DOE PRIMARY LANGUAGE";
    protected static final String ALIAS_PRIMARY_RES_CAT = "DOE PRIMARY RES CAT";
    protected static final String ALIAS_RACE_PRIORITY = "race-priority";
    protected static final String ALIAS_SSID = "DOE SASID";

    /*
     * Match constants
     */
    protected static final String MATCH_BIRTH_COUNTRY_CODE_USPR = "^(US|PR)$";
    protected static final String MATCH_ELA_STATUS_CODE_EL = "EL";
    protected static final String MATCH_ELA_STATUS_CODE_EO = "EO";
    protected static final String MATCH_ELA_STATUS_CODE_ERI = "^(EL|RFEP|IFEP)$";
    protected static final String MATCH_ELA_STATUS_CODE_RFEP = "RFEP";
    protected static final String MATCH_GRADE_LEVEL_CODE_912 = "^(09|10|11|12)$";
    protected static final String MATCH_GRADE_LEVEL_CODE_AD = "AD";
    protected static final String MATCH_GRADE_LEVEL_CODE_UEUS = "^(UE|US)$";
    protected static final String MATCH_PRIMARY_LANGUAGE_CODE_0037 = "^(00|37)$";
    protected static final String MATCH_PRIMARY_RES_CAT = "^1[0-3]0$|^310$";

    /*
     * Constants for history helper parameters from user input template.
     */
    protected static final String PARAM_ALL_SCHOOLS = "allSchools";
    protected static final String PARAM_REPORT_DATE = "reportDate";

    protected static final String PARAM_SCHOOL_OIDS = "schoolOids";

    private static final String INPUT_PARAM_WITHOUT_SSID = "withoutSsid";

    /*
     * Instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_fieldBirthCountry;
    protected String m_fieldDistrictHome;
    protected String m_fieldELAStartDate;
    protected String m_fieldELAStatusCode;
    protected String m_fieldExcludeSchool;
    protected String m_fieldHighEdLevel;
    protected String m_fieldInitUSDate;
    protected String m_fieldSsid;
    protected String m_fieldMissingEthnicityInd;
    protected String m_fieldPrimaryLanguage;
    protected String m_fieldPrimaryResCat;
    protected String m_fieldRacePriority;
    protected List<String> m_gradeCodes = null;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;
    protected Map<String, Collection<StudentContact>> m_studentContactsMap;
    protected Boolean m_isWithoutSsid;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {

        initializeFields();
        initializeGradeCodes();

        /*
         * Build helper object.
         */
        m_isWithoutSsid = (Boolean) getParameter(INPUT_PARAM_WITHOUT_SSID);
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        }
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getParameter(PARAM_REPORT_DATE));

        // Create studentContacts selection criteria
        X2Criteria studentContactsCriteria = new X2Criteria();

        // get student criteria
        X2Criteria studentCriteria = m_helper.getStudentCriteria();

        if (m_isWithoutSsid.booleanValue()) {
            studentCriteria.addEmpty(m_fieldSsid, getBroker().getPersistenceKey());
        }
        if (!m_gradeCodes.isEmpty()) {
            m_helper.getStudentCriteria().addIn(SisStudent.COL_GRADE_LEVEL, m_gradeCodes);
        }

        studentCriteria.addIn(SisStudent.COL_SCHOOL_OID, getSchoolSubQuery());

        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        studentContactsCriteria.addIn(StudentContact.COL_STUDENT_OID, studentSubQuery);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            QueryByCriteria studentContactsQuery = new QueryByCriteria(StudentContact.class, studentContactsCriteria);
            studentContactsQuery.addOrderBy(StudentContact.COL_STUDENT_OID, true);
            m_studentContactsMap = getBroker().getGroupedCollectionByQuery(studentContactsQuery,
                    StudentContact.COL_STUDENT_OID, 5000);

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));

            setEntityClass(CAStudentInformationEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STD-CONTACTS", new RetrieveContacts());
            calcs.put("STD-ENROLLMENT", new RetrieveEnrollment());
            calcs.put("STD-RACES", new RetrieveRaces());
            calcs.put("STD-TYCIND", new RetrieveTYCInd());
            super.addCalcs(calcs);

            // Build a map of validators
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("STD-ADDRESS-VAL", new ValidateAddress());
            validators.put("STD-BIRTHDATE-VAL", new ValidateBirthDate());
            validators.put("STD-ELATEST-VAL", new ValidateELATest());
            validators.put("STD-ENDDATE-VAL", new ValidateEndDate());
            validators.put("STD-ETHNICITY-VAL", new ValidateEthnicity());
            validators.put("STD-INITUSDATE-VAL", new ValidateInitialUSDate());
            validators.put("STD-NGEYEAR-VAL", new ValidateNGEYear());
            validators.put("STD-RACE-VAL", new ValidateRace());
            validators.put("STD-TYCIND-VAL", new ValidateTYCIndicator());
            validators.put("STD-EMP-VAL", new ValidateEmptyValue());
            super.addValidators(validators);
        }
    }


    /**
     * Get the integer priority for this race or 0 if not defined.
     *
     * @param race Race
     * @return priority
     */
    int getRacePriority(Race race) {
        int value = 0;
        if (m_fieldRacePriority != null) {
            String priority = (String) race.getFieldValueByBeanPath(m_fieldRacePriority);
            try {
                value = Integer.parseInt(priority);
            } catch (NumberFormatException e) {
                // retain 0 if format exception
            }
        }
        return value;
    }

    /**
     * Returns the StudentContacts records for the student.
     *
     * @param student SisStudent
     * @return List containing the records for the student
     */
    protected Collection<StudentContact> getStudentContacts(SisStudent student) {
        return m_studentContactsMap.get(student.getOid());
    }

    /**
     * Returns only selected active schools.
     *
     * @return Sub query
     */
    private SubQuery getSchoolSubQuery() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(School.COL_INACTIVE_INDICATOR, BooleanAsStringConverter.FALSE);
        schoolCriteria.addEqualTo(School.COL_ARCHIVE_INDICATOR, BooleanAsStringConverter.FALSE);

        Boolean allSchools = (Boolean) getParameter(PARAM_ALL_SCHOOLS);
        if (allSchools == null || !allSchools.booleanValue()) {
            String schoolOids = (String) getParameter(PARAM_SCHOOL_OIDS);
            if (schoolOids != null && !schoolOids.isEmpty()) {
                List<String> schoolOidList = Arrays.asList(schoolOids.split(","));
                schoolCriteria.addInIgnoreCase(X2BaseBean.COL_OID, schoolOidList);
            }
        }

        return new SubQuery(School.class, X2BaseBean.COL_OID, schoolCriteria);
    }

    /**
     * initialize grade codes into m_gradeCodes where exist state code and disable indicator is not
     * true.
     */
    private void initializeGradeCodes() {
        if (m_gradeCodes == null) {
            m_gradeCodes = new ArrayList<String>();
            DataDictionaryField gradeField = getDataDictionaryField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
            String referenceTableOid = null;
            if (gradeField != null) {
                referenceTableOid = gradeField.getReferenceTableOid();
            }

            if (!StringUtils.isEmpty(referenceTableOid)) {
                X2Criteria refCodeCriteria = new X2Criteria();
                refCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
                refCodeCriteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
                refCodeCriteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());

                QueryIterator iterator =
                        getBroker().getIteratorByQuery(new QueryByCriteria(ReferenceCode.class, refCodeCriteria));
                try {
                    while (iterator.hasNext()) {
                        ReferenceCode refCode = (ReferenceCode) iterator.next();
                        m_gradeCodes.add(refCode.getCode());
                    }
                } finally {
                    iterator.close();
                }

            }

        }
    }


    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldBirthCountry = translateAliasToJavaName(ALIAS_BIRTH_COUNTRY, true);
        m_fieldDistrictHome = translateAliasToJavaName(ALIAS_DISTRICT_HOME, true);
        m_fieldELAStartDate = translateAliasToJavaName(ALIAS_ELA_START_DATE, true);
        m_fieldELAStatusCode = translateAliasToJavaName(ALIAS_ELA_STATUS_CODE, true);
        m_fieldExcludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldHighEdLevel = translateAliasToJavaName(ALIAS_HIGH_ED_LEVEL, true);
        m_fieldInitUSDate = translateAliasToJavaName(ALIAS_INITIAL_US_DATE, true);
        m_fieldSsid = translateAliasToJavaName(ALIAS_SSID, true);
        m_fieldMissingEthnicityInd = translateAliasToJavaName(ALIAS_MISSING_ETHNICITY_IND, true);
        m_fieldPrimaryLanguage = translateAliasToJavaName(ALIAS_PRIMARY_LANGUAGE, true);
        m_fieldPrimaryResCat = translateAliasToJavaName(ALIAS_PRIMARY_RES_CAT, true);
        m_fieldRacePriority = translateAliasToJavaName(ALIAS_RACE_PRIORITY, false);
    }
}
