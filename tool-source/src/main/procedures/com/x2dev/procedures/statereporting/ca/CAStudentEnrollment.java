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
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export Data Module for CA SSID Enrollment.
 *
 * @author X2 Development Corporation
 */
public class CAStudentEnrollment extends StateReportData {
    /**
     * Entity class for CA SSID Enrollment export.
     *
     */
    public static class CASSIDEnrollmentEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        CAStudentEnrollment m_data;
        List<StudentEnrollmentSpan> m_enrollments;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CASSIDEnrollmentEntity() {
            // public no argument constructor for dynamic instantiation.
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
            StudentEnrollmentSpan span = getEnrollment();
            if (span != null) {
                name += span.getSchool().getName();
            }

            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (CAStudentEnrollment) data;
            SisStudent std = (SisStudent) bean;
            List<StudentEnrollmentSpan> enrollmentSpans = m_data.m_helper.getStudentEnrollmentSpans(std, true);

            m_enrollments = new ArrayList<StudentEnrollmentSpan>();
            if (!enrollmentSpans.isEmpty()) {
                for (StudentEnrollmentSpan span : enrollmentSpans) {
                    if (span.getSchool() != null
                            && !BooleanAsStringConverter.TRUE
                                    .equals(span.getSchool().getFieldValueByBeanPath(m_data.m_fieldExcludeSchool))) {
                        if (std.getFieldValueByBeanPath(m_data.m_fieldSsid) == null) {
                            if (m_enrollments.isEmpty()) {
                                m_enrollments.add(span);
                            }
                        } else {
                            m_enrollments.add(span);
                        }
                    }
                }
            }

            setRowCount(m_enrollments.size());

        }

        /**
         * Returns the StudentEnrollment record for the current index.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollmentSpan getEnrollment() {
            return m_enrollments.get(getCurrentRow());
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
     * Retrieve data from the student enrollment record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEnrollment implements FieldRetriever {
        private final String PARAM_ENROLLMENT_STATUS = "ENROLLMENT_STATUS";
        private final String PARAM_EXIT_REASON = "EXIT_REASON";
        private final String PARAM_EXPECTED_RECIEVER_SCHOOL_OF_ATTENDANCE = "EXPECTED_RECIEVER_SCHOOL_OF_ATTENDANCE";
        private final String PARAM_LOCAL_RECORD_ID = "LOCAL_RECORD_ID";
        private final String PARAM_MET_ALL_UC_CSU_REQUIREMENTS_INDICATOR = "MET_ALL_UC_CSU_REQUIREMENTS_INDICATOR";
        private final String PARAM_SCHOOL_COMPLETION_STATUS = "SCHOOL_COMPLETION_STATUS";
        private final String PARAM_SCHOOL_EXIT_DATE = "SCHOOL_EXIT_DATE";
        private final String PARAM_SCHOOL_OF_ATTENDANCE = "SCHOOL_OF_ATTENDANCE";
        private final String PARAM_SCHOOL_OF_ATTENDANCE_NPS = "SCHOOL_OF_ATTENDANCE_NPS";
        private final String PARAM_SCHOOL_START_DATE = "SCHOOL_START_DATE";

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
            StudentEnrollmentSpan span = ((CASSIDEnrollmentEntity) entity).getEnrollment();
            Object value = null;

            if (span != null) {
                if (PARAM_LOCAL_RECORD_ID.equals(parameter)) {
                    value = span.getFirstActiveEnrollment().getOid();
                } else if (PARAM_SCHOOL_OF_ATTENDANCE.equals(parameter)) {
                    if (span.getSchool() != null) {
                        value = span.getSchool().getFieldValueByBeanPath(m_fieldSchoolID);
                    }
                } else if (PARAM_SCHOOL_OF_ATTENDANCE_NPS.equals(parameter)) {
                    value = span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_fieldSchoolAttNPS);
                } else if (PARAM_SCHOOL_START_DATE.equals(parameter)) {
                    value = span.getFirstActiveEnrollment().getEnrollmentDate();
                } else if (PARAM_SCHOOL_EXIT_DATE.equals(parameter)) {
                    if (span.getFirstInactiveEnrollment() != null) {
                        value = span.getFirstInactiveEnrollment().getEnrollmentDate();
                    }
                } else if (PARAM_ENROLLMENT_STATUS.equals(parameter)) {
                    String enrollStatus =
                            (String) span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_fieldEnrollStatus);
                    value = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldEnrollStatus,
                            enrollStatus,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (PARAM_EXIT_REASON.equals(parameter)) {
                    if (span.getFirstInactiveEnrollment() != null) {
                        String exitReason = span.getFirstInactiveEnrollment().getEnrollmentCode();
                        if (!StringUtils.isEmpty(exitReason)) {
                            value = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                    StudentEnrollment.COL_ENROLLMENT_CODE,
                                    exitReason,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        }
                    }
                } else if (PARAM_SCHOOL_COMPLETION_STATUS.equals(parameter)) {
                    if (span.getFirstInactiveEnrollment() != null) {
                        String completionStatus = (String) span.getFirstInactiveEnrollment()
                                .getFieldValueByBeanPath(m_fieldEnrollCompleteStatus);
                        if (!StringUtils.isEmpty(completionStatus)) {
                            value = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                    m_fieldEnrollCompleteStatus,
                                    completionStatus,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        }
                    }
                } else if (PARAM_EXPECTED_RECIEVER_SCHOOL_OF_ATTENDANCE.equals(parameter)) {
                    if (span.getFirstInactiveEnrollment() != null) {
                        value = span.getFirstInactiveEnrollment().getFieldValueByBeanPath(m_fieldReceiverSchool);
                    }
                } else if (PARAM_MET_ALL_UC_CSU_REQUIREMENTS_INDICATOR.equals(parameter)) {
                    if (span.getFirstInactiveEnrollment() != null) {
                        value = span.getFirstInactiveEnrollment().getFieldValueByBeanPath(m_fieldMeetsUcCsuReq);
                    }
                }
            }
            return value;
        }

    }

    /**
     * Validate values of:
     *
     * 1.17 Student Birth Date
     * If Grade Level Code is equal to Adult (AD) Then Student age should be greater than or equal
     * to 16 and less than 80
     * Else, Student Age should be greater than 0 and less than or equal to 22
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
            String gradeLevelCode = entity.getFieldValue(DATA_FIELD_GRADE_LEVEL);
            if (gradeLevelCode == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "Field name " + DATA_FIELD_GRADE_LEVEL + " is missing",
                        ""));
            } else {
                SisStudent student = (SisStudent) entity.getBean();
                int age = student.getPerson().getAge();
                if (gradeLevelCode.matches(MATCH_GRADE_LEVEL_AD)) {
                    if (age < 16 || age >= 80) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Student age should be greater than or equal to 16 and less than 80",
                                "Grade Level Code = " + STYLE_BOLD + gradeLevelCode +
                                        STYLE_END + "  Age = " + STYLE_BOLD + age +
                                        STYLE_END));
                    }
                } else {
                    if (age <= 0 || age > 22) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Student Age should be greater than 0 and less than or equal to 22",
                                "Grade Level Code = " + STYLE_BOLD + gradeLevelCode +
                                        STYLE_END + "  Age = " + STYLE_BOLD + age +
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
     * District of Geographic Residence
     *
     * <LI>If Interdistrict Transfer Code not null then this field is required .
     *
     * @author X2 Development Corporation
     */
    protected class ValidateDistrictofGeogrResid implements FieldValidator {

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

            if (student != null) {
                Object intTransf = student.getFieldValueByBeanPath(m_fieldGeoRes);

                if (intTransf != null && StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If Interdistrict Transfer Code not null then this field is required",
                            "Interdistrict Transfer Code = " + STYLE_BOLD +
                                    (String) intTransf + STYLE_END +
                                    " District of Geographic Residence = " + STYLE_BOLD +
                                    "NULL" + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validate values of:
     *
     * 1.29 Expected Receiver School of Attendance
     * If Student Exit Reason Code = T165 (TransEnrollDiscip) Then this field is required
     *
     * @author X2 Development Corporation
     */
    protected class ValidateExpectReceivSchAtten implements FieldValidator {

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
            String exitReason = entity.getFieldValue(DATA_FIELD_EXIT_REASON);
            if (exitReason == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "Field name " + DATA_FIELD_EXIT_REASON + " is missing",
                        ""));
            } else {
                if (exitReason.matches(MATCH_EXIT_REASON_T165) && value.isEmpty()) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If Student Exit Reason = T165 Then Expected Receiver School of Attendance must be populated",
                            "Exit Reason " + STYLE_BOLD + exitReason + STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * Validate values of:
     *
     * 1.25 Grade Level Code
     * If SSID is null Then this field is required
     *
     * @author X2 Development Corporation
     */
    protected class ValidateGradeLvlCode implements FieldValidator {

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
            String gradeLevelCode = value;
            String ssid = entity.getFieldValue(DATA_FIELD_SSID);
            if (ssid == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "Field name " + DATA_FIELD_SSID + " is missing",
                        ""));
            } else {
                if (ssid.isEmpty() && gradeLevelCode.isEmpty()) {
                    errors.add(new StateReportValidationError(entity, field,
                            "This field is required if SSID is empty ", ""));
                }
            }
            return errors;
        }
    }

    /**
     * Validate values of:
     *
     * If "Student Golden State Seal Merit Diploma Indicator" = "Y" then
     * "Student School Completion Status" = "100" (Field Position 280) then "Y" Else "N".
     *
     * @author X2 Development Corporation
     */
    protected class ValidateIndicators implements FieldValidator {

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
            String schoolCompletionStatus = entity.getFieldValue(DATA_FIELD_SCHOOL_COMPLETION);
            if ((!"Y".equals(value) && "100".equals(schoolCompletionStatus))
                    || (!"N".equals(value) && !"100".equals(schoolCompletionStatus))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Wrong value = " + value + "; " + DATA_FIELD_SCHOOL_COMPLETION + " = " + schoolCompletionStatus,
                        "When " + DATA_FIELD_SCHOOL_COMPLETION + " is " + schoolCompletionStatus + " then "
                                + field.getFieldId() + " should not be " + value));
            }

            return errors;
        }
    }

    /**
     * Validate values of:
     *
     * 1.30 Student Met all UC CSU Requirements Indicator
     * If Student School Completion Status = 100 (Graduated), 106 (Grad, CAHSEE Waiver), or 108
     * (Grad, CAHSEE Exempt) Then this field is required
     *
     * @author X2 Development Corporation
     */
    protected class ValidateMetUCCSUIndicator implements FieldValidator {

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
            String schoolCopletionStatus = entity.getFieldValue(DATA_FIELD_SCHOOL_COMPLETION);
            if (schoolCopletionStatus == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "Field name " + DATA_FIELD_SCHOOL_COMPLETION + " is missing",
                        ""));
            } else {
                if (schoolCopletionStatus.matches(MATCH_SCHOOL_COMPLETION) && value.isEmpty()) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If School Completion Status = 100, 106, 108 Then Student Met all UC CSU Requirements Indicator must be populated",
                            "School Completion Status " + STYLE_BOLD +
                                    schoolCopletionStatus + STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * Validate values of:
     *
     * 1.22 Primary Language Code
     * IF English Language Acquisition Status Code (2.41) equal to EL, RFEP, or IFEP THEN Primary
     * Language Code <> 00 (English) and <> 37 (Sign Language)
     *
     * @author X2 Development Corporation
     */
    protected class ValidatePrimaryLanguage implements FieldValidator {

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
            String primLang = (String) student.getFieldValueByBeanPath(m_fieldPrimaryLanguage);

            if (StringUtils.isEmpty(primLang)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
                return errors;
            }

            if (elaStatusCode != null && primLang != null) {
                elaStatusCode =
                        data.lookupReferenceCodeByBeanPath(SisStudent.class, m_fieldELAStatusCode, elaStatusCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                primLang = data.lookupReferenceCodeByBeanPath(SisStudent.class, m_fieldPrimaryLanguage, primLang,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                boolean isCorrectEla = elaStatusCode == null
                        ? false
                        : elaStatusCode.matches(MATCH_ELA_STATUS_CODE);

                boolean isCorrectLanguage = primLang == null
                        ? false
                        : primLang.matches(MATCH_PRIMARY_LANG);

                if (isCorrectEla && isCorrectLanguage) {
                    errors.add(new StateReportValidationError(entity, field,
                            "IF English Language Acquisition Status Code equal to EL, RFEP, or IFEP then Primary Language Code <> 00 (English) and <> 37 (Sign Language)",
                            "ELA Status Code = " + STYLE_BOLD + elaStatusCode + STYLE_END +
                                    " Primary Language Code = " + STYLE_BOLD + primLang +
                                    STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * Validate values of:
     *
     * 1.28 Student School Completion Status
     * If Student School Completion Status Code is equal to 108 THEN Student School Exit Date must
     * NOT be within the academic year of 2007-2008 or 2008-2009
     * If Student Exit Reason Code = E230 (CompleterExit) Then this field is required
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSchoolCompletion implements FieldValidator {

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
            String exitReason = entity.getFieldValue(DATA_FIELD_EXIT_REASON);
            if (exitReason == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "Field name " + DATA_FIELD_EXIT_REASON + " is missing",
                        ""));
            } else {
                if (exitReason.matches(MATCH_EXIT_REASON_E230) && value.isEmpty()) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If Student Exit Reason = E230 Then Student School Completion Status must be populated",
                            "Exit Reason " + STYLE_BOLD + exitReason + STYLE_END));
                }
            }
            if (value.matches(MATCH_SCHOOL_COMPLETION_108)) {
                if (((CASSIDEnrollmentEntity) entity).getEnrollment() != null
                        && ((CASSIDEnrollmentEntity) entity).getEnrollment().getFirstInactiveEnrollment() != null) {
                    PlainDate exitDate = ((CASSIDEnrollmentEntity) entity).getEnrollment().getFirstInactiveEnrollment()
                            .getEnrollmentDate();
                    PlainDate y2007 = m_schoolYearMap.get(ACADEMIC_YEAR_2007_2008).getStartDate();
                    PlainDate y2009 = m_schoolYearMap.get(ACADEMIC_YEAR_2008_2009).getEndDate();
                    if (exitDate.after(y2007) && exitDate.before(y2009)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "If School Completion Status Code is equal to 108 School Exit Date must NOT be within the academic year of 2007-2008 or 2008-2009",
                                "Exit Date " + STYLE_BOLD + exitDate + STYLE_END));
                    }
                }
            }
            return errors;
        }
    }

    /**
     * Validate values of:
     *
     * 1.26 Student School Exit Date
     * If Student Exit Reason Code = N470 (NoShowOther) THEN Student School Exit Date must be equal
     * to or one day prior to the Student School Start Date
     * ELSE Must be greater than or equal to Student School Start Date
     * If Student School Completion Status Code is equal to 108 THEN Student School Exit Date must
     * NOT be within the academic year of 2007-2008 or 2008-2009
     * If Student Exit Reason Code = N420 THEN Student School Exit Date should be between May 15 and
     * August 15 (inclusive)
     * Must be less than or equal to current date plus 30 days
     * If Student Exit Reason Code is populated Then Student School Exit Date must be populated
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSchoolExitDate implements FieldValidator {

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
            PlainDate startDate =
                    ((CASSIDEnrollmentEntity) entity).getEnrollment().getFirstActiveEnrollment().getEnrollmentDate();
            PlainDate startDateDayPrior = DateUtils.add(startDate, Calendar.HOUR, -24);
            String exitReason = entity.getFieldValue(DATA_FIELD_EXIT_REASON);
            String schoolCopletionStatus = entity.getFieldValue(DATA_FIELD_SCHOOL_COMPLETION);
            if (exitReason == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "Field name " + DATA_FIELD_EXIT_REASON + " is missing",
                        ""));
            }
            if (schoolCopletionStatus == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "Field name " + DATA_FIELD_SCHOOL_COMPLETION + " is missing",
                        ""));
            }

            if (StringUtils.isEmpty(exitReason) ^ StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Student Exit Reason or Student Exit Date is populated Then both must be populated",
                        "Exit Reason " + STYLE_BOLD + exitReason + STYLE_END + " " +
                                "Exit Date" + STYLE_BOLD + value + STYLE_END));
            }

            if (((CASSIDEnrollmentEntity) entity).getEnrollment() != null
                    && ((CASSIDEnrollmentEntity) entity).getEnrollment().getFirstInactiveEnrollment() != null) {
                PlainDate exitDate = ((CASSIDEnrollmentEntity) entity).getEnrollment().getFirstInactiveEnrollment()
                        .getEnrollmentDate();
                if (exitReason != null && exitReason.matches(MATCH_EXIT_REASON_N470)) {
                    if (!(exitDate.equals(startDate) || exitDate.equals(startDateDayPrior))) {
                        errors.add(new StateReportValidationError(entity, field,
                                "if Exit Reason = 'N470'  School Exit Date must be equal to or one day prior to the Student School Start Date",
                                "Exit Date " + STYLE_BOLD + exitDate + STYLE_END +
                                        " Start Date " + STYLE_BOLD + startDate +
                                        STYLE_END));
                    }
                } else {
                    if (!(exitDate.equals(startDate) || exitDate.after(startDate))) {
                        errors.add(new StateReportValidationError(entity, field,
                                "if Exit Reason = 'N470'  School Exit Date must be greater than or equal to Student School Start Date",
                                "Exit Date " + STYLE_BOLD + exitDate + STYLE_END +
                                        " Start Date " + STYLE_BOLD + startDate +
                                        STYLE_END));
                    }
                }

                if (schoolCopletionStatus != null && schoolCopletionStatus.matches(MATCH_SCHOOL_COMPLETION_108)) {
                    PlainDate y2007 = m_schoolYearMap.get(ACADEMIC_YEAR_2007_2008).getStartDate();
                    PlainDate y2009 = m_schoolYearMap.get(ACADEMIC_YEAR_2008_2009).getEndDate();
                    if (exitDate.after(y2007) && exitDate.before(y2009)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "If School Completion Status Code is equal to 108 School Exit Date must NOT be within the academic year of 2007-2008 or 2008-2009",
                                "Exit Date " + STYLE_BOLD + exitDate + STYLE_END));
                    }
                }
                if (exitReason != null && exitReason.matches(MATCH_EXIT_REASON_N420)) {
                    Calendar calendarA = Calendar.getInstance();
                    Calendar calendarB = Calendar.getInstance();
                    calendarA.setTime(m_reportDate);
                    calendarA.set(Calendar.MONTH, Calendar.MAY);
                    calendarA.set(Calendar.DAY_OF_MONTH, 15);
                    calendarB.setTime(m_reportDate);
                    calendarB.set(Calendar.MONTH, Calendar.AUGUST);
                    calendarB.set(Calendar.DAY_OF_MONTH, 15);
                    PlainDate may15 = new PlainDate(calendarA.getTime());
                    PlainDate aug15 = new PlainDate(calendarB.getTime());
                    if (exitDate.before(may15) || exitDate.after(aug15)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "if Exit Reason = 'N420' School Exit Date should be between May 15 and August 15 (inclusive) ",
                                "Exit Date " + STYLE_BOLD + exitDate + STYLE_END));
                    }
                }
                if (exitDate.after(DateUtils.add(m_reportDate, Calendar.DAY_OF_MONTH, 30))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "School Exit Date must be less than or equal to current date plus 30 days",
                            "Exit Date " +
                                    STYLE_BOLD +
                                    exitDate +
                                    STYLE_END +
                                    " Current Date 30 days" +
                                    STYLE_BOLD +
                                    DateUtils.add(m_reportDate, Calendar.DAY_OF_MONTH, 30) +
                                    STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * Validate values of:
     *
     * 1.5 School of Attendance
     * If School of Attendance NPS is populated Then School of Attendance must equal 0000001
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSchoolOfAttend implements FieldValidator {

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
            String schoolOfAttendNPS = entity.getFieldValue(DATA_FIELD_SCHOOL_OF_ATTENDANCE_NPS);
            if (schoolOfAttendNPS == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "Field name " + DATA_FIELD_SCHOOL_OF_ATTENDANCE_NPS +
                                " is missing",
                        ""));
            } else {
                if (!schoolOfAttendNPS.isEmpty() && (value == null || !value.matches(MATCH_SCHOOL_OF_ATTEND))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "This field must be = 0000001 if School of Attendance NPS is populated",
                            "School of Attendance NPS = " + STYLE_BOLD +
                                    schoolOfAttendNPS + STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * Validate values of:
     *
     * 1.6 School of Attendance NPS
     * If School of Attendance = 0000001 Then this field is required
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSchoolOfAttendNPS implements FieldValidator {

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
            String schoolOfAttend = entity.getFieldValue(DATA_FIELD_SCHOOL_OF_ATTENDANCE);
            if (schoolOfAttend == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "Field name " + DATA_FIELD_SCHOOL_OF_ATTENDANCE + " is missing",
                        ""));
            } else {
                if (schoolOfAttend.matches(MATCH_SCHOOL_OF_ATTEND) && value.isEmpty()) {
                    errors.add(new StateReportValidationError(entity, field,
                            "This field is required if School of Attendance = 0000001 ",
                            ""));
                }
            }
            return errors;
        }
    }

    /**
     * Validate values of:
     *
     * 1.23 Student School Start Date
     * Must be greater than Student Birth Date;
     * Must be less than or equal to current date plus six months;
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSchoolStartDate implements FieldValidator {

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
            PlainDate birthDate = ((SisStudent) entity.getBean()).getPerson().getDob();
            if (birthDate == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "Birth date is required",
                        ""));
            } else {
                PlainDate startDate = ((CASSIDEnrollmentEntity) entity).getEnrollment().getFirstActiveEnrollment()
                        .getEnrollmentDate();
                PlainDate currentDatePlusSixMonth = DateUtils.add(m_reportDate, Calendar.MONTH, 6);
                if (!startDate.after(birthDate) || startDate.after(currentDatePlusSixMonth)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Start Date must be greater than Student Birth Date and must be less than or equal to current date plus six months ",
                            "currentDate " + STYLE_BOLD + m_reportDate + STYLE_END +
                                    " start Date = " + STYLE_BOLD + startDate + STYLE_END +
                                    " birthDate = " + STYLE_BOLD + birthDate + STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * Validate values of:
     *
     * 1.8 SSID
     * If Transaction Type Code = D or R Then this field is required
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSSID implements FieldValidator {

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
            String transactionTypeCod = entity.getFieldValue(DATA_FIELD_TRANSACTION_TYPE_COD);
            String ssid = value;
            if (transactionTypeCod == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "Field name " + DATA_FIELD_TRANSACTION_TYPE_COD + " is missing",
                        ""));
            } else {
                if (transactionTypeCod.matches(MATCH_TRANSACTION_TYPE) && StringUtils.isEmpty(ssid)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "This field is required for Transaction Type Code  = D or R",
                            "Transaction Type Code = " + STYLE_BOLD + transactionTypeCod +
                                    STYLE_END));
                }
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

    protected static final String ACADEMIC_YEAR_2007_2008 = "2007-2008";
    protected static final String ACADEMIC_YEAR_2008_2009 = "2008-2009";
    /*
     * Aliases for fields to look up.
     */
    protected static final String ALIAS_ELA_STATUS_CODE = "DOE ELA STATUS CODE";
    protected static final String ALIAS_ENROLL_COMPLETE_STATUS = "DOE ENROLL COMPLETE STATUS";
    protected static final String ALIAS_ENROLL_EXIT_REASON = "DOE ENROLL EXIT REASON";
    protected static final String ALIAS_ENROLL_STATUS = "DOE ENROLL STATUS";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_DISTR_GEO_CODE = "DOE GEO RESIDENCE";
    protected static final String ALIAS_INTERDISTRICT_TRANSFER = "DOE INTERDISTRICT TRANSFER";
    protected static final String ALIAS_MEETS_UC_CSU_REQ = "DOE MEETS UC CSU REQ";
    protected static final String ALIAS_PRIMARY_LANGUAGE = "DOE PRIMARY LANGUAGE";
    protected static final String ALIAS_RECEIVER_SCHOOL = "DOE RECEIVER SCHOOL";
    protected static final String ALIAS_SCHOOL_ATT_NPS = "DOE SCHOOL ATT NPS";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SSID = "DOE SASID";

    protected static final String DATA_FIELD_EXIT_REASON = "ExitReason";
    protected static final String DATA_FIELD_GRADE_LEVEL = "GradeLevel";
    protected static final String DATA_FIELD_SCHOOL_COMPLETION = "SchoolCompletion";
    protected static final String DATA_FIELD_SCHOOL_OF_ATTENDANCE = "SchoolOfAttendance";
    protected static final String DATA_FIELD_SCHOOL_OF_ATTENDANCE_NPS = "SchoolOfAttendNPS";
    protected static final String DATA_FIELD_SSID = "SSID";
    protected static final String DATA_FIELD_TRANSACTION_TYPE_COD = "TransactionTypeCod";

    protected static final String MATCH_ELA_STATUS_CODE = "^(EL|RFEP|IFEP)$";
    protected static final String MATCH_EXIT_REASON_E230 = "^E230$";
    protected static final String MATCH_EXIT_REASON_N420 = "^N420$";
    protected static final String MATCH_EXIT_REASON_N470 = "^N470$";
    protected static final String MATCH_EXIT_REASON_T165 = "^T165$";
    protected static final String MATCH_GRADE_LEVEL_AD = "^AD$";
    protected static final String MATCH_PRIMARY_LANG = "^(00|37)$";
    protected static final String MATCH_SCHOOL_COMPLETION = "^(100|106|108)$";
    protected static final String MATCH_SCHOOL_COMPLETION_108 = "^108$";
    protected static final String MATCH_SCHOOL_OF_ATTEND = "^0000001$";
    protected static final String MATCH_TRANSACTION_TYPE = "^(D|R)$";

    /*
     * Constants for history helper parameters from user input template.
     */
    private static final String PARAM_ALL_SCHOOLS = "allSchools";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_SCHOOLS = "schoolOids";
    protected static final String PARAM_WITHOUT_SSID = "withoutSsid";

    /*
     * Instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_fieldELAStatusCode;
    protected String m_fieldEnrollCompleteStatus;
    protected String m_fieldEnrollExitReason;
    protected String m_fieldEnrollStatus;
    protected String m_fieldExcludeSchool;
    protected String m_fieldGeoRes;
    protected String m_fieldMeetsUcCsuReq;
    protected String m_fieldPrimaryLanguage;
    protected String m_fieldReceiverSchool;
    protected String m_fieldSchoolAttNPS;
    protected String m_fieldSchoolID;
    protected String m_fieldSsid;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;
    protected List<String> m_gradeCodes = null;

    protected Map<String, SisDistrictSchoolYearContext> m_schoolYearMap;

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
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        }
        Boolean isWithoutSsid = (Boolean) getParameter(PARAM_WITHOUT_SSID);
        String schoolOids = (String) getParameter(PARAM_SCHOOLS);
        Boolean isAllSchools = (Boolean) getParameter(PARAM_ALL_SCHOOLS);
        X2Criteria sklCriteria = new X2Criteria();

        if (isAllSchools.booleanValue()) {
            sklCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            sklCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        } else {
            Set<String> setSchoolOids = new HashSet<String>();
            setSchoolOids.addAll(Arrays.asList(schoolOids.split(",")));
            sklCriteria.addIn(X2BaseBean.COL_OID, setSchoolOids);
        }

        sklCriteria.addNotEqualTo(m_fieldExcludeSchool, BooleanAsStringConverter.TRUE);

        SubQuery sklSubQuery = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, sklCriteria);
        m_helper.getStudentCriteria().addIn(Student.COL_SCHOOL_OID, sklSubQuery);

        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        if (isWithoutSsid.booleanValue()) {
            m_helper.getStudentCriteria().addEmpty(m_fieldSsid, getBroker().getPersistenceKey());
        }
        if (!m_gradeCodes.isEmpty()) {
            m_helper.getStudentCriteria().addIn(SisStudent.COL_GRADE_LEVEL, m_gradeCodes);
        }

        // Create schoolYear selection criteria
        X2Criteria schoolYearCriteria = new X2Criteria();
        QueryByCriteria schoolYearQuery = new QueryByCriteria(SisDistrictSchoolYearContext.class, schoolYearCriteria);
        schoolYearQuery.addOrderBy(SisDistrictSchoolYearContext.COL_CONTEXT_ID, true);
        m_schoolYearMap = getBroker().getMapByQuery(schoolYearQuery, SisDistrictSchoolYearContext.COL_CONTEXT_ID, 50);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));

            setEntityClass(CASSIDEnrollmentEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("ENR-ENROLLMENT", new RetrieveEnrollment());
            super.addCalcs(calcs);

            // Build a map of validators
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("ENR-SOA-VAL", new ValidateSchoolOfAttend());
            validators.put("ENR-SOANPS-VAL", new ValidateSchoolOfAttendNPS());
            validators.put("ENR-SSID-VAL", new ValidateSSID());
            validators.put("ENR-BIRTHDATE-VAL", new ValidateBirthDate());
            validators.put("STD-DISTGEORES-VAL", new ValidateDistrictofGeogrResid());
            validators.put("ENR-PRIMLANG-VAL", new ValidatePrimaryLanguage());
            validators.put("ENR-SCHSTARTDATE-VAL", new ValidateSchoolStartDate());
            validators.put("ENR-GRADELVLCODE-VAL", new ValidateGradeLvlCode());
            validators.put("ENR-SCHEXITDATE-VAL", new ValidateSchoolExitDate());
            validators.put("ENR-SCHCOMPLSTTS-VAL", new ValidateSchoolCompletion());
            validators.put("ENR-ERSA-VAL", new ValidateExpectReceivSchAtten());
            validators.put("ENR-MUCI-VAL", new ValidateMetUCCSUIndicator());
            validators.put("STD-RACE-VAL", new ValidateRace());
            validators.put("INDICATORS-VAL", new ValidateIndicators());
            super.addValidators(validators);
        }
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
        m_fieldELAStatusCode = translateAliasToJavaName(ALIAS_ELA_STATUS_CODE, true);
        m_fieldEnrollCompleteStatus = translateAliasToJavaName(ALIAS_ENROLL_COMPLETE_STATUS, true);
        m_fieldEnrollExitReason = translateAliasToJavaName(ALIAS_ENROLL_EXIT_REASON, true);
        m_fieldEnrollStatus = translateAliasToJavaName(ALIAS_ENROLL_STATUS, true);
        m_fieldExcludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldGeoRes = translateAliasToJavaName(ALIAS_DISTR_GEO_CODE, true);
        m_fieldMeetsUcCsuReq = translateAliasToJavaName(ALIAS_MEETS_UC_CSU_REQ, true);
        m_fieldPrimaryLanguage = translateAliasToJavaName(ALIAS_PRIMARY_LANGUAGE, true);
        m_fieldReceiverSchool = translateAliasToJavaName(ALIAS_RECEIVER_SCHOOL, true);
        m_fieldSchoolID = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldSchoolAttNPS = translateAliasToJavaName(ALIAS_SCHOOL_ATT_NPS, true);
        m_fieldSsid = translateAliasToJavaName(ALIAS_SSID, true);

    }
}
