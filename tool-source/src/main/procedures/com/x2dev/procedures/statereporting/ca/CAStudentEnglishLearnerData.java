/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2015 X2 Development Corporation.
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
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;

/**
 * Procedure for Student English Language Acquisition export.
 */
public class CAStudentEnglishLearnerData extends StateReportData {

    /**
     * The Class CAStudentEnglishLearnerEntity.
     */
    public static class CAStudentEnglishLearnerEntity extends StateReportEntity {

        /**
         * Instantiates a new CA student english learner entity.
         */
        public CAStudentEnglishLearnerEntity() {

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
            return name;
        }
    }

    /**
     * 1) IF Primary Language Code <> 00 (English) and <> 37 (Sign Language)
     * THEN English Language Acquisition Status Code (2.41) must equal EL, RFEP, IFEP, or TBD
     * 2) English Language Acquisition Status Code being submitted must be a valid subsequent
     * English Language Acquisition Status Code as compared to the most recent
     * English Language Acquisition Status Code in the ODS as defined in the CALPADS Valid Code
     * Combinations document.
     * See Current ELAS-Submtd ELAS worksheet.
     * 3) If a student's English Language Acquisition Status (ELAS) Code is being submitted = EL,
     * then the most
     * recent ELAS Code currently stored in the CALPADS ODS cannot equal IFEP or RFEP.
     * 4) If no English Language Acquisition Status (ELAS) Code exists in the ODS for a student,
     * then the ELAS code being submitted must equal TBD, EO, IFEP, or EL.
     * 5) IF English Language Acquisition Status Code (2.41) equals EO THEN Primary Language Code
     * must equal 00 (English) or 37 (Sign Language)
     * 6) If an English Language Acquisition Status (ELAS) Code exists in the ODS for a student,
     * then the ELAS code being submitted cannot equal TBD.
     */
    protected class ELAStatusCodeValidator implements FieldValidator {
        public static final String VAL_ID = "ELA_STATUS_CODE";
        private static final String ERROR_INVALID_VALUE = "ELA Status Code is invalid";
        private static final String ERROR_VALUE_IS_EMPTY = "ELA Status Code is empty";
        private static final String RULE_ONE =
                "IF English Language Acquisition Status Code (2.41) equal to EL, RFEP, or IFEP THEN Primary Language Code <>  00 (English) and <> 37 (Sign Language)";
        private static final String RULE_TWO =
                "IF English Language Acquisition Status Code (2.41) equals EO THEN Primary Language Code must equal 00";

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
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String primaryLangCode = entity.getFieldValue(FIELD_PRIMARY_LANG_CODE);

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, ERROR_VALUE_IS_EMPTY, ""));
                return errors;
            }

            if (("00".equals(primaryLangCode) || "37".equals(primaryLangCode)) &&
                    ("EL".equals(value) || "RFEP".equals(value) || "IFEP".equals(value))) {
                String message =
                        "Primary Lang. Code: " + primaryLangCode + ", ELA Status Code: " + value + ". " + RULE_ONE;
                StateReportValidationError error =
                        new StateReportValidationError(entity, field, ERROR_INVALID_VALUE, message);
                errors.add(error);
            }
            if (!"00".equals(primaryLangCode) && "EO".equals(value)) {
                String message =
                        "Primary Lang. Code: " + primaryLangCode + ", ELA Status Code: " + value + ". " + RULE_TWO;
                StateReportValidationError error =
                        new StateReportValidationError(entity, field, ERROR_INVALID_VALUE, message);
                errors.add(error);
            }
            return errors;
        }

    }

    /**
     * 1) Must be greater than Student Birth Date
     * 2) Must be less than or equal to current system date–
     * 3) Must be equal to the English Language Acquisition Status Start Date for the same
     * English Language Acquisition Status Code in the CALPADS ODS.
     */
    protected class ELAStatusStartDateValidator implements FieldValidator {
        public static final String VAL_ID = "ELA_STATUS_START_DATE";
        private static final String ERROR_INVALID_VALUE = "ELA Start Date is invalid";
        private static final String ERROR_VALUE_IS_EMPTY = "ELA Start Date is empty";
        private static final String RULE_ONE = "Must be greater than Student Birth Date";
        private static final String RULE_TWO = "Must be less than or equal to current system date";

        // private static final String RULE_THREE =
        // "Must be equal to the English Language Acquisition Status Start Date for the same English
        // Language Acquisition Status Code in the CALPADS ODS";

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
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, ERROR_VALUE_IS_EMPTY, ""));
                return errors;
            }

            SystemStringConverter converter = field.getConverter();
            PlainDate elaStartDate = (PlainDate) converter.parseSystemString(value);

            SisStudent student = ((StudentProgramParticipation) entity.getBean()).getStudent();
            PlainDate dob = student.getPerson().getDob();
            if (elaStartDate.before(dob)) {
                String message =
                        "ELA Start Date: " + value + ", Student DOB: " + converter.getSystemString(dob) + ". " +
                                RULE_ONE;
                errors.add(new StateReportValidationError(entity, field, ERROR_INVALID_VALUE, message));
            }

            PlainDate currentDate = new PlainDate();
            if (elaStartDate.after(currentDate)) {
                String message =
                        "ELA Start Date: " + value + ", current Date: " + converter.getSystemString(currentDate) +
                                ". " + RULE_TWO;
                errors.add(new StateReportValidationError(entity, field, ERROR_INVALID_VALUE, message));
            }
            return errors;
        }

    }

    /**
     * 1) IF English Language Acquisition Status Code (2.41) equal to EL, RFEP,
     * or IFEP THEN Primary Language Code <> 00 (English) and <> 37 (Sign Language)
     * 2) IF English Language Acquisition Status Code (2.41) equals EO THEN Primary Language Code
     * must equal 00
     */
    protected class PrimaryLanguageValidator implements FieldValidator {
        public static final String VAL_ID = "PRIMARY_CODE";
        private static final String ERROR_INVALID_VALUE = "Primary Lang Code is invalid.";
        private static final String ERROR_VALUE_IS_EMPTY = "Primary Lang Code is empty";
        private static final String RULE_ONE =
                "IF English Language Acquisition Status Code (2.41) equal to EL, RFEP, or IFEP THEN Primary Language Code <>  00 (English) and <> 37 (Sign Language)";
        private static final String RULE_TWO =
                "IF English Language Acquisition Status Code (2.41) equals EO THEN Primary Language Code must equal 00";

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
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = ((StudentProgramParticipation) entity.getBean()).getStudent();
            String gradeLevelCode = data.lookupStateValue(SisStudent.class, SisStudent.COL_GRADE_LEVEL,
                    student.getGradeLevel());
            if ("AD".equals(gradeLevelCode) && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, ERROR_VALUE_IS_EMPTY,
                        "Primary Lang Code is requered then Grade Level Code is AD"));
                return errors;
            }
            String elaStatusCode = entity.getFieldValue(FIELD_ELA_STATUS_CODE);
            if (("00".equals(value) || "37".equals(value)) &&
                    ("EL".equals(elaStatusCode) || "RFEP".equals(elaStatusCode) || "IFEP".equals(elaStatusCode))) {
                String message =
                        "Primary Lang. Code: " + value + ", ELA Status Code: " + elaStatusCode + ". " + RULE_ONE;
                StateReportValidationError error =
                        new StateReportValidationError(entity, field, ERROR_INVALID_VALUE, message);
                errors.add(error);
            }
            if (!"00".equals(value) && "EO".equals(elaStatusCode)) {
                String message =
                        "Primary Lang. Code: " + value + ", ELA Status Code: " + elaStatusCode + ". " + RULE_TWO;
                StateReportValidationError error =
                        new StateReportValidationError(entity, field, ERROR_INVALID_VALUE, message);
                errors.add(error);
            }
            return errors;
        }

    }

    /**
     * FieldValidator for:
     *
     * English Language Acquisition Status Start Date
     *
     * <LI>If English Language Acquisition Status State Code not equal to EO then this field is
     * required
     * <LI>Must be greater than Student Birth Date
     * <LI>Must be less than or equal to current system date
     * <LI>Must be less than or equal to Effective Start Date.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateELAStartDate implements FieldValidator {

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
            // StudentEnrollmentSpan span = ((CAStudentEnglishLearnerEntity)
            // entity).getEnrollmentSpan();
            SisStudent student = (SisStudent) entity.getBean();

            if (student.getPerson() == null) {
                return errors;
            }

            PlainDate birthDate = student.getPerson().getDob();
            if (birthDate == null) {
                return errors;
            }

            String elaStartDateRaw = (String) student.getFieldValueByBeanPath(m_fieldELAStartDate);
            if (StringUtils.isEmpty(elaStartDateRaw)) {
                return errors;
            }

            DateAsStringConverter m_converter =
                    (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                            Locale.getDefault(),
                            true);
            PlainDate elaStartDate = (PlainDate) m_converter.parseSystemString(elaStartDateRaw);
            if (elaStartDate != null) {
                PlainDate currentDate = m_reportDate;
                PlainDate startDate = null;// span.getFirstActiveEnrollment().getEnrollmentDate();
                if (!elaStartDate.after(birthDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Must be greater than Student Birth Date",
                            "Student Birth Date = " + STYLE_BOLD + birthDate + STYLE_END +
                                    " ELA Start Date = " + STYLE_BOLD + elaStartDate +
                                    STYLE_END));
                }
                if (elaStartDate.after(currentDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Must be less than or equal to current system date",
                            "Current System Date = " + STYLE_BOLD + currentDate +
                                    STYLE_END +
                                    " ELA Start Date = " + STYLE_BOLD + elaStartDate +
                                    STYLE_END));
                }
                if (elaStartDate.after(startDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Must be less than or equal to Effective Start Date",
                            "Effective Start Date = " + STYLE_BOLD + startDate +
                                    STYLE_END +
                                    " ELA Start Date = " + STYLE_BOLD + elaStartDate +
                                    STYLE_END));
                }
            }

            String elaStatusCode = (String) student.getFieldValueByBeanPath(m_fieldELAStatus);
            if (StringUtils.isEmpty(elaStatusCode)) {
                return errors;
            }

            if (elaStatusCode != null) {
                elaStatusCode = data.lookupReferenceCodeByBeanPath(SisStudent.class,
                        m_fieldELAStatus,
                        elaStatusCode,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                boolean isCorrectEla = StringUtils.isEmpty(elaStatusCode)
                        ? false
                        : !elaStatusCode.matches(MATCH_ELA_STATUS_CODE_EO);

                if (isCorrectEla && elaStartDate == null) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If English Language Acquisition Status State Code not equal to EO then this field is required",
                            "ELA Status State Code = " + STYLE_BOLD + elaStatusCode +
                                    STYLE_END +
                                    " ELA Start Date = " + STYLE_BOLD + "NULL" + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * FieldValidator for:
     *
     * Primary Language Code,
     * English Language Acquisition Status Code
     * <LI>IF English Language Acquisition Status Code equal to EL, RFEP, or IFEP then Primary
     * Language Code <> 00 (English) and <> 37 (Sign Language).
     *
     * @author X2 Development Corporation
     */
    protected class ValidatePrimaryLanguageCode implements FieldValidator {

        public static final String VAL_ID = "STD-PRIMLANG-VAL";

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

            String elaStatusCode = (String) student.getFieldValueByBeanPath(m_fieldELAStatus);
            String primLang = (String) student.getFieldValueByBeanPath(m_fieldPrimaryLanguage);

            if (StringUtils.isEmpty(primLang)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
                return errors;
            }

            if (elaStatusCode == null || primLang == null) {
                return errors;
            }

            elaStatusCode = data.lookupReferenceCodeByBeanPath(SisStudent.class, m_fieldELAStatus, elaStatusCode,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            primLang = data.lookupReferenceCodeByBeanPath(SisStudent.class,
                    m_fieldPrimaryLanguage,
                    primLang,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            boolean isCorrectEla = StringUtils.isEmpty(elaStatusCode)
                    ? false
                    : elaStatusCode.matches(MATCH_ELA_STATUS_CODE_ERI);

            boolean isCorrectLanguage = StringUtils.isEmpty(primLang)
                    ? false
                    : primLang.matches(MATCH_PRIMARY_LANGUAGE_CODE_0037);

            if (isCorrectEla && isCorrectLanguage) {
                errors.add(new StateReportValidationError(entity, field,
                        "IF English Language Acquisition Status Code equal to EL,"
                                + " RFEP, or IFEP then Primary Language Code <> 00 (English)"
                                + " and <> 37 (Sign Language)",
                        "ELA Status Code = "
                                + STYLE_BOLD + elaStatusCode + STYLE_END +
                                " Primary Language Code = "
                                + STYLE_BOLD + primLang + STYLE_END));

            }
            return errors;
        }
    }

    protected static final String ALIAS_ELA_START_DATE = "DOE ELA START DATE";
    protected static final String ALIAS_ELA_STATUS = "DOE ELA STATUS CODE";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_PRIMARY_LANGUAGE = "DOE PRIMARY LANGUAGE";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SSID = "DOE SASID";

    protected static final String MATCH_ELA_STATUS_CODE_EL = "EL";
    protected static final String MATCH_ELA_STATUS_CODE_EO = "EO";
    protected static final String MATCH_ELA_STATUS_CODE_ERI = "^(EL|RFEP|IFEP)$";
    protected static final String MATCH_ELA_STATUS_CODE_RFEP = "RFEP";
    protected static final String MATCH_GRADE_LEVEL_CODE_912 = "^(09|10|11|12)$";
    protected static final String MATCH_GRADE_LEVEL_CODE_AD = "AD";
    protected static final String MATCH_GRADE_LEVEL_CODE_UEUS = "^(UE|US)$";
    protected static final String MATCH_PRIMARY_LANGUAGE_CODE_0037 = "^(00|37)$";

    private static final String FIELD_ELA_STATUS_CODE = "ELA Status Code";
    private static final String FIELD_PRIMARY_LANG_CODE = "Primary Lang. Code";

    private static final String INPUT_PARAM_REPORT_DATE = "reportDate";
    private static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_PARAM_SCHOOLS = "schoolOids";
    private static final String INPUT_PARAM_WITHOUT_SSID = "withoutSsid";

    protected String m_fieldELAStatus;
    protected String m_fieldExcludeSchool;
    protected String m_fieldPrimaryLanguage;
    protected String m_fieldELAStartDate;
    protected String m_fieldSchoolId;
    protected String m_fieldSsid;

    protected PlainDate m_reportDate;

    private StudentHistoryHelper m_helper;

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
        if (getSetupErrors().size() != 0) {
            return;
        }

        initializeEntityQuery();
        setEntityClass(CAStudentEnglishLearnerEntity.class);

        // Build a map of validators
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(ELAStatusCodeValidator.VAL_ID, new ELAStatusCodeValidator());
        validators.put(ELAStatusStartDateValidator.VAL_ID, new ELAStatusStartDateValidator());
        validators.put(ValidatePrimaryLanguageCode.VAL_ID, new ValidatePrimaryLanguageCode());
        super.addValidators(validators);

    }

    /**
     * Returns student enrollment spans.
     *
     * @param student SisStudent
     * @return List
     */
    protected List<StudentEnrollmentSpan> getStudentEnrollentSpans(SisStudent student) {
        return m_helper.getStudentEnrollmentSpans(student, true);
    }

    /**
     * Initializes query.
     */
    private void initializeEntityQuery() {
        // get student criteria
        Boolean isWithoutSsid = (Boolean) getParameter(INPUT_PARAM_WITHOUT_SSID);
        String schoolOids = (String) getParameter(INPUT_PARAM_SCHOOLS);
        Boolean isAllSchools = (Boolean) getParameter(INPUT_PARAM_ALL_SCHOOLS);
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

        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        studentCriteria.addIn(Student.COL_SCHOOL_OID, sklSubQuery);

        if (isWithoutSsid.booleanValue()) {
            studentCriteria.addEmpty(m_fieldSsid, getBroker().getPersistenceKey());
        }

        studentCriteria.addNotEmpty(m_fieldELAStatus, getBroker().getPersistenceKey());
        setQuery(m_helper.getStudentQuery(false));
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_reportDate = (PlainDate) getParameter(INPUT_PARAM_REPORT_DATE);

        m_fieldELAStatus = translateAliasToJavaName(ALIAS_ELA_STATUS, true);
        m_fieldELAStartDate = translateAliasToJavaName(ALIAS_ELA_START_DATE, true);
        m_fieldExcludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldPrimaryLanguage = translateAliasToJavaName(ALIAS_PRIMARY_LANGUAGE, true);
        m_fieldSchoolId = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldSsid = translateAliasToJavaName(ALIAS_SSID, true);
        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

    }
}
