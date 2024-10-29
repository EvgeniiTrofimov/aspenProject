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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentMultiYearHelper;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for student extract export.
 */
public class TNStudentExtractData extends TNEnrollReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for student extract export.
     *
     */
    public static class TNStudentExtractEntity extends TNStateReportEntity
            implements TNStateReportData.HasStudentRecordHelper {

        private List<StudentRecordHelper> m_records;
        private HashMap<String, StudentRecordHelper> m_recordsMap;
        private TNStudentExtractData m_tnData;

        /**
         * Instantiates a new TN student extract entity.
         */
        public TNStudentExtractEntity() {
            // Public no argument constructor for dynamic instantiation.
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
                    ", SASID: " + student.getFieldValueByAlias(ALIAS_STD_STATE_ID) +
                    "] ";
            return name;
        }

        /**
         * @see com.x2dev.procedures.statereporting.tn.TNStateReportData.HasStudentRecordHelper#getCurrentRecord()
         */
        @Override
        public StudentRecordHelper getCurrentRecord() {
            return m_records.get(getCurrentRow());
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

            m_tnData = (TNStudentExtractData) data;
            SisStudent student = (SisStudent) getBean();
            List<StudentRecordHelper> list = m_tnData.getStudentHelperMap().get(student.getOid());
            if (list != null) {
                m_recordsMap = new HashMap<String, StudentRecordHelper>();
                // Extract one row per schoolID/instrPgm.
                for (StudentRecordHelper item : list) {
                    String key = String.valueOf(item.getSchoolId()) + String.valueOf(item.getInstrProgram());
                    m_recordsMap.put(key, item);
                }
                m_records = new ArrayList(m_recordsMap.values());

                setRowCount(m_records.size());
            } else {
                setRowCount(0);
            }

            m_tnData.addEntityRowsCount(getRowCount());
        }
    }

    /**
     *
     * Field retriever for SSN.
     * This retriever normalizes SSN value to format 999999999 (nine digits without any other
     * characters)
     *
     */
    protected class FieldRetrieverSSN implements FieldRetriever {
        protected static final String STD_CALC_ID = "STD_CALC_SSN";

        private static final String CALC_PARAM_PREVSSN = "PREVSSN";
        private static final String CALC_PARAM_SSN = "SSN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentExtractEntity seEntity = (TNStudentExtractEntity) entity;
            SisStudent student = (SisStudent) seEntity.getBean();
            Person psn = student.getPerson();
            String param = (String) field.getParameter();

            String ssn = "";

            if (psn != null) {
                if (CALC_PARAM_SSN.equalsIgnoreCase(param)) {
                    ssn = psn.getPersonId();
                }

                if (CALC_PARAM_PREVSSN.equalsIgnoreCase(param)) {
                    ssn = (String) psn.getFieldValueByBeanPath(m_fieldDoePrevSSN);
                }
            }

            return StringUtils.isEmpty(ssn) ? ssn : ssn.replaceAll("([^\\d]?)", "");
        }

    }

    /**
     * Field validator for SSN.
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999
     */
    protected class FiledValidatorSSN implements FieldValidator {
        protected static final String STD_VAL_ID = "STD_VAL_SSN";
        private static final String patternSSN = "^[0-9]{9}$|^$";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (value != null && !value.matches(patternSSN)) {
                errors.add(new StateReportValidationError(entity, field, "Invalid format",
                        "SSN value must be 9 digits long"));
            }

            return errors;
        }

    }

    /**
     * Field retriever for Native language field.
     */
    protected class RetrieveNativeLanguage implements FieldRetriever {
        public static final String CALC_ID_NLANG = "STD_CALC_NLANG";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String value = null;
            String nativeLang = (String) student.getFieldValueByBeanPath(m_fieldNlng);
            if (!StringUtils.isEmpty(nativeLang)) {
                value = data.lookupStateValue(SisStudent.class, m_fieldNlng, nativeLang);
            }
            if (!isReport()) {
                value = StringUtils.isEmpty(value) ? "ENG" : value;
            }
            return value;
        }
    }


    /**
     * Field retriever for Latino indicator.
     */
    protected class RetrievePersonInfo implements FieldRetriever {
        public static final String CALC_ID_PERSONINFO = "STD_CALC_PERSONINFO";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStudent student = (SisStudent) entity.getBean();
            Person studentPerson = student.getPerson();

            if (studentPerson == null) {
                return "N";
            }

            return studentPerson.getHispanicLatinoIndicator() ? "H" : "N";
        }
    }

    /**
     * Field retriever for School Year field.
     */
    protected class RetrieveSchool implements FieldRetriever {
        public static final String CALC_ID_SCHOOL = "STD_CALC_SCHOOL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentExtractEntity seEntity = (TNStudentExtractEntity) entity;
            return seEntity.getCurrentRecord().getSchoolId();
        }
    }

    /**
     * Field retriever for School Year field.
     */
    protected class RetrieveSchoolYear implements FieldRetriever {
        public static final String CALC_ID_SCHOOLYEAR = "STD_CALC_SCHOOLYEAR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentExtractData seData = (TNStudentExtractData) data;
            return seData.m_schoolYearByParam;
        }
    }

    /**
     * Retrieve YEAR NINTH GRADE from bean by alias and set it to zeroes if it doesn't exist or
     * match pattern.
     */
    protected class RetrieveYearNinthGrade implements FieldRetriever {
        protected static final String CALC_ID_YEAR_NINTH = "CALC_YEAR_NINTH";
        protected final Pattern s_pattern = Pattern.compile(".?([1-9]\\d{3}).?");

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();

            String ninthGrade = (String) student.getFieldValueByBeanPath(m_fieldNinthGRade);

            if (ninthGrade != null) {
                Matcher matcher = s_pattern.matcher(ninthGrade);
                if (matcher.find()) {
                    ninthGrade = matcher.group(1);
                } else {
                    ninthGrade = "0000";
                }
            } else {
                ninthGrade = "0000";
            }
            return ninthGrade;
        }

    }

    /**
     * Validate Student.[DOE PIN]
     */
    protected class ValidatePIN implements FieldValidator {
        protected static final String STD_VAL_ID = "STD_VAL_PIN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();
            String ssn = entity.getFieldValue("STUDENT SSN");
            if (StringUtils.isEmpty(value) && StringUtils.isEmpty(ssn)) {
                StateReportValidationError error = new StateReportValidationError(entity, field,
                        "Student SSN and DOE PIN are missing.", "At least one of values must represent.");
                errors.add(error);
            } else if (!StringUtils.isEmpty(value) && !StringUtils.isEmpty(ssn)) {
                StringBuilder errorMessage = new StringBuilder("PIN should exist only if SSN is empty. ");
                errorMessage.append("SSN: ");
                errorMessage.append(ssn);
                errorMessage.append(". DOE PIN: ");
                errorMessage.append(value);

                StateReportValidationError error = new StateReportValidationError(entity, field,
                        "Student SSN and DOE PIN both exist.", errorMessage.toString());
                errors.add(error);
            }
            return errors;
        }
    }

    /**
     * Validate Student.[DOE PIN]
     */
    protected class ValidateDateESL implements FieldValidator {
        protected static final String FIELD_DATE_ESL = "DATE ENR IN ESL";
        protected static final String STD_DATE_ESL = "STD_VAL_DATE_ESL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();

            String dateEsl = entity.getFieldValue(FIELD_DATE_ESL);
            String doeElbValue = (String) getFieldValueByBeanPath(student, m_fieldDoeElb);

            if (!StringUtils.isEmpty(dateEsl) && !VALID_ELB.contains(doeElbValue)) {
                StateReportValidationError error = new StateReportValidationError(entity, field,
                        "DATE ENR IN ESL should be empty",
                        "Value of 1, 2, F, G, or L should be in STUDENT.DOE ELB when DATE ENR IN ESL is not empty.");
                errors.add(error);
            }
            if (StringUtils.isEmpty(dateEsl) && VALID_ELB.contains(doeElbValue)) {
                StateReportValidationError error =
                        new StateReportValidationError(entity, field,
                                "DATE ENR IN ESL should be populated",
                                "When STUDENT.DOE ELB contains a value of 1, 2, F, G, or L then DATE ENR IN ESL must not be empty.");
                errors.add(error);
            }

            return errors;
        }
    }

    /**
     * Validate Student.[DOE DATE FIRST ENTER US SCHOOL]
     */
    protected class ValidateFirstEnr implements FieldValidator {
        protected static final String VAL_ID = "VAL_FIRST_ENR";

        private static final String ALIAS_STD_IMMIGRANT = "DOE IMMIGRANT";

        private Collection<String> m_elbCodes;
        private Collection<String> m_checkedGrades;

        /**
         * Instantiates a new validate first enr.
         */
        public ValidateFirstEnr() {
            m_elbCodes = Arrays.asList("L", "W", "1", "2", "3", "4", "F", "N");
            m_checkedGrades =
                    Arrays.asList("K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12");
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();

            TNStudentExtractEntity seEntity = (TNStudentExtractEntity) entity;

            if (StringUtils.isEmpty(value)) {
                SisStudent student = (SisStudent) entity.getBean();

                ReferenceCode gradeLvlRefCode = getGradeLevel(student, seEntity.getCurrentRecord().getYog());
                if (gradeLvlRefCode == null) {
                    errors.add(
                            new StateReportValidationError(entity, field, "Grade cannot be determined",
                                    "Student " + STYLE_BOLD + student.getNameView() + STYLE_END + " with YOG "
                                            + STYLE_BOLD + seEntity.getCurrentRecord().getYog() + STYLE_END + "."));
                } else {
                    String gradeLvlCode = gradeLvlRefCode.getStateCode();

                    if (m_checkedGrades.contains(gradeLvlCode)) {
                        String immigrantInd = (String) student.getFieldValueByAlias(ALIAS_STD_IMMIGRANT);
                        String elbCode = (String) getFieldValueByBeanPath(student, m_fieldDoeElb);

                        if (BooleanAsStringConverter.TRUE.equals(immigrantInd) ||
                                m_elbCodes.contains(elbCode)) {
                            errors.add(
                                    new StateReportValidationError(entity, field,
                                            "Effective 2017-18, for k-12 students, " +
                                                    "districts MUST enter Date First Enrolled in US School for Student flagged as Immigrant or English Language Background [L, W, 1, 2, 3, 4, F, N].",
                                            "School year = " + STYLE_BOLD + getCurrentContext().getSchoolYear()
                                                    + STYLE_END + ", " +
                                                    "grade level = " + STYLE_BOLD + gradeLvlCode + STYLE_END + ", " +
                                                    "immigrant indicator = " + STYLE_BOLD + immigrantInd + STYLE_END
                                                    + ", " + "ELB code = " + STYLE_BOLD + elbCode
                                                    + STYLE_END));
                        }
                    }
                }
            }

            return errors;
        }
    }

    /**
     * Validate enroll reason.
     */
    protected class ValidateLangBg implements FieldValidator {
        protected static final String VAL_ID = "VAL_LANG_BG";

        private Collection<String> m_elbCodes;

        /**
         * Instantiates a new field validator lang bg.
         */
        public ValidateLangBg() {
            m_elbCodes = Arrays.asList("L", "W", "1", "2", "3", "4", "E", "N", "F");
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();

            if ((!m_elbCodes.contains(value)) || (StringUtils.isEmpty(value))) {
                errors.add(new StateReportValidationError(entity, field,
                        "English language background classifications must be a valid value " +
                                m_elbCodes + " as of 2017-18",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                "school year = " + STYLE_BOLD + getCurrentContext().getSchoolYear() + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate Native Language.
     */
    protected class ValidateNativeLang implements FieldValidator {
        protected static final String VAL_ID = "VAL_NAT_LANG";

        private Collection<String> m_elbCodes;

        /**
         * Instantiates a new validate native lang.
         */
        public ValidateNativeLang() {
            m_elbCodes = Arrays.asList("L", "W", "1", "2", "3", "4", "F", "N");
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();
            if ("ENG".equals(value)) {
                String elbCode = (String) getFieldValueByBeanPath(student, m_fieldDoeElb);
                if (m_elbCodes.contains(elbCode)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Student with English language background classifications of " +
                                    m_elbCodes + " cannot have " + field.getFieldId() + " = ENG",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                    "ELB classification code = " + STYLE_BOLD + elbCode + STYLE_END));
                }
            }

            String nativeLang = (String) student.getFieldValueByBeanPath(m_fieldNlng);
            if (!StringUtils.isEmpty(nativeLang)) {
                String natStateValue = data.lookupStateValue(SisStudent.class, m_fieldNlng, nativeLang);
                if (StringUtils.isEmpty(natStateValue)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Invalif Native Language Code: " +
                                    STYLE_BOLD + nativeLang + STYLE_END + " is not mapped to any State Code. ",
                            field.getFieldId()));
                }
            }
            return errors;
        }
    }

    /**
     * Create error if school id is missing.
     */
    protected class ValidateSchoolId implements FieldValidator {
        protected static final String STD_VAL_ID = "STD_VAL_SCHOOLID";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();
            if (StringUtils.isEmpty(value)) {
                StateReportValidationError error =
                        new StateReportValidationError(entity, field, "School ID is missing", "");
                errors.add(error);
            }
            return errors;
        }

    }

    /**
     * Create error if YEAR NINTH GRADE is missing or not matching format.
     */
    protected class ValidateYearNinthGrade implements FieldValidator {
        protected static final String VAL_YEAR_NINTH = "VAL_YEAR_NINTH";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            String errString = " can only be a numeric (4 digit year) value.";
            String pattern = "0000";
            Collection errors = new ArrayList<StateReportValidationError>();
            if (StringUtils.isEmpty(value) || pattern.matches(value)) {
                SisStudent student = (SisStudent) entity.getBean();
                String ninthGrade = (String) student.getFieldValueByBeanPath(m_fieldNinthGRade);

                String test =
                        StringUtils.isEmpty(ninthGrade) ? ninthGrade : ninthGrade.replace("0", "").replace(".", "");

                if (!StringUtils.isEmpty(test)) {
                    StateReportValidationError error = new StateReportValidationError(entity,
                            field,
                            field.getFieldId() + errString,
                            "Incorrect value is " + ninthGrade);
                    errors.add(error);
                }
            }
            return errors;
        }

    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     *
     */
    protected static final String ALIAS_DOE_ELB = "DOE ELB";
    protected static final String ALIAS_DOEPREVSSN = "DOE PREVIOUS SSN";
    protected static final String ALIAS_EXCLUDE_STUDENT = "DOE EXCLUDE STD";
    protected static final String ALIAS_NATIVE_LANGUAGE = "DOE NATIVE LANGUAGE";
    protected static final String ALIAS_NINTH_GRADE = "DOE NINTH GRADE YEAR";
    protected static final String ALIAS_SCHOOL_STATE_ID = "DOE SCHOOL STATE ID";
    protected static final String ALIAS_STD_STATE_ID = "DOE EIS STATE ID";

    protected static final String PARAM_CURR_CONTEXT = "currentYearContext";

    /**
     * Valid STUDEND.[DOE ELB]
     */
    protected static final List<String> VALID_ELB = Arrays.asList(new String[] {"1", "2", "F", "G", "L"});
    /**
     * Instance variables.
     *
     */
    protected String m_fieldDoeElb;
    protected String m_fieldDoePrevSSN;
    protected String m_fieldExcludeStudent;
    protected String m_fieldNlng;
    protected String m_fieldNinthGRade;
    protected String m_fieldSchoolStateId;
    protected String m_schoolYearByParam;
    protected TNStudentHistoryHelper m_studentHelper;
    protected TNEnrollmentHelper m_enrHelper;
    protected Map<String, ReferenceCode> m_referenceGradeCodeMap;
    protected TNStudentMultiYearHelper m_stdMultiYearHelper;

    /**
     * @see com.x2dev.procedures.statereporting.tn.TNEnrollReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        // insure that all aliases and any other resources needed to operate the
        // report are available in the database.
        // setup error is created if the alias is not found.
        initializeFields();

        getCalendarsForContextOid(m_contextOid);

        if (getCurrentContext().getSchoolYear() > 2017) {

            setExportVersion(6);
            loadGradeCodes();
        } else if (getCurrentContext().getSchoolYear() > 2015) {
            setExportVersion(5);
        }

        if (getSetupErrors().size() != 0) {
            return;
        }

        X2Criteria criteria = getStudentCriteria();
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        applyInputSort(query, null);
        setQuery(query);
        setEntityClass(TNStudentExtractEntity.class);

        initStudentHelperMap(m_studentHelper, query);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Calculate grade code from StudentEnrollmentSpan.
     *
     * @param student SisStudent
     * @param yog int
     * @return Reference code
     */
    protected ReferenceCode getGradeLevel(SisStudent student, int yog) {
        ReferenceCode gradeCode = null;
        if (yog == ((Integer) getStudentMultiYearHelper().getFieldValueByBeanPath(student, SisStudent.COL_YOG))
                .intValue()) {
            String gradeLevel =
                    (String) getStudentMultiYearHelper().getFieldValueByBeanPath(student, SisStudent.COL_GRADE_LEVEL);
            gradeCode = m_referenceGradeCodeMap.get(gradeLevel);
        }
        if (gradeCode == null) {
            ModelBroker broker = new ModelBroker(getPrivilegeSet());
            TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
            List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog,
                    getCurrentContext().getSchoolYear(), sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }
        }
        return gradeCode;
    }

    /**
     * Gets the field value by bean path.
     *
     * @param student SisStudent
     * @param beanPath String
     * @return Object
     * @see TNEnrollmentHelper#getStudentValueByBeanPath(com.follett.fsc.core.k12.beans.Student,
     *      String)
     */
    Object getFieldValueByBeanPath(SisStudent student, String beanPath) {
        return m_enrHelper.getStudentValueByBeanPath(student, beanPath);
    }



    /**
     * Method for implementing business rule for schoolYear
     * (CTX_SCHOOL_YEAR - 1) where reporting date falls within `CTX_START_DATE` and `CTX_END_DATE`.
     * else CTX_SCHOOL_YEAR
     *
     * @return string representation of school year
     */
    private String getCurentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Function for building custom Student criteria.
     *
     * @return criteria for query for list of active students
     *         limited by reportDate, school and not excluded students
     */
    private X2Criteria getStudentCriteria() {

        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria = m_studentHelper.getStudentCriteria();
        studentCriteria.addAndCriteria(m_stdMultiYearHelper.getWithAttributesCriteria());
        if (isSchoolContext()) {
            m_stdMultiYearHelper.adjustCriteria(studentCriteria, Strategy.EQUAL_TO, SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        }

        return studentCriteria;
    }

    // person.[DOE PREVIOUS SSN]

    /**
     * Lookup field aliases and paths.
     *
     */
    private void initializeFields() {
        m_fieldExcludeStudent = translateAliasToJavaName(ALIAS_EXCLUDE_STUDENT, true);
        m_fieldSchoolStateId = translateAliasToJavaName(ALIAS_SCHOOL_STATE_ID, true);
        m_fieldNlng = translateAliasToJavaName(ALIAS_NATIVE_LANGUAGE, true);
        m_fieldNinthGRade = translateAliasToJavaName(ALIAS_NINTH_GRADE, true);
        m_fieldDoePrevSSN = translateAliasToJavaName(ALIAS_DOEPREVSSN, true);
        m_fieldDoeElb = translateAliasToJavaName(ALIAS_DOE_ELB, true);

        // change to m_enrHelper
        m_enrHelper = new TNEnrollmentHelper(this);
        m_studentHelper = m_enrHelper.getStudentHistoryHelper();
        m_stdMultiYearHelper = m_enrHelper.getStudentMultiYearHelper();
        if (getParameter(PARAM_REPORT_DATE) != null) {
            m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        } else {
            m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        }
        m_schoolYearByParam = getCurentSchoolYear();
    }

    /**
     * Load grade codes.
     */
    private void loadGradeCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop =
                new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        m_referenceGradeCodeMap = referenceTable.getCodeMap();
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveInstProgramStdBean.TN_CALC_INSTPGM_ID, new RetrieveInstProgramStdBean());
        calcs.put(RetrievePersonInfo.CALC_ID_PERSONINFO, new RetrievePersonInfo());
        calcs.put(RetrieveSchool.CALC_ID_SCHOOL, new RetrieveSchool());
        calcs.put(RetrieveSchoolYear.CALC_ID_SCHOOLYEAR, new RetrieveSchoolYear());
        calcs.put(RetrieveNativeLanguage.CALC_ID_NLANG, new RetrieveNativeLanguage());
        calcs.put(RetrieveYearNinthGrade.CALC_ID_YEAR_NINTH, new RetrieveYearNinthGrade());
        calcs.put(FieldRetrieverSSN.STD_CALC_ID, new FieldRetrieverSSN());
        calcs.put(RetrieveStudentContextAttribute.CALC_ID, new RetrieveStudentContextAttribute(m_enrHelper));
        calcs.put(RetrieveEnglishLangBG.CALC_ID_BG, new RetrieveEnglishLangBG(m_enrHelper));
        calcs.put(RetrieveTruncatedNames.TN_CALC_NAME, new RetrieveTruncatedNames());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FiledValidatorSSN.STD_VAL_ID, new FiledValidatorSSN());
        validators.put(ValidatePIN.STD_VAL_ID, new ValidatePIN());
        validators.put(ValidateSchoolId.STD_VAL_ID, new ValidateSchoolId());
        validators.put(ValidateDateESL.STD_DATE_ESL, new ValidateDateESL());
        validators.put(ValidateYearNinthGrade.VAL_YEAR_NINTH, new ValidateYearNinthGrade());
        validators.put(ValidateFirstEnr.VAL_ID, new ValidateFirstEnr());
        validators.put(ValidateNativeLang.VAL_ID, new ValidateNativeLang());
        validators.put(ValidateLangBg.VAL_ID, new ValidateLangBg());
        super.addValidators(validators);
    }
}
