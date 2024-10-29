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

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataTableConfig.OrganizationAccess;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * 042, 044, 052, 081, 082 Student Program Reports Data Module.
 */
public class TNStudentProgramData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for Student Program Extracts.
     *
     * @author Follett Software Company
     */
    public static class TNStudentProgramEntity extends TNStateReportEntity {
        private List<StudentRecordHelper> m_list;
        private StudentProgramParticipation m_program;
        private TNStudentProgramData m_tnData;

        /**
         * Instantiates a new TN student program entity.
         */
        public TNStudentProgramEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the end date.
         *
         * @param record StudentRecordHelper
         * @return Plain date
         */
        public PlainDate getEndDate(StudentRecordHelper record) {
            StudentProgramParticipation program = (StudentProgramParticipation) getBean();

            PlainDate value = null;
            if (program.getEndDate() == null) {
                value = record.getExitDate();
            } else if (record.getExitDate() == null) {
                value = program.getEndDate();
            } else {
                value = program.getEndDate().before(record.getExitDate()) ? program.getEndDate() : record.getExitDate();
            }

            return value;
        }

        /**
         * Gets the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StudentProgramParticipation program = (StudentProgramParticipation) getBean();
            SisStudent student = program.getStudent();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    ", PRGM: " + program.getProgramCode() +
                    "] ";
            return name;
        }

        /**
         * Gets the start date.
         *
         * @param record StudentRecordHelper
         * @return Plain date
         */
        public PlainDate getStartDate(StudentRecordHelper record) {
            StudentProgramParticipation program = (StudentProgramParticipation) getBean();

            PlainDate value = null;
            if (program.getStartDate() == null) {
                value = record.getEnrollDate();
            } else if (record.getEnrollDate() == null) {
                value = program.getStartDate();
            } else {
                value = program.getStartDate().before(record.getEnrollDate()) ? record.getEnrollDate()
                        : program.getStartDate();
            }

            return value;
        }

        /**
         * Get the helper for the current row.
         *
         * @return Student record helper
         */
        public StudentRecordHelper getStudentData() {
            return m_list.get(getCurrentRow());
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

            m_tnData = (TNStudentProgramData) data;
            m_program = (StudentProgramParticipation) bean;

            String programCode = m_tnData.lookupStateValue(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_PROGRAM_CODE, m_program.getProgramCode());
            String cepEligibleFlag = (String) m_program.getFieldValueByAlias(ALIAS_DOE_CEP_ELIGIBLE);

            boolean isCepEligibleChecked = BooleanAsStringConverter.TRUE.equals(cepEligibleFlag);
            boolean isCepProgramCode = PROGRAM_CODE_CEP_ELIGIBLE.equals(programCode);

            /**
             * Remove record if :
             * 1. Program is CEP Eligible and CEP Eligible indicator is checked.
             */
            boolean removeRow = isCepProgramCode && !isCepEligibleChecked;

            List<StudentRecordHelper> list = m_tnData.getStudentHelperMap().get(m_program.getStudentOid());
            if (list == null) {
                setRowCount(0);
            } else {
                m_list = new ArrayList(list.size());

                if (!removeRow) {
                    if (list != null) {

                        for (StudentRecordHelper record : list) {
                            if (record.getIsPrimary() &&
                                    datesOverlap(m_program.getStartDate(), m_program.getEndDate(),
                                            record.getEnrollDate(), record.getExitDate())) {
                                PlainDate startDate = getStartDate(record);
                                PlainDate endDate = getEndDate(record);
                                EntityRowKeys keys = m_tnData.new EntityRowKeys(generateKeys(record));
                                // Add to list only unique rows.
                                if (m_tnData.isUniqueEntityRow(keys)
                                        && (startDate == null || endDate == null || !endDate.before(startDate))) {
                                    m_list.add(record);
                                }
                            }
                        }
                    }
                }

                setRowCount(m_list.size());
                if (m_list.size() > 1) {
                    setRowCount(m_list.size());
                }
            }
            m_tnData.addEntityRowsCount(getRowCount());
        }


        /**
         * Returns key values for record.
         *
         * @param record StudentRecordHelper
         * @return String[]
         * @throws X2BaseException exception
         */
        private String[] generateKeys(StudentRecordHelper record) throws X2BaseException {
            List<String> fieldIds = m_tnData.getKeyFieldIds();

            ArrayList<String> keys = new ArrayList<String>();

            int fieldsSize = m_tnData.getFieldDefs().size();

            for (int i = 0; i < fieldsSize; i++) {
                FieldDefinition field = m_tnData.getFieldDefinition(i);

                if (fieldIds.contains(field.getFieldId())) {
                    if (field.getCalcId() == null) {
                        String key = m_tnData.getFieldValue(m_program, field.getFieldId());
                        keys.add(key);
                    } else {
                        Object value;
                        switch (field.getCalcId()) {
                            case RetrieveInstProgramPgmBean.TN_CALC_INSTPGM_ID:
                                value = record.getInstrProgram();
                                keys.add(value == null ? null : value.toString());
                                break;
                            case RetrieveSchoolYear.STD_CALC_ID:
                                value = TNStudentProgramHelper.getSchoolYear(m_tnData);
                                keys.add(value == null ? null : value.toString());
                                break;
                            case RetrieveProgramCode.PGM_CALC_ID:
                                value = TNStudentProgramHelper.getProgramCode(m_tnData, m_program);
                                keys.add(value == null ? null : value.toString());
                                break;
                            case RetrieveProgramEndDate.PGM_CALC_ID:
                                value = TNStudentProgramHelper.getProgramCode(m_tnData, m_program);
                                keys.add(value == null ? null : value.toString());
                                break;
                            case RetrieveProgramSchool.PGM_CALC_ID:
                                value = TNStudentProgramHelper.getProgramSchool(m_tnData, m_program, record);
                                keys.add(value == null ? null : value.toString());
                                break;
                            case RetrieveProgramStartDate.PGM_CALC_ID:
                                value = TNStudentProgramHelper.getProgramStartDate(this, record);
                                keys.add(value == null ? null : value.toString());
                                break;
                            case RetrieveSSN.PGM_CALC_ID:
                                value = TNStudentProgramHelper.getSSN(this);
                                keys.add(value == null ? null : value.toString());
                                break;

                            default:
                                break;
                        }
                    }
                }

            }

            String[] keysArray = new String[keys.size()];
            return keys.toArray(keysArray);
        }
    }

    /**
     * Class contains methods to calculate retrieved values.
     *
     * @author
     */
    protected static class TNStudentProgramHelper {
        private static final String PROGRAM_CODE_LUNCH = "FRL";

        /**
         * Calculates value for fields with PGM_PROGRAM_CODE calculationId.
         *
         * @param tnData TNStudentProgramData
         * @param program StudentProgramParticipation
         * @return String
         * @throws X2BaseException exception
         */
        public static String getProgramCode(TNStudentProgramData tnData,
                                            StudentProgramParticipation program)
                throws X2BaseException {
            String value = null;
            FieldDefinition field = tnData.getFieldByCalcId(RetrieveProgramCode.PGM_CALC_ID);

            // There is no field with PGM_PROGRAM_CODE calculation ID.
            if (field == null) {
                return value;
            }

            if (PROGRAM_CODE_LUNCH.equals(program.getProgramCode())) {
                String rowValue = (String) program.getFieldValueByBeanPath(tnData.m_fieldLunch);
                value = tnData.lookupStateValue(program.getClass(), tnData.m_fieldLunch, rowValue);
            } else {
                String rowValue = (String) tnData.getProperty(program, field.getBeanPath());
                value = tnData.lookupStateValue(program.getClass(), field.getBeanPath(), rowValue);
            }
            return value;
        }

        /**
         * Calculates value for fields with PGM_END_DATE calculationId.
         *
         * @param entity TNStudentProgramEntity
         * @param record StudentRecordHelper
         * @return Plain date
         */
        public static PlainDate getProgramEndDate(TNStudentProgramEntity entity,
                                                  StudentRecordHelper record) {
            PlainDate value = entity.getEndDate(record);
            return value;
        }

        /**
         * Calculates value for fields with PGM_SCHOOL calculationId.
         *
         * @param tnData TNStudentProgramData
         * @param program StudentProgramParticipation
         * @param record StudentRecordHelper
         * @return String
         */
        public static String getProgramSchool(TNStudentProgramData tnData,
                                              StudentProgramParticipation program,
                                              StudentRecordHelper record) {
            String value = null;
            if (!StringUtils.isEmpty(tnData.m_fieldProgramSchoolOverride)) {
                value = (String) program.getFieldValueByBeanPath(tnData.m_fieldProgramSchoolOverride);
            }
            if (!StringUtils.isEmpty(value)) {
                value = tnData.lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                        tnData.m_fieldProgramSchoolOverride, value,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            if (StringUtils.isEmpty(value)) {
                value = record.getSchoolId();
            }

            return value;
        }

        /**
         * Calculates value for fields with PGM_START_DATE calculationId.
         *
         * @param entity TNStudentProgramEntity
         * @param record StudentRecordHelper
         * @return Plain date
         */
        public static PlainDate getProgramStartDate(TNStudentProgramEntity entity,
                                                    StudentRecordHelper record) {
            PlainDate value = entity.getStartDate(record);
            return value;
        }

        /**
         * Calculates value for fields with STD_CALC_SCHOOLYEAR calculationId.
         *
         * @param tnData TNStudentProgramData
         * @return String
         */
        public static String getSchoolYear(TNStudentProgramData tnData) {
            return tnData.m_schoolYear;
        }

        /**
         * Calculates value for fields with PGM_CALC_SSN calculationId.
         *
         * @param entity TNStudentProgramEntity
         * @return String
         */
        public static String getSSN(TNStudentProgramEntity entity) {
            // student.person.personId
            StudentProgramParticipation spp = (StudentProgramParticipation) entity.getBean();

            SisStudent student = spp.getStudent();

            if (student == null) {
                return "";
            }

            Person psn = student.getPerson();

            if (psn == null) {
                return "";
            }

            String ssn = psn.getPersonId();
            if (StringUtils.isEmpty(ssn)) {
                return "";
            }

            return ssn.replaceAll("([^\\d]?)", "");
        }
    }

    /**
     * Validator for student classification.
     */
    protected class FieldValidatorClObsolete implements FieldValidator {
        protected static final String VAL_ID = "VAL_CL_OBSOLETE";

        private Collection<String> m_obsoleteCodes;

        /**
         * Instantiates a new field validator cl obsolete.
         */
        public FieldValidatorClObsolete() {
            m_obsoleteCodes = Arrays.asList("3", "X", "Y", "Z");
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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (m_obsoleteCodes.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        m_obsoleteCodes + " are obsolete codes as of 2017-18",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                "school year = " + STYLE_BOLD + getCurrentContext().getSchoolYear() + STYLE_END));
            }

            if (!StringUtils.isEmpty(value) && "H".equals(value)) {
                StudentProgramParticipation program = (StudentProgramParticipation) entity.getBean();
                if (program.getFieldValueByBeanPath(m_fieldHomelessNightTimeResidence) == null) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Existence of a 'H' record requires a valid value by alias 'DOE NIGHT TIME RES' for program.",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                    "school year = " + STYLE_BOLD + getCurrentContext().getSchoolYear() + STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * Field validator for SSN.
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999
     */
    protected class FiledValidatorSSN implements FieldValidator {
        protected static final String PGM_VAL_ID = "PGM_VAL_SSN";
        private static final String patternSSN = "^[0-9]{9}$|^$";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (value != null && !value.matches(patternSSN)) {
                errors.add(new StateReportValidationError(entity, field, "Invalid format",
                        "SSN value must be 9 digits long"));
            }
            return errors;
        }
    }

    /**
     * Field retriever for Instructional program field.
     * Can be used only with SisStudent beans.
     */
    protected class RetrieveInstProgramPgmBean implements FieldRetriever {
        public static final String TN_CALC_INSTPGM_ID = "TN_INSTRPGM_PGM";

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            TNStudentProgramEntity tnEntity = (TNStudentProgramEntity) entity;
            StudentRecordHelper record = tnEntity.getStudentData();

            return record.getInstrProgram();
        }
    }

    /**
     * Field retriever for School Year field.
     */
    protected class RetrieveSchoolYear implements FieldRetriever {
        private static final String STD_CALC_ID = "STD_CALC_SCHOOLYEAR";

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentProgramData tnData = (TNStudentProgramData) data;

            return TNStudentProgramHelper.getSchoolYear(tnData);
        }
    }

    /**
     * Field retriever for Lunch program.
     */
    protected class RetrieveProgramCode implements FieldRetriever {
        private static final String PGM_CALC_ID = "PGM_PROGRAM_CODE";

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentProgramData tnData = (TNStudentProgramData) data;
            StudentProgramParticipation program = (StudentProgramParticipation) entity.getBean();

            return TNStudentProgramHelper.getProgramCode(tnData, program);
        }

    }

    /**
     * Field retriever for programEndDate.
     */
    protected class RetrieveProgramEndDate implements FieldRetriever {
        public static final String PGM_CALC_ID = "PGM_END_DATE";

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentProgramEntity tnEntity = (TNStudentProgramEntity) entity;
            StudentRecordHelper record = tnEntity.getStudentData();

            return TNStudentProgramHelper.getProgramEndDate(tnEntity, record);
        }
    }

    /**
     * Field retriever for school.
     */
    protected class RetrieveProgramSchool implements FieldRetriever {
        public static final String PGM_CALC_ID = "PGM_SCHOOL";

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentProgramData tnData = (TNStudentProgramData) data;
            StudentProgramParticipation program = (StudentProgramParticipation) entity.getBean();
            TNStudentProgramEntity tnEntity = (TNStudentProgramEntity) entity;
            StudentRecordHelper record = tnEntity.getStudentData();

            return TNStudentProgramHelper.getProgramSchool(tnData, program, record);
        }
    }

    /**
     * Field retriever for program start date.
     */
    protected class RetrieveProgramStartDate implements FieldRetriever {
        public static final String PGM_CALC_ID = "PGM_START_DATE";

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentProgramEntity tnEntity = (TNStudentProgramEntity) entity;
            StudentRecordHelper record = tnEntity.getStudentData();

            return TNStudentProgramHelper.getProgramStartDate(tnEntity, record);
        }

    }

    /**
     *
     * Field retriever for SSN.
     * This retriever normalizes SSN value to format 999999999 (nine digits without any other
     * characters)
     *
     */
    protected class RetrieveSSN implements FieldRetriever {
        protected static final String PGM_CALC_ID = "PGM_CALC_SSN";

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentProgramEntity tnEntity = (TNStudentProgramEntity) entity;

            return TNStudentProgramHelper.getSSN(tnEntity);
        }
    }

    static final String PARAM_CLASSIFICATIONS = "classifications"; // Used by TN Restaging Procedure
    static final String PARAM_END_DATE = "endDate";
    static final String PARAM_PROCEDURE_ID = "procedureId";
    static final String PARAM_PROGRAM_CATEGORY = "programCategory";
    static final String PARAM_RESTRICT_CLASSIFIC = "restrictClassific"; // Used by TN Restaging
                                                                        // Procedure
    static final String PARAM_START_DATE = "startDate";

    private static final String ALIAS_DOE_EXCLUDE_STD = "DOE EXCLUDE STD";
    private static final String ALIAS_DOE_CEP_ELIGIBLE = "DOE CEP ELIGIBLE";
    private static final String ALIAS_DOE_LUNCH = "DOE LUNCH PROGRAM";
    private static final String ALIAS_PGM_NIGHT_TIME_RES = "DOE NIGHT TIME RES";
    private static final String ALIAS_PGM_SCHOOL_OVERRIDE = "DOE PROGRAM SCHOOL OVERRIDE";

    private static final String PROGRAM_CODE_CEP_ELIGIBLE = "3";
    private static final String PROGRAM_CODE_CLUB_MEMB = "Club Membership";
    private static final String PROGRAM_CODE_INELIGIBILITY = "Ineligibility Funding";
    private static final String PROGRAM_CODE_TA_INSTR_SERV = "TA Instruct Services";
    private static final String PROGRAM_CODE_TA_SUPPORT_SERV = "TA Support Services";

    private static final String STRING_PARAM_DELIMITER = ",";

    /**
     * Gets the field defs.
     *
     * @return List
     */
    public List<FieldDefinition> getFieldDefs() {
        return getFieldDefinitions();
    }

    /**
     * Checks for overlap of dates.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @param enrollDate PlainDate
     * @param exitDate PlainDate
     * @return true, if successful
     */
    protected static boolean datesOverlap(PlainDate startDate,
                                          PlainDate endDate,
                                          PlainDate enrollDate,
                                          PlainDate exitDate) {
        boolean overlaps = false;
        // If endDate is same as enrollDate, overlap is false
        // If startDate is same as exitDate, overlap is false
        if ((exitDate == null || startDate.before(exitDate)) && (endDate == null || endDate.after(enrollDate))) {
            overlaps = true;
        }
        return overlaps;
    }

    protected String m_fieldExcludeStd;
    protected String m_fieldCEPEligible;
    protected String m_fieldHomelessNightTimeResidence;
    protected String m_fieldInstrPgm;
    protected String m_fieldLunch;
    protected String m_fieldProgramSchoolOverride;
    protected String m_schoolYear;



    /**
     * Determine the withdrawal date that should be used as the exit date for
     * this student. The default case is the enrollment date from the first inactive enrollment.
     *
     * @param student SisStudent
     * @param span TNStudentEnrollmentSpan
     * @return PlainDate
     */
    @Override
    protected PlainDate determineWithdrawalDate(SisStudent student, TNStudentEnrollmentSpan span) {
        return span.getFirstInactiveEnrollment() == null ? null : span.getFirstInactiveEnrollment().getEnrollmentDate();
    }

    /**
     * Returns field definition by calculation ID.
     *
     * @param calcId String
     * @return field definition
     */
    protected FieldDefinition getFieldByCalcId(String calcId) {
        FieldDefinition field = null;

        List<FieldDefinition> list = getFieldDefinitions();

        for (FieldDefinition fieldDefinition : list) {
            if (calcId.equals(fieldDefinition.getCalcId())) {
                field = fieldDefinition;
            }
        }
        return field;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        String programCategory = (String) getParameter(PARAM_PROGRAM_CATEGORY);

        initializeFields();
        getCalendarsForContextOid(m_contextOid);

        if (getSetupErrors().size() != 0) {
            return;
        }

        if (getCurrentContext().getSchoolYear() > 2017 && !PROGRAM_CODE_CLUB_MEMB.equals(programCategory)
                && !PROGRAM_CODE_INELIGIBILITY.equals(programCategory)
                && !PROGRAM_CODE_TA_INSTR_SERV.equals(programCategory)
                && !PROGRAM_CODE_TA_SUPPORT_SERV.equals(programCategory)) {
            setExportVersion(2);
        } else if (PROGRAM_CODE_CLUB_MEMB.equals(programCategory) && getCurrentContext().getSchoolYear() > 2022) {
            setExportVersion(2);
        }

        Criteria criteria = getCriteria(programCategory);

        // Load enrollment information
        PlainDate endDate = (PlainDate) getParameter(PARAM_END_DATE);
        if (endDate == null || getStudentMultiYearHelper().isContextOverride()) {
            endDate = getCurrentContext().getEndDate();
        }
        TNEnrollmentHelper helper = new TNEnrollmentHelper(this);
        TNStudentHistoryHelper studentHelper = helper.getStudentHistoryHelper();
        studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, endDate);
        studentHelper.getStudentCriteria().addIn(X2BaseBean.COL_OID,
                new SubQuery(StudentProgramParticipation.class, StudentProgramParticipation.COL_STUDENT_OID, criteria));

        QueryByCriteria queryStudent = new QueryByCriteria(SisStudent.class, studentHelper.getStudentCriteria());
        initStudentHelperMap(studentHelper, queryStudent);

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);
        applyInputSort(query, StudentProgramParticipation.REL_STUDENT);
        setQuery(query);
        setEntityClass(TNStudentProgramEntity.class);

        registerFieldValidators();
        registerFieldRetrievers(helper);
    }


    /**
     * Create criteria based on time period, school input parameters
     * and state Program Reference Code,
     * and time period and school input parameters.
     *
     * @param programCategory String
     * @return Criteria
     */
    private Criteria getCriteria(String programCategory) {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                getStudentMultiYearHelper().getWithAttributesCriteria(StudentProgramParticipation.REL_STUDENT));
        if (isSchoolContext()) {
            X2Criteria schoolOidCriteria = new X2Criteria();

            if (!getStudentMultiYearHelper().isContextOverride()) {
                schoolOidCriteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER
                        + SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                getStudentMultiYearHelper().adjustCriteria(schoolOidCriteria, Strategy.EQUAL_TO,
                        StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER
                                + SisStudent.COL_SCHOOL_OID,
                        getSchool().getOid());
            }

            SubQuery subQuery = new SubQuery(StudentProgramParticipation.class, X2BaseBean.COL_OID, schoolOidCriteria);

            criteria.addIn(X2BaseBean.COL_OID, subQuery);
        } else {
            getStudentMultiYearHelper().adjustCriteria(criteria, Strategy.NOT_EQUAL_TO,
                    StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                            Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);

            getStudentMultiYearHelper().adjustCriteria(criteria, Strategy.NOT_EQUAL_TO,
                    StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                            Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
        }

        PlainDate startDate = (PlainDate) getParameter(PARAM_START_DATE);
        if (startDate == null || getStudentMultiYearHelper().isContextOverride()) {
            startDate = getCurrentContext().getStartDate();
        }
        PlainDate endDate = (PlainDate) getParameter(PARAM_END_DATE);
        if (endDate == null || getStudentMultiYearHelper().isContextOverride()) {
            endDate = getCurrentContext().getEndDate();
        }

        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, endDate);
        X2Criteria endDate1Criteria = new X2Criteria();
        endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
        X2Criteria endDate2Criteria = new X2Criteria();
        endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, startDate);
        endDate1Criteria.addOrCriteria(endDate2Criteria);
        criteria.addAndCriteria(endDate1Criteria);

        Collection<String> programCodes = getProgramCodes(programCategory);
        if (programCodes != null && !programCodes.isEmpty()) {
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, programCodes);
        }
        // need for backward compatibility
        if (PROGRAM_CODE_INELIGIBILITY.equals(programCategory)) {
            criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, programCategory);
        }

        // Create criteria on UDF (DOE EXCLUDE STD) to the STUDENT table in
        // Aspen to identify whether or not to report the Student Member to the
        // State.
        getStudentMultiYearHelper().adjustCriteria(criteria, Strategy.NOT_EQUAL_TO,
                StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER + m_fieldExcludeStd,
                BooleanAsStringConverter.TRUE);

        // Parse input parameters
        applyInputCriteria(criteria, false, StudentProgramParticipation.REL_STUDENT);

        /*
         * Add School criteria.
         */
        if (isSchoolContext()) {
            DataDictionaryTable table = getDataDictionary().findDataDictionaryTableByClass(getBeanClass().getName());
            if (table != null && !StringUtils.isEmpty(table.getDataTableConfig().getSchoolPath())) {
                ModelProperty property =
                        new ModelProperty(table.getDataTableConfig().getSchoolPath(), getBroker().getPersistenceKey());
                getStudentMultiYearHelper().adjustCriteria(criteria, Strategy.EQUAL_TO, property.getBeanPath(),
                        getSchool().getOid());
            }

            /*
             * Add Organization criteria.
             */
            if (table != null && !StringUtils.isEmpty(table.getDataTableConfig().getOrganizationPath())) {
                int level = getOrganization().getOrganizationDefinition().getLevel();
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                Collection<ModelProperty> properties =
                        OrganizationManager.getOrganizationPaths(table.getBeanClass(), dictionary, level);

                if (!CollectionUtils.isEmpty(properties)) {
                    for (ModelProperty property : properties) {
                        criteria.addAndCriteria(OrganizationManager.getOrganizationAccessCriteria(getOrganization(),
                                property, OrganizationAccess.NONE, OrganizationAccess.READ_WRITE));
                    }
                }
            }
        }

        return criteria;
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
     * Get collection of program codes corresponding to given state reference code.
     *
     * @param programCategory String
     * @return Collection of program codes
     */
    private Collection<String> getProgramCodes(String programCategory) {
        X2Criteria criteria = new X2Criteria();
        DataDictionaryField field = getDataDictionaryField(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_PROGRAM_CODE);


        Boolean restrictClassifications = (Boolean) getParameter(PARAM_RESTRICT_CLASSIFIC);
        if (restrictClassifications != null && restrictClassifications.booleanValue()) {
            String codeOidsString = (String) getParameter(PARAM_CLASSIFICATIONS);
            List<String> codeOids = new ArrayList<String>();
            if (!StringUtils.isEmpty(codeOidsString)) {
                String[] classificCodeOids = codeOidsString.split(STRING_PARAM_DELIMITER);
                codeOids.addAll(Arrays.asList(classificCodeOids));
            }

            criteria.addIn(X2BaseBean.COL_OID, codeOids);
        } else {
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            criteria.addEqualTo(ReferenceCode.COL_CATEGORY, programCategory);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
        }

        String[] columns = new String[] {ReferenceCode.COL_CODE};

        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

        List<String> result = new ArrayList<String>();
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                result.add(code);
            }
        } finally {
            iterator.close();
        }
        return result;
    }

    /**
     * Initialize fields and members.
     */
    private void initializeFields() {
        if (getParameter(PARAM_REPORT_DATE) != null) {
            m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        } else {
            m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        }
        m_fieldCEPEligible = translateAliasToJavaName(ALIAS_DOE_CEP_ELIGIBLE, false);
        m_fieldExcludeStd = translateAliasToJavaName(ALIAS_DOE_EXCLUDE_STD, false);
        m_fieldInstrPgm = translateAliasToJavaName(ALIAS_INSTR_PGM, true);
        m_fieldLunch = translateAliasToJavaName(ALIAS_DOE_LUNCH, true);
        m_fieldProgramSchoolOverride = translateAliasToJavaName(ALIAS_PGM_SCHOOL_OVERRIDE, false);
        m_fieldHomelessNightTimeResidence = translateAliasToJavaName(ALIAS_PGM_NIGHT_TIME_RES, true);
        m_schoolYear = getCurentSchoolYear();
    }

    /**
     * Register custom field retrievers.
     *
     * @param helper TNEnrollmentHelper
     */
    private void registerFieldRetrievers(TNEnrollmentHelper helper) {
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveProgramCode.PGM_CALC_ID, new RetrieveProgramCode());
        calcs.put(RetrieveProgramEndDate.PGM_CALC_ID, new RetrieveProgramEndDate());
        calcs.put(RetrieveProgramSchool.PGM_CALC_ID, new RetrieveProgramSchool());
        calcs.put(RetrieveProgramStartDate.PGM_CALC_ID, new RetrieveProgramStartDate());
        calcs.put(RetrieveInstProgramPgmBean.TN_CALC_INSTPGM_ID, new RetrieveInstProgramPgmBean());
        calcs.put(RetrieveSchoolYear.STD_CALC_ID, new RetrieveSchoolYear());
        calcs.put(RetrieveSSN.PGM_CALC_ID, new RetrieveSSN());
        calcs.put(RetrieveStudentContextAttribute.CALC_ID, new RetrieveStudentContextAttribute(helper));
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        // add validators
        HashMap<String, FieldValidator> vals = new HashMap<String, FieldValidator>();
        vals.put(FiledValidatorSSN.PGM_VAL_ID, new FiledValidatorSSN());
        vals.put(FieldValidatorClObsolete.VAL_ID, new FieldValidatorClObsolete());
        addValidators(vals);
    }
}
