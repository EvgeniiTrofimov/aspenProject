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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Class for student program enrollment export *.
 */
public class TNStudentEnrollmentData extends TNEnrollReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for student enrollment export.
     */
    public static class TNStudentEnrollmentEntity extends TNStateReportEntity
            implements TNStateReportData.HasStudentRecordHelper {
        private List<StudentRecordHelper> m_list;

        /**
         * Instantiates a new TN student enrollment entity.
         */
        public TNStudentEnrollmentEntity() {}

        /**
         * Gets the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            return student.getNameView()
                    + " [LASID: " + student.getLocalId()
                    + ", SASID: " + student.getStateId()
                    + "] ";
        }

        /**
         * Get the record helper for the current row.
         *
         * @return Student enroll record helper
         */
        @Override
        public StudentEnrollRecordHelper getCurrentRecord() {
            return (StudentEnrollRecordHelper) m_list.get(getCurrentRow());
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

            TNStudentEnrollmentData tnData = (TNStudentEnrollmentData) data;
            SisStudent student = (SisStudent) getBean();
            m_list = tnData.getStudentHelperMap().get(student.getOid());

            setRowCount(m_list == null ? 0 : m_list.size());

            tnData.addEntityRowsCount(getRowCount());
        }
    }

    /**
     * Field retriever for Enrollment fields.
     */
    protected class FieldRetrieverEnrollment implements FieldRetriever {
        protected static final String CALC_PARAM_ENROLLDATE = "ENR_ENROLLDATE";
        protected static final String CALC_PARAM_ENROLLREASON = "ENR_ENROLLREASON";
        protected static final String CALC_PARAM_SCHOOLID = "ENR_SCHOOLID";
        protected static final String CALC_PARAM_SERVICE_DISTRICTID = "ENR_DISTRICTSERVICEID";
        protected static final String CALC_PARAM_SERVICE_SCHOOLID = "ENR_SCHOOLSERVICEID";
        protected static final String ENR_CALC_ID = "ENR_CALC_ENROLL";

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
            TNStudentEnrollmentEntity seEntity = (TNStudentEnrollmentEntity) entity;
            StudentEnrollRecordHelper studentData = seEntity.getCurrentRecord();

            String param = (String) field.getParameter();
            Object value = "##Invalid-Param: " + param;

            if (param.equalsIgnoreCase(CALC_PARAM_ENROLLDATE)) {
                value = studentData.getEnrollDate();
            } else if (param.equalsIgnoreCase(CALC_PARAM_ENROLLREASON)) {
                value = studentData.getEnrollReason();
            } else if (param.equalsIgnoreCase(CALC_PARAM_SERVICE_DISTRICTID)) {
                value = studentData.getServiceDistrictId();
            } else if (param.equalsIgnoreCase(CALC_PARAM_SERVICE_SCHOOLID)) {
                value = studentData.getServiceSchoolId();
            } else if (param.equalsIgnoreCase(CALC_PARAM_SCHOOLID)) {
                value = studentData.getSchoolId();
            }

            return value;
        }
    }

    /**
     * Field retriever for RecordType, Instr program and SchoolDate fields.
     */
    protected class FieldRetrieverOther implements FieldRetriever {
        protected static final String CALC_PARAM_INSTR_SERV = "ENR_INSTR_SERVICE";
        protected static final String CALC_PARAM_SCHOOLYEAR = "ENR_SCHOOLYEAR";
        protected static final String ENR_CALC_ID = "ENR_CALC_OTHER";

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
            TNStudentEnrollmentEntity seEntity = (TNStudentEnrollmentEntity) entity;
            TNStudentEnrollmentData seData = (TNStudentEnrollmentData) data;

            String param = (String) field.getParameter();
            Object value = "##Invalid-Param: " + param;

            if (CALC_PARAM_INSTR_SERV.equals(param)) {
                value = seEntity.getCurrentRecord().getInstrServiceType();
            } else if (param.equalsIgnoreCase(CALC_PARAM_SCHOOLYEAR)) {
                value = seData.m_schoolYear;
            }

            return value;
        }
    }

    /**
     * Field retriever for Program_Participation fields.
     */
    protected class FieldRetrieverProgramParticipation implements FieldRetriever {
        protected static final String CALC_PARAM_HOMELESS_MCKINNEY = "ENR_HOMELESS_MCKINNEY";
        protected static final String CALC_PARAM_HOMELESS_PRIMARY = "ENR_HOMELESS_PRIMARY";
        protected static final String CALC_PARAM_HOMELESS_UNACC = "ENR_HOMELESS_UNACC";
        protected static final String CALC_PARAM_SES_APPLIED = "ENR_SES_APPLIED";
        protected static final String CALC_PARAM_SES_RECEIVING = "ENR_SES_RECEIVING";
        protected static final String ENR_CALC_ID = "ENR_CALC_PROGRAM";

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
            TNStudentEnrollmentEntity seEntity = (TNStudentEnrollmentEntity) entity;
            StudentEnrollRecordHelper studentData = seEntity.getCurrentRecord();

            String param = (String) field.getParameter();
            Object value = "##Invalid-Param: " + param;

            if (param.equalsIgnoreCase(CALC_PARAM_HOMELESS_PRIMARY)) {
                return studentData.getHomelessPrimaryNightTimeRes();
            } else if (param.equalsIgnoreCase(CALC_PARAM_HOMELESS_MCKINNEY)) {
                return studentData.getHomelessMcKinney();
            } else if (param.equalsIgnoreCase(CALC_PARAM_HOMELESS_UNACC)) {
                return studentData.getHomelessUnaccompanied();
            } else if (param.equalsIgnoreCase(CALC_PARAM_SES_APPLIED)) {
                return studentData.getHomelessSesApplied();
            } else if (param.equalsIgnoreCase(CALC_PARAM_SES_RECEIVING)) {
                return studentData.getHomelessSesReceived();
            }

            return value;
        }
    }

    /**
     * Field retriever for SSN.
     * This retriever normalizes SSN value to format 999999999 (nine digits without any other
     * characters)
     */
    protected class FieldRetrieverSSN implements FieldRetriever {
        protected static final String ENR_CALC_ID = "ENR_CALC_SSN";

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
            TNStudentEnrollmentEntity seEntity = (TNStudentEnrollmentEntity) entity;
            SisStudent student = (SisStudent) seEntity.getBean();
            Person psn = student.getPerson();

            String value = "";
            if (psn != null) {
                String ssn = psn.getPersonId();
                if (!StringUtils.isEmpty(ssn)) {
                    value = ssn.replaceAll("([^\\d]?)", "");
                }
            }

            return value;
        }
    }

    /**
     * Validate enr reason.
     */
    protected class FieldValidatorEnrReason implements FieldValidator {
        protected static final String VAL_ID = "VAL_ENR_REASON";

        private Collection<String> m_nonValidReasons;

        /**
         * Instantiates a new field validator enr reason.
         */
        public FieldValidatorEnrReason() {
            m_nonValidReasons = Arrays.asList("EC", "TC");
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
            if (m_nonValidReasons.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        m_nonValidReasons + " codes are not valid for " + field.getFieldId() + " as of 2017-18",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                "school year = " + STYLE_BOLD + getCurrentContext().getSchoolYear() + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate enroll reason.
     */
    protected class FieldValidatorLangBg implements FieldValidator {
        protected static final String VAL_ID = "VAL_LANG_BG";

        private Collection<String> m_elbCodes;

        /**
         * Instantiates a new field validator lang bg.
         */
        public FieldValidatorLangBg() {
            m_elbCodes = Arrays.asList("L", "W", "1", "2", "3", "4", "F", "N", "E");
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
     * Field validator for SSN.
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999
     */
    protected class FiledValidatorPgmDates implements FieldValidator {
        protected static final String VAL_ID = "VAL_PGM_DATES";

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
            String stdOid = entity.getBean().getOid();
            if (stdOid != null && m_pgmDatesErrorsByStd.containsKey(stdOid)) {
                Collection<String> datesErros = m_pgmDatesErrorsByStd.get(stdOid);
                for (String dateError : datesErros) {
                    errors.add(new StateReportValidationError(entity, field, "Invalid dates", dateError));
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
        protected static final String ENR_VAL_ID = "ENR_VAL_SSN";
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
     * Helper class for storing enrollment and programs data for a student.
     */
    protected class StudentEnrollRecordHelper extends TNStateReportData.StudentRecordHelper {
        private String m_homelessPrimaryNightTimeResidence;
        private String m_instrServiceType;
        private String m_mcKinney;
        private String m_sesApplied;
        private String m_sesReceived;
        private String m_unaccompanied;

        /**
         * Instantiates a new student enroll record helper.
         */
        public StudentEnrollRecordHelper() {
            m_homelessPrimaryNightTimeResidence = "";
            m_mcKinney = "N";
            m_unaccompanied = "N";
            m_sesApplied = "N";
            m_sesReceived = "N";
            m_instrServiceType = "P";
        }

        /**
         * Copy record helper.
         *
         * @return StudentEnrollRecordHelper
         * @see com.x2dev.procedures.statereporting.tn.TNStateReportData.StudentRecordHelper#copy()
         */
        @Override
        public StudentEnrollRecordHelper copy() {
            StudentEnrollRecordHelper record = new StudentEnrollRecordHelper();

            record.setDebug(getDebug());
            record.setEnrollDate(getEnrollDate());
            record.setEnrollReason(getEnrollReason());
            record.setExitDate(getExitDate());
            record.setInstrProgram(getInstrProgram());
            record.setIsPrimary(getIsPrimary());
            record.setSchoolId(getSchoolId());
            record.setServiceDistrictId(getServiceDistrictId());
            record.setServiceSchoolId(getServiceSchoolId());
            record.setWithdrawReason(getWithdrawReason());
            record.setYog(getYog());

            record.m_homelessPrimaryNightTimeResidence = m_homelessPrimaryNightTimeResidence;
            record.m_instrServiceType = m_instrServiceType;
            record.m_mcKinney = m_mcKinney;
            record.m_sesApplied = m_sesApplied;
            record.m_sesReceived = m_sesReceived;
            record.m_unaccompanied = m_unaccompanied;

            return record;
        }

        /**
         * Gets the homeless ses applied.
         *
         * @return String
         */
        public String getHomelessSesApplied() {
            return m_sesApplied;
        }

        /**
         * Gets the homeless ses received.
         *
         * @return String
         */
        public String getHomelessSesReceived() {
            return m_sesReceived;
        }

        /**
         * Gets the homeless mc kinney.
         *
         * @return String
         */
        public String getHomelessMcKinney() {
            return m_mcKinney;
        }

        /**
         * Gets the homeless unaccompanied.
         *
         * @return String
         */
        public String getHomelessUnaccompanied() {
            return m_unaccompanied;
        }

        /**
         * Gets the homeless primary night time res.
         *
         * @return String
         */
        public String getHomelessPrimaryNightTimeRes() {
            return m_homelessPrimaryNightTimeResidence;
        }

        /**
         * Gets the instr service type.
         *
         * @return String
         */
        public String getInstrServiceType() {
            return m_instrServiceType;
        }

        /**
         * Sets the homeless mc kinney.
         *
         * @param value void
         */
        public void setHomelessMcKinney(String value) {
            m_mcKinney = value;
        }

        /**
         * Sets the homeless primary night time res.
         *
         * @param value void
         */
        public void setHomelessPrimaryNightTimeRes(String value) {
            m_homelessPrimaryNightTimeResidence = value;
        }

        /**
         * Sets the homeless unaccompanied.
         *
         * @param value void
         */
        public void setHomelessUnaccompanied(String value) {
            m_unaccompanied = value;
        }

        /**
         * Sets the instr service type.
         *
         * @param data TNStudentEnrollmentData
         * @param enrollment StudentEnrollment
         */
        public void setInstrServiceType(TNStudentEnrollmentData data, StudentEnrollment enrollment) {
            String rowValue = (String) enrollment.getFieldValueByBeanPath(m_fieldInstrServiceType);
            String serviceType = data.lookupStateValue(enrollment.getClass(), m_fieldInstrServiceType, rowValue);
            if (!StringUtils.isEmpty(serviceType)) {
                m_instrServiceType = serviceType;
            }
            if (!getIsPrimary()) {
                m_instrServiceType = STATE_CODE_SECONDARY_STUDENT;
            }
        }

        /**
         * Sets the instr service type secondary.
         */
        public void setInstrServiceTypeSecondary() {
            m_instrServiceType = STATE_CODE_SECONDARY_STUDENT;
        }

        /**
         * Sets the program info.
         *
         * @param data TNStudentEnrollmentData
         * @param studentOid String
         */
        public void setProgramInfo(TNStudentEnrollmentData data, String studentOid) {
            Collection<StudentProgramParticipation> homelessPgms = m_programHomeless.get(studentOid);
            StudentProgramParticipation homelessPgm = selectProgram(homelessPgms);
            Collection<String> errors = m_pgmDatesErrorsByStd.get(studentOid);
            if (homelessPgm != null) {
                m_homelessPrimaryNightTimeResidence =
                        data.lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                                m_fieldHomelessNightTimeResidence,
                                (String) homelessPgm.getFieldValueByBeanPath(m_fieldHomelessNightTimeResidence),
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                m_mcKinney = (String) homelessPgm.getFieldValueByBeanPath(m_fieldHomelessMcKinney);
                m_unaccompanied = (String) homelessPgm.getFieldValueByBeanPath(m_fieldHomelessUnaccompanied);
                if (homelessPgm.getStartDate() != null && homelessPgm.getEndDate() != null
                        && homelessPgm.getStartDate().after(homelessPgm.getEndDate())) {
                    if (errors == null) {
                        errors = new ArrayList<>();
                        m_pgmDatesErrorsByStd.put(studentOid, errors);
                    }
                    errors.add("Program with code = '" + homelessPgm.getProgramCode() + "' has start date = '"
                            + homelessPgm.getStartDate() + "' after end date = '" + homelessPgm.getEndDate() + ".");
                }
            }

            Collection<StudentProgramParticipation> sesPgms = m_programSes.get(studentOid);
            StudentProgramParticipation sesPgm = selectProgram(sesPgms);
            if (sesPgm != null) {
                m_sesApplied = (String) sesPgm.getFieldValueByBeanPath(m_fieldSesApplied);
                m_sesReceived = (String) sesPgm.getFieldValueByBeanPath(m_fieldSesReceived);
                if (sesPgm.getStartDate() != null && sesPgm.getEndDate() != null
                        && sesPgm.getStartDate().after(sesPgm.getEndDate())) {
                    if (errors == null) {
                        errors = new ArrayList<>();
                        m_pgmDatesErrorsByStd.put(studentOid, errors);
                    }
                    errors.add("Program with code = '" + sesPgm.getProgramCode() + "' has start date = '"
                            + sesPgm.getStartDate() + "' after end date = '" + sesPgm.getEndDate() + ".");
                }
            }
        }

        /**
         * Sets the ses applied.
         *
         * @param value void
         */
        public void setSesApplied(String value) {
            m_sesApplied = value;
        }

        /**
         * Sets the ses received.
         *
         * @param value void
         */
        public void setSesReceived(String value) {
            m_sesReceived = value;
        }

        /**
         * Select program.
         *
         * @param programs Collection<StudentProgramParticipation>
         * @return StudentProgramParticipation
         */
        private StudentProgramParticipation selectProgram(Collection<StudentProgramParticipation> programs) {
            if (programs != null) {
                for (StudentProgramParticipation program : programs) {
                    if (program.getEndDate() != null && program.getEndDate().before(getEnrollDate())) {
                        continue;
                    }
                    // Since getExitDate() is withdrawal date, we must check if this is a member
                    // date before comparing end dates
                    if (TNStudentEnrollmentData.this.isWithdrawalDateMember() && getExitDate() != null
                            && program.getStartDate().after(getExitDate())) {
                        continue;
                    }
                    if (!TNStudentEnrollmentData.this.isWithdrawalDateMember() && getExitDate() != null
                            && !program.getStartDate().before(getExitDate())) {
                        continue;
                    }
                    return program;
                }
            }
            return null;
        }


    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String ALIAS_HOMELESS_UNACCOMPANIED = "DOE UNACCOMPANIED HOMELESS";
    protected static final String ALIAS_INSTR_SERVICE_TYPE = "DOE INSTR SERVICE TYPE";
    protected static final String ALIAS_MCKINNEY_VENTO = "DOE MCKINNEY VENTO";
    protected static final String ALIAS_NIGHT_TIME_RES = "DOE NIGHT TIME RES";
    protected static final String ALIAS_SES_APPLIED = "DOE SES APPLIED";
    protected static final String ALIAS_SES_RECEIVED = "DOE SES RECEIVING";

    // Export format column "T" Business rule
    // "Where Student Programs code State = 'Homeless Primary' "
    // Requires following state_codes for student programs
    // protected static final String STATE_CODE_HOMELESS_MCKINNEY =
    // "Homeless McKinney - Vento";
    // protected static final String STATE_CODE_HOMELESS_PRIMARY =
    // "Homeless Primary";
    // protected static final String STATE_CODE_HOMELESS_SES_APPLIED =
    // "SES Applied";
    // protected static final String STATE_CODE_HOMELESS_SES_RECEIVING =
    // "SES Receiving";
    // protected static final String STATE_CODE_HOMELESS_UNACCOMPANIED =
    // "Homeless Unacompanied";

    // But actual values from database is
    protected static final String STATE_CODE_HOMELESS_MCKINNEY = "Homeless";
    protected static final String STATE_CODE_HOMELESS_PRIMARY = "Homeless";
    protected static final String STATE_CODE_HOMELESS_SES_APPLIED = "SES";
    protected static final String STATE_CODE_HOMELESS_SES_RECEIVING = "SES";
    protected static final String STATE_CODE_HOMELESS_UNACCOMPANIED = "Homeless";
    protected static final String STATE_CODE_SECONDARY_STUDENT = "S";



    protected PlainDate m_contextEndDate;

    protected PlainDate m_contextStartDate;
    protected TNEnrollmentHelper m_enrollmentHelper;
    protected String m_fieldHomelessMcKinney;
    protected String m_fieldHomelessNightTimeResidence;
    protected String m_fieldHomelessUnaccompanied;
    protected String m_fieldInstructionalProgram;
    protected String m_fieldInstrServiceType;
    protected String m_fieldSesApplied;
    protected String m_fieldSesReceived;
    protected Boolean m_preferenceMemberOnWithdrawal;
    protected PlainDate m_previousDate;
    protected Map<String, Collection<String>> m_pgmDatesErrorsByStd = new HashMap<>();
    protected Map<String, Collection<StudentProgramParticipation>> m_programHomeless;
    protected Map<String, Collection<StudentProgramParticipation>> m_programSes;
    protected String m_schoolYear;
    protected TNStudentHistoryHelper m_studentHelper;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        getCalendarsForContextOid(m_contextOid);

        if (getSetupErrors().size() != 0) {
            return;
        }

        if (getCurrentContext().getSchoolYear() > 2022) {
            setExportVersion(4);
        } else if (getCurrentContext().getSchoolYear() > 2017) {
            setExportVersion(3);
        }

        X2Criteria criteria = getStudentCriteria();

        m_programHomeless = loadProgram("H");
        m_programSes = loadProgram("SES");

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

        applyInputSort(query, null);
        setQuery(query);

        setEntityClass(TNStudentEnrollmentEntity.class);

        initStudentHelperMap(m_studentHelper, query);

        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Gets the student record.
     *
     * @param helper TNStudentHistoryHelper
     * @param enrollment StudentEnrollment
     * @param withdrawal StudentEnrollment
     * @param enrStartDate PlainDate
     * @param lastActiveDate PlainDate
     * @param school StudentSchool
     * @param yog int
     * @return Student record helper
     * @see com.x2dev.procedures.statereporting.tn.TNStateReportData#getStudentRecord(com.x2dev.sis.model
     *      .beans.StudentEnrollment, com.x2dev.utils.types.PlainDate,
     *      com.x2dev.utils.types.PlainDate)
     */
    @Override
    protected StudentRecordHelper getStudentRecord(TNStudentHistoryHelper helper,
                                                   StudentEnrollment enrollment,
                                                   StudentEnrollment withdrawal,
                                                   PlainDate enrStartDate,
                                                   PlainDate lastActiveDate,
                                                   StudentSchool school,
                                                   int yog) {
        StudentEnrollRecordHelper record =
                (StudentEnrollRecordHelper) super.getStudentRecord(helper, enrollment, withdrawal,
                        enrStartDate, lastActiveDate, school, yog);

        record.setProgramInfo(this, enrollment.getStudentOid());

        record.setInstrServiceType(this, enrollment);
        return record;
    }

    /**
     * Checks and caches the value of the student member on withdrawal preference which is needed to
     * test for membership overlap.
     *
     * @return true, if is withdrawal date member
     */
    protected boolean isWithdrawalDateMember() {
        if (m_preferenceMemberOnWithdrawal == null) {
            m_preferenceMemberOnWithdrawal = Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                    SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE));
        }
        return m_preferenceMemberOnWithdrawal.booleanValue();
    }

    /**
     * New student record helper.
     *
     * @return StudentRecordHelper
     * @see com.x2dev.procedures.statereporting.tn.TNStateReportData#newStudentRecordHelper()
     */
    @Override
    protected StudentRecordHelper newStudentRecordHelper() {
        return new StudentEnrollRecordHelper();
    }

    /**
     * Create criteria based on state Program Reference Code,
     * and time period and school input parameters.
     *
     * @param stateProgramReferenceCode String
     * @return Criteria
     */
    private Criteria getCriteria(String stateProgramReferenceCode) {
        X2Criteria criteria = new X2Criteria();

        if (isSchoolContext()) {
            getStudentMultiYearHelper().adjustCriteria(criteria, Strategy.EQUAL_TO,
                    StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        }

        criteria.addAndCriteria(getPeriodCriteria());

        criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, getProgramCode(stateProgramReferenceCode));

        return criteria;
    }

    /**
     * Method for implementing business rule for schoolYear.
     *
     * @return string representation of school year = (CTX_SCHOOL_YEAR - 1)
     */
    private String getCurentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Get criteria based on state report procedure input date period parameters.
     *
     * @return period criteria
     */
    private Criteria getPeriodCriteria() {
        X2Criteria periodCriteria = new X2Criteria();

        periodCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_contextEndDate);

        X2Criteria criteria2 = new X2Criteria();
        X2Criteria criteria3 = new X2Criteria();
        criteria2.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
        criteria3.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_contextStartDate);
        criteria2.addOrCriteria(criteria3);
        periodCriteria.addAndCriteria(criteria2);

        return periodCriteria;
    }

    /**
     * Get collection of program codes corresponding to given state reference code.
     *
     * @param stateReferenceCode state reference code
     * @return Collection of program codes
     */
    private Collection<String> getProgramCode(String stateReferenceCode) {
        X2Criteria criteria = new X2Criteria();
        DataDictionaryField field = getDataDictionaryField(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_PROGRAM_CODE);
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, stateReferenceCode);
        String[] columns = new String[] {ReferenceCode.COL_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

        Collection<String> result = new ArrayList<String>();
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            if (iterator.hasNext()) {
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
        /*
         * m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL,
         * isSchoolContext() ? Boolean.TRUE : Boolean.FALSE);
         */

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria = m_studentHelper.getStudentCriteria();

        if (isSchoolContext()) {
            getStudentMultiYearHelper().adjustCriteria(studentCriteria, Strategy.EQUAL_TO, SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        }

        return studentCriteria;
    }

    /**
     * Lookup field aliases and paths.
     * Get data from input definition
     */
    private void initializeFields() {
        m_fieldInstructionalProgram = translateAliasToJavaName(ALIAS_INSTR_PGM, true);
        m_fieldInstrServiceType = translateAliasToJavaName(ALIAS_INSTR_SERVICE_TYPE, true);
        m_fieldHomelessMcKinney = translateAliasToJavaName(ALIAS_MCKINNEY_VENTO, true);
        m_fieldHomelessNightTimeResidence = translateAliasToJavaName(ALIAS_NIGHT_TIME_RES, true);
        m_fieldHomelessUnaccompanied = translateAliasToJavaName(ALIAS_HOMELESS_UNACCOMPANIED, true);
        m_fieldSesApplied = translateAliasToJavaName(ALIAS_SES_APPLIED, true);
        m_fieldSesReceived = translateAliasToJavaName(ALIAS_SES_RECEIVED, true);
        m_enrollmentHelper = new TNEnrollmentHelper(this);
        m_studentHelper = m_enrollmentHelper.getStudentHistoryHelper();
        m_contextStartDate = getCurrentContext().getStartDate();
        m_contextEndDate = getCurrentContext().getEndDate();
        if (getParameter(PARAM_REPORT_DATE) != null) {
            m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        } else {
            m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        }
        m_schoolYear = getCurentSchoolYear();
    }

    /**
     * loads program map for a particular state program code.
     *
     * @param programCode String
     * @return Map
     */
    private Map<String, Collection<StudentProgramParticipation>> loadProgram(String programCode) {
        Criteria criteria = getCriteria(programCode);
        criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentHelper.getStudentCriteria()));
        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);
        query.addOrderByAscending(StudentProgramParticipation.COL_STUDENT_OID);
        query.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
        return getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 32);
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(FieldRetrieverEnrollment.ENR_CALC_ID, new FieldRetrieverEnrollment());
        calcs.put(FieldRetrieverProgramParticipation.ENR_CALC_ID, new FieldRetrieverProgramParticipation());
        calcs.put(FieldRetrieverOther.ENR_CALC_ID, new FieldRetrieverOther());
        calcs.put(FieldRetrieverSSN.ENR_CALC_ID, new FieldRetrieverSSN());
        calcs.put(RetrieveInstProgramStdBean.TN_CALC_INSTPGM_ID, new RetrieveInstProgramStdBean());
        calcs.put(RetrieveStudentContextAttribute.CALC_ID, new RetrieveStudentContextAttribute(m_enrollmentHelper));
        calcs.put(RetrieveEnglishLangBG.CALC_ID_BG, new RetrieveEnglishLangBG(m_enrollmentHelper));
        super.addCalcs(calcs);

    }

    /**
     * Register field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FiledValidatorSSN.ENR_VAL_ID, new FiledValidatorSSN());
        validators.put(FieldValidatorLangBg.VAL_ID, new FieldValidatorLangBg());
        validators.put(FieldValidatorEnrReason.VAL_ID, new FieldValidatorEnrReason());
        validators.put(FiledValidatorPgmDates.VAL_ID, new FiledValidatorPgmDates());
        super.addValidators(validators);
    }
}
