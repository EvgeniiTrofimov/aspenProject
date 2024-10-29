/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.SubmissionType;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisError;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisRecord.HeaderErrors;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisResult;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisResult.ErrorParser;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisStateReportEntity;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.ValidationErrorType;
import com.x2dev.utils.StringUtils;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The Class OnsisValidations.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class OnsisValidations {

    /**
     * The Enum OnsisElement.
     */
    enum OnsisElement {
        ACADEMIC_YEAR, ACTION, ADE, ALL_REQUIREMENTS_MET_DATE, APPEAL_DECISION_DATE, APPEAL_FLAG, APPEAL_OUTCOME_TYPE,
        //
        APPEAL_START_DATE, ASSIGNED_GRADE, ASSIGNED_SUBJECT, ASSIGNMENT_END_DATE, ASSIGNMENT_START_DATE, ASSIGNMENT_WTHD_TYPE,
        //
        ATTENDANCE_TYPE, BIRTH_DATE, BOARD_BASED_FLAG, BOARD_EMPLOYEE_NUMBER, BOARD_MINISTRY_SET_FLAG, BSID_SCHOOL_NUMBER,
        //
        CALENDAR_FLAG, CARE_TRTMNT_RES_TYPE, CERTIFICATE_ISSUED, CERTIFICATION_HOURS, CLASS, CLASSES_MISSED, CLASSROOM_TYPE,
        //
        CLASS_ASSIGNMENT, CLASS_CODE, CLASS_END_DATE, CLASS_START_DATE, CLASS_TYPE, CLEAR_AGGREGATE_DATA, CLEAR_ASSIGNED_SUBJECT,
        //
        CLEAR_AVG_REPORT_CARD_GRADE, CLEAR_BOARD_EDUCATOR_ASSIGNMENT, CLEAR_BOARD_SCHOOL_PERSONNEL, CLEAR_CHILD_CARE, CLEAR_CLASS,
        //
        CLEAR_CLASS_ASSIGNMENT, CLEAR_CONTINUING_EDU, CLEAR_COURSE_CALENDAR, CLEAR_DIPLOMA, CLEAR_EXCEPTIONAL_STU_TCHR,
        //
        CLEAR_HABITUAL_ABSENT, CLEAR_ILE, CLEAR_INSTRUCTIONAL_TM, CLEAR_LANGUAGE_PROGRAM, CLEAR_LITERACY_AND_NUMERACY,
        //
        CLEAR_MEDIAN_REPORT_CARD_GRADE, CLEAR_NADE, CLEAR_NE_DIPLOMA, CLEAR_NE_OPTION_SHEET, CLEAR_NE_SHSM, CLEAR_NON_ATTENDANCE,
        //
        CLEAR_NTIP, CLEAR_OPTION_SHEET, CLEAR_OTHER_CREDIT, CLEAR_PENDING_AREA, CLEAR_PLAR, CLEAR_PLAR_MATURE_REPORT,
        //
        CLEAR_PPA, CLEAR_REMEDIAL_PROGRAM, CLEAR_REPORT_CARD, CLEAR_SALEP, CLEAR_SCHOOL_EDUCATOR_ASSIGNMENT, CLEAR_SCHOOL_INCIDENT,
        //
        CLEAR_SECOND_LANGUAGE_PROGRAM, CLEAR_SEGMENT, CLEAR_SHSM, CLEAR_SPECIAL_EDUCATION, CLEAR_SPVSD_ALTLR, CLEAR_STUDENT_CLASS_ENROLMENT,
        //
        CLEAR_STUDENT_NON_ENROLMENT, CLEAR_STUDENT_SCHOOL_ENROLMENT, CLEAR_SURVEY, CLEAR_SWT, CLEAR_TLA, CLEAR_TPA, CLEAR_TPA_ELEMENTAL,
        //
        CLEAR_VIOLENT_INCIDENT, COMMUNITY_INVOLMENT_ACCU_HOURS, COMPONENT, COMPULSORY_COURSE_FLAG, COMPULSORY_CREDIT, COMPULSORY_IND_FLAG,
        //
        CORE_FLAG, COUNTRY_TYPE, COUNTRY_TYPE_BIRTH, COUNTRY_TYPE_EXIT, COUNTRY_TYPE_PERM, COURSE_CALENDAR, COURSE_CALENDAR_ACADEMIC_YEAR,
        //
        COURSE_CALENDAR_COURSE, COURSE_COMPLETE_DATE, COURSE_COMPLETE_FLAG, COURSE_CONTINUED_FLAG, COURSE_DELIVERY_TYPE, COURSE_END_DATE,
        //
        COURSE_INCOMPLETE_FLAG, COURSE_REPEAT_FLAG, COURSE_SBST_FLAG, COURSE_SEM_TYPE, COURSE_START_DATE, COURSE_TYPE, CREDIT, CREDIT_TYPE,
        //
        CREDIT_VALUE, CRS_IDENTIFIER, CURRENT_RESIDENCE_COUNTRY, CURRENT_RESIDENCE_PROVINCE, DATE, DATE_APPROVED, DATE_COMPLETED, DATE_OF_OCCURRENCE, DEPARTMENT_TYPE, DIPLOMA,
        //
        DISCRETIONARY_MANDATORY_FLAG, EARNED_CREDIT, EARNED_CREDIT_VALUE, EDUCATOR_LEAVE_TYPE, ELEMENTARY_SUBJECT_TYPE,
        //
        EMPLOYER_INVOLVEMENT_FLAG, END_DATE, ENROLMENT_END_DATE, ENROLMENT_START_DATE, EXCEPTIONALITY_TYPE, EXIT_TYPE, E_OPTION_SHEET,
        //
        E_OPTION_SHEET_COURSE, FILE_ID, FINAL_MARK, FIRST_DAY_SUSPENSION_EXPULSION, FRENCH_ADMISSION_APPROVAL_DATE, FTE,
        //
        GENDER_TYPE, GRADE_11_12_CHA_FLAG, GRADE_11_12_EQU_FLAG, GRADE_9_10_IND_FLAG, GRADE_DESIGNATION_TYPE, GRADE_FLAG, GRADE_TYPE,
        //
        HIGH_CREDIT_ADE, HIGH_CREDIT_FTE, IEP_FLAG, INCIDENT_SITE_TYPE, INDIGENOUS_SELF_IDENTIFICATION, INDIVIDUAL_EDUCATION_PLAN_FLAG,
        //
        INFRACTION_AUTH_TYPE, INFRACTION_OUTCOM_TYPE, INFRACTION_TYPE, INSTITUTION_TYPE, INSTRUCTIONAL_TM, INSTRUCTIONAL_TM_TYPE,
        //
        IPRC_REVIEW_DATE, IPRC_STUDENT_FLAG, JK_TIMETABLE_TYPE, LANGUAGE_TYPE, LANGUAGE_TYPE_PREVIOUS_SCH, LAST_DAY_SUSPENSION_EXPULSION,
        //
        LETTER_PERMISSION, LITERACY_STATUS_TYPE, LOCAL_DEV_CRS, LOCAL_SCHOOL_PROGRAM_FLAG, LONGER_TERM_HEAD_COUNT,
        //
        MAIN_EXCEPTIONALITY_FLAG, MAIN_SCHOOL_FLAG, MATURE_STUDENT_FLAG, MEDIUM_TERM_HEAD_COUNT, MEN, MIN, MINISTRY_DFND_CRS,
        //
        MINUTES_PER_DAY_OF_INSTRUCTION, NEW_ASSIGNMENT_WTHD_TYPE, NEW_COURSE_COMPLETE_DATE, NEW_CREDIT_TYPE, NEW_DATE_APPROVED,
        //
        NEW_EDUCATOR_LEAVE_TYPE, NEW_POSITION_TYPE, NON_ACADEMIC_PROGRAM, NON_CREDIT_AVERAGE_DAILY_ENROLMENT, NON_IDENTIFIED_STUDENT_FLAG,
        //
        NON_TEACHING_FLAG, NTIP_STATUS_TYPE, NUMBER_OF_CLASSES, NUMBER_OF_SCHOOL_DAYS, OEN, OEN_BATCH_MULTIPLES, OEN_DETAILS,
        //
        ONTARIO_SCHOLARSHIP_DATE, ONTARIO_SCHOLARSHIP_FLAG, OPTION_SHEET_ACADEMIC_YEAR, OPTION_SHEET_GRADE, OTHER_COURSE_INFO, OTHER_CREDIT, OTHER_DESCRIPTION,
        //
        OTHER_HALFTIME_DESC, OYAP_FLAG, PLACE_HLDR_CRS, PLAR, PLAR_MATURE_EQ_CNT_TYPE, POSITION_TYPE, POSTAL_AREA_TYPE, PREFERRED_FIRST_NAME,
        //
        PREFERRED_SECOND_NAME, PREFERRED_SURNAME, PROGRAM, PROMOTION_FLAG, PROVINCE_STATE_TYPE, PROVINCE_STATE_TYPE_BIRTH, PROVINCE_STATE_TYPE_EXIT,
        //
        PROVINCE_STATE_TYPE_PERM, REACH_AHEAD_FLAG, REFERENCE_NUMBER, REMEDIAL_PROGRAM, REPORT_CARD, REPORTED_TO_POLICE_FLAG,
        //
        RESIDENCE_STATUS_TYPE, REVISED_NUMBER_OF_SCHOOL_DAYS, SALEP, SCHOOL, SCHOOL_BASED_FLAG, SCHOOL_CRS_NAME, SCHOOL_EDUCATOR_ASSIGNMENT,
        //
        SCHOOL_INCIDENT_ID, SCHOOL_NUMBER, SCHOOL_STUDENT_NUMBER, SCHOOL_SUBMISSION, SECONDARY_SUBJECT_TYPE, SECOND_LANGUAGE_PROGRAM, SEGMENT, SHORT_TERM_HEAD_COUNT,
        //
        SHSM_CERTIFICATION, SHSM_PROGRAM, SOURCE_DOCUMENT, SPECIAL_EDUCATION, SPECIAL_EDUCATION_FLAG, SPECIAL_EDU_PLMNT_TYPE, START_DATE,
        //
        STATUS_TYPE, STUDENT, STUDENT_CLASS_ENROLMENT, STUDENT_DOB, STUDENT_HEAD_COUNT, STUDENT_INCIDENT, STUDENT_INFRACTION, STUDENT_LEGAL_GIVEN_NAME,
        //
        STUDENT_LEGAL_LAST_NAME, STUDENT_LEGAL_SECOND_NAME, STUDENT_MIN_NUMBER, STUDENT_MOBILITY_TYPE, STUDENT_MOBILITY_TYPE_EXIT, STUDENT_NON_ENROLMENT,
        //
        STUDENT_OUTCOME, STUDENT_REF, STUDENT_SCHOOL_ENROLMENT, STU_BRD_RES_STAT_TYPE, SUBMISSION_PERIOD_TYPE, SUSPENSION_EXPULSION_FLAG,
        //
        SUSPENSION_EXPULSION_PROGRAM, SWT, SWT_PROGRAM, SWT_PROGRAM_BY_GENDER, TEACHING_TYPE, TEMPORARY_LETTER_APPROVAL, TIME, TIMES_LATE,
        //
        TIME_OF_OCCURRENCE, TLA, TOTAL_CLASSES, TOTAL_CLASS_COURSE, TRACK_PROGRESS_FLAG, TYPE, WITHDRAWAL_DATE, WITHDRAWAL_TYPE, YEAR_OF_ENTRY_TO_CANADA,
        //
        YOUR_REFERENCE_NUMBER;
    }

    /**
     * The Class OnsisValidator.
     */
    public static class OnsisValidator {
        private final X2Broker m_broker;
        private boolean m_doValidate = false;
        private List<OnsisError> m_errors = new ArrayList<OnsisError>();
        private Set<String> m_exceptions = new HashSet<>();
        private final Organization m_organization;
        private Map<String, OnsisRecord> m_recordsByElement;
        private OnsisResult m_result;
        private final SubmissionType m_submissionType;

        /**
         * Instantiates a new onsis validator.
         *
         * @param organization Organization
         * @param broker X2Broker
         * @param submissionType SubmissionType
         * @param csvRecordsHelper OnsisExtractHelper
         * @param doValidate boolean
         */
        private OnsisValidator(Organization organization, X2Broker broker, SubmissionType submissionType,
                OnsisExtractHelper csvRecordsHelper, boolean doValidate) {
            m_organization = organization;
            m_broker = broker;
            m_submissionType = submissionType;
            Collection<ExportFormatDefinition> formats =
                    FilterableFactory.create(broker, ExportFormatDefinition.class).extract();
            // TODO: Implement validator
            // m_exsmsElementsHelper =
            // new ExsmsElementsHelper(broker, new ArrayList<ExportFormatDefinition>(formats));
            // m_databaseSearcher = new AspenDatabaseSearcher(broker);
            // m_csvRecordsSearcher =
            // new CsvRecordsSearcher(csvRecordsHelper, m_exsmsElementsHelper);
            // m_cache = new NodeValidationResultCache();
            m_doValidate = doValidate;
        }

        /**
         * Validate entity.
         *
         * @param entity OnsisStateReportEntity
         * @param rule ValidationRule<OnsisStateReportEntity>
         */
        public void validateEntity(OnsisStateReportEntity entity, ValidationRule<OnsisStateReportEntity> rule) {
            if (m_doValidate) {
                try {
                    System.out.println("Current validation rule: " + rule.getGroupName() + rule.getMessageCode());
                    if (!rule.isValid(entity)) {
                        OnsisRecord.HeaderErrors header = new OnsisRecord.EntityHeaderErrors(entity);
                        OnsisRecord record = getRecord(entity);
                        String customMessage = rule.getCustomMessage(entity);
                        OnsisError error =
                                createOnsisError(record, header,
                                        rule.getErrorType().getDescription() + ": " + rule.getMessageEn()
                                                + customMessage,
                                        rule.getErrorType().getDescription() + ": " + rule.getMessageFr()
                                                + customMessage,
                                        rule.getMessageCode(), rule.getFieldName(), rule.getFieldValue(entity));
                        if (!m_errors.contains(error)) {
                            m_result.addError(record, error);
                            m_errors.add(error);
                        }
                    }
                } catch (Exception e) {
                    StringBuilder output = new StringBuilder();
                    // output node
                    output.append("Element: ");
                    output.append(entity.getElementName());
                    output.append(" - Entity Description: ");
                    output.append(entity.getEntityName());
                    output.append("\r\n");
                    // output message
                    String message = e.getMessage();
                    if (!StringUtils.isEmpty(message)) {
                        output.append(message);
                        output.append(":\r\n");
                    }
                    // output trace
                    StringWriter sw = new StringWriter();
                    PrintWriter pw = new PrintWriter(sw);
                    e.printStackTrace(pw);
                    output.append(sw.toString());
                    m_exceptions.add(output.toString());
                }
            }
        }


        /**
         * Creates the onsis error.
         *
         * @param record OnsisRecord
         * @param headerErrors HeaderErrors
         * @param messageEn String
         * @param messageFr String
         * @param messageCode String
         * @param fieldName String
         * @param fieldValue String
         * @return OnsisError
         */
        private OnsisError createOnsisError(OnsisRecord record,
                                            HeaderErrors headerErrors,
                                            final String messageEn,
                                            final String messageFr,
                                            final String messageCode,
                                            final String fieldName,
                                            final String fieldValue) {
            OnsisError error = m_result.createError(record, headerErrors, new ErrorParser() {

                @Override
                public void parseToBean(X2BaseBean bean, HeaderErrors header, DataDictionary dictionary) {
                    bean.setFieldValueByAlias(OnsisError.ErrorField.MESSAGE_CODE.toString(),
                            messageCode,
                            dictionary);
                    bean.setFieldValueByAlias(OnsisError.ErrorField.E_MESSAGE.toString(),
                            messageEn,
                            dictionary);
                    bean.setFieldValueByAlias(OnsisError.ErrorField.F_MESSAGE.toString(),
                            messageFr,
                            dictionary);
                    bean.setFieldValueByAlias(OnsisError.ErrorField.FIELD_NAME.toString(),
                            fieldName,
                            dictionary);
                    bean.setFieldValueByAlias(OnsisError.ErrorField.FIELD_VALUE.toString(),
                            fieldValue,
                            dictionary);
                }
            });

            return error;
        }

        /**
         * Gets the record.
         *
         * @param entity OnsisStateReportEntity
         * @return Onsis record
         */
        private OnsisRecord getRecord(OnsisStateReportEntity entity) {
            return getRecord(new OnsisRecord.EntityHeaderErrors(entity));
        }

        /**
         * Gets the record.
         *
         * @param header HeaderErrors
         * @return Onsis record
         */
        private OnsisRecord getRecord(OnsisRecord.HeaderErrors header) {
            if (m_result == null) {
                instantiateResult();
            }
            if (m_recordsByElement == null) {
                m_recordsByElement = new HashMap<>();
            }
            String path = header.getPathToElement();
            String keyFields = header.getKeyFields(m_result.getElementsHelper());
            String keyValues = header.getKeyValues(m_result.getElementsHelper());
            String key = path + keyFields + keyValues;
            OnsisRecord record = m_recordsByElement.get(key);
            if (record == null) {
                record = m_result.addRecord(header);
                record.beforeSave();
                m_broker.saveBean(record.getBean());
                m_recordsByElement.put(key, record);
            }
            return record;
        }

        /**
         * Instantiate result.
         */
        private void instantiateResult() {
            if (m_result == null) {
                Filterable<ExportFormatDefinition> formats =
                        FilterableFactory.create(m_broker, ExportFormatDefinition.class);
                m_result = OnsisResult.createResult(m_organization, m_broker, "OnSIS Validations Result",
                        new ExsmsElementsHelper(m_broker,
                                new ArrayList<ExportFormatDefinition>(formats.extract())));
                m_broker.saveBean(m_result.getBean());
            }
        }

    }

    /**
     * The Interface ValidationRule.
     *
     * @param <T> the generic type
     */
    interface ValidationRule<T> {

        /**
         * Gets the custom message.
         *
         * @param entity T
         * @return String
         */
        public String getCustomMessage(T entity);

        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName();

        /**
         * Gets the field value.
         *
         * @param entity T
         * @return String
         */
        public String getFieldValue(T entity);

        /**
         * Gets the group name.
         *
         * @return String
         */
        public String getGroupName();

        /**
         * Gets the message code.
         *
         * @return String
         */
        public String getMessageCode();

        /**
         * Gets the message en.
         *
         * @return String
         */
        public String getMessageEn();

        /**
         * Gets the message fr.
         *
         * @return String
         */
        public String getMessageFr();

        /**
         * Gets the error type.
         *
         * @return Validation error type
         */
        public ValidationErrorType getErrorType();

        /**
         * Checks if is valid.
         *
         * @param entity T
         * @return true, if is valid
         * @throws Exception exception
         */
        boolean isValid(T entity) throws Exception;
    }


}
