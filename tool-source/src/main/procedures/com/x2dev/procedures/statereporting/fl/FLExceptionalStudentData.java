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
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

/**
 * The Class FLExceptionalStudentData.
 */
public class FLExceptionalStudentData extends FLStateReportData {

    /**
     * The Class FLExceptionalStudentEntity.
     */
    public static class FLExceptionalStudentEntity extends FLStateReportEntity {
        private FLExceptionalStudentData m_data;
        private SisStudent m_record;

        /**
         * Instantiates a new FL exceptional student entity.
         */
        public FLExceptionalStudentEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current record.
         *
         * @return Sis student
         */
        public SisStudent getCurrentRecord() {
            return m_record;
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
            String name = student.getNameView() + " [LASID: " + student.getLocalId() + "] ";
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

            m_data = (FLExceptionalStudentData) data;
            m_record = (SisStudent) getBean();
            setRowCount(m_data.getStudentHelper().isStudentEligible(m_record) ? 1 : 0);
        }
    }

    /**
     * Field retriever for Exceptional Student Fields.
     */
    protected class RetrieveExceptionalStudentFields implements FieldRetriever {
        public static final String CALC_ID = "PGM_EXCEPT";

        private static final String DDX_ID = "FL-PGM-EXCEPT";

        private static final String ALIAS_60D_EXCEPT_EXTENSION = "pgm-60-day-except-extens";
        private static final String ALIAS_ALT_ASSESSMENT_ADM = "pgm-alt-assessment-adm";
        private static final String ALIAS_DISMISSAL_DATE = "pgm-dismissal-date";
        private static final String ALIAS_ELIG_DETERM_DATE = "pgm-eligibility-determination";
        private static final String ALIAS_EVAL_COMPLETION_DATE = "pgm-eval-completion-date";
        private static final String ALIAS_EVAL_CONSENT_DATE = "pgm-evaluation-consent-date";
        private static final String ALIAS_EXCEPT_OTHER = "pgm-other";
        private static final String ALIAS_GIFTED_ELIGIBILITY = "pgm-gifted-eligibility";
        private static final String ALIAS_IDEA_EE = "pgm-idea-edu-environments";
        private static final String ALIAS_NONDISABLED_PEERS_TIME = "pgm-non-disabled-peers-time";
        private static final String ALIAS_PLACEMENT_DATE = "pgm-placement-date";
        private static final String ALIAS_PLACEMENT_STATUS = "pgm-placement-status";
        private static final String ALIAS_PLAN_DATE = "pgm-plan-date";
        private static final String ALIAS_PRIMARY = "pgm-primary";
        private static final String ALIAS_REFERRAL_REASON = "pgm-referral-reason";
        private static final String ALIAS_TOTAL_SCHOOL_WEEK = "pgm-total-school-week";

        private static final String FIELD_NAME_EXCEPT_OTHER = "Exceptionality Other";
        private static final String FIELD_NAME_EXCEPT_PRIMARY = "Except Primary";

        private static final String PARAM_60D_EXCEPT_EXTENSION = "60D_EXCEPT_EXTENSION";
        private static final String PARAM_ALT_ASSESSMENT_ADM = "ALT_ASSESSMENT_ADM";
        private static final String PARAM_DISMISSAL_DATE = "DISMISSAL_DATE";
        private static final String PARAM_ELIG_DETERM_DATE = "ELIG_DETERM_DATE";
        private static final String PARAM_EVAL_COMPLETION_DATE = "EVAL_COMPLETION_DATE";
        private static final String PARAM_EVAL_CONSENT_DATE = "EVAL_CONSENT_DATE";
        private static final String PARAM_EXCEPT_OTHER = "EXCEPT_OTHER";
        private static final String PARAM_GIFTED_ELIGIBILITY = "GIFTED_ELIGIBILITY";
        private static final String PARAM_IDEA_EE = "IDEA_EE";
        private static final String PARAM_NONDISABLED_PEERS_TIME = "NONDISABLED_PEERS_TIME";
        private static final String PARAM_PLACEMENT_DATE = "PLACEMENT_DATE";
        private static final String PARAM_PLACEMENT_STATUS = "PLACEMENT_STATUS";
        private static final String PARAM_PLAN_DATE = "PLAN_DATE";
        private static final String PARAM_PRIMARY = "PRIMARY";
        private static final String PARAM_REFERRAL_REASON = "REFERRAL_REASON";
        private static final String PARAM_TOTAL_SCHOOL_WEEK = "TOTAL_SCHOOL_WEEK";

        private DataDictionaryField m_field60DayExceptionsExtensions;
        private DataDictionaryField m_fieldAltAssessmentAdm;
        private DataDictionaryField m_fieldDismissalDate;
        private DataDictionaryField m_fieldEligibilityDeterminationDate;
        private DataDictionaryField m_fieldEvalCompletionDate;
        private DataDictionaryField m_fieldEvaluationConsentDate;
        private DataDictionaryField m_fieldGiftedEligibility;
        private DataDictionaryField m_fieldIDEA;
        private DataDictionaryField m_fieldNonDisabledPeersTime;
        private DataDictionaryField m_fieldOther;
        private DataDictionaryField m_fieldPlacementDate;
        private DataDictionaryField m_fieldPlacementStatus;
        private DataDictionaryField m_fieldPlanDate;
        private DataDictionaryField m_fieldPrimary;
        private DataDictionaryField m_fieldReferralReason;
        private DataDictionaryField m_fieldTotalSchoolWeek;

        /**
         * Instantiates a new retrieve exceptional student fields.
         *
         * @throws X2BaseException exception
         */
        public RetrieveExceptionalStudentFields() throws X2BaseException {
            StudentProgramDataset pgmExceptionalStudentDataset = getStudentHelper().getStudentProgramDataset(DDX_ID,
                    getSurveyPeriod());
            DataDictionary pgmExceptionalStudentDataDictionary = pgmExceptionalStudentDataset.getDataDictionary();

            m_fieldIDEA = translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary,
                    ALIAS_IDEA_EE, true);
            m_fieldPrimary = translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary,
                    ALIAS_PRIMARY, true);
            m_fieldAltAssessmentAdm = translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary,
                    ALIAS_ALT_ASSESSMENT_ADM, true);
            m_fieldGiftedEligibility = translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary,
                    ALIAS_GIFTED_ELIGIBILITY, true);
            m_fieldDismissalDate = translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary,
                    ALIAS_DISMISSAL_DATE, true);
            m_fieldPlanDate = translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary,
                    ALIAS_PLAN_DATE, true);
            m_fieldPlacementStatus = translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary,
                    ALIAS_PLACEMENT_STATUS, true);
            m_fieldReferralReason = translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary,
                    ALIAS_REFERRAL_REASON, true);
            m_fieldEvalCompletionDate = translateAliasToDictionaryField(
                    pgmExceptionalStudentDataDictionary, ALIAS_EVAL_COMPLETION_DATE, true);
            m_fieldPlacementDate = translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary,
                    ALIAS_PLACEMENT_DATE, true);
            m_fieldEligibilityDeterminationDate = translateAliasToDictionaryField(
                    pgmExceptionalStudentDataDictionary, ALIAS_ELIG_DETERM_DATE, true);
            m_fieldEvaluationConsentDate = translateAliasToDictionaryField(
                    pgmExceptionalStudentDataDictionary, ALIAS_EVAL_CONSENT_DATE, true);
            m_field60DayExceptionsExtensions = translateAliasToDictionaryField(
                    pgmExceptionalStudentDataDictionary, ALIAS_60D_EXCEPT_EXTENSION, true);
            m_fieldOther = translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary,
                    ALIAS_EXCEPT_OTHER, true);
            m_fieldTotalSchoolWeek = translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary,
                    ALIAS_TOTAL_SCHOOL_WEEK, true);
            m_fieldNonDisabledPeersTime = translateAliasToDictionaryField(
                    pgmExceptionalStudentDataDictionary, ALIAS_NONDISABLED_PEERS_TIME, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            FLExceptionalStudentEntity flEntity = (FLExceptionalStudentEntity) entity;
            FLExceptionalStudentData flData = (FLExceptionalStudentData) data;

            StudentProgramParticipation pgm =
                    getStudentHelper().getStudentProgram(flEntity.getBean().getOid(), DDX_ID, flData.getSurveyPeriod());

            String parameter = (String) field.getParameter();
            Object value = null;

            if (pgm != null) {
                switch (parameter) {
                    case PARAM_IDEA_EE:
                        if (SURVEY_PERIOD_2.equals(getSurveyPeriodCode())) {
                            value = flData.getFieldValue(pgm, m_fieldIDEA);
                        }
                        break;
                    case PARAM_PRIMARY:
                        value = flData.getFieldValue(pgm, m_fieldPrimary);
                        break;
                    case PARAM_ALT_ASSESSMENT_ADM:
                        if (SURVEY_PERIOD_2.equals(getSurveyPeriodCode())
                                || SURVEY_PERIOD_3.equals(getSurveyPeriodCode())) {
                            value = flData.getFieldValue(pgm, m_fieldAltAssessmentAdm);
                        }
                        break;
                    case PARAM_GIFTED_ELIGIBILITY:
                        value = flData.getFieldValue(pgm, m_fieldGiftedEligibility);
                        break;
                    case PARAM_DISMISSAL_DATE:
                        value = flData.getFieldValue(pgm, m_fieldDismissalDate);
                        break;
                    case PARAM_PLAN_DATE:
                        value = flData.getFieldValue(pgm, m_fieldPlanDate);
                        break;
                    case PARAM_PLACEMENT_STATUS:
                        value = flData.getFieldValue(pgm, m_fieldPlacementStatus);
                        break;
                    case PARAM_REFERRAL_REASON:
                        if (SURVEY_PERIOD_5.equals(getSurveyPeriodCode())) {
                            value = flData.getFieldValue(pgm, m_fieldReferralReason);
                        }
                        break;
                    case PARAM_EVAL_COMPLETION_DATE:
                        value = flData.getFieldValue(pgm, m_fieldEvalCompletionDate);
                        break;
                    case PARAM_PLACEMENT_DATE:
                        value = flData.getFieldValue(pgm, m_fieldPlacementDate);
                        break;
                    case PARAM_ELIG_DETERM_DATE:
                        value = flData.getFieldValue(pgm, m_fieldEligibilityDeterminationDate);
                        break;
                    case PARAM_EVAL_CONSENT_DATE:
                        value = flData.getFieldValue(pgm, m_fieldEvaluationConsentDate);
                        break;
                    case PARAM_60D_EXCEPT_EXTENSION:
                        value = flData.getFieldValue(pgm, m_field60DayExceptionsExtensions);
                        break;
                    case PARAM_EXCEPT_OTHER:
                        value = flData.getFieldValue(pgm, m_fieldOther);
                        break;
                    case PARAM_TOTAL_SCHOOL_WEEK:
                        if (SURVEY_PERIOD_2.equals(getSurveyPeriodCode())) {
                            value = flData.getFieldValue(pgm, m_fieldTotalSchoolWeek);
                        }
                        break;
                    case PARAM_NONDISABLED_PEERS_TIME:
                        if (SURVEY_PERIOD_2.equals(getSurveyPeriodCode()) ||
                                (entity.getFieldValue(FIELD_NAME_EXCEPT_PRIMARY).equals("L") &&
                                        entity.getFieldValue(FIELD_NAME_EXCEPT_OTHER).matches("[Z]+"))) {
                            value = flData.getFieldValue(pgm, m_fieldNonDisabledPeersTime);
                        }
                        break;

                }
            }

            return value;
        }
    }

    protected static final List<String> CUSTOM_SURVEY_PERIOD_VALID_CODES = Arrays.asList(
            FLStateReportData.SURVEY_PERIOD_1, FLStateReportData.SURVEY_PERIOD_2, FLStateReportData.SURVEY_PERIOD_3,
            FLStateReportData.SURVEY_PERIOD_4, FLStateReportData.SURVEY_PERIOD_5);

    private static final String ALIAS_FL_EDUCATION_ID = "all-psn-StateEducationId";
    private static final String ALIAS_STUDENT_NUMBER = "all-std-StateId";

    /**
     * Initialize the data module. Initialize necessary working resources. Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        Collection<StateReportValidationError> errors = getSetupErrors();
        if (errors.size() != 0) {
            return;
        }

        X2Criteria stdExtractCriteria = FLStudentExtractData.getExportStudentHelper(this).getStudentCriteria();
        SubQuery stdExtractSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdExtractCriteria);

        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID,
                getStudentHelper().getStudentProgramDataset(RetrieveExceptionalStudentFields.DDX_ID, getSurveyPeriod())
                        .getStudentSubQuery());
        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID, stdExtractSubQuery);

        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLExceptionalStudentEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getValidSurveyPeriods()
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return CUSTOM_SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Lookup field aliases and paths.
     *
     */
    private void initializeFields() {
        translateAliasToJavaName(ALIAS_STUDENT_NUMBER, true);
        translateAliasToJavaName(ALIAS_FL_EDUCATION_ID, true);
    }

    /**
     * Register custom field Retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        calcs.put(RetrieveExceptionalStudentFields.CALC_ID, new RetrieveExceptionalStudentFields());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<>();
        super.addValidators(validators);
    }
}
