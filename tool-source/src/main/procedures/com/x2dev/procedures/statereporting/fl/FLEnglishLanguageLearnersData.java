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
 * The Class FLEnglishLanguageLearnersData.
 */
public class FLEnglishLanguageLearnersData extends FLStateReportData {

    /**
     * The Class FLEnglishLanguageLearnersEntity.
     */
    public static class FLEnglishLanguageLearnersEntity extends FLStateReportEntity {

        private FLEnglishLanguageLearnersData m_data;
        private StudentProgramParticipation m_pgm;
        private String m_pgmCode;
        private SisStudent m_record;

        /**
         * Instantiates a new FL english language learners entity.
         */
        public FLEnglishLanguageLearnersEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return the entity name
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = m_record;
            String name = student.getNameView() + " [LASID: " + student.getLocalId() + "] ";
            return name;
        }

        /**
         * Gets the program code.
         *
         * @return the program code
         */
        public String getProgramCode() {
            return m_pgmCode;
        }

        /**
         * Gets the student program.
         *
         * @return the student program
         */
        public StudentProgramParticipation getStudentProgram() {
            return m_pgm;
        }

        /**
         * Intitialize.
         *
         * @param data the data
         * @param bean the bean
         * @throws X2BaseException the x 2 base exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (FLEnglishLanguageLearnersData) data;
            m_record = (SisStudent) bean;
            List<StudentProgramParticipation> list = m_data.m_pgmEllDataset.getPrograms(m_record.getOid());

            int rc = 0;
            if (list != null && !list.isEmpty()) {
                m_pgm = list.get(list.size() - 1);
                m_pgmCode = (String) m_data.getFieldValue(m_pgm, m_data.m_fieldELLCode);
                if (ELL_REPORTABLE_CODES.contains(m_pgmCode)) {
                    rc = 1;
                }
            }
            setRowCount(rc);
        }
    }

    /**
     * Field retriever for English Language Learners fields.
     */
    protected class RetrieveEnglishLanguageLearners implements FieldRetriever {
        public static final String CALC_ID = "PGM_ELL";
        public static final String DDX_ID = "FL-PGM-ELL";

        private static final String ALIAS_BASIS_OF_ENTRY = "pgm-basis-of-entry";
        private static final String ALIAS_BASIS_OF_EXIT_FIRST = "pgm-basis-of-exit-first";
        private static final String ALIAS_BASIS_OF_EXIT_SECOND = "pgm-basis-of-exit-second";
        private static final String ALIAS_CLASSIFICATION_DATE = "pgm-classification-date";
        private static final String ALIAS_EXT_OF_INSTRUCTION = "pgm-extension-of-instruction";
        private static final String ALIAS_FUND_SRC = "pgm-ell-fund-src";
        private static final String ALIAS_PROG_PARTICIPATION = "pgm-prog-participation";
        private static final String ALIAS_RECLASSIFICATION_DATE = "pgm-reclassification-date";
        private static final String ALIAS_RECLASS_EXIT_DATE = "pgm-reclassification-exit-date";
        private static final String ALIAS_REEVALUATION_DATE = "pgm-reevaluation-date";
        private static final String ALIAS_STUDENT_PLAN_DATE = "pgm-student-plan-date";
        private static final String ALIAS_TEST_CONT_LISTENING = "pgm-test-cont-listening";
        private static final String ALIAS_TEST_CONT_READING = "pgm-test-cont-reading";
        private static final String ALIAS_TEST_CONT_SPEAKING = "pgm-test-cont-speaking";
        private static final String ALIAS_TEST_DATE_LISTENING = "pgm-test-date-listening";
        private static final String ALIAS_TEST_DATE_READING = "pgm-test-date-reading";
        private static final String ALIAS_TEST_DATE_SPEAKING = "pgm-test-date-speaking";
        private static final String ALIAS_TEST_DATE_WRITING = "pgm-test-date-writing";
        private static final String ALIAS_TEST_FORM_LISTENING = "pgm-test-form-listening";
        private static final String ALIAS_TEST_FORM_READING = "pgm-test-form-reading";
        private static final String ALIAS_TEST_FORM_SPEAKING = "pgm-test-form-speaking";
        private static final String ALIAS_TEST_FORM_WRITING = "pgm-test-form-writing";
        private static final String ALIAS_TEST_LEVEL_LISTENING = "pgm-test-level-listening";
        private static final String ALIAS_TEST_LEVEL_READING = "pgm-test-level-reading";
        private static final String ALIAS_TEST_LEVEL_SPEAKING = "pgm-test-level-speaking";
        private static final String ALIAS_TEST_LEVEL_WRITING = "pgm-test-level-writing";
        private static final String ALIAS_TEST_NAME_LISTENING = "pgm-test-name-listening";
        private static final String ALIAS_TEST_NAME_READING = "pgm-test-name-reading";
        private static final String ALIAS_TEST_NAME_SPEAKING = "pgm-test-name-speaking";
        private static final String ALIAS_TEST_NAME_WRITING = "pgm-test-name-writing";
        private static final String ALIAS_TEST_SCORE_LISTENING = "pgm-test-score-listening";
        private static final String ALIAS_TEST_SCORE_READING = "pgm-test-score-reading";
        private static final String ALIAS_TEST_SCORE_SPEAKING = "pgm-test-score-speaking";
        private static final String ALIAS_TEST_SCORE_WRITING = "pgm-test-score-writing";
        private static final String ALIAS_TEST_SC_T_LISTENING = "pgm-test-score-type-listening";
        private static final String ALIAS_TEST_SC_T_READING = "pgm-test-score-type-reading";
        private static final String ALIAS_TEST_SC_T_SPEAKING = "pgm-test-score-type-speaking";
        private static final String ALIAS_TEST_SC_T_WRITING = "pgm-test-score-type-writing";
        private static final String ALIAS_TEST_SUBJECT_WRITING = "pgm-test-subject-writing";
        private static final String ALIAS_TIER_PLACEMENT = "pgm-tier-placement";

        private static final String PARAM_BASIS_OF_ENTRY = "BASIS_OF_ENTRY";
        private static final String PARAM_BASIS_OF_EXIT_FIRST = "BASIS_OF_EXIT_FIRST";
        private static final String PARAM_BASIS_OF_EXIT_SECOND = "BASIS_OF_EXIT_SECOND";
        private static final String PARAM_CLASSIFICATION_DATE = "CLASSIFICATION_DATE";
        private static final String PARAM_ENTRY_DATE = "ENTRY_DATE";
        private static final String PARAM_EXIT_DATE = "EXIT_DATE";
        private static final String PARAM_EXTENSION_OF_INSTRUCTION = "EXTENSION_OF_INSTRUCTION";
        private static final String PARAM_FUND_SRC = "FUND_SRC";
        private static final String PARAM_PROG_PARTICIPATION = "PROG_PARTICIPATION";
        private static final String PARAM_RECLASSIFICATION_DATE = "RECLASSIFICATION_DATE";
        private static final String PARAM_RECLASSIFICATION_EXIT_DATE = "RECLASSIFICATION_EXIT_DATE";
        private static final String PARAM_REEVALUATION_DATE = "REEVALUATION_DATE";
        private static final String PARAM_STUDENT_PLAN_DATE = "STUDENT_PLAN_DATE";
        private static final String PARAM_TEST_CONT_LISTENING = "TEST_CONT_LISTENING";
        private static final String PARAM_TEST_CONT_READING = "TEST_CONT_READING";
        private static final String PARAM_TEST_CONT_SPEAKING = "TEST_CONT_SPEAKING";
        private static final String PARAM_TEST_DATE_LISTENING = "TEST_DATE_LISTENING";
        private static final String PARAM_TEST_DATE_READING = "TEST_DATE_READING";
        private static final String PARAM_TEST_DATE_SPEAKING = "TEST_DATE_SPEAKING";
        private static final String PARAM_TEST_DATE_WRITING = "TEST_DATE_WRITING";
        private static final String PARAM_TEST_FORM_LISTENING = "TEST_FORM_LISTENING";
        private static final String PARAM_TEST_FORM_READING = "TEST_FORM_READING";
        private static final String PARAM_TEST_FORM_SPEAKING = "TEST_FORM_SPEAKING";
        private static final String PARAM_TEST_FORM_WRITING = "TEST_FORM_WRITING";
        private static final String PARAM_TEST_LEVEL_LISTENING = "TEST_LEVEL_LISTENING";
        private static final String PARAM_TEST_LEVEL_READING = "TEST_LEVEL_READING";
        private static final String PARAM_TEST_LEVEL_SPEAKING = "TEST_LEVEL_SPEAKING";
        private static final String PARAM_TEST_LEVEL_WRITING = "TEST_LEVEL_WRITING";
        private static final String PARAM_TEST_NAME_LISTENING = "TEST_NAME_LISTENING";
        private static final String PARAM_TEST_NAME_READING = "TEST_NAME_READING";
        private static final String PARAM_TEST_NAME_SPEAKING = "TEST_NAME_SPEAKING";
        private static final String PARAM_TEST_NAME_WRITING = "TEST_NAME_WRITING";
        private static final String PARAM_TEST_SCORE_LISTENING = "TEST_SCORE_LISTENING";
        private static final String PARAM_TEST_SCORE_READING = "TEST_SCORE_READING";
        private static final String PARAM_TEST_SCORE_SPEAKING = "TEST_SCORE_SPEAKING";
        private static final String PARAM_TEST_SCORE_TYPE_LISTENING = "TEST_SC_T_LISTENING";
        private static final String PARAM_TEST_SCORE_TYPE_READING = "TEST_SC_T_READING";
        private static final String PARAM_TEST_SCORE_TYPE_SPEAKING = "TEST_SC_T_SPEAKING";
        private static final String PARAM_TEST_SCORE_TYPE_WRITING = "TEST_SC_T_WRITING";
        private static final String PARAM_TEST_SCORE_WRITING = "TEST_SCORE_WRITING";
        private static final String PARAM_TEST_SUBJECT_WRITING = "TEST_SUBJECT_WRITING";
        private static final String PARAM_TIER_PLACEMENT = "TIER_PLACEMENT";

        private DataDictionaryField m_fieldBasisofEntry;
        private DataDictionaryField m_fieldBasisofExitFirst;
        private DataDictionaryField m_fieldBasisofExitSecond;
        private DataDictionaryField m_fieldClassificationDate;
        private DataDictionaryField m_fieldExtensionofInstruction;
        private DataDictionaryField m_fieldFundSrc;
        private DataDictionaryField m_fieldProgParticipation;
        private DataDictionaryField m_fieldReclassificationDate;
        private DataDictionaryField m_fieldReclassificationExitDate;
        private DataDictionaryField m_fieldReevaluationDate;
        private DataDictionaryField m_fieldStudentPlanDate;
        private DataDictionaryField m_fieldTestContListening;
        private DataDictionaryField m_fieldTestContReading;
        private DataDictionaryField m_fieldTestContSpeaking;
        private DataDictionaryField m_fieldTestDateListening;
        private DataDictionaryField m_fieldTestDateReading;
        private DataDictionaryField m_fieldTestDateSpeaking;
        private DataDictionaryField m_fieldTestDateWriting;
        private DataDictionaryField m_fieldTestFormListening;
        private DataDictionaryField m_fieldTestFormReading;
        private DataDictionaryField m_fieldTestFormSpeaking;
        private DataDictionaryField m_fieldTestFormWriting;
        private DataDictionaryField m_fieldTestLevelListening;
        private DataDictionaryField m_fieldTestLevelReading;
        private DataDictionaryField m_fieldTestLevelSpeaking;
        private DataDictionaryField m_fieldTestLevelWriting;
        private DataDictionaryField m_fieldTestNameListening;
        private DataDictionaryField m_fieldTestNameReading;
        private DataDictionaryField m_fieldTestNameSpeaking;
        private DataDictionaryField m_fieldTestNameWriting;
        private DataDictionaryField m_fieldTestScoreListening;
        private DataDictionaryField m_fieldTestScoreReading;
        private DataDictionaryField m_fieldTestScoreSpeaking;
        private DataDictionaryField m_fieldTestScoreTypeListening;
        private DataDictionaryField m_fieldTestScoreTypeReading;
        private DataDictionaryField m_fieldTestScoreTypeSpeaking;
        private DataDictionaryField m_fieldTestScoreTypeWriting;
        private DataDictionaryField m_fieldTestScoreWriting;
        private DataDictionaryField m_fieldTestSubjectWriting;
        private DataDictionaryField m_fieldTierPlacement;

        /**
         * Instantiates a new retrieve english language learners.
         *
         * @throws X2BaseException the x 2 base exception
         */
        public RetrieveEnglishLanguageLearners() throws X2BaseException {

            StudentProgramDataset pgmEllDataset =
                    getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
            DataDictionary dataDict = pgmEllDataset.getDataDictionary();

            m_fieldProgParticipation = translateAliasToDictionaryField(dataDict, ALIAS_PROG_PARTICIPATION, true);
            m_fieldTierPlacement = translateAliasToDictionaryField(dataDict, ALIAS_TIER_PLACEMENT, true);
            m_fieldStudentPlanDate = translateAliasToDictionaryField(dataDict, ALIAS_STUDENT_PLAN_DATE, true);
            m_fieldClassificationDate = translateAliasToDictionaryField(dataDict, ALIAS_CLASSIFICATION_DATE, true);
            m_fieldFundSrc = translateAliasToDictionaryField(dataDict, ALIAS_FUND_SRC, true);
            m_fieldReevaluationDate = translateAliasToDictionaryField(dataDict, ALIAS_REEVALUATION_DATE, true);
            m_fieldExtensionofInstruction = translateAliasToDictionaryField(dataDict, ALIAS_EXT_OF_INSTRUCTION, true);
            m_fieldReclassificationDate = translateAliasToDictionaryField(dataDict, ALIAS_RECLASSIFICATION_DATE, true);
            m_fieldReclassificationExitDate = translateAliasToDictionaryField(dataDict, ALIAS_RECLASS_EXIT_DATE, true);
            m_fieldBasisofEntry = translateAliasToDictionaryField(dataDict, ALIAS_BASIS_OF_ENTRY, true);
            m_fieldBasisofExitFirst = translateAliasToDictionaryField(dataDict, ALIAS_BASIS_OF_EXIT_FIRST, true);
            m_fieldBasisofExitSecond = translateAliasToDictionaryField(dataDict, ALIAS_BASIS_OF_EXIT_SECOND, true);
            m_fieldTestNameListening = translateAliasToDictionaryField(dataDict, ALIAS_TEST_NAME_LISTENING, true);
            m_fieldTestScoreTypeListening = translateAliasToDictionaryField(dataDict, ALIAS_TEST_SC_T_LISTENING, true);
            m_fieldTestContListening = translateAliasToDictionaryField(dataDict, ALIAS_TEST_CONT_LISTENING, true);
            m_fieldTestScoreListening = translateAliasToDictionaryField(dataDict, ALIAS_TEST_SCORE_LISTENING, true);
            m_fieldTestDateListening = translateAliasToDictionaryField(dataDict, ALIAS_TEST_DATE_LISTENING, true);
            m_fieldTestNameSpeaking = translateAliasToDictionaryField(dataDict, ALIAS_TEST_NAME_SPEAKING, true);
            m_fieldTestScoreTypeSpeaking = translateAliasToDictionaryField(dataDict, ALIAS_TEST_SC_T_SPEAKING, true);
            m_fieldTestContSpeaking = translateAliasToDictionaryField(dataDict, ALIAS_TEST_CONT_SPEAKING, true);
            m_fieldTestScoreSpeaking = translateAliasToDictionaryField(dataDict, ALIAS_TEST_SCORE_SPEAKING, true);
            m_fieldTestDateSpeaking = translateAliasToDictionaryField(dataDict, ALIAS_TEST_DATE_SPEAKING, true);
            m_fieldTestNameReading = translateAliasToDictionaryField(dataDict, ALIAS_TEST_NAME_READING, true);
            m_fieldTestScoreTypeReading = translateAliasToDictionaryField(dataDict, ALIAS_TEST_SC_T_READING, true);
            m_fieldTestContReading = translateAliasToDictionaryField(dataDict, ALIAS_TEST_CONT_READING, true);
            m_fieldTestScoreReading = translateAliasToDictionaryField(dataDict, ALIAS_TEST_SCORE_READING, true);
            m_fieldTestDateReading = translateAliasToDictionaryField(dataDict, ALIAS_TEST_DATE_READING, true);
            m_fieldTestNameWriting = translateAliasToDictionaryField(dataDict, ALIAS_TEST_NAME_WRITING, true);
            m_fieldTestScoreTypeWriting = translateAliasToDictionaryField(dataDict, ALIAS_TEST_SC_T_WRITING, true);
            m_fieldTestSubjectWriting = translateAliasToDictionaryField(dataDict, ALIAS_TEST_SUBJECT_WRITING, true);
            m_fieldTestScoreWriting = translateAliasToDictionaryField(dataDict, ALIAS_TEST_SCORE_WRITING, true);
            m_fieldTestDateWriting = translateAliasToDictionaryField(dataDict, ALIAS_TEST_DATE_WRITING, true);
            m_fieldTestFormListening = translateAliasToDictionaryField(dataDict, ALIAS_TEST_FORM_LISTENING, true);
            m_fieldTestLevelListening = translateAliasToDictionaryField(dataDict, ALIAS_TEST_LEVEL_LISTENING, true);
            m_fieldTestFormSpeaking = translateAliasToDictionaryField(dataDict, ALIAS_TEST_FORM_SPEAKING, true);
            m_fieldTestLevelSpeaking = translateAliasToDictionaryField(dataDict, ALIAS_TEST_LEVEL_SPEAKING, true);
            m_fieldTestFormReading = translateAliasToDictionaryField(dataDict, ALIAS_TEST_FORM_READING, true);
            m_fieldTestLevelReading = translateAliasToDictionaryField(dataDict, ALIAS_TEST_LEVEL_READING, true);
            m_fieldTestFormWriting = translateAliasToDictionaryField(dataDict, ALIAS_TEST_FORM_WRITING, true);
            m_fieldTestLevelWriting = translateAliasToDictionaryField(dataDict, ALIAS_TEST_LEVEL_WRITING, true);
        }

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

            FLEnglishLanguageLearnersEntity flEntity = (FLEnglishLanguageLearnersEntity) entity;
            FLEnglishLanguageLearnersData flData = (FLEnglishLanguageLearnersData) data;

            String parameter = (String) field.getParameter();
            StudentProgramParticipation currentPgm = flEntity.getStudentProgram();
            boolean noRWTestScores = flEntity.getProgramCode().equals(FLEnglishLanguageLearnersData.ELL_CODE_ACTIVE);

            Object value = null;

            switch (parameter) {
                case PARAM_ENTRY_DATE:
                    value = currentPgm.getStartDate();
                    break;
                case PARAM_EXIT_DATE:
                    value = currentPgm.getEndDate();
                    break;
                case PARAM_STUDENT_PLAN_DATE:
                    value = flData.getFieldValue(currentPgm, m_fieldStudentPlanDate);
                    break;
                case PARAM_REEVALUATION_DATE:
                    value = flData.getFieldValue(currentPgm, m_fieldReevaluationDate);
                    break;
                case PARAM_PROG_PARTICIPATION:
                    value = flData.getFieldValue(currentPgm, m_fieldProgParticipation);
                    break;
                case PARAM_TIER_PLACEMENT:
                    if (SURVEY_PERIOD_2.equals(getSurveyPeriod().getCode())) {
                        value = flData.getFieldValue(currentPgm, m_fieldTierPlacement);
                    }
                    break;
                case PARAM_EXTENSION_OF_INSTRUCTION:
                    value = flData.getFieldValue(currentPgm, m_fieldExtensionofInstruction);
                    break;
                case PARAM_CLASSIFICATION_DATE:
                    value = flData.getFieldValue(currentPgm, m_fieldClassificationDate);
                    break;
                case PARAM_FUND_SRC:
                    value = flData.getFieldValue(currentPgm, m_fieldFundSrc);
                    break;
                case PARAM_RECLASSIFICATION_DATE:
                    value = flData.getFieldValue(currentPgm, m_fieldReclassificationDate);
                    break;
                case PARAM_RECLASSIFICATION_EXIT_DATE:
                    value = flData.getFieldValue(currentPgm, m_fieldReclassificationExitDate);
                    break;
                case PARAM_BASIS_OF_ENTRY:
                    value = flData.getFieldValue(currentPgm, m_fieldBasisofEntry);
                    break;
                case PARAM_BASIS_OF_EXIT_FIRST:
                    value = flData.getFieldValue(currentPgm, m_fieldBasisofExitFirst);
                    break;
                case PARAM_BASIS_OF_EXIT_SECOND:
                    value = flData.getFieldValue(currentPgm, m_fieldBasisofExitSecond);
                    break;
                case PARAM_TEST_NAME_LISTENING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestNameListening);
                    break;
                case PARAM_TEST_SCORE_LISTENING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestScoreListening);
                    break;
                case PARAM_TEST_CONT_LISTENING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestContListening);
                    break;
                case PARAM_TEST_SCORE_TYPE_LISTENING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestScoreTypeListening);
                    break;
                case PARAM_TEST_DATE_LISTENING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestDateListening);
                    break;
                case PARAM_TEST_NAME_SPEAKING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestNameSpeaking);
                    break;
                case PARAM_TEST_SCORE_SPEAKING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestScoreSpeaking);
                    break;
                case PARAM_TEST_SCORE_TYPE_SPEAKING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestScoreTypeSpeaking);
                    break;
                case PARAM_TEST_CONT_SPEAKING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestContSpeaking);
                    break;
                case PARAM_TEST_DATE_SPEAKING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestDateSpeaking);
                    break;
                case PARAM_TEST_NAME_READING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestNameReading);
                    break;
                case PARAM_TEST_SCORE_READING:
                    if (!noRWTestScores) {
                        value = flData.getFieldValue(currentPgm, m_fieldTestScoreReading);
                    }
                    break;
                case PARAM_TEST_SCORE_TYPE_READING:
                    if (!noRWTestScores) {
                        value = flData.getFieldValue(currentPgm, m_fieldTestScoreTypeReading);
                    }
                    break;
                case PARAM_TEST_CONT_READING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestContReading);
                    break;
                case PARAM_TEST_DATE_READING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestDateReading);
                    break;
                case PARAM_TEST_NAME_WRITING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestNameWriting);
                    break;
                case PARAM_TEST_SCORE_WRITING:
                    if (!noRWTestScores) {
                        value = flData.getFieldValue(currentPgm, m_fieldTestScoreWriting);
                    }
                    break;
                case PARAM_TEST_SCORE_TYPE_WRITING:
                    if (!noRWTestScores) {
                        value = flData.getFieldValue(currentPgm, m_fieldTestScoreTypeWriting);
                    }
                    break;
                case PARAM_TEST_SUBJECT_WRITING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestSubjectWriting);
                    break;
                case PARAM_TEST_DATE_WRITING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestDateWriting);
                    break;
                case PARAM_TEST_FORM_LISTENING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestFormListening);
                    break;
                case PARAM_TEST_LEVEL_LISTENING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestLevelListening);
                    break;
                case PARAM_TEST_FORM_SPEAKING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestFormSpeaking);
                    break;
                case PARAM_TEST_LEVEL_SPEAKING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestLevelSpeaking);
                    break;
                case PARAM_TEST_FORM_READING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestFormReading);
                    break;
                case PARAM_TEST_LEVEL_READING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestLevelReading);
                    break;
                case PARAM_TEST_FORM_WRITING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestFormWriting);
                    break;
                case PARAM_TEST_LEVEL_WRITING:
                    value = flData.getFieldValue(currentPgm, m_fieldTestLevelWriting);
                    break;
            }

            return value;
        }
    }

    protected static final String ELL_CODE_ACTIVE = "LY";
    protected static final String ELL_CODE_FOLLOWUP = "LF";

    protected static final List<String> ELL_REPORTABLE_CODES = Arrays.asList(ELL_CODE_ACTIVE, ELL_CODE_FOLLOWUP);
    protected static final List<String> ELL_SURVEY_PERIOD_VALID_CODES = Arrays.asList(FLStateReportData.SURVEY_PERIOD_2,
            FLStateReportData.SURVEY_PERIOD_3, FLStateReportData.SURVEY_PERIOD_5);

    private static final String ALIAS_ELL_CODE = "pgm-ell-code";

    protected DataDictionaryField m_fieldELLCode;
    protected StudentProgramDataset m_pgmEllDataset;

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

        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID, m_pgmEllDataset.getStudentSubQuery());
        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLEnglishLanguageLearnersEntity.class);


        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Gets the valid survey periods.
     *
     * @return Collection
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getValidSurveyPeriods()
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return ELL_SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Lookup field aliases and paths.
     *
     * @throws X2BaseException the x 2 base exception
     */
    private void initializeFields() throws X2BaseException {
        m_pgmEllDataset =
                getStudentHelper().getStudentProgramDataset(RetrieveEnglishLanguageLearners.DDX_ID, getSurveyPeriod());
        m_fieldELLCode = translateAliasToDictionaryField(m_pgmEllDataset.getDataDictionary(), ALIAS_ELL_CODE, true);
    }

    /**
     * Register custom field Retrievers.
     *
     * @throws X2BaseException the x 2 base exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveEnglishLanguageLearners.CALC_ID, new RetrieveEnglishLanguageLearners());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        super.addValidators(validators);
    }
}
