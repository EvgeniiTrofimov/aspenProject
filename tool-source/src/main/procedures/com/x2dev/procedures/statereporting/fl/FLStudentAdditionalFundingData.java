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
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLStudentAdditionalFundingData.
 */
public class FLStudentAdditionalFundingData extends FLStateReportData {

    /**
     * The Class FLStudentAdditionalFundingEntity.
     */
    public static class FLStudentAdditionalFundingEntity extends FLStateReportEntity {
        private FLStudentAdditionalFundingData m_data;
        private SisStudent m_record;

        /**
         * Instantiates a new FL student additional funding entity.
         */
        public FLStudentAdditionalFundingEntity() {
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

            m_data = (FLStudentAdditionalFundingData) data;
            m_record = (SisStudent) getBean();
            setRowCount(m_data.getStudentHelper().isStudentEligible(m_record) ? 1 : 0);
        }
    }

    /**
     * Field retriever for FTE score fields.
     */
    protected class RetrieveFTEScore implements FieldRetriever {
        public static final String CALC_ID = "FTE_SCORE";

        public static final String CALC_PARAM_AICE = "AICE";
        public static final String CALC_PARAM_AP = "AP";
        public static final String CALC_PARAM_IB = "IB";

        private static final String ASD_ID_AP = "AP";
        private static final String ASD_ID_AICE = "AICE";
        private static final String ASD_ID_IB = "IB";

        private static final String FIELD_ALIAS_AICE_SCORE = "asm-aice-score";
        private static final String FIELD_ALIAS_AP_SCORE = "asm-ap-score";
        private static final String FIELD_ALIAS_IB_SCORE = "asm-ib-score";

        private DataDictionaryField m_fieldAICEScore;
        private DataDictionaryField m_fieldAPScore;
        private DataDictionaryField m_fieldIBScore;

        /**
         * Instantiates a new retrieve FTE score.
         *
         * @throws X2BaseException exception
         */
        public RetrieveFTEScore() throws X2BaseException {

        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            FLStudentAdditionalFundingEntity flEntity = (FLStudentAdditionalFundingEntity) entity;
            FLStudentAdditionalFundingData flData = (FLStudentAdditionalFundingData) data;

            double value = 0;
            Object scoreObj;
            double score;

            String parameter = (String) field.getParameter();

            SisStudent student = flEntity.getCurrentRecord();
            List<StudentAssessment> assessments = getStudentHelper().getAssessments(student);

            for (StudentAssessment assessment : assessments) {
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(assessment.getExtendedDataDictionary(),
                        assessment.getPersistenceKey());

                switch (parameter) {
                    case CALC_PARAM_AP:
                        m_fieldAPScore = translateAliasToDictionaryField(dictionary, FIELD_ALIAS_AP_SCORE, true);

                        if (!assessment.getAssessmentDefinition().getId().contentEquals(ASD_ID_AP)) {
                            continue;
                        }

                        scoreObj = flData.getFieldValue(assessment, m_fieldAPScore);
                        if (scoreObj == null) {
                            continue;
                        }

                        score = Double.valueOf(scoreObj.toString()).doubleValue();
                        if (score >= 3) {
                            value = value + 0.16;
                        }

                        break;
                    case CALC_PARAM_AICE:
                        // TODO: determine one-half credit courses to add 0.08 instead of 0.16

                        m_fieldAICEScore = translateAliasToDictionaryField(dictionary, FIELD_ALIAS_AICE_SCORE, true);

                        if (!assessment.getAssessmentDefinition().getId().contentEquals(ASD_ID_AICE)) {
                            continue;
                        }

                        scoreObj = flData.getFieldValue(assessment, m_fieldAICEScore);
                        if (scoreObj == null) {
                            continue;
                        }

                        score = Double.valueOf(scoreObj.toString()).doubleValue();

                        // Scores: 8(A*), 7(A), 6(B), 5(C), 4(D), 3(E), 2(F), 1(G), 0(U)
                        if (score >= 3) {
                            value = value + 0.16;
                        }

                        break;
                    case CALC_PARAM_IB:
                        m_fieldIBScore = translateAliasToDictionaryField(dictionary, FIELD_ALIAS_IB_SCORE, true);

                        if (!assessment.getAssessmentDefinition().getId().contentEquals(ASD_ID_IB)) {
                            continue;
                        }

                        scoreObj = flData.getFieldValue(assessment, m_fieldIBScore);
                        if (scoreObj == null) {
                            continue;
                        }

                        score = Double.valueOf(scoreObj.toString()).doubleValue();
                        if (score >= 4) {
                            value = value + 0.16;
                        }

                        break;
                }
            }

            return String.format("%.2f", Double.valueOf(value));
        }
    }

    /**
     * Field retriever for FTE fields assigned with withdrawal codes.
     */
    protected class RetrieveFTEWithdrawal implements FieldRetriever {
        public static final String CALC_ID = "FTE_WITHDRAWAL";

        public static final String CALC_PARAM_AICE = "AICE";

        public static final String CALC_PARAM_EG = "EG";

        public static final String CALC_PARAM_IB = "IB";
        public static final String FIELD_ALIAS_AICE_DIPLOMA_EARNED = "all-std-AICEDiplomaEarned";

        public static final String FIELD_ALIAS_EARLY_GRADUATE = "all-std-EarlyGraduate";

        public static final String FIELD_ALIAS_IB_DIPLOMA_EARNED = "all-std-IBDiplomaEarned";
        private DataDictionaryField m_fieldAICEDiplomaEarned;

        private DataDictionaryField m_fieldEarlyGraduate;

        private DataDictionaryField m_fieldIBDiplomaEarned;

        /**
         * Instantiates a new retrieve FTE withdrawal.
         *
         * @throws X2BaseException exception
         */
        public RetrieveFTEWithdrawal() throws X2BaseException {
            m_fieldIBDiplomaEarned = translateAliasToDictionaryField(FIELD_ALIAS_IB_DIPLOMA_EARNED, true);
            m_fieldAICEDiplomaEarned = translateAliasToDictionaryField(FIELD_ALIAS_AICE_DIPLOMA_EARNED, true);
            m_fieldEarlyGraduate = translateAliasToDictionaryField(FIELD_ALIAS_EARLY_GRADUATE, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            FLStudentAdditionalFundingEntity flEntity = (FLStudentAdditionalFundingEntity) entity;
            FLStudentAdditionalFundingData flData = (FLStudentAdditionalFundingData) data;

            double value = 0;

            String parameter = (String) field.getParameter();

            SisStudent student = flEntity.getCurrentRecord();

            List<String> withdrawalCodes = Arrays.asList("W06", "W07", "W08", "W8A", "W8B", "W09", "W10");

            List<StudentEnrollment> studentEnrollments = getStudentHelper().getStudentEnrollments(student);

            switch (parameter) {
                case CALC_PARAM_IB:
                    if (studentEnrollments != null && !studentEnrollments.isEmpty()) {
                        for (StudentEnrollment studentEnrollment : studentEnrollments) {
                            if (studentEnrollment.getEnrollmentType() == null) {
                                continue;
                            }
                            if (studentEnrollment.getEnrollmentCode() == null) {
                                continue;
                            }

                            String studentEnrollmentTypeAndCode =
                                    studentEnrollment.getEnrollmentType() + studentEnrollment.getEnrollmentCode();

                            Boolean studentIBDiplomaEarned =
                                    (Boolean) flData.getFieldValue(student, m_fieldIBDiplomaEarned);

                            if (studentIBDiplomaEarned != null && studentIBDiplomaEarned.booleanValue() == true
                                    && withdrawalCodes.contains(studentEnrollmentTypeAndCode)) {
                                value = 0.30;

                                break;
                            }
                        }
                    }

                    break;
                case CALC_PARAM_AICE:
                    if (studentEnrollments != null && !studentEnrollments.isEmpty()) {
                        for (StudentEnrollment studentEnrollment : studentEnrollments) {
                            if (studentEnrollment.getEnrollmentType() == null) {
                                continue;
                            }
                            if (studentEnrollment.getEnrollmentCode() == null) {
                                continue;
                            }

                            String studentEnrollmentTypeAndCode =
                                    studentEnrollment.getEnrollmentType() + studentEnrollment.getEnrollmentCode();

                            Boolean studentAICEDiplomaEarned =
                                    (Boolean) flData.getFieldValue(student, m_fieldAICEDiplomaEarned);

                            if (studentAICEDiplomaEarned != null && studentAICEDiplomaEarned.booleanValue() == true
                                    && withdrawalCodes.contains(studentEnrollmentTypeAndCode)) {
                                value = 0.30;

                                break;
                            }
                        }
                    }

                    break;
                case CALC_PARAM_EG:
                    if (studentEnrollments != null && !studentEnrollments.isEmpty()) {
                        for (StudentEnrollment studentEnrollment : studentEnrollments) {
                            if (studentEnrollment.getEnrollmentType() == null) {
                                continue;
                            }
                            if (studentEnrollment.getEnrollmentCode() == null) {
                                continue;
                            }

                            String studentEnrollmentTypeAndCode =
                                    studentEnrollment.getEnrollmentType() + studentEnrollment.getEnrollmentCode();

                            Object studentEarlyGraduate = flData.getFieldValue(student, m_fieldEarlyGraduate);

                            if (studentEarlyGraduate != null
                                    && withdrawalCodes.contains(studentEnrollmentTypeAndCode)) {
                                value = Double.valueOf(studentEarlyGraduate.toString()).doubleValue();

                                break;
                            }
                        }
                    }

                    break;
            }

            switch (parameter) {
                case CALC_PARAM_EG:
                    return String.format("%.4f", Double.valueOf(value));
                default:
                    return String.format("%.2f", Double.valueOf(value));
            }
        }
    }

    protected static final List<String> CUSTOM_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_5);



    /**
     * Initialize the data module. Initialize necessary working resources. Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        if (getSetupErrors().size() != 0) {
            return;
        }

        X2Criteria assessmentDefinitionCriteria = new X2Criteria();
        assessmentDefinitionCriteria.addInIgnoreCase(AssessmentDefinition.COL_ID, Arrays.asList("AP", "IB", "AICE"));
        SubQuery assessmentDefinitionQuery =
                new SubQuery(AssessmentDefinition.class, X2BaseBean.COL_OID, assessmentDefinitionCriteria);

        X2Criteria studentAssessmentCriteria = new X2Criteria();
        studentAssessmentCriteria.addIn(
                StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                assessmentDefinitionQuery);
        SubQuery studentAssessmentQuery =
                new SubQuery(StudentAssessment.class, X2BaseBean.COL_OID, studentAssessmentCriteria);

        X2Criteria studentCriteria = getStudentHelper().getStudentCriteria();
        studentCriteria.addIn(SisStudent.REL_STUDENT_ASSESSMENTS + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                studentAssessmentQuery);
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria, true);

        setQuery(studentQuery);
        setEntityClass(FLStudentAdditionalFundingEntity.class);

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
     * Register custom field Retrievers.
     *
     * @throws X2BaseException exception
     */

    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveFTEScore.CALC_ID, new RetrieveFTEScore());
        calcs.put(RetrieveFTEWithdrawal.CALC_ID, new RetrieveFTEWithdrawal());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */

    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<>();
        validators.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        super.addValidators(validators);
    }
}
