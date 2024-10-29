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
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentEnrollmentSpanInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.commons.lang3.StringUtils;

/**
 * The Class FLStudentEndOfYearStatusData.
 */
public class FLStudentEndOfYearStatusData extends FLStateReportData {

    /**
     * The Class FLStudentEndOfYearStatusEntity.
     */
    public static class FLStudentEndOfYearStatusEntity extends FLStateReportEntity {
        private FLStudentEndOfYearStatusData m_data;
        private StudentEnrollmentSpanInfo m_enrollment;

        /**
         * Instantiates a new FL student end of year status entity.
         */
        public FLStudentEndOfYearStatusEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current enrollment.
         *
         * @return Student enrollment span info
         */
        public StudentEnrollmentSpanInfo getCurrentEnrollment() {
            return m_enrollment;
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

            m_data = (FLStudentEndOfYearStatusData) data;
            m_enrollment = null;

            SisStudent student = (SisStudent) getBean();
            PlainDate snapShotDate = m_data.getSurveyPeriod().getSnapshotDate();

            if (m_data.getStudentHelper().isStudentEligible(student)) {
                List<StudentEnrollmentSpan> spans =
                        m_data.getStudentHelper().getStudentEnrollmentSpans(student, true);
                if (spans != null && !spans.isEmpty()) {
                    StudentEnrollmentSpan lastSpan = null;
                    for (StudentEnrollmentSpan span : spans) {
                        if (span.getFirstActiveDate() != null && !span.getFirstActiveDate().after(snapShotDate)) {
                            if (lastSpan == null ||
                                    !lastSpan.getFirstActiveDate().after(span.getFirstActiveDate())) {
                                lastSpan = span;
                            }
                        }
                    }
                    if (lastSpan != null) {
                        m_enrollment = m_data.getStudentHelper().new StudentEnrollmentSpanInfo(student, lastSpan);
                    }
                }
            }
            setRowCount((m_enrollment != null) ? 1 : 0);
        }
    }

    /**
     * Field retriever for Career Pathways Student Participant field.
     */
    protected class RetrieveCareerPathways implements FieldRetriever {

        public static final String CALC_ID = "PGM_CAPE";
        public static final String DDX_ID = "FL-PGM-CAPE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            FLStudentEndOfYearStatusData flData = (FLStudentEndOfYearStatusData) data;

            StudentProgramParticipation currentPgm =
                    flData.getStudentHelper().getStudentProgram(entity.getBean().getOid(),
                            RetrieveCareerPathways.DDX_ID, flData.getSurveyPeriod());

            return (currentPgm != null) ? "Y" : "N";
        }
    }

    /**
     * Field retriever for Dropout Program fields.
     */
    protected class RetrieveDropoutPrevention implements FieldRetriever {

        public static final String CALC_ID = "PGM_DROPOUT";
        public static final String DROPOUT_DDX_ID = "FL-PGM-DROP";

        private static final String ALIAS_ACTIONS_TAKEN = "pgm-drp-actions-taken";
        private static final String ALIAS_INFLUENCES = "pgm-drp-pos-influences";
        private static final String ALIAS_PARAM_EXIT_OPT_TEST = "pgm-drp-exit-opt-test";
        private static final String ALIAS_PARAM_PRIMARY_REASON = "pgm-drp-primary-reason";
        private static final String ALIAS_PARAM_SECOND_REASON = "pgm-drp-secondary-reason";

        private DataDictionaryField m_fieldActionsTaken;
        private DataDictionaryField m_fieldExitOptTest;
        private DataDictionaryField m_fieldInfluences;
        private DataDictionaryField m_fieldPrimaryReason;
        private DataDictionaryField m_fieldSecondaryReason;

        /**
         * Instantiates a new retrieve dropout prevention.
         *
         * @throws X2BaseException exception
         */
        public RetrieveDropoutPrevention() throws X2BaseException {

            StudentProgramDataset pgmDropoutDataset = getStudentHelper().getStudentProgramDataset(DROPOUT_DDX_ID,
                    getSurveyPeriod());
            DataDictionary dataDictionary = pgmDropoutDataset.getDataDictionary();

            m_fieldActionsTaken = translateAliasToDictionaryField(dataDictionary, ALIAS_ACTIONS_TAKEN, true);
            m_fieldInfluences = translateAliasToDictionaryField(dataDictionary, ALIAS_INFLUENCES, true);
            m_fieldPrimaryReason = translateAliasToDictionaryField(dataDictionary, ALIAS_PARAM_PRIMARY_REASON, true);
            m_fieldSecondaryReason = translateAliasToDictionaryField(dataDictionary, ALIAS_PARAM_SECOND_REASON, true);
            m_fieldExitOptTest = translateAliasToDictionaryField(dataDictionary, ALIAS_PARAM_EXIT_OPT_TEST, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLStudentEndOfYearStatusEntity flEntity = (FLStudentEndOfYearStatusEntity) entity;
            FLStudentEndOfYearStatusData flData = (FLStudentEndOfYearStatusData) data;

            StudentProgramParticipation pgm =
                    getStudentHelper().getStudentProgram(flEntity.getBean().getOid(), DROPOUT_DDX_ID,
                            flData.getSurveyPeriod());

            Object value = "Z";
            String parameter = (String) field.getParameter();

            if (parameter.contains(ALIAS_ACTIONS_TAKEN)) {
                String values = (String) flData.getFieldValue(pgm, m_fieldActionsTaken);
                if (!StringUtils.isEmpty(values)) {
                    List<String> codes = Arrays.asList((values).split(","));
                    String codeMatch = parameter.substring(parameter.length() - 1).toUpperCase();
                    if (codes.contains(codeMatch)) {
                        value = codeMatch;
                    }
                }
            } else if (parameter.contains(ALIAS_INFLUENCES)) {
                String values = (String) flData.getFieldValue(pgm, m_fieldInfluences);
                if (!StringUtils.isEmpty(values)) {
                    List<String> codes = Arrays.asList((values).split(","));
                    String codeMatch = parameter.substring(parameter.length() - 1).toUpperCase();
                    if (codes.contains(codeMatch)) {
                        value = codeMatch;
                    }
                }
            } else {
                switch (parameter) {
                    case ALIAS_PARAM_PRIMARY_REASON:
                        value = flData.getFieldValue(pgm, m_fieldPrimaryReason);
                        break;
                    case ALIAS_PARAM_SECOND_REASON:
                        value = flData.getFieldValue(pgm, m_fieldSecondaryReason);
                        break;
                    case ALIAS_PARAM_EXIT_OPT_TEST:
                        value = flData.getFieldValue(pgm, m_fieldExitOptTest);
                        break;
                }
            }


            return value;
        }
    }

    /**
     * Field retriever for Grade Point Average Cumulative field.
     */
    protected class RetrieveGradePointAverageCumulative implements FieldRetriever {

        public static final String CALC_ID = "GRADE_POINT_AVERAGE";
        private final List<String> REPORTING_GRADES = Arrays.asList("06", "07", "08", "09", "10", "11", "12");

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLStudentEndOfYearStatusData flData = (FLStudentEndOfYearStatusData) data;
            StudentInfo studentInfo = flData.getStudentHelper().getStudentInfo((SisStudent) entity.getBean());

            Object value = "99999";
            if (REPORTING_GRADES.contains(studentInfo.getGradeLevel(flData.getSurveyPeriod().getSnapshotDate()))) {
                BigDecimal gpa = studentInfo.getStdGradePointAverage();
                if (gpa != null) {
                    value = String.format("%.5s", "" + ((int) (gpa.floatValue() * 10000)));
                }
            }
            return value;
        }
    }

    /**
     * Field retriever for Withdrawal Codes and Date.
     */
    protected class RetrieveWithdrawal implements FieldRetriever {

        public static final String CALC_ID = "WITHDRAW";

        private static final String PARAM_DATE = "DATE";
        private static final String PARAM_REASON = "REASON";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            Object value = null;
            StudentEnrollmentSpanInfo info = ((FLStudentEndOfYearStatusEntity) entity).getCurrentEnrollment();
            String parameter = (String) field.getParameter();

            switch (parameter) {
                case PARAM_REASON:
                    value = info.getEndOfYearWithdrawalCode();
                    break;
                case PARAM_DATE:
                    StudentEnrollment withdrawal = info.getWithdrawalEnrollment();
                    if (withdrawal != null) {
                        value = withdrawal.getEnrollmentDate();
                    }
                    break;
            }

            return value;
        }
    }

    /**
     * Field retriever for Year Entered Ninth Grade field.
     */
    protected class RetrieveYearEnteredNinthGrade implements FieldRetriever {

        public static final String CALC_ID = "NINTH_GRADE";

        private static final String ALIAS_NINTH_GRADE = "all-std-NinthGradeYear";

        private DataDictionaryField m_fieldNinthGrade;

        private final List<String> REPORTING_GRADES = Arrays.asList("09", "10", "11", "12", "PG");

        /**
         * Instantiates a new retrieve year entered ninth grade.
         *
         * @throws X2BaseException exception
         */
        public RetrieveYearEnteredNinthGrade() throws X2BaseException {
            m_fieldNinthGrade = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_NINTH_GRADE);
            if (m_fieldNinthGrade == null) {
                throw new X2RuntimeException(
                        new IllegalStateException("The alias " + ALIAS_NINTH_GRADE + " must be defined."));
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLStudentEndOfYearStatusData flData = (FLStudentEndOfYearStatusData) data;
            StudentInfo studentInfo = flData.getStudentHelper().getStudentInfo((SisStudent) entity.getBean());

            Object value = "00000000";
            if (REPORTING_GRADES.contains(studentInfo.getGradeLevel(flData.getSurveyPeriod().getSnapshotDate()))) {
                Object ninthGrade = flData.getFieldValue(entity.getBean(), m_fieldNinthGrade);
                if (ninthGrade != null) {
                    Integer ninthGradeStartYear = Integer.valueOf(ninthGrade.toString());
                    int ninthGradeEndYear = ninthGradeStartYear.intValue() + 1;
                    value = ninthGradeStartYear.toString() + ninthGradeEndYear;
                }
            }

            return value;
        }
    }


    /**
     * Instance variables.
     */
    protected static final List<String> DRP_SURVEY_PERIOD_VALID_CODES =
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

        initializeFields();

        Collection<StateReportValidationError> errors = getSetupErrors();
        if (errors.size() != 0) {
            return;
        }
        getStudentHelper().setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                getCurrentContext().getStartDate());

        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLStudentEndOfYearStatusEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getValidSurveyPeriods()
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return DRP_SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Lookup field aliases and paths.
     *
     */
    private void initializeFields() {
        // TODO: Add initialization which produces setup errors here
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
        calcs.put(RetrieveDropoutPrevention.CALC_ID, new RetrieveDropoutPrevention());
        calcs.put(RetrieveWithdrawal.CALC_ID, new RetrieveWithdrawal());
        calcs.put(RetrieveYearEnteredNinthGrade.CALC_ID, new RetrieveYearEnteredNinthGrade());
        calcs.put(RetrieveGradePointAverageCumulative.CALC_ID, new RetrieveGradePointAverageCumulative());
        calcs.put(RetrieveCareerPathways.CALC_ID, new RetrieveCareerPathways());
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
