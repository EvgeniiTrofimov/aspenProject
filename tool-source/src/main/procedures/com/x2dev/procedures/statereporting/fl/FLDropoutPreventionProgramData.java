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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

/**
 * The Class FLDropoutPreventionProgramData.
 */
public class FLDropoutPreventionProgramData extends FLStateReportData {

    /**
     * The Class FLDropoutPreventionProgramEntity.
     */
    public static class FLDropoutPreventionProgramEntity extends FLStateReportEntity {
        private FLDropoutPreventionProgramData m_data;
        private List<DropoutProgramInfo> m_programInfos;
        private SisStudent m_record;

        /**
         * Instantiates a new FL dropout prevention program entity.
         */
        public FLDropoutPreventionProgramEntity() {
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
         * Gets the dropout program info.
         *
         * @return Dropout program info
         */
        public DropoutProgramInfo getDropoutProgramInfo() {
            return m_programInfos.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = m_record;
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

            m_data = (FLDropoutPreventionProgramData) data;
            m_record = (SisStudent) getBean();
            m_programInfos = null;
            if (m_data.getStudentHelper().isStudentEligible(m_record)) {
                List<StudentProgramParticipation> list = m_data.m_pgmDataset.getPrograms(m_record.getOid());
                if (list != null && !list.isEmpty()) {
                    m_programInfos = new ArrayList(list.size());
                    for (StudentProgramParticipation pgm : list) {
                        m_programInfos.add(m_data.new DropoutProgramInfo(pgm));
                    }
                }

                if (m_programInfos != null) {
                    setRowCount(m_programInfos.size());
                } else {
                    setRowCount(0);
                }
            }
        }
    }

    /**
     * The Class DropoutProgramInfo.
     */
    protected class DropoutProgramInfo {
        private StudentProgramParticipation m_program;

        /**
         * Instantiates a new dropout program info.
         *
         * @param program StudentProgramParticipation
         */
        public DropoutProgramInfo(StudentProgramParticipation program) {
            m_program = program;
        }

        /**
         * Gets the program.
         *
         * @return Student program participation
         */
        public StudentProgramParticipation getProgram() {
            return m_program;
        }
    }

    /**
     * Field retriever for Dropout Program fields.
     */
    protected class RetrieveDropoutPrevention implements FieldRetriever {
        public static final String CALC_ID = "PGM_DROPOUT";
        public static final String DDX_ID = "FL-PGM-DROP";

        // private static final String ALIAS_ADJUDICATION_START = "pgm-adjudication-start";
        // private static final String ALIAS_ADJUDICATION_END = "pgm-adjudication-end";
        private static final String ALIAS_DROPOUT_CODE = "pgm-dropout-code";
        private static final String ALIAS_FUND_SRC = "pgm-drp-fund-src";
        private static final String ALIAS_PARTICIPATION_LENGTH = "pgm-participation-length";
        private static final String ALIAS_PRESCRIBED_LENGTH = "pgm-prescribed-length";
        private static final String ALIAS_PRETEST_MATH = "pgm-pretest-math";
        private static final String ALIAS_PRETEST_READING = "pgm-pretest-reading";
        private static final String ALIAS_PROGRESS_MATH = "pgm-progress-math";
        private static final String ALIAS_PROGRESS_READING = "pgm-progress-reading";
        private static final String ALIAS_TERM_CODE = "pgm-term-code";

        private static final String PARAM_ENROLLMENT_DATE = "ENROLLMENT_DATE";
        private static final String PARAM_FUND_SRC = "FUND_SRC";
        private static final String PARAM_PARTICIPATION_LENGTH = "PARTICIPATION_LENGTH";
        private static final String PARAM_PGM_CODE = "PGM_CODE";
        private static final String PARAM_PRESCRIBED_LENGTH = "PRESCRIBED_LENGTH";
        private static final String PARAM_PRETEST_MATH = "PRETEST_MATH";
        private static final String PARAM_PRETEST_READING = "PRETEST_READING";
        private static final String PARAM_PROGRESS_MATH = "PROGRESS_MATH";
        private static final String PARAM_PROGRESS_READING = "PROGRESS_READING";
        private static final String PARAM_TERM = "TERM";
        private static final String PARAM_WITHDRAWAL_DATE = "WITHDRAWAL_DATE";

        // private DataDictionaryField m_fieldAdjudicationStart;
        // private DataDictionaryField m_fieldAdjudicationEnd;
        private DataDictionaryField m_fieldDropoutCode;
        private DataDictionaryField m_fieldFundSrc;
        private DataDictionaryField m_fieldParticipationLength;
        private DataDictionaryField m_fieldPrescribedLength;
        private DataDictionaryField m_fieldPretestMath;
        private DataDictionaryField m_fieldPretestReading;
        private DataDictionaryField m_fieldProgressMath;
        private DataDictionaryField m_fieldProgressReading;
        private DataDictionaryField m_fieldScheduleTermCode;

        /**
         * Instantiates a new retrieve dropout prevention.
         *
         * @throws X2BaseException exception
         */
        public RetrieveDropoutPrevention() throws X2BaseException {
            StudentProgramDataset pgmDropoutDataset = getStudentHelper().getStudentProgramDataset(DDX_ID,
                    getSurveyPeriod());
            DataDictionary dataDictionary = pgmDropoutDataset.getDataDictionary();
            // m_fieldAdjudicationStart = translateAliasToDictionaryField(dataDictionary,
            // ALIAS_ADJUDICATION_START, true);
            // m_fieldAdjudicationEnd = translateAliasToDictionaryField(dataDictionary,
            // ALIAS_ADJUDICATION_END, true);
            m_fieldDropoutCode = translateAliasToDictionaryField(dataDictionary, ALIAS_DROPOUT_CODE, true);
            m_fieldFundSrc = translateAliasToDictionaryField(dataDictionary, ALIAS_FUND_SRC, true);
            m_fieldParticipationLength =
                    translateAliasToDictionaryField(dataDictionary, ALIAS_PARTICIPATION_LENGTH, true);
            m_fieldPrescribedLength = translateAliasToDictionaryField(dataDictionary, ALIAS_PRESCRIBED_LENGTH, true);
            m_fieldPretestReading = translateAliasToDictionaryField(dataDictionary, ALIAS_PRETEST_READING, true);
            m_fieldPretestMath = translateAliasToDictionaryField(dataDictionary, ALIAS_PRETEST_MATH, true);
            m_fieldProgressReading = translateAliasToDictionaryField(dataDictionary, ALIAS_PROGRESS_READING, true);
            m_fieldProgressMath = translateAliasToDictionaryField(dataDictionary, ALIAS_PROGRESS_MATH, true);
            m_fieldScheduleTermCode = translateAliasToDictionaryField(dataDictionary, ALIAS_TERM_CODE, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLDropoutPreventionProgramEntity flEntity = (FLDropoutPreventionProgramEntity) entity;
            FLDropoutPreventionProgramData flData = (FLDropoutPreventionProgramData) data;

            DropoutProgramInfo info = flEntity.getDropoutProgramInfo();
            StudentProgramParticipation pgm = info.getProgram();

            Object value = null;
            String parameter = (String) field.getParameter();

            switch (parameter) {
                case PARAM_PGM_CODE:
                    value = flData.getFieldValue(pgm, m_fieldDropoutCode);
                    break;
                case PARAM_PRESCRIBED_LENGTH:
                    value = flData.getFieldValue(pgm, m_fieldPrescribedLength);
                    break;
                case PARAM_PARTICIPATION_LENGTH:
                    value = flData.getFieldValue(pgm, m_fieldParticipationLength);
                    break;
                case PARAM_ENROLLMENT_DATE:
                    value = pgm.getStartDate();
                    break;
                case PARAM_FUND_SRC:
                    value = flData.getFieldValue(pgm, m_fieldFundSrc);
                    break;
                case PARAM_WITHDRAWAL_DATE:
                    value = pgm.getEndDate();
                    break;
                case PARAM_PROGRESS_READING:
                    value = flData.getFieldValue(pgm, m_fieldProgressReading);
                    break;
                case PARAM_PROGRESS_MATH:
                    value = flData.getFieldValue(pgm, m_fieldProgressMath);
                    break;
                case PARAM_PRETEST_READING:
                    value = flData.getFieldValue(pgm, m_fieldPretestReading);
                    break;
                case PARAM_PRETEST_MATH:
                    value = flData.getFieldValue(pgm, m_fieldPretestMath);
                    break;
                case PARAM_TERM:
                    value = flData.getFieldValue(pgm, m_fieldScheduleTermCode);
                    break;
            }
            return value;
        }
    }

    /**
     * Instance variables.
     */
    protected static final List<String> DRP_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_5);

    protected StudentProgramDataset m_pgmDataset;

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

        m_pgmDataset = getStudentHelper().getStudentProgramDataset(
                RetrieveDropoutPrevention.DDX_ID,
                getSurveyPeriod());
        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID,
                m_pgmDataset.getStudentSubQuery());
        X2Criteria stdExtractCriteria = FLStudentExtractData.getExportStudentHelper(this).getStudentCriteria();
        SubQuery stdExtractSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdExtractCriteria);
        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID, stdExtractSubQuery);
        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLDropoutPreventionProgramEntity.class);

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
        calcs.put(RetrieveDropoutPrevention.CALC_ID, new RetrieveDropoutPrevention());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
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
