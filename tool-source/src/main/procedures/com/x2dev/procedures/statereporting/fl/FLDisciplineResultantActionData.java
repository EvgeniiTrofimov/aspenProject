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
import com.ibm.icu.math.BigDecimal;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentConductDataset;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLDisciplineResultantActionData.
 */
public class FLDisciplineResultantActionData extends FLStateReportData {

    /**
     * The Class FLDisciplineResultantActionEntity.
     */
    public static class FLDisciplineResultantActionEntity extends FLStateReportEntity {

        private FLDisciplineResultantActionData m_data;
        private ConductAction m_record;

        /**
         * Instantiates a new FL discipline resultant action entity.
         */
        public FLDisciplineResultantActionEntity() {
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

            ConductAction action = (ConductAction) getBean();
            String name =
                    "[" + action.getIncident().getIncidentId() + ", " + action.getStudent().getNameView() + "]";

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

            m_data = (FLDisciplineResultantActionData) data;
            m_record = (ConductAction) bean;

            Object value = m_data.getFieldValue(m_record.getIncident(), m_data.m_fieldInvolventType);
            if (value != null) {
                String involventType = value.toString();
                if (involventType.contentEquals("N") || involventType.contentEquals("U")) {
                    setRowCount(0);
                }
            }
        }
    }

    /**
     * Field retriever for Action Duration field.
     */
    protected class RetrieveActionDuration implements FieldRetriever {

        public static final String CALC_ID = "CALC_ACT_DURATION";

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
            ConductAction action = (ConductAction) entity.getBean();

            Object value = BigDecimal.ZERO;
            if (action.getActionPenaltyTime() != null) {
                value = action.getActionPenaltyTime().setScale(0, RoundingMode.DOWN);
            }

            return value;
        }
    }

    /**
     * Field retriever for English Language Learner Code field.
     */
    protected class RetrieveEnglishLanguageLearnerCode implements FieldRetriever {

        public static final String CALC_ID = "CALC_ELL";
        public static final String DDX_ID = "FL-PGM-ELL";

        private static final String ALIAS_ELL_CODE = "pgm-ell-code";
        private DataDictionaryField m_fieldELLCode;

        /**
         * Instantiates a new retrieve english language learner code.
         *
         * @throws X2BaseException exception
         */
        public RetrieveEnglishLanguageLearnerCode() throws X2BaseException {
            StudentProgramDataset pgmEllDataset =
                    getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
            DataDictionary dataDict = pgmEllDataset.getDataDictionary();

            m_fieldELLCode = translateAliasToDictionaryField(dataDict, ALIAS_ELL_CODE, true);
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

            FLDisciplineResultantActionData flData = (FLDisciplineResultantActionData) data;
            FLDisciplineResultantActionEntity flEntity = (FLDisciplineResultantActionEntity) entity;
            ConductAction action = (ConductAction) flEntity.getBean();

            Object value = null;
            StudentProgramParticipation program = getStudentHelper().getStudentProgram(action.getStudent().getOid(),
                    DDX_ID, flData.getSurveyPeriod());
            if (program != null) {
                value = flData.getFieldValue(program, m_fieldELLCode);
            }

            return value;
        }
    }

    /**
     * Field retriever for student grade level code.
     */
    protected class RetrieveGradeLevel implements FieldRetriever {
        public static final String CALC_ID = "CALC_GRADE_LEVEL";

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
            SisStudent student = ((ConductAction) entity.getBean()).getStudent();
            StudentInfo info = getStudentHelper().getStudentInfo(student);
            return info.getGradeLevel(((FLStateReportData) data).getSurveyPeriod().getDateCertain());
        }
    }

    /**
     * Field retriever for Lunch Status field.
     */
    protected class RetrieveLunchStatus implements FieldRetriever {

        public static final String CALC_ID = "CALC_LUNCH_STATUS";
        public static final String DDX_ID = "FL-PGM-LUNCH";
        private static final String ALIAS_LUNCH_STATUS_CODE = "pgm-lunch-status";
        private DataDictionaryField m_fieldLunchStatus;

        /**
         * Instantiates a new retrieve lunch status.
         *
         * @throws X2BaseException exception
         */
        public RetrieveLunchStatus() throws X2BaseException {
            StudentProgramDataset pgmLunchStatusDataset =
                    getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
            DataDictionary dataDict = pgmLunchStatusDataset.getDataDictionary();

            m_fieldLunchStatus = translateAliasToDictionaryField(dataDict, ALIAS_LUNCH_STATUS_CODE, true);
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

            FLDisciplineResultantActionData flData = (FLDisciplineResultantActionData) data;
            FLDisciplineResultantActionEntity flEntity = (FLDisciplineResultantActionEntity) entity;
            ConductAction action = (ConductAction) flEntity.getBean();

            Object value = null;
            StudentProgramParticipation program = getStudentHelper().getStudentProgram(action.getStudent().getOid(),
                    DDX_ID, flData.getSurveyPeriod());
            if (program != null) {
                value = flData.getFieldValue(program, m_fieldLunchStatus);
            }

            return value;
        }
    }

    // Export input parameters
    /**
     * Instance variables.
     */
    static final String ALIAS_INVOLVENT_TYPE = "all-cnd-OffenderType";
    protected static final List<String> SDRA_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_2,
                    FLStateReportData.SURVEY_PERIOD_3, FLStateReportData.SURVEY_PERIOD_5);
    private DataDictionaryField m_fieldInvolventType;

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

        getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                getCurrentContext().getStartDate());
        getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);

        StudentConductDataset conductDataset = getStudentHelper()
                .getStudentConductDataset(getCurrentContext().getStartDate(), getSurveyPeriod().getEndDate());

        X2Criteria actionCriteria = conductDataset.getActionCriteria().copy();
        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID,
                new SubQuery(ConductAction.class, ConductAction.COL_STUDENT_OID, actionCriteria));
        SubQuery studentSubQuery =
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getStudentHelper().getStudentCriteria());
        Collection<String> studentsOids = getBroker().getSubQueryCollectionByQuery(studentSubQuery);
        actionCriteria.addIn(ConductAction.COL_STUDENT_OID, studentsOids);
        QueryByCriteria actionQuery = new QueryByCriteria(ConductAction.class, actionCriteria, true);
        setQuery(actionQuery);
        setEntityClass(FLDisciplineResultantActionEntity.class);

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
        return SDRA_SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Lookup field aliases and paths.
     *
     */
    private void initializeFields() {

        m_fieldInvolventType = translateAliasToDictionaryField(ALIAS_INVOLVENT_TYPE, true);
    }

    /**
     * Register custom field Retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {

        HashMap calcs = new HashMap<String, FieldRetriever>();

        calcs.put(RetrieveEnglishLanguageLearnerCode.CALC_ID, new RetrieveEnglishLanguageLearnerCode());
        calcs.put(RetrieveLunchStatus.CALC_ID, new RetrieveLunchStatus());
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveActionDuration.CALC_ID, new RetrieveActionDuration());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        calcs.put(RetrieveGradeLevel.CALC_ID, new RetrieveGradeLevel());

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
