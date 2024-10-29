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

import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentEnrollmentSpanInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentInfo;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

/**
 * The Class FLMcKayPrepaymentVerificationData.
 */
public class FLMcKayPrepaymentVerificationData extends FLStateReportData {

    /**
     * The Class FLMcKayPrepaymentVerificationEntity.
     */
    public static class FLMcKayPrepaymentVerificationEntity extends FLStateReportEntity {
        private FLMcKayPrepaymentVerificationData m_data;
        private StudentEnrollmentSpanInfo m_enrollment;
        private SisStudent m_record;

        /**
         * Instantiates a new FL mc kay prepayment verification entity.
         */
        public FLMcKayPrepaymentVerificationEntity() {
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

            m_data = (FLMcKayPrepaymentVerificationData) data;
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
     * Field retriever for Military Family Student field.
     */
    protected class RetrieveMilitaryFamilyStudent implements FieldRetriever {
        public static final String CALC_ID = "PGM_MFS";

        private static final String DEFAULT_VALUE = "N";
        private static final String PK_VALUE = "Z";
        private static final String TRUE_VALUE = "Y";

        private static final String MFS_DDX_ID = "FL-PGM-MFS";

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
            String value = DEFAULT_VALUE;
            FLMcKayPrepaymentVerificationEntity flEntity = (FLMcKayPrepaymentVerificationEntity) entity;
            FLMcKayPrepaymentVerificationData flData = (FLMcKayPrepaymentVerificationData) data;
            StudentInfo info = getStudentHelper().getStudentInfo((SisStudent) flEntity.getBean());
            String gradeLevel = info.getGradeLevel(flData.getSurveyPeriod().getStartDate());
            if (FLStudentHelper.GRADE_LEVEL_PK.equals(gradeLevel)) {
                value = PK_VALUE;
            } else {
                List<StudentProgramParticipation> pgms =
                        getStudentHelper().getStudentPrograms(flEntity.getBean().getOid(),
                                MFS_DDX_ID, flData.getSurveyPeriod());
                if (pgms != null && !pgms.isEmpty()) {
                    value = TRUE_VALUE;
                }

            }

            return value;
        }
    }

    /**
     * Field retriever for Withdrawal Date.
     */
    protected class RetrieveWithdrawal implements FieldRetriever {

        public static final String CALC_ID = "WITHDRAW_DATE";

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

            Object value = null;
            StudentEnrollmentSpanInfo info = ((FLMcKayPrepaymentVerificationEntity) entity).getCurrentEnrollment();

            StudentEnrollment withdrawal = info.getWithdrawalEnrollment();
            if (withdrawal != null) {
                value = withdrawal.getEnrollmentDate();
            }

            return value;
        }
    }

    /**
     * Field retriever for Race fields.
     */
    protected class RetrieveRaceFields implements FieldRetriever {

        public static final String CALC_ID = "CALC_RACE";

        private static final String PARAM_ASIAN = "ASIAN";
        private static final String PARAM_BLACK = "BLACK";
        private static final String PARAM_ETHNICITY = "ETHNICITY";
        private static final String PARAM_HAWAII_OTHER = "HAWAII_OTHER";
        private static final String PARAM_IND_ALASKA = "IND_ALASKA";
        private static final String PARAM_WHITE = "WHITE";

        private static final String VALUE_ASIAN = "A";
        private static final String VALUE_BLACK = "B";
        private static final String VALUE_HAWAII_OTHER = "P";
        private static final String VALUE_IND_ALASKA = "I";
        private static final String VALUE_WHITE = "W";

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
            FLMcKayPrepaymentVerificationEntity flEntity = (FLMcKayPrepaymentVerificationEntity) entity;
            SisStudent student = (SisStudent) flEntity.getBean();
            Object value = null;
            Collection<Race> races = getStudentHelper().getStudentRaceMap().get(student.getPersonOid());

            if (races != null) {
                List<String> raceCodeArray = new ArrayList<String>();
                for (Race race : races) {
                    String raceCode = race.getRaceCode();
                    raceCodeArray.add(raceCode);
                }

                switch (field.getParameter().toString()) {
                    case PARAM_IND_ALASKA:
                        value = Boolean.valueOf(raceCodeArray.contains(VALUE_IND_ALASKA));
                        break;
                    case PARAM_ASIAN:
                        value = Boolean.valueOf(raceCodeArray.contains(VALUE_ASIAN));
                        break;
                    case PARAM_BLACK:
                        value = Boolean.valueOf(raceCodeArray.contains(VALUE_BLACK));
                        break;
                    case PARAM_HAWAII_OTHER:
                        value = Boolean.valueOf(raceCodeArray.contains(VALUE_HAWAII_OTHER));
                        break;
                    case PARAM_WHITE:
                        value = Boolean.valueOf(raceCodeArray.contains(VALUE_WHITE));
                        break;
                    case PARAM_ETHNICITY:
                        value = Boolean.valueOf(student.getPerson().getHispanicLatinoIndicator());
                        break;
                }
            }


            return value;
        }
    }

    /**
     * Instance variables.
     */
    protected static final List<String> MCKAY_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_A, FLStateReportData.SURVEY_PERIOD_B,
                    FLStateReportData.SURVEY_PERIOD_C, FLStateReportData.SURVEY_PERIOD_D);

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
        setEntityClass(FLMcKayPrepaymentVerificationEntity.class);

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
        return MCKAY_SURVEY_PERIOD_VALID_CODES;
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
        calcs.put(RetrieveWithdrawal.CALC_ID, new RetrieveWithdrawal());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveRaceFields.CALC_ID, new RetrieveRaceFields());
        calcs.put(RetrieveMilitaryFamilyStudent.CALC_ID, new RetrieveMilitaryFamilyStudent());
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
