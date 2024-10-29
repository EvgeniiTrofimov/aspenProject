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
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentConductDataset;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentVictim;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

/**
 * The Class FLIndicatorStatusData.
 */
public class FLIndicatorStatusData extends FLStateReportData {

    /**
     * The Class FLIndicatorStatusEntity.
     */
    public static class FLIndicatorStatusEntity extends FLStateReportEntity {
        private FLIndicatorStatusData m_data;
        private SisStudent m_record;
        private List<StudentScheduleInfo> m_scheduleInfos;

        /**
         * Instantiates a new FL indicator status entity.
         */
        public FLIndicatorStatusEntity() {
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
            SisStudent student = m_record;
            String name = student.getNameView() + " [LASID: " + student.getLocalId() + "] ";
            return name;
        }

        /**
         * Gets the schedule info.
         *
         * @return Student schedule info
         */
        public StudentScheduleInfo getScheduleInfo() {
            return m_scheduleInfos.get(getCurrentRow());
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

            m_data = (FLIndicatorStatusData) data;
            m_record = (SisStudent) getBean();

            if (m_data.getStudentHelper().isStudentEligible(m_record)) {
                m_scheduleInfos = m_data.m_studentScheduleHelper.getStudentScheduleInfo(m_record);
                setRowCount(m_scheduleInfos.size());
            } else {
                setRowCount(0);
            }
        }
    }

    /**
     * Field retriever for Harassment fields.
     */
    protected class RetrieveConductIncidents implements FieldRetriever {
        public static final String CALC_ID = "CND_VICTIM_HARASS";


        private static final String DEFAULT_VALUE = "Z";
        private static final String PARAM_HARASSED_DISABILITY = "DISABILITY";
        private static final String PARAM_HARASSED_ORIENTATION = "ORIENTATION";
        private static final String PARAM_HARASSED_RACE = "RACE";
        private static final String PARAM_HARASSED_RELIGION = "RELIGION";
        private static final String PARAM_HARASSED_SEX = "SEX";
        private static final String VALUE_N = "N";
        private static final String VALUE_Y = "Y";

        private StudentConductDataset m_studentConductDataset;

        /**
         * Instantiates a new retrieve conduct incidents.
         *
         * @throws X2BaseException exception
         */
        public RetrieveConductIncidents() throws X2BaseException {
            m_studentConductDataset = getStudentHelper().getStudentConductDataset(
                    FLIndicatorStatusData.this.getCurrentContext().getStartDate(),
                    getSurveyPeriod().getEndDate());

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
            Object value = DEFAULT_VALUE;
            StudentVictim victim = m_studentConductDataset.getVictimIncidents(entity.getBean().getOid());
            if (victim != null) {
                value = VALUE_N;
                String parameter = (String) field.getParameter();
                switch (parameter) {
                    case PARAM_HARASSED_DISABILITY:
                        if (victim.isDisabilityBased()) {
                            value = VALUE_Y;
                        }
                        break;
                    case PARAM_HARASSED_RACE:
                        if (victim.isRaceBased()) {
                            value = VALUE_Y;
                        }
                        break;
                    case PARAM_HARASSED_SEX:
                        if (victim.isSexBased()) {
                            value = VALUE_Y;
                        }
                        break;
                    case PARAM_HARASSED_ORIENTATION:
                        if (victim.isSexualOrientationBased()) {
                            value = VALUE_Y;
                        }
                        break;
                    case PARAM_HARASSED_RELIGION:
                        if (victim.isReligionBased()) {
                            value = VALUE_Y;
                        }
                        break;
                }
            }

            return value;
        }
    }

    /**
     * Field retriever for Dropout Prevention field.
     */
    protected class RetrieveDropoutPrevention implements FieldRetriever {
        public static final String CALC_ID = "PGM_DROPOUT";

        private static final String ALIAS_DROPOUT_CODE = "pgm-dropout-code";

        private static final String DDX_ID = "FL-PGM-DROP";

        private DataDictionaryField m_fieldDropoutCode;

        /**
         * Instantiates a new retrieve dropout prevention.
         *
         * @throws X2BaseException exception
         */
        public RetrieveDropoutPrevention() throws X2BaseException {
            StudentProgramDataset pgmDropoutDataset = getStudentHelper().getStudentProgramDataset(DDX_ID,
                    getSurveyPeriod());
            DataDictionary dataDictionary = pgmDropoutDataset.getDataDictionary();
            m_fieldDropoutCode = translateAliasToDictionaryField(dataDictionary, ALIAS_DROPOUT_CODE, true);
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
            FLIndicatorStatusEntity flEntity = (FLIndicatorStatusEntity) entity;
            FLIndicatorStatusData flData = (FLIndicatorStatusData) data;

            StudentProgramParticipation pgm =
                    getStudentHelper().getStudentProgram(flEntity.getBean().getOid(), DDX_ID, flData.getSurveyPeriod());

            Object value = null;
            boolean isSurvey5 = flData.getSurveyPeriodCode().contentEquals("5");
            value = (isSurvey5) ? "Z" : flData.getFieldValue(pgm, m_fieldDropoutCode);

            return value;
        }
    }

    /**
     * Field retriever for Federally Connected Student Indicator field.
     */
    protected class RetrieveFedConnectedIndicator implements FieldRetriever {

        public static final String CALC_ID = "STD_FEDIND";

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

            FLIndicatorStatusEntity flEntity = (FLIndicatorStatusEntity) entity;
            FLIndicatorStatusData flData = (FLIndicatorStatusData) data;

            SisStudent student = (SisStudent) flEntity.getBean();

            Object value = null;
            boolean isSurvey25 =
                    flData.getSurveyPeriodCode().contentEquals("2") || flData.getSurveyPeriodCode().contentEquals("5");
            value = (isSurvey25) ? "Z" : getStudentHelper().getFedConnectedIndicator(student);

            return value;
        }

    }

    /**
     * Field retriever for Homeless Student field.
     */
    protected class RetrieveHomelessStudent implements FieldRetriever {
        public static final String CALC_ID = "PGM_HOMELESS";

        private static final String ALIAS_HOMELESS_CODE = "pgm-homeless-code";
        private static final String ALIAS_HOMELESSNESS_CAUSE = "pgm-homelessness-cause";
        private static final String ALIAS_UNACCOMPANIED = "pgm-homeless-unaccompanied";

        private static final String DDX_ID = "FL-PGM-HOMELESS";

        private static final String PARAM_HOMELESS = "HOMELESS";
        private static final String PARAM_HOMELESSNESS_CAUSE = "HOMELESSNESS_CAUSE";
        private static final String PARAM_UNACCOMPANIED = "UNACCOMPANIED";

        private static final String VALUE_N = "N";
        private static final String VALUE_Y = "Y";

        private DataDictionaryField m_fieldHomelessCode;
        private DataDictionaryField m_fieldHomelessnessCause;
        private DataDictionaryField m_fieldUnaccompanied;

        /**
         * Instantiates a new retrieve homeless student.
         *
         * @throws X2BaseException exception
         */
        public RetrieveHomelessStudent() throws X2BaseException {
            StudentProgramDataset pgmHomelessDataset = getStudentHelper().getStudentProgramDataset(DDX_ID,
                    getSurveyPeriod());
            DataDictionary dataDictionary = pgmHomelessDataset.getDataDictionary();
            m_fieldUnaccompanied = translateAliasToDictionaryField(dataDictionary, ALIAS_UNACCOMPANIED, true);
            m_fieldHomelessCode = translateAliasToDictionaryField(dataDictionary, ALIAS_HOMELESS_CODE, true);
            m_fieldHomelessnessCause = translateAliasToDictionaryField(dataDictionary, ALIAS_HOMELESSNESS_CAUSE, true);
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
            FLIndicatorStatusEntity flEntity = (FLIndicatorStatusEntity) entity;
            FLIndicatorStatusData flData = (FLIndicatorStatusData) data;

            StudentProgramParticipation pgm =
                    getStudentHelper().getStudentProgram(flEntity.getBean().getOid(), DDX_ID, flData.getSurveyPeriod());

            String parameter = (String) field.getParameter();
            Object value = null;

            if (pgm != null) {
                switch (parameter) {
                    case PARAM_HOMELESS:
                        value = flData.getFieldValue(pgm, m_fieldHomelessCode);
                        break;
                    case PARAM_HOMELESSNESS_CAUSE:
                        value = flData.getFieldValue(pgm, m_fieldHomelessnessCause);
                        break;
                    case PARAM_UNACCOMPANIED:
                        Boolean m_unaccompanied =
                                (Boolean) flData.getFieldValue(pgm, m_fieldUnaccompanied);
                        value = (m_unaccompanied.booleanValue()) ? VALUE_Y : VALUE_N;
                        break;
                }
            }
            return value;
        }
    }

    /**
     * Field retriever for Immigrant Student field.
     */
    protected class RetrieveMigrantStudent implements FieldRetriever {

        public static final String CALC_ID = "PGM_MIGRANT";

        private static final String ALIAS_STATUS = "pgm-migrant-status";

        private static final String DDX_ID = "FL-PGM-MIGRANT";

        private DataDictionaryField m_fieldStatus = null;

        /**
         * Instantiates a new retrieve migrant student.
         *
         * @throws X2BaseException exception
         */
        public RetrieveMigrantStudent() throws X2BaseException {
            StudentProgramDataset dataset = getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
            m_fieldStatus = translateAliasToDictionaryField(dataset.getDataDictionary(), ALIAS_STATUS, true);
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
            StudentProgramParticipation migrantPgm =
                    getStudentHelper().getStudentProgram(entity.getBean().getOid(), DDX_ID, getSurveyPeriod());
            boolean value = migrantPgm != null &&
                    !StringUtils.isEmpty((String) FLIndicatorStatusData.this.getFieldValue(migrantPgm, m_fieldStatus));
            return Boolean.valueOf(value);
        }
    }

    /**
     * Field retriever for Military Family Student field.
     */
    protected class RetrieveMilitaryFamilyStudent implements FieldRetriever {
        public static final String CALC_ID = "PGM_MFS";

        private static final String DDX_ID = "FL-PGM-MFS";

        private static final String DEFAULT_VALUE = "N";
        private static final String PK_VALUE = "Z";
        private static final String TRUE_VALUE = "Y";

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
            FLIndicatorStatusEntity flEntity = (FLIndicatorStatusEntity) entity;
            FLIndicatorStatusData flData = (FLIndicatorStatusData) data;
            StudentInfo info = getStudentHelper().getStudentInfo((SisStudent) flEntity.getBean());
            String gradeLevel = info.getGradeLevel(flData.getSurveyPeriod().getStartDate());
            if (FLStudentHelper.GRADE_LEVEL_PK.equals(gradeLevel)) {
                value = PK_VALUE;
            } else {
                List<StudentProgramParticipation> pgms =
                        getStudentHelper().getStudentPrograms(flEntity.getBean().getOid(),
                                DDX_ID, flData.getSurveyPeriod());
                if (pgms != null && !pgms.isEmpty()) {
                    value = TRUE_VALUE;
                }

            }

            return value;
        }
    }

    /**
     * Field retriever for Fund Source field.
     */
    protected class RetrieveFundSource implements FieldRetriever {
        public static final String CALC_ID = "PGM_IDEA";

        private static final String DDX_ID = "FL-PGM-IDEA-B";

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
            FLIndicatorStatusEntity flEntity = (FLIndicatorStatusEntity) entity;
            FLIndicatorStatusData flData = (FLIndicatorStatusData) data;

            List<StudentProgramParticipation> pgms =
                    getStudentHelper().getStudentPrograms(flEntity.getBean().getOid(),
                            DDX_ID, flData.getSurveyPeriod());

            return Boolean.valueOf(pgms != null && !pgms.isEmpty());
        }
    }


    /**
     * Field retriever for Program 504 Eligible field.
     *
     */
    protected class RetrieveProgram504EligibleStudent implements FieldRetriever {
        public static final String CALC_ID = "PGM_504";

        private static final String ALIAS_ELIGIBLE = "pgm-504-eligible";
        private static final String ALIAS_REQUIRES_PLAN = "pgm-504-requires-plan";

        private static final String DDX_ID = "FL-PGM-504";

        private static final String VALUE_I = "I";
        private static final String VALUE_N = "N";
        private static final String VALUE_Y = "Y";
        private static final String VALUE_Z = "Z";

        private DataDictionaryField m_fieldEligible;
        private DataDictionaryField m_fieldRequiresPlan;

        /**
         * Instantiates a new retrieve Program 504 Eligible field.
         *
         * @throws X2BaseException the x 2 base exception
         */
        public RetrieveProgram504EligibleStudent() throws X2BaseException {
            StudentProgramDataset pgmDataset = getStudentHelper().getStudentProgramDataset(DDX_ID,
                    getSurveyPeriod());
            DataDictionary pgmDataDictionary = pgmDataset.getDataDictionary();

            m_fieldEligible = translateAliasToDictionaryField(pgmDataDictionary,
                    ALIAS_ELIGIBLE, true);
            m_fieldRequiresPlan = translateAliasToDictionaryField(pgmDataDictionary,
                    ALIAS_REQUIRES_PLAN, true);
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
            String value = VALUE_Z;

            FLIndicatorStatusEntity flEntity = (FLIndicatorStatusEntity) entity;
            FLIndicatorStatusData flData = (FLIndicatorStatusData) data;

            StudentInfo info = getStudentHelper().getStudentInfo((SisStudent) flEntity.getBean());
            String gradeLevel = info.getGradeLevel(flData.getSurveyPeriod().getStartDate());

            if (!FLStudentHelper.GRADE_LEVEL_PK.equals(gradeLevel)) {
                List<StudentProgramParticipation> pgms =
                        getStudentHelper().getStudentPrograms(flEntity.getBean().getOid(),
                                DDX_ID, flData.getSurveyPeriod());

                if (pgms != null && !pgms.isEmpty()) {
                    StudentProgramParticipation pgm = pgms.get(0);

                    Boolean eligible = (Boolean) flData.getFieldValue(pgm, m_fieldEligible);
                    Boolean requiresPlan = (Boolean) flData.getFieldValue(pgm, m_fieldRequiresPlan);

                    if (eligible.equals(Boolean.FALSE)) {
                        value = VALUE_I;
                    } else if (requiresPlan.equals(Boolean.FALSE)) {
                        value = VALUE_N;
                    } else if (requiresPlan.equals(Boolean.TRUE)) {
                        // TODO: to detect student which has not any disabilities under the
                        // Individuals with Disabilities Education Act (IDEA)
                        value = VALUE_Y;
                    }
                }
            }

            return value;
        }
    }

    /**
     * Field retriever for Physical Education Waiver field.
     */
    protected class RetrievePhysicalEducationWaiver implements FieldRetriever {
        public static final String CALC_ID = "PGM_PE_WAIVER";

        private static final String DDX_ID = "FL-PGM-PE-WAIVER";

        private static final String DEFAULT_VALUE = "N";
        private static final String PK_VALUE = "Z";
        private static final String TRUE_VALUE = "Y";

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
            FLIndicatorStatusEntity flEntity = (FLIndicatorStatusEntity) entity;
            FLIndicatorStatusData flData = (FLIndicatorStatusData) data;

            String value = DEFAULT_VALUE;
            StudentInfo info = getStudentHelper().getStudentInfo((SisStudent) flEntity.getBean());
            String gradeLevel = info.getGradeLevel(flData.getSurveyPeriod().getStartDate());
            if (FLStudentHelper.GRADE_LEVEL_PK.equals(gradeLevel)
                    || FLStudentHelper.GRADE_LEVELS_9_12.contains(gradeLevel)) {
                value = PK_VALUE;
            } else {
                List<StudentProgramParticipation> pgms =
                        getStudentHelper().getStudentPrograms(flEntity.getBean().getOid(),
                                DDX_ID, flData.getSurveyPeriod());
                if (pgms != null && pgms.size() > 0) {
                    value = TRUE_VALUE;
                }
            }
            return value;
        }
    }

    /**
     * Field retriever for CAPE ID field.
     */
    protected class RetrieveProfessionalAcademyIdenitifier implements FieldRetriever {
        public static final String CALC_ID = "PGM_CAPE";

        private static final String ALIAS_CAPE_DISTRICT = "pgm-cape-district";
        private static final String ALIAS_CAPE_IDENTIFIER = "pgm-cape-identifier";

        private static final String DDX_ID = "FL-PGM-CAPE";

        private static final String PARAM_DISTRICT_FIRST = "DISTRICT_FIRST";
        private static final String PARAM_DISTRICT_SECOND = "DISTRICT_SECOND";

        private static final String PARAM_ID_FIRST = "ID_FIRST";
        private static final String PARAM_ID_SECOND = "ID_SECOND";

        private DataDictionaryField m_fieldCapeDistrict;
        private DataDictionaryField m_fieldCapeIdentifier;

        /**
         * Instantiates a new retrieve professional academy idenitifier.
         *
         * @throws X2BaseException exception
         */
        public RetrieveProfessionalAcademyIdenitifier() throws X2BaseException {
            StudentProgramDataset pgmCapeDataset =
                    getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
            DataDictionary dataDict = pgmCapeDataset.getDataDictionary();
            m_fieldCapeDistrict = translateAliasToDictionaryField(dataDict, ALIAS_CAPE_DISTRICT, true);
            m_fieldCapeIdentifier = translateAliasToDictionaryField(dataDict, ALIAS_CAPE_IDENTIFIER, true);
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

            FLIndicatorStatusEntity flEntity = (FLIndicatorStatusEntity) entity;
            FLIndicatorStatusData flData = (FLIndicatorStatusData) data;

            List<StudentProgramParticipation> pgms =
                    (flData.getSurveyPeriodCode().contentEquals(FLStateReportData.SURVEY_PERIOD_5))
                            ? Arrays.asList(getStudentHelper().getStudentProgram(flEntity.getBean().getOid(),
                                    DDX_ID, flData.getSurveyPeriod()))
                            : getStudentHelper().getStudentPrograms(flEntity.getBean().getOid(),
                                    DDX_ID, flData.getSurveyPeriod());

            String parameter = (String) field.getParameter();
            Object value = null;

            if (pgms != null) {
                switch (parameter) {
                    case PARAM_DISTRICT_FIRST:
                        if (pgms.size() > 0) {
                            StudentProgramParticipation first = pgms.get(0);
                            value = flData.getFieldValue(first, m_fieldCapeDistrict);
                            return value;
                        }
                        break;
                    case PARAM_DISTRICT_SECOND:
                        if (pgms.size() > 1) {
                            StudentProgramParticipation second = pgms.get(1);
                            value = flData.getFieldValue(second, m_fieldCapeDistrict);
                            return value;
                        }
                        break;
                    case PARAM_ID_FIRST:
                        if (pgms.size() > 0) {
                            StudentProgramParticipation first = pgms.get(0);
                            value = flData.getFieldValue(first, m_fieldCapeIdentifier);
                            return value;
                        }
                        break;
                    case PARAM_ID_SECOND:
                        if (pgms.size() > 1) {
                            StudentProgramParticipation second = pgms.get(1);
                            value = flData.getFieldValue(second, m_fieldCapeIdentifier);
                            return value;
                        }
                        break;
                }
            }

            return value;
        }
    }

    /**
     * Field retriever for arrest field.
     */
    protected class RetrieveStudentArrest implements FieldRetriever {
        public static final String CALC_ID = "CND_ARREST";


        private StudentConductDataset m_studentConductDataset;

        /**
         * Instantiates a new retrieve student arrest.
         *
         * @throws X2BaseException exception
         */
        public RetrieveStudentArrest() throws X2BaseException {
            m_studentConductDataset = getStudentHelper().getStudentConductDataset(
                    FLIndicatorStatusData.this.getCurrentContext().getStartDate(),
                    getSurveyPeriod().getEndDate());
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
            FLIndicatorStatusEntity flEntity = (FLIndicatorStatusEntity) entity;

            boolean arrestedIndicator = m_studentConductDataset.getArrestedIndicator(flEntity.getBean().getOid());

            return arrestedIndicator ? Boolean.TRUE : Boolean.FALSE;
        }

    }

    /**
     * Instance variables.
     */
    protected static final List<String> FED_SURVEY_PERIOD_VALID_CODES = Arrays.asList(FLStateReportData.SURVEY_PERIOD_2,
            FLStateReportData.SURVEY_PERIOD_3, FLStateReportData.SURVEY_PERIOD_5);
    protected FLScheduleHelper m_scheduleHelper;

    protected StudentScheduleHelper m_studentScheduleHelper;

    /**
     * Initialize the data module. Initialize necessary working resources. Define query to load.
     *
     * @throws X2BaseException exception
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

        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLIndicatorStatusEntity.class);

        m_scheduleHelper =
                new FLScheduleHelper(this, this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());
        m_studentScheduleHelper = getStudentHelper().new StudentScheduleHelper(m_scheduleHelper,
                this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());

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
        return FED_SURVEY_PERIOD_VALID_CODES;
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
        calcs.put(RetrieveConductIncidents.CALC_ID, new RetrieveConductIncidents());
        calcs.put(RetrieveDropoutPrevention.CALC_ID, new RetrieveDropoutPrevention());
        calcs.put(RetrieveFedConnectedIndicator.CALC_ID, new RetrieveFedConnectedIndicator());
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveHomelessStudent.CALC_ID, new RetrieveHomelessStudent());
        calcs.put(RetrieveMigrantStudent.CALC_ID, new RetrieveMigrantStudent());
        calcs.put(RetrieveMilitaryFamilyStudent.CALC_ID, new RetrieveMilitaryFamilyStudent());
        calcs.put(RetrievePhysicalEducationWaiver.CALC_ID, new RetrievePhysicalEducationWaiver());
        calcs.put(RetrieveProfessionalAcademyIdenitifier.CALC_ID, new RetrieveProfessionalAcademyIdenitifier());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        calcs.put(RetrieveStudentArrest.CALC_ID, new RetrieveStudentArrest());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveProgram504EligibleStudent.CALC_ID, new RetrieveProgram504EligibleStudent());
        calcs.put(RetrieveFundSource.CALC_ID, new RetrieveFundSource());
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
