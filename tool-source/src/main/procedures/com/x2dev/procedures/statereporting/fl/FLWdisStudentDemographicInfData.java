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
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.fl.FLStateReportEntity.WdisEntity;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;


/**
 * The Class FLWdisStudentDemographicInfData.
 */
public class FLWdisStudentDemographicInfData extends FLStateReportData {


    /**
     * The Class FLSdiEntity.
     */
    public static class FLSdiEntity extends FLStateReportEntity implements WdisEntity {

        private FLWdisStudentDemographicInfData m_data;
        private SisStudent m_student;


        /**
         * Instantiates a new FL sdi entity.
         */
        public FLSdiEntity() {
            // Public no argument constructor for dynamic instantiation.
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() + "] ";
            return name;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportEntity.WdisEntity#getProgram()
         */
        @Override
        public StudentProgramParticipation getProgram() {
            StudentProgramParticipation pgm = null;

            pgm = getProgram(m_data.m_pgmDatasetAge);

            if (pgm == null) {
                pgm = getProgram(m_data.m_pgmDatasetCte);
            }

            return pgm;
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (FLWdisStudentDemographicInfData) data;
            m_student = (SisStudent) bean;
            List<StudentScheduleInfo> infos = m_data.m_studentScheduleHelper.getStudentScheduleInfo(m_student);
            if (infos.size() == 0 || getProgram() == null) {
                setRowCount(0);
            }
        }


        /**
         * Gets the program.
         *
         * @param dataset StudentProgramDataset
         * @return Student program participation
         */
        private StudentProgramParticipation getProgram(StudentProgramDataset dataset) {
            StudentProgramParticipation pgm = null;

            List<StudentProgramParticipation> pgms = dataset.getPrograms(m_student.getOid());
            if (pgms != null && pgms.size() > 0) {
                pgm = pgms.get(pgms.size() - 1);
            }
            return pgm;
        }
    }


    /**
     * The Class RetrieveELLFields.
     */
    protected class RetrieveELLFields implements FieldRetriever {
        public static final String CALC_ID = "PGM_ELL";
        public static final String DDX_ID = "FL-PGM-ELL";

        protected StudentProgramDataset m_pgmEllDataset;


        /**
         * Instantiates a new retrieve ELL fields.
         *
         * @throws X2BaseException exception
         */
        public RetrieveELLFields() throws X2BaseException {
            m_pgmEllDataset = getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            List<StudentProgramParticipation> programs = m_pgmEllDataset.getPrograms(student.getOid());
            boolean isEll = ((programs != null) && (programs.size() > 0));
            return Boolean.valueOf(isEll);
        }
    }


    /**
     * The Class RetrieveExceptionalStudentFields.
     */
    protected class RetrieveExceptionalStudentFields implements FieldRetriever {
        public static final String CALC_ID = "PGM_EXCEPT";

        private static final String DDX_ID = "FL-PGM-EXCEPT";

        private static final String ALIAS_PRIMARY = "pgm-primary";

        private static final String PARAM_PRIMARY = "Exceptionality, Prim";

        private DataDictionaryField m_fieldPrimary;


        /**
         * Instantiates a new retrieve exceptional student fields.
         *
         * @throws X2BaseException exception
         */
        public RetrieveExceptionalStudentFields() throws X2BaseException {
            StudentProgramDataset pgmExceptionalStudentDataset = getStudentHelper().getStudentProgramDataset(DDX_ID,
                    getSurveyPeriod());
            DataDictionary pgmExceptionalStudentDataDictionary = pgmExceptionalStudentDataset.getDataDictionary();

            m_fieldPrimary = translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary,
                    ALIAS_PRIMARY, true);
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            FLSdiEntity flEntity = (FLSdiEntity) entity;
            FLWdisStudentDemographicInfData flData = (FLWdisStudentDemographicInfData) data;

            StudentProgramParticipation pgm =
                    getStudentHelper().getStudentProgram(flEntity.getBean().getOid(), DDX_ID, flData.getSurveyPeriod());

            String parameter = (String) field.getParameter();
            Object value = null;

            if (pgm != null) {
                switch (parameter) {
                    case PARAM_PRIMARY:
                        value = flData.getFieldValue(pgm, m_fieldPrimary);
                        break;
                }
            }

            return value;
        }
    }


    /**
     * The Class RetrieveGradeLevel.
     */
    protected class RetrieveGradeLevel implements FieldRetriever {
        public static final String CALC_ID = "Grade Level";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            StudentInfo info = getStudentHelper().getStudentInfo(student);
            return info.getGradeLevel(((FLStateReportData) data).getSurveyPeriod().getStartDate());
        }
    }


    /**
     * The Class RetrieveRace.
     */
    protected class RetrieveRace implements FieldRetriever {
        public static final String CALC_ID = "RACE";

        private static final String VALUE_NO = "N";
        private static final String VALUE_YES = "Y";

        private Map<String, String> m_raceStateCodes = new HashMap();


        /**
         * Instantiates a new retrieve race.
         */
        public RetrieveRace() {
            super();

            DataDictionaryField field =
                    getDataDictionary().findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);
            if (field != null && field.getReferenceTableOid() != null) {
                Map<String, ReferenceCode> codes = getReferenceCodes(field.getReferenceTableOid());
                for (ReferenceCode item : codes.values()) {
                    m_raceStateCodes.put(item.getCode(), item.getStateCode());
                }
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
            SisStudent student = (SisStudent) entity.getBean();
            String value = VALUE_NO;
            Collection<Race> races = getStudentHelper().getStudentRaceMap().get(student.getPersonOid());
            String raceCode = (String) field.getParameter();
            if (races != null) {
                for (Race race : races) {
                    if (raceCode.equalsIgnoreCase(m_raceStateCodes.get(race.getRaceCode()))) {
                        value = VALUE_YES;
                        break;
                    }
                }
            }
            return value;
        }
    }


    /**
     * The Class RetrieveResidentStatus.
     */
    protected class RetrieveResidentStatus implements FieldRetriever {
        public static final String CALC_ID = "RESIDENT_STATUS";

        private static final String ALIAS_RESIDENT_STATUS = "all-enr-ResidentStatus";
        private static final String DEFAULT_STATUS = "3";

        private DataDictionaryField m_fieldResidentStatus;


        /**
         * Instantiates a new retrieve resident status.
         */
        public RetrieveResidentStatus() {
            m_fieldResidentStatus = translateAliasToDictionaryField(ALIAS_RESIDENT_STATUS, true);
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            FLWdisStudentDemographicInfData flData = (FLWdisStudentDemographicInfData) data;
            StudentEnrollment enrollment = flData.getStudentHelper().getEnrollmentForDate(student.getOid(),
                    flData.getSurveyPeriod().getSnapshotDate(), StudentEnrollment.ENTRY);
            String value = null;
            if (enrollment != null) {
                value = (String) flData.getFieldValue(enrollment, m_fieldResidentStatus);
            }
            return StringUtils.isEmpty(value) ? DEFAULT_STATUS : value;
        }
    }


    /**
     * The Class RetrieveStudentName.
     */
    protected class RetrieveStudentName implements FieldRetriever {
        public static final String CALC_ID = "STD_NAME";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            StudentInfo info = getStudentHelper().getStudentInfo(student);
            return info.formatStudentLegalName();
        }
    }

    protected static final List<String> CUSTOM_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_F, FLStateReportData.SURVEY_PERIOD_G,
                    FLStateReportData.SURVEY_PERIOD_W, FLStateReportData.SURVEY_PERIOD_X,
                    FLStateReportData.SURVEY_PERIOD_S);

    protected static final String DDX_ID_AGE = "FL-PGM-AGE";
    protected static final String DDX_ID_CTE = "FL-PGM-CTE";

    protected StudentProgramDataset m_pgmDatasetAge;
    protected StudentProgramDataset m_pgmDatasetCte;

    protected FLScheduleHelper m_scheduleHelper;
    protected StudentScheduleHelper m_studentScheduleHelper;


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        if (getSetupErrors().size() != 0) {
            return;
        }
        getStudentHelper().setWdisMode(WDIS_MODE_BOTH);
        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLSdiEntity.class);

        m_scheduleHelper =
                new FLScheduleHelper(this, this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());
        m_studentScheduleHelper = getStudentHelper().new StudentScheduleHelper(m_scheduleHelper,
                this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());

        m_pgmDatasetAge = getStudentHelper().getStudentProgramDataset(DDX_ID_AGE, getSurveyPeriod());
        m_pgmDatasetCte = getStudentHelper().getStudentProgramDataset(DDX_ID_CTE, getSurveyPeriod());

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
     * Register field retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveStudentName.CALC_ID, new RetrieveStudentName());
        calcs.put(RetrieveWdisYear.CALC_ID, new RetrieveWdisYear());
        calcs.put(RetrieveResidentStatus.CALC_ID, new RetrieveResidentStatus());
        calcs.put(RetrieveExceptionalStudentFields.CALC_ID, new RetrieveExceptionalStudentFields());
        calcs.put(RetrieveELLFields.CALC_ID, new RetrieveELLFields());
        calcs.put(RetrieveRace.CALC_ID, new RetrieveRace());
        calcs.put(RetrieveGradeLevel.CALC_ID, new RetrieveGradeLevel());
        super.addCalcs(calcs);
    }


    /**
     * Register field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<>();
        super.addValidators(validators);
    }

}
