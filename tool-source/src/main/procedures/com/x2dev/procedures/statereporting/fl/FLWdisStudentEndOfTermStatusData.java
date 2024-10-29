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
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;


/**
 * The Class FLWdisStudentEndOfTermStatusData.
 */
public class FLWdisStudentEndOfTermStatusData extends FLStateReportData {


    /**
     * The Class FLSetsEntity.
     */
    public static class FLSetsEntity extends FLStateReportEntity implements WdisEntity {
        FLWdisStudentEndOfTermStatusData m_data;
        SisStudent m_student;


        /**
         * Instantiates a new FL sets entity.
         */
        public FLSetsEntity() {
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
            m_data = (FLWdisStudentEndOfTermStatusData) data;
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

    protected static final List<String> CUSTOM_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_F, FLStateReportData.SURVEY_PERIOD_W,
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
        setEntityClass(FLSetsEntity.class);

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
        calcs.put(RetrieveWdisYear.CALC_ID, new RetrieveWdisYear());
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
