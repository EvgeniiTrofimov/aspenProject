/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnGraduationStudentProgram;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnTranscript;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * The Class OnsisSchoolData.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class OnsisSchoolData extends OnsisStateReportData {
    /**
     * The Class OnsisSchoolEntity.
     */
    public static class OnsisSchoolEntity extends OnsisStateReportEntity {
        private Filterable<OnTranscript> m_maturePlarTranscripts;
        Filterable<OnGraduationStudentProgram> m_matureStudentPrograms;

        /**
         * Instantiates a new sif school info entity.
         */
           /*
            * Public no argument constructor for dynamic instantiation.
            */
        public OnsisSchoolEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            OnSchool school = (OnSchool) getBean();
            String name = school.getName();

            return name;
        }

        /**
         * Gets the mature plar transcripts.
         *
         * @return Filterable
         */
        public Filterable<OnTranscript> getMaturePlarTranscripts() {
            return m_maturePlarTranscripts;
        }

        /**
         * Gets the mature students programs.
         *
         * @return Filterable
         */
        public Filterable<OnGraduationStudentProgram> getMatureStudentsPrograms() {
            return m_matureStudentPrograms;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            OnsisStateReportData reportData = (OnsisStateReportData) data;
            Optional<List<OnSchool>> schools =
                    reportData.getGlobalData().getSchools().stream().filter(list -> list.contains(bean)).findFirst();
            reportData.getGlobalData().setCurrentSchool(schools.get());
            reportData.getGlobalData().preload();

            if (getGlobalData().getFieldsRepository().contains(FieldsRepository.PATH_PLAR_MATURE_REPORT) ||
                    getGlobalData().getFieldsRepository().contains(FieldsRepository.PLAR_MATURE_EQ_COUNT) ||
                    getGlobalData().getFieldsRepository().contains(FieldsRepository.PLAR_MATURE_EQ_COUNT)) {
                m_maturePlarTranscripts = FilterableFactory.createFilterableToolBeans(
                        ToolBean.getCachedToolBeans(OnsisStudent.class).stream()
                                .filter(std -> std.isMatureStudent(getBroker(), getGlobalData().getEndDate(),
                                        getGlobalData().getCurrentSchoolOids()))
                                .flatMap(std -> std.getStudentTranscripts(getBroker()).stream()
                                        .map(trn -> (OnTranscript) trn)
                                        .filter(trn -> !StringUtils.isEmpty(trn.getPlarType()))
                                        .filter(trn -> {
                                            PlainDate dateCompleted = trn.getDateCompleted();
                                            if (dateCompleted == null
                                                    || !getGlobalData().getDateRange().contains(dateCompleted)) {
                                                return false;
                                            }
                                            return OnTranscript.filterBySchool(trn,
                                                    getGlobalData().getCurrentSchoolOids(),
                                                    getGlobalData().getCurrentSchoolBsids().get(0),
                                                    getGlobalData().getSubmissionType()
                                                            .getExcludedSpecialConditionCodes());
                                        }))
                                .collect(Collectors.toList()));

                m_matureStudentPrograms = FilterableFactory.createFilterableToolBeans(
                        ToolBean.getCachedToolBeans(OnsisStudent.class).stream()
                                .filter(std -> std.isMatureStudent(getBroker(), getGlobalData().getEndDate(),
                                        getGlobalData().getCurrentSchoolOids()))
                                .flatMap(std -> std.getProgramStudies(getBroker()).stream()
                                        .filter(gsr -> gsr.getIssuedDate() != null
                                                && getGlobalData().getDateRange().contains(gsr.getIssuedDate()))
                                        .filter(gsr -> !StringUtils.isEmpty(gsr.getDiplomaType())))
                                .collect(Collectors.toList()));
            }
        }

        /**
         * Post process.
         *
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity#postProcess()
         */
        @Override
        public void postProcess() {
            super.postProcess();
            if (m_maturePlarTranscripts != null) {
                m_maturePlarTranscripts = null;
            }
            if (m_matureStudentPrograms != null) {
                m_matureStudentPrograms = null;
            }
        }

    }

    public static final String FIELD_SCHOOL_NUMBER = "School Number";

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans(com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity)
     */
    @Override
    public void buildBeans() throws X2BaseException {
        // TODO: Cleanup other cached schools here
        // clearOtherCachedSchools();
        setBeans(getGlobalData().getSchools().stream().map(list -> list.get(0)).collect(Collectors.toList()));
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisSchoolEntity.class);
    }

    /**
     * Close.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#close()
     */
    @Override
    public void close() {

        super.close();
    }
}
