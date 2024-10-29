/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentECPPProgram;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEnrollmentEntity;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The Class OnsisTransitionDate.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisTransitionDate extends OnsisStateReportData {


    /**
     * The Class OnsisTransitionDateEntity.
     */
    public static class OnsisTransitionDateEntity extends OnsisStateReportEntity {

        private List<OnStudentECPPProgram> m_programs;
        private OnsisTransitionDate m_reportData;

        /**
         * Gets the admission date.
         *
         * @return the admission date
         */
        public PlainDate getAdmissionDate() {
            return getAdmissionDate(getProgram());
        }

        /**
         * Gets the discharge date.
         *
         * @return the discharge date
         */
        public PlainDate getDischargeDate() {
            PlainDate result = getProgram().getEndDate();

            PlainDate enrEndDate = m_reportData.getParentEntity().getOnsisEnrollmentEndDate();
            if (result == null || (enrEndDate != null && result.after(enrEndDate))) {
                result = enrEndDate;
            }

            PlainDate submissionEndDate = getGlobalData().getSubmissionType().getPeriodEndDate();
            if (result == null || result.after(submissionEndDate)) {
                result = submissionEndDate;
            }

            return result;
        }

        /**
         * Gets the program.
         *
         * @return Student program participation
         */
        public OnStudentECPPProgram getProgram() {
            return m_programs.get(getCurrentRow());
        }

        /**
         * Gets the reside day total.
         *
         * @return the reside day total
         */
        public String getResideDayTotal() {
            if (getGlobalData().getCurrentSchool().stream()
                    .filter(skl -> OnStudentECPPProgram.PROGRAM_TYPE_DAY_TREATMENT.equals(skl.getEcppProgramType()))
                    .findAny().isPresent()) {
                return null;
            }
            int totalDays = DateUtils.getDayDifference(getAdmissionDate(), getDischargeDate());
            int programDays = getProgram().getNumberOfDays();
            if (programDays > 0) {
                if (programDays > totalDays) {
                    throw new IllegalStateException("ECPP number of days cannot be greater than the date interval");
                }
                totalDays = programDays;
            } else if (programDays == 0 && getProgram().isECPPDaysZero()) {
                totalDays = 0;
            } else {
                totalDays = DateUtils.getDayDifference(getAdmissionDate(), getDischargeDate());
            }
            return String.valueOf(totalDays);
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

            m_reportData = (OnsisTransitionDate) data;
            OnStudent student = (OnStudent) bean;

            StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;
            if (debugOutput != null) {
                debugOutput.append("OnsisTransitionDateEntity.initialize \n");
                debugOutput.append(bean.toString() + "\n");
                student.getEcppPrograms(getBroker()).stream()
                        .forEach(item -> debugOutput.append(item.toString() + "\n"));
            }
            m_programs = student.getEcppPrograms(getBroker()).stream()
                    .filter(pgm -> !getDischargeDate(pgm).before(getAdmissionDate(pgm)))
                    .sorted(new Comparator<OnStudentECPPProgram>() {

                        @Override
                        public int compare(OnStudentECPPProgram pgm0, OnStudentECPPProgram pgm1) {
                            int value = pgm0.getStartDate().compareTo(pgm1.getStartDate());
                            if (value == 0) {
                                value = pgm0.getOid().compareTo(pgm1.getOid());
                            }
                            return value;
                        }
                    })
                    .collect(Collectors.toList());
            setRowCount(m_programs.size());

            if (debugOutput != null) {
                ((OnsisTransitionDate) data).log(debugOutput.toString());
            }
        }

        /**
         * Gets the admission date.
         *
         * @return the admission date
         */
        private PlainDate getAdmissionDate(OnStudentECPPProgram program) {
            PlainDate result = program.getStartDate();

            PlainDate enrStartDate = m_reportData.getParentEntity().getOnsisEnrollmentStartDate();
            if (result == null || (enrStartDate != null && result.before(enrStartDate))) {
                result = enrStartDate;
            }

            PlainDate submissionStartDate = getGlobalData().getSubmissionType().getPeriodStartDate();
            if (result == null || result.before(submissionStartDate)) {
                result = submissionStartDate;
            }

            return result;
        }

        /**
         * Gets the discharge date.
         *
         * @param program the program
         * @return the discharge date
         */
        private PlainDate getDischargeDate(OnStudentECPPProgram program) {
            PlainDate result = program.getEndDate();

            PlainDate enrEndDate = m_reportData.getParentEntity().getOnsisEnrollmentEndDate();
            if (result == null || (enrEndDate != null && result.after(enrEndDate))) {
                result = enrEndDate;
            }

            PlainDate submissionEndDate = getGlobalData().getSubmissionType().getPeriodEndDate();
            if (result == null || result.after(submissionEndDate)) {
                result = submissionEndDate;
            }

            return result;
        }


    }

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        setBeans(Arrays.asList(getParentEntity().getBean()));
    }

    /**
     * Gets the parent entity.
     *
     * @return Onsis student enrollment entity
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getParentEntity()
     */
    @Override
    public OnsisStudentEnrollmentEntity getParentEntity() {
        return (OnsisStudentEnrollmentEntity) super.getParentEntity();
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisTransitionDateEntity.class);
    }

}
