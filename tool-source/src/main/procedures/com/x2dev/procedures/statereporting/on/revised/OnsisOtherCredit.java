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
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnTranscript;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisOtherCredit extends OnsisStateReportData {
    /**
     * The Class OnsisOtherCreditEntity.
     */
    public static class OnsisOtherCreditEntity extends OnsisStateReportEntity {
        private List<OnTranscript> m_transcripts;

        /**
         * Gets the final mark.
         *
         * @return String
         */
        public String getFinalMark() {
            String finalMark = getTranscript().getFinalGrade();
            if (!StringUtils.isNumeric(finalMark)) {
                finalMark = null;
            }
            return finalMark;
        }

        /**
         * Gets the report data.
         *
         * @return Onsis other credit
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#getReportData()
         */
        @Override
        public OnsisOtherCredit getReportData() {
            return (OnsisOtherCredit) super.getReportData();
        }

        /**
         * Gets the transcript.
         *
         * @return Transcript
         */
        public OnTranscript getTranscript() {
            return m_transcripts.get(getCurrentRow());
        }

        /**
         * Gets the total credit.
         *
         * @return BigDecimal
         */
        public BigDecimal getTranscriptCredit() {
            BigDecimal credit = getTranscript().getTotalCredit();
            if ((credit == null || credit.compareTo(BigDecimal.ZERO) <= 0)
                    && OnTranscript.FLAG_REPEATED.equals(getTranscript().getCourseRepeated())) {
                try {
                    credit = new BigDecimal(getTranscript().getPotentialCredit());
                } catch (Exception e) {
                    // Ignore conversion error
                }
                if ((credit == null || credit.compareTo(BigDecimal.ZERO) <= 0)) {
                    credit = getTranscript().getSchoolCourseCredit();
                }
            }
            return credit;
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
            OnsisStudent student = (OnsisStudent) getBean();
            List<StudentScheduleSpan> studentScheduleSpans = student.getStudentScheduleSpans(getBroker());
            StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;
            if (debugOutput != null) {
                debugOutput.append("Other Credit initialize:\n");
            }

            m_transcripts = student.getStudentTranscripts(getBroker()).stream()
                    .map(trn -> {
                        if (debugOutput != null) {
                            debugOutput.append("Candidate:" + trn + "\n");
                        }
                        return (OnTranscript) trn;
                    })
                    .filter(trn -> {
                        PlainDate dateCompleted = trn.getDateCompleted();
                        if (dateCompleted == null || !getGlobalData().getDateRange().contains(dateCompleted)) {
                            return false;
                        }
                        if (debugOutput != null) {
                            debugOutput.append("Date OK:" + trn + "\n");
                        }
                        return OnTranscript.filterBySchool(trn, getGlobalData().getCurrentSchoolOids(),
                                getGlobalData().getCurrentSchoolBsids().get(0),
                                getGlobalData().getSubmissionType().getExcludedSpecialConditionCodes());
                    })
                    .map(trn -> {
                        if (debugOutput != null) {
                            debugOutput.append("School OK:" + trn + "\n");
                        }
                        return trn;
                    })
                    .filter(toFilter -> {
                        Boolean isFiltered = false;
                        String plarType = toFilter.getPlarType();
                        if (StringUtils.isEmpty(plarType)) {
                            String courseCodeType = toFilter.getCourseCodeTypeState();
                            if (OnSection.COURSE_CODE_TYPES_MDC_DCC_LDC.contains(courseCodeType)
                                    && !StringUtils.isEmpty(toFilter.getMinistryCourseCode())) {
                                isFiltered = true;
                                if (debugOutput != null) {
                                    debugOutput.append("Course Code OK:" + toFilter + "\n");
                                }
                            }
                        }
                        return isFiltered;
                    })
                    .map(trn -> {
                        if (debugOutput != null) {
                            debugOutput.append("Filter OK:" + trn + "\n");
                        }
                        return trn;
                    })
                    .filter(toFilter -> {
                        return studentScheduleSpans.stream()
                                .filter(span -> span.getTranscript() != null)
                                .noneMatch(span -> toFilter.getOid().equals(span.getTranscript().getOid()));
                    })
                    .map(trn -> {
                        if (debugOutput != null) {
                            debugOutput.append("Schedules OK:" + trn + "\n");
                        }
                        return trn;
                    })
                    .filter(toFilter -> !toFilter.isStudentScheduled(getBroker(), getGlobalData().getDateRange()))
                    .map(trn -> {
                        if (debugOutput != null) {
                            debugOutput.append("Student scheduled OK:" + trn + "\n");
                        }
                        return trn;
                    })
                    .sorted(new Comparator<OnTranscript>() {
                        @Override
                        public int compare(OnTranscript trn1, OnTranscript trn2) {
                            return trn1.getOid().compareTo(trn2.getOid());
                        }
                    })
                    .collect(Collectors.toList());
            if (debugOutput != null) {
                ((OnsisOtherCredit) data).log(debugOutput.toString());
            }
            setRowCount(m_transcripts.size());
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
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisOtherCreditEntity.class);
    }
}
