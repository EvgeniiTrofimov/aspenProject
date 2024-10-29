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
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnTranscript;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.lang3.math.NumberUtils;

/**
 * The Class OnsisPlar.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisPlar extends OnsisStateReportData {

    /**
     * The Class OnsisPlarEntity.
     */
    public static class OnsisPlarEntity extends OnsisStateReportEntity {
        private static final String FIELD_ID_TYPE = "Type";

        private static final String VALUE_MDC_PLE = "PLE99";
        private static final String VALUE_PLAR_TYPE_7 = "7";

        private List<List<OnTranscript>> m_groups = new ArrayList<>();
        private OnsisPlar m_reportData;

        /**
         * Gets the compulsory credit.
         *
         * @return Float
         * @throws X2BaseException exception
         */
        public Float getCompulsoryCredit() throws X2BaseException {
            float compulsoryCredit = NumberUtils.FLOAT_ZERO;
            if (VALUE_PLAR_TYPE_7.equals(getFieldValue(FIELD_ID_TYPE))) {
                for (OnTranscript transcript : getCurrentGroup()) {
                    String requirementDetail = transcript.getCreditByRequirementDetail();
                    if (!StringUtils.isEmpty(requirementDetail)) {
                        String[] values = requirementDetail.split(":");
                        if (values.length >= 2 && !"ELE".equals(values[0]) && StringUtils.isNumeric(values[1])) {
                            m_reportData.log("getCompulsoryCredit() - VALUE_PLAR_TYPE_7 included " + transcript);
                            compulsoryCredit += new BigDecimal(values[1]).floatValue();
                        }
                    }
                }
            } else {
                for (OnTranscript transcript : getCurrentGroup()) {
                    BigDecimal compulsoryCreditApplied = transcript.getCompulsoryCreditApplied();
                    if (compulsoryCreditApplied != null) {
                        compulsoryCredit += compulsoryCreditApplied.floatValue();
                    }
                }
            }
            return compulsoryCredit;
        }

        /**
         * Gets the first transcript.
         *
         * @return Transcript
         */
        public OnTranscript getFirstTranscript() {
            return m_groups.get(getCurrentRow()).get(0);
        }

        /**
         * Gets the ministry defined course.
         *
         * @return String
         */
        public String getMinistryDefinedCourse() {
            return getMinistryDefinedCourse(getFirstTranscript());
        }

        /**
         * Gets the total credit.
         *
         * @return Float
         */
        public Float getTotalCredit() {
            List<OnTranscript> transcripts = getCurrentGroup();
            float totalCredit = NumberUtils.FLOAT_ZERO;
            for (OnTranscript transcript : transcripts) {
                BigDecimal currentTotalCredit = transcript.getTotalCredit();
                if (currentTotalCredit != null) {
                    totalCredit = totalCredit + currentTotalCredit.floatValue();
                }
            }
            return totalCredit;
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
            StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;
            if (debugOutput != null) {
                debugOutput.append("Plar initialize:\n");
            }

            m_reportData = (OnsisPlar) getReportData();
            OnsisStudent student = (OnsisStudent) getBean();
            m_groups.addAll(student.getStudentTranscripts(getBroker()).stream()
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
                    .filter(trn -> !StringUtils.isEmpty(trn.getPlarType()))
                    .map(trn -> {
                        if (debugOutput != null) {
                            debugOutput.append("Plar Type OK:" + trn + "\n");
                        }
                        return trn;
                    })
                    .collect(Collectors.groupingBy(trn -> getKey(trn)))
                    .values().stream()
                    .sorted(new Comparator<List<OnTranscript>>() {

                        @Override
                        public int compare(List<OnTranscript> trn1, List<OnTranscript> trn2) {
                            return getKey(trn1.iterator().next()).compareTo(getKey(trn2.iterator().next()));
                        }
                    })
                    .collect(Collectors.toList()));
            if (debugOutput != null) {
                ((OnsisPlar) data).log(debugOutput.toString());
            }
            setRowCount(m_groups.size());
        }

        /**
         * Gets the current group.
         *
         * @return List
         */
        private List<OnTranscript> getCurrentGroup() {
            return m_groups.get(getCurrentRow());
        }

        /**
         * Gets the key.
         *
         * @param trn OnsisTranscript
         * @return String
         */
        private String getKey(OnTranscript trn) {
            StringBuilder value = new StringBuilder();
            String plarType = trn.getPlarType();
            value.append(plarType);
            value.append(getMinistryDefinedCourse(trn));
            // value.append(m_reportData.getStateValueByAlias(trn,
            // BeanHelperTranscripts.ALIAS_TRN_PLAR_STATUS));
            value.append("" + trn.getDateCompleted());
            return value.toString();
        }

        /**
         * Gets the ministry defined course.
         *
         * @param trn OnsisTranscript
         * @return String
         */
        private String getMinistryDefinedCourse(OnTranscript trn) {
            // MPL0030
            OnsisStudent student = (OnsisStudent) getBean();
            boolean isMature = student.isMatureStudent(getBroker(), getGlobalData().getEndDate(),
                    getGlobalData().getCurrentSchoolOids());
            String result = null;
            String plarType = trn.getPlarType();
            if (OnTranscript.PLAR_TYPE_GRADE_IND_9_10.equals(plarType)
                    || (!isMature && OnTranscript.PLAR_TYPE_GRADE_EQU_11_12.equals(plarType))) {
                result = VALUE_MDC_PLE;
            } else {
                result = trn.getMinistryDefinedCourse();
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
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisPlarEntity.class);
    }
}
