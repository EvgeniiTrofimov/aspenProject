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
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSped;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.CsvField;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEnrollmentEntity;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The Class OnsisSped.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisSped extends OnsisStateReportData {
    /**
     * The Class OnsisSpedEntity.
     */
    public static class OnsisSpedEntity extends OnsisStateReportEntity {

        PlainDate m_evaluationDate;
        private List<OnStudentSped> m_programs;
        private OnsisSped m_reportData;

        /**
         * Gets the exceptionality type.
         *
         * @return String
         */
        public String getExceptionalityType() {
            return getProgram().getExceptionality();
        }

        /**
         * Gets the iep flag.
         *
         * @return Boolean
         */
        public Boolean getIepFlag() {
            return getProgram().getIepFlag(getBroker(), m_evaluationDate);
        }

        /**
         * Gets the iprc student flag.
         *
         * @return Boolean
         */
        public Boolean getIprcStudentFlag() {
            return getProgram().getIprcStudentFlag(getBroker(), m_evaluationDate);
        }

        /**
         * Gets the main except flag.
         *
         * @return Boolean
         */
        public Boolean getMainExceptFlag() {
            return getProgram().getMainExceptFlag();
        }

        /**
         * Gets the non identified flag.
         *
         * @return Boolean
         */
        public Boolean getNonIdentifiedFlag() {
            return StringUtils.isEmpty(getProgram().getExceptionality()) ? Boolean.TRUE : Boolean.FALSE;
        }

        /**
         * Gets the placement type.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getPlacementType() throws X2BaseException {
            return getProgram().getPlacementType(getBroker(), m_evaluationDate);
        }

        /**
         * Gets the review date.
         *
         * @return Plain date
         */
        public PlainDate getReviewDate() {
            return getNonIdentifiedFlag() ? null : getProgram().getReviewDate(getBroker(), m_evaluationDate);
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
            m_reportData = (OnsisSped) getReportData();

            PlainDate withdrawalDate = getGlobalData().getEndDate();

            if (m_reportData.getParentEntity() instanceof OnsisStudentEnrollmentEntity) {
                PlainDate endDate =
                        ((OnsisStudentEnrollmentEntity) m_reportData.getParentEntity()).getOnsisEnrollmentEndDate();
                if (endDate != null && getGlobalData().getDateRange().contains(endDate)) {
                    withdrawalDate = endDate;
                }
            }
            m_evaluationDate = withdrawalDate;
            m_programs = student.getSpedPrograms(getBroker()).stream()
                    .filter(pgm -> pgm.getDateRange().contains(m_evaluationDate) && pgm.getSpedReportIndicator())
                    .collect(Collectors.toList());

            setRowCount(m_programs.size());
        }

        /**
         * Gets the program.
         *
         * @return Student program participation
         */
        public OnStudentSped getProgram() {
            return m_programs.get(getCurrentRow());
        }

    }

    private static final String ELEMENT_INDIVIDUAL_EDUCATION_PLAN_FLAG = "INDIVIDUAL_EDUCATION_PLAN_FLAG";
    private static final String ELEMENT_IPRC_REVIEW_DATE = "IPRC_REVIEW_DATE";
    private static final String ELEMENT_IPRC_STUDENT_FLAG = "IPRC_STUDENT_FLAG";
    private static final String ELEMENT_MAIN_EXCEPTIONALITY_FLAG = "MAIN_EXCEPTIONALITY_FLAG";
    private static final String ELEMENT_NON_IDENTIFIED_STUDENT_FLAG = "NON_IDENTIFIED_STUDENT_FLAG";
    private static final String ELEMENT_SPECIAL_EDU_PLMNT_TYPE = "SPECIAL_EDU_PLMNT_TYPE";

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        if (getGlobalData().getSubmissionType().isOctoberSubmission()
                && getParentEntity() instanceof OnsisStudentEnrollmentEntity
                && ((OnsisStudentEnrollmentEntity) getParentEntity()).getSpan() == null) {
            setBeans(Collections.EMPTY_LIST);
        } else {
            setBeans(Arrays.asList(getParentEntity().getBean()));
        }
    }

    /**
     * Gets the calcs.
     *
     * @return Map
     * @see com.follett.fsc.aspensif.framework.SifStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();

        calcs.put(OnsisRetrieverAction.CALC_ID, new OnsisRetrieverAction());

        return calcs;
    }

    /**
     * Generate and append delete.
     *
     * @param record OnsisCsvDataRecord
     * @param currentEntityKeySet List<String>
     * @param currentEntityValueSet List<String>
     * @param parentElement Element
     * @return Element
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#generateAndAppendDelete(com.x2dev.procedures.statereporting.on.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord,
     *      java.util.List, java.util.List, org.w3c.dom.Element)
     */
    @Override
    protected Element generateAndAppendDelete(OnsisCsvDataRecord record,
                                              List<String> currentEntityKeySet,
                                              List<String> currentEntityValueSet,
                                              Element parentElement) {
        Element thisElement =
                super.generateAndAppendDelete(record, currentEntityKeySet, currentEntityValueSet, parentElement);

        // Get additional required fields from the CSV and add onto this Element
        if (record == null) {
            return thisElement;
        }

        // Exceptionality Type is already in the Delete because it's a key
        // addIfNotBlank(record, EXCEPTIONALITY_TYPE, ELEMENT_EXCEPTIONALITY_TYPE, thisElement);
        addIfNotBlank(record, CsvField.INDIVIDUAL_EDUCATION_PLAN_FLAG, ELEMENT_INDIVIDUAL_EDUCATION_PLAN_FLAG,
                thisElement);
        addIfNotBlank(record, CsvField.IPRC_STUDENT_FLAG, ELEMENT_IPRC_STUDENT_FLAG, thisElement);
        addIfNotBlank(record, CsvField.IPRC_REVIEW_DATE, ELEMENT_IPRC_REVIEW_DATE, thisElement);
        addIfNotBlank(record, CsvField.MAIN_EXCEPTIONALITY_FLAG, ELEMENT_MAIN_EXCEPTIONALITY_FLAG, thisElement);
        addIfNotBlank(record, CsvField.NON_IDENTIFIED_STUDENT_FLAG, ELEMENT_NON_IDENTIFIED_STUDENT_FLAG, thisElement);
        addIfNotBlank(record, CsvField.SPECIAL_EDU_PLMNT_TYPE, ELEMENT_SPECIAL_EDU_PLMNT_TYPE, thisElement);

        return thisElement;
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisSpedEntity.class);
    }

}
