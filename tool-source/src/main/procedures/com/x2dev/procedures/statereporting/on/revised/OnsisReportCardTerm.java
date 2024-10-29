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
import com.x2dev.procedures.statereporting.common.ToolBean.ToolDistrictContext;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolGradeTermDate;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentContextAttributes;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.OnsisAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisReportCard.OnsisReportCardEntity;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEnrollmentEntity;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The Class OnsisReportCardTerm.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisReportCardTerm extends OnsisStateReportData {

    /**
     * The Class OnsisReportCardTermEntity.
     */
    public static class OnsisReportCardTermEntity extends OnsisStateReportEntity {
        public static final String ALT_REPORT_CARD_BOTH = "B";
        public static final String ALT_REPORT_CARD_TERM_1 = "1";
        public static final String ALT_REPORT_CARD_TERM_2 = "2";
        public static final List<String> ALT_REPORT_CARD_VALUES =
                Arrays.asList(ALT_REPORT_CARD_BOTH, ALT_REPORT_CARD_TERM_1, ALT_REPORT_CARD_TERM_2);

        public static final String ELEMENT_LEARNING_SKILL = "LEARNING_SKILL";
        public static final String ELEMENT_NA_FLAG = "NA_FLAG";
        public static final String ELEMENT_SUBJECT_STRAND = "SUBJECT_STRAND";
        public static final String ELEMENT_SUBJECT_STRAND_CODE = "SUBJECT_STRAND_CODE";

        public static final String FIELD_NAME_ALT_REPORT_CARD = "AltReportCardFlag";
        public static final String FLAG_T = "T";


        private List<ToolGradeTermDate> m_gradeTerms = new ArrayList<>();

        /**
         * Gets the alt report card flag.
         *
         * @return the alt report card flag
         */
        public Boolean getAltReportCardFlag() {
            boolean debug = getGlobalData().getDebugDetail();
            OnsisStudent student = getStudent();
            if (debug) {
                String studentOid = (student == null) ? null : student.getOid();
                getReportData().log("getAltReportCardFlag for " + studentOid);
            }

            String altReportCardValue = null;
            /*
             * Get value from W record on Enrollment span
             */
            OnsisStudentEnrollmentEntity enrolmentEntity =
                    getReportData().getParentEntity().getReportData().getParentEntity();
            OnEnrollment lastEnrollment = (OnEnrollment) enrolmentEntity.getSpan().getDataStorageEnrollment();
            if (debug) {
                getReportData().log("lastEnrollment " + lastEnrollment);
            }
            if (lastEnrollment != null) {
                String altReportCardFlagState = lastEnrollment.getElementaryAlternateReportCardFlag();
                if (debug) {
                    getReportData().log("altReportCardFlagState from lastEnrollment " + altReportCardFlagState);
                }
                if (altReportCardFlagState != null && ALT_REPORT_CARD_VALUES.contains(altReportCardFlagState)) {
                    altReportCardValue = altReportCardFlagState;
                }
            }

            /*
             * else get value from Student record or Student Context Attributes
             * depending on context
             */
            if (altReportCardValue == null) {
                ToolDistrictContext spanContext = getSpan().getContext();

                // Only return TRUE/FALSE if the question has been positively answered Yes or No
                if (spanContext == null || spanContext.getOid().equals(getReportData().getCurrentContext().getOid())) {
                    // Get current value from Student record
                    String altReportCardFlagState = getStudent().getElementaryAlternateReportCardFlag();
                    if (debug) {
                        getReportData().log(
                                "spanContext is current year so not looking at Student Context Attributes. state value from from Student [all-std-ElementaryAlternateReportCard] "
                                        + altReportCardFlagState);
                    }
                    if (altReportCardFlagState != null && ALT_REPORT_CARD_VALUES.contains(altReportCardFlagState)) {
                        altReportCardValue = altReportCardFlagState;
                    }
                } else {
                    // Get value from StudentContextAttributes
                    String altReportCardFlagState;
                    ToolStudentContextAttributes attributes =
                            getStudent().getStudentContextAttributes(getBroker()).stream()
                                    .filter(attr -> spanContext.getOid().equals(attr.getContextOid())).findFirst()
                                    .orElse(null);
                    if (attributes != null) {
                        attributes.getAttributeValue(getBroker(), getDictionaryExtractor(), OnsisStudent.FIELD_RETAINED,
                                false);
                    }
                    altReportCardFlagState = (String) attributes.getAttributeValue(getBroker(),
                            getDictionaryExtractor(), OnsisStudent.FIELD_ELEM_ALT_REPORT_CARD_FLAG, true);
                    if (debug) {
                        getReportData().log("Looking at Student Context Attributes for " + spanContext
                                + "State value from from context attribute [all-std-ElementaryAlternateReportCard] "
                                + altReportCardFlagState);
                    }
                    if (altReportCardFlagState != null && ALT_REPORT_CARD_VALUES.contains(altReportCardFlagState)) {
                        altReportCardValue = altReportCardFlagState;
                    }
                }
            }
            if (altReportCardValue != null && ALT_REPORT_CARD_VALUES.contains(altReportCardValue)) {
                if (ALT_REPORT_CARD_BOTH.equals(altReportCardValue)) {
                    return Boolean.TRUE;
                } else if (altReportCardValue.equals(getTermCode())) {
                    return Boolean.TRUE;
                }

            }
            return Boolean.FALSE;
        }

        /**
         * Get the days absent.
         *
         * @return the days absent
         */
        public Double getDaysAbsent() {
            throw new IllegalStateException("DAYS_ABSENT not supported");

        }

        /**
         * Gets the grade term date.
         *
         * @return the grade term date
         */
        public ToolGradeTermDate getGradeTermDate() {
            return m_gradeTerms.get(getCurrentRow());
        }

        /**
         * Gets the report data.
         *
         * @return the report data
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#getReportData()
         */
        @Override
        public OnsisReportCardTerm getReportData() {
            return (OnsisReportCardTerm) super.getReportData();
        }

        /**
         * Gets the span.
         *
         * @return AnnualSpan
         */
        public OnsisAnnualSpan getSpan() {
            OnsisReportCardEntity parentEntity = getReportData().getParentEntity();

            return parentEntity.getAnnualSpan();
        }

        /**
         * Gets the student.
         *
         * @return the student
         */
        public OnsisStudent getStudent() {
            OnsisReportCardEntity parentEntity = getReportData().getParentEntity();

            return parentEntity.getStudent();
        }

        /**
         * Get the times late.
         *
         * @return the times late
         */
        public String getTimesLate() {
            throw new IllegalStateException("TIMES_LATE not supported");
        }

        /**
         * Gets the term code.
         *
         * @return the term code
         */
        public String getTermCode() {
            ToolGradeTermDate gradeTermDate = getGradeTermDate();
            if (gradeTermDate == null) {
                return null;
            }

            return Integer.toString(gradeTermDate.getGradeTermNum());
        }

        /**
         * Intitialize.
         *
         * @param data the data
         * @param bean the bean
         * @throws X2BaseException the x 2 base exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            OnsisReportCardTerm reportData = getReportData();
            OnsisReportCardEntity parentEntity = reportData.getParentEntity();
            List<ToolGradeTermDate> gradeTerms = parentEntity.getGradeTerms();
            for (ToolGradeTermDate gtd : gradeTerms) {
                String gradeTermId = gtd.getGradeTermId();
                PlainDate startDate = gtd.getStartDate();
                PlainDate endDate = gtd.getEndDate();
                getReportData().log("OnsisReportCardTerm.intitialize(GradeTermDate " + gradeTermId + ": " + startDate
                        + " to " + endDate + ".)");
            }

            m_gradeTerms = gradeTerms.stream()
                    // the string "Ã©lÃ©mentaires" is "élémentaires" once translated to UTF-8
                    .filter(gta -> gta.getGradeTermDefinitionName().contains("Elementary")
                            || gta.getGradeTermDefinitionName().contains("élémentaires")
                            || gta.getGradeTermDefinitionName().contains("Ã©lÃ©mentaires"))
                    .sorted(Comparator.comparing(ToolGradeTermDate::getGradeTermNum))
                    .collect(Collectors.toList());

            setRowCount(m_gradeTerms.size());
        }

        /**
         * Checks if is cancelable.
         *
         * @return true, if is cancelable
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isCancelable()
         */
        @Override
        protected boolean isCancelable() {
            return true;
        }

        /**
         * Checks if is row canceled.
         *
         * @param entityElement the entity element
         * @param parentElement the parent element
         * @return true, if is row canceled
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isRowCanceled(org.jdom.Element,
         *      org.jdom.Element)
         */
        @Override
        protected boolean isRowCanceled(Element entityElement, Element parentElement) {
            /*
             * A Report Card Term requires:
             * 1. At least one Learning Skill or Subject Strand
             * 2. OR altReportCardFlag=Y
             */
            boolean isLearningSkill = entityElement.getElementsByTagName(ELEMENT_LEARNING_SKILL).getLength() > 0;
            boolean isSubjectStrand = entityElement.getElementsByTagName(ELEMENT_SUBJECT_STRAND).getLength() > 0;
            boolean isAltReportCard = FLAG_T.equals(getFieldValue(FIELD_NAME_ALT_REPORT_CARD));
            boolean isRowCanceled = !(isLearningSkill || isSubjectStrand || isAltReportCard);

            getReportData().log("ReportCardTerm.isRowCanceled termCode [" + getFieldValue("TermCode") + "] returning ["
                    + isRowCanceled
                    + "]. Detail: isLearningSkill[" + isLearningSkill + "] isSubjectStrand[" + isSubjectStrand
                    + "] isAltReportCard[" + isAltReportCard + "]");

            return isRowCanceled;
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
     * Gets the calcs.
     *
     * @return Map
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();

        calcs.put(OnsisRetrieverAction.CALC_ID, new OnsisRetrieverAction());

        return calcs;
    }

    /**
     * Gets the parent entity.
     *
     * @return the parent entity
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getParentEntity()
     */
    @Override
    public OnsisReportCardEntity getParentEntity() {
        return (OnsisReportCardEntity) super.getParentEntity();
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisReportCardTermEntity.class);
    }

}
