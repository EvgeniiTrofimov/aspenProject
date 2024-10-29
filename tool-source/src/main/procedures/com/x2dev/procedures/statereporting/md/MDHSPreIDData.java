/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * MD HS PreID export.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class MDHSPreIDData extends MDStudentReportData {

    /**
     * MD HS PreID Entity.
     *
     * @author X2 Development Corporation
     */
    public static class MDHSPreIDEntity extends MDStudentReportEntity {
        private MDHSPreIDData m_data;
        private String m_testFormatHSGOV;
        private String m_testFormatHSMISA;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public MDHSPreIDEntity() {
            // public no argument constructor for dynamic instantiation.
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
            String name = "[" + student.getStateId() + "]"
                    + " [" + student.getLocalId() + "]"
                    + " [" + student.getNameView() + "]";
            return name;
        }

        /**
         * Gets the school.
         *
         * @param data MDStudentReportData
         * @return Sis school
         * @see com.x2dev.procedures.statereporting.md.MDStudentReportData.MDStudentReportEntity#getSchool(com.x2dev.procedures.statereporting.md.MDStudentReportData)
         */
        @Override
        public SisSchool getSchool(MDStudentReportData data) {
            return ((SisStudent) getBean()).getSchool();
        }

        /**
         * Gets the test format HSGOV.
         *
         * @return the m_testFormatHSGOV
         */
        public String getTestFormatHSGOV() {
            return m_testFormatHSGOV;
        }

        /**
         * Gets the test format HSMISA.
         *
         * @return the m_testFormatHSMISA
         */
        public String getTestFormatHSMISA() {
            return m_testFormatHSMISA;
        }

        /**
         * Initialize the entity for the student bean provided.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (MDHSPreIDData) data;
            if (metHSGuidlines((SisStudent) bean)) {
                setRowCount(0);
            }
        }

        /**
         * Sets the test format HSGOV.
         *
         * @param testFormatHSGOV void
         */
        public void setTestFormatHSGOV(String testFormatHSGOV) {
            this.m_testFormatHSGOV = testFormatHSGOV;
        }

        /**
         * Sets the test format HSMISA.
         *
         * @param testFormatHSMISA void
         */
        public void setTestFormatHSMISA(String testFormatHSMISA) {
            this.m_testFormatHSMISA = testFormatHSMISA;
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * Find best score.
         *
         * @param assessments Collection<StudentAssessment>
         * @param defIds List<String>
         * @return StudentAssessment
         */
        private StudentAssessment findBestScoreAsmForDefId(Collection<StudentAssessment> assessments,
                                                           List<String> defIds) {
            StudentAssessment asm = null;
            BigDecimal bestScore = BigDecimal.ZERO;
            if (assessments != null && !assessments.isEmpty()) {
                for (StudentAssessment assessment : assessments) {
                    if (defIds.contains(assessment.getAssessmentDefinition().getId())) {
                        if (assessment.getScaleScore() != null && assessment.getScaleScore().compareTo(bestScore) > 0) {
                            bestScore = assessment.getScaleScore();
                            asm = assessment;
                        }
                    }
                }
            }
            return asm;
        }

        /**
         * Find best score checking subject area.
         *
         * @param assessments Collection<StudentAssessment>
         * @param defIds List<String>
         * @param subjectArea String
         * @return StudentAssessment
         */
        private StudentAssessment findBestScoreAsmForDefId(Collection<StudentAssessment> assessments,
                                                           List<String> defIds,
                                                           String subjectArea) {
            StudentAssessment asmToReturn = null;
            BigDecimal bestScore = BigDecimal.ZERO;
            if (assessments != null && !assessments.isEmpty()) {
                for (StudentAssessment assessment : assessments) {
                    if (defIds.contains(assessment.getAssessmentDefinition().getId())) {
                        if (subjectArea.equals(assessment.getFieldValueByBeanPath(m_data.m_asmFieldSubjArea))
                                && assessment.getScaleScore() != null
                                && assessment.getScaleScore().compareTo(bestScore) > 0) {
                            bestScore = assessment.getScaleScore();
                            asmToReturn = assessment;
                        }
                    }
                }
            }
            return asmToReturn;
        }

        /**
         * Calculate count of the ASMs founded for the given conditions.
         *
         * @param assessments Collection<StudentAssessment>
         * @param defIdNoSubgArea String
         * @param defIdSubgArea String
         * @param subjectArea String
         * @param countToCompare int
         * @return true, if successful
         */
        private boolean findAsmCountForDefId(Collection<StudentAssessment> assessments,
                                             String defIdNoSubgArea,
                                             String defIdSubgArea,
                                             String subjectArea,
                                             int countToCompare) {
            boolean moreTimes = false;
            if (assessments != null && !assessments.isEmpty()) {
                int count = 0;
                for (StudentAssessment assessment : assessments) {
                    if (defIdNoSubgArea.equals(assessment.getAssessmentDefinition().getId())
                            || (defIdSubgArea.equals(assessment.getAssessmentDefinition().getId()) && subjectArea
                                    .equals(assessment.getFieldValueByBeanPath(m_data.m_asmFieldSubjArea)))) {
                        count++;
                        if (count >= countToCompare) {
                            moreTimes = true;
                            break;
                        }
                    }
                }
            }
            return moreTimes;
        }

        /**
         * Calculate combined scores for HSGOV and HSMISA definition.
         *
         * @param asmHsaGov StudentAssessment
         * @param asmHsaSubjAreaHSGOV StudentAssessment
         * @param asmHsaBio StudentAssessment
         * @param asmHsaSubjAreaHSMISA StudentAssessment
         * @return Big decimal
         */
        private BigDecimal getCombinedScores(StudentAssessment asmHsaGov,
                                             StudentAssessment asmHsaSubjAreaHSGOV,
                                             StudentAssessment asmHsaBio,
                                             StudentAssessment asmHsaSubjAreaHSMISA) {
            BigDecimal combinedScore = BigDecimal.ZERO;
            BigDecimal asmHsaGovScore =
                    asmHsaGov != null && asmHsaGov.getScaleScore() != null ? asmHsaGov.getScaleScore()
                            : BigDecimal.ZERO;
            BigDecimal asmHsaSubjAreaHSGOVScore =
                    asmHsaSubjAreaHSGOV != null && asmHsaSubjAreaHSGOV.getScaleScore() != null
                            ? asmHsaSubjAreaHSGOV.getScaleScore()
                            : BigDecimal.ZERO;
            combinedScore = combinedScore.add(asmHsaGovScore.compareTo(asmHsaSubjAreaHSGOVScore) >= 0 ? asmHsaGovScore
                    : asmHsaSubjAreaHSGOVScore);
            BigDecimal asmHsaBioScore =
                    asmHsaBio != null && asmHsaBio.getScaleScore() != null ? asmHsaBio.getScaleScore()
                            : BigDecimal.ZERO;
            BigDecimal asmHsaSubjAreaHSMISAScore =
                    asmHsaSubjAreaHSMISA != null && asmHsaSubjAreaHSMISA.getScaleScore() != null
                            ? asmHsaSubjAreaHSMISA.getScaleScore()
                            : BigDecimal.ZERO;
            combinedScore = combinedScore.add(asmHsaBioScore.compareTo(asmHsaSubjAreaHSMISAScore) >= 0 ? asmHsaBioScore
                    : asmHsaSubjAreaHSMISAScore);
            return combinedScore;
        }

        /**
         * Check if we need to include student.
         *
         * @param std SisStudent
         * @return true, if successful
         */
        private boolean metHSGuidlines(SisStudent std) {
            Collection<StudentAssessment> asmsHSGOV = m_data.m_asmHSGOVMap.get(std.getOid());
            Collection<StudentAssessment> asmsHSMISA = m_data.m_asmHSMISAMap.get(std.getOid());

            Boolean metHsGov = null;
            Boolean metHsMisa = null;
            StudentAssessment asmHsaGov = findBestScoreAsmForDefId(asmsHSGOV, Arrays.asList(ASM_DEF_ID_HSA_GOV));
            StudentAssessment asmHsaSubjAreaHSGOV =
                    findBestScoreAsmForDefId(asmsHSGOV, Arrays.asList(ASM_DEF_ID_HSA), SUBJ_AREA_HSGOV);
            StudentAssessment asmHsaSubjAreaHSMISA =
                    findBestScoreAsmForDefId(asmsHSGOV, Arrays.asList(ASM_DEF_ID_HSA), SUBJ_AREA_HSMISA);
            StudentAssessment asmHsaBio = findBestScoreAsmForDefId(asmsHSMISA, Arrays.asList(ASM_DEF_ID_HSA_BIO));

            if (!m_data.hasGovernmentCredit(std)) {
                metHsGov = Boolean.TRUE;
            } else if (asmHsaGov != null && asmHsaGov.getScaleScore().compareTo(SCALE_SCORE_394) >= 0) {
                metHsGov = Boolean.TRUE;
            } else if (asmHsaSubjAreaHSGOV != null
                    && asmHsaSubjAreaHSGOV.getScaleScore().compareTo(SCALE_SCORE_394) >= 0) {
                metHsGov = Boolean.TRUE;
            } else if (passedBridgeAssessment(asmsHSGOV,
                    Arrays.asList(ASM_DEF_ID_BRIDGE_GOVT, ASM_DEF_ID_HSA_BRIDGE_GOV), null)) {
                metHsGov = Boolean.TRUE;
            } else if (findAsmCountForDefId(asmsHSGOV, ASM_DEF_ID_HSA_GOV, ASM_DEF_ID_HSA,
                    SUBJ_AREA_HSGOV, 2)) {
                metHsGov = Boolean.TRUE;
            } else if (getCombinedScores(asmHsaGov, asmHsaSubjAreaHSGOV, asmHsaBio,
                    asmHsaSubjAreaHSMISA)
                            .compareTo(SCALE_SCORE_794) >= 0) {
                metHsGov = Boolean.TRUE;
                metHsMisa = Boolean.TRUE;
            }
            if (metHsGov == null) {
                metHsGov = Boolean.FALSE;
                m_testFormatHSGOV = m_data.m_testFormat;
            }

            if (metHsMisa == null) {
                if (!m_data.isSenior(std)) {
                    metHsMisa = Boolean.TRUE;
                } else if (asmHsaBio != null && asmHsaBio.getScaleScore().compareTo(SCALE_SCORE_400) >= 0) {
                    metHsMisa = Boolean.TRUE;
                } else if (asmHsaSubjAreaHSMISA != null
                        && asmHsaSubjAreaHSMISA.getScaleScore().compareTo(SCALE_SCORE_400) >= 0) {
                    metHsMisa = Boolean.TRUE;
                } else if (findAsmCountForDefId(asmsHSMISA, ASM_DEF_ID_HSA_BIO, ASM_DEF_ID_HSA,
                        SUBJ_AREA_HSMISA, 2)) {
                    metHsMisa = Boolean.TRUE;
                } else if (tookAssessmentBefore(asmsHSMISA, Arrays.asList(ASM_DEF_ID_HSA_BIO), m_data.dateEnd2017())) {
                    metHsMisa = Boolean.TRUE;
                } else if (passedBridgeAssessment(asmsHSMISA, Arrays.asList(ASM_DEF_ID_HSA_BRIDGE_BIO), null)) {
                    metHsMisa = Boolean.TRUE;
                } else if (passedBridgeAssessment(asmsHSMISA, Arrays.asList(ASM_DEF_ID_HSA), SUBJ_AREA_HSMISA)) {
                    metHsMisa = Boolean.TRUE;
                }
                if (metHsMisa == null) {
                    metHsMisa = Boolean.FALSE;
                    m_testFormatHSMISA = m_data.m_testFormat;
                }
            }
            return metHsGov.booleanValue() && metHsMisa.booleanValue();
        }

        /**
         * Passed bridge assessment.
         *
         * @param assessments Collection<StudentAssessment>
         * @param defIds List<String>
         * @param string
         * @return true, if successful
         */
        private boolean passedBridgeAssessment(Collection<StudentAssessment> assessments,
                                               List<String> defIds,
                                               String subjectArea) {
            boolean value = false;
            if (assessments != null && !assessments.isEmpty()) {
                for (StudentAssessment assessment : assessments) {
                    if (defIds.contains(assessment.getAssessmentDefinition().getId())
                            &&
                            (subjectArea == null || subjectArea
                                    .equals(assessment.getFieldValueByBeanPath(m_data.m_asmFieldSubjArea)))
                            &&
                            ("P".equals(assessment.getFieldValueByBeanPath(m_data.m_asmFieldHsaPassFlag))
                                    || "P".equals(assessment.getPerformanceLevel()))) {
                        value = true;
                        break;
                    }
                }
            }
            return value;
        }

        /**
         * Took assessment before.
         *
         * @param assessments Collection<StudentAssessment>
         * @param defIds List<String>
         * @param date PlainDate
         * @return true, if successful
         */
        private boolean tookAssessmentBefore(Collection<StudentAssessment> assessments,
                                             List<String> defIds,
                                             PlainDate date) {
            boolean value = false;
            if (assessments != null && !assessments.isEmpty()) {
                for (StudentAssessment assessment : assessments) {
                    if (defIds.contains(assessment.getAssessmentDefinition().getId())) {
                        if (assessment.getDate() != null && !assessment.getDate().after(date)) {
                            value = true;
                            break;
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * The Class RetrieveIACValues.
     */
    /*
     * Get values for fields concerning Student Accommodations
     */
    protected class RetrieveIACValues implements FieldRetriever {
        protected static final String VALUE_CRS_TNR_LEVEL_CODE = "TNR";
        protected static final String VALUE_ELA_TEST_CODE = "ELA";
        protected final List VALUE_STD_DISAB = Arrays.asList(new String[] {"IEP", "504"});

        /**
         * CALC ID
         */
        protected static final String CALC_ID = "ACCOMODATIONS";

        /**
         * Values
         */
        protected static final String VALUE_EL = "EL";
        protected static final String VALUE_IEP504 = "IEP504";
        protected static final String VALUE_N = "N";
        protected static final String VALUE_P = "P";
        protected static final String VALUE_Y = "Y";

        /**
         * FIELDS
         */
        protected static final String FIELD_ENGLISH_LEARNER = "English Learner";
        protected static final String FIELD_PAPER_BASED_TEST = "TestFormat";
        protected static final String FIELD_STD_DISAB = "StdDisabilities";
        protected static final String FIELD_TEST_CODE = "TestCode";

        /**
         * CALC PARAMS
         */
        protected static final String CALC_PARAM_ALT_LOCATION = "2c";
        protected static final String CALC_PARAM_ALT_REPR_PAPER = "3g";
        protected static final String CALC_PARAM_ANSW_MASK = "1a";
        protected static final String CALC_PARAM_ANSW_PAPER = "4t";
        protected static final String CALC_PARAM_ASL_VIDEO = "ASLVideo";
        protected static final String CALC_PARAM_BRAILLE_DISPL_ELA = "3c";
        protected static final String CALC_PARAM_BRAILLE_RESP = "BrailleResp";
        protected static final String CALC_PARAM_CAPT_ELA = "3h";
        protected static final String CALC_PARAM_DAY_TIME = "2b";
        protected static final String CALC_PARAM_ELA_CONSTR = "ConstrRespELA";
        protected static final String CALC_PARAM_ELA_SELECTED = "SelectedRespELA";
        protected static final String CALC_PARAM_EXT_TIME = "ExtendedTime";
        protected static final String CALC_PARAM_HUM_SIGNER = "3m";
        protected static final String CALC_PARAM_FR_BREAKS = "2f";
        protected static final String CALC_PARAM_LARGE_PAPER = "LargePrintPaper";
        protected static final String CALC_PARAM_MATH_EL = "ResponseMathEL";
        protected static final String CALC_PARAM_MATH_TOOLS = "4e";
        protected static final String CALC_PARAM_MATH_TRANSL = "TranslationOfMath";
        protected static final String CALC_PARAM_MONITOR_TEST = "4r";
        protected static final String CALC_PARAM_NAT_LANG_ALOUD = "7e";
        protected static final String CALC_PARAM_NAT_LANG_IN = "7f";
        protected static final String CALC_PARAM_NON_SCR_READER = "NonScreenReader";
        protected static final String CALC_PARAM_READ_ASM_ALOUD = "1q";
        protected static final String CALC_PARAM_UNIQUE = "Unique";
        protected static final String CALC_PARAM_RESP_MATH = "ResponseMath";
        protected static final String CALC_PARAM_SCR_READER = "3b";
        protected static final String CALC_PARAM_SPEC_AREA = "2d";
        protected static final String CALC_PARAM_SPEC_EQUIP = "2e";
        protected static final String CALC_PARAM_TEST_GROUPS = "2a";
        protected static final String CALC_PARAM_WORD_DICT = "7b";
        protected static final String CALC_PARAM_WORD_PREDICTION = "4s";

        /**
         * Calculation Parameters.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;
            Collection<String> stdIacCodes = m_iacsCodesMap.get(entity.getBean().getOid());
            String paramString = (String) field.getParameter();
            if (stdIacCodes != null && !stdIacCodes.isEmpty()) {
                if (CALC_PARAM_FR_BREAKS.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_ALT_LOCATION.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_TEST_GROUPS.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_READ_ASM_ALOUD.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_SPEC_EQUIP.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_SPEC_AREA.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_DAY_TIME.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_ANSW_MASK.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_ASL_VIDEO.equals(paramString)) {
                    String code1 = "3j";
                    String code2 = "3l";
                    if (!StringUtils.isEmpty(entity.getFieldValue(FIELD_TEST_CODE)) &&
                            ((!entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE)
                                    && stdIacCodes.contains(code1)) ||
                                    (!entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE)
                                            && stdIacCodes.contains(code2)))) {
                        value = VALUE_Y;
                    }
                } else if (CALC_PARAM_SCR_READER.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_NON_SCR_READER.equals(paramString)) {
                    String code1 = "3a";
                    String code2 = "4a";

                    if (stdIacCodes.contains(code1) || stdIacCodes.contains(code2)) {
                        value = VALUE_Y;
                    }
                } else if (CALC_PARAM_CAPT_ELA.equals(paramString) &&
                        !StringUtils.isEmpty(entity.getFieldValue(FIELD_TEST_CODE)) &&
                        entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE) &&
                        stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_BRAILLE_DISPL_ELA.equals(paramString) &&
                        !StringUtils.isEmpty(entity.getFieldValue(FIELD_TEST_CODE)) &&
                        entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE) &&
                        stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_ALT_REPR_PAPER.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_LARGE_PAPER.equals(paramString)) {
                    String code1 = "3f";
                    String code2 = "7i";

                    if (stdIacCodes.contains(code1) || stdIacCodes.contains(code2)) {
                        value = VALUE_Y;
                    }
                } else if (CALC_PARAM_HUM_SIGNER.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_ANSW_PAPER.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_BRAILLE_RESP.equals(paramString)) {
                    String code1 = "4b";
                    String code2 = "4c";

                    if (stdIacCodes.contains(code1)) {
                        value = "01";
                    }

                    if (stdIacCodes.contains(code2)) {
                        value = "02";
                    }
                } else if (CALC_PARAM_MATH_TOOLS.equals(paramString) &&
                        !StringUtils.isEmpty(entity.getFieldValue(FIELD_TEST_CODE)) &&
                        !entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE) &&
                        stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_ELA_CONSTR.equals(paramString) &&
                        !StringUtils.isEmpty(entity.getFieldValue(FIELD_TEST_CODE)) &&
                        entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE)) {
                    String code1 = "4n";
                    String code2 = "40";
                    String code3 = "4p";
                    String code4 = "4q";

                    if (stdIacCodes.contains(code1)) {
                        value = "01";
                    } else if (stdIacCodes.contains(code2)) {
                        value = "02";
                    } else if (stdIacCodes.contains(code3)) {
                        value = "03";
                    } else if (stdIacCodes.contains(code4)) {
                        value = "04";
                    }
                } else if (CALC_PARAM_ELA_SELECTED.equals(paramString) &&
                        !StringUtils.isEmpty(entity.getFieldValue(FIELD_TEST_CODE)) &&
                        entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE)) {
                    String code1 = "4f";
                    String code2 = "4g";
                    String code3 = "4h";
                    String code4 = "4i";

                    if (stdIacCodes.contains(code1)) {
                        value = "01";
                    } else if (stdIacCodes.contains(code2)) {
                        value = "02";
                    } else if (stdIacCodes.contains(code3)) {
                        value = "03";
                    } else if (stdIacCodes.contains(code4)) {
                        value = "04";
                    }
                } else if (CALC_PARAM_RESP_MATH.equals(paramString) &&
                        !StringUtils.isEmpty(entity.getFieldValue(FIELD_TEST_CODE)) &&
                        !entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE)) {
                    String code1 = "4j";
                    String code2 = "4k";
                    String code3 = "4l";
                    String code4 = "4m";

                    if (stdIacCodes.contains(code1)) {
                        value = "01";
                    } else if (stdIacCodes.contains(code2)) {
                        value = "02";
                    } else if (stdIacCodes.contains(code3)) {
                        value = "03";
                    } else if (stdIacCodes.contains(code4)) {
                        value = "04";
                    }
                } else if (CALC_PARAM_MONITOR_TEST.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_WORD_PREDICTION.equals(paramString) &&
                        !StringUtils.isEmpty(entity.getFieldValue(FIELD_TEST_CODE)) &&
                        entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE) &&
                        stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_NAT_LANG_IN.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_NAT_LANG_ALOUD.equals(paramString) && stdIacCodes.contains(paramString)) {
                    String homeLangCode = ((SisStudent) entity.getBean()).getHomeLanguageCode();
                    value = lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_HOME_LANGUAGE_CODE,
                            homeLangCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (CALC_PARAM_MATH_EL.equals(paramString) &&
                        !StringUtils.isEmpty(entity.getFieldValue(FIELD_TEST_CODE)) &&
                        !entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE)) {
                    String code1 = "7c";
                    String code2 = "7d";

                    if (stdIacCodes.contains(code1)) {
                        value = "01";
                    } else if (stdIacCodes.contains(code2)) {
                        value = "02";
                    }
                } else if (CALC_PARAM_MATH_TRANSL.equals(paramString) &&
                        !StringUtils.isEmpty(entity.getFieldValue(FIELD_TEST_CODE)) &&
                        !entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE)) {
                    String code1 = "7g";
                    String code2 = "7h";
                    String code3 = "7j";
                    String code4 = "7k";

                    if (stdIacCodes.contains(code1) ||
                            stdIacCodes.contains(code2) ||
                            stdIacCodes.contains(code3) ||
                            stdIacCodes.contains(code4)) {
                        value = "SPA";
                    }
                } else if (CALC_PARAM_WORD_DICT.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_UNIQUE.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_EXT_TIME.equals(paramString)) {
                    String code1 = "5a";
                    String code2 = "7a";

                    if (stdIacCodes.contains(code1) &&
                            VALUE_Y.equals(entity.getFieldValue(FIELD_ENGLISH_LEARNER)) &&
                            VALUE_P.equals(entity.getFieldValue(FIELD_PAPER_BASED_TEST))) {
                        value = VALUE_EL;
                    } else if (stdIacCodes.contains(code2) &&
                            VALUE_STD_DISAB.contains(entity.getFieldValue(FIELD_STD_DISAB)) &&
                            !StringUtils.isEmpty(entity.getFieldValue(FIELD_TEST_CODE)) &&
                            entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE)) {
                        value = VALUE_IEP504;
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retriever for sped services.
     *
     */
    protected class RetrieveInitConstants implements FieldRetriever {

        protected static final String CALC_ID = "INIT_CONSTS";
        protected static final String CALC_PARAM_ADMIN_CODE = "ADMIN-CODE";
        protected static final String CALC_PARAM_TEST_YEAR = "TEST-YEAR";
        protected static final String CALC_PARAM_TF_HSGOV = "TF-HSGOV";
        protected static final String CALC_PARAM_TF_HSMISA = "TF-HSMISA";
        protected static final String CURRENT_YEAR = "TF-HSMISA";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever.getFieldValue(
         *      StateReportData
         *      data, StateReportEntity entity, FieldDefinition field) throws X2BaseException
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;
            String param = (String) field.getParameter();
            MDHSPreIDEntity hsEntity = (MDHSPreIDEntity) entity;
            if (CALC_PARAM_ADMIN_CODE.equals(param)) {
                value = m_adminCode;
            } else if (CALC_PARAM_TEST_YEAR.equals(param)) {
                switch (m_adminCode) {
                    case "1":
                        value = "01" + getCurrentContext().getSchoolYear();
                        break;
                    case "2":
                        value = "05" + getCurrentContext().getSchoolYear();
                        break;
                    case "3":
                        value = "07" + getCurrentContext().getSchoolYear();
                        break;
                    default:
                        break;
                }
            } else if (CALC_PARAM_TF_HSGOV.equals(param)) {
                value = hsEntity.getTestFormatHSGOV();
            } else if (CALC_PARAM_TF_HSMISA.equals(param)) {
                value = hsEntity.getTestFormatHSMISA();
            }
            return value;
        }
    }

    /**
     * Class RetrievePrimaryDisability is the FieldRetriever used to retrieve
     * the primary disability code for the current student.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePrimaryDisab implements FieldRetriever {
        protected static final String CALC_ID = "PR_DIS_TYPE";

        private static final String DIS_CODE_AUT = "14";
        private static final String DIS_CODE_DB = "12";
        private static final String DIS_CODE_DD = "15";
        private static final String DIS_CODE_EMN = "06";
        private static final String DIS_CODE_HI = "02";
        private static final String DIS_CODE_ID = "01";
        private static final String DIS_CODE_MD = "10";
        private static final String DIS_CODE_OI = "07";
        private static final String DIS_CODE_OHI = "08";
        private static final String DIS_CODE_SLD = "09";
        private static final String DIS_CODE_SLI = "04";
        private static final String DIS_CODE_TBI = "13";
        private static final String DIS_CODE_VI = "05";

        private static final String DIS_RETURN_AUT = "AUT";
        private static final String DIS_RETURN_DB = "DB";
        private static final String DIS_RETURN_DD = "DD";
        private static final String DIS_RETURN_EMN = "EMN";
        private static final String DIS_RETURN_HI = "HI";
        private static final String DIS_RETURN_ID = "ID";
        private static final String DIS_RETURN_MD = "MD";
        private static final String DIS_RETURN_NC = "NC";
        private static final String DIS_RETURN_OI = "OI";
        private static final String DIS_RETURN_OHI = "OHI";
        private static final String DIS_RETURN_SLD = "SLD";
        private static final String DIS_RETURN_SLI = "SLI";
        private static final String DIS_RETURN_TBI = "TBI";
        private static final String DIS_RETURN_VI = "VI";

        private static final String REG_EX_FROM_1_TO_9 = "[1-9]{1}";

        /**
         * Returns the corresponding alias value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            String sped504 = (String) student.getFieldValueByBeanPath(m_studentSped504Field);
            String disCode = (String) student.getFieldValueByBeanPath(m_studentDisabField);
            if (!StringUtils.isEmpty(disCode)) {
                // Append "0" if code is a single digit
                if (disCode.matches(REG_EX_FROM_1_TO_9)) {
                    disCode = "0" + disCode;
                }
            }
            if (sped504 != null && "IEP".equals(sped504) && disCode != null) {
                switch (disCode) {
                    case DIS_CODE_AUT:
                        value = DIS_RETURN_AUT;
                        break;
                    case DIS_CODE_DB:
                        value = DIS_RETURN_DB;
                        break;
                    case DIS_CODE_DD:
                        value = DIS_RETURN_DD;
                        break;
                    case DIS_CODE_EMN:
                        value = DIS_RETURN_EMN;
                        break;
                    case DIS_CODE_HI:
                        value = DIS_RETURN_HI;
                        break;
                    case DIS_CODE_ID:
                        value = DIS_RETURN_ID;
                        break;
                    case DIS_CODE_MD:
                        value = DIS_RETURN_MD;
                        break;
                    case DIS_CODE_OHI:
                        value = DIS_RETURN_OHI;
                        break;
                    case DIS_CODE_OI:
                        value = DIS_RETURN_OI;
                        break;
                    case DIS_CODE_SLD:
                        value = DIS_RETURN_SLD;
                        break;
                    case DIS_CODE_SLI:
                        value = DIS_RETURN_SLI;
                        break;
                    case DIS_CODE_TBI:
                        value = DIS_RETURN_TBI;
                        break;
                    case DIS_CODE_VI:
                        value = DIS_RETURN_VI;
                        break;
                    default:
                        value = DIS_RETURN_NC;
                        break;
                }
            }
            return value == null ? "" : value;
        }
    }

    /*
     * Aliases
     */
    // ASM table
    protected static final String ALIAS_ASM_PASS_FLAG = "HSA_PASS_FLAG";
    protected static final String ALIAS_ASM_SUBJ_AREA = "HSA_SUBJECT_AREA";

    // CRS Table
    protected static final String ALIAS_CRS_GOV_CLASS = "all-crs-GovernmentClass";

    // STUDENT Table
    protected static final String ALIAS_DISABILITY = "DOE DISABILITY";
    protected static final String ALIAS_SPED_504 = "DOE SPED 504";
    protected static final String ALIAS_SPED_TO_504 = "DOE SPED TO 504";
    protected static final String ALIAS_STD_SPED_CERT = "DOE SPED CERT";

    /**
     * ASM Def IDs
     */
    protected static final String ASM_DEF_ID_BRIDGE_GOVT = "BRDG GOVT";
    protected static final String ASM_DEF_ID_HSA = "HSA";
    protected static final String ASM_DEF_ID_HSA_BIO = "HSA - BIOLOGY";
    protected static final String ASM_DEF_ID_HSA_BRIDGE_BIO = "HSA BRIDGE - BI";
    protected static final String ASM_DEF_ID_HSA_BRIDGE_GOV = "HSA BRIDGE - GO";
    protected static final String ASM_DEF_ID_HSA_GOV = "HSA - GOVERNMEN";

    /**
     * Other internal constants
     */
    protected static final BigDecimal SCALE_SCORE_394 = BigDecimal.valueOf(394);
    protected static final BigDecimal SCALE_SCORE_400 = BigDecimal.valueOf(400);
    protected static final BigDecimal SCALE_SCORE_794 = BigDecimal.valueOf(794);
    protected static final String SUBJ_AREA_HSGOV = "HSGOV";
    protected static final String SUBJ_AREA_HSMISA = "HSMISA";

    /**
     * Input parameters.
     */
    protected static final String INPUT_PARAM_ADMIN_CODE = "adminCode";
    protected static final String INPUT_PARAM_TEST_FORMAT = "testFormat";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_adminCode;
    protected String m_asmFieldSubjArea;
    protected String m_asmFieldHsaPassFlag;
    protected Map<String, Collection<StudentAssessment>> m_asmHSGOVMap;
    protected Map<String, Collection<StudentAssessment>> m_asmHSMISAMap;
    protected PlainDate m_dateEnd2017;
    protected String m_fieldCrsGovClass;
    protected StudentHistoryHelper m_helper;
    protected Map<String, Collection<String>> m_iacsCodesMap;
    protected Set<String> m_setGovernmentStudents;
    protected String m_stdFieldSpedCert;
    protected String m_studentDisabField;
    protected String m_studentSped504Field;
    protected String m_testFormat;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        initializeFields();
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
        m_helper.getStudentCriteria().addNotEqualTo(m_stdFieldSpedCert, BooleanAsStringConverter.TRUE);
        super.initialize();
        loadHSGOVAsmMap();
        loadHSMISAAsmMap();
        loadIacCodesMap();
        if (getSetupErrors().size() == 0) {
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(MDHSPreIDEntity.class);

            // Assign custom field retriever calculations.
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrievePrimaryDisab.CALC_ID, new RetrievePrimaryDisab());
            calcs.put(RetrieveInitConstants.CALC_ID, new RetrieveInitConstants());
            calcs.put(RetrieveIACValues.CALC_ID, new RetrieveIACValues());
            super.addCalcs(calcs);
        }
    }

    /**
     * Date end 2017.
     *
     * @return PlainDate
     */
    protected PlainDate dateEnd2017() {
        if (m_dateEnd2017 == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(2017));
            BeanQuery query = new BeanQuery(DistrictSchoolYearContext.class, criteria);
            DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) getBroker().getBeanByQuery(query);
            if (ctx != null) {
                m_dateEnd2017 = ctx.getEndDate();
            } else {
                addSetupError("Context year not found", "2017");
            }
        }
        return m_dateEnd2017;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    @Override
    protected Criteria getStudentCriteria() {
        return m_helper.getStudentCriteria();
    }

    /**
     * Gets the student query.
     *
     * @return Query by criteria
     */
    @Override
    protected QueryByCriteria getStudentQuery() {
        return m_helper.getStudentQuery(false);
    }

    /**
     * Checks for government credit.
     *
     * @param student SisStudent
     * @return true, if successful
     */
    protected boolean hasGovernmentCredit(SisStudent student) {
        if (m_setGovernmentStudents == null) {
            m_setGovernmentStudents = new HashSet();
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(Transcript.COL_STUDENT_OID,
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria()));
            criteria.addGreaterThan(Transcript.COL_TOTAL_CREDIT, BigDecimal.ZERO);
            criteria.addIn(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + m_fieldCrsGovClass,
                    Arrays.asList(new String[] {"Y", BooleanAsStringConverter.TRUE}));

            String[] columns = new String[] {Transcript.COL_STUDENT_OID};
            ColumnQuery query = new ColumnQuery(Transcript.class, columns, criteria);
            try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    m_setGovernmentStudents.add((String) row[0]);
                }
            }
        }
        return m_setGovernmentStudents.contains(student.getOid());
    }

    /**
     * Checks if is senior.
     *
     * @param std SisStudent
     * @return true, if is senior
     */
    protected boolean isSenior(SisStudent std) {
        return getCurrentContext().getSchoolYear() == std.getYog();
    }

    /**
     * initialize Fields.
     */
    private void initializeFields() {
        m_reportDate = (PlainDate) getParameter(INPUT_PARAM_REPORT_DATE);
        m_adminCode = (String) getParameter(INPUT_PARAM_ADMIN_CODE);

        dateEnd2017();

        // Load Alias database UDF names
        m_studentDisabField = translateAliasToJavaName(ALIAS_DISABILITY, true);
        m_studentSped504Field = translateAliasToJavaName(ALIAS_SPED_504, true);
        m_fieldCrsGovClass = translateAliasToJavaName(ALIAS_CRS_GOV_CLASS, true);
        m_stdFieldSpedCert = translateAliasToJavaName(ALIAS_STD_SPED_CERT, true);
        m_asmFieldSubjArea = translateAliasToJavaName(ALIAS_ASM_SUBJ_AREA, true);
        m_asmFieldHsaPassFlag = translateAliasToJavaName(ALIAS_ASM_PASS_FLAG, true);
        String rcdTestFormatOid = (String) getParameter(INPUT_PARAM_TEST_FORMAT);
        ReferenceCode rcdTestFormat = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, rcdTestFormatOid);
        m_testFormat = rcdTestFormat != null ? rcdTestFormat.getStateCode() : null;

    }

    /**
     * Get Student Assessment Criteria using the selected Student Oids.
     *
     * @return X2Criteria
     */
    private void loadHSGOVAsmMap() {
        X2Criteria asmCriteria = new X2Criteria();
        asmCriteria.addIn(StudentAssessment.COL_STUDENT_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria()));
        asmCriteria
                .addIn(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                        AssessmentDefinition.COL_ID,
                        Arrays.asList(ASM_DEF_ID_HSA, ASM_DEF_ID_HSA_GOV, ASM_DEF_ID_HSA_BRIDGE_GOV,
                                ASM_DEF_ID_BRIDGE_GOVT));
        m_asmHSGOVMap =
                getBroker().getGroupedCollectionByQuery(new QueryByCriteria(StudentAssessment.class, asmCriteria),
                        StudentAssessment.COL_STUDENT_OID, 1024);
    }

    /**
     * Get Student Assessment Criteria using the selected Student Oids.
     *
     * @return X2Criteria
     */
    private void loadHSMISAAsmMap() {
        X2Criteria studentAssessmentCriteria = new X2Criteria();
        // studentAssessmentCriteria.addEqualTo(
        // StudentAssessment.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_YOG,
        // String.valueOf(getCurrentContext().getSchoolYear()));
        studentAssessmentCriteria.addIn(StudentAssessment.COL_STUDENT_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria()));
        studentAssessmentCriteria
                .addIn(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                        AssessmentDefinition.COL_ID,
                        Arrays.asList(ASM_DEF_ID_HSA, ASM_DEF_ID_HSA_GOV, ASM_DEF_ID_HSA_BIO,
                                ASM_DEF_ID_HSA_BRIDGE_BIO));
        m_asmHSMISAMap = getBroker().getGroupedCollectionByQuery(
                new QueryByCriteria(StudentAssessment.class, studentAssessmentCriteria),
                StudentAssessment.COL_STUDENT_OID, 1024);
    }

    /**
     * Load map with current accommodations keyed on stdOid.
     *
     */
    private void loadIacCodesMap() {
        X2Criteria iacCriteria = new X2Criteria();
        iacCriteria.addNotNull(IepAccommodation.COL_NAME);
        QueryByCriteria query = m_helper.getStudentSelectionQuery(IepAccommodation.class, iacCriteria,
                IepAccommodation.COL_STUDENT_OID);
        Map<String, Collection<IepAccommodation>> iacsMap = getBroker().getGroupedCollectionByQuery(query,
                IepAccommodation.COL_STUDENT_OID,
                1024);
        m_iacsCodesMap = new HashMap<String, Collection<String>>();
        for (Entry<String, Collection<IepAccommodation>> entry : iacsMap.entrySet()) {
            String stdOid = entry.getKey();
            Collection<IepAccommodation> iacs = entry.getValue();
            Collection<String> iacCodes = new ArrayList<String>();
            if (iacs != null) {
                for (IepAccommodation iac : iacs) {
                    iacCodes.add(lookupReferenceCodeByBeanPath(IepAccommodation.class,
                            IepAccommodation.COL_NAME,
                            iac.getName(),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()));
                }
                if (!iacCodes.isEmpty()) {
                    m_iacsCodesMap.put(stdOid, iacCodes);
                }
            }
        }
    }
}
