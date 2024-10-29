/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RIPARCCPNP - Personal Needs Profile export.
 *
 * @author X2 Development Corporation
 */
public class RIPARCCPNP extends StateReportData {
    static RIPARCCPNP m_data;

    /**
     * Implementation of StateReportEntity to be used for PARCCPNP export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class RIPARCCPNPEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public RIPARCCPNPEntity() {
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
            StudentAssessment studentAssessment = (StudentAssessment) getBean();
            AssessmentDefinition assessmentDefinition = studentAssessment.getAssessmentDefinition();
            Student student = studentAssessment.getStudent();

            String name = " [" + assessmentDefinition.getId() + "]"
                    + " [" + student.getStateId() + "]"
                    + " [" + student.getLocalId() + "]"
                    + " [" + student.getNameView() + "]";

            return name;
        }

        /**
         * Returns the current student.
         *
         * @return SisStudent
         */
        public SisStudent getStudent() {
            StudentAssessment studentAssessment = (StudentAssessment) getBean();
            SisStudent student = studentAssessment.getStudent();

            return student;
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
            m_data = (RIPARCCPNP) data;
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

    }// end of Entity class

    /**
     * The Class RetrieveAssessment.
     */
    /*
     * Has the Student taken a PNP PBA, EOY or PBA & EOY test
     */
    protected class RetrieveAssessment implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            RIPARCCPNPEntity pnpEntity = (RIPARCCPNPEntity) entity;
            Object value = null;
            Object result = null;

            StudentAssessment studentAssessment = (StudentAssessment) pnpEntity.getBean();
            String param = (String) field.getParameter();

            if (CALC_PARAM_PARCC_TEST_CODE.equals(param)) {
                if (m_data.m_assessmentTestCodeField != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentTestCodeField);
                }
            } else if (CALC_PARAM_TEST_FORMAT.equals(param)) {
                Object code;
                if (m_data.m_assessmentTestFormatField != null) {
                    code = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentTestFormatField);

                    if (code != null) {
                        value = code;
                    }
                }
            } else if (CALC_PARAM_TEST_RETEST.equals(param)) {
                Object reTest;
                if (m_data.m_assessmentRetestField != null) {
                    reTest = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentRetestField);

                    value = CODE_NO;
                    if (CODE_ONE.equals(reTest)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_ALTR_PAPER_TEST.equals(param)) {
                if (m_data.m_assessmentAltRepPaperTestField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentAltRepPaperTestField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_MATH_ASSESS_IN_PAPER.equals(param)) {
                if (m_data.m_assessmentTransMathPaperField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentTransMathPaperField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_HUMAN_READ_HUMAN_SIG.equals(param)) {
                if (m_data.m_assessmentHumanField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentHumanField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_LARGE_PRINT.equals(param)) {
                if (m_data.m_assessmentLargePrintField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentLargePrintField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_BRAILLE.equals(param)) {
                if (m_data.m_assessmentBrailleField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentBrailleField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_FREQUENT_BREAKS.equals(param)) {
                if (m_data.m_assessmentACFreqBreaksField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACFreqBreaksField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_SEP_ALT_LOC.equals(param)) {
                if (m_data.m_assessmentACSepAltLocField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACSepAltLocField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_SMALL_TEST_GROUP.equals(param)) {
                if (m_data.m_assessmentACSmtsGrpField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACSmtsGrpField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_SPEC_EQUIP_FURN.equals(param)) {
                if (m_data.m_assessmentACSPEquipFurnField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACSPEquipFurnField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_SPEC_AREA_SETTING.equals(param)) {
                if (m_data.m_assessmentACSpecAreaField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACSpecAreaField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_TIME_OF_DAY.equals(param)) {
                if (m_data.m_assessmentACTodField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACTodField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_AF_ANS_MASKING.equals(param)) {
                if (m_data.m_assessmentACAnsMaskField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACAnsMaskField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_AF_COLOR_CONTRAST.equals(param)) {
                if (m_data.m_assessmentAFColorContrstField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentAFColorContrstField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_AF_TEST_SPEECH_MATH.equals(param)) {
                if (m_data.m_assessmentAFTTSMatFieldh != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentAFTTSMatFieldh);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_AF_HR_HS_MATH.equals(param)) {
                if (m_data.m_assessmentACHrHsMathField != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACHrHsMathField);
                }
            } else if (CALC_PARAM_PA_ASL_VIDEO.equals(param)) {
                if (m_data.m_assessmentACAslVideoField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACAslVideoField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_PA_AT_SRN_RDR.equals(param)) {
                if (m_data.m_assessmentACScreenRdrField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACScreenRdrField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_PA_LSD_CAP_ELA.equals(param)) {
                if (m_data.m_assessmentACCCELAField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACCCELAField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_PA_HR_HS_ELA.equals(param)) {
                if (m_data.m_assessmentACHRHSELAField != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACHRHSELAField);
                }
            } else if (CALC_PARAM_PA_REF_BRAL_ELA.equals(param)) {
                if (m_data.m_assessmentACRBELAField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACRBELAField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_PA_TAC_GRAPH.equals(param)) {
                if (m_data.m_assessmentACTGField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentACTGField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_PA_TXT_TO_SPCH.equals(param)) {
                if (m_data.m_assessmentPATTSELAField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentPATTSELAField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_RA_ANS_REC_TST_BK.equals(param)) {
                if (m_data.m_assessmentRAArtBField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentRAArtBField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_RA_BRAL_RESP.equals(param)) {
                if (m_data.m_assessmentRABRField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentRABRField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_RA_CALC_DEV_MATH.equals(param)) {
                if (m_data.m_assessmentRACDMTField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentRACDMTField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_RA_CNST_RESP_ELA.equals(param)) {
                if (m_data.m_assessmentRAELACRField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentRAELACRField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_RA_MATH_RESP.equals(param)) {
                if (m_data.m_assessmentRAMRField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentRAMRField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_RA_MON_TEST_RESP.equals(param)) {
                if (m_data.m_assessmentRAMTRField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentRAMTRField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_RA_SEL_RESP_TCH_ENH.equals(param)) {
                if (m_data.m_assessmentRASRTEField != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentRASRTEField);
                }
            } else if (CALC_PARAM_RA_WORD_PRED.equals(param)) {
                if (m_data.m_assessmentRAWPField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentRAWPField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_AEL_DIRC_STD_NAV_LANG.equals(param)) {
                if (m_data.m_assessmentAELDSNLField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentAELDSNLField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_AEL_DIRC_READ_ALOUD.equals(param)) {
                if (m_data.m_assessmentAELDRAField != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentAELDRAField);
                }
            } else if (CALC_PARAM_AEL_MATH_RESP_EL.equals(param)) {
                if (m_data.m_assessmentAELMRELField != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentAELMRELField);
                }
            } else if (CALC_PARAM_AEL_TRNS_MATH_TXT_SPCH.equals(param)) {
                if (m_data.m_assessmentAELTMATSField != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentAELTMATSField);
                }
            } else if (CALC_PARAM_AEL_TRANS_MATH_ONLINE.equals(param)) {
                if (m_data.m_assessmentAELTMAOLField != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentAELTMAOLField);
                }
            } else if (CALC_PARAM_AEL_WRD_TO_WRD_DICT.equals(param)) {
                if (m_data.m_assessmentAELWWDictField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentAELWWDictField);
                    if (CODE_ONE.equals(result)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_TSA_EXTND_TIME.equals(param)) {
                if (m_data.m_assessmentTCAETField != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentTCAETField);
                }
            }

            return value;
        }
    }

    /**
     * The parameter is the name of the program code on student program participation that is the
     * ELL program code for this district.
     */
    protected class RetrieveEnglishLanguageLearner implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();

            Object value = null;
            RIPARCCPNPEntity pnpEntity = (RIPARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            if (m_englishLanguageLearnerPrograms.containsKey(student.getOid())) {
                StudentProgramParticipation program = m_englishLanguageLearnerPrograms.get(student.getOid());

                /*
                 * Having had loaded the ell map with only ELL programs
                 */
                if (CALC_PARAM_HAS_ELL_CODE.equals(param)) {
                    value = CODE_YES;
                }

                if (CALC_PARAM_LEP_BEGIN_DATE.equals(param)) {
                    value = program.getStartDate().toString();
                }

                if (CALC_PARAM_LEP_END_DATE.equals(param)) {
                    value = program.getEndDate().toString();
                }
            }

            return value;
        }
    }

    /**
     * The Class RetrieveFreeReducedLunch.
     */
    /*
     * Retrieve Free, Reduced, Full Price Lunch
     */
    protected class RetrieveFreeReducedLunch implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = "";
            String lunchStatus = "";
            RIPARCCPNPEntity pnpEntity = (RIPARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            lunchStatus = (String) student.getFieldValueByAlias(ALIAS_LUNCH_STATUS);

            if (CODE_LUNCH_STATUS_F.equalsIgnoreCase(lunchStatus)
                    || CODE_LUNCH_STATUS_FREE.equalsIgnoreCase(lunchStatus)
                    || CODE_LUNCH_STATUS_R.equalsIgnoreCase(lunchStatus)
                    || CODE_LUNCH_STATUS_REDUCED.equalsIgnoreCase(lunchStatus)) {
                value = CODE_YES;
            }

            return value;
        }
    }

    /**
     * Retriever for gifted and talented.
     *
     */
    protected class RetrieveGifted implements FieldRetriever {

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
            Object value = NOT_GIFTED_OR_TALENTED;

            RIPARCCPNPEntity pnpEntity = (RIPARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            if (m_giftedPrograms.containsKey(student.getOid())) {
                StudentProgramParticipation programParticipation = m_giftedPrograms.get(student.getOid());
                String programCode = programParticipation.getProgramCode();

                if (!StringUtils.isEmpty(programCode)) {
                    String giftedAndTalentedProgramCode = (String) field.getParameter();

                    if (programCode.equalsIgnoreCase(giftedAndTalentedProgramCode)) {
                        if (m_programCode.containsKey(giftedAndTalentedProgramCode)) {
                            ReferenceCode giftedAndTalentedRefCode = m_programCode.get(giftedAndTalentedProgramCode);
                            value = giftedAndTalentedRefCode.getStateCode();
                        }
                    }
                }
            }

            return value;
        }
    }

    /**
     * Retriever for gifted and talented.
     *
     */
    protected class RetrieveLocation implements FieldRetriever {

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
            RIPARCCPNPEntity pnpEntity = (RIPARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            Object value = null;
            String param = (String) field.getParameter();

            if (CALC_PARAM_STATE_ID.equals(param)) {
                value = STATE_CODE_RI;
            } else if (CALC_PARAM_DISTRICT.equalsIgnoreCase(param)) {
                value = student.getOrganization1().getFieldValueByBeanPath(m_orgDistrictCodeField);
            } else if (CALC_PARAM_SCHOOL.equalsIgnoreCase(param)) {
                value = student.getSchool().getFieldValueByBeanPath(m_schoolCodeField);
            }

            return value;
        }
    }

    /**
     * Class RetrievePrimaryDisability is the FieldRetriever used to retrieve
     * the primary disability state code for the current student.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePrimaryDisability implements FieldRetriever {

        /**
         * Returns the corresponding alias value.
         *
         * This retriever handles both disability fields but only the HAS disability has a param
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
            RIPARCCPNPEntity pnpEntity = (RIPARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            Object value = null;
            IepDisability primaryDisability = null;
            String disabilityStateCode = null;

            IepData iepData = student.getActiveIep(getBroker());
            if (iepData == null) {
                iepData = student.getPreviousIep(getBroker());
            }
            if (iepData != null) {
                String status = student.getSpedStatusCode();
                if ((status != null) && (status.compareToIgnoreCase(CODE_SPED_STATUS_ACTIVE) == 0)) {
                    primaryDisability = iepData.getPrimaryDisability(getBroker());
                    if (primaryDisability != null) {
                        disabilityStateCode = lookupReferenceCodeByBeanPath(IepDisability.class,
                                IepDisability.COL_DISABILITY_CODE,
                                primaryDisability.getDisabilityCode(),
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                        value = m_disabilityCodes.get(disabilityStateCode);
                    }
                }
            }

            return value;
        }
    }

    /**
     * The Class RetrieveStudentAccomodations.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStudentAccomodations implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            RIPARCCPNPEntity pnpEntity = (RIPARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            String result = null;
            Object value = null;
            String param = (String) field.getParameter();

            if (CALC_PARAM_PARCC_504.equals(param)) {
                result = student.getSection504StatusCode();
                if ((result != null) && (result.compareToIgnoreCase(CODE_SECTION_504_STATUS_ACTIVE) == 0)) {
                    value = CODE_YES;
                }
            } else if (CALC_PARAM_PARCC_ELL.equals(param)) {
                result = (String) student.getFieldValueByAlias(ALIAS_ESL_FLAG);
                if ((result != null) && (result.compareToIgnoreCase("0") != 0)) {
                    value = CODE_YES;
                }
            } else if (CALC_PARAM_PARCC_IEP.equals(param)) {
                result = (String) student.getFieldValueByAlias(ALIAS_SPED_FLAG);
                if ((result != null) && (result.compareToIgnoreCase("0") != 0)) {
                    value = CODE_YES;
                }
            }

            return value;
        }
    }

    /**
     * The Class RetrieveSPED.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSPED implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            RIPARCCPNPEntity pnpEntity = (RIPARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            Object value = null;

            if (CALC_PARAM_SPED_STATUS.equals(param)) {
                String status = student.getSpedStatusCode();
                if ((status != null) && (status.compareToIgnoreCase(CODE_SPED_STATUS_ACTIVE) == 0)) {
                    value = CODE_YES;
                }
            } else if (CALC_PARAM_SPED_SERVICE.equals(param)) {
                value = student.getFieldValueByAlias(ALIAS_SPED_SERVICE);
            } else if (CALC_PARAM_SPED_END_DATE.equals(param)) {
                value = student.getFieldValueByAlias(ALIAS_SPED_END_DATE);
            }

            return value;
        }
    }

    /**
     * Returns the school code for the given student.
     * <p>
     * Retrieve the student title 1 and TAS indicators.
     * The school Title 1 school flag is used for Title 1.
     * The school Title 1 and the student
     * <p>
     * WICOMICO SPECIFIC.
     */
    protected class RetrieveTitle1 implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            RIPARCCPNPEntity pnpEntity = (RIPARCCPNPEntity) entity;
            Object value = null;

            String param = (String) field.getParameter();
            value = Boolean.FALSE;
            SisSchool school = pnpEntity.getStudent().getSchool();

            if (school != null) {
                value = CODE_NO;
                if (CALC_PARAM_TITLE1.equals(param)) {
                    value = CODE_YES;
                }
            }

            return value;
        }
    }

    /*
     * Input Definition Parameters
     */
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_QUERY_BY_PARAM = "queryBy";
    protected static final String PARAM_QUERY_STRING_PARAM = "queryString";
    protected static final String PARAM_SORT_PARAM = "sort";

    /*
     * Retriever Parameters
     */
    protected static final String CALC_ID_PNP_ELL = "PNP-ELL";
    protected static final String CALC_ID_PNP_FARM = "PNP-FARM";
    protected static final String CALC_ID_PNP_DISABILITY = "PNP-DISABILITY";
    protected static final String CALC_ID_PNP_GT = "PNP-GT";
    protected static final String CALC_ID_PNP_SPED = "PNP-SPED";
    protected static final String CALC_ID_PNP_T1 = "PNP-T1";
    protected static final String CALC_ID_PNP_RACE = "PNP-RACE";
    protected static final String CALC_ID_PNP_LOC = "PNP-LOC";
    protected static final String CALC_ID_PNP_TEST = "PNP-TEST";
    protected static final String CALC_ID_PNP_STDACC = "PNP-STDACC";

    protected static final String CALC_PARAM_STATE_ID = "STATE_ID";
    protected static final String CALC_PARAM_HISPANIC_LATINO = "HISPANIC_LATINO";
    protected static final String CALC_PARAM_INDIAN_ALASKIAN_NATIVE = "INDIAN_ALASKIAN_NATIVE";
    protected static final String CALC_PARAM_ASIAN = "ASIAN";
    protected static final String CALC_PARAM_BLACK_AFRICAN_AMERICAN = "BLACK_AFRICAN_AMERICAN";
    protected static final String CALC_PARAM_HAWAIIAN_PACIFIC_ISLANDER = "HAWAIIAN_PACIFIC_ISLANDER";
    protected static final String CALC_PARAM_WHITE = "WHITE";
    protected static final String CALC_PARAM_HAS_ELL_CODE = "HAS_ELL_CODE";
    protected static final String CALC_PARAM_LEP_BEGIN_DATE = "LEP_BEGIN_DATE";
    protected static final String CALC_PARAM_LEP_END_DATE = "LEP_END_DATE";
    protected static final String CALC_PARAM_SPED_STATUS = "SPED_STATUS";
    protected static final String CALC_PARAM_SPED_SERVICE = "SPED_SERVICE";
    protected static final String CALC_PARAM_SPED_END_DATE = "SPED_END_DATE";
    protected static final String CALC_PARAM_TITLE1 = "T1";
    protected static final String CALC_PARAM_DISTRICT = "DISTRICT";
    protected static final String CALC_PARAM_SCHOOL = "SCHOOL";
    protected static final String CALC_PARAM_PARCC_TEST_CODE = "PARCC_TEST_CODE";
    protected static final String CALC_PARAM_TEST_FORMAT = "PARCC_TEST_FORMAT";
    protected static final String CALC_PARAM_TEST_RETEST = "PARCC_TEST_RETEST";
    protected static final String CALC_PARAM_PARCC_504 = "PARCC_504";
    protected static final String CALC_PARAM_PARCC_ELL = "PARCC_ELL";
    protected static final String CALC_PARAM_PARCC_IEP = "PARCC_IEP";
    protected static final String CALC_PARAM_ALTR_PAPER_TEST = "ALTR_PAPER_TEST";
    protected static final String CALC_PARAM_MATH_ASSESS_IN_PAPER = "MATH_ASSESS_IN_PAPER";
    protected static final String CALC_PARAM_HUMAN_READ_HUMAN_SIG = "HUMAN_READ_HUMAN_SIG";
    protected static final String CALC_PARAM_LARGE_PRINT = "LARGE_PRINT";
    protected static final String CALC_PARAM_BRAILLE = "BRAILLE";
    protected static final String CALC_PARAM_FREQUENT_BREAKS = "FREQUENT_BREAKS";
    protected static final String CALC_PARAM_SEP_ALT_LOC = "SEP_ALT_LOC";
    protected static final String CALC_PARAM_SMALL_TEST_GROUP = "SMALL_TEST_GROUP";
    protected static final String CALC_PARAM_SPEC_EQUIP_FURN = "SPEC_EQUIP_FURN";
    protected static final String CALC_PARAM_SPEC_AREA_SETTING = "SPEC_AREA_SETTING";
    protected static final String CALC_PARAM_TIME_OF_DAY = "TIME_OF_DAY";
    protected static final String CALC_PARAM_AF_ANS_MASKING = "AF_ANS_MASKING";
    protected static final String CALC_PARAM_AF_COLOR_CONTRAST = "AF_COLOR_CONTRAST";
    protected static final String CALC_PARAM_AF_TEST_SPEECH_MATH = "AF_TEST_SPEECH_MATH";
    protected static final String CALC_PARAM_AF_HR_HS_MATH = "AF_HR_HS_MATH";
    protected static final String CALC_PARAM_PA_ASL_VIDEO = "PA_ASL_VIDEO";
    protected static final String CALC_PARAM_PA_AT_SRN_RDR = "PA_AT_SRN_RDR";
    protected static final String CALC_PARAM_PA_LSD_CAP_ELA = "PA_LSD_CAP_ELA";
    protected static final String CALC_PARAM_PA_HR_HS_ELA = "PA_HR_HS_ELA";
    protected static final String CALC_PARAM_PA_REF_BRAL_ELA = "PA_REF_BRAL_ELA";
    protected static final String CALC_PARAM_PA_TAC_GRAPH = "PA_TAC_GRAPH";
    protected static final String CALC_PARAM_PA_TXT_TO_SPCH = "PA_TXT_TO_SPCH";
    protected static final String CALC_PARAM_RA_ANS_REC_TST_BK = "RA_ANS_REC_TST_BK";
    protected static final String CALC_PARAM_RA_BRAL_RESP = "RA_BRAL_RESP";
    protected static final String CALC_PARAM_RA_CALC_DEV_MATH = "RA_CALC_DEV_MATH";
    protected static final String CALC_PARAM_RA_CNST_RESP_ELA = "RA_CNST_RESP_ELA";
    protected static final String CALC_PARAM_RA_MATH_RESP = "RA_MATH_RESP";
    protected static final String CALC_PARAM_RA_MON_TEST_RESP = "RA_MON_TEST_RESP";
    protected static final String CALC_PARAM_RA_SEL_RESP_TCH_ENH = "RA_SEL_RESP_TCH_ENH";
    protected static final String CALC_PARAM_RA_WORD_PRED = "RA_WORD_PRED";
    protected static final String CALC_PARAM_AEL_DIRC_STD_NAV_LANG = "AEL_DIRC_STD_NAV_LANG";
    protected static final String CALC_PARAM_AEL_DIRC_READ_ALOUD = "AEL_DIRC_READ_ALOUD";
    protected static final String CALC_PARAM_AEL_MATH_RESP_EL = "AEL_MATH_RESP_EL";
    protected static final String CALC_PARAM_AEL_TRNS_MATH_TXT_SPCH = "AEL_TRNS_MATH_TXT_SPCH";
    protected static final String CALC_PARAM_AEL_TRANS_MATH_ONLINE = "AEL_TRANS_MATH_ONLINE";
    protected static final String CALC_PARAM_AEL_WRD_TO_WRD_DICT = "AEL_WRD_TO_WRD_DICT";
    protected static final String CALC_PARAM_TSA_EXTND_TIME = "TSA_Extnd_TIME";
    protected static final String CALC_PARAM_SATID = "SATID";
    protected static final String CALC_PARAM_LATID = "LATID";

    /*
     * Aliases
     */
    // ORGANIZATION Table
    protected static final String ALIAS_DISTRICT_CODE = "RI Reporting District Code";

    // SCHOOL Table
    protected static final String ALIAS_SCHOOL_CODE = "State School Id";

    // STUDENT Table
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_LUNCH_STATUS = "Lunch Description";
    protected static final String ALIAS_ESL_FLAG = "ESL Flag";
    protected static final String ALIAS_SPED_FLAG = "SPED Flag";
    protected static final String ALIAS_SPED_SERVICE = "DOE SPECIAL ED";
    protected static final String ALIAS_SPED_END_DATE = "DOE SPED END";

    // STUDENT_SCHEDULE Table
    protected static final String ALIAS_SUBJECT = "RI Course ID";

    // STUDENT_ASSESSMENT Table (PARCC)
    protected static final String ALIAS_PARCC_RETEST = "DOE PARCC_RETEST";
    protected static final String ALIAS_PARCC_ALTREP_PAPERTST = "DOE ALTREP PAPERTST";
    protected static final String ALIAS_PARCC_TSTCODE = "DOE PARCC_TSTCODE";
    protected static final String ALIAS_PARCC_BRAILLE = "DOE PARCC_Braille";
    protected static final String ALIAS_PARCC_LARGEPRT = "DOE PARCC_LARGEPRT";
    protected static final String ALIAS_PARCC_TRANSMATHPAPER = "DOE PARCC_TRANSMATHPAPER";
    protected static final String ALIAS_PARCC_TESTFORMAT = "DOE PARCC_TSTFORMAT";
    protected static final String ALIAS_PARCC_HUMAN = "DOE PARCC_Human";

    // STUDENT_ASSESSMENT Table (PARCC PNP)
    protected static final String ALIAS_PARCC_AC_FREQBREAKS = "DOE PARCC_AC_FreqBreaks";
    protected static final String ALIAS_PARCC_AC_SEPALTLOC = "DOE PARCC_AC_SEPALTLOC";
    protected static final String ALIAS_PARCC_AC_SMTSTGRP = "DOE PARCC_AC_SMTSTGRP";
    protected static final String ALIAS_PARCC_AC_SP_EQUIP_FURN = "DOE PARCC_AC_SP_EQUIP_FURN";
    protected static final String ALIAS_PARCC_AC_SPEC_AREA = "DOE PARCC_AC_SPEC_AREA";
    protected static final String ALIAS_PARCC_AC_TOD = "DOE PARCC_AC_TOD";
    protected static final String ALIAS_PARCC_AF_ANSMASK = "DOE PARCC_AF_ANSMASK";
    protected static final String ALIAS_PARCC_AF_COLORCONTRST = "DOE PARCC_AF_COLORCONTRST";
    protected static final String ALIAS_PARCC_AF_TTS_MATH = "DOE PARCC_AF_TTS_MATH";
    protected static final String ALIAS_PARCC_AC_HRHS_MATH = "DOE PARCC_AC_HRHS_MATH";
    protected static final String ALIAS_PARCC_AC_ASLVIDEO = "DOE PARCC_AC_ASLVIDEO";
    protected static final String ALIAS_PARCC_AC_ATSCREENRDR = "DOE PARCC_AC_ATSCREENRDR";
    protected static final String ALIAS_PARCC_AC_CCELA = "DOE PARCC_AC_CCELA";
    protected static final String ALIAS_PARCC_AC_HRHS_ELA = "DOE PARCC_AC_HRHS_ELA";
    protected static final String ALIAS_PARCC_AC_RBELA = "DOE PARCC_AC_RBELA";
    protected static final String ALIAS_PARCC_AC_TG = "DOE PARCC_AC_TG";
    protected static final String ALIAS_PARCC_PA_TTS_ELA = "DOE PARCC PA_TTS_ELA";
    protected static final String ALIAS_PARCC_RA_ARTB = "DOE PARCC_RA_ARTB";
    protected static final String ALIAS_PARCC_RA_BR = "DOE PARCC_RA_BR";
    protected static final String ALIAS_PARCC_RA_CDMT = "DOE PARCC_RA_CDMT";
    protected static final String ALIAS_PARCC_RA_ELA_CR = "DOE PARCC_RA_ELA_CR";
    protected static final String ALIAS_PARCC_RA_MR = "DOE PARCC_RA_MR";
    protected static final String ALIAS_PARCC_RA_MTR = "DOE PARCC_RA_MTR";
    protected static final String ALIAS_PARCC_RA_SRTE = "DOE PARCC_RA_SRTE";
    protected static final String ALIAS_PARCC_RA_WP = "DOE PARCC_RA_WP";
    protected static final String ALIAS_PARCC_AEL_DSNL = "DOE PARCC_AEL_DSNL";
    protected static final String ALIAS_PARCC_AEL_DRA = "DOE PARCC_AEL_DRA";
    protected static final String ALIAS_PARCC_AEL_MREL = "DOE PARCC_AEL_MREL";
    protected static final String ALIAS_PARCC_AEL_TMATS = "DOE PARCC_AEL_TMATS";
    protected static final String ALIAS_PARCC_AEL_TMAOL = "DOE PARCC_AEL_TMAOL";
    protected static final String ALIAS_PARCC_AEL_WWDICT = "DOE PARCC_AEL_WWDict";
    protected static final String ALIAS_PARCC_TSA_ET = "DOE PARCC_TSA_ET";

    /**
     * Other internal constants
     */
    protected static final String STATE_CODE_RI = "RI";

    protected static final String NOT_GIFTED_OR_TALENTED = "N";

    protected static final String CODE_GIFTED_AND_TALENTED_PROGRAM = "ALP";
    protected static final String CODE_ENGLISH_LANGUAGE_LEARNER_PROGRAM = "ELL%";

    protected static final String CODE_ASSESSMENT_DEFINITION_ID_RI_PNP = "PARCC";
    protected static final String ERROR_TYPE_WARNING = "Warning";
    protected static final String ERROR_MESSAGE_ASSESS_DEF_ALIAS = " Assessment Definition Alias: ";
    protected static final String ERROR_MESSAGE_IS_NOT_DEFINED = " is not defined";
    protected static final String ERROR_MESSAGE_STATE_CODE_NOT_DEFINED =
            " System State Code (sys.state) is not set in System Preferences";
    protected static final String ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED =
            " Assessment Definition is not loaded in the Data Dictionary.";
    protected static final String ERROR_MESSAGE_NO_ACTIVE_STUDENTS =
            "No students were active in the previous School Year.";
    protected static final String ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS =
            "No RI PARCC PNP Student Assessment's were created by the selected students.";

    protected static final String CODE_STATE_PARCC_PNP = "PARCC PNP";
    protected static final String CODE_YES = "Y";
    protected static final String CODE_NO = "N";
    protected static final String CODE_ONE = "1";

    protected static final String CODE_SECTION_504_STATUS_ACTIVE = "Active";
    protected static final String CODE_SPED_STATUS_ACTIVE = "Active";
    protected static final String CODE_LUNCH_STATUS_F = "F";
    protected static final String CODE_LUNCH_STATUS_FREE = "Free";
    protected static final String CODE_LUNCH_STATUS_R = "R";
    protected static final String CODE_LUNCH_STATUS_REDUCED = "Reduced";

    protected static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Boolean m_removeHeader;
    protected PlainDate m_endDate;
    protected PlainDate m_firstDayDate;
    protected PlainDate m_reportDate;
    protected PlainDate m_startDate;
    protected StudentHistoryHelper m_helper;

    protected Map<String, ReferenceCode> m_programCode;
    protected Map<String, String> m_disabilityCodes;
    protected Map<String, StudentProgramParticipation> m_englishLanguageLearnerPrograms;
    protected Map<String, StudentProgramParticipation> m_giftedPrograms;

    protected String m_orgDistrictCodeField;
    protected String m_programESlCodeColumnName;
    protected String m_schoolCodeField;
    protected String m_studentExcludeField;

    protected String m_assessmentAltRepPaperTestField;
    protected String m_assessmentBrailleField;
    protected String m_assessmentFormatField;
    protected String m_assessmentHumanField;
    protected String m_assessmentLargePrintField;
    protected String m_assessmentRetestField;
    protected String m_assessmentTestCodeField;
    protected String m_assessmentTestFormatField;
    protected String m_assessmentTransMathPaperField;

    protected String m_assessmentACFreqBreaksField;
    protected String m_assessmentACSepAltLocField;
    protected String m_assessmentACSmtsGrpField;
    protected String m_assessmentACSPEquipFurnField;
    protected String m_assessmentACSpecAreaField;
    protected String m_assessmentACTodField;
    protected String m_assessmentACAnsMaskField;
    protected String m_assessmentAFColorContrstField;
    protected String m_assessmentAFTTSMatFieldh;
    protected String m_assessmentACHrHsMathField;
    protected String m_assessmentACAslVideoField;
    protected String m_assessmentACScreenRdrField;
    protected String m_assessmentACCCELAField;
    protected String m_assessmentACHRHSELAField;
    protected String m_assessmentACRBELAField;
    protected String m_assessmentACTGField;
    protected String m_assessmentPATTSELAField;
    protected String m_assessmentRAArtBField;
    protected String m_assessmentRABRField;
    protected String m_assessmentRACDMTField;
    protected String m_assessmentRAELACRField;
    protected String m_assessmentRAMRField;
    protected String m_assessmentRAMTRField;
    protected String m_assessmentRASRTEField;
    protected String m_assessmentRAWPField;
    protected String m_assessmentAELDSNLField;
    protected String m_assessmentAELDRAField;
    protected String m_assessmentAELMRELField;
    protected String m_assessmentAELTMATSField;
    protected String m_assessmentAELTMAOLField;
    protected String m_assessmentAELWWDictField;
    protected String m_assessmentTCAETField;


    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (m_removeHeader == null || m_removeHeader.booleanValue()) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    /*
     * The outer class must contain an initialize method if not the exports java code will not get
     * run,
     * even thought the report still does, proper spelling is also required
     */
    @Override
    public void initialize() {
        initializeFields();

        loadRIPNPAssessmentDefinition();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        X2Criteria studentCritiera = m_helper.getStudentCriteria();
        SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCritiera);

        // Check Student count
        QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCritiera);
        int studentCount = getBroker().getCount(studentQuery);
        if (studentCount == 0) {
            addSetupError(ERROR_TYPE_WARNING, ERROR_MESSAGE_NO_ACTIVE_STUDENTS);
        }

        X2Criteria studentAssessmentCriteria = getStudentAssessmentCriteria(studentSubQuery);
        QueryByCriteria studentAssessmentQuery =
                new QueryByCriteria(StudentAssessment.class, studentAssessmentCriteria);

        int studentAssessmentCount = getBroker().getCount(studentAssessmentQuery);
        if (studentAssessmentCount == 0) {
            addSetupError(ERROR_TYPE_WARNING, ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS);
        }

        loadProgramCodes();

        loadStudentEnglishLanguagePrograms(studentSubQuery);
        loadStudentGiftedPrograms(studentSubQuery);


        if (getSetupErrors().size() == 0) {
            // Assign the custom entity class, if there is one.
            setQuery(studentAssessmentQuery);
            setEntityClass(RIPARCCPNPEntity.class);

            // Assign custom field retriever calculations.
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_PNP_DISABILITY, new RetrievePrimaryDisability());
            calcs.put(CALC_ID_PNP_ELL, new RetrieveEnglishLanguageLearner());
            calcs.put(CALC_ID_PNP_FARM, new RetrieveFreeReducedLunch());
            calcs.put(CALC_ID_PNP_GT, new RetrieveGifted());
            calcs.put(CALC_ID_PNP_LOC, new RetrieveLocation());
            calcs.put(CALC_ID_PNP_SPED, new RetrieveSPED());
            calcs.put(CALC_ID_PNP_STDACC, new RetrieveStudentAccomodations());
            calcs.put(CALC_ID_PNP_T1, new RetrieveTitle1());
            calcs.put(CALC_ID_PNP_TEST, new RetrieveAssessment());
            super.addCalcs(calcs);
        }

    } // end of PARCCPNP init

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria X2Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(X2Criteria criteria, String recordSetName) {
        X2Criteria recordSetCriteria = new X2Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER
                + RecordSet.COL_NAME, recordSetName);

        criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Get Student Assessment Criteria using the selected Student Oids.
     *
     * @param studentSubQuery SubQuery
     * @return X 2 criteria
     */
    private X2Criteria getStudentAssessmentCriteria(SubQuery studentSubQuery) {
        X2Criteria studentAssessmentCriteria = new X2Criteria();

        studentAssessmentCriteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);

        studentAssessmentCriteria
                .addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                        AssessmentDefinition.COL_ID, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP);

        String queryString = (String) getParameter(PARAM_QUERY_STRING_PARAM);
        String queryBy = ((String) getParameter(PARAM_QUERY_BY_PARAM));
        switch (queryBy) {
            case "yog": // YOG
                studentAssessmentCriteria.addEqualTo(
                        StudentAssessment.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_YOG, queryString);
                break;

            case "localId": // LASID
                studentAssessmentCriteria.addEqualTo(
                        StudentAssessment.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_LOCAL_ID,
                        queryString);
                break;

            case "stateId": // SASID
                studentAssessmentCriteria.addEqualTo(
                        StudentAssessment.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        queryString);
                break;

            case "##snapshot": // Snapshot
                addRecordSetCriteria(studentAssessmentCriteria, queryString);
                break;

            default:
                // Take all students
                break;
        }

        return studentAssessmentCriteria;
    }

    /**
     * initialize Fields.
     */
    private void initializeFields() {
        // System Parameters
        m_startDate = getOrganization().getCurrentContext().getStartDate();
        m_endDate = getOrganization().getCurrentContext().getEndDate();

        // Load Input Definition Parameters
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        m_removeHeader = (Boolean) getParameter(PARAM_REMOVE_HEADER);
        if (m_removeHeader == null) {
            m_removeHeader = Boolean.valueOf(false);
        }

        /*
         * PARCCPNP requires a specific representation of the student disability
         * compare the IEP Disability Code to the Aspen Disability reference code table
         *
         * Need to check that the State has setup a reference table for Disability codes
         */
        m_disabilityCodes = new HashMap<String, String>();
        m_disabilityCodes.put("K", "MD");
        m_disabilityCodes.put("G", "HI");
        m_disabilityCodes.put("I", "SLI");
        m_disabilityCodes.put("J", "VI");
        m_disabilityCodes.put("A", "EMN");
        m_disabilityCodes.put("C", "OI");
        m_disabilityCodes.put("D", "OHI");
        m_disabilityCodes.put("E", "SLD");
        m_disabilityCodes.put("F", "DB");
        m_disabilityCodes.put("N", "TBI");
        m_disabilityCodes.put("M", "AUT");
        m_disabilityCodes.put("L", "DD");

        // Load Alias database UDF names
        m_orgDistrictCodeField = translateAliasToJavaName(ALIAS_DISTRICT_CODE, true);
        // m_programESlCodeColumnName = translateAliasToColumnName(ALIAS_ELL_PROGRAM_CODE, false);
        m_schoolCodeField = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
        m_studentExcludeField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
    }

    /**
     * Load Program Codes.
     */
    private void loadProgramCodes() {
        /*
         * This gets the ELL code from the reference table there should be one, if not there's no
         * ELL program
         */
        m_programCode = new HashMap<String, ReferenceCode>();
        DataDictionaryField programCodeField = DataDictionary.getDistrictDictionary(getBroker()
                .getPersistenceKey()).findDataDictionaryField(StudentProgramParticipation.class.getName(),
                        StudentProgramParticipation.COL_PROGRAM_CODE);
        if (programCodeField != null && programCodeField.getReferenceTableOid() != null) {
            X2Criteria programCriteria = new X2Criteria();
            programCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, programCodeField.getReferenceTableOid());

            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, programCriteria);

            m_programCode = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 4);
        }
    }

    /**
     * Load PNP Assessment Definition Alias field names.
     */
    private void loadRIPNPAssessmentDefinition() {
        X2Criteria assessmentDefinitonCriteria = new X2Criteria();
        assessmentDefinitonCriteria.addEqualTo(AssessmentDefinition.COL_ID, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP);

        QueryByCriteria assessmentDefinitonQuery =
                new QueryByCriteria(AssessmentDefinition.class, assessmentDefinitonCriteria);

        AssessmentDefinition rIPnpDefinition =
                (AssessmentDefinition) getBroker().getBeanByQuery(assessmentDefinitonQuery);

        // Load PNP database field names by the PNP Assessment Definition
        if (rIPnpDefinition == null) {
            addSetupError(ERROR_TYPE_WARNING,
                    CODE_ASSESSMENT_DEFINITION_ID_RI_PNP + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
        } else {
            DataDictionary dataDictionary =
                    DataDictionary.getDistrictDictionary(rIPnpDefinition, getBroker().getPersistenceKey());

            if (dataDictionary == null) {
                addSetupError(ERROR_TYPE_WARNING,
                        CODE_ASSESSMENT_DEFINITION_ID_RI_PNP + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
            } else {
                DataDictionaryField testFormatField =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TESTFORMAT);
                if (testFormatField != null && dataDictionary.containsAlias(ALIAS_PARCC_TESTFORMAT)) {
                    m_assessmentFormatField = testFormatField.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_TESTFORMAT + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField retestField = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RETEST);
                if (retestField != null && dataDictionary.containsAlias(ALIAS_PARCC_RETEST)) {
                    m_assessmentRetestField = retestField.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_RETEST + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField altrepPaperTstName =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_ALTREP_PAPERTST);
                if (altrepPaperTstName != null && dataDictionary.containsAlias(ALIAS_PARCC_ALTREP_PAPERTST)) {
                    m_assessmentAltRepPaperTestField = altrepPaperTstName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING,
                            CODE_ASSESSMENT_DEFINITION_ID_RI_PNP + ERROR_MESSAGE_ASSESS_DEF_ALIAS
                                    + ALIAS_PARCC_ALTREP_PAPERTST + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField tstCodeName = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TSTCODE);
                if (tstCodeName != null && dataDictionary.containsAlias(ALIAS_PARCC_TSTCODE)) {
                    m_assessmentTestCodeField = tstCodeName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_TSTCODE + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField brailleName = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_BRAILLE);
                if (brailleName != null && dataDictionary.containsAlias(ALIAS_PARCC_BRAILLE)) {
                    m_assessmentBrailleField = brailleName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_BRAILLE + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField largePrtName = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_LARGEPRT);
                if (largePrtName != null && dataDictionary.containsAlias(ALIAS_PARCC_LARGEPRT)) {
                    m_assessmentLargePrintField = largePrtName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_LARGEPRT + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField transMathPaperName =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TRANSMATHPAPER);
                if (transMathPaperName != null && dataDictionary.containsAlias(ALIAS_PARCC_TRANSMATHPAPER)) {
                    m_assessmentTransMathPaperField = transMathPaperName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING,
                            CODE_ASSESSMENT_DEFINITION_ID_RI_PNP + ERROR_MESSAGE_ASSESS_DEF_ALIAS
                                    + ALIAS_PARCC_TRANSMATHPAPER + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField tstFormatName =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TESTFORMAT);
                if (tstFormatName != null && dataDictionary.containsAlias(ALIAS_PARCC_TESTFORMAT)) {
                    m_assessmentTestFormatField = tstFormatName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_TESTFORMAT + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField humanName = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_HUMAN);
                if (tstFormatName != null && dataDictionary.containsAlias(ALIAS_PARCC_HUMAN)) {
                    m_assessmentHumanField = humanName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_HUMAN + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField freqBreaks =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_FREQBREAKS);
                if (freqBreaks != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_FREQBREAKS)) {
                    m_assessmentACFreqBreaksField = freqBreaks.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING,
                            CODE_ASSESSMENT_DEFINITION_ID_RI_PNP + ERROR_MESSAGE_ASSESS_DEF_ALIAS
                                    + ALIAS_PARCC_AC_FREQBREAKS + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField sepAltLoc = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_SEPALTLOC);
                if (sepAltLoc != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_SEPALTLOC)) {
                    m_assessmentACSepAltLocField = sepAltLoc.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AC_SEPALTLOC + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField smtstGrp = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_SMTSTGRP);
                if (sepAltLoc != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_SMTSTGRP)) {
                    m_assessmentACSmtsGrpField = smtstGrp.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AC_SMTSTGRP + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField spEquipFurn =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_SP_EQUIP_FURN);
                if (sepAltLoc != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_SP_EQUIP_FURN)) {
                    m_assessmentACSPEquipFurnField = spEquipFurn.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING,
                            CODE_ASSESSMENT_DEFINITION_ID_RI_PNP + ERROR_MESSAGE_ASSESS_DEF_ALIAS
                                    + ALIAS_PARCC_AC_SP_EQUIP_FURN + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField specArea = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_SPEC_AREA);
                if (sepAltLoc != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_SPEC_AREA)) {
                    m_assessmentACSpecAreaField = specArea.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AC_SPEC_AREA + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField tod = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_TOD);
                if (tod != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_TOD)) {
                    m_assessmentACTodField = tod.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AC_TOD + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField ansMask = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AF_ANSMASK);
                if (tod != null && dataDictionary.containsAlias(ALIAS_PARCC_AF_ANSMASK)) {
                    m_assessmentACAnsMaskField = ansMask.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AF_ANSMASK + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField colorContrast =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AF_COLORCONTRST);
                if (colorContrast != null && dataDictionary.containsAlias(ALIAS_PARCC_AF_COLORCONTRST)) {
                    m_assessmentAFColorContrstField = colorContrast.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING,
                            CODE_ASSESSMENT_DEFINITION_ID_RI_PNP + ERROR_MESSAGE_ASSESS_DEF_ALIAS
                                    + ALIAS_PARCC_AF_COLORCONTRST + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField ttsMath = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AF_TTS_MATH);
                if (ttsMath != null && dataDictionary.containsAlias(ALIAS_PARCC_AF_TTS_MATH)) {
                    m_assessmentAFTTSMatFieldh = ttsMath.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AF_TTS_MATH + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField hrHsMath = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_HRHS_MATH);
                if (hrHsMath != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_HRHS_MATH)) {
                    m_assessmentACHrHsMathField = hrHsMath.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AC_HRHS_MATH + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField aslVideo = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_ASLVIDEO);
                if (aslVideo != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_ASLVIDEO)) {
                    m_assessmentACAslVideoField = aslVideo.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AC_ASLVIDEO + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField screenRdr =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_ATSCREENRDR);
                if (screenRdr != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_ATSCREENRDR)) {
                    m_assessmentACScreenRdrField = screenRdr.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING,
                            CODE_ASSESSMENT_DEFINITION_ID_RI_PNP + ERROR_MESSAGE_ASSESS_DEF_ALIAS
                                    + ALIAS_PARCC_AC_ATSCREENRDR + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField ccEla = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_CCELA);
                if (ccEla != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_CCELA)) {
                    m_assessmentACCCELAField = ccEla.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AC_CCELA + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField hrHsEla = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_HRHS_ELA);
                if (hrHsEla != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_HRHS_ELA)) {
                    m_assessmentACHRHSELAField = hrHsEla.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AC_HRHS_ELA + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField rbEla = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_RBELA);
                if (rbEla != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_RBELA)) {
                    m_assessmentACRBELAField = rbEla.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AC_RBELA + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField acTg = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_TG);
                if (acTg != null && dataDictionary.containsAlias(ALIAS_PARCC_AC_TG)) {
                    m_assessmentACTGField = acTg.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AC_TG + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField ttsEla = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_PA_TTS_ELA);
                if (ttsEla != null && dataDictionary.containsAlias(ALIAS_PARCC_PA_TTS_ELA)) {
                    m_assessmentPATTSELAField = ttsEla.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_PA_TTS_ELA + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField raArtb = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_ARTB);
                if (raArtb != null && dataDictionary.containsAlias(ALIAS_PARCC_RA_ARTB)) {
                    m_assessmentRAArtBField = raArtb.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_RA_ARTB + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField raBr = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_BR);
                if (raBr != null && dataDictionary.containsAlias(ALIAS_PARCC_RA_BR)) {
                    m_assessmentRABRField = raBr.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_RA_BR + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField cdmt = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_CDMT);
                if (cdmt != null && dataDictionary.containsAlias(ALIAS_PARCC_RA_CDMT)) {
                    m_assessmentRACDMTField = cdmt.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_RA_CDMT + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField elaCr = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_ELA_CR);
                if (elaCr != null && dataDictionary.containsAlias(ALIAS_PARCC_RA_ELA_CR)) {
                    m_assessmentRAELACRField = elaCr.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_RA_ELA_CR + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField raMr = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_MR);
                if (raMr != null && dataDictionary.containsAlias(ALIAS_PARCC_RA_ELA_CR)) {
                    m_assessmentRAMRField = raMr.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_RA_ELA_CR + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField raMtr = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_MTR);
                if (raMtr != null && dataDictionary.containsAlias(ALIAS_PARCC_RA_MTR)) {
                    m_assessmentRAMTRField = raMtr.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_RA_MTR + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField raSrTe = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_SRTE);
                if (raSrTe != null && dataDictionary.containsAlias(ALIAS_PARCC_RA_SRTE)) {
                    m_assessmentRASRTEField = raSrTe.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_RA_SRTE + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField raWp = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_WP);
                if (raWp != null && dataDictionary.containsAlias(ALIAS_PARCC_RA_WP)) {
                    m_assessmentRAWPField = raWp.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_RA_WP + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField eldSnl = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AEL_DSNL);
                if (eldSnl != null && dataDictionary.containsAlias(ALIAS_PARCC_AEL_DSNL)) {
                    m_assessmentAELDSNLField = eldSnl.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AEL_DSNL + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField elDra = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AEL_DRA);
                if (elDra != null && dataDictionary.containsAlias(ALIAS_PARCC_AEL_DRA)) {
                    m_assessmentAELDRAField = elDra.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AEL_DRA + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField elMrel = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AEL_MREL);
                if (elMrel != null && dataDictionary.containsAlias(ALIAS_PARCC_AEL_MREL)) {
                    m_assessmentAELMRELField = elMrel.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AEL_MREL + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField eltMats = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AEL_TMATS);
                if (eltMats != null && dataDictionary.containsAlias(ALIAS_PARCC_AEL_TMATS)) {
                    m_assessmentAELTMATSField = eltMats.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AEL_TMATS + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField eltMaol = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AEL_TMAOL);
                if (eltMaol != null && dataDictionary.containsAlias(ALIAS_PARCC_AEL_TMAOL)) {
                    m_assessmentAELTMAOLField = eltMaol.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AEL_TMAOL + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField ellWwDict = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AEL_WWDICT);
                if (ellWwDict != null && dataDictionary.containsAlias(ALIAS_PARCC_AEL_WWDICT)) {
                    m_assessmentAELWWDictField = ellWwDict.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_AEL_WWDICT + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField tcAet = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TSA_ET);
                if (tcAet != null && dataDictionary.containsAlias(ALIAS_PARCC_TSA_ET)) {
                    m_assessmentTCAETField = tcAet.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_RI_PNP
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_TSA_ET + ERROR_MESSAGE_IS_NOT_DEFINED);
                }
            }
        }
    }

    /**
     * loads existing Student Program Participation records for the CATE program.
     *
     * @param studentSubQuery SubQuery
     */
    private void loadStudentEnglishLanguagePrograms(SubQuery studentSubQuery) {
        m_englishLanguageLearnerPrograms = new HashMap<String, StudentProgramParticipation>();

        if (!StringUtils.isEmpty(m_programESlCodeColumnName)) {
            X2Criteria studentProgramCriteria = new X2Criteria();

            studentProgramCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);

            studentProgramCriteria.addLike(StudentProgramParticipation.COL_PROGRAM_CODE,
                    CODE_ENGLISH_LANGUAGE_LEARNER_PROGRAM);
            QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, studentProgramCriteria);

            m_englishLanguageLearnerPrograms = getBroker().getMapByQuery(query,
                    StudentProgramParticipation.COL_STUDENT_OID, getBroker().getCount(query));
        }
    }

    /**
     * loads existing Student Gifted Program Participation records.
     *
     * @param studentSubQuery SubQuery
     */
    private void loadStudentGiftedPrograms(SubQuery studentSubQuery) {
        X2Criteria studentProgramcriteria = new X2Criteria();
        studentProgramcriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);

        studentProgramcriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE,
                CODE_GIFTED_AND_TALENTED_PROGRAM);
        studentProgramcriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_firstDayDate);
        studentProgramcriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, studentProgramcriteria);

        m_giftedPrograms = getBroker().getMapByQuery(query, StudentProgramParticipation.COL_STUDENT_OID,
                getBroker().getCount(query));
    }

}
