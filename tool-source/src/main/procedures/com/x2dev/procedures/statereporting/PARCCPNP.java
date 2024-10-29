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

package com.x2dev.procedures.statereporting;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * PARCCPNP - Personal Needs Profle export.
 *
 * @author X2 Development Corporation
 */
public class PARCCPNP extends StateReportData {
    static PARCCPNP m_data;
    Logger logger = AppGlobals.getLog();
    private String m_currentState;

    /**
     * Implementation of StateReportEntity to be used for PARCCPNP export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class PARCCPNPEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        /**
         * Courses the student has taken
         */
        ArrayList<ScheduleInfo> m_courses;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public PARCCPNPEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Retrieve the current ScheduleInfo this entity is on.
         *
         * @return current schedule info
         */
        public ScheduleInfo getCurrentSchedule() {
            return m_courses.get(getCurrentRow());
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
            m_data = (PARCCPNP) data;

            // SisStudent student = (SisStudent) bean;

            /*
             * Put student schedules into the map first, THEN transcripts second
             * This is because transcript records should override the student schedule records
             */

            // Collection<StudentAssessment> studentAssessments =
            // m_data.m_studentAssessmentMap.get(student.getOid());

            // Map<String, ScheduleInfo> list = new HashMap<String, ScheduleInfo>();
            /*
             * Collection<StudentSchedule> studentSchedules =
             * m_data.m_scheduleMap.get(student.getOid());
             * if (studentSchedules != null)
             * {
             * for (StudentSchedule studentSchedule : studentSchedules)
             * {
             * ScheduleInfo info = new ScheduleInfo();
             * info.m_schedule = studentSchedule;
             * list.put(studentSchedule.getSectionOid(), info);
             * }
             * }
             */
            // m_courses = new ArrayList<PARCCPNP.ScheduleInfo>(list.values());

            // setRowCount(list.size());
            /*
             * if (studentAssessments == null)
             * {
             * setRowCount(0);
             * }
             * else
             * {
             * setRowCount(studentAssessments.size());
             * }
             */

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
     * A container class for schedule information for one scheduled class.
     * This will contain one of a StudentSchedule or Transcript
     *
     * @author X2 Development Corporation
     */
    protected static class ScheduleInfo {
        public Transcript m_transcript;
        public StudentSchedule m_schedule;
    }

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
            PARCCPNPEntity pnpEntity = (PARCCPNPEntity) entity;
            Object value = null;
            Object result = null;

            StudentAssessment studentAssessment = (StudentAssessment) pnpEntity.getBean();
            String param = (String) field.getParameter();

            if (PARAM_PARCC_TEST_CODE.equals(param)) {
                if (m_data.m_definitionTestCodeName != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionTestCodeName);
                }
            } else if (PARAM_TEST_FORMAT.equals(param)) {
                Object code;
                if (m_data.m_definitionTestFormatName != null) {
                    code = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionTestFormatName);

                    if (code != null) {
                        // Should be P or O
                        value = code;
                    }
                }
            } else if (PARAM_TEST_RETEST.equals(param)) {
                Object reTest;
                if (m_data.m_definitionRetestName != null) {
                    reTest = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionRetestName);

                    value = "N";
                    if ("1".equals(reTest)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_ALTR_PAPER_TEST.equals(param)) {
                if (m_data.m_definitionAltRepPaperTestName != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionAltRepPaperTestName);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_MATH_ASSESS_IN_PAPER.equals(param)) {
                if (m_data.m_definitionTransMathPaperName != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionTransMathPaperName);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_HUMAN_READ_HUMAN_SIG.equals(param)) {
                if (m_data.m_definitionHumanName != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionHumanName);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_LARGE_PRINT.equals(param)) {
                if (m_data.m_definitionLargePrintName != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionLargePrintName);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_BRAILLE.equals(param)) {
                if (m_data.m_definitionBrailleName != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionBrailleName);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_FREQUENT_BREAKS.equals(param)) {
                if (m_data.m_definitionACFreqBreaks != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACFreqBreaks);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_SEP_ALT_LOC.equals(param)) {
                if (m_data.m_definitionACSepAltLoc != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACSepAltLoc);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_SMALL_TEST_GROUP.equals(param)) {
                if (m_data.m_definitionACSmtsGrp != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACSmtsGrp);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_SPEC_EQUIP_FURN.equals(param)) {
                if (m_data.m_definitionACSPEquipFurn != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACSPEquipFurn);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_SPEC_AREA_SETTING.equals(param)) {
                if (m_data.m_definitionACSpecArea != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACSpecArea);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_TIME_OF_DAY.equals(param)) {
                if (m_data.m_definitionACTod != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACTod);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_AF_ANS_MASKING.equals(param)) {
                if (m_data.m_definitionACAnsMask != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACAnsMask);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_AF_COLOR_CONTRAST.equals(param)) {
                if (m_data.m_definitionACColorContrst != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACColorContrst);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_AF_TEST_SPEECH_MATH.equals(param)) {
                if (m_data.m_definitionACTTSMath != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACTTSMath);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_AF_HR_HS_MATH.equals(param)) {
                if (m_data.m_definitionACHrHsMath != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACHrHsMath);
                }
            } else if (PARAM_PA_ASL_VIDEO.equals(param)) {
                if (m_data.m_definitionACAslVideo != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACAslVideo);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_PA_AT_SRN_RDR.equals(param)) {
                if (m_data.m_definitionACScreenRdr != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACScreenRdr);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_PA_LSD_CAP_ELA.equals(param)) {
                if (m_data.m_definitionACCCELA != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACCCELA);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_PA_HR_HS_ELA.equals(param)) {
                if (m_data.m_definitionACHRHSELA != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACHRHSELA);
                }
            } else if (PARAM_PA_REF_BRAL_ELA.equals(param)) {
                if (m_data.m_definitionACRBELA != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACRBELA);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_PA_TAC_GRAPH.equals(param)) {
                if (m_data.m_definitionACTG != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionACTG);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_PA_TXT_TO_SPCH.equals(param)) {
                if (m_data.m_definitionPATTSELA != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionPATTSELA);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_RA_ANS_REC_TST_BK.equals(param)) {
                if (m_data.m_definitionRAArtB != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionRAArtB);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_RA_BRAL_RESP.equals(param)) {
                if (m_data.m_definitionRABR != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionRABR);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_RA_CALC_DEV_MATH.equals(param)) {
                if (m_data.m_definitionRACDMT != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionRACDMT);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_RA_CNST_RESP_ELA.equals(param)) {
                if (m_data.m_definitionRAELACR != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionRAELACR);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_RA_MATH_RESP.equals(param)) {
                if (m_data.m_definitionRAMR != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionRAMR);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_RA_MON_TEST_RESP.equals(param)) {
                if (m_data.m_definitionRAMTR != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionRAMTR);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_RA_SEL_RESP_TCH_ENH.equals(param)) {
                if (m_data.m_definitionRASRTE != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionRASRTE);
                }
            } else if (PARAM_RA_WORD_PRED.equals(param)) {
                if (m_data.m_definitionRAWP != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionRAWP);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_AEL_DIRC_STD_NAV_LANG.equals(param)) {
                if (m_data.m_definitionAELDSNL != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionAELDSNL);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_AEL_DIRC_READ_ALOUD.equals(param)) {
                if (m_data.m_definitionAELDRA != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionAELDRA);
                }
            } else if (PARAM_AEL_MATH_RESP_EL.equals(param)) {
                if (m_data.m_definitionAELMREL != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionAELMREL);
                }
            } else if (PARAM_AEL_TRNS_MATH_TXT_SPCH.equals(param)) {
                if (m_data.m_definitionAELTMATS != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionAELTMATS);
                }
            } else if (PARAM_AEL_TRANS_MATH_ONLINE.equals(param)) {
                if (m_data.m_definitionAELTMAOL != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionAELTMAOL);
                }
            } else if (PARAM_AEL_WRD_TO_WRD_DICT.equals(param)) {
                if (m_data.m_definitionAELWWDict != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionAELWWDict);
                    if ("1".equals(result)) {
                        value = "Y";
                    }
                }
            } else if (PARAM_TSA_Extnd_TIME.equals(param)) {
                if (m_data.m_definitionTCAET != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_definitionTCAET);
                }
            }

            return value;
        }
    }

    /**
     * Retrieve a students Ethnicity the Sate uses single character designation for race
     * without a description of the race associated with the char H could mean Hispanic, Hawaiian.
     * State id, Race Ethnicity .
     *
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveEthnicity implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PARCCPNPEntity pnpEntity = (PARCCPNPEntity) entity;
            Object value = null;
            String param = (String) field.getParameter();
            SisStudent student = pnpEntity.getStudent();

            Collection<Race> races = m_personRaceMap.get(student.getPersonOid());

            if (PARAM_STATE_ID.equals(param)) {
                value = getCurrentState();
            } else if (PARAM_HISPANIC_LATINO.equals(param)) {
                if (student.getPerson().getHispanicLatinoIndicator()) {
                    value = "Y";
                } else {
                    value = "N";
                }
            } else if (PARAM_INDIAN_ALASKIAN_NATIVE.equals(param)) {
                for (Race r : races) {
                    if ((r.getRaceCode().compareToIgnoreCase("I") == 0) ||
                            (r.getRaceCode().compareToIgnoreCase("Indian") == 0) ||
                            (r.getRaceCode().compareToIgnoreCase("Alaska") == 0)) {
                        value = "Y";
                    } else {
                        value = "N";
                    }
                }
            } else if (PARAM_ASIAN.equals(param)) {
                for (Race r : races) {
                    if ((r.getRaceCode().compareToIgnoreCase("A") == 0) ||
                            (r.getRaceCode().compareToIgnoreCase("Asian") == 0)) {
                        value = "Y";
                    } else {
                        value = "N";
                    }
                }
            } else if (PARAM_BLACK_AFRICAN_AMERICAN.equals(param)) {
                for (Race r : races) {
                    if ((r.getRaceCode().compareToIgnoreCase("B") == 0) ||
                            (r.getRaceCode().compareToIgnoreCase("Black") == 0) ||
                            (r.getRaceCode().compareToIgnoreCase("African") == 0)) {
                        value = "Y";
                    } else {
                        value = "N";
                    }
                }
            } else if (PARAM_HAWAIIAN_PACIFIC_ISLANDER.equals(param)) {
                for (Race r : races) {
                    if ((r.getRaceCode().compareToIgnoreCase("Hawaiian") == 0) ||
                            (r.getRaceCode().compareToIgnoreCase("Pacific") == 0) ||
                            (r.getRaceCode().compareToIgnoreCase("P") == 0)) {
                        value = "Y";
                    } else {
                        value = "N";
                    }
                }
            } else if (PARAM_WHITE.equals(param)) {
                for (Race r : races) {
                    if ((r.getRaceCode().compareToIgnoreCase("White") == 0) ||
                            (r.getRaceCode().compareToIgnoreCase("W") == 0)) {
                        value = "Y";
                    } else {
                        value = "N";
                    }
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
            PARCCPNPEntity pnpEntity = (PARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            if (m_englishLanguageLearnerPrograms.containsKey(student.getOid())) {
                StudentProgramParticipation program = m_englishLanguageLearnerPrograms.get(student.getOid());
                /*
                 * Having had loaded the ell map with only ELL programs
                 */
                if (PARAM_HAS_ELL_CODE.equals(param)) {
                    value = "Y";
                }

                if (PARAM_LEP_BEGIN_DATE.equals(param)) {
                    value = program.getStartDate().toString();
                }

                if (PARAM_LEP_END_DATE.equals(param)) {
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
            PARCCPNPEntity pnpEntity = (PARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            lunchStatus = (String) student.getFieldValueByAlias(ALIAS_LUNCH_STATUS);

            if (lunchStatus != null) {
                if (lunchStatus.compareToIgnoreCase("Free") == 0) {
                    value = "Y";
                }
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

            PARCCPNPEntity pnpEntity = (PARCCPNPEntity) entity;
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
            PARCCPNPEntity pnpEntity = (PARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            Object value = null;
            String param = (String) field.getParameter();

            if (PARAM_DISTRICT.equalsIgnoreCase(param)) {
                value = student.getOrganization1().getFieldValueByBeanPath(m_districtCode);
            } else if (PARAM_SCHOOL.equalsIgnoreCase(param)) {
                value = student.getSchool().getFieldValueByBeanPath(m_schoolCode);
            }

            return value;
        }
    }

    /**
     * Retrieve master schedule information based from the beanpath (which is passed
     * from the calc ID).
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveMasterScheduleInfo implements FieldRetriever {

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
            String param = (String) field.getParameter();
            PARCCPNPEntity pnpEntity = (PARCCPNPEntity) entity;
            ScheduleInfo info = pnpEntity.getCurrentSchedule();

            boolean msFound = true;
            MasterSchedule ms = null;
            if (info.m_transcript != null) {
                ms = info.m_transcript.getMasterSchedule();
            } else if (info.m_schedule != null) {
                ms = info.m_schedule.getSection();
            }

            if (ms == null) {
                msFound = false;
            }

            Object value = null;
            if (msFound) {
                param = param.replace("[" + DOE_SUBJECT + "]", m_doeSubjectCodeField);
                param = param.replace("[" + DOE_IB + "]", m_doeIb);
                param = param.replace("[" + DOE_HSA_PREREQ + "]", m_doeHsaPreReq);
                param = param.replace("[" + DOE_CLASS_OF_RECORD + "]", m_doeClassOfRecord);
                param = param.replace("[" + DOE_CTE_CONCENTRATOR + "]", m_doeCteConcentrator);

                if (!StringUtils.isEmpty(m_doeCteAssessment) && !StringUtils.isEmpty(m_doeAdvTechEd)) {
                    param = param.replace("[" + DOE_CTE_CIP + "]", m_doeCteCip);
                    param = param.replace("[" + DOE_CTE_ASSESSMENT + "]", m_doeCteAssessment);
                    param = param.replace("[" + DOE_TECH_ED + "]", m_doeTechEd);
                    param = param.replace("[" + DOE_ADV_TECH_ED + "]", m_doeAdvTechEd);
                }

                value = getProperty(ms, param);

                if (field.getFieldId().matches("AP/Honors|MSA Subject Flag")) {
                    value = lookupStateValue(ms.getClass(), param, (String) value);
                }
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
            PARCCPNPEntity pnpEntity = (PARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            Object value = null;
            // String param = (String) field.getParameter();
            IepDisability primaryDisability = null;
            String disabilityStateCode = null;

            IepData iepData = student.getActiveIep(getBroker());
            if (iepData == null) {
                iepData = student.getPreviousIep(getBroker());
            }
            if (iepData != null) {
                String status = student.getSpedStatusCode();
                if ((status != null) && (status.compareToIgnoreCase("Active") == 0)) {
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
         * Gets the field value.
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
            PARCCPNPEntity pnpEntity = (PARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            String result = null;
            Object value = null;
            String param = (String) field.getParameter();

            if (PARAM_PARCC_504.equals(param)) {
                result = student.getSection504StatusCode();
                if ((result != null) && (result.compareToIgnoreCase("Active") == 0)) {
                    value = "Y";
                }
            } else if (PARAM_PARCC_ELL.equals(param)) {
                result = (String) student.getFieldValueByAlias(DOE_ESL_FLAG);
                if ((result != null) && (result.compareToIgnoreCase("0") != 0)) {
                    value = "Y";
                }
            } else if (PARAM_PARCC_IEP.equals(param)) {
                result = (String) student.getFieldValueByAlias(DOE_SPED_FLAG);
                if ((result != null) && (result.compareToIgnoreCase("0") != 0)) {
                    value = "Y";
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
            PARCCPNPEntity pnpEntity = (PARCCPNPEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            Object value = null;

            if (PARAM_SPED_STATUS.equals(param)) {
                String status = student.getSpedStatusCode();
                if ((status != null) && (status.compareToIgnoreCase("Active") == 0)) {
                    value = "Y";
                }
            } else if (PARAM_SPED_SERVICE.equals(param)) {
                value = student.getFieldValueByAlias(DOE_SPED_SERVICE);
            } else if (PARAM_SPED_END_DATE.equals(param)) {
                value = student.getFieldValueByAlias(DOE_SPED_END_DATE);
            }

            return value;
        }
    }

    /**
     * The Class RetrieveTeacherInfo.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTeacherInfo implements FieldRetriever {
        /**
         * Values for field calculation parameters. Course related values.
         */
        private static final String PARAM_SATID = "SATID";
        private static final String PARAM_LATID = "LATID";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PARCCPNPEntity pnpEntity = (PARCCPNPEntity) entity;
            ScheduleInfo info = pnpEntity.getCurrentSchedule();
            Object value = null;

            MasterSchedule section = null;
            if (info.m_transcript != null) {
                section = info.m_transcript.getMasterSchedule();
            } else if (info.m_schedule != null) {
                section = info.m_schedule.getSection();
            }

            String param = (String) field.getParameter();

            // Need the section for anything.
            if (section != null) {
                // Not concerned of the staff member is primary
                Staff staff = section.getPrimaryStaff();

                if (staff != null) {
                    if (PARAM_SATID.equals(param)) {
                        value = staff.getStateId();
                    } else if (PARAM_LATID.equals(param)) {
                        value = staff.getLocalId();
                    }
                }
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
            PARCCPNPEntity pnpEntity = (PARCCPNPEntity) entity;
            Object value = null;

            String param = (String) field.getParameter();
            value = Boolean.FALSE;
            SisSchool school = pnpEntity.getStudent().getSchool();

            if (school != null) {
                // String sklT1Flag = (String) school.getFieldValueByAlias("DOE Title 1");

                if (PARAM_TITLE1.equals(param)) {
                    value = "Y";
                } else {
                    value = "N";
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
     * Parameters
     */
    protected static final String PARAM_STATE_ID = "STATE_ID";
    protected static final String PARAM_RACE_PLUS = "RACE_PLUS";
    protected static final String PARAM_HISPANIC_LATINO = "HISPANIC_LATINO";
    protected static final String PARAM_INDIAN_ALASKIAN_NATIVE = "INDIAN_ALASKIAN_NATIVE";
    protected static final String PARAM_ASIAN = "ASIAN";
    protected static final String PARAM_BLACK_AFRICAN_AMERICAN = "BLACK_AFRICAN_AMERICAN";
    protected static final String PARAM_HAWAIIAN_PACIFIC_ISLANDER = "HAWAIIAN_PACIFIC_ISLANDER";
    protected static final String PARAM_WHITE = "WHITE";
    protected static final String PARAM_HAS_DISABILITY = "HAS_DISABILITY";
    protected static final String PARAM_HAS_DISABILITY_CODE = "HAS_DISABILITY_CODE";
    protected static final String PARAM_HAS_ELL_CODE = "HAS_ELL_CODE";
    protected static final String PARAM_LEP_BEGIN_DATE = "LEP_BEGIN_DATE";
    protected static final String PARAM_LEP_END_DATE = "LEP_END_DATE";
    protected static final String PARAM_SPED_STATUS = "SPED_STATUS";
    protected static final String PARAM_SPED_SERVICE = "SPED_SERVICE";
    protected static final String PARAM_SPED_END_DATE = "SPED_END_DATE";
    protected static final String PARAM_TAS = "TAS";
    protected static final String PARAM_TITLE1 = "T1";
    protected static final String PARAM_LEP_READING_EXEMPT = "LEP_READING_EXEMPT";
    protected static final String PARAM_LEP_MATH_EXEMPT = "LEP_MATH_EXEMPT";
    protected static final String PARAM_ACCOUNTABLE = "ACCOUNTABLE";
    protected static final String PARAM_COUNTY = "COUNTY";
    protected static final String PARAM_DISTRICT = "DISTRICT";
    protected static final String PARAM_SCHOOL = "SCHOOL";
    protected static final String PARAM_PARCC_TEST_CODE = "PARCC_TEST_CODE";
    protected static final String PARAM_TEST_FORMAT = "PARCC_TEST_FORMAT";
    protected static final String PARAM_TEST_RETEST = "PARCC_TEST_RETEST";
    protected static final String PARAM_PARCC_504 = "PARCC_504";
    protected static final String PARAM_PARCC_ELL = "PARCC_ELL";
    protected static final String PARAM_PARCC_IEP = "PARCC_IEP";
    protected static final String PARAM_ALTR_PAPER_TEST = "ALTR_PAPER_TEST";
    protected static final String PARAM_MATH_ASSESS_IN_PAPER = "MATH_ASSESS_IN_PAPER";
    protected static final String PARAM_HUMAN_READ_HUMAN_SIG = "HUMAN_READ_HUMAN_SIG";
    protected static final String PARAM_LARGE_PRINT = "LARGE_PRINT";
    protected static final String PARAM_BRAILLE = "BRAILLE";
    protected static final String PARAM_TRANS_MATH_PAPER = "MATH_ASSESS_IN_PAPER";
    protected static final String PARAM_FREQUENT_BREAKS = "FREQUENT_BREAKS";
    protected static final String PARAM_SEP_ALT_LOC = "SEP_ALT_LOC";
    protected static final String PARAM_SMALL_TEST_GROUP = "SMALL_TEST_GROUP";
    protected static final String PARAM_SPEC_EQUIP_FURN = "SPEC_EQUIP_FURN";
    protected static final String PARAM_SPEC_AREA_SETTING = "SPEC_AREA_SETTING";
    protected static final String PARAM_TIME_OF_DAY = "TIME_OF_DAY";
    protected static final String PARAM_AF_ANS_MASKING = "AF_ANS_MASKING";
    protected static final String PARAM_AF_COLOR_CONTRAST = "AF_COLOR_CONTRAST";
    protected static final String PARAM_AF_TEST_SPEECH_MATH = "AF_TEST_SPEECH_MATH";
    protected static final String PARAM_AF_HR_HS_MATH = "AF_HR_HS_MATH";
    protected static final String PARAM_PA_ASL_VIDEO = "PA_ASL_VIDEO";
    protected static final String PARAM_PA_AT_SRN_RDR = "PA_AT_SRN_RDR";
    protected static final String PARAM_PA_LSD_CAP_ELA = "PA_LSD_CAP_ELA";
    protected static final String PARAM_PA_HR_HS_ELA = "PA_HR_HS_ELA";
    protected static final String PARAM_PA_REF_BRAL_ELA = "PA_REF_BRAL_ELA";
    protected static final String PARAM_PA_TAC_GRAPH = "PA_TAC_GRAPH";
    protected static final String PARAM_PA_TXT_TO_SPCH = "PA_TXT_TO_SPCH";
    protected static final String PARAM_RA_ANS_REC_TST_BK = "RA_ANS_REC_TST_BK";
    protected static final String PARAM_RA_BRAL_RESP = "RA_BRAL_RESP";
    protected static final String PARAM_RA_CALC_DEV_MATH = "RA_CALC_DEV_MATH";
    protected static final String PARAM_RA_CNST_RESP_ELA = "RA_CNST_RESP_ELA";
    protected static final String PARAM_RA_MATH_RESP = "RA_MATH_RESP";
    protected static final String PARAM_RA_MON_TEST_RESP = "RA_MON_TEST_RESP";
    protected static final String PARAM_RA_SEL_RESP_TCH_ENH = "RA_SEL_RESP_TCH_ENH";
    protected static final String PARAM_RA_WORD_PRED = "RA_WORD_PRED";
    protected static final String PARAM_AEL_DIRC_STD_NAV_LANG = "AEL_DIRC_STD_NAV_LANG";
    protected static final String PARAM_AEL_DIRC_READ_ALOUD = "AEL_DIRC_READ_ALOUD";
    protected static final String PARAM_AEL_MATH_RESP_EL = "AEL_MATH_RESP_EL";
    protected static final String PARAM_AEL_TRNS_MATH_TXT_SPCH = "AEL_TRNS_MATH_TXT_SPCH";
    protected static final String PARAM_AEL_TRANS_MATH_ONLINE = "AEL_TRANS_MATH_ONLINE";
    protected static final String PARAM_AEL_WRD_TO_WRD_DICT = "AEL_WRD_TO_WRD_DICT";
    protected static final String PARAM_TSA_Extnd_TIME = "TSA_Extnd_TIME";

    /*
     * Aliases
     */
    // MSA Subject Flag of the Course table
    protected static final String DOE_CLASS_OF_RECORD = "DOE CLASS OF RECORD";
    // HSA Subject Flag of the Course table
    protected static final String DOE_HSA_PREREQ = "DOE HSA PREREQ";
    // International Baccalaureate of the Course table
    protected static final String DOE_IB = "DOE IB";
    // the Course may not have a subject code of the Course table
    protected static final String DOE_SUBJECT = "RI Course ID";
    // Technology Education of the Course table
    protected static final String DOE_TECH_ED = "DOE TECH ED";
    // Advanced Technology Education of the Course table
    protected static final String DOE_ADV_TECH_ED = "DOE ADV TECH ED";
    // CIP Number of the Course table
    protected static final String DOE_CTE_CIP = "DOE CTE CIP";
    protected static final String DOE_CTE_ASSESSMENT = "DOE CTE ASSESSMENT";
    // CTE Concentrator of the Course table
    protected static final String DOE_CTE_CONCENTRATOR = "DOE CTE CONCENTRATOR";
    // add on student program participation table
    protected static final String DOE_ELL_PROGRAM_CODE = "RI Program Code";
    protected static final String DOE_ELL_BEGIN_DATE = "DOE ELL BEGIN DATE";
    protected static final String DOE_ELL_END_DATE = "DOE ELL END DATE";
    protected static final String DOE_SPED_FLAG = "SPED Flag";
    protected static final String DOE_SPED_SERVICE = "DOE SPECIAL ED";
    protected static final String DOE_SPED_END_DATE = "DOE SPED END";
    protected static final String DOE_ESL_FLAG = "ESL Flag";
    // off the Student table
    protected static final String ALIAS_ACCOUNTABLE_COUNTY = "DOE ACCOUNTABLE COUNTY";
    protected static final String ALIAS_ACCOUNTABLE_DISTRICT = "DOE ACCOUNTABLE DISTRICT";
    protected static final String ALIAS_ACCOUNTABLE_SCHOOL = "DOE ACCOUNTABLE SCHOOL";
    protected static final String ALIAS_DISTRICT_ID = "RI Reporting District Code";
    protected static final String ALIAS_ELL_IND = "ESL Flag";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    // District code is of the Organization Code
    protected static final String ALIAS_DISTRICT_CODE = "RI Reporting District Code";
    // School Code is of the School table
    protected static final String ALIAS_SCHOOL_CODE = "State School Id";
    //
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_DISTRICT_CODE = "DOE DISTRICT SERVICE";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_LUNCH_STATUS = "Lunch Description";

    // of the Course table
    protected static final String ALIAS_COURSE_GRADE_SPAN = "DOE GRADE SPAN";
    protected static final String ALIAS_COURSE_START_YEAR = "DOE COURSE START YEAR";
    protected static final String ALIAS_COURSE_END_YEAR = "DOE COURSE END YEAR";
    protected static final String ALIAS_CTE_CIP_NUMBER = "DOE CTE CIP";
    protected static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    protected static final String ALIAS_REMARKS = "DOE REMARKS";
    // SCED Course Code off the Course table
    protected static final String ALIAS_SCED_CODE = "DOE SCED CODE";
    // set on the Assessment Definition
    protected static final String ALIAS_PARCC_RETEST = "DOE PARCC_RETEST";
    protected static final String ALIAS_PARCC_ALTREP_PAPERTST = "DOE ALTREP PAPERTST";
    protected static final String ALIAS_PARCC_TSTCODE = "DOE PARCC_TSTCODE";
    protected static final String ALIAS_PARCC_BRAILLE = "DOE PARCC_Braille";
    protected static final String ALIAS_PARCC_LARGEPRT = "DOE PARCC_LARGEPRT";
    protected static final String ALIAS_PARCC_TRANSMATHPAPER = "DOE PARCC_TRANSMATHPAPER";
    protected static final String ALIAS_PARCC_TSTFORMAT = "DOE PARCC_TSTFORMAT";
    protected static final String ALIAS_PARCC_HUMAN = "DOE PARCC_Human";
    // new to PNP
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
    protected static final String ALIAS_PARCC_AEL_WWDict = "DOE PARCC_AEL_WWDict";
    protected static final String ALIAS_PARCC_TSA_ET = "DOE PARCC_TSA_ET";

    protected static final String REF_ELL_KEY = "DOE PR ELL";

    /**
     * Other internal constants
     */
    protected static final String NOT_GIFTED_OR_TALENTED = "N";

    protected static final String GIFTED_AND_TALENTED_PROGRAM = "ALP";
    protected static final String REPORTABLE_CODE = "report";
    protected static final String ENGLISH_LANGUAGE_LEARNER_PROGRAM = "ELL%";

    protected static final String LEP_READING_MATH_EXEMPT = "03, 04, 05, 06, 07, 08";

    protected static final String CODE_ASSESSMENT_DEFINITION_ID_PNP = "PARCC";

    protected static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Boolean m_removeHeader;
    protected int m_schoolYear;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;
    protected PlainDate m_startDate;
    protected PlainDate m_endDate;
    protected PlainDate m_firstDayDate;
    protected String m_servDistrict;
    protected String m_servSchool;
    protected String m_fieldEslInd;

    protected String m_fieldDistrictCode;
    protected String m_enrollmentGradeLevel;
    protected String m_enrollmentGradeLevelOverride;
    protected String m_excludeStdField;
    protected String m_fieldLunchStatusCode;
    protected String m_scedCode = null;
    protected Map<String, String> m_disabilityCodes;
    protected Map<String, ReferenceCode> m_programCode;

    protected String m_doeClassOfRecord;
    protected String m_doeCteAssessment;
    protected String m_doeCteCip;
    protected String m_doeHsaPreReq;
    protected String m_doeCteConcentrator;
    protected String m_doeIb;
    protected String m_doeSubjectCodeField;
    protected String m_doeTechEd; // added for allegany (and wicomico)
    protected String m_doeTraditionalCourseFlag;
    protected String m_doeAdvTechEd; // added for allegany
    protected Map<String, ReferenceCode> m_gradeCodes;
    protected Map<String, ReferenceCode> m_techEduCodes; // added for wicomico
    protected Map<String, Collection<GradeScalePoints>> m_gradePoints;
    protected Map<String, GradeScale> m_gradeScales;
    protected Integer m_reportingPeriod;
    protected Map<String, Collection<StudentSchedule>> m_studentScheduleMap;
    protected Map<String, Collection<StudentAssessment>> m_studentAssessmentMap;
    protected String m_spedActiveCode;
    protected String m_studentActiveCode;
    protected Map<String, Collection<ScheduleTeacher>> m_teacherSchedules;
    protected Map<String, Collection<ScheduleTermDate>> m_termDates;
    protected Map<String, Collection<Transcript>> m_transcriptMap;
    protected Set<String> m_withdrawalCodes;
    protected String m_accountableSchool;
    protected String m_accountableDistrict;
    protected String m_districtCode;
    protected String m_schoolCode;
    protected String m_eslCodeColumnName;

    protected Map<String, StudentProgramParticipation> m_englishLanguageLearnerPrograms;
    protected Map<String, StudentProgramParticipation> m_giftedPrograms;
    protected Map<String, Collection<Race>> m_personRaceMap;

    protected String m_definitionAltRepPaperTestName;
    protected String m_definitionBrailleName;
    protected String m_definitionFormatName;
    protected String m_definitionHumanName;
    protected String m_definitionLargePrintName;
    protected String m_definitionRetestName;
    protected String m_definitionTestCodeName;
    protected String m_definitionTestFormatName;
    protected String m_definitionTransMathPaperName;

    protected String m_definitionACFreqBreaks;
    protected String m_definitionACSepAltLoc;
    protected String m_definitionACSmtsGrp;
    protected String m_definitionACSPEquipFurn;
    protected String m_definitionACSpecArea;
    protected String m_definitionACTod;
    protected String m_definitionACAnsMask;
    protected String m_definitionACColorContrst;
    protected String m_definitionACTTSMath;
    protected String m_definitionACHrHsMath;
    protected String m_definitionACAslVideo;
    protected String m_definitionACScreenRdr;
    protected String m_definitionACCCELA;
    protected String m_definitionACHRHSELA;
    protected String m_definitionACRBELA;
    protected String m_definitionACTG;
    protected String m_definitionPATTSELA;
    protected String m_definitionRAArtB;
    protected String m_definitionRABR;
    protected String m_definitionRACDMT;
    protected String m_definitionRAELACR;
    protected String m_definitionRAMR;
    protected String m_definitionRAMTR;
    protected String m_definitionRASRTE;
    protected String m_definitionRAWP;
    protected String m_definitionAELDSNL;
    protected String m_definitionAELDRA;
    protected String m_definitionAELMREL;
    protected String m_definitionAELTMATS;
    protected String m_definitionAELTMAOL;
    protected String m_definitionAELWWDict;
    protected String m_definitionTCAET;

    /*
     * PARCC required aliases
     *
     */
    protected String m_doeSubject;
    protected String m_doeReportingDistrictCode;
    protected String m_doeSchoolId;
    protected String m_doeProgramCode;
    protected String m_doeProgramStartDate;
    protected String m_doeProgramEndDate;
    protected String m_doeLunchDeescription;

    /**
     * A map of student enrollments (Collection[StudentEnrollment]) by student oid
     *
     * Sorted in descending order. (First entry is recent, last entry is earliest)
     */
    Map<String, List<StudentEnrollment>> m_enrollmentMap;

    /**
     * A map of student program participations ("Free/Reduced Lunch"), for use in the FRL retriever
     */
    protected Map<String, List<StudentProgramParticipation>> m_frlMap;

    /**
     * Keep track of number of ELL programs
     */
    int m_totalProgramCount;


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
        setCurrentState();
        initializeFields();

        loadPNPAssessmentDefinition();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        X2Criteria studentCritiera = m_helper.getStudentCriteria();
        SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCritiera);

        X2Criteria studentAssessmentCriteria = getStudentAssessmentCriteria(studentSubQuery);
        QueryByCriteria studentAssessmentQuery =
                new QueryByCriteria(StudentAssessment.class, studentAssessmentCriteria);

        loadProgramCodes();
        loadScheduleTermDates();

        // setPersonRaceMap(m_helper.getPersonRaceMap());

        loadStudentEnglishLanguagePrograms(studentSubQuery);
        loadStudentGiftedPrograms(studentSubQuery);
        loadStudentSchedules(studentSubQuery);

        loadTeacherSchedules();
        m_personRaceMap = m_helper.getPersonRaceMap();

        // Assign the custom entity class, if there is one.
        setQuery(studentAssessmentQuery);
        setEntityClass(PARCCPNPEntity.class);

        /*
         * load the current years Gifted Talented
         * Not for RI
         */
        // loadCurrentYearGTPrograms();

        // Assign custom field retriever calculations.
        Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("PNP-ELL", new RetrieveEnglishLanguageLearner());
        calcs.put("PNP-FARM", new RetrieveFreeReducedLunch());
        calcs.put("PNP-DISABILITY", new RetrievePrimaryDisability());
        calcs.put("PNP-GT", new RetrieveGifted());
        calcs.put("PNP-SPED", new RetrieveSPED());
        calcs.put("PNP-T1", new RetrieveTitle1());
        calcs.put("PNP-RACE", new RetrieveEthnicity());
        calcs.put("PNP-LOCATION", new RetrieveLocation());
        calcs.put("PNP-TEST", new RetrieveAssessment());
        calcs.put("PNP-STDACC", new RetrieveStudentAccomodations());
        // calcs.put("PNP-MASTER", new RetrieveMasterScheduleInfo());
        // calcs.put("PNP-TEACHER", new RetrieveTeacherInfo());

        super.addCalcs(calcs);

        // Assign custom field validation calculations.
        /*
         * Map<String, FieldValidator> validations = new HashMap<String, FieldValidator>();
         * calcs.put("SIMS-ENROLLMENT-DATE", new ValidateEnrollmentDate());
         * super.addValidators(validations);
         * 
         */
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
                        AssessmentDefinition.COL_ID, CODE_ASSESSMENT_DEFINITION_ID_PNP);

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
        m_startDate = getCurrentContext().getStartDate();
        m_endDate = getCurrentContext().getEndDate();

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

        /*
         * The export fails to output data but still get the headeer
         * if an ALIAS doesn't exist in the Student table?
         */
        // Were is the ELL indicator set, hope its the is Student table
        // m_fieldEslInd = translateAliasToJavaName(ALIAS_ELL_IND, true);
        // m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        // m_scedCode = translateAliasToJavaName(ALIAS_SCED_CODE, true);
        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        /*
         * Log missing alias.
         */
        if (m_excludeStdField == null) {
            logger.log(Level.WARNING, ALIAS_EXCLUDE_STD + " is not defined");
        }

        /*
         * The following are used to get the Master Schedule
         */
        // Need this used in loadStudentSchedules method
        m_doeSubjectCodeField = translateAliasToJavaName(DOE_SUBJECT, true);
        if (m_doeSubjectCodeField == null) {
            logger.log(Level.WARNING, DOE_SUBJECT + " is not defined");
        }

        m_districtCode = translateAliasToJavaName(ALIAS_DISTRICT_CODE, true);
        if (m_districtCode == null) {
            logger.log(Level.WARNING, ALIAS_DISTRICT_CODE + " is not defined");
        }
        m_schoolCode = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
        if (m_schoolCode == null) {
            logger.log(Level.WARNING, ALIAS_SCHOOL_CODE + " is not defined");
        }

        m_eslCodeColumnName = translateAliasToColumnName(DOE_ELL_PROGRAM_CODE, false);
        if (m_eslCodeColumnName == null) {
            logger.log(Level.WARNING, DOE_ELL_PROGRAM_CODE + " is not defined");
        }
    }

    /**
     * Gets the current state.
     *
     * @return String
     */
    public String getCurrentState() {
        return m_currentState;
    }

    /**
     * Sets the current state.
     */
    public void setCurrentState() {
        this.m_currentState = PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STATE);
    }

    /**
     * Load PNP Assessment Definition Alias field names.
     */
    private void loadPNPAssessmentDefinition() {
        X2Criteria assessmentDefinitonCriteria = new X2Criteria();
        assessmentDefinitonCriteria.addEqualTo(AssessmentDefinition.COL_ID, CODE_ASSESSMENT_DEFINITION_ID_PNP);

        QueryByCriteria assessmentDefinitonQuery =
                new QueryByCriteria(AssessmentDefinition.class, assessmentDefinitonCriteria);

        AssessmentDefinition pnpDefinition =
                (AssessmentDefinition) getBroker().getBeanByQuery(assessmentDefinitonQuery);

        // Load PNP database field names by the PNP Assessment Definition
        if (pnpDefinition != null) {
            DataDictionary dataDictionary =
                    DataDictionary.getDistrictDictionary(pnpDefinition, getBroker().getPersistenceKey());

            if (dataDictionary != null) {
                DataDictionaryField formatField = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TSTFORMAT);
                if (formatField != null) {
                    m_definitionFormatName = formatField.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_TSTFORMAT)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_TSTFORMAT + " is not defined");
                }

                DataDictionaryField retestField = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RETEST);
                if (retestField != null) {
                    m_definitionRetestName = retestField.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_RETEST)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_RETEST + " is not defined");
                }

                DataDictionaryField altrepPaperTstName =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_ALTREP_PAPERTST);
                if (altrepPaperTstName != null) {
                    m_definitionAltRepPaperTestName = altrepPaperTstName.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_ALTREP_PAPERTST)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_ALTREP_PAPERTST + " is not defined");
                }

                DataDictionaryField tstCodeName = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TSTCODE);
                if (tstCodeName != null) {
                    m_definitionTestCodeName = tstCodeName.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_TSTCODE)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_TSTCODE + " is not defined");
                }

                DataDictionaryField brailleName = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_BRAILLE);
                if (brailleName != null) {
                    m_definitionBrailleName = brailleName.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_BRAILLE)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_BRAILLE + " is not defined");
                }

                DataDictionaryField largePrtName = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_LARGEPRT);
                if (largePrtName != null) {
                    m_definitionLargePrintName = largePrtName.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_LARGEPRT)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_LARGEPRT + " is not defined");
                }

                DataDictionaryField transMathPaperName =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TRANSMATHPAPER);
                if (transMathPaperName != null) {
                    m_definitionTransMathPaperName = transMathPaperName.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_TRANSMATHPAPER)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_TRANSMATHPAPER + " is not defined");
                }

                DataDictionaryField tstFormatName =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TSTFORMAT);
                if (tstFormatName != null) {
                    m_definitionTestFormatName = tstFormatName.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_TSTFORMAT)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_TSTFORMAT + " is not defined");
                }

                DataDictionaryField humanName = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_HUMAN);
                if (tstFormatName != null) {
                    m_definitionHumanName = humanName.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_HUMAN)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_HUMAN + " is not defined");
                }

                DataDictionaryField freqBreaks =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_FREQBREAKS);
                if (freqBreaks != null) {
                    m_definitionACFreqBreaks = freqBreaks.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_FREQBREAKS)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_FREQBREAKS + " is not defined");
                }

                DataDictionaryField sepAltLoc = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_SEPALTLOC);
                if (sepAltLoc != null) {
                    m_definitionACSepAltLoc = sepAltLoc.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_SEPALTLOC)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_SEPALTLOC + " is not defined");
                }

                DataDictionaryField smtstGrp = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_SMTSTGRP);
                if (sepAltLoc != null) {
                    m_definitionACSmtsGrp = smtstGrp.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_SMTSTGRP)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_SMTSTGRP + " is not defined");
                }

                DataDictionaryField spEquipFurn =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_SP_EQUIP_FURN);
                if (sepAltLoc != null) {
                    m_definitionACSPEquipFurn = spEquipFurn.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_SP_EQUIP_FURN)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_SP_EQUIP_FURN + " is not defined");
                }

                DataDictionaryField specArea = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_SPEC_AREA);
                if (sepAltLoc != null) {
                    m_definitionACSpecArea = specArea.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_SPEC_AREA)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_SPEC_AREA + " is not defined");
                }

                DataDictionaryField tod = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_TOD);
                if (tod != null) {
                    m_definitionACTod = tod.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_SPEC_AREA)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_SPEC_AREA + " is not defined");
                }

                DataDictionaryField ansMask = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AF_ANSMASK);
                if (tod != null) {
                    m_definitionACAnsMask = ansMask.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AF_ANSMASK)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AF_ANSMASK + " is not defined");
                }

                DataDictionaryField colorContrast =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AF_COLORCONTRST);
                if (colorContrast != null) {
                    m_definitionACColorContrst = colorContrast.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AF_ANSMASK)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AF_ANSMASK + " is not defined");
                }

                DataDictionaryField ttsMath = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AF_TTS_MATH);
                if (ttsMath != null) {
                    m_definitionACTTSMath = ttsMath.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AF_TTS_MATH)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AF_TTS_MATH + " is not defined");
                }

                DataDictionaryField hrHsMath = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_HRHS_MATH);
                if (hrHsMath != null) {
                    m_definitionACHrHsMath = hrHsMath.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_HRHS_MATH)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_HRHS_MATH + " is not defined");
                }

                DataDictionaryField aslVideo = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_ASLVIDEO);
                if (aslVideo != null) {
                    m_definitionACAslVideo = aslVideo.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_ASLVIDEO)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_ASLVIDEO + " is not defined");
                }

                DataDictionaryField screenRdr =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_ATSCREENRDR);
                if (screenRdr != null) {
                    m_definitionACScreenRdr = screenRdr.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_ATSCREENRDR)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_ATSCREENRDR + " is not defined");
                }

                DataDictionaryField ccEla = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_CCELA);
                if (ccEla != null) {
                    m_definitionACCCELA = ccEla.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_CCELA)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_CCELA + " is not defined");
                }

                DataDictionaryField hrHsEla = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_HRHS_ELA);
                if (hrHsEla != null) {
                    m_definitionACHRHSELA = hrHsEla.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_HRHS_ELA)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_HRHS_ELA + " is not defined");
                }

                DataDictionaryField rbEla = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_RBELA);
                if (rbEla != null) {
                    m_definitionACRBELA = rbEla.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_RBELA)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_RBELA + " is not defined");
                }

                DataDictionaryField acTg = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AC_TG);
                if (acTg != null) {
                    m_definitionACTG = acTg.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AC_TG)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AC_TG + " is not defined");
                }

                DataDictionaryField ttsEla = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_PA_TTS_ELA);
                if (ttsEla != null) {
                    m_definitionPATTSELA = ttsEla.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_PA_TTS_ELA)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_PA_TTS_ELA + " is not defined");
                }

                DataDictionaryField raArtb = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_ARTB);
                if (raArtb != null) {
                    m_definitionRAArtB = raArtb.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_RA_ARTB)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_RA_ARTB + " is not defined");
                }

                DataDictionaryField raBr = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_BR);
                if (raBr != null) {
                    m_definitionRABR = raBr.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_RA_BR)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_RA_BR + " is not defined");
                }

                DataDictionaryField cdmt = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_CDMT);
                if (cdmt != null) {
                    m_definitionRACDMT = cdmt.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_RA_CDMT)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_RA_CDMT + " is not defined");
                }

                DataDictionaryField elaCr = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_ELA_CR);
                if (elaCr != null) {
                    m_definitionRAELACR = elaCr.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_RA_ELA_CR)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_RA_ELA_CR + " is not defined");
                }

                DataDictionaryField raMr = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_MR);
                if (raMr != null) {
                    m_definitionRAMR = raMr.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_RA_ELA_CR)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_RA_ELA_CR + " is not defined");
                }

                DataDictionaryField raMtr = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_MTR);
                if (raMtr != null) {
                    m_definitionRAMTR = raMtr.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_RA_MTR)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_RA_MTR + " is not defined");
                }

                DataDictionaryField raSrTe = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_SRTE);
                if (raSrTe != null) {
                    m_definitionRASRTE = raSrTe.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_RA_SRTE)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_RA_SRTE + " is not defined");
                }

                DataDictionaryField raWp = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RA_WP);
                if (raWp != null) {
                    m_definitionRAWP = raWp.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_RA_WP)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_RA_WP + " is not defined");
                }

                DataDictionaryField eldSnl = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AEL_DSNL);
                if (eldSnl != null) {
                    m_definitionAELDSNL = eldSnl.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AEL_DSNL)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AEL_DSNL + " is not defined");
                }

                DataDictionaryField elDra = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AEL_DRA);
                if (elDra != null) {
                    m_definitionAELDRA = elDra.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AEL_DRA)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AEL_DRA + " is not defined");
                }

                DataDictionaryField elMrel = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AEL_MREL);
                if (elMrel != null) {
                    m_definitionAELMREL = elMrel.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AEL_MREL)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AEL_MREL + " is not defined");
                }

                DataDictionaryField eltMats = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AEL_TMATS);
                if (eltMats != null) {
                    m_definitionAELTMATS = eltMats.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AEL_TMATS)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AEL_TMATS + " is not defined");
                }

                DataDictionaryField eltMaol = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AEL_TMAOL);
                if (eltMaol != null) {
                    m_definitionAELTMAOL = eltMaol.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AEL_TMAOL)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AEL_TMAOL + " is not defined");
                }

                DataDictionaryField ellWwDict = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_AEL_WWDict);
                if (ellWwDict != null) {
                    m_definitionAELWWDict = ellWwDict.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_AEL_WWDict)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_AEL_WWDict + " is not defined");
                }

                DataDictionaryField tcAet = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TSA_ET);
                if (tcAet != null) {
                    m_definitionTCAET = tcAet.getJavaName();
                }
                if (!dataDictionary.containsAlias(ALIAS_PARCC_TSA_ET)) {
                    logger.log(Level.WARNING, ALIAS_PARCC_TSA_ET + " is not defined");
                }

            }
        }
    }

    /**
     * Load Program Codes.
     */
    @SuppressWarnings("unchecked")
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
     * Load Schedule Term Dates.
     */
    @SuppressWarnings("unchecked")
    private void loadScheduleTermDates() {
        X2Criteria termDatesCriteria = new X2Criteria();
        termDatesCriteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        if (isSchoolContext()) {
            termDatesCriteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            termDatesCriteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            termDatesCriteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }

        QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, termDatesCriteria);

        m_termDates = getBroker().getGroupedCollectionByQuery(query, ScheduleTermDate.COL_SCHEDULE_TERM_OID, 100);
    }

    /**
     * loads existing Student Program Participation records for the CATE program.
     *
     * @param studentSubQuery SubQuery
     */
    @SuppressWarnings("unchecked")
    private void loadStudentEnglishLanguagePrograms(SubQuery studentSubQuery) {
        m_englishLanguageLearnerPrograms = new HashMap<String, StudentProgramParticipation>();

        if (!StringUtils.isEmpty(m_eslCodeColumnName)) {
            X2Criteria studentProgramCriteria = new X2Criteria();

            studentProgramCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);

            studentProgramCriteria.addLike(StudentProgramParticipation.COL_PROGRAM_CODE,
                    ENGLISH_LANGUAGE_LEARNER_PROGRAM);
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
    @SuppressWarnings("unchecked")
    private void loadStudentGiftedPrograms(SubQuery studentSubQuery) {
        X2Criteria studentProgramcriteria = new X2Criteria();
        studentProgramcriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);

        studentProgramcriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, GIFTED_AND_TALENTED_PROGRAM);
        studentProgramcriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_firstDayDate);
        studentProgramcriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, studentProgramcriteria);

        m_giftedPrograms = getBroker().getMapByQuery(query, StudentProgramParticipation.COL_STUDENT_OID,
                getBroker().getCount(query));
    }

    /**
     * Load all StudentSchedules and load them into a map by student OID.
     *
     * @param studentSubQuery SubQuery
     */
    @SuppressWarnings("unchecked")
    private void loadStudentSchedules(SubQuery studentSubQuery) {
        X2Criteria studentScheduleCriteria = new X2Criteria();

        studentScheduleCriteria.addIn(StudentSchedule.COL_STUDENT_OID, studentSubQuery);

        if (isSchoolContext()) {
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }

        studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, "Class");
        studentScheduleCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        // Exclude courses with no subject code.
        studentScheduleCriteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                m_doeSubjectCodeField,
                getBroker().getPersistenceKey());

        String queryString = (String) getParameter(PARAM_QUERY_STRING_PARAM);
        String queryBy = ((String) getParameter(PARAM_QUERY_BY_PARAM));
        switch (queryBy) {
            case "yog": // YOG
                studentScheduleCriteria.addEqualTo(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_YOG, queryString);
                break;

            case "localId": // LASID
                studentScheduleCriteria.addEqualTo(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_LOCAL_ID,
                        queryString);
                break;

            case "stateId": // SASID
                studentScheduleCriteria.addEqualTo(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        queryString);
                break;

            case "##snapshot": // Snapshot
                addRecordSetCriteria(studentScheduleCriteria, queryString);
                break;

            default:
                // Take all students
                break;
        }

        QueryByCriteria studentScheduleQuery = new QueryByCriteria(StudentSchedule.class, studentScheduleCriteria);

        m_studentScheduleMap =
                getBroker().getGroupedCollectionByQuery(studentScheduleQuery, StudentSchedule.COL_STUDENT_OID, 500);
    }

    /**
     * Load maps of teacher schedules.
     */
    @SuppressWarnings("unchecked")
    private void loadTeacherSchedules() {
        X2Criteria teacherSchedulesCriteria = new X2Criteria();
        teacherSchedulesCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        if (isSchoolContext()) {
            teacherSchedulesCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            teacherSchedulesCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            teacherSchedulesCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }

        QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, teacherSchedulesCriteria);

        m_teacherSchedules = getBroker().getGroupedCollectionByQuery(query, ScheduleTeacher.COL_SECTION_OID, 100);
    }

    /**
     * Translate Alias to Column Name.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    private String translateAliasToColumnName(String alias, boolean required) {
        String columnName = null;

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);

        if (field != null) {
            columnName = field.getDatabaseName();
        } else if (required) {
            Locale locale = LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey());
            String aliasMsg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), locale)
                    .getMessage(ERROR_ALIAS_LOOKUP);
            addSetupError(aliasMsg, alias);
        }

        return columnName;
    }

} // end of PARCCPNP

