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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.AssessmentExportData;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RI Assessment export for PARCC - Personal Needs Profile export.
 *
 * @author X2 Development Corporation
 */
public class RIAssessmentExportData extends AssessmentExportData {

    /**
     * The Class RetrieveIACValues.
     */
    /*
     * Get values for fields concerning Student Accomodations
     */
    protected class RetrieveIACValues implements FieldRetriever {
        protected static final String VALUE_ELA_TEST_CODE = "ELA";
        protected final List VALUE_STD_DISAB = Arrays.asList(new String[] {"IEP", "504"});

        /**
         * CALC ID
         */
        protected static final String CALC_ID = "ACCOMODATIONS";

        /**
         * Values
         */
        protected static final String VALUE_ASTERIX = "*";
        protected static final String VALUE_BOTH = "Both";
        protected static final String VALUE_IEP504 = "IEP504";
        protected static final String VALUE_Y = "Y";

        /**
         * FIELDS
         */
        protected static final String FIELD_ENGLISH_LEARNER = "English Learner";
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
        protected static final String CALC_PARAM_COLOR_CONTRAST = "1d";
        protected static final String CALC_PARAM_DAY_TIME = "2b";
        protected static final String CALC_PARAM_ELA_CONSTR = "ConstrRespELA";
        protected static final String CALC_PARAM_ELA_SELECTED = "SelectedRespELA";
        protected static final String CALC_PARAM_EXT_TIME = "ExtendedTime";
        protected static final String CALC_PARAM_GRAPH_PAPER = "3d";
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
        protected static final String CALC_PARAM_READ_OR_SIGN = "HumanReadOrSign";
        protected static final String CALC_PARAM_RESP_MATH = "ResponseMath";
        protected static final String CALC_PARAM_SCR_READER = "3b";
        protected static final String CALC_PARAM_SPEC_AREA = "2d";
        protected static final String CALC_PARAM_SPEC_EQUIP = "2e";
        protected static final String CALC_PARAM_SPEECH_TEXT = "TextToSpeech";
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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            String value = null;

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            String stdOId = studentAssessment.getStudentOid();
            Collection<String> stdIacCodes = m_iacsCodesMap.get(stdOId);

            String paramString = (String) field.getParameter();

            if (stdIacCodes != null && !stdIacCodes.isEmpty()) {
                if (CALC_PARAM_FR_BREAKS.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_ALT_LOCATION.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_TEST_GROUPS.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_SPEC_EQUIP.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_SPEC_AREA.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_DAY_TIME.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_ANSW_MASK.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_COLOR_CONTRAST.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
                } else if (CALC_PARAM_ASL_VIDEO.equals(paramString)) {
                    String code1 = "3j";
                    String code2 = "3l";
                    if (!StringUtils.isEmpty(entity.getFieldValue(FIELD_TEST_CODE)) &&
                            (!entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE)
                                    && stdIacCodes.contains(code1))
                            ||
                            (!entity.getFieldValue(FIELD_TEST_CODE).contains(VALUE_ELA_TEST_CODE)
                                    && stdIacCodes.contains(code2))) {
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
                } else if (CALC_PARAM_GRAPH_PAPER.equals(paramString) && stdIacCodes.contains(paramString)) {
                    value = VALUE_Y;
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
                    value = "*";
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
                    value = VALUE_ASTERIX;
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
                } else if (CALC_PARAM_SPEECH_TEXT.equals(paramString)) {
                    String code1 = "1r";
                    String code2 = "3i";
                    String code3 = "7j";

                    if (stdIacCodes.contains(code1) ||
                            stdIacCodes.contains(code2) ||
                            stdIacCodes.contains(code3)) {
                        value = "*";
                    }
                } else if (CALC_PARAM_READ_OR_SIGN.equals(paramString)) {
                    String code1 = "1S";
                    String code2 = "3k";
                    String code3 = "7k";

                    if (stdIacCodes.contains(code1) ||
                            stdIacCodes.contains(code2) ||
                            stdIacCodes.contains(code3)) {
                        value = "*";
                    }
                } else if (CALC_PARAM_EXT_TIME.equals(paramString)) {
                    String code1 = "5a";
                    String code2 = "7a";

                    if ((stdIacCodes.contains(code1) || stdIacCodes.contains(code2))
                            && VALUE_Y.equals(entity.getFieldValue(FIELD_ENGLISH_LEARNER))) {
                        value = VALUE_Y;
                    } else if (VALUE_STD_DISAB.contains(entity.getFieldValue(FIELD_STD_DISAB))) {
                        value = VALUE_IEP504;
                    } else if (VALUE_BOTH.contains(entity.getFieldValue(FIELD_STD_DISAB))) {
                        value = VALUE_BOTH;
                    }
                }
            }
            return value;
        }
    }

    /**
     * The Class RetrieveAssessment.
     */
    /*
     * Get Student Assessment field
     */
    protected class RetrieveAssessment implements FieldRetriever {

        protected static final String CALC_ID = "PARCC-TEST";

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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            Object value = null;

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            String alias = (String) field.getParameter();
            String beanPath = ((RIAssessmentExportData) data).m_asmAlias.get(alias);
            if (!StringUtils.isEmpty(beanPath)) {
                value = studentAssessment.getFieldValueByBeanPath(beanPath);
            }
            return value;
        }
    }

    /**
     * The Class RetrieveCourseName.
     */
    /*
     * Get course name from student assessment master schedule
     */
    protected class RetrieveCourseName implements FieldRetriever {

        protected static final String CALC_ID = "COURSE-NAME";

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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            String value = "";

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            MasterSchedule section = studentAssessment.getMasterSchedule();

            if (section != null && !StringUtils.isEmpty(section.getCourseView())) {
                value = section.getCourseView();
            }
            return value;
        }
    }

    /**
     * Returns the district code or school code for the given class section.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDistrictSchool implements FieldRetriever {
        private static final String CALC_ID = "DISTRICT-SCHOOL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            // Check the student adjusted school code.

            StudentAssessment studentAssessment = (StudentAssessment) entity.getBean();
            SisSchool school = studentAssessment.getStudent().getSchool();


            return !StringUtils.isEmpty((String) school.getFieldValueByBeanPath(m_sklFieldAdjDistr))
                    ? school.getFieldValueByBeanPath(m_sklFieldAdjDistr)
                    : school.getOrganization1().getFieldValueByBeanPath(m_orgDistrictIdField);
        }
    }

    /**
     * The Class RetrieveLepInfo.
     */
    /*
     * Get info about LEP
     */
    protected class RetrieveLepInfo implements FieldRetriever {
        protected static final String CALC_ID = "CALC_LEP";

        private static final String PARAM_LEP_LIMITED = "LEP_LIMITED";

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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            String value = null;

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            SisStudent student = studentAssessment.getStudent();

            if (PARAM_LEP_LIMITED.equals(field.getParameter())) {
                Collection<StudentProgramParticipation> pgms = m_lepPgmsMap.get(student.getOid());

                if (pgms != null && !pgms.isEmpty()) {
                    value = BooleanAsStringConverter.TRUE;
                }

            }

            return value;
        }
    }

    /**
     * Retrieve Free, Reduced, Full Price Lunch.
     */
    protected class RetrieveFreeReducedLunch implements FieldRetriever {
        public static final String CALC_ID = "PARCC-FARM";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = CODE_NO;

            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            SisStudent student = parccEntity.getStudent();

            String lunchStatus = (String) student.getFieldValueByBeanPath(m_lunchDescriptionField);
            lunchStatus = data.lookupReferenceCodeByAlias(ALIAS_LUNCH_DESCRIPTION, lunchStatus,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if (Arrays.asList(CODE_LUNCH_STATUS).contains(lunchStatus)) {
                value = CODE_YES;
            }

            return value;
        }
    }

    /**
     * Retriever for Multi Races.
     *
     */
    protected class RetrieveMultRaces implements FieldRetriever {

        protected static final String CALC_ID = "MULT_RACE";

        /**
         * Fields' positions
         */
        protected static final String FIELD_AM_IND = "RaceAmericanIndian";
        protected static final String FIELD_ASIAN = "Race Asian";
        protected static final String FIELD_BLACK = "Race Black";
        protected static final String FIELD_PACIFIC = "Race Pacific";
        protected static final String FIELD_WHITE = "Race White";

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
            int count = 0;

            if (CODE_YES.equals(entity.getFieldValue(FIELD_AM_IND))) {
                count += 1;

                if (count > 1) {
                    return CODE_YES;
                }
            }
            if (CODE_YES.equals(entity.getFieldValue(FIELD_ASIAN))) {
                count += 1;

                if (count > 1) {
                    return CODE_YES;
                }
            }
            if (CODE_YES.equals(entity.getFieldValue(FIELD_BLACK))) {
                count += 1;

                if (count > 1) {
                    return CODE_YES;
                }
            }
            if (CODE_YES.equals(entity.getFieldValue(FIELD_PACIFIC))) {
                count += 1;

                if (count > 1) {
                    return CODE_YES;
                }
            }

            if (CODE_YES.equals(entity.getFieldValue(FIELD_WHITE))) {
                count += 1;

                if (count > 1) {
                    return CODE_YES;
                }
            }

            return CODE_NO;
        }
    }

    /**
     * Class RetrievePrimaryDisability is the FieldRetriever used to retrieve
     * the primary disability state code for the current student.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePrimaryDisability implements FieldRetriever {
        protected static final String CALC_ID = "PR_DIS_TYPE";

        private final Map<String, String> PARCC_DISAB_MAP =
                Collections.unmodifiableMap(new HashMap<String, String>() {
                    {
                        put("A", "EMN");
                        put("B", "ID");
                        put("C", "OI");
                        put("D", "OHI");
                        put("E", "SLD");
                        put("F", "DB");
                        put("G", "HI");
                        put("H", "HH");
                        put("I", "SLI");
                        put("J", "VI");
                        put("K", "MD");
                        put("L", "DD");
                        put("M", "AUT");
                        put("N", "TBI");
                    }
                });

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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            SisStudent student = parccEntity.getStudent();

            Object value = null;
            if (isIep(parccEntity)) {
                Collection<IepDisability> disabilities = m_disabilitiesMap.get(student.getOid());
                if (disabilities != null && !disabilities.isEmpty()) {
                    IepDisability disability = disabilities.iterator().next();

                    if (!StringUtils.isEmpty(disability.getDisabilityCode()) && disability.getPrimaryIndicator()) {
                        String disabilityCode = disability.getDisabilityCode();
                        disabilityCode = data.lookupReferenceCodeByBeanPath(IepDisability.class,
                                IepDisability.COL_DISABILITY_CODE, disabilityCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                        if (disabilityCode != null && PARCC_DISAB_MAP.containsKey(disabilityCode)) {
                            value = PARCC_DISAB_MAP.get(disabilityCode);
                        }
                    }
                }
            }
            return value == null ? "" : value;
        }

        /**
         * Checks if is iep.
         *
         * @param entity AssessmentEntity
         * @return true, if is iep
         */
        private boolean isIep(AssessmentEntity entity) {
            String fieldStdDisabilities = entity.getFieldValue(RetrieveSection504.FIELD_NAME);
            return RetrieveSection504.CODE_IEP.equals(fieldStdDisabilities)
                    || RetrieveSection504.CODE_BOTH.equals(fieldStdDisabilities);
        }
    }

    /**
     * Retrieves the Section 504 status
     *
     * StdDisabilities export format position 290 should return "IEP", "504" or "B" for both
     * When Student field with alias [RI SPED] where code = "Active" return "IEP"
     * When Student field section504StatusCode where code = "Active" return "504"
     * If both Student alias [RI SPED] and Student section504StatusCode are both "Y" then return
     * "B".
     */
    protected class RetrieveSection504 implements FieldRetriever {
        protected static final String CALC_ID = "STD_DISABIL";

        protected static final String CODE_504 = "504";
        protected static final String CODE_BOTH = "B";
        protected static final String CODE_IEP = "IEP";

        protected static final String FIELD_NAME = "StdDisabilities";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = CODE_NO;

            SisStudent student = ((AssessmentEntity) entity).getStudent();

            if (CODE_SPED_STATUS_ACTIVE.equals(student.getSection504StatusCode())) {
                value = CODE_504;
            }

            if (CODE_SPED_STATUS_ACTIVE.equals(student.getFieldValueByBeanPath(m_spedField))) {
                value = CODE_504.equals(value) ? CODE_BOTH : CODE_IEP;
            }

            return value;
        }
    }

    /**
     * The Class RetrieveSessionName.
     */
    /*
     * Get course name from student assessment master schedule
     */
    protected class RetrieveSessionName implements FieldRetriever {

        protected static final String CALC_ID = "SESSION-NAME";

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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            String value = "";

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            MasterSchedule section = studentAssessment.getMasterSchedule();

            if (section != null && !StringUtils.isEmpty(section.getCourseView())) {
                value = section.getCourseView();
            }
            return value;
        }
    }

    // STUDENT Table
    protected static final String ALIAS_ADJUSTED_DISTR = "DOE ADJUSTED DISTRICT";
    protected static final String ALIAS_LUNCH_DESCRIPTION = "Lunch Description";
    protected static final String ALIAS_ORG_DISTR_CODE = "RI Reporting District Code";
    protected static final String ALIAS_SPED = "RI SPED";

    /**
     * Other internal constants
     */
    protected static final String CODE_ASSESSMENT_DEFINITION_ID_RI = "RI PARCC";
    protected static final String CODE_STATE_LEP_PROGRAM = "1051";
    protected static final String CODE_SPED_STATUS_ACTIVE = "Active";
    protected static final String ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS =
            "No RI PARCC Student Assessment Records were created for the selected students.";
    protected static final String[] CODE_LUNCH_STATUS = {"R", "F"};
    protected static final String[] CODE_SPED = {"Y", "S"};

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, Collection<IepDisability>> m_disabilitiesMap;
    protected Map<String, Collection<String>> m_iacsCodesMap;
    protected Map<String, Collection<StudentProgramParticipation>> m_lepPgmsMap;
    protected String m_lunchDescriptionField;
    protected String m_orgDistrictIdField;
    protected String m_sklFieldAdjDistr;
    protected String m_spedField;

    /**
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (m_removeHeader == null || m_removeHeader.booleanValue()) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();
        super.initialize();
        loadLimitedProgramsMap();
        loadDisabilitiesMap();
        loadAccommodationsMap();
        if (getSetupErrors().size() == 0) {
            // Assign custom field retriever calculations.
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveMultRaces.CALC_ID, new RetrieveMultRaces());
            calcs.put(RetrieveSection504.CALC_ID, new RetrieveSection504());
            calcs.put(RetrieveAssessment.CALC_ID, new RetrieveAssessment());
            calcs.put(RetrieveIACValues.CALC_ID, new RetrieveIACValues());
            calcs.put(RetrieveCourseName.CALC_ID, new RetrieveCourseName());
            calcs.put(RetrieveSessionName.CALC_ID, new RetrieveSessionName());
            calcs.put(RetrievePrimaryDisability.CALC_ID, new RetrievePrimaryDisability());
            calcs.put(RetrieveFreeReducedLunch.CALC_ID, new RetrieveFreeReducedLunch());
            calcs.put(RetrieveLepInfo.CALC_ID, new RetrieveLepInfo());
            calcs.put(RetrieveDistrictSchool.CALC_ID, new RetrieveDistrictSchool());
            super.addCalcs(calcs);
        }
    }

    /**
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getAsmDefinitionId()
     */
    @Override
    protected String getAsmDefinitionId() {
        return CODE_ASSESSMENT_DEFINITION_ID_RI;
    }

    /**
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getNoStudentAssessmentMessage()
     */
    @Override
    protected String getNoStudentAssessmentMessage() {
        return ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS;
    }

    /**
     * initialize Fields.
     */
    private void initializeFields() {
        // Load Alias database UDF names
        m_lunchDescriptionField = translateAliasToJavaName(ALIAS_LUNCH_DESCRIPTION, true);
        m_spedField = translateAliasToJavaName(ALIAS_SPED, true);
        m_orgDistrictIdField = translateAliasToJavaName(ALIAS_ORG_DISTR_CODE, true);
        m_sklFieldAdjDistr = translateAliasToJavaName(ALIAS_ADJUSTED_DISTR, true);

    }

    /**
     * Load map with current accommodations keyed on stdOid.
     */
    private void loadAccommodationsMap() {
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

    /**
     * Load map with current disabilities keyed on stdOid.
     */
    private void loadDisabilitiesMap() {
        X2Criteria disCriteria = new X2Criteria();
        disCriteria.addIsNull(IepDisability.COL_END_DATE);
        disCriteria.addEqualTo(IepDisability.COL_PRIMARY_INDICATOR, Boolean.TRUE);

        QueryByCriteria query =
                m_helper.getStudentSelectionQuery(IepDisability.class, disCriteria, IepDisability.COL_STUDENT_OID);
        query.addOrderByDescending(IepDisability.COL_START_DATE);

        m_disabilitiesMap = getBroker().getGroupedCollectionByQuery(query, IepDisability.COL_STUDENT_OID, 1024);
    }

    /**
     * Load map with current programs with state code "1051" keyed on stdOid.
     */
    private void loadLimitedProgramsMap() {
        DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField pgmCodeField =
                dataDictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                        StudentProgramParticipation.COL_PROGRAM_CODE);
        ReferenceTable refTable = pgmCodeField.getReferenceTable();

        Collection<String> lepCodes = new ArrayList<String>();
        if (refTable != null) {
            Collection<ReferenceCode> codes = refTable.getReferenceCodes();

            for (ReferenceCode code : codes) {
                if (!StringUtils.isEmpty(code.getStateCode()) && CODE_STATE_LEP_PROGRAM.equals(code.getStateCode())) {
                    lepCodes.add(code.getCode());
                }
            }
        }

        X2Criteria pgmCriteria = new X2Criteria();
        pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, lepCodes);
        pgmCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        QueryByCriteria query = m_helper.getStudentSelectionQuery(StudentProgramParticipation.class, pgmCriteria,
                StudentProgramParticipation.COL_STUDENT_OID);
        query.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);

        m_lepPgmsMap =
                getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 1024);
    }
}
