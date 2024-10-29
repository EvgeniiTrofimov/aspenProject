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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.AssessmentExportData;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
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
 * MD Assessment export for PARCC - Personal Needs Profile export.
 *
 * @author X2 Development Corporation
 */
public class MDAssessmentExportData extends AssessmentExportData {

    public static class MDPARCCAssessmentEntity extends AssessmentEntity {
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
            StudentAssessment assessment = (StudentAssessment) bean;
            MDAssessmentExportData asmExportData = (MDAssessmentExportData) data;
            String crsTestCode = (String) assessment.getFieldValueByBeanPath(asmExportData.m_asmTestCodeField);
            if (StringUtils.isEmpty(crsTestCode) || !asmExportData.m_testCodes.contains(crsTestCode)) {
                setRowCount(0);
            }
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
            String beanPath = ((MDAssessmentExportData) data).m_asmAlias.get(alias);
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
            StringBuilder value = new StringBuilder();

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            MasterSchedule section = studentAssessment.getMasterSchedule();

            if (section != null && section.getPrimaryStaff() != null && section.getPrimaryStaff().getPerson() != null) {
                value.append(section.getCourseView());
                value.append(" ");
                value.append(section.getPrimaryStaff().getPerson().getLastName());
                value.append(" ");
                value.append(section.getScheduleDisplay());
            }
            return value.toString();
        }
    }

    /**
     * Retrieve Free, Reduced, Full Price Lunch.
     */
    protected class RetrieveFreeReducedLunch implements FieldRetriever {
        public static final String CALC_ID = "PARCC-FARM";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = CODE_NO;

            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            SisStudent student = parccEntity.getStudent();

            String lunchStatus = (String) student.getFieldValueByAlias(ALIAS_LUNCH_STATUS);
            if (Arrays.asList(CODE_LUNCH_STATUS).contains(lunchStatus)) {
                value = CODE_YES;
            }
            lunchStatus = data.lookupReferenceCodeByAlias(ALIAS_LUNCH_STATUS, lunchStatus,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if (Arrays.asList(CODE_LUNCH_STATUS).contains(lunchStatus)) {
                value = CODE_YES;
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
         * @return Objectfv
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
                    String homeLangCode = studentAssessment.getStudent().getHomeLanguageCode();

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
     * Retriever for gifted and talented.
     *
     */
    protected class RetrieveLocation implements FieldRetriever {

        protected static final String CALC_ID = "LOCATION";

        /**
         * Calculation parameters
         */
        protected static final String CALC_PARAM_RESP_DISTRICT = "RESP-DISTRICT";
        protected static final String CALC_PARAM_RESP_SCHOOL = "RESP-SCHOOL";
        protected static final String CALC_PARAM_TEST_SCHOOL = "TEST-SCHOOL";

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
            AssessmentEntity pnpEntity = (AssessmentEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            Object value = null;
            String param = (String) field.getParameter();
            SisSchool school = student.getSchool();

            if (CALC_PARAM_TEST_SCHOOL.equals(param)) {
                String schoolCode = (String) school.getFieldValueByBeanPath(m_schoolCodeField);

                DataDictionaryField dictionaryField = getDataDictionaryField(SisSchool.class, m_schoolCodeField);
                if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                    schoolCode = lookupReferenceCodeByBeanPath(SisSchool.class, m_schoolCodeField,
                            schoolCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }

                value = schoolCode;
            } else if (CALC_PARAM_RESP_SCHOOL.equalsIgnoreCase(param)) {
                String resSchoolCode = (String) school.getFieldValueByBeanPath(m_schoolCodeField);
                DataDictionaryField dictionaryField = getDataDictionaryField(SisSchool.class, m_schoolCodeField);
                if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                    resSchoolCode = lookupReferenceCodeByBeanPath(SisSchool.class, m_schoolCodeField,
                            resSchoolCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }

                value = resSchoolCode;
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

            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            SisStudent student = parccEntity.getStudent();
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

    /**
     * Returns an indicator as to whether the student is participating in a program.
     *
     * The program is identified by the field.getParameter() value matching the State code value
     * of the reference code behind the program code.
     *
     * To match, the student program must have a start date before the report date
     * and the end date must be after the report date or must be empty.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveProgram implements FieldRetriever {
        String m_fieldToRetreive = null;
        Map<String, Collection<StudentProgramParticipation>> m_studentPrograms;

        /**
         * Constructor loads a map of programs by student.
         *
         * @param programStateCode String
         * @param fieldToRetreive String
         * @param stdOids Collection<String>
         */
        public RetrieveProgram(String programStateCode, String fieldToRetreive, Collection<String> stdOids) {
            m_fieldToRetreive = fieldToRetreive;
            String refTableOid = null;
            Collection<String> codes = new ArrayList<>();
            if (m_pgmCodeDDField != null
                    && !StringUtils.isEmpty(refTableOid = m_pgmCodeDDField.getReferenceTableOid())) {
                X2Criteria refCodesCriteria = new X2Criteria();
                refCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, refTableOid);
                refCodesCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, programStateCode);
                codes.addAll(getBroker()
                        .getGroupedCollectionByQuery(new QueryByCriteria(ReferenceCode.class, refCodesCriteria),
                                ReferenceCode.COL_CODE, 128)
                        .keySet());
            }

            // Load programs for use in the retriever.
            Criteria progCriteria = new Criteria();
            progCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, stdOids);
            if (!codes.isEmpty()) {
                progCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, codes);
            } else {
                progCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, "___dummy___");
            }
            QueryByCriteria progQuery = new QueryByCriteria(StudentProgramParticipation.class, progCriteria);
            m_studentPrograms = getBroker().getGroupedCollectionByQuery(progQuery,
                    StudentProgramParticipation.COL_STUDENT_OID, 100);
        }

        /**
         * Retrieve programs that match the field parameter as a state code for the program code,
         * and
         * included in the program start/end date range.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            Collection<StudentProgramParticipation> programs =
                    m_studentPrograms.get(((StudentAssessment) entity.getBean()).getStudentOid());
            Object value = null;
            if (programs != null) {
                for (StudentProgramParticipation program : programs) {
                    if (!StringUtils.isEmpty(m_fieldToRetreive)) {
                        value = getProperty(program, m_fieldToRetreive);
                    } else if (program.getStartDate() != null &&
                            (!m_endDate.before(program.getStartDate()) &&
                                    (program.getEndDate() == null ||
                                            !m_startDate.after(program.getEndDate())))) {
                        value = "Y";
                    }
                    if (StringUtils.isEmpty(m_fieldToRetreive)
                            && !StringUtils.isEmpty(entity.getFieldValue(EXPORT_FIELD_LEP_END_DATE))) {
                        value = "E";
                    }
                    break;
                }
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
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            StringBuilder value = new StringBuilder();

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();

            SisSchool school = studentAssessment.getSchool();
            if (school != null) {
                value.append(school.getSchoolId());
                value.append("-");
                FieldDefinition testCodeField = data.getFieldDefinition("TestCode");
                String testCode = (String) m_testCodeRetriever.getFieldValue(data, entity, testCodeField);
                value.append(testCode);
            }
            return value.toString();
        }
    }

    protected class RetrieveSpedServices implements FieldRetriever {

        protected static final String CALC_ID = "SPED_SERV";

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
            String value = CODE_NO;
            AssessmentEntity pnpEntity = (AssessmentEntity) entity;
            SisStudent student = pnpEntity.getStudent();
            String spedSevices = (String) student.getFieldValueByBeanPath(m_studentSpedField);
            if (BooleanAsStringConverter.TRUE.equals(spedSevices)) {
                value = CODE_YES;
            }
            String activeCode = PreferenceManager.getPreferenceValue(student.getSchool(),
                    SisPreferenceConstants.SECTION_504_STUDENT_ACTIVE_CODE);
            if (activeCode != null && activeCode.equalsIgnoreCase(student.getSection504StatusCode())) {
                value = "2";
            }
            if (BooleanAsStringConverter.TRUE.equals(student.getFieldValueByBeanPath(m_studentSpedTo504Field))) {
                value = "3";
            }
            PlainDate endDate = (PlainDate) student.getFieldValueByBeanPath(m_studentSpedEndField);
            if (endDate != null) {
                value = "E";
            }
            if (!StringUtils.isEmpty(entity.getFieldValue(EXPORT_FIELD_DISABILITY_TYPE))) {
                value = CODE_YES;
            }
            return value;
        }
    }
    /**
     * Retriever for fields with Y/N values except fields concerning assessments.
     *
     */
    protected class RetrieveYN implements FieldRetriever {

        protected static final String CALC_ID = "YN_RETRIEVE";

        /**
         * Calculation parameters
         */
        protected static final String CALC_PARAM_ECON_DISADVAN_STATUS = "ECON_DISADVAN_STATUS";
        protected static final String CALC_PARAM_PR_DISAB_TYPE = "PR_DISAB_TYPE";
        protected static final String CALC_PARAM_EXEMPT_FROM_PASSING = "EXEMPT_FROM_PASSING";

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
            String value = CODE_NO;
            AssessmentEntity pnpEntity = (AssessmentEntity) entity;
            SisStudent student = pnpEntity.getStudent();
            String param = (String) field.getParameter();
            StudentAssessment asm = (StudentAssessment) pnpEntity.getBean();

            if (CALC_PARAM_ECON_DISADVAN_STATUS.equalsIgnoreCase(param)) {

                if ("F".equals(student.getFieldValueByAlias(ALIAS_DOE_FREE_REDUCED_LUNCH)) ||
                        "R".equals(student.getFieldValueByAlias(ALIAS_DOE_FREE_REDUCED_LUNCH))) {
                    value = CODE_YES;
                }
            } else if (CALC_PARAM_EXEMPT_FROM_PASSING.equalsIgnoreCase(param)) {
                String parccExpPass = null;
                String beanPath = ((MDAssessmentExportData) data).m_asmAlias.get(ALIAS_PARCC_PARCCEXPPASS);
                if (!StringUtils.isEmpty(beanPath)) {
                    parccExpPass = (String) asm.getFieldValueByBeanPath(beanPath);
                }
                if (BooleanAsStringConverter.TRUE.equals(parccExpPass)) {
                    value = CODE_YES;
                }
            }

            return value;
        }
    }

    /**
     * Validate division ID.
     */
    protected class ValidateSpedEndDate implements FieldValidator {
        protected static final String VAL_ID = "SPED-END-DATE-VAL";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String stdWithDisabilityValue = entity.getFieldValue(EXPORT_FIELD_STD_WITH_DISABILITY);
            if (value != null && !Arrays.asList("E", "3").contains(stdWithDisabilityValue)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Must be Blank if Student with Disability is not (E or 3).",
                        EXPORT_FIELD_STD_WITH_DISABILITY + "=" + STYLE_BOLD + stdWithDisabilityValue + STYLE_END + ", "
                                +
                                field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }
    }

    /*
     * Aliases
     */

    // CRS Table
    protected static final String ALIAS_CRS_DOE_LEVEL = "DOE COURSE LEVEL";

    // ASM Table
    protected static final String ALIAS_ASM_PARCC_TEST_CODE = "PARCCTSTCODE";

    // Export fields
    protected static final String EXPORT_FIELD_DISABILITY_TYPE = "PrDisabilityType";
    protected static final String EXPORT_FIELD_LEP_END_DATE = "LEPEndDate";
    protected static final String EXPORT_FIELD_STD_WITH_DISABILITY = "SpedServices";

    // SCHOOL Table
    protected static final String ALIAS_DOE_FREE_REDUCED_LUNCH = "DOE FREE REDUCED LUNCH";
    protected static final String ALIAS_DOE_SPECIAL_ED_CLASSIFICATION = "DOE SPECIAL ED CLASSIFICATION";
    protected static final String ALIAS_SCHOOL_CODE = "DOE SCHOOL CODE";

    // STUDENT Table
    protected static final String ALIAS_DISABILITY = "DOE DISABILITY";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_FEDERAL_PLACEMENT = "DOE FEDERAL PLACEMENT";
    protected static final String ALIAS_LUNCH_STATUS = "DOE FREE REDUCED LUNCH";
    protected static final String ALIAS_SPECIAL_ED_PLACEMENT = "DOE SPECIAL ED PLACEMENT";
    protected static final String ALIAS_SPECIAL_ED = "DOE special ed";
    protected static final String ALIAS_SPED_504 = "DOE SPED 504";
    protected static final String ALIAS_SPED_END = "DOE SPED END";
    protected static final String ALIAS_SPED_TO_504 = "DOE SPED TO 504";
    protected static final String ALIAS_TESTING_COUNTY = "DOE TESTING COUNTY";
    protected static final String ALIAS_ESL_FLAG = "ESL Flag";

    // STUDENTENROLLMENT Table
    protected static final String ALIAS_ATTENDING_SCHOOL = "DOE ATTENDING SCHOOL";

    /*
     * Reference codes aliases for the student program code
     */
    private static final String DOE_PROG_CODE_ELL = "DOE PR ELL";

    /**
     * Other internal constants
     */
    protected static final String CODE_ASSESSMENT_DEFINITION_ID_MD = "MD PARCC";
    protected static final String CODE_SPED_STATUS_ACTIVE = "Active";
    protected static final String ERROR_MESSAGE_STATE_CODE_NOT_DEFINED =
            " System State Code (sys.state) is not set in System Preferences";
    protected static final String ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS =
            "No MD PARCC Student Assessment's were created by the selected students.";
    protected static final String CODE_ONE = "1";
    protected static final String[] CODE_LUNCH_STATUS = {"F", "Free", "R", "Reduced"};
    protected static final String LEP_TEST_NOT_EXEMPT = "N";
    protected static final String LEP_TEST_EXEMPT = "Y";
    protected static final String INPUT_PARAM_ASM_DEFINITION = "asmDefinition";
    protected static final String INPUT_PARAM_TEST_CODE = "testCode";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_assessmentRetestField;
    protected String m_asmDefinitionID;
    protected String m_crsLevelField;
    protected String m_asmTestCodeField;
    protected Map<String, Collection<String>> m_iacsCodesMap;
    protected DataDictionaryField m_pgmCodeDDField;
    protected String m_schoolCodeField;
    protected int m_schoolYear;
    protected String m_studentDisabField;
    protected String m_studentSped504Field;
    protected String m_studentSpedEndField;
    protected String m_studentSpedField;
    protected String m_studentSpedTo504Field;
    protected FieldRetriever m_testCodeRetriever;
    private Set<String> m_testCodes = new HashSet();


    /**
     * Gets the heading.
     *
     * @return String
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
     * Initialize.
     *
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();

        super.initialize();
        loadIacCodesMap();

        if (getSetupErrors().size() == 0) {
            setEntityClass(MDPARCCAssessmentEntity.class);
            // Assign custom field retriever calculations.
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveLocation.CALC_ID, new RetrieveLocation());
            m_testCodeRetriever = new RetrieveAssessment();
            calcs.put(RetrieveAssessment.CALC_ID, m_testCodeRetriever);
            calcs.put(RetrieveCourseName.CALC_ID, new RetrieveCourseName());
            calcs.put(RetrieveSessionName.CALC_ID, new RetrieveSessionName());
            calcs.put(RetrievePrimaryDisab.CALC_ID, new RetrievePrimaryDisab());
            calcs.put(RetrieveFreeReducedLunch.CALC_ID, new RetrieveFreeReducedLunch());
            calcs.put(RetrieveIACValues.CALC_ID, new RetrieveIACValues());
            calcs.put(RetrieveSpedServices.CALC_ID, new RetrieveSpedServices());

            // Check Student Assessment count
            QueryByCriteria studentAssessmentQuery = getQuery();
            Collection<String> stdOids = getBroker()
                    .getGroupedCollectionByQuery(studentAssessmentQuery, StudentAssessment.COL_STUDENT_OID, 512)
                    .keySet();
            calcs.put("ASM-ELLIND", new RetrieveProgram(DOE_PROG_CODE_ELL, null, stdOids));
            calcs.put("ASM-ELL-START", new RetrieveProgram(DOE_PROG_CODE_ELL, "startDate", stdOids));
            calcs.put("ASM-ELL-END", new RetrieveProgram(DOE_PROG_CODE_ELL, "endDate", stdOids));
            Map<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(ValidateSpedEndDate.VAL_ID, new ValidateSpedEndDate());

            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Gets the asm definition id.
     *
     * @return String
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getAsmDefinitionId()
     */
    @Override
    protected String getAsmDefinitionId() {
        return m_asmDefinitionID;
    }

    /**
     * Gets the no student assessment message.
     *
     * @return String
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
        m_crsLevelField = translateAliasToJavaName(ALIAS_CRS_DOE_LEVEL, true);
        m_schoolCodeField = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
        m_studentDisabField = translateAliasToJavaName(ALIAS_DISABILITY, true);
        m_studentSped504Field = translateAliasToJavaName(ALIAS_SPED_504, true);
        m_studentSpedField = translateAliasToJavaName(ALIAS_SPECIAL_ED, true);
        m_studentSpedTo504Field = translateAliasToJavaName(ALIAS_SPED_TO_504, true);
        m_studentSpedEndField = translateAliasToJavaName(ALIAS_SPED_END, true);
        String asmDefinitionOid = (String) getParameter(INPUT_PARAM_ASM_DEFINITION);
        AssessmentDefinition asmDefinition =
                (AssessmentDefinition) getBroker().getBeanByOid(AssessmentDefinition.class, asmDefinitionOid);
        m_asmDefinitionID = asmDefinition.getId();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_pgmCodeDDField = dictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                StudentProgramParticipation.COL_PROGRAM_CODE);
        DataDictionary extDictionary =
                DataDictionary.getDistrictDictionary(asmDefinition, asmDefinition.getPersistenceKey());
        DataDictionaryField field = extDictionary.findDataDictionaryFieldByAlias(ALIAS_ASM_PARCC_TEST_CODE);
        if (field == null) {
            addSetupError("Assessment Definition " + asmDefinition.getId() + " alias not found",
                    ALIAS_ASM_PARCC_TEST_CODE);
        } else {
            m_asmTestCodeField = field.getJavaName();
        }
        String testCodeOids = (String) getParameter(INPUT_PARAM_TEST_CODE);
        if (!StringUtils.isEmpty(testCodeOids)) {
            for (String codeOid : testCodeOids.split(",")) {
                ReferenceCode code = getBroker().getBeanByOid(ReferenceCode.class, codeOid);
                if (code != null) {
                    m_testCodes.add(code.getCode());
                }
            }
        }

    }

    /**
     * Load map with current accommodations keyed on stdOid.
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
