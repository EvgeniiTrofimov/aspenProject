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

package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentEdPlan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * MA Assessment export for PARCC - Personal Needs Profile export.
 *
 * @author X2 Development Corporation
 */
public class MAMcasPnpExportData extends StateReportData {

    /**
     * The Class MAMcasPnpEntity.
     */
    public static class MAMcasPnpEntity extends StateReportEntity {

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
            MAMcasPnpExportData mcasData = (MAMcasPnpExportData) data;
            StudentAssessment assessment = (StudentAssessment) bean;

            String testContextValue = (String) assessment.getFieldValueByAliasExtended(ALIAS_MCAS_YEAR,
                    mcasData.m_dataDictionary);
            String testPeriod =
                    (String) assessment.getFieldValueByAliasExtended(ALIAS_MCAS_CYCLE, mcasData.m_dataDictionary);
            String asmSchoolOid = assessment.getSchoolOid();

            boolean exportAssessment = mcasData.m_cycle.equals(testPeriod) &&
                    mcasData.m_ctxForMcasValue.equals(testContextValue) &&
                    (!mcasData.isSchoolContext() || mcasData.getSchool().getOid().equals(asmSchoolOid));

            if (!exportAssessment) {
                setRowCount(0);
            }

            super.intitialize(data, bean);
        }
    }

    /**
     * The Class RetrieveAssessment.
     */
    /*
     * Get Student Assessment field
     */
    protected class RetrieveAssessment implements FieldRetriever {

        protected static final String CALC_ID = "MCAS-TEST";

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
            MAMcasPnpEntity mcasEntity = (MAMcasPnpEntity) entity;
            Object value = null;

            StudentAssessment studentAssessment = (StudentAssessment) mcasEntity.getBean();
            String alias = (String) field.getParameter();
            String beanPath = ((MAMcasPnpExportData) data).m_mcasAlias.get(alias);
            if (!StringUtils.isEmpty(beanPath)) {
                if (StringUtils.isEqual(alias, ALIAS_MCAS_REMOTE_ADMIN)
                        || StringUtils.isEqual(alias, ALIAS_MCAS_MED_ABS)) {
                    String remoteAdmin = (String) studentAssessment.getFieldValueByBeanPath(beanPath);
                    return (StringUtils.isEqual(remoteAdmin, BooleanAsStringConverter.TRUE)) ? "Y" : "";
                }
                value = studentAssessment.getFieldValueByBeanPath(beanPath);
            }
            return value;
        }
    }

    /**
     * The Class RetrieveDistrictSchool.
     */
    /*
     * Get test id with state abbreviation prefix
     */
    protected class RetrieveDistrictSchool implements FieldRetriever {
        protected static final String CALC_ID = "CALC_TEST_ID";

        private static final String PARAM_DISTRICT = "DISTRICT";
        private static final String PARAM_SCHOOL = "SCHOOL";

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
            MAMcasPnpEntity mcasEntity = (MAMcasPnpEntity) entity;

            StudentAssessment studentAssessment = (StudentAssessment) mcasEntity.getBean();
            SisSchool school = studentAssessment.getSchool();
            String testId = null;

            if (school != null) {
                if (PARAM_DISTRICT.equals(field.getParameter())) {
                    testId = (String) school.getFieldValueByBeanPath(m_testDistrictIdField);
                } else if (PARAM_SCHOOL.equals(field.getParameter())) {
                    testId = (String) school.getFieldValueByBeanPath(m_testSchoolIdField);
                }
            }
            return testId;
        }
    }

    /**
     * The Class RetrieveIACValues.
     */
    /*
     * Get values for fields concerning Student Accomodations
     */
    protected class RetrieveIACValues implements FieldRetriever {
        protected final List VALUE_STD_DISAB = Arrays.asList(new String[] {"IDEA", "504"});

        /**
         * CALC ID
         */
        protected static final String CALC_ID = "ACCOMODATIONS";

        /**
         * Values
         */
        protected static final String VALUE_Y = "Y";

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
            MAMcasPnpEntity mcasEntity = (MAMcasPnpEntity) entity;
            String value = null;
            StudentAssessment studentAssessment = (StudentAssessment) mcasEntity.getBean();
            String stdOId = studentAssessment.getStudentOid();
            Collection<String> stdIacCodes = m_iacsCodesMap.get(stdOId);
            String testCode = entity.getFieldValue(FIELD_TEST_CODE);
            String testFormat = entity.getFieldValue(FIELD_TEST_FORMAT);
            String paramString = (String) field.getParameter();

            if (stdIacCodes != null && !stdIacCodes.isEmpty()) {
                if (CALC_PARAM_ALT_CURSOR_POINTER.equals(paramString)) {
                    if (!StringUtils.isEmpty(testFormat) && TEST_FORMAT_ONLINE.equals(testFormat)) {
                        if (stdIacCodes.contains("UF4")) {
                            if (m_iacsCodesMapHelper.containsKey(stdOId + "UF4")) {
                                value = m_iacsCodesMapHelper.get(stdOId + "UF4");
                            } else {
                                value = VALUE_Y;
                            }
                        }
                    }
                } else if (CALC_PARAM_LARGE_PRINT_TEST.equals(paramString)) {
                    if (!StringUtils.isEmpty(testFormat) && TEST_FORMAT_PRINTED.equals(testFormat)) {
                        if (stdIacCodes.contains("A2")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_SCREEN_READ.equals(paramString)) {
                    if (!StringUtils.isEmpty(testFormat) && TEST_FORMAT_ONLINE.equals(testFormat)) {
                        if (stdIacCodes.contains("A3.1")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_ASSITIVE_TECH.equals(paramString)) {
                    value = "";
                } else if (CALC_PARAM_BRAILLE_TEST_ED.equals(paramString)) {
                    if (!StringUtils.isEmpty(testFormat) && TEST_FORMAT_PRINTED.equals(testFormat)) {
                        if (stdIacCodes.contains("A3.2") || stdIacCodes.contains("12")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_READ_ALOUD_STANDARD.equals(paramString) && !testCode.contains("ELA")
                        && !Arrays.asList("ENRNG", "EMRNG").contains(testCode)) {
                    if (stdIacCodes.contains("A5") || stdIacCodes.contains("EL3.2") ||
                            stdIacCodes.contains("16")) {
                        value = VALUE_Y;
                    }
                } else if (CALC_PARAM_READ_ALOUD.equals(paramString) && !testCode.contains("MAT")
                        && !Arrays
                                .asList("MNRNG", "MMRNG", "BIOFE", "PHYFE", "BIOSP", "PHYSP", "SCI05",
                                        "SCI08")
                                .contains(testCode)) {
                    if (!StringUtils.isEmpty(testFormat)
                            && (TEST_FORMAT_PRINTED.equals(testFormat) || (TEST_FORMAT_ONLINE.equals(testFormat)))) {
                        if (stdIacCodes.contains("SA1.2") || stdIacCodes.contains("26")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_HUM_SIGN_ST.equals(paramString) && !testCode.contains("ELA")
                        && !Arrays.asList("ENRNG", "EMRNG").contains(testCode)) {
                    if (!StringUtils.isEmpty(testFormat)
                            && (TEST_FORMAT_PRINTED.equals(testFormat) || (TEST_FORMAT_ONLINE.equals(testFormat)))) {
                        if (stdIacCodes.contains("A6") || stdIacCodes.contains("A6.1")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_HUM_SIGN_ST_NON.equals(paramString) && !testCode.contains("MAT")
                        && !Arrays
                                .asList("MNRNG", "MMRNG", "BIOFE", "PHYFE", "BIOSP", "PHYSP", "SCI05",
                                        "SCI08")
                                .contains(testCode)) {
                    if (!StringUtils.isEmpty(testFormat)
                            && (TEST_FORMAT_PRINTED.equals(testFormat) || (TEST_FORMAT_ONLINE.equals(testFormat)))) {
                        if (stdIacCodes.contains("SA2")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_TEXT_SPEECH.equals(paramString)) {
                    if ((testCode.contains("MAT")
                            || testCode.contains("SCI")
                            || testCode.contains("BIO")
                            || testCode.contains("PHY")
                            || Arrays.asList("MMRNG", "MNRNG").contains(testCode))
                            && (stdIacCodes.contains("EL3.1") || stdIacCodes.contains("A4")
                                    || stdIacCodes.contains("A4.1"))) {
                        value = VALUE_Y;
                    }
                    if ((testCode.contains("ELA") || Arrays.asList("EMRNG",
                            "ENRNG").contains(testCode))
                            && (stdIacCodes.contains("SA1.1"))) {
                        value = VALUE_Y;
                    }
                } else if (CALC_PARAM_ASL_VIDEO.equals(paramString) && !testCode.contains("ELA")
                        && !Arrays
                                .asList("MAT03", "MAT04", "MAT05", "MAT06", "MAT07", "MAT08", "ENRNG", "EMRNG", "MNRNG",
                                        "MMRNG", "SCI05", "SCI08")
                                .contains(testCode)) {
                    if (!StringUtils.isEmpty(testFormat) && TEST_FORMAT_ONLINE.equals(testFormat)) {
                        if (stdIacCodes.contains("A6.2")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_HUM_SCRIBE_ST.equals(paramString) && !testCode.contains("ELA")
                        && !Arrays.asList("ENRNG", "EMRNG")
                                .contains(testCode)) {
                    if (!StringUtils.isEmpty(testFormat)
                            && (TEST_FORMAT_PRINTED.equals(testFormat) || (TEST_FORMAT_ONLINE.equals(testFormat)))) {
                        if (stdIacCodes.contains("A10.1") || stdIacCodes.contains("EL4.1")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_HUM_SCRIBE_ST_NON.equals(paramString) && !testCode.contains("MAT")
                        && !Arrays
                                .asList("MNRNG", "MMRNG", "BIOFE", "PHYFE", "BIOSP", "PHYSP", "SCI05", "SCI08")
                                .contains(testCode)) {
                    if (!StringUtils.isEmpty(testFormat)
                            && (TEST_FORMAT_PRINTED.equals(testFormat) || (TEST_FORMAT_ONLINE.equals(testFormat)))) {
                        if (stdIacCodes.contains("SA3.1")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_SPEECH_TEXT_ST.equals(paramString) && !testCode.contains("ELA")
                        && !Arrays.asList("ENRNG", "EMRNG")
                                .contains(testCode)) {
                    if (!StringUtils.isEmpty(testFormat)
                            && (TEST_FORMAT_PRINTED.equals(testFormat) || (TEST_FORMAT_ONLINE.equals(testFormat)))) {
                        if (stdIacCodes.contains("A10.2") || stdIacCodes.contains("EL4.2")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_SPEECH_TEXT_ST_NON.equals(paramString)
                        && !testCode.contains("MAT")
                        && !Arrays
                                .asList("MNRNG", "MMRNG", "BIOFE", "PHYFE", "BIOSP", "PHYSP", "SCI05", "SCI08")
                                .contains(testCode)) {
                    if (!StringUtils.isEmpty(testFormat)
                            && (TEST_FORMAT_PRINTED.equals(testFormat) || (TEST_FORMAT_ONLINE.equals(testFormat)))) {
                        if (stdIacCodes.contains("SA3.2")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_TYPE_RESP.equals(paramString)) {
                    if (!StringUtils.isEmpty(testFormat) && TEST_FORMAT_PRINTED.equals(testFormat)) {
                        if (stdIacCodes.contains("A12")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_CALC_DEVICE.equals(paramString) && !testCode.contains("ELA") && !Arrays
                        .asList("ENRNG", "EMRNG", "PHYFE", "BIOSP", "BIOFE", "PHYSP", "SCI05", "SCI08")
                        .contains(testCode)) {
                    if (!StringUtils.isEmpty(testFormat)
                            && (TEST_FORMAT_PRINTED.equals(testFormat) || (TEST_FORMAT_ONLINE.equals(testFormat)))) {
                        if (stdIacCodes.contains("SA4")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_SPELL_CHECK.equals(paramString) && !testCode.contains("MAT") && !Arrays
                        .asList("BIOFE", "PHYFE", "BIOSP", "PHYSP", "SCI05", "SCI08", "MNRNG", "MMRNG")
                        .contains(testCode)) {
                    if (!StringUtils.isEmpty(testFormat)
                            && (TEST_FORMAT_PRINTED.equals(testFormat) || (TEST_FORMAT_ONLINE.equals(testFormat)))) {
                        if (stdIacCodes.contains("SA5")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_WORD_PREDICT.equals(paramString) && !testCode.contains("MAT") && !Arrays
                        .asList("PHYFE", "MMRNG", "PHYSP", "MNRNG")
                        .contains(testCode)) {
                    if (!StringUtils.isEmpty(testFormat)
                            && (TEST_FORMAT_PRINTED.equals(testFormat) || (TEST_FORMAT_ONLINE.equals(testFormat)))) {
                        if (stdIacCodes.contains("SA6")) {
                            value = VALUE_Y;
                        }
                    }
                } else if (CALC_PARAM_ENGLISH_SPANISH.equals(paramString)) {
                    if (stdIacCodes.contains("EL7")) {
                        value = VALUE_Y;
                    }
                } else if (CALC_PARAM_GRAPH_ORG.equals(paramString)) {
                    if (!StringUtils.isEmpty(testFormat)
                            && (TEST_FORMAT_PRINTED.equals(testFormat) || (TEST_FORMAT_ONLINE.equals(testFormat)))) {
                        if (stdIacCodes.contains("A9")) {
                            value = VALUE_Y;
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * The Class RetrieveMiddleInitial.
     */
    protected class RetrieveMiddleInitial implements FieldRetriever {
        protected static final String CALC_ID = "MCAS-MI";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = ((StudentAssessment) entity.getBean()).getStudent().getPerson().getMiddleName();
            return StringUtils.isEmpty(value) ? value : value.substring(0, 1);
        }
    }


    protected class RetrieveStudent implements FieldRetriever {
        protected static final String CALC_ID = "MCAS-STD";
        protected static final String CALC_PARAM_EL = "1ST_YEAR_EL";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = "";
            String param = (String) field.getParameter();
            SisStudent std = ((StudentAssessment) entity.getBean()).getStudent();
            if (CALC_PARAM_EL.equals(param)) {
                String firstYearEL = (String) std.getFieldValueByBeanPath(m_stdFieldFirstYearEL);
                if (!StringUtils.isEmpty(firstYearEL)
                        && "01".equals(lookupStateValue(SisStudent.class, m_stdFieldFirstYearEL, firstYearEL))) {
                    value = "Y";
                }
            }
            return value;
        }
    }


    /**
     * Validations to accommodation fields.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateIACValues implements FieldValidator {
        protected static final String VAL_ERROR_MSG1 = "If the accomodation code reference code equals ";
        protected static final String VAL_ERROR_MSG2 = "The test format must be ";
        protected static final String VAL_ID = "VAL-IAC";
        protected static final String VALUE_Y = "Y";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            MAMcasPnpEntity mcasEntity = (MAMcasPnpEntity) entity;
            StudentAssessment studentAssessment = (StudentAssessment) mcasEntity.getBean();
            String stdOId = studentAssessment.getStudentOid();
            Collection<String> stdIacCodes = m_iacsCodesMap.get(stdOId);
            String testFormat = entity.getFieldValue(FIELD_TEST_FORMAT);
            String paramString = (String) field.getParameter();

            if (value.equals(VALUE_Y)) {
                if (stdIacCodes != null && !stdIacCodes.isEmpty()) {
                    if (CALC_PARAM_ALT_CURSOR_POINTER.equals(paramString)) {
                        if (stdIacCodes.contains("UF4")) {
                            if (!StringUtils.isEmpty(testFormat) && !TEST_FORMAT_ONLINE.equals(testFormat)) {
                                errors.add(new StateReportValidationError(entity, field,
                                        VAL_ERROR_MSG1 + paramString, VAL_ERROR_MSG2 + TEST_FORMAT_ONLINE));
                            }
                        }
                    } else if (CALC_PARAM_LARGE_PRINT_TEST.equals(paramString)) {
                        if (stdIacCodes.contains("A2")) {
                            if (!StringUtils.isEmpty(testFormat) && !TEST_FORMAT_PRINTED.equals(testFormat)) {
                                errors.add(new StateReportValidationError(entity, field,
                                        VAL_ERROR_MSG1 + paramString, VAL_ERROR_MSG2 + TEST_FORMAT_PRINTED));
                            }
                        }
                    } else if (CALC_PARAM_SCREEN_READ.equals(paramString)) {
                        if (stdIacCodes.contains("A3.1")) {
                            if (!StringUtils.isEmpty(testFormat) && !TEST_FORMAT_ONLINE.equals(testFormat)) {
                                errors.add(new StateReportValidationError(entity, field,
                                        VAL_ERROR_MSG1 + paramString, VAL_ERROR_MSG2 + TEST_FORMAT_ONLINE));
                            }
                        }
                    } else if (CALC_PARAM_ASSITIVE_TECH.equals(paramString)) {
                        if (!StringUtils.isEmpty(testFormat) && TEST_FORMAT_ONLINE.equals(testFormat)) {
                            if (stdIacCodes.contains("A10.2") || stdIacCodes.contains("SA6")
                                    || stdIacCodes.contains("EL4.2") ||
                                    stdIacCodes.contains("SA3.2")) {
                                if (!StringUtils.isEmpty(testFormat) && !TEST_FORMAT_ONLINE.equals(testFormat)) {
                                    errors.add(new StateReportValidationError(entity, field,
                                            VAL_ERROR_MSG1 + paramString, VAL_ERROR_MSG2 + TEST_FORMAT_ONLINE));
                                }
                            }
                        }
                    } else if (CALC_PARAM_BRAILLE_TEST_ED.equals(paramString)) {
                        if (stdIacCodes.contains("A3.2") || stdIacCodes.contains("12")) {
                            if (!StringUtils.isEmpty(testFormat) && !TEST_FORMAT_PRINTED.equals(testFormat)) {
                                errors.add(new StateReportValidationError(entity, field,
                                        VAL_ERROR_MSG1 + paramString, VAL_ERROR_MSG2 + TEST_FORMAT_PRINTED));
                            }
                        }
                    } else if (CALC_PARAM_READ_ALOUD_STANDARD.equals(paramString)) {
                        if (stdIacCodes.contains("A5") || stdIacCodes.contains("EL3.2") ||
                                stdIacCodes.contains("16")) {
                            if (!StringUtils.isEmpty(testFormat) && !TEST_FORMAT_ONLINE.equals(testFormat)) {
                                errors.add(new StateReportValidationError(entity, field,
                                        VAL_ERROR_MSG1 + paramString, VAL_ERROR_MSG2 + TEST_FORMAT_ONLINE));
                            }
                        }
                    } else if (CALC_PARAM_TEXT_SPEECH.equals(paramString)) {
                        if (stdIacCodes.contains("A4.1") || stdIacCodes.contains("A4") ||
                                stdIacCodes.contains("EL3.1") || stdIacCodes.contains("SA1.1")) {
                            if (!StringUtils.isEmpty(testFormat) && !TEST_FORMAT_ONLINE.equals(testFormat)) {
                                errors.add(new StateReportValidationError(entity, field,
                                        VAL_ERROR_MSG1 + paramString, VAL_ERROR_MSG2 + TEST_FORMAT_ONLINE));
                            }
                        }
                    } else if (CALC_PARAM_ASL_VIDEO.equals(paramString)) {
                        if (stdIacCodes.contains("A6.2")) {
                            if (!StringUtils.isEmpty(testFormat) && !TEST_FORMAT_ONLINE.equals(testFormat)) {
                                errors.add(new StateReportValidationError(entity, field,
                                        VAL_ERROR_MSG1 + paramString, VAL_ERROR_MSG2 + TEST_FORMAT_ONLINE));
                            }
                        }
                    } else if (CALC_PARAM_TYPE_RESP.equals(paramString)) {
                        if (stdIacCodes.contains("A12")) {
                            if (!StringUtils.isEmpty(testFormat) && !TEST_FORMAT_PRINTED.equals(testFormat)) {
                                errors.add(new StateReportValidationError(entity, field,
                                        VAL_ERROR_MSG1 + paramString, VAL_ERROR_MSG2 + TEST_FORMAT_PRINTED));
                            }
                        }
                    }
                }
            }

            return errors;
        }
    }

    // STUDENT Table
    protected static final String ALIAS_ADJUSTED_DISTR = "DOE ADJUSTED DISTRICT";
    protected static final String ALIAS_DDX_504_PLAN_NAME = "504-acc-name";
    protected static final String ALIAS_MCAS_CYCLE = "MCASTestCycle";
    protected static final String ALIAS_MCAS_EXCLUSION = "MCASTestExclusion";
    protected static final String ALIAS_MCAS_MED_ABS = "MCASMedAbs";
    protected static final String ALIAS_MCAS_REMOTE_ADMIN = "MCASRemoteAdmin";
    protected static final String ALIAS_MCAS_SESSION_NAME = "MCASSessionName";
    protected static final String ALIAS_MCAS_TSTCODE = "MCASTestCode";
    protected static final String ALIAS_MCAS_TSTFORMAT = "MCASTestFormat";
    protected static final String ALIAS_MCAS_YEAR = "MCASTestYear";
    protected static final String ALIAS_STD_FIRST_YEAR_EL = "DOE 21";
    protected static final String ALIAS_TEST_DISTRICT_ID = "skl-sif-district-id";
    protected static final String ALIAS_TEST_SCHOOL_ID = "DOE 15";

    /*
     * Input Definition Parameters
     */
    protected static final String PARAM_CTX = "schoolYearContext";
    protected static final String PARAM_CTX_BEAN_PATH = "ctxBeanPath";
    protected static final String PARAM_CYCLE = "cycle";
    protected static final String PARAM_INCLUDE_DRAFTS = "includeDrafts";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_REPORT_END_DATE = "endDate";
    protected static final String PARAM_REPORT_START_DATE = "startDate";
    protected static final String PARAM_SORT_PARAM = "sort";

    /**
     * Errors
     */
    protected static final String ERROR_MESSAGE_ASSESS_DEF_ALIAS = " Assessment Definition Alias: ";
    protected static final String ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED =
            " Assessment Definition is not loaded in the Data Dictionary.";
    protected static final String ERROR_MESSAGE_IS_NOT_DEFINED = " is not defined";
    protected static final String ERROR_MESSAGE_NO_ACTIVE_STUDENTS =
            "No students were active in the previous School Year.";
    protected static final String ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS =
            "No MA MCAS Student Assessment records were created for the selected students.";
    protected static final String ERROR_TYPE_WARNING = "Warning";

    /**
     * Other internal constants
     */
    protected static final String CODE_ASSESSMENT_DEFINITION_ID = "MA MCAS";
    protected static final String DDX_ID_504_PLAN = "STD-504-PLAN";
    protected static final String TEST_FORMAT_ONLINE = "O";
    protected static final String TEST_FORMAT_PRINTED = "P";

    /**
     * Export fields
     */
    protected static final String FIELD_ASL_VIDEO = "ASLVideo";
    protected static final String FIELD_ALT_COLOR = "AltColor";
    protected static final String FIELD_ANSWER_MASKING = "AnswerMasking";
    protected static final String FIELD_ASS_TECH = "AssitiveTech";
    protected static final String FIELD_BRAILLE_TEST_ED = "BrailleTestEd";
    protected static final String FIELD_HUM_READ_ALOUD = "HumanReadAloud";
    protected static final String FIELD_HUM_READ_ALOUD_ST = "HumanReadAloudSt";
    protected static final String FIELD_HUM_SCRIBE_ST = "HumScrStandard";
    protected static final String FIELD_HUM_SIGNER = "HumanSigner";
    protected static final String FIELD_KURZWEIL_SPED = "KurzweilSPED";
    protected static final String FIELD_LARGE_PRINT_ED = "LargePrintTestEd";
    protected static final String FIELD_SCREEN_READER = "ScreenReader";
    protected static final String FIELD_TEST_CODE = "TestCode";
    protected static final String FIELD_TEST_FORMAT = "TestFormat";
    protected static final String FIELD_TEXT_SPEECH = "TextToSpeech";

    /**
     * Test codes
     */
    protected static final String[] GROUP1_TEST_CODES = {"MAT03", "MAT04", "MAT05", "MAT06",
            "MAT07", "MAT08", "MAT10", "MATNR", "MATMR", "MATHS", "SCI05", "SCI08", "PHSY9", "BIOL9", "BIOHS"};
    protected static final String[] GROUP2_TEST_CODES =
            {"ELA03", "ELA04", "ELA05", "ELA06", "ELA07", "ELA08", "ELA10", "ELANR", "ELAMR", "ELAHS"};

    /**
     * CALC PARAMS
     */
    protected static final String CALC_PARAM_ALT_CURSOR_POINTER = "UF4";
    protected static final String CALC_PARAM_ASL_VIDEO = "A6.2";
    protected static final String CALC_PARAM_ASSITIVE_TECH = "A3.3;A10.2;SA6;EL4.2";
    protected static final String CALC_PARAM_BRAILLE_TEST_ED = "A3.2;12";
    protected static final String CALC_PARAM_CALC_DEVICE = "SA4";
    protected static final String CALC_PARAM_GRAPH_ORG = "A9";
    protected static final String CALC_PARAM_READ_ALOUD = "SA1.2;26";
    protected static final String CALC_PARAM_READ_ALOUD_STANDARD = "A5;EL3.2;16";
    protected static final String CALC_PARAM_HUM_SIGN_ST = "A6;A6.1";
    protected static final String CALC_PARAM_HUM_SIGN_ST_NON = "SA2";
    protected static final String CALC_PARAM_HUM_SCRIBE_ST = "A10.1;EL4.1";
    protected static final String CALC_PARAM_HUM_SCRIBE_ST_NON = "SA3.1";
    protected static final String CALC_PARAM_LARGE_PRINT_TEST = "A2";
    protected static final String CALC_PARAM_SCREEN_READ = "A3.1";
    protected static final String CALC_PARAM_SPEECH_TEXT_ST = "A10.2;EL4.2";
    protected static final String CALC_PARAM_SPEECH_TEXT_ST_NON = "SA3.2";
    protected static final String CALC_PARAM_SPELL_CHECK = "SA5";
    protected static final String CALC_PARAM_TEXT_SPEECH = "A4.1;A4;EL3.1;SA1.1";
    protected static final String CALC_PARAM_TYPE_RESP = "A12";
    protected static final String CALC_PARAM_WORD_PREDICT = "SA6";
    protected static final String CALC_PARAM_ENGLISH_SPANISH = "EL7";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    public PlainDate m_endDate;
    public StudentHistoryHelper m_helper;
    public Map<String, String> m_mcasAlias = new HashMap();
    protected String m_ctxForMcasValue;
    protected String m_cycle;
    protected DataDictionary m_dataDictionary;
    protected Map<String, Collection<String>> m_iacsCodesMap;
    protected Map<String, String> m_iacsCodesMapHelper = new HashMap<>();
    protected Boolean m_includeDrafts;
    protected DataDictionary m_ddx504Plan;
    protected String m_ddxField504PlanName;
    protected Map<String, ReferenceCode> m_ddxField504PlanNameCodeMap;
    protected Boolean m_removeHeader;
    protected PlainDate m_startDate;
    protected String m_stdFieldFirstYearEL;
    protected String m_testDistrictIdField;
    protected String m_testSchoolIdField;

    /**
     * Gets the heading.
     *
     * @return String
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
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();

        m_startDate = (PlainDate) getParameter(PARAM_REPORT_START_DATE);
        if (m_startDate == null) {
            m_startDate = getOrganization().getCurrentContext().getStartDate();
        }

        // Load Input Definition Parameters
        m_endDate = (PlainDate) getParameter(PARAM_REPORT_END_DATE);
        if (m_endDate == null) {
            m_endDate = new PlainDate();
        }
        m_removeHeader = (Boolean) getParameter(PARAM_REMOVE_HEADER);
        if (m_removeHeader == null) {
            m_removeHeader = Boolean.valueOf(false);
        }

        m_includeDrafts = (Boolean) getParameter(PARAM_INCLUDE_DRAFTS);
        if (m_includeDrafts == null) {
            m_includeDrafts = Boolean.valueOf(false);
        }

        DistrictSchoolYearContext ctx =
                (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                        (String) getParameter(PARAM_CTX));
        m_ctxForMcasValue = (String) ctx.getFieldValueByBeanPath((String) getParameter(PARAM_CTX_BEAN_PATH));

        m_cycle = (String) getParameter(PARAM_CYCLE);

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

        QueryByCriteria stdQueryBy = m_helper.getStudentQuery(true);
        int studentCount = getBroker().getCount(stdQueryBy);
        if (studentCount == 0) {
            addSetupError(ERROR_TYPE_WARNING, ERROR_MESSAGE_NO_ACTIVE_STUDENTS);
        }

        loadMCASAssessmentDefinition();
        loadAccommodationsMap();

        // Check Student Assessment count
        X2Criteria studentAssessmentCriteria = getStudentAssessmentCriteria();
        QueryByCriteria studentAssessmentQuery = m_helper.getStudentSelectionQuery(StudentAssessment.class,
                studentAssessmentCriteria,
                StudentAssessment.COL_STUDENT_OID);

        int studentAssessmentCount = getBroker().getCount(studentAssessmentQuery);
        if (studentAssessmentCount == 0) {
            addSetupError(ERROR_TYPE_WARNING, getNoStudentAssessmentMessage());
        }

        if (getSetupErrors().size() == 0) {
            // Assign the custom entity class, if there is one.
            setQuery(studentAssessmentQuery);
            setEntityClass(MAMcasPnpEntity.class);
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveAssessment.CALC_ID, new RetrieveAssessment());
            calcs.put(RetrieveIACValues.CALC_ID, new RetrieveIACValues());
            calcs.put(RetrieveDistrictSchool.CALC_ID, new RetrieveDistrictSchool());
            calcs.put(RetrieveMiddleInitial.CALC_ID, new RetrieveMiddleInitial());
            calcs.put(RetrieveStudent.CALC_ID, new RetrieveStudent());
            super.addCalcs(calcs);
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(ValidateIACValues.VAL_ID, new ValidateIACValues());
            super.addValidators(validators);
        }
    }

    /**
     * This method returns Assessment Definition ID.
     *
     * @return String
     */
    protected String getAsmDefinitionId() {
        return CODE_ASSESSMENT_DEFINITION_ID;
    }

    /**
     * This method returns message informing that needed Student Assessments weren't created.
     *
     * @return String
     */
    protected String getNoStudentAssessmentMessage() {
        return ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS;
    }

    /**
     * initialize Fields.
     */
    private void initializeFields() {
        // Load Alias database UDF names
        m_testDistrictIdField = translateAliasToJavaName(ALIAS_TEST_DISTRICT_ID, true);
        m_testSchoolIdField = translateAliasToJavaName(ALIAS_TEST_SCHOOL_ID, true);
        m_stdFieldFirstYearEL = translateAliasToJavaName(ALIAS_STD_FIRST_YEAR_EL, true);
        X2Criteria ddxCriteria = new X2Criteria();
        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, "STD-504-PLAN");
        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        m_ddx504Plan = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
        DataDictionaryField ddxDictField = m_ddx504Plan.findDataDictionaryFieldByAlias(ALIAS_DDX_504_PLAN_NAME);
        if (ddxDictField != null && ddxDictField.hasReferenceTable()) {
            m_ddxField504PlanName = m_ddx504Plan.findDataDictionaryFieldByAlias(ALIAS_DDX_504_PLAN_NAME).getJavaName();
            m_ddxField504PlanNameCodeMap = ddxDictField.getReferenceTable().getCodeMap();
        }
    }

    /**
     * Returns an X2Criteria for the Student Accommodation records that link to an IEP or 504
     * Student Ed Plan which was active as of the date ran.
     *
     * @return X2Criteria
     */
    private X2Criteria getAccommodationCriteria() {
        X2Criteria activeIepCriteria = new X2Criteria();
        List<Integer> statusCodes = m_includeDrafts.booleanValue() ? Arrays.asList(
                Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()),
                Integer.valueOf(IepData.StatusCode.DRAFT.ordinal()))
                : Arrays.asList(
                        Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));

        activeIepCriteria.addIn(IepData.COL_STATUS_CODE, statusCodes);
        activeIepCriteria.addNotNull(IepData.COL_START_DATE);
        activeIepCriteria.addLessOrEqualThan(IepData.COL_START_DATE, new PlainDate());
        QueryByCriteria iepsQuery = new QueryByCriteria(IepData.class, activeIepCriteria);
        iepsQuery.addOrderByDescending(IepData.COL_START_DATE);
        Collection<String> ieps = new ArrayList<>();
        Map<String, Collection<IepData>> iepDataByStdOId =
                getBroker().getGroupedCollectionByQuery(iepsQuery, IepData.COL_STUDENT_OID, 1024);
        for (Entry<String, Collection<IepData>> entry : iepDataByStdOId.entrySet()) {
            ieps.add(entry.getValue().iterator().next().getOid());
        }

        X2Criteria active504EdPlanCriteria = new X2Criteria();
        // active504EdPlanCriteria.addEqualTo(StudentEdPlan.COL_EXTENDED_DATA_DICTIONARY_OID,
        // "ddxStandard504");
        active504EdPlanCriteria.addIn(StudentEdPlan.COL_STATUS_CODE, Arrays.asList(
                Integer.valueOf(StudentEdPlan.StatusCode.ACTIVE.ordinal()),
                Integer.valueOf(StudentEdPlan.StatusCode.PREVIOUS.ordinal())));
        active504EdPlanCriteria.addLessOrEqualThan(StudentEdPlan.COL_EFFECTIVE_DATE, new PlainDate());
        active504EdPlanCriteria.addGreaterOrEqualThan(StudentEdPlan.COL_END_DATE, new PlainDate());

        SubQuery active504s = new SubQuery(StudentEdPlan.class, X2BaseBean.COL_OID, active504EdPlanCriteria);

        X2Criteria active504Criteria = new X2Criteria();
        active504Criteria.addIn(IepAccommodation.COL_STUDENT_ED_PLAN_OID, active504s);

        X2Criteria activeCriteria = new X2Criteria();
        activeCriteria.addIn(IepAccommodation.COL_IEP_DATA_OID, ieps);
        activeCriteria.addOrCriteria(active504Criteria);
        activeCriteria.addOrCriteria(getGeneralAccommodationCriteria());

        X2Criteria iacCriteria = new X2Criteria();
        iacCriteria.addNotNull(IepAccommodation.COL_NAME);
        iacCriteria.addAndCriteria(activeCriteria);

        return iacCriteria;
    }

    /**
     * Get Field Java Name according given alias.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return String
     */
    private String getAsmJavaName(String alias, DataDictionary dataDictionary) {
        String javaName = null;

        DataDictionaryField dictField = dataDictionary.findDataDictionaryFieldByAlias(alias);
        if (dictField != null && dataDictionary.containsAlias(alias)) {
            javaName = dictField.getJavaName();
        }

        return javaName;
    }

    /**
     * Gets the general accommodation criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getGeneralAccommodationCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEmpty(IepAccommodation.COL_STUDENT_ED_PLAN_OID, getBroker().getPersistenceKey());
        criteria.addEmpty(IepAccommodation.COL_IEP_DATA_OID, getBroker().getPersistenceKey());
        criteria.addGreaterOrEqualThan(IepAccommodation.COL_IMPLEMENTATION_DATE,
                getCurrentContext().getStartDate());
        criteria.addLessOrEqualThan(IepAccommodation.COL_IMPLEMENTATION_DATE, getCurrentContext().getEndDate());
        return criteria;
    }

    /**
     * Get Student Assessment Criteria using the selected Student Oids.
     *
     * @return X2Criteria
     */
    private X2Criteria getStudentAssessmentCriteria() {
        X2Criteria studentAssessmentCriteria = new X2Criteria();
        studentAssessmentCriteria
                .addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                        AssessmentDefinition.COL_ID, getAsmDefinitionId());
        studentAssessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE,
                getOrganization().getCurrentContext().getStartDate());
        studentAssessmentCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE,
                getOrganization().getCurrentContext().getEndDate());
        String assessmentExclusionField = getAsmJavaName(ALIAS_MCAS_EXCLUSION, m_dataDictionary);
        if (!StringUtils.isEmpty(assessmentExclusionField)) {
            studentAssessmentCriteria.addNotEqualTo(assessmentExclusionField, BooleanAsStringConverter.TRUE);
        }
        return studentAssessmentCriteria;
    }

    /**
     * Load map with current accommodations keyed on stdOid.
     */
    private void loadAccommodationsMap() {
        QueryByCriteria query =
                m_helper.getStudentSelectionQuery(IepAccommodation.class, getAccommodationCriteria(),
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
                    String iacCodeState = lookupReferenceCodeByBeanPath(IepAccommodation.class,
                            IepAccommodation.COL_NAME,
                            iac.getName(),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (StringUtils.isBlank(iacCodeState) && !StringUtils.isBlank(m_ddxField504PlanName)) {
                        String iacCode = (String) iac.getFieldValueByBeanPath(m_ddxField504PlanName);
                        if (!StringUtils.isBlank(iacCode) && m_ddxField504PlanNameCodeMap.containsKey(iacCode)) {
                            iacCodeState = m_ddxField504PlanNameCodeMap.get(iacCode).getStateCode();
                        }
                    }
                    if (!StringUtils.isEmpty(iacCodeState)) {
                        iacCodes.add(iacCodeState);
                    }
                    if ("UF2".equalsIgnoreCase(iacCodeState)) {
                        String iacCodeLocal = lookupReferenceCodeByBeanPath(IepAccommodation.class,
                                IepAccommodation.COL_NAME,
                                iac.getName(),
                                ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                        if (!StringUtils.isEmpty(iacCodeLocal) && !m_iacsCodesMapHelper.containsKey(stdOid + "UF2")) {
                            m_iacsCodesMapHelper.put(stdOid + "UF2", iacCodeLocal);
                        }
                    }
                    if ("UF4".equalsIgnoreCase(iacCodeState)) {
                        String iacCodeLocal = lookupReferenceCodeByBeanPath(IepAccommodation.class,
                                IepAccommodation.COL_NAME,
                                iac.getName(),
                                ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                        if (!StringUtils.isEmpty(iacCodeLocal) && !m_iacsCodesMapHelper.containsKey(stdOid + "UF4")) {
                            m_iacsCodesMapHelper.put(stdOid + "UF4", iacCodeLocal);
                        }
                    }
                }

                if (!iacCodes.isEmpty()) {
                    Collection<String> splittedIacCodes = new ArrayList<String>();

                    for (String iacCode : iacCodes) {
                        String[] codesToAdd = iacCode.split(", ");
                        for (String codeToAdd : codesToAdd) {
                            splittedIacCodes.add(StringUtils.removeAllWhitespace(codeToAdd));
                        }
                    }
                    m_iacsCodesMap.put(stdOid, splittedIacCodes);
                }
            }
        }
    }

    /**
     * Load PARCC Assessment Definition Alias field names.
     */
    private void loadMCASAssessmentDefinition() {
        X2Criteria assessmentDefinitonCriteria = new X2Criteria();
        assessmentDefinitonCriteria.addEqualTo(AssessmentDefinition.COL_ID, getAsmDefinitionId());

        QueryByCriteria assessmentDefinitonQuery =
                new QueryByCriteria(AssessmentDefinition.class, assessmentDefinitonCriteria);

        AssessmentDefinition mcasDef = (AssessmentDefinition) getBroker().getBeanByQuery(assessmentDefinitonQuery);

        // Load PARCC database field names by the PARCC Assessment Definition
        if (mcasDef == null) {
            addSetupError(ERROR_TYPE_WARNING, getAsmDefinitionId() + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
        } else {
            m_dataDictionary = DataDictionary.getDistrictDictionary(mcasDef,
                    getBroker().getPersistenceKey());

            if (m_dataDictionary == null) {
                addSetupError(ERROR_TYPE_WARNING, getAsmDefinitionId() + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
            } else {
                m_mcasAlias.put(ALIAS_MCAS_REMOTE_ADMIN, getAsmJavaName(ALIAS_MCAS_REMOTE_ADMIN, m_dataDictionary));
                m_mcasAlias.put(ALIAS_MCAS_TSTCODE, getAsmJavaName(ALIAS_MCAS_TSTCODE, m_dataDictionary));
                m_mcasAlias.put(ALIAS_MCAS_TSTFORMAT, getAsmJavaName(ALIAS_MCAS_TSTFORMAT, m_dataDictionary));
                m_mcasAlias.put(ALIAS_MCAS_SESSION_NAME, getAsmJavaName(ALIAS_MCAS_SESSION_NAME, m_dataDictionary));
                m_mcasAlias.put(ALIAS_MCAS_MED_ABS, getAsmJavaName(ALIAS_MCAS_MED_ABS, m_dataDictionary));
            }
        }
    }
}
