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
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RIPARCC - Partnership for Assessment of Readiness for College and Career export.
 *
 * @author X2 Development Corporation
 */
public class RIPARCC extends StateReportData {
    static RIPARCC m_data;

    /**
     * Implementation of StateReportEntity to be used for RIPARCC export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class RIPARCCEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public RIPARCCEntity() {
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
            m_data = (RIPARCC) data;
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

    }

    /**
     * The Class RetrieveAssessment.
     */
    /*
     * Has the Student taken a PARCC PBA, EOY or PBA & EOY test
     */
    protected class RetrieveAssessment implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            RIPARCCEntity parccEntity = (RIPARCCEntity) entity;
            Object value = null;
            Object result = null;

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            String param = (String) field.getParameter();

            if (CALC_PARAM_PARCC_TEST_CODE.equals(param)) {
                if (m_data.m_assessmentTestCodeField != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentTestCodeField);
                }
            } else if (CALC_PARAM_TEST_FORMAT.equals(param)) {
                String code;
                if (m_data.m_assessmentTestFormatField != null) {
                    code = (String) studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentTestFormatField);

                    if (code != null) {
                        value = code;
                    }
                }
            } else if (CALC_PARAM_TEST_RETEST.equals(param)) {
                value = CODE_NO;
                if (m_data.m_assessmentRetestField != null) {
                    result = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentRetestField);

                    if (CODE_ONE.equals(result)) {
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
                    if (CODE_ASSESSMENT_SPA.equals(result)) {
                        value = result;
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
            } else if (CALC_PARAM_TSA_EXTND_TIME.equals(param)) {
                if (m_data.m_assessmentTCAETField != null) {
                    value = studentAssessment.getFieldValueByBeanPath(m_data.m_assessmentTCAETField);
                }
            }

            return value;
        }
    }

    /**
     * The parameter is the name of the program code on student program participation
     * that is the ELL program code for this district.
     */
    protected class RetrieveEnglishLanguageLearner implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String result = null;
            RIPARCCEntity parccEntity = (RIPARCCEntity) entity;
            SisStudent student = parccEntity.getStudent();

            String param = (String) field.getParameter();

            if (CALC_PARAM_ELL.equals(param)) {
                value = CODE_NO;
                result = (String) student.getFieldValueByAlias(ALIAS_ESL_FLAG);
                if (CODE_ONE.equals(result)) {
                    value = CODE_YES;
                }
            } else if (CALC_PARAM_TITLE_III.equals(param)) {
                value = CODE_NO;
                result = (String) student.getFieldValueByAlias(ALIAS_ESL_FLAG);
                if (CODE_ONE.equals(result)) {
                    value = CODE_YES;
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
            RIPARCCEntity parccEntity = (RIPARCCEntity) entity;
            Object value = null;
            String param = (String) field.getParameter();
            SisStudent student = parccEntity.getStudent();

            Collection<Race> races = m_personRaceMap.get(student.getPersonOid());

            if (CALC_PARAM_HISPANIC_LATINO.equals(param)) {
                value = CODE_NO;
                if (student.getPerson().getHispanicLatinoIndicator()) {
                    value = CODE_YES;
                }
            } else if (CALC_PARAM_INDIAN_ALASKIAN_NATIVE.equals(param)) {
                value = CODE_NO;
                for (Race race : races) {
                    String raceCode = race.getRaceCode();
                    String stateCode = lookupStateValue(Race.class, Race.COL_RACE_CODE, raceCode);
                    if (REF_CODE_RI_NATIVE_AMERICAN.equalsIgnoreCase(stateCode)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_ASIAN.equals(param)) {
                value = CODE_NO;
                for (Race race : races) {
                    String raceCode = race.getRaceCode();
                    String stateCode = lookupStateValue(Race.class, Race.COL_RACE_CODE, raceCode);
                    if (REF_CODE_RI_ASIAN.equalsIgnoreCase(stateCode)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_BLACK_AFRICAN_AMERICAN.equals(param)) {
                value = CODE_NO;
                for (Race race : races) {
                    String raceCode = race.getRaceCode();
                    String stateCode = lookupStateValue(Race.class, Race.COL_RACE_CODE, raceCode);
                    if (REF_CODE_RI_AFRICAN_AMERICAN.equalsIgnoreCase(stateCode)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_HAWAIIAN_PACIFIC_ISLANDER.equals(param)) {
                value = CODE_NO;
                for (Race race : races) {
                    String raceCode = race.getRaceCode();
                    String stateCode = lookupStateValue(Race.class, Race.COL_RACE_CODE, raceCode);
                    if (REF_CODE_RI_PACIFIC_ISLANDER.equalsIgnoreCase(stateCode)) {
                        value = CODE_YES;
                    }
                }
            } else if (CALC_PARAM_WHITE.equals(param)) {
                value = CODE_NO;
                for (Race race : races) {
                    String raceCode = race.getRaceCode();
                    String stateCode = lookupStateValue(Race.class, Race.COL_RACE_CODE, raceCode);
                    if (REF_CODE_RI_CAUCASIAN.equalsIgnoreCase(stateCode)) {
                        value = CODE_YES;
                    }
                }
            }

            return value;
        }
    }

    /**
     * Retrieve Free, Reduced, Full Price Lunch.
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
            RIPARCCEntity parccEntity = (RIPARCCEntity) entity;
            SisStudent student = parccEntity.getStudent();

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
            RIPARCCEntity parccEntity = (RIPARCCEntity) entity;
            SisStudent student = parccEntity.getStudent();

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
            RIPARCCEntity parccEntity = (RIPARCCEntity) entity;
            SisStudent student = parccEntity.getStudent();

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
            RIPARCCEntity parccEntity = (RIPARCCEntity) entity;
            SisStudent student = parccEntity.getStudent();

            Object value = null;

            if (CALC_PARAM_SPED_STATUS.equals(param)) {
                String status = student.getSpedStatusCode();
                if (CODE_SPED_STATUS_ACTIVE.equals(status)) {
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
            RIPARCCEntity parccEntity = (RIPARCCEntity) entity;
            SisStudent student = parccEntity.getStudent();

            String result = null;
            Object value = null;
            String param = (String) field.getParameter();

            if (CALC_PARAM_PARCC_ELL.equals(param)) {
                result = (String) student.getFieldValueByAlias(ALIAS_ESL_FLAG);

                value = CODE_NO;
                if (CODE_ONE.equals(result)) {
                    value = CODE_YES;
                }
            } else if (CALC_PARAM_PARCC_504.equals(param)) {
                result = student.getSection504StatusCode();
                if (CODE_SECTION_504_STATUS_ACTIVE.equals(result)) {
                    value = CODE_YES;
                }
            } else if (CALC_PARAM_PARCC_IEP.equals(param)) {
                result = student.getSpedStatusCode();
                if (CODE_SPED_STATUS_ACTIVE.equals(result)) {
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

    /*
     * Retriever Parameters
     */
    protected static final String CALC_ID_PARCC_DISABILITY = "PARCC-DISABILITY";
    protected static final String CALC_ID_PARCC_ELL = "PARCC-ELL";
    protected static final String CALC_ID_PARCC_FARM = "PARCC-FARM";
    protected static final String CALC_ID_PARCC_LOC = "PARCC-LOC";
    protected static final String CALC_ID_PARCC_RACE = "PARCC-RACE";
    protected static final String CALC_ID_PARCC_SPED = "PARCC-SPED";
    protected static final String CALC_ID_PARCC_STDACC = "PARCC-STDACC";
    protected static final String CALC_ID_PARCC_TEST = "PARCC-TEST";

    protected static final String CALC_PARAM_STATE_ID = "STATE_ID";
    protected static final String CALC_PARAM_HISPANIC_LATINO = "HISPANIC_LATINO";
    protected static final String CALC_PARAM_INDIAN_ALASKIAN_NATIVE = "INDIAN_ALASKIAN_NATIVE";
    protected static final String CALC_PARAM_ASIAN = "ASIAN";
    protected static final String CALC_PARAM_BLACK_AFRICAN_AMERICAN = "BLACK_AFRICAN_AMERICAN";
    protected static final String CALC_PARAM_HAWAIIAN_PACIFIC_ISLANDER = "HAWAIIAN_PACIFIC_ISLANDER";
    protected static final String CALC_PARAM_WHITE = "WHITE";
    protected static final String CALC_PARAM_SPED_STATUS = "SPED_STATUS";
    protected static final String CALC_PARAM_SPED_SERVICE = "SPED_SERVICE";
    protected static final String CALC_PARAM_SPED_END_DATE = "SPED_END_DATE";
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
    protected static final String CALC_PARAM_TSA_EXTND_TIME = "TSA_EXTND_TIME";
    protected static final String CALC_PARAM_ELL = "ELL";
    protected static final String CALC_PARAM_TITLE_III = "TITLE_III";

    /*
     * Aliases
     */
    // ORGANIZATION Table
    protected static final String ALIAS_DISTRICT_CODE = "RI Reporting District Code";

    // SCHOOL Table
    protected static final String ALIAS_SCHOOL_CODE = "State School Id";

    // STUDENT Table
    protected static final String ALIAS_ESL_FLAG = "ESL Flag";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_LUNCH_STATUS = "Lunch Description";
    protected static final String ALIAS_SPED_FLAG = "SPED Flag";
    protected static final String ALIAS_SPED_SERVICE = "DOE SPECIAL ED";
    protected static final String ALIAS_SPED_END_DATE = "DOE SPED END";

    // STUDENT_SCHEDULE Table
    protected static final String ALIAS_SUBJECT = "RI Course ID";

    // STUDENT_PROGRAM_PARTICIPATION
    protected static final String ALIAS_ELL_PROGRAM_CODE = "RI Program Code";

    // STUDENT_ASSESSMENT Table (PARCC)
    protected static final String ALIAS_PARCC_RETEST = "DOE PARCC_RETEST";
    protected static final String ALIAS_PARCC_ALTREP_PAPERTST = "DOE ALTREP PAPERTST";
    protected static final String ALIAS_PARCC_TSTCODE = "DOE PARCC_TSTCODE";
    protected static final String ALIAS_PARCC_BRAILLE = "DOE PARCC_Braille";
    protected static final String ALIAS_PARCC_LARGEPRT = "DOE PARCC_LARGEPRT";
    protected static final String ALIAS_PARCC_TRANSMATHPAPER = "DOE PARCC_TRANSMATHPAPER";
    protected static final String ALIAS_PARCC_TSTFORMAT = "DOE PARCC_TSTFORMAT";
    protected static final String ALIAS_PARCC_HUMAN = "DOE PARCC_Human";
    protected static final String ALIAS_PARCC_TSA_ET = "DOE PARCC_TSA_ET";

    /**
     * Other internal constants
     */
    protected static final String STATE_CODE_RI = "RI";

    protected static final String CODE_ASSESSMENT_DEFINITION_ID_PARCC = "PARCC";
    protected static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
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
            "No NJ PARCC Student Assessment's were created by the selected students.";

    protected static final String ENGLISH_LANGUAGE_LEARNER_PROGRAM = "%ELL%";
    protected static final String TITLE_PROGRAM = "%Title%";

    protected static final String REF_CODE_RI_CAUCASIAN = "E";
    protected static final String REF_CODE_RI_AFRICAN_AMERICAN = "C";
    protected static final String REF_CODE_RI_ASIAN = "B";
    protected static final String REF_CODE_RI_NATIVE_AMERICAN = "A";
    protected static final String REF_CODE_RI_PACIFIC_ISLANDER = "P";

    protected static final String CODE_ASSESSMENT_SPA = "SPA";
    protected static final String CODE_SECTION_504_STATUS_ACTIVE = "Active";
    protected static final String CODE_SPED_STATUS_ACTIVE = "Active";
    protected static final String CODE_LUNCH_STATUS_F = "F";
    protected static final String CODE_LUNCH_STATUS_FREE = "Free";
    protected static final String CODE_LUNCH_STATUS_R = "R";
    protected static final String CODE_LUNCH_STATUS_REDUCED = "Reduced";

    protected static final String CODE_YES = "Y";
    protected static final String CODE_NO = "N";
    protected static final String CODE_ONE = "1";
    protected static final String CODE_ZERO = "0";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected boolean m_removeHeader;
    protected int m_schoolYear;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;
    protected PlainDate m_startDate;
    protected PlainDate m_endDate;
    protected PlainDate m_firstDayDate;

    protected String m_courseSubjectCodeField;
    protected String m_orgDistrictCodeField;
    protected String m_schoolCodeField;
    protected String m_programEslCodeColumnName;

    protected String m_assessmentAltRepPaperTestField;
    protected String m_assessmentBrailleField;
    protected String m_assessmentFormatField;
    protected String m_assessmentHumanField;
    protected String m_assessmentLargePrintField;
    protected String m_assessmentRetestField;
    protected String m_assessmentTestCodeField;
    protected String m_assessmentTestFormatField;
    protected String m_assessmentTransMathPaperField;
    protected String m_assessmentTCAETField;

    protected Map<String, Collection<Race>> m_personRaceMap;
    protected Map<String, String> m_disabilityCodes;
    protected Map<String, StudentProgramParticipation> m_englishLanguageLearnerPrograms;


    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (m_removeHeader) {
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

        loadPARCCAssessmentDefinition();

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

        if (getSetupErrors().size() == 0) {
            loadStudentEnglishLanguagePrograms(studentSubQuery);

            m_personRaceMap = m_helper.getPersonRaceMap();

            // Assign the custom entity class, if there is one.
            setQuery(studentAssessmentQuery);
            setEntityClass(RIPARCCEntity.class);

            // Assign custom field retriever calculations.
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_PARCC_DISABILITY, new RetrievePrimaryDisability());
            calcs.put(CALC_ID_PARCC_ELL, new RetrieveEnglishLanguageLearner());
            calcs.put(CALC_ID_PARCC_FARM, new RetrieveFreeReducedLunch());
            calcs.put(CALC_ID_PARCC_LOC, new RetrieveLocation());
            calcs.put(CALC_ID_PARCC_RACE, new RetrieveEthnicity());
            calcs.put(CALC_ID_PARCC_SPED, new RetrieveSPED());
            calcs.put(CALC_ID_PARCC_STDACC, new RetrieveStudentAccomodations());
            calcs.put(CALC_ID_PARCC_TEST, new RetrieveAssessment());
            super.addCalcs(calcs);
        }
    }

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
                        AssessmentDefinition.COL_ID, CODE_ASSESSMENT_DEFINITION_ID_PARCC);

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
        m_reportDate = new PlainDate();
        if (getParameter(PARAM_REPORT_DATE) != null) {
            m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        }

        m_removeHeader = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeader = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }

        /*
         * RIPARCC requires a specific representation of the student disability
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

        // m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false); //check if null
        // if not used
        m_courseSubjectCodeField = translateAliasToJavaName(ALIAS_SUBJECT, true);
        m_orgDistrictCodeField = translateAliasToJavaName(ALIAS_DISTRICT_CODE, true);
        m_schoolCodeField = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
        m_programEslCodeColumnName = translateAliasToColumnName(ALIAS_ELL_PROGRAM_CODE, false);
    }

    /**
     * Load PARCC Assessment Definition Alias field names.
     */
    private void loadPARCCAssessmentDefinition() {
        X2Criteria assessmentDefinitonCriteria = new X2Criteria();
        assessmentDefinitonCriteria.addEqualTo(AssessmentDefinition.COL_ID, CODE_ASSESSMENT_DEFINITION_ID_PARCC);

        QueryByCriteria assessmentDefinitonQuery =
                new QueryByCriteria(AssessmentDefinition.class, assessmentDefinitonCriteria);

        AssessmentDefinition pARCCDefinition =
                (AssessmentDefinition) getBroker().getBeanByQuery(assessmentDefinitonQuery);

        // Load PARCC database field names by the PARCC Assessment Definition
        if (pARCCDefinition == null) {
            addSetupError(ERROR_TYPE_WARNING,
                    CODE_ASSESSMENT_DEFINITION_ID_PARCC + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
        } else {
            DataDictionary dataDictionary =
                    DataDictionary.getDistrictDictionary(pARCCDefinition, getBroker().getPersistenceKey());

            if (dataDictionary == null) {
                addSetupError(ERROR_TYPE_WARNING,
                        CODE_ASSESSMENT_DEFINITION_ID_PARCC + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
            } else {
                DataDictionaryField formatField = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TSTFORMAT);
                if (formatField != null && dataDictionary.containsAlias(ALIAS_PARCC_TSTFORMAT)) {
                    m_assessmentFormatField = formatField.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_PARCC
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_TSTFORMAT + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField retestField = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_RETEST);
                if (retestField != null && dataDictionary.containsAlias(ALIAS_PARCC_RETEST)) {
                    m_assessmentRetestField = retestField.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_PARCC
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_RETEST + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField altRepPaperTstName =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_ALTREP_PAPERTST);
                if (altRepPaperTstName != null && dataDictionary.containsAlias(ALIAS_PARCC_ALTREP_PAPERTST)) {
                    m_assessmentAltRepPaperTestField = altRepPaperTstName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING,
                            CODE_ASSESSMENT_DEFINITION_ID_PARCC + ERROR_MESSAGE_ASSESS_DEF_ALIAS
                                    + ALIAS_PARCC_ALTREP_PAPERTST + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField testCodeName = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TSTCODE);
                if (testCodeName != null && dataDictionary.containsAlias(ALIAS_PARCC_TSTCODE)) {
                    m_assessmentTestCodeField = testCodeName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_PARCC
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_TSTCODE + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField brailleName = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_BRAILLE);
                if (brailleName != null && dataDictionary.containsAlias(ALIAS_PARCC_BRAILLE)) {
                    m_assessmentBrailleField = brailleName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_PARCC
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_BRAILLE + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField largePrintName =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_LARGEPRT);
                if (largePrintName != null && dataDictionary.containsAlias(ALIAS_PARCC_LARGEPRT)) {
                    m_assessmentLargePrintField = largePrintName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_PARCC
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_LARGEPRT + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField transMathPaperName =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TRANSMATHPAPER);
                if (transMathPaperName != null && dataDictionary.containsAlias(ALIAS_PARCC_TRANSMATHPAPER)) {
                    m_assessmentTransMathPaperField = transMathPaperName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING,
                            CODE_ASSESSMENT_DEFINITION_ID_PARCC + ERROR_MESSAGE_ASSESS_DEF_ALIAS
                                    + ALIAS_PARCC_TRANSMATHPAPER + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField testFormatName =
                        dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TSTFORMAT);
                if (testFormatName != null && dataDictionary.containsAlias(ALIAS_PARCC_TSTFORMAT)) {
                    m_assessmentTestFormatField = testFormatName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_PARCC
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_TSTFORMAT + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField humanName = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_HUMAN);
                if (humanName != null && dataDictionary.containsAlias(ALIAS_PARCC_HUMAN)) {
                    m_assessmentHumanField = humanName.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_PARCC
                            + ERROR_MESSAGE_ASSESS_DEF_ALIAS + ALIAS_PARCC_HUMAN + ERROR_MESSAGE_IS_NOT_DEFINED);
                }

                DataDictionaryField tcAet = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_PARCC_TSA_ET);
                if (tcAet != null && dataDictionary.containsAlias(ALIAS_PARCC_TSA_ET)) {
                    m_assessmentTCAETField = tcAet.getJavaName();
                } else {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_PARCC
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

        if (!StringUtils.isEmpty(m_programEslCodeColumnName)) {
            X2Criteria studentProgramCriteria = new X2Criteria();

            studentProgramCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);

            studentProgramCriteria.addLike(StudentProgramParticipation.COL_PROGRAM_CODE,
                    ENGLISH_LANGUAGE_LEARNER_PROGRAM);
            studentProgramCriteria.addLike(StudentProgramParticipation.COL_PROGRAM_CODE, TITLE_PROGRAM);
            QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, studentProgramCriteria);

            m_englishLanguageLearnerPrograms = getBroker().getMapByQuery(query,
                    StudentProgramParticipation.COL_STUDENT_OID, getBroker().getCount(query));
        }
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

}
