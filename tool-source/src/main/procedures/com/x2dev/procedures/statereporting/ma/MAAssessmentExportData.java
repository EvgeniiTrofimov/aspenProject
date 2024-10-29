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

package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.AssessmentExportData;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * MA Assessment export for PARCC - Personal Needs Profile export.
 *
 * @author X2 Development Corporation
 */
public class MAAssessmentExportData extends AssessmentExportData {

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
            String beanPath = ((MAAssessmentExportData) data).m_asmAlias.get(alias);
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
                value += section.getCourseView();
                if (!StringUtils.isEmpty(section.getRoomView())) {
                    value += "-";
                    value += section.getRoomView();
                }
            }
            return value;
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

        private static final String PARAM_LEP = "LEP";
        private static final String PARAM_LEP_LIMITED = "LEP_LIMITED";

        private static final String CODE_LEP_NO = "00";

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

            if (PARAM_LEP.equals(field.getParameter())) {
                String lep = (String) student.getFieldValueByBeanPath(m_lepField);
                if (!StringUtils.isEmpty(lep)) {
                    lep = lookupReferenceCodeByAlias(ALIAS_LEP, lep,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (!CODE_LEP_NO.equals(lep)) {
                        value = BooleanAsStringConverter.TRUE;
                    }
                }
            } else if (PARAM_LEP_LIMITED.equals(field.getParameter())) {
                String lepLimitedStatus = (String) student.getFieldValueByBeanPath(m_lepLimitedField);
                if (!StringUtils.isEmpty(lepLimitedStatus)) {
                    lepLimitedStatus = lookupReferenceCodeByAlias(ALIAS_LEP_LIMIT_STATUS, lepLimitedStatus,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (!CODE_LEP_NO.equals(lepLimitedStatus)) {
                        value = BooleanAsStringConverter.TRUE;
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

            String lunchStatus = (String) student.getFieldValueByBeanPath(m_economDisSatusField);
            lunchStatus = data.lookupReferenceCodeByAlias(ALIAS_ECONOM_DIS_STATUS, lunchStatus,
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
     * The Class RetrievePrefixGrade.
     */
    /*
     * Get student grade with prefix
     */
    protected class RetrievePrefixGrade implements FieldRetriever {
        protected static final String CALC_ID = "CALC_PREF_GR";

        private static final String REG_EX_FROM_1_TO_9 = "[1-9]{1}";

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
            SisStudent student = studentAssessment.getStudent();

            if (student != null) {
                String gradeLevel = student.getGradeLevel();

                if (!StringUtils.isEmpty(gradeLevel)) {
                    // Append "0" if code is a single digit
                    if (gradeLevel.matches(REG_EX_FROM_1_TO_9)) {
                        gradeLevel = "0" + gradeLevel;
                    }

                    value.append("GRADE").append(gradeLevel);
                }
            }

            return value.toString();
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

        private static final String DIS_CODE_AUT = "11";
        private static final String DIS_CODE_DB = "09";
        private static final String DIS_CODE_DD = "13";
        private static final String DIS_CODE_EMN = "05";
        private static final String DIS_CODE_HI = "02";
        private static final String DIS_CODE_ID = "01";
        private static final String DIS_CODE_MD = "10";
        private static final String DIS_CODE_OI = "06";
        private static final String DIS_CODE_OHI = "07";
        private static final String DIS_CODE_SLD = "08";
        private static final String DIS_CODE_SLI = "03";
        private static final String DIS_CODE_TBI = "12";
        private static final String DIS_CODE_VI = "04";

        private static final String DIS_RETURN_AUT = "AUT";
        private static final String DIS_RETURN_DB = "DB";
        private static final String DIS_RETURN_DD = "DD";
        private static final String DIS_RETURN_EMN = "EMN";
        private static final String DIS_RETURN_HI = "HI";
        private static final String DIS_RETURN_ID = "ID";
        private static final String DIS_RETURN_MD = "MD";
        private static final String DIS_RETURN_OI = "OI";
        private static final String DIS_RETURN_OHI = "OHI";
        private static final String DIS_RETURN_SLD = "SLD";
        private static final String DIS_RETURN_SLI = "SLI";
        private static final String DIS_RETURN_TBI = "TBI";
        private static final String DIS_RETURN_VI = "VI";

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

            String studDisabilCode = (String) student.getFieldValueByBeanPath(m_disabilityCodeField);
            studDisabilCode = data.lookupReferenceCodeByAlias(ALIAS_STUD_DISABILITIES, studDisabilCode,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if (!StringUtils.isEmpty(studDisabilCode) && !Arrays.asList(CODE_NOT_DISAB).contains(studDisabilCode)) {
                String studSpedDisabilCode = (String) student.getFieldValueByBeanPath(m_spedDisabCodeField);
                studSpedDisabilCode = data.lookupReferenceCodeByAlias(ALIAS_STUD_SPED_DISAB, studSpedDisabilCode,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                if (studSpedDisabilCode != null) {
                    switch (studSpedDisabilCode) {
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

                            break;
                    }
                }
            }

            return value == null ? "" : value;
        }
    }

    /**
     * Retrieves the Section 504 status.
     */
    protected class RetrieveSection504 implements FieldRetriever {
        protected static final String CALC_ID = "STD_DISABIL";

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

            String stud504DisabCode = (String) student.getFieldValueByBeanPath(m_disability504CodeField);
            stud504DisabCode = data.lookupReferenceCodeByAlias(ALIAS_STUD_DISAB_504, stud504DisabCode,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if (CODE_WAS_IN_504.equals(stud504DisabCode)) {
                value = "504";
            }

            String studDisabilCode = (String) student.getFieldValueByBeanPath(m_disabilityCodeField);
            studDisabilCode = data.lookupReferenceCodeByAlias(ALIAS_STUD_DISABILITIES, studDisabilCode,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            String studSpedDisabilCode = (String) student.getFieldValueByBeanPath(m_spedDisabCodeField);
            if (!StringUtils.isEmpty(studDisabilCode) && !Arrays.asList(CODE_NOT_DISAB).contains(studDisabilCode)
                    && !StringUtils.isEmpty(studSpedDisabilCode)) {
                value = "IEP";
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
            StringBuilder value = new StringBuilder();

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            MasterSchedule section = studentAssessment.getMasterSchedule();

            if (section != null && section.getPrimaryStaff() != null && section.getPrimaryStaff().getPerson() != null) {
                value.append(section.getCourseView());
                value.append(" ");
                value.append(section.getPrimaryStaff().getPerson().getLastName());
            }
            return value.toString().substring(0, 50);
        }
    }

    /**
     * The Class RetrieveTestId.
     */
    /*
     * Get test id with state abbreviation prefix
     */
    protected class RetrieveTestId implements FieldRetriever {
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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            StringBuilder value = new StringBuilder();

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            SisSchool school = studentAssessment.getSchool();

            if (school != null) {
                String testId = null;
                if (PARAM_DISTRICT.equals(field.getParameter())) {
                    testId = (String) school.getFieldValueByBeanPath(m_testDistrictIdField);
                } else if (PARAM_SCHOOL.equals(field.getParameter())) {
                    testId = (String) school.getFieldValueByBeanPath(m_testSchoolIdField);
                }

                if (!StringUtils.isEmpty(testId)) {
                    value.append("MA");
                    value.append(testId);
                }
            }

            return value.toString();
        }
    }

    /*
     * Aliases
     */
    // SCHOOL Table
    protected static final String ALIAS_DOE_FREE_REDUCED_LUNCH = "DOE FREE REDUCED LUNCH";
    protected static final String ALIAS_DOE_SPECIAL_ED_CLASSIFICATION = "DOE SPECIAL ED CLASSIFICATION";
    protected static final String ALIAS_TEST_DISTRICT_ID = "skl-sif-district-id";
    protected static final String ALIAS_TEST_SCHOOL_ID = "DOE 15";

    // STUDENT Table
    protected static final String ALIAS_ECONOM_DIS_STATUS = "DOE 19";
    protected static final String ALIAS_LEP = "DOE 25";
    protected static final String ALIAS_LEP_LIMIT_STATUS = "DOE 26";
    protected static final String ALIAS_STUD_DISABILITIES = "DOE 34";
    protected static final String ALIAS_STUD_SPED_DISAB = "DOE 36";
    protected static final String ALIAS_STUD_DISAB_504 = "DOE 39";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_FEDERAL_PLACEMENT = "DOE FEDERAL PLACEMENT";
    protected static final String ALIAS_SPECIAL_ED_PLACEMENT = "DOE SPECIAL ED PLACEMENT";
    protected static final String ALIAS_TESTING_COUNTY = "DOE TESTING COUNTY";
    protected static final String ALIAS_ESL_FLAG = "ESL Flag";

    // STUDENTENROLLMENT Table
    protected static final String ALIAS_ATTENDING_SCHOOL = "DOE ATTENDING SCHOOL";

    /**
     * Other internal constants
     */
    protected static final String CODE_ASSESSMENT_DEFINITION_ID_MA = "MA PARCC";
    protected static final String CODE_SPED_STATUS_ACTIVE = "Active";
    protected static final String ERROR_MESSAGE_STATE_CODE_NOT_DEFINED =
            " System State Code (sys.state) is not set in System Preferences";
    protected static final String ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS =
            "No MA PARCC Student Assessment's were created by the selected students.";
    protected static final String STATE_CODE_MA = "MA";
    protected static final String CODE_ONE = "1";
    protected static final String CODE_WAS_IN_504 = "01";
    protected static final String[] CODE_LUNCH_STATUS = {"01", "02"};
    protected static final String[] CODE_NOT_DISAB = {"00", "01"};
    protected static final String LEP_TEST_NOT_EXEMPT = "N";
    protected static final String LEP_TEST_EXEMPT = "Y";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_disability504CodeField;
    protected String m_disabilityCodeField;
    protected String m_economDisSatusField;
    protected String m_lepField;
    protected String m_lepLimitedField;
    protected String m_spedDisabCodeField;
    protected String m_testDistrictIdField;
    protected String m_testSchoolIdField;

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
        if (getSetupErrors().size() == 0) {
            // Assign custom field retriever calculations.
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveMultRaces.CALC_ID, new RetrieveMultRaces());
            calcs.put(RetrieveSection504.CALC_ID, new RetrieveSection504());
            calcs.put(RetrieveAssessment.CALC_ID, new RetrieveAssessment());
            calcs.put(RetrieveCourseName.CALC_ID, new RetrieveCourseName());
            calcs.put(RetrieveSessionName.CALC_ID, new RetrieveSessionName());
            calcs.put(RetrievePrimaryDisability.CALC_ID, new RetrievePrimaryDisability());
            calcs.put(RetrieveFreeReducedLunch.CALC_ID, new RetrieveFreeReducedLunch());
            calcs.put(RetrieveTestId.CALC_ID, new RetrieveTestId());
            calcs.put(RetrievePrefixGrade.CALC_ID, new RetrievePrefixGrade());
            calcs.put(RetrieveLepInfo.CALC_ID, new RetrieveLepInfo());
            super.addCalcs(calcs);
        }
    }

    /**
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getAsmDefinitionId()
     */
    @Override
    protected String getAsmDefinitionId() {
        return CODE_ASSESSMENT_DEFINITION_ID_MA;
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
        m_disabilityCodeField = translateAliasToJavaName(ALIAS_STUD_DISABILITIES, true);
        m_disability504CodeField = translateAliasToJavaName(ALIAS_STUD_DISAB_504, true);
        m_economDisSatusField = translateAliasToJavaName(ALIAS_ECONOM_DIS_STATUS, true);
        m_lepField = translateAliasToJavaName(ALIAS_LEP, true);
        m_lepLimitedField = translateAliasToJavaName(ALIAS_LEP_LIMIT_STATUS, true);
        m_spedDisabCodeField = translateAliasToJavaName(ALIAS_STUD_SPED_DISAB, true);
        m_testDistrictIdField = translateAliasToJavaName(ALIAS_TEST_DISTRICT_ID, true);
        m_testSchoolIdField = translateAliasToJavaName(ALIAS_TEST_SCHOOL_ID, true);
    }
}
