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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
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
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New York state procedure for Assessment Export export.
 *
 * @author X2 Development Corporation
 */

public class NYAssessmentExport extends StateReportData {
    /**
     * Entity class for Assessment Export export.
     *
     * @author X2 Development Corporation
     */

    public static class AssessmentExportEntity extends StateReportEntity {

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public AssessmentExportEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            NYAssessmentExport afData = (NYAssessmentExport) data;
            StudentAssessment asm = (StudentAssessment) bean;
            SisStudent std = asm.getStudent();
            Collection<StudentEnrollmentSpan> spans = afData.m_helper.getStudentEnrollmentSpans(std, true);
            boolean noRows = true;
            for (StudentEnrollmentSpan span : spans) {
                PlainDate startDate = span.getFirstActiveDate();
                PlainDate endDate = span.getLastActiveDate();
                if (startDate != null && !startDate.after(asm.getDate())
                        && (endDate == null || !endDate.before(asm.getDate()))) {
                    noRows = false;
                    break;
                }
            }
            if (noRows) {
                setRowCount(0);
            }
        }
    }

    /**
     * Input Definition Parameters
     */
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_ASSESS_MOD = "assessMod";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";

    /**
     * Retriever Parameters
     */
    protected static final String CALC_PARAM_NUMERIC = "NUMERIC";
    protected static final String CALC_PARAM_SCALE = "SCALE";
    protected static final String CALC_PARAM_RAW = "RAW";

    protected static final String AST_CODE_WRITING = "writing";
    protected static final String AST_CODE_REGENTS = "regents";
    protected static final String AST_CODE_RCT = "rct";
    protected static final String AST_CODE_FLACS = "flacs";
    protected static final String AST_CODE_NYS = "nys";
    protected static final String AST_CODE_NYSAA = "nysaa";
    protected static final String AST_CODE_NYSESLAT = "nyseslat";
    protected static final String AST_CODE_SLP = "slp";
    protected static final String AST_CODE_CTE = "cte";

    /**
     * Aliases
     */
    // SCHOOL
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    // ASSESSMENT_DEFINITION
    protected static final String ALIAS_EXCLUDE_FROM_SIRS = "NY EXCLUDE SIRS";

    // STUDENT_ASSESSMENT
    protected static final String ALIAS_DESCRIPTION = "DOE ITEM DESCRIPTION";
    protected static final String ALIAS_NUMERIC_SCORE = "DOE NUMERIC SCORE";
    protected static final String ALIAS_RAW_SCORE = "DOE RAW SCORE";
    protected static final String ALIAS_SCALE_SCORE = "DOE SCALE SCORE";
    protected static final String ALIAS_TEST_DESCRIPTION = "DOE TEST DESCRIPTION";

    /**
     * Local variables for reporting information.
     */
    protected StudentHistoryHelper m_helper;

    protected boolean m_excludeSchoolIndicator;
    protected boolean m_removeHeaderIndicator;
    protected PlainDate m_reportDate;
    protected PlainDate m_schoolYearStartDate;

    protected String m_fieldStudentAssessmentNumericScore;
    protected String m_fieldStudentAssessmentRawScore;
    protected String m_fieldStudentAssessmentScaleScore;
    protected String m_fieldStudentAssessmentTestDescription;
    protected String m_fieldDefinitionExcludeFromReporting;
    protected String m_fieldSchoolExclude;

    /**
     * Retrieves the current date.
     */
    protected class RetrieveSnapshotDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return new PlainDate();
        }
    }

    /**
     * Retrieves score of students based on assessment values. Depending on the type of assessment,
     * the score changes.
     * This may need to be revisited in the future as this needed to be hard coded and is not a
     * 'best practice' of Aspen use.
     */
    protected class RetrieveScoreValue implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            StudentAssessment studentAssessment = (StudentAssessment) entity.getBean();
            String param = (String) field.getParameter();

            AssessmentDefinition assessmentDefinition = studentAssessment.getAssessmentDefinition();

            String description =
                    (String) studentAssessment.getFieldValueByBeanPath(m_fieldStudentAssessmentTestDescription);

            description = (StringUtils.isEmpty(description)) ? null : description.toLowerCase();
            String id = assessmentDefinition.getId().toLowerCase();

            if (CALC_PARAM_NUMERIC.equals(param)) {
                if (AST_CODE_REGENTS.equals(id)
                        || (id.contains(AST_CODE_RCT) && description != null
                                && description.contains(AST_CODE_WRITING))) {
                    value = studentAssessment.getFieldValueByBeanPath(m_fieldStudentAssessmentScaleScore);
                } else if (id.contains(AST_CODE_RCT)) {
                    value = studentAssessment.getFieldValueByBeanPath(m_fieldStudentAssessmentRawScore);
                } else {
                    value = studentAssessment.getFieldValueByBeanPath(m_fieldStudentAssessmentNumericScore);
                }
            } else if (CALC_PARAM_SCALE.equals(param)) {
                if (!AST_CODE_REGENTS.equals(id) && !id.contains(AST_CODE_RCT)) {
                    value = studentAssessment.getFieldValueByBeanPath(m_fieldStudentAssessmentScaleScore);
                }
            } else if (CALC_PARAM_RAW.equals(param)) {
                // if the id is regents or rct and the description does not contain writing
                if (!AST_CODE_REGENTS.equals(id) ||
                        !(id.contains(AST_CODE_RCT) && description != null && description.contains(AST_CODE_WRITING))) {
                    value = studentAssessment.getFieldValueByBeanPath(m_fieldStudentAssessmentRawScore);
                }
            }

            if (value != null) {
                try {
                    Float converted = Float.valueOf(value.toString());
                    value = converted;
                } catch (NumberFormatException nfe) {
                    // this is fine
                }
            }

            return value;
        }
    }

    /**
     * Validate the Instruction Dates.
     */
    protected class ValidateAchived implements FieldValidator {
        private static final String VAL_ID = "VAL-ACHIEVED";

        /**
         * Fields' IDs
         */
        private static final String FIELD_ALPHA_SCR = "Alpha Score";
        private static final String FIELD_NUMERIC_SCR = "Numeric Score";

        private final List<String> VALID_ASD_ACHIEVED = Arrays.asList("AICE", "IGSCE");

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String alphaScore = entity.getFieldValue(FIELD_ALPHA_SCR);
            String numericScore = entity.getFieldValue(FIELD_NUMERIC_SCR);
            String asdName = ((StudentAssessment) entity.getBean()).getAssessmentDefinition().getName();


            if (!VALID_ASD_ACHIEVED.contains(asdName) && StringUtils.isEmpty(alphaScore)
                    && StringUtils.isEmpty(numericScore)) {
                errors.add(new StateReportValidationError(entity, field, "AS6018",
                        "Record must have an Alpha or Numeric score and a Std. Achieve Code"));
            }

            if (VALID_ASD_ACHIEVED.contains(asdName) && (!"01".equals(value) || !"03".equals(value))) {
                errors.add(new StateReportValidationError(entity, field, "AS6047",
                        "Missing or Invalid Std. Achieve Code for assessment (Must be 01 or 03)"));
            }

            if ("999".equals(numericScore) && Integer.valueOf(value).intValue() > 90) {
                errors.add(new StateReportValidationError(entity, field, "AS6020",
                        "Invalid Std. Achieve code for a 999 numeric score record: " + value));
            }

            return errors;
        }
    }

    /**
     * Validate the Instruction Dates.
     */
    protected class ValidateAsmScores implements FieldValidator {
        private static final String VAL_ID = "VAL-ASM-SCORE";

        /**
         * Fields' IDs
         */
        private static final String FIELD_ALPHA_SCR = "Alpha Score";
        private static final String FIELD_NUMERIC_SCR = "Numeric Score";

        private final List<String> VALID_RCT_CTE_CODES = Arrays.asList("P", "Pass", "F", "Fall");
        private final List<String> VALID_ASM_NOT_WITH_NUM_SCR = Arrays.asList("AICE", "IGSCE", "RCT", "CTE", "COSF");

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String alphaScore = entity.getFieldValue(FIELD_ALPHA_SCR);
            String numericScore = entity.getFieldValue(FIELD_NUMERIC_SCR);
            String asdName = ((StudentAssessment) entity.getBean()).getAssessmentDefinition().getName();


            if (("RCT".equals(asdName) || "CTE".equals(asdName)) && !VALID_RCT_CTE_CODES.contains(alphaScore)) {
                errors.add(new StateReportValidationError(entity, field, "AS6012",
                        "Invalid alpha score for CTE or RCT assessment definition: " + alphaScore));
            }

            if (StringUtils.isEmpty(numericScore) && !StringUtils.isEmpty(alphaScore)
                    && !VALID_ASM_NOT_WITH_NUM_SCR.contains(asdName)) {
                errors.add(new StateReportValidationError(entity, field, "AS6013", "Record needs a Numeric score"));
            }

            if (StringUtils.isEmpty(numericScore) && StringUtils.isEmpty(alphaScore) && "IGCSE".equals(asdName)
                    || "AICE".equals(asdName)) {
                errors.add(new StateReportValidationError(entity, field, "AS6014", "Alpha or Numeric score provided"));
            }

            if (!StringUtils.isEmpty(numericScore) && "COSF".equals(asdName)) {
                errors.add(new StateReportValidationError(entity, field, "AS6039",
                        "COSF: Progress assessments must only have an Alpha score"));
            }

            if (!StringUtils.isEmpty(alphaScore) && VALID_ASM_NOT_WITH_NUM_SCR.contains(asdName)) {
                errors.add(new StateReportValidationError(entity, field, "AS6110",
                        "No Alpha score expected: " + alphaScore));
            }

            return errors;
        }
    }

    /**
     * Overrided getHeading Method. The reason is so that the user can decide if the header is
     * included or not at runtime.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        if (m_removeHeaderIndicator) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * Initializes the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Do not include schools flags as excluded.
            if (m_excludeSchoolIndicator) {
                m_helper.getStudentCriteria().addNotEqualTo(
                        SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSchoolExclude,
                        BooleanAsStringConverter.TRUE);
            }

            SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());

            X2Criteria assessmentCriteria = new X2Criteria();
            assessmentCriteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
            assessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, m_schoolYearStartDate);

            // Exclude StudentAssessments that its AssessmentDefintion's is flag as exclude form
            // reporting.
            if (m_fieldDefinitionExcludeFromReporting != null) {
                assessmentCriteria.addNotEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION
                        + ModelProperty.PATH_DELIMITER + m_fieldDefinitionExcludeFromReporting,
                        BooleanAsStringConverter.TRUE);
            }

            QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, assessmentCriteria);

            applyInputSort(query, null);

            setQuery(query);
            setEntityClass(AssessmentExportEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("AR-SSDATE", new RetrieveSnapshotDate());
            calcs.put("AF-SCORE", new RetrieveScoreValue());

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(ValidateAsmScores.VAL_ID, new ValidateAsmScores());
            validators.put(ValidateAchived.VAL_ID, new ValidateAchived());

            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Initializes fields to be used in the export.
     */
    private void initializeFields() {
        // System Parameters
        m_schoolYearStartDate = ((SisOrganization) getOrganization()).getCurrentContext().getStartDate();

        // Load Parameters
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        m_removeHeaderIndicator = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeaderIndicator = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }

        m_excludeSchoolIndicator = false;
        if (getParameter(PARAM_EXCLUDE_SCHOOL) != null) {
            m_excludeSchoolIndicator = ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue();
        }

        // Load Alias database field Names
        m_fieldStudentAssessmentNumericScore = translateAliasToJavaName(ALIAS_NUMERIC_SCORE, true);
        m_fieldStudentAssessmentScaleScore = translateAliasToJavaName(ALIAS_SCALE_SCORE, true);
        m_fieldStudentAssessmentRawScore = translateAliasToJavaName(ALIAS_RAW_SCORE, true);
        m_fieldStudentAssessmentTestDescription = translateAliasToJavaName(ALIAS_TEST_DESCRIPTION, true);
        m_fieldSchoolExclude = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldDefinitionExcludeFromReporting = translateAliasToJavaName(ALIAS_EXCLUDE_FROM_SIRS, true);
    }
}
