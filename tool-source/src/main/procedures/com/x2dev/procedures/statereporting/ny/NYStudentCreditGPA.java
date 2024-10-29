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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
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
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class NYStudentCreditGPA.
 */
public class NYStudentCreditGPA extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used for Student Class Grade Detail export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class NYStudentCreditGPAEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        protected List<Transcript> m_transcripts;
        protected BigDecimal m_totalCredits;
        protected BigDecimal m_totalCourseCredits;
        protected float m_maxGPA = 0;
        protected NYStudentCreditGPA gpaData;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NYStudentCreditGPAEntity() {
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
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] " + " Trnscript: " +
                    getCurrentTranscript().getOid();
            return name;
        }

        /**
         * Initialize the entity for the student bean provided.
         * This method counts transcripts credits and totals credits.
         * The entity can produce multiple rows. Each transcript - one row.
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
            SisStudent student = (SisStudent) bean;
            gpaData = (NYStudentCreditGPA) data;
            m_transcripts = null;
            m_transcripts = ((NYStudentCreditGPA) data).getTranscriptsForStudent(student);
            if (m_transcripts != null) {
                m_totalCredits = new BigDecimal(0);
                m_totalCourseCredits = new BigDecimal(0);
                for (Transcript trans : m_transcripts) {
                    BigDecimal totalCredit = trans.getTotalCredit();
                    if (totalCredit != null) {
                        m_totalCredits = m_totalCredits.add(totalCredit);
                    }
                    if (trans.getSchoolCourse() != null) {
                        m_totalCourseCredits = m_totalCourseCredits.add(trans.getSchoolCourse().getCredit());
                    }

                    // Calculation for maxGPA
                    if (trans.getSchoolCourse() != null && trans.getSchoolCourse().getCourse() != null) {
                        Course course = trans.getSchoolCourse().getCourse();
                        String academicLevel = course.getAcademicLevel();
                        if (course.getGpaIndicator() && !StringUtils.isEmpty(academicLevel)) {
                            float floatValue = getFloatGPALevel(trans, academicLevel).floatValue();
                            if (floatValue > m_maxGPA) {
                                m_maxGPA = floatValue;
                            }
                        }
                        trans.getTranscriptDefinition().getGradePointAverageDefinitions();
                    }
                }
            } else {
                setRowCount(0);
            }
        }

        /**
         * Looks up the value of the academic level of the course by the transcripts definition ->
         * gpa definition .
         *
         * @param trans Transcript
         * @param academicLevel String
         * @return Big decimal
         */
        private BigDecimal getFloatGPALevel(Transcript trans, String academicLevel) {
            BigDecimal highestValue = new BigDecimal(0);
            TranscriptDefinition transcriptDefinition = trans.getTranscriptDefinition();
            if (transcriptDefinition != null) {
                if (!gpaData.m_gradePointMap.containsKey(transcriptDefinition.getOid() + academicLevel)) {
                    if (transcriptDefinition.getGradePointAverageDefinitions() != null) {
                        for (GradePointAverageDefinition definition : transcriptDefinition
                                .getGradePointAverageDefinitions()) {
                            if (definition.getGpaDefinitionName().toUpperCase().contains("WEIGHTED") &&
                                    definition.getGpaDefinitionName().toUpperCase().contains("HS")) {
                                for (GradeScalePoints scalePoint : definition.getGradeScalePoints()) {
                                    if (academicLevel.equals(scalePoint.getAcademicLevel())) {
                                        BigDecimal gradePoints = scalePoint.getGradePoints();
                                        if (highestValue == null
                                                || highestValue.floatValue() < gradePoints.floatValue()) {
                                            highestValue = gradePoints;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    gpaData.m_gradePointMap.put(transcriptDefinition.getOid() + academicLevel, highestValue);
                }
                return gpaData.m_gradePointMap.get(transcriptDefinition.getOid() + academicLevel);
            }
            return new BigDecimal(0);
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
         * Return transcript for currentRow .
         *
         * @return Transcript
         */
        public Transcript getCurrentTranscript() {
            return m_transcripts.get(getCurrentRow());
        }

        /**
         * Simple getter for m_totalCredits.
         *
         * @return Object
         */
        public Object getTotalCredits() {
            return m_totalCredits;
        }

        /**
         * Simple getter for m_totalCredits.
         *
         * @return Object
         */
        public Object getTotalCourseCredits() {
            return m_totalCourseCredits;
        }

        /**
         * Gets the GPA max.
         *
         * @return float
         */
        public float getGPAMax() {
            return m_maxGPA;
        }
    }

    /*
     * Field alias names
     */
    protected static final String ALIAS_CREDIT_CODE = "DOE CREDIT CODE";
    protected static final String ALIAS_WEIGHTED_CUM_GPA = "DOE CUM WEIGHT GPA";
    protected static final String ALIAS_UNWEIGHTED_CUM_GPA = "DOE CUM UNWEIGHT GPA";
    protected static final String ALIAS_MARKING_PERIOD_CODE = "DOE MARKING PERIOD CODE";

    /*
     * Tool input definition parameter names.
     */
    protected static final String PARAM_GPA_TYPE = "gpaType";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_WEIGHTED = "W";

    protected static final String PARAM_TRANS_CREDIT_GPA_CODE = "CREDIT_GPA_CODE";
    protected static final String PARAM_TRANS_MARKING_PERIOD = "MARKING_PERIOD";
    protected static final String PARAM_TRANS_TERM_CODE = "TERM_CODE";

    protected static final String PARAM_CALC_COMMENT = "COMMENT";
    protected static final String PARAM_CALC_CUM_CREDITS_EARNED = "CUM_CREDITS_EARNED";
    protected static final String PARAM_CALC_CUMULATIVE_GPA = "CUMULATIVE_GPA";
    protected static final String PARAM_CALC_STD_GPA_RANGE_MIN = "STD_GPA_RANGE_MIN";
    protected static final String PARAM_CALC_STD_GPA_RANGE_MAX = "STD_GPA_RANGE_MAX";
    protected static final String PARAM_CALC_CUM_CREDITS_ATTEMPTED = "CUM_CREDITS_ATTEMPTED";

    protected static final String CALC_NAME_TRANCRIPT = "TRANSCRIPT";
    protected static final String CALC_NAME_CALCULATION = "CALCULATION";

    /*
     * Instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */

    protected String m_creditGpaCode;
    protected String m_weightedCumGpa;
    protected String m_unweightedCumGpa;
    protected StudentHistoryHelper m_helper;
    protected boolean m_weightedOrUnweighted;
    protected String m_markingPeriodCode;
    protected PlainDate m_reportDate;
    protected Map<String, ReferenceCode> m_gradeTermCodesMap;
    protected Map<String, ReferenceCode> m_scheduleTermCodesMap;
    protected Map<String, List<Transcript>> m_studentTranscripts;
    protected HashMap<String, Map<GradeTerm, String>> m_beanPaths;
    protected HashMap<String, BigDecimal> m_gradePointMap = new HashMap<String, BigDecimal>();

    /**
     * Returns calculations.
     */
    protected class RetrieveCalculation implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();

            NYStudentCreditGPAEntity gpaEntity = (NYStudentCreditGPAEntity) entity;
            if (PARAM_CALC_CUM_CREDITS_EARNED.equals(param)) {
                value = gpaEntity.getTotalCredits();
            } else if (PARAM_CALC_CUMULATIVE_GPA.equals(param)) {
                if (m_weightedOrUnweighted) {
                    value = ((SisStudent) gpaEntity.getBean()).getFieldValueByBeanPath(m_weightedCumGpa);
                } else {
                    value = ((SisStudent) gpaEntity.getBean()).getFieldValueByBeanPath(m_unweightedCumGpa);
                }
                if (value == null) {
                    value = new BigDecimal(0);
                }
            } else if (PARAM_CALC_COMMENT.equals(param)) {
                if (m_weightedOrUnweighted) {
                    value = "Weighted GPA";
                } else {
                    value = "Unweighted GPA";
                }
            } else if (PARAM_CALC_STD_GPA_RANGE_MIN.equals(param)) {
                value = new BigDecimal(0);
            } else if (PARAM_CALC_STD_GPA_RANGE_MAX.equals(param)) {
                if (m_weightedOrUnweighted) {
                    value = Float.valueOf(gpaEntity.getGPAMax());
                } else {
                    value = new BigDecimal(100);
                }
            } else if (PARAM_CALC_CUM_CREDITS_ATTEMPTED.equals(param)) {
                value = gpaEntity.getTotalCourseCredits();
            }
            return value;
        }
    }

    /**
     * Returns information for the current student's transcript from the schedule span.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTranscript implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            Transcript transcript = ((NYStudentCreditGPAEntity) entity).getCurrentTranscript();
            if (transcript != null) {
                if (PARAM_TRANS_CREDIT_GPA_CODE.equals(param)) {
                    value = transcript.getFieldValueByBeanPath(m_creditGpaCode);
                } else if (PARAM_TRANS_MARKING_PERIOD.equals(param)) {
                    /*
                     * This field takes from last GradeTerm in transcript, that has score.
                     * As beanpath, where value is stored, in transcript not persistence, and
                     * defines by transcript
                     * definition, we were needed to get this beanpath mapped by GradeTerm.
                     */
                    Map<GradeTerm, String> grades = getGradePathsMapping(transcript.getTranscriptDefinitionOid());
                    GradeTerm max = null;
                    for (GradeTerm term : grades.keySet()) {
                        String gradeScore = (String) transcript.getFieldValueByBeanPath(grades.get(term));
                        if (gradeScore != null) {
                            if (max == null) {
                                max = term;
                                continue;
                            }
                            if (max.getGradeTermNum() < term.getGradeTermNum()) {
                                max = term;
                            }
                        }
                    }
                    if (max != null) {
                        value = max.getFieldValueByBeanPath(m_markingPeriodCode);
                    }
                    if ((m_gradeTermCodesMap != null) &&
                            m_gradeTermCodesMap.containsKey(value) &&
                            (m_gradeTermCodesMap.get(value).getStateCode() != null)) {
                        value = m_gradeTermCodesMap.get(value).getStateCode();
                    }
                } else if (PARAM_TRANS_TERM_CODE.equals(param)) {
                    if ((transcript.getMasterSchedule() != null) &&
                            (transcript.getMasterSchedule().getScheduleTerm() != null)) {
                        value = transcript.getMasterSchedule().getScheduleTerm().getCode();
                        if ((m_scheduleTermCodesMap != null) && m_scheduleTermCodesMap.containsKey(value) &&
                                (m_scheduleTermCodesMap.get(value).getStateCode() != null)) {
                            value = m_scheduleTermCodesMap.get(value).getStateCode();
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Validate cumulative credits attempted.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateCumulCreditsAttempted implements FieldValidator {
        protected static final String FIELD_CREDITS_EARNED = "CumCredEarned";
        protected static final String VAL_ID = "VAL_CREDIT_ATTEMPTED";

        /**
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
            String cumCreditsEarned = entity.getFieldValue(FIELD_CREDITS_EARNED);
            try {
                BigDecimal cumCreditsNumAtt = new BigDecimal(value);
                if (cumCreditsNumAtt.compareTo(BigDecimal.valueOf(0.1)) == -1 ||
                        cumCreditsNumAtt.compareTo(BigDecimal.valueOf(100.0)) == 1) {
                    errors.add(new StateReportValidationError(entity, field, "CG7116",
                            "Cumulative Credits Earned missing/invalid or value is outside valid range (with no more than 3 decimal places): "
                                    +
                                    value));
                }

                BigDecimal cumCreditsNumEarned = new BigDecimal(cumCreditsEarned);
                if (cumCreditsNumAtt.compareTo(cumCreditsNumEarned) == -1) {
                    errors.add(new StateReportValidationError(entity, field, "CG7117",
                            "Cumulative Credits Attempted must be greater than or equal to Cumulative Credits Earned: "
                                    +
                                    value));
                }
            } catch (NumberFormatException exception) {
                errors.add(new StateReportValidationError(entity, field, "CG7116",
                        "Cumulative Credits Earned missing/invalid or value is outside valid range (with no more than 3 decimal places): "
                                +
                                value));
            }

            return errors;
        }
    }

    /**
     * Validate cumulative credits earned.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateCumulCreditsEarned implements FieldValidator {
        protected static final String VAL_ID = "VAL_CREDIT_EARNED";

        /**
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

            try {
                BigDecimal cumCreditsNum = new BigDecimal(value);
                if (cumCreditsNum.compareTo(BigDecimal.ZERO) == -1 ||
                        cumCreditsNum.compareTo(BigDecimal.valueOf(70.0)) == 1) {
                    errors.add(new StateReportValidationError(entity, field, "CG7111",
                            "Cumulative Credits Earned missing/invalid or value is outside valid range (with no more than 3 decimal places): "
                                    +
                                    value));
                }
            } catch (NumberFormatException exception) {
                errors.add(new StateReportValidationError(entity, field, "CG7111",
                        "Cumulative Credits Earned missing/invalid or value is outside valid range (with no more than 3 decimal places): "
                                +
                                value));
            }
            return errors;
        }
    }

    /**
     * Returns a string heading to include at the top of an export file.
     *
     * @return String heading text
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue()) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * Return beanPath mapping for TranscriptDefinition.
     *
     * @param transcriptDefinitionOid String
     * @return Map
     */
    public Map<GradeTerm, String> getGradePathsMapping(String transcriptDefinitionOid) {
        return m_beanPaths.get(transcriptDefinitionOid);
    }

    /**
     * Reatrieve from StudentHistoryHelper List of Transcript for given student.
     *
     * @param student SisStudent
     * @return List<Transcript>
     */
    protected List<Transcript> getTranscriptsForStudent(SisStudent student) {
        return m_helper.getStudentTranscripts(student);
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * 
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();
        // Build helper object.
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_TRANSCRIPT, Boolean.TRUE);
        m_weightedOrUnweighted = PARAM_WEIGHTED.equals(getParameter(PARAM_GPA_TYPE));

        m_helper.getStudentTranscriptCriteria().addNotNull(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + Course.COL_CREDIT);

        m_helper.getStudentTranscriptCriteria()
                .addGreaterThan(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + Course.COL_CREDIT, Integer.valueOf(0));
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(NYStudentCreditGPAEntity.class);

            // Build maps of retriever functions
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_NAME_TRANCRIPT, new RetrieveTranscript());
            calcs.put(CALC_NAME_CALCULATION, new RetrieveCalculation());

            HashMap vals = new HashMap<String, FieldValidator>();
            vals.put(ValidateCumulCreditsEarned.VAL_ID, new ValidateCumulCreditsEarned());
            vals.put(ValidateCumulCreditsAttempted.VAL_ID, new ValidateCumulCreditsAttempted());

            addCalcs(calcs);
            addValidators(vals);
        }
        loadDictionarys();
        m_beanPaths = loadBeanPaths();
    }

    /**
     * Loads mapping of transcripts beanpaths and GradeTerms.
     *
     * @return HashMap
     */
    private HashMap<String, Map<GradeTerm, String>> loadBeanPaths() {
        HashMap<String, Map<GradeTerm, String>> tempMap = new HashMap<String, Map<GradeTerm, String>>();

        X2Criteria crit = new X2Criteria();
        QueryByCriteria query = new QueryByCriteria(GradeTermDefinition.class, crit);
        List<GradeTermDefinition> definitions = (List<GradeTermDefinition>) getBroker().getCollectionByQuery(query);
        for (GradeTermDefinition gradeTermDef : definitions) {
            List<GradeTerm> terms = (List<GradeTerm>) gradeTermDef.getGradeTerms();
            List<TranscriptDefinition> transDefs = (List<TranscriptDefinition>) gradeTermDef.getTranscriptDefinitions();
            for (TranscriptDefinition transDef : transDefs) {
                if (tempMap.get(transDef.getOid()) == null) {
                    tempMap.put(transDef.getOid(), new HashMap<GradeTerm, String>());
                }
                List<TranscriptColumnDefinition> colDefs =
                        (List<TranscriptColumnDefinition>) transDef.getTranscriptColumnDefinitions();
                for (GradeTerm term : terms) {
                    String termId = term.getGradeTermId();
                    for (TranscriptColumnDefinition col : colDefs) {
                        if (termId.equals(col.getGradeName())) {
                            String stdTrnBeanPath = col.getDataFieldConfig().getDataField().getJavaName();
                            tempMap.get(transDef.getOid()).put(term, stdTrnBeanPath);
                            break;
                        }
                    }
                }
            }
        }
        return tempMap;
    }

    /**
     * Load reference codes for GradeTerm.COL_GRADE_TERM_ID and ScheduleTerm.COL_CODE fields.
     */
    private void loadDictionarys() {
        // Fill grade term code reference code map if reference table exists
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField gradeField = dictionary.findDataDictionaryField(GradeTerm.class.getName(),
                GradeTerm.COL_GRADE_TERM_ID);
        if (gradeField != null) {
            ReferenceTable referenceTable = gradeField.getReferenceTable();
            if (referenceTable != null) {
                m_gradeTermCodesMap = referenceTable.getCodeMap();
            }
        }

        // Fill schedule term code reference code map if reference table exists
        DataDictionaryField scheduleField = dictionary.findDataDictionaryField(ScheduleTerm.class.getName(),
                ScheduleTerm.COL_CODE);
        if (scheduleField != null) {
            ReferenceTable referenceTable = scheduleField.getReferenceTable();
            if (referenceTable != null) {
                m_scheduleTermCodesMap = referenceTable.getCodeMap();
            }
        }
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
        m_creditGpaCode = translateAliasToJavaName(ALIAS_CREDIT_CODE, true);
        m_weightedCumGpa = translateAliasToJavaName(ALIAS_WEIGHTED_CUM_GPA, true);
        m_unweightedCumGpa = translateAliasToJavaName(ALIAS_UNWEIGHTED_CUM_GPA, true);
        m_markingPeriodCode = translateAliasToJavaName(ALIAS_MARKING_PERIOD_CODE, true);
    }
}
