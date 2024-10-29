/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.GradeTerm;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Massachusetts state report for the Grade Earned export.
 * This class implements the data export for the Grade Earned export.
 *
 * @author X2 Development Corporation
 */
public class GradeEarned extends RIStateReportData {
    /**
     * Implementation of StateReportEntity to be used by the Grade Earned export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class GradeEarnedEntity extends StateReportEntity {

        private GradeEarned m_data = null;
        private Transcript m_transcript = null;
        private List<TranscriptColumnDefinition> m_transcriptColumns = null;

        /**
         * Instantiates a new grade earned entity.
         */
        public GradeEarnedEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Transcript transcript = (Transcript) getBean();
            SisStudent student = transcript.getStudent();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    ", COURSE DESCRIPTION: " + transcript.getCourseDescription() +
                    "] ";

            return name;
        }

        /**
         * Gets the current TranscriptColumnDefinition.
         *
         * @return Transcript column definition
         */
        public TranscriptColumnDefinition getCurrentColumn() {
            return m_transcriptColumns.get(getCurrentRow());
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (GradeEarned) data;
            m_transcript = (Transcript) bean;
            m_transcriptColumns = new ArrayList<TranscriptColumnDefinition>();
            ScheduleTerm trm =
                    m_transcript.getMasterSchedule() != null ? m_transcript.getMasterSchedule().getScheduleTerm()
                            : null;
            if (trm != null) {
                KeyValuePair<PlainDate, PlainDate> trmDates = m_data.getDatesTRM(trm);
                if (trmDates != null) {
                    PlainDate trmStartDate = trmDates.getKey();
                    PlainDate trmEndDate = trmDates.getValue();
                    if (trmStartDate != null && trmEndDate != null) {
                        List<String> gtcOidsFromTrnDef = new ArrayList<String>();
                        for (TranscriptColumnDefinition gtc : m_transcript.getTranscriptDefinition()
                                .getTranscriptColumnDefinitions()) {
                            if (gtc.getGradeScale() != null) {
                                gtcOidsFromTrnDef.add(gtc.getOid());
                            }
                        }

                        X2Criteria gtcCriteria = new X2Criteria();
                        gtcCriteria.addIn(X2BaseBean.COL_OID, m_data.m_selectedTranscriptColumnDefinitionOids);
                        QueryByCriteria query = new QueryByCriteria(TranscriptColumnDefinition.class, gtcCriteria);
                        Collection<TranscriptColumnDefinition> gtcs = m_data.getBroker().getCollectionByQuery(query);

                        List<TranscriptColumnDefinition> gtcsToExport = new ArrayList<TranscriptColumnDefinition>();
                        for (TranscriptColumnDefinition gtc : gtcs) {
                            GradeTerm gtm = gtc.getGradeTerm();
                            KeyValuePair<PlainDate, PlainDate> gtmDates = m_data.getDatesGTM(gtm);
                            PlainDate gtmStartDate = null;
                            PlainDate gtmEndDate = null;
                            if (gtmDates != null) {
                                gtmStartDate = gtmDates.getKey();
                                gtmEndDate = gtmDates.getValue();
                            }
                            if ((gtmStartDate != null && gtmEndDate != null
                                    && DateUtils.isBetween(gtmStartDate, trmStartDate, trmEndDate)
                                    && DateUtils.isBetween(gtmEndDate, trmStartDate, trmEndDate))
                                    || SCHEDULE_TERM_CODE_FINAL
                                            .equals(gtc.getFieldValueByBeanPath(m_data.m_gtcGradingTermField))) {
                                if (gtcOidsFromTrnDef.contains(gtc.getOid())) {
                                    if (m_data.m_excludeEmptyGrades) {
                                        String gradeBeanPath = gtc.getTranscriptBeanAttribute();
                                        String value = (String) m_transcript.getFieldValueByBeanPath(gradeBeanPath);
                                        if (!StringUtils.isEmpty(value)) {
                                            gtcsToExport.add(gtc);
                                        }
                                    } else {
                                        gtcsToExport.add(gtc);
                                    }
                                }
                            }
                        }
                        m_transcriptColumns.addAll(gtcsToExport);

                        setRowCount(gtcsToExport.size());
                    }
                }
            } else {
                setRowCount(0);
            }
        }
    }

    /**
     * Returns trnTotalCredit value when Term Code = 'FINAL', otherwise, null
     *
     * @author Follett Software Company
     */
    protected class RetrieveCreditsReceived implements FieldRetriever {
        private static final String RETRIEVER_ID = "CREDITS_RECEIVED";
        private static final String TERM_CODE_FIELD = "TERMCODE";

        /**
         * Gets the field value.
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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Transcript trns = (Transcript) entity.getBean();
            Object value = null;
            if (SCHEDULE_TERM_CODE_FINAL.equals(entity.getFieldValue(TERM_CODE_FIELD))) {
                value = trns.getTotalCredit();
            }
            return value;
        }
    }

    /**
     * The Class RetrieveGradeValue.
     */
    protected class RetrieveGradeValue implements FieldRetriever {
        private String PARAM_LETTER_GRADE = "LETTER_GRADE";
        private String PARAM_NUMERIC_GRADE = "NUMERIC_GRADE";
        private static final String RETRIEVER_ID = "GRADE_VALUE";

        private GradesManager m_gradesManager;
        private DecimalFormat m_format = new DecimalFormat("##0.00");

        /**
         * Constructor. Initialize GradesManager and grade scales map
         */
        public RetrieveGradeValue() {
            m_gradesManager = new GradesManager(getBroker());
        }

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
            Transcript transcript = (Transcript) entity.getBean();
            GradeEarnedEntity gradeEntity = (GradeEarnedEntity) entity;
            TranscriptColumnDefinition gtc = gradeEntity.getCurrentColumn();
            String gradeBeanPath = gtc.getTranscriptBeanAttribute();

            String value = (String) transcript.getFieldValueByBeanPath(gradeBeanPath);
            String param = (String) field.getParameter();
            if (PARAM_LETTER_GRADE.equals(param)) {
                value = getLetterGrade(transcript, gtc.getGradeScale(), value);
            } else if (PARAM_NUMERIC_GRADE.equals(param)) {
                value = getNumericGrade(transcript, gtc.getGradeScale(), value);
            }
            return value;
        }

        /**
         * Gets the letter grade.
         *
         * @param transcript Transcript
         * @param grade String
         * @return String
         */
        private String getLetterGrade(Transcript transcript, GradeScale scale, String grade) {
            String value = grade;

            SisSchool school = transcript.getSchool();
            String schoolCourseOid = getSchoolCourseOid(transcript);
            String letterGrade = null;
            if (StringUtils.isNumeric(value)) {

                if (!StringUtils.isEmpty(grade) && scale != null) {
                    BigDecimal gradeAsNumber = null;
                    try {
                        gradeAsNumber = new BigDecimal(grade);
                    } catch (NumberFormatException nfe) {
                        // nothing. The grade is not numeric.
                    }

                    if (gradeAsNumber != null) {
                        letterGrade = m_gradesManager.getLetterValue(gradeAsNumber, scale, school, schoolCourseOid);
                        if (!StringUtils.isEmpty(letterGrade)) {
                            letterGrade = lookupStateValue(GradeScaleGradeDefinition.class,
                                    GradeScaleGradeDefinition.COL_GRADE_CODE,
                                    letterGrade);
                        }
                    }

                    if (!StringUtils.isEmpty(letterGrade)) {
                        value = letterGrade;
                    }
                }
            } else {
                GradeScaleGradeDefinition gsgLetter =
                        m_gradesManager.getGradeDefinition(grade, scale, school.getOid(), schoolCourseOid);
                if (gsgLetter != null) {
                    letterGrade = lookupStateValue(GradeScaleGradeDefinition.class,
                            GradeScaleGradeDefinition.COL_GRADE_CODE,
                            gsgLetter.getGradeCode());
                }
                if (!StringUtils.isEmpty(letterGrade)) {
                    value = letterGrade;
                }
            }
            return value;
        }

        /**
         * Gets the numeric grade.
         *
         * @param transcript Transcript
         * @param grade String
         * @return String
         */
        private String getNumericGrade(Transcript transcript, GradeScale scale, String grade) {
            BigDecimal numericGrade = null;

            if (!StringUtils.isEmpty(grade) && scale != null) {
                // If the final grade is a letter value, convert it to a number.
                SisSchool school = transcript.getSchool();
                String schoolCourseOid = getSchoolCourseOid(transcript);
                numericGrade = m_gradesManager.getNumericValue(grade, scale, school, schoolCourseOid);
            }
            if (numericGrade == null && StringUtils.isNumeric(grade)) {
                // Interpret directly as a numeric grade.
                try {
                    numericGrade = new BigDecimal(grade);
                } catch (NumberFormatException nfe) {
                    // nothing. The grade is not numeric.
                }
            }

            if (numericGrade != null && numericGrade.compareTo(new BigDecimal(10.00)) < 0) {
                return m_format.format(numericGrade);
            }
            return numericGrade != null ? m_format.format(numericGrade).substring(0, 5) : null;
        }

        /**
         * Find the SchoolCourse for a transcript.
         *
         * @param transcript
         *
         * @return String
         */
        private String getSchoolCourseOid(Transcript transcript) {
            String schoolCourseOid = null;
            if (transcript.getMasterSchedule() != null) {
                schoolCourseOid = transcript.getMasterSchedule().getSchoolCourseOid();
            }
            if (schoolCourseOid == null) {
                schoolCourseOid = transcript.getEquivalentSchoolCourseOid();
            }
            if (schoolCourseOid == null) {
                schoolCourseOid = transcript.getSchoolCourseOid();
            }
            return schoolCourseOid;
        }
    }

    /**
     * The Class RetrieveGradeValue.
     */
    protected class RetrieveTermCode implements FieldRetriever {
        private static final String RETRIEVER_ID = "TERM_CODE";


        /**
         * Constructor
         */
        public RetrieveTermCode() {}

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
            GradeEarnedEntity gradeEntity = (GradeEarnedEntity) entity;
            TranscriptColumnDefinition gtc = gradeEntity.getCurrentColumn();
            return gtc.getFieldValueByBeanPath(m_gtcGradingTermField);
        }
    }

    /**
     * ValidateNumericValue validates that a numeric grade is greater than or equal to 0 and less
     * than or equal
     * to 100.
     *
     * @author Follett Software Company
     */
    protected class ValidateNumericGradeEarned implements FieldValidator {

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

            String stringGradeValue = entity.getFieldValue(FIELD_NUMERIC_GRADE_EARNED);

            if (!StringUtils.isEmpty(stringGradeValue)) {

                BigDecimal numericGradeValue = new BigDecimal(stringGradeValue);

                if (numericGradeValue.compareTo(MAX_GRADE_VALUE) == 1 ||
                        numericGradeValue.compareTo(MIN_GRADE_VALUE) == -1) {
                    errors.add(new StateReportValidationError(entity, field,
                            "NUMERICGRADEEARNED grade must be greater than or equal to 0 and less than or equal to 100",
                            "NUMERICGRADEEARNED=" + STYLE_BOLD + numericGradeValue + STYLE_END));
                }

            }

            return errors;
        }

    }

    /**
     * ValidateGradeExists validates that either a letter or numeric grade exist.
     *
     * @author Follett Software Company
     */
    protected class ValidateGradeExists implements FieldValidator {

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

            String numericGradeValue = entity.getFieldValue(FIELD_NUMERIC_GRADE_EARNED);
            String letterGradeValue = entity.getFieldValue(FIELD_LETTER_GRADE_EARNED);

            if (StringUtils.isEmpty(numericGradeValue) && StringUtils.isEmpty(letterGradeValue)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Either NUMERICGRADEEARNED or LETTERGRADEEARNED are required",
                        "NUMERICGRADEEARNED=" + STYLE_BOLD + numericGradeValue + STYLE_END + ", " +
                                "LETTERGRADEEARNED=" + STYLE_BOLD + letterGradeValue + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Aliases
     */
    private static final String ALIAS_DOE_EXCLUDE_STD = "DOE EXCLUDE STD";
    private static final String ALIAS_DOE_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    private static final String ALIAS_GTC_GRADING_TRM = "all-gtc-GradingTerm";

    /**
     * Input parameters
     */
    private static final String PARAM_GCD_OIDS = "transcriptColumnDefinitionOids";
    private static final String PARAM_EXCLUDE_EMPTY_GRADES = "excludeEmptyGrades";

    /**
     * Values needed for validations
     */
    protected static final String FIELD_NUMERIC_GRADE_EARNED = "NUMERICGRADEEARNED";
    protected static final String FIELD_LETTER_GRADE_EARNED = "LETTERGRADEEARNED";
    protected static final BigDecimal MAX_GRADE_VALUE = new BigDecimal(100);
    protected static final BigDecimal MIN_GRADE_VALUE = BigDecimal.ZERO;
    protected static final String SCHEDULE_TERM_CODE_FINAL = "FINAL";

    /**
     * Instance variables.
     */
    protected Map<String, KeyValuePair<PlainDate, PlainDate>> m_datesGTM =
            new HashMap<String, KeyValuePair<PlainDate, PlainDate>>();
    protected Map<String, KeyValuePair<PlainDate, PlainDate>> m_datesTRM =
            new HashMap<String, KeyValuePair<PlainDate, PlainDate>>();
    protected String m_excludeStdField = null;
    protected String m_excludeCrsField = null;
    protected String m_gtcGradingTermField = null;
    protected List<String> m_selectedTranscriptColumnDefinitionOids;
    protected List<String> m_gradeLevels = new ArrayList<>();
    protected boolean m_excludeEmptyGrades;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        m_excludeStdField = translateAliasToJavaName(ALIAS_DOE_EXCLUDE_STD, false);
        m_excludeCrsField = translateAliasToJavaName(ALIAS_DOE_EXCLUDE_CRS, false);
        m_gtcGradingTermField = translateAliasToJavaName(ALIAS_GTC_GRADING_TRM, false);
        m_selectedTranscriptColumnDefinitionOids =
                StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_GCD_OIDS), ",");
        m_excludeEmptyGrades = (Boolean) getParameter(PARAM_EXCLUDE_EMPTY_GRADES);
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            loadGradeLevelRefList();
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria transcriptCriteria = getTranscriptCriteria();
            QueryByCriteria transcriptQuery = new QueryByCriteria(Transcript.class, transcriptCriteria);

            applyInputSort(transcriptQuery, Transcript.REL_STUDENT);

            // Set the query to be used for student selection.
            setQuery(transcriptQuery);
            setEntityClass(GradeEarnedEntity.class);

            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveGradeValue.RETRIEVER_ID, new RetrieveGradeValue());
            calcs.put(RetrieveTermCode.RETRIEVER_ID, new RetrieveTermCode());
            calcs.put(RetrieveCreditsReceived.RETRIEVER_ID, new RetrieveCreditsReceived());

            addCalcs(calcs);

            // Build maps of validator functions
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("GRADE_VALID", new ValidateNumericGradeEarned());
            validators.put("GRADE_EXISTS", new ValidateGradeExists());

            addValidators(validators);
        }
    }

    /**
     * Gets the dates GTM.
     *
     * @param gtm GradeTerm
     * @return Key value pair
     */
    private KeyValuePair<PlainDate, PlainDate> getDatesGTM(GradeTerm gtm) {
        if (gtm != null) {
            if (!m_datesGTM.containsKey(gtm.getGradeTermId())) {
                PlainDate gtmLastDate = gtm.getGradeTermDates()
                        .stream()
                        .map(gta -> gta.getEndDate())
                        .max(Comparator.naturalOrder()).orElse(null);

                PlainDate gtmFirstDate = gtm.getGradeTermDates(getBroker())
                        .stream()
                        .map(gta -> gta.getStartDate())
                        .max(Comparator.naturalOrder()).orElse(null);
                if (gtmLastDate != null && gtmFirstDate != null) {
                    KeyValuePair<PlainDate, PlainDate> datesToPut = new KeyValuePair(gtmFirstDate, gtmLastDate);
                    m_datesTRM.put(gtm.getGradeTermId(), datesToPut);
                    return datesToPut;
                }
            } else {
                return m_datesTRM.get(gtm.getGradeTermId());
            }
        }
        return null;

    }

    /**
     * Gets the dates TRM.
     *
     * @param trm ScheduleTerm
     * @return Key value pair
     */
    private KeyValuePair<PlainDate, PlainDate> getDatesTRM(ScheduleTerm trm) {
        if (trm != null) {
            if (!m_datesTRM.containsKey(trm.getCode())) {
                PlainDate trmLastDate = trm.getScheduleTermDates(getBroker())
                        .stream()
                        .map(tmd -> tmd.getEndDate())
                        .max(Comparator.naturalOrder()).orElse(null);

                PlainDate trmFirstDate = trm.getScheduleTermDates(getBroker())
                        .stream()
                        .map(tmd -> tmd.getStartDate())
                        .max(Comparator.naturalOrder()).orElse(null);
                if (trmLastDate != null && trmFirstDate != null) {
                    KeyValuePair<PlainDate, PlainDate> datesToPut = new KeyValuePair(trmFirstDate, trmLastDate);
                    m_datesTRM.put(trm.getCode(), datesToPut);
                    return datesToPut;
                }
            } else {
                return m_datesTRM.get(trm.getCode());
            }
        }
        return null;
    }

    /**
     * Returns the criteria that retrieves all transcripts that should be
     * included in the export.
     *
     * @return Criteria
     */

    private X2Criteria getTranscriptCriteria() {
        X2Criteria transcriptCriteria = new X2Criteria();



        // transcriptCriteria.addEqualTo(SchoolCourse.COL_MASTER_TYPE, "Class");

        if (isSchoolContext()) {
            transcriptCriteria.addEqualTo(Transcript.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        // Check if the course exclusion custom field is present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            transcriptCriteria.addNotEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }
        transcriptCriteria.addIn(Transcript.REL_STUDENT + PATH_DELIMITER + Student.COL_GRADE_LEVEL, m_gradeLevels);

        applyInputCriteria(transcriptCriteria, false, Transcript.REL_STUDENT);


        transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                + PATH_DELIMITER + m_excludeCrsField, BooleanAsStringConverter.TRUE);
        transcriptCriteria.addNotEmpty(Transcript.REL_MASTER_SCHEDULE, getBroker().getPersistenceKey());

        return transcriptCriteria;
    }

    /**
     * Load grade level ref map.
     */
    private void loadGradeLevelRefList() {
        ReferenceTable gradeLevelRefTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                ReferenceTable.REF_TABLE_OID_GRADE_LEVEL);
        Map<String, ReferenceCode> gradeLevelMap = gradeLevelRefTable.getCodeMap();

        ExtendedDataDictionary gradeLevelDdx = gradeLevelRefTable.getExtendedDataDictionary();
        DataDictionary gradeLevelDdxDictionary =
                DataDictionary.getDistrictDictionary(gradeLevelDdx, getBroker().getPersistenceKey());
        DataDictionaryField field = gradeLevelDdxDictionary.findDataDictionaryFieldByAlias("NumericGradeLevel");

        List<String> gradeLevels = new ArrayList<String>();
        for (ReferenceCode code : gradeLevelMap.values()) {
            if (field != null && !StringUtils.isBlank(field.getJavaName())) {
                String numericLevel = (String) code.getFieldValueByBeanPath(field.getJavaName());
                if (!StringUtils.isBlank(numericLevel)) {
                    Integer grade = Integer.parseInt(numericLevel);
                    if (grade >= 6 && grade <= 12) {
                        gradeLevels.add(code.getCode());
                    }
                }
            }
        }
        m_gradeLevels.addAll(gradeLevels);
    }
}
