/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ca;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.AssessmentColumnDefinition;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class CAPreIDAssessment.
 */
public class CAPreIDAssessment extends StateReportData {

    /**
     * The Class CAPreIDAssessmentEntity.
     */
    public static class CAPreIDAssessmentEntity extends StateReportEntity {
        CAPreIDAssessment m_data;

        /**
         * Instantiates a new CA pre ID assessment entity.
         */
        public CAPreIDAssessmentEntity() {
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
                    "] ";
            return name;
        }

        /**
         * Intitialize.
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

            m_data = (CAPreIDAssessment) data;
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
     * The Class AssessmentHelper.
     */
    protected class AssessmentHelper {
        protected static final String CELDT_LISTENING = "CELDT Listening";
        protected static final String CELDT_OVERALL = "CELDT Overall";
        protected static final String CELDT_SPEAKING = "CELDT Speaking";
        protected static final String CELDT_READING = "CELDT Reading";
        protected static final String CELDT_WRITING = "CELDT Writing";

        PlainDate m_date;
        String m_gradeLevel;
        Map<String, BigDecimal> m_scores = new HashMap();

        /**
         * Adds assessment.
         *
         * @param data CAPreIDAssessment
         * @param assessment StudentAssessment
         */
        void addAssessment(CAPreIDAssessment data, StudentAssessment assessment) {
            if (assessment.getDate() != null) {
                if (m_date == null) {
                    m_date = assessment.getDate();
                    m_gradeLevel =
                            data.lookupStateValue(StudentAssessment.class, StudentAssessment.COL_GRADE_LEVEL_CODE,
                                    assessment.getGradeLevelCode());
                }
                if (assessment.getDate().equals(m_date)) {
                    String beanPath = data.getScoreColumn(assessment);
                    if (beanPath != null) {
                        String score = (String) assessment.getFieldValueByBeanPath(beanPath);
                        if (!StringUtils.isEmpty(score)) {
                            m_scores.put(assessment.getAssessmentDefinition().getName(), new BigDecimal(score));
                        }
                    }
                }
            }
        }

        /**
         * Returns date.
         *
         * @return Plain date
         */
        protected PlainDate getDate() {
            return m_date;
        }

        /**
         * Returns grade level.
         *
         * @return String
         */
        protected String getGrade() {
            return m_gradeLevel;
        }

        /**
         * Returns scores by assessment.
         *
         * @param assessment String
         * @return Big decimal
         */
        protected BigDecimal getScore(String assessment) {
            return m_scores.get(assessment);
        }
    }

    /**
     * Class to retrieve assessment date.
     */
    protected class RetrieveDate implements FieldRetriever {
        protected static final String RETRIEVER_ID = "CELDT_DATE";

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            CAPreIDAssessment data = (CAPreIDAssessment) dataDefault;

            return data.getAssessmentDate(student);
        }
    }

    /**
     * Class to retrieve assessment date.
     */
    protected class RetrieveGrade implements FieldRetriever {
        protected static final String RETRIEVER_ID = "CELDT_GRADE";

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            CAPreIDAssessment data = (CAPreIDAssessment) dataDefault;

            return data.getAssessmentGrade(student);
        }
    }

    /**
     * Class to retrieve assessment purpose.
     */
    protected class RetrievePurpose implements FieldRetriever {
        protected static final String CODE_INITIAL = "1";
        protected static final String CODE_ANNUAL = "2";
        protected static final String RETRIEVER_ID = "CELDT_PURPOSE";

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            CAPreIDAssessment data = (CAPreIDAssessment) dataDefault;

            PlainDate date = data.getAssessmentDate(student);
            return date == null ? CODE_INITIAL : CODE_ANNUAL;
        }
    }

    /**
     * Class to retrieve assessment score.
     */
    protected class RetrieveScore implements FieldRetriever {
        protected static final String RETRIEVER_ID = "CELDT_SCORE";

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            CAPreIDAssessment data = (CAPreIDAssessment) dataDefault;

            return data.getAssessmentScore(student, (String) field.getParameter());
        }
    }

    /**
     * Class to retrieve assessment score.
     */
    protected class RetrieveTruncate implements FieldRetriever {
        protected static final String RETRIEVER_ID = "CELDT_TRUNCATE";

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            try {
                int max = Integer.parseInt((String) field.getParameter());
                if (value != null && max > 0 && max < value.length()) {
                    value = value.substring(0, max);
                }
            } catch (NumberFormatException e) {
                // Improper parse value
            }
            return value;
        }
    }

    /**
     * The Class ValidateDistrictCode.
     */
    protected class ValidateDistrictCode implements FieldValidator {
        protected static final String VALIDATOR_ID = "DISTRICT_CODE_VAL";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
            }

            if (errors.size() == 0 && value.length() > 1) {
                String county = value.substring(0, 2);
                int countyNumber = -1;
                try {
                    countyNumber = Integer.parseInt(county);
                } catch (NumberFormatException e) {
                    //
                }
                if (!county.matches("\\d{2}") || countyNumber < 1 || (countyNumber > 58 && countyNumber != 99)) {
                    errors.add(new StateReportValidationError(entity, field, "Wrong format",
                            "Columns 2-3: County Code, Alpha 01-58 or 99"));
                }
            }
            if (errors.size() == 0 && value.length() > 6) {
                String districtCode = value.substring(2, 7);
                if (!districtCode.matches("\\d{5}")) {
                    errors.add(new StateReportValidationError(entity, field, "Wrong format",
                            "Columns 4-8: County Code, Alpha 00000 - 99999"));
                    return errors;
                }
            }

            return errors;
        }
    }

    /**
     * The Class ValidateEmptyValue.
     */
    protected class ValidateEmptyValue implements FieldValidator {
        protected static final String VALIDATOR_ID = "EMPTY_VAL";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
                return errors;
            }
            return errors;
        }
    }

    /**
     * The Class ValidateScores.
     */
    protected class ValidateScores implements FieldValidator {

        protected static final String VALIDATOR_ID = "SCORE_VAL";
        private static final String FIELD_TEST_PURPOSE = "Test Purpose";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String parameter = (String) field.getParameter();
            String testPurpose = entity.getFieldValue(FIELD_TEST_PURPOSE);

            if (RetrievePurpose.CODE_INITIAL.equals(testPurpose) && !StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Error",
                        "When test puprose is \"1\" value should be blank."));
                return errors;
            }

            if (RetrievePurpose.CODE_ANNUAL.equals(testPurpose) && value == null) {
                errors.add(new StateReportValidationError(entity, field, "Error",
                        "When test puprose is \"2\" value should not be blank."));
                return errors;
            }
            if (RetrievePurpose.CODE_ANNUAL.equals(testPurpose) && value.matches("\\d{3}")) {
                if (AssessmentHelper.CELDT_LISTENING.equals(parameter)) {
                    int lScore = Integer.valueOf(value).intValue();
                    if (lScore < 220 || lScore > 725) {
                        errors.add(new StateReportValidationError(entity, field, "Wrong format",
                                "Numeric; 220-725 or Blank"));
                    }
                    return errors;
                } else if (AssessmentHelper.CELDT_SPEAKING.equals(parameter)) {
                    int sScore = Integer.valueOf(value).intValue();
                    if (sScore < 140 || sScore > 740) {
                        errors.add(new StateReportValidationError(entity, field, "Wrong format",
                                "Numeric; 140-740 or Blank"));
                    }
                    return errors;
                } else if (AssessmentHelper.CELDT_READING.equals(parameter)) {
                    int rScore = Integer.valueOf(value).intValue();
                    if (rScore < 220 || rScore > 770) {
                        errors.add(new StateReportValidationError(entity, field, "Wrong format",
                                "Numeric; 220-770 or Blank"));
                    }
                    return errors;
                } else if (AssessmentHelper.CELDT_WRITING.equals(parameter)) {
                    int wScore = Integer.valueOf(value).intValue();
                    if (wScore < 220 || wScore > 810) {
                        errors.add(new StateReportValidationError(entity, field, "Wrong format",
                                "Numeric; 220-810 or Blank"));
                    }
                    return errors;
                } else if (AssessmentHelper.CELDT_OVERALL.equals(parameter)) {
                    int oScore = Integer.valueOf(value).intValue();
                    if (oScore < 180 || oScore > 761) {
                        errors.add(new StateReportValidationError(entity, field, "Wrong format",
                                "Numeric; 180-761 or Blank"));
                    }
                    return errors;
                }
            } else if (!RetrievePurpose.CODE_INITIAL.equals(testPurpose)) {
                errors.add(new StateReportValidationError(entity, field, "Wrong format", "Should be numeric"));
            }

            return errors;
        }
    }

    /*
     * Constants for history helper parameters from user input template.
     */
    protected static final String PARAM_REPORT_DATE = "reportDate";

    private static final String ALIAS_ELA_STATUS = "DOE ELA STATUS CODE";
    private static final String ALIAS_PRIMARY_LANGUAGE = "DOE PRIMARY LANGUAGE";
    private static final List<String> ASSESSMENT_NAMES = Arrays.asList(new String[] {AssessmentHelper.CELDT_LISTENING,
            AssessmentHelper.CELDT_OVERALL, AssessmentHelper.CELDT_READING,
            AssessmentHelper.CELDT_SPEAKING, AssessmentHelper.CELDT_WRITING});
    private static final String CODE_EL = "EL";
    private static final String CODE_ENGLISH = "00";
    private static final String CODE_TBD = "TBD";
    private static final String SCORE_COL_NAME = "Raw score";

    protected Map<String, String> m_assessmentScorePaths = new HashMap();
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;

    private String m_fieldELAStatus;
    private String m_fieldLanguage;
    private Map<String, AssessmentHelper> m_studentAssessments;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        }
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getParameter(PARAM_REPORT_DATE));
        m_fieldELAStatus = translateAliasToJavaName(ALIAS_ELA_STATUS, true);
        m_fieldLanguage = translateAliasToJavaName(ALIAS_PRIMARY_LANGUAGE, true);
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            updateCriteria();

            loadAssessments();

            setEntityClass(CAPreIDAssessmentEntity.class);

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveDate.RETRIEVER_ID, new RetrieveDate());
            calcs.put(RetrieveGrade.RETRIEVER_ID, new RetrieveGrade());
            calcs.put(RetrievePurpose.RETRIEVER_ID, new RetrievePurpose());
            calcs.put(RetrieveScore.RETRIEVER_ID, new RetrieveScore());
            calcs.put(RetrieveTruncate.RETRIEVER_ID, new RetrieveTruncate());
            super.addCalcs(calcs);

            // Build a map of validators
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(ValidateDistrictCode.VALIDATOR_ID, new ValidateDistrictCode());
            validators.put(ValidateScores.VALIDATOR_ID, new ValidateScores());
            validators.put(ValidateEmptyValue.VALIDATOR_ID, new ValidateEmptyValue());
            super.addValidators(validators);
        }
    }

    /**
     * Returns assessment date by student.
     *
     * @param student SisStudent
     * @return Plain date
     */
    protected PlainDate getAssessmentDate(SisStudent student) {
        PlainDate date = null;
        AssessmentHelper helper = m_studentAssessments.get(student.getOid());
        if (helper != null) {
            date = helper.getDate();
        }
        return date;
    }

    /**
     * Returns assessment grade by student.
     *
     * @param student SisStudent
     * @return String
     */
    protected String getAssessmentGrade(SisStudent student) {
        String grade = null;
        AssessmentHelper helper = m_studentAssessments.get(student.getOid());
        if (helper != null) {
            grade = helper.getGrade();
        }
        return grade;
    }

    /**
     * Returns assessment score by student and assessment.
     *
     * @param student SisStudent
     * @param assessment String
     * @return Big decimal
     */
    protected BigDecimal getAssessmentScore(SisStudent student, String assessment) {
        BigDecimal score = null;
        AssessmentHelper helper = m_studentAssessments.get(student.getOid());
        if (helper != null) {
            score = helper.getScore(assessment);
        }
        return score;
    }

    /**
     * Returns score column by assessment.
     *
     * @param assessment StudentAssessment
     * @return String
     */
    protected String getScoreColumn(StudentAssessment assessment) {
        String beanPath = null;

        if (m_assessmentScorePaths.containsKey(assessment.getAssessmentDefinitionOid())) {
            beanPath = m_assessmentScorePaths.get(assessment.getAssessmentDefinitionOid());
        } else {
            for (AssessmentColumnDefinition column : assessment.getAssessmentDefinition()
                    .getAssessmentColumnDefinitions(getBroker())) {
                if (SCORE_COL_NAME.equals(column.getUserLongName())) {
                    beanPath = column.getDataFieldConfig().getDataField().getJavaName();
                }
            }
            m_assessmentScorePaths.put(assessment.getAssessmentDefinitionOid(), beanPath);
        }
        return beanPath;
    }

    /**
     * Returns reference codes by state code.
     *
     * @param field DataDictionaryField
     * @param stateCode String
     * @return List
     */
    private List<String> getReferenceCodes(DataDictionaryField field, String stateCode) {
        List<String> values = new LinkedList<String>();

        if (field.getReferenceTable() != null) {
            for (ReferenceCode code : field.getReferenceTable().getReferenceCodes(getBroker())) {
                if (stateCode.equals(code.getStateCode())) {
                    values.add(code.getCode());
                }
            }
        }
        return values;
    }

    /**
     * Loads assessments.
     */
    private void loadAssessments() {
        m_studentAssessments = new HashMap();

        X2Criteria criteria = new X2Criteria();
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
        criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
        criteria.addIn(StudentAssessment.REL_ASSESSMENT_DEFINITION + "." + AssessmentDefinition.COL_NAME,
                ASSESSMENT_NAMES);
        QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, criteria);
        query.addOrderByAscending(StudentAssessment.COL_STUDENT_OID);
        query.addOrderByDescending(StudentAssessment.COL_DATE);

        QueryIterator assessments = getBroker().getIteratorByQuery(query);
        try {
            while (assessments.hasNext()) {
                StudentAssessment assessment = (StudentAssessment) assessments.next();
                AssessmentHelper helper = m_studentAssessments.get(assessment.getStudentOid());
                if (helper == null) {
                    helper = new AssessmentHelper();
                    m_studentAssessments.put(assessment.getStudentOid(), helper);
                }
                helper.addAssessment(this, assessment);
            }
        } finally {
            if (assessments != null) {
                assessments.close();
            }
        }
    }

    /**
     * Updates student criteria.
     */
    private void updateCriteria() {
        X2Criteria criteria = new X2Criteria();
        DataDictionaryField statusField = getDataDictionaryField(SisStudent.class, m_fieldELAStatus);
        DataDictionaryField languageField = getDataDictionaryField(SisStudent.class, m_fieldLanguage);
        if (statusField != null && statusField.hasReferenceTable() && languageField != null &&
                languageField.hasReferenceTable()) {
            X2Criteria criteriaEnglishLearner = new X2Criteria();
            criteriaEnglishLearner.addIn(m_fieldELAStatus, getReferenceCodes(statusField, CODE_EL));
            X2Criteria criteriaToBeDetermined = new X2Criteria();
            criteriaToBeDetermined.addIn(m_fieldELAStatus, getReferenceCodes(statusField, CODE_TBD));
            X2Criteria criteriaStatusEmpty = new X2Criteria();
            criteriaStatusEmpty.addEmpty(m_fieldELAStatus, getBroker().getPersistenceKey());
            X2Criteria criteriaNotEnglish = new X2Criteria();
            criteriaNotEnglish.addNotIn(m_fieldLanguage, getReferenceCodes(languageField, CODE_ENGLISH));

            X2Criteria criteriaOrStatus = new X2Criteria();
            criteriaOrStatus.addOrCriteria(criteriaToBeDetermined);
            criteriaOrStatus.addOrCriteria(criteriaStatusEmpty);
            X2Criteria criteriaOtherIncluded = new X2Criteria();
            criteriaOtherIncluded.addAndCriteria(criteriaOrStatus);
            criteriaOtherIncluded.addAndCriteria(criteriaNotEnglish);

            criteria.addOrCriteria(criteriaEnglishLearner);
            criteria.addOrCriteria(criteriaOtherIncluded);
        } else {
            criteria.addEqualTo(X2BaseBean.COL_OID, "__dummy__");
        }
        m_helper.getStudentCriteria().addAndCriteria(criteria);
    }

}
