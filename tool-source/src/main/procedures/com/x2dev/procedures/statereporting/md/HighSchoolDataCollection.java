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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.KeyValueTrio;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.MessageFormat;
import java.util.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class HighSchoolDataCollection.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class HighSchoolDataCollection extends MDStudentReportData {

    /**
     * The Enum ComparisonType.
     */
    public enum ComparisonType {
        EQUALS, NOT_EQUALS, GREATER_THAN, GREATER_THAN_OR_EQUALS, LESS_THAN, LESS_THAN_OR_EQUALS, IN_LIST, LIKE;

        /**
         * Adds the qualifying criteria.
         *
         * @param criteria X2Criteria
         * @param qualifyingCriteria List<KeyValueTrio>
         */
        static void addQualifyingCriteria(X2Criteria criteria, List<KeyValueTrio> qualifyingCriteria) {
            for (KeyValueTrio pair : qualifyingCriteria) {
                ComparisonType comparison = (ComparisonType) pair.getValue2();
                String beanPath = (String) pair.getKey();
                Object value = pair.getValue1();
                if (!StringUtils.isEmpty(beanPath)) {
                    ComparisonType.addToCriteria(criteria, comparison, beanPath, value);
                }
            }
        }

        /**
         * Adds the comparison value to criteria.
         *
         * @param criteria X2Criteria
         * @param comparison ComparisonType
         * @param beanPath String
         * @param value Object
         */
        static void addToCriteria(X2Criteria criteria, ComparisonType comparison, String beanPath, Object value) {
            switch (comparison) {
                case EQUALS:
                    criteria.addEqualTo(beanPath, value);
                    break;
                case NOT_EQUALS:
                    criteria.addNotEqualTo(beanPath, value);
                    break;
                case GREATER_THAN_OR_EQUALS:
                    criteria.addGreaterOrEqualThan(beanPath, value);
                    break;
                case GREATER_THAN:
                    criteria.addGreaterThan(beanPath, value);
                    break;
                case LESS_THAN_OR_EQUALS:
                    criteria.addLessOrEqualThan(beanPath, value);
                    break;
                case LESS_THAN:
                    criteria.addLessThan(beanPath, value);
                    break;
                case IN_LIST:
                    criteria.addIn(beanPath, (Collection) value);
                    break;
                case LIKE:
                    criteria.addLike(beanPath, value);
                    break;
                default:
                    break;
            }
        }

        /**
         * Populate the qualified list.
         *
         * @param broker X2Broker
         * @param beanClass Class
         * @param criteria X2Criteria
         * @param field String
         * @param qualifiedList Set<String>
         */
        public static void populateQualifiedList(X2Broker broker,
                                                 Class beanClass,
                                                 X2Criteria criteria,
                                                 String field,
                                                 Set<String> qualifiedList) {
            String[] columns = new String[] {field};
            ColumnQuery query = new ColumnQuery(beanClass, columns, criteria);
            try (QueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    qualifiedList.add((String) row[0]);
                }
            }
        }
    }

    /**
     * The Interface AssessmentQualifier is used to evaluate assessments for a set of
     * characteristics.
     */
    public interface AssessmentQualifier {

        /**
         * Checks if is qualified.
         *
         * @param asmList List<StudentAssessment>
         * @return true, if is qualified
         */
        boolean isQualified(List<StudentAssessment> asmList);
    }

    /**
     * The Class AssessmentQualifierPSSAT.
     */
    public static class AssessmentQualifierPSSAT implements AssessmentQualifier {
        private static final BigDecimal SCORE_MIN_MATH = BigDecimal.valueOf(530.0);
        private static final BigDecimal SCORE_MIN_READING = BigDecimal.valueOf(480.0);

        private String m_fieldScoreMath;
        private String m_fieldScoreReading;


        /**
         * Instantiates a new assessment qualifier PSSAT.
         *
         * @param fieldScoreMath String
         * @param fieldScoreReading String
         */
        public AssessmentQualifierPSSAT(String fieldScoreMath, String fieldScoreReading) {
            super();
            m_fieldScoreMath = fieldScoreMath;
            m_fieldScoreReading = fieldScoreReading;
        }


        /**
         * Evaluates list of assessments to determine if the highest math and reading scores are
         * qualified.
         *
         * @param asmList List<StudentAssessment>
         * @return true, if is qualified
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.AssessmentQualifier#isQualified(java.util.List)
         */
        @Override
        public boolean isQualified(List<StudentAssessment> asmList) {
            boolean result = false;
            BigDecimal maxMathScore = BigDecimal.ZERO;
            BigDecimal maxReadingScore = BigDecimal.ZERO;
            for (StudentAssessment asm : asmList) {
                BigDecimal mathScore = getBigDecimalValue(asm.getFieldValueByBeanPath(m_fieldScoreMath));
                BigDecimal readingScore = getBigDecimalValue(asm.getFieldValueByBeanPath(m_fieldScoreReading));
                if (mathScore.compareTo(maxMathScore) > 0) {
                    maxMathScore = mathScore;
                }
                if (readingScore.compareTo(maxReadingScore) > 0) {
                    maxReadingScore = readingScore;
                }
            }
            if (maxMathScore.compareTo(SCORE_MIN_MATH) >= 0 && maxReadingScore.compareTo(SCORE_MIN_READING) >= 0) {
                result = true;
            }
            return result;
        }


        /**
         * Gets the big decimal value.
         *
         * @param value Object
         * @return Big decimal
         */
        private BigDecimal getBigDecimalValue(Object value) {
            BigDecimal result = BigDecimal.ZERO;
            if (value != null) {
                try {
                    result = new BigDecimal(value.toString());
                } catch (Exception e) {
                    // Do nothing - bad format
                }
            }
            return result;
        }
    }

    /**
     * The Class TSAData.
     */
    public static class TSAData {
        private static final String CODE = "061099";

        private boolean m_calculated = false;
        private List<String> m_codes = new ArrayList<>(4);
        private boolean m_hasCode = false;
        private List<String> m_results = new ArrayList<>(4);

        /**
         * Instantiates a new TSA data.
         *
         * @param std SisStudent
         */
        public TSAData(SisStudent std) {
            for (int i = 1; i <= 4; i++) {
                String resultAlias = MessageFormat.format("all-std-TechnicalSkillsAssessment{0}Result", i);
                String codeAlias = MessageFormat.format("all-std-TechnicalSkillsAssessment{0}Code", i);

                Object result = std.getFieldValueByAlias(resultAlias);
                Object code = std.getFieldValueByAlias(codeAlias);

                m_results.add(result != null ? result.toString() : null);
                m_codes.add(code != null ? code.toString() : null);
            }
            m_hasCode = m_codes.contains(CODE);
        }

        /**
         * Sets the calculation result.
         *
         * @param result void
         */
        public void setCalculationResult(String result) {
            if (!StringUtils.isEmpty(result)) {
                for (int i = 0; i < m_results.size(); i++) {
                    if (!StringUtils.isEmpty(m_codes.get(i)) && CODE.equals(m_codes.get(i))) {
                        // do not replace existing value
                        break;
                    }
                    if (StringUtils.isEmpty(m_results.get(i)) && StringUtils.isEmpty(m_codes.get(i))) {
                        m_results.set(i, result);
                        m_codes.set(i, CODE);
                        break;
                    }
                }
            }
            m_calculated = true;
        }

        /**
         * Gets the value by alias.
         *
         * @param alias String
         * @return String
         */
        public String getValueByAlias(String alias) {
            for (int i = 0; i < m_results.size(); i++) {
                if (alias != null && alias.contains(Integer.toString(i + 1))) {
                    return alias.contains("Result") ? m_results.get(i) : m_codes.get(i);
                }
            }
            return null;
        }
    }

    /**
     * The Class HighSchoolDataCollectionEntity.
     */
    public static class HighSchoolDataCollectionEntity extends MDStudentReportEntity {

        private TSAData m_tsaData;

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            HighSchoolDataCollection hsData = (HighSchoolDataCollection) data;
            if (!hsData.getMembershipList().contains(bean.getOid())
                    && !hsData.getGraduateList().contains(bean.getOid())) {
                setRowCount(0);
            }
            m_tsaData = new TSAData((SisStudent) bean);
        }

        /**
         * Post process.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#postProcess()
         */
        @Override
        public void postProcess() {
            super.postProcess();
            SisStudent student = (SisStudent) getBean();
            if (student.isDirty()) {
                getData().getBroker().saveBeanForced(student);
            }
        }

        /**
         * Gets the school.
         *
         * @param data MDStudentReportData
         * @return Sis school
         * @see com.x2dev.procedures.statereporting.md.MDStudentReportData.MDStudentReportEntity#getSchool(com.follett.fsc.core.k12.tools.stateexports.StateReportData)
         */
        @Override
        public SisSchool getSchool(MDStudentReportData data) {
            SisStudent student = (SisStudent) getBean();
            SisSchool school = data.getOverrideSchool(student);
            return school == null ? student.getSchool() : school;
        }

    }

    /**
     * Names for aliases.
     */
    static final private String ALIAS_CTE_CONCENTRATOR = "DOE CTE CONCENTRATOR";
    static final private String ALIAS_CRS_ADV_TECH_ED = "DOE ADV TECH ED";
    static final private String ALIAS_CRS_DEPARTMENT = "all-crs-Department";
    static final private String ALIAS_CRS_RIGOROUS_INDICATOR = "all-crs-RigorousHSIndicator";
    static final private String ALIAS_CRS_TRANSITION = "all-crs-TransitionCourse";
    static final private String ALIAS_CRS_WORLD_LANGUAGE = "all-crs-WorldLangage";
    static final private String ALIAS_DUAL_ENROLLMENT = "DOE DUAL ENROLLMENT";
    static final private String ALIAS_STD_CCR_11TH_MATH = "all-stdCCRDertermination11thGradeMath";
    static final private String ALIAS_STD_CCR_11TH_ELA = "all-stdCCRDertermination11thGradeELA";

    /**
     * Bridge assessment constants
     */
    static final private String BRIDGE_ASD_ID_ELA = "BRDG ENGLISH";
    static final private String BRIDGE_ASD_ID_GOVERNMENT = "BRDG GOVT";
    static final private String BRIDGE_ASD_ID_MATH = "BRDG ALGEBRA";
    static final private String BRIDGE_ASD_ID_SCIENCE = "BRDG MISA";
    static final private String BRIDGE_PLAN_COMPLETED = "C";
    /**
     * Retriever parameters
     */
    public static final String CALC_PARAM_SUBJECT_ELA = "ELA";
    public static final String CALC_PARAM_SUBJECT_GOVERNMENT = "GOVERNMENT";
    public static final String CALC_PARAM_SUBJECT_MATH = "MATH";
    public static final String CALC_PARAM_SUBJECT_SCIENCE = "SCIENCE";
    public static final String CALC_PARAM_RESULT_1 = "RESULT-1";
    public static final String CALC_PARAM_CODE_1 = "CODE-1";

    /**
     * Calc ids
     */
    public static final String CALC_ID_RIGOR_SCIENCE = "RIGOR-SCIENCE";
    public static final String CALC_ID_RIGOR_ADV_TECH = "RIGOR-ADV-TECH";
    public static final String CALC_ID_TRAN_MATH = "TRAN-MATH";
    public static final String CALC_ID_TRAN_ELA = "TRAN-ELA";
    public static final String CALC_ID_CCR_12_MATH = "CCR-12-MATH";
    public static final String CALC_ID_CCR_12_ELA = "CCR-12-ELA";
    public static final String CALC_ID_TSA_RESULT = "TSA-RESULT";

    /**
     * Field names
     */
    public static final String FIELD_CTE_CONCENTRATOR = "CTE Concentrator";
    public static final String FIELD_MHSA_ELA = "MHSA ELA";
    public static final String FIELD_MHSA_GOVERNMENT = "MHSA Government";
    public static final String FIELD_MHSA_MATH = "MHSA Math";
    public static final String FIELD_MHSA_SCIENCE = "MHSA Science";
    public static final String FIELD_SPED_CERT = "Special Ed Cert";
    public static final String FIELD_CCR_11_MATH = "CCR 11th Grade Math";
    public static final String FIELD_CCR_11_ELA = "CCR 11th Grade ELA";

    /**
     * Name for the clear student parameter. The value is an Boolean.
     */
    public static final String INPUT_PARAM_CLEAR_STUDENT = "clearStudent";

    /**
     * Name for the context oid parameter. The value is an String.
     */
    public static final String INPUT_PARAM_CONTEXT_OID = "contextOid";

    /**
     * Name for the GPA definition oid parameter. The value is an String.
     */
    public static final String INPUT_PARAM_GPA_DEFINITION_OID = "gpaDefinitionOid";

    /**
     * Name for the update student parameter. The value is an Boolean.
     */
    public static final String INPUT_PARAM_UPDATE_STUDENT = "updateStudent";

    /**
     * The Class BridgeHelper.
     */
    protected class BridgeHelper {
        private Map<String, Set<String>> m_qualifiedStudentOids = new HashMap();

        /**
         * Inits the assessment definition.
         *
         * @param asdId String
         * @param studentSubQuery SubQuery
         * @param qualifyingCriteria List<KeyValueTrio>
         */
        void initAssessmentDefinition(String asdId, SubQuery studentSubQuery, List<KeyValueTrio> qualifyingCriteria) {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
            ComparisonType.addQualifyingCriteria(criteria, qualifyingCriteria);

            Set<String> qualifiedStudents = new HashSet();
            ComparisonType.populateQualifiedList(getBroker(), StudentAssessment.class, criteria,
                    StudentAssessment.COL_STUDENT_OID,
                    qualifiedStudents);
            m_qualifiedStudentOids.put(asdId, qualifiedStudents);
        }

        /**
         * Checks if is qualified.
         *
         * @param asdId String
         * @param studentOid String
         * @return true, if is qualified
         */
        boolean isQualified(String asdId, String studentOid) {
            boolean result = false;
            Set<String> qualifiedStudentOids = m_qualifiedStudentOids.get(asdId);
            if (qualifiedStudentOids != null) {
                result = qualifiedStudentOids.contains(studentOid);
            }
            return result;
        }
    }
    /**
     * The Class GraduateHelper.
     */
    protected class GraduateHelper {

        /**
         * The Class CollegeRequirements.
         */
        private class CollegeRequirements {
            private BigDecimal m_advancedTechCredit = BigDecimal.ZERO;
            private BigDecimal m_foreignLanguageCredit = BigDecimal.ZERO;
            private BigDecimal m_mathCredit = BigDecimal.ZERO;

            /**
             * Increment.
             *
             * @param type String
             * @param amount BigDecimal
             */
            public void increment(String type, BigDecimal amount) {
                if ("Math".equals(type)) {
                    m_mathCredit = m_mathCredit.add(amount);
                } else if ("Advanced Technology".equals(type)) {
                    m_advancedTechCredit = m_advancedTechCredit.add(amount);
                } else if ("Foreign Language".equals(type)) {
                    m_foreignLanguageCredit = m_foreignLanguageCredit.add(amount);
                }
            }

            /**
             * Checks if is met.
             *
             * @return true, if is met
             */
            public boolean isMet() {
                return BIG_DECIMAL_FOUR.compareTo(m_mathCredit) <= 0 &&
                        (BIG_DECIMAL_TWO.compareTo(m_advancedTechCredit) <= 0
                                || BIG_DECIMAL_TWO.compareTo(m_foreignLanguageCredit) <= 0);
            }
        }

        final protected BigDecimal BIG_DECIMAL_TWO = BigDecimal.valueOf(2.0);
        final protected BigDecimal BIG_DECIMAL_FOUR = BigDecimal.valueOf(4.0);

        static final private String ALIAS_COLLEGE_REQ = "all-crs-CollegeRequirements";

        Map<String, CollegeRequirements> m_collegeRequirements = new HashMap();
        Set<String> m_graduates = new HashSet();

        /**
         * Instantiates a new graduate helper.
         *
         * @param studentSubQuery SubQuery
         */
        public GraduateHelper(SubQuery studentSubQuery) {
            initGraduates(studentSubQuery);
            String beanPath = translateAliasToJavaName(ALIAS_COLLEGE_REQ, true);
            if (!StringUtils.isEmpty(beanPath)) {
                initCollegeRequirements(studentSubQuery, beanPath);
            }
        }

        /**
         * Checks if is freshman req.
         *
         * @param oid String
         * @return true, if is freshman req
         */
        public boolean metCollegeRequirements(String oid) {
            return m_collegeRequirements.containsKey(oid) && m_collegeRequirements.get(oid).isMet();
        }

        /**
         * Checks if is graduate.
         *
         * @param oid String
         * @return true, if is graduate
         */
        public boolean isGraduate(String oid) {
            return m_graduates.contains(oid);
        }

        /**
         * Inits the by transcript.
         *
         * @param studentSubQuery SubQuery
         * @param beanPath String
         */
        private void initCollegeRequirements(SubQuery studentSubQuery, String beanPath) {
            String fullBeanPath = Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER + SchoolCourse.REL_COURSE
                    + ModelProperty.PATH_DELIMITER + beanPath;
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);
            criteria.addGreaterThan(Transcript.COL_TOTAL_CREDIT, BigDecimal.ZERO);
            criteria.addNotEmpty(fullBeanPath, getBroker().getPersistenceKey());

            String[] columns = new String[] {Transcript.COL_STUDENT_OID, Transcript.COL_TOTAL_CREDIT, fullBeanPath};
            ColumnQuery query = new ColumnQuery(Transcript.class, columns, criteria);
            try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    String oid = (String) row[0];
                    BigDecimal credit = (BigDecimal) row[1];
                    String type = (String) row[2];
                    CollegeRequirements req = m_collegeRequirements.get(oid);
                    if (req == null) {
                        req = new CollegeRequirements();
                        m_collegeRequirements.put(oid, req);
                    }
                    req.increment(type, credit);
                }
            }
        }

        /**
         * Inits the graduates.
         *
         * @param studentSubQuery SubQuery
         */
        private void initGraduates(SubQuery studentSubQuery) {
            String rtbWithdrawalCodesOid = PreferenceManager.getPreferenceValue(getOrganization(),
                    SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
            if (!StringUtils.isEmpty(rtbWithdrawalCodesOid)) {
                X2Criteria rcdCriteria = new X2Criteria();
                rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbWithdrawalCodesOid);
                rcdCriteria.addIn(ReferenceCode.COL_STATE_CODE, Arrays.asList(new String[] {"C60", "C64", "C70"}));
                SubQuery codeSubQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, rcdCriteria);

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
                criteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentSubQuery);
                criteria.addIn(StudentEnrollment.COL_ENROLLMENT_CODE, codeSubQuery);

                String[] columns = new String[] {StudentEnrollment.COL_STUDENT_OID};
                ColumnQuery query = new ColumnQuery(StudentEnrollment.class, columns, criteria);
                try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        m_graduates.add((String) row[0]);
                    }
                }
            }
        }
    }

    protected class RetrieveBiliteracyValues implements FieldRetriever {
        private static final String CALC_ID = "BIL-VALUES";

        private Map<String, Collection<Transcript>> m_qualifyingTranscripts;

        /**
         * Instantiates a new retrieve student by transcript.
         *
         * @param studentSubQuery SubQuery
         * @param qualifyingCriteria List<KeyValueTrio>
         * @param groupFields List<String>
         * @param distinctFields List<String>
         * @param count int
         */
        public RetrieveBiliteracyValues(SubQuery studentSubQuery) {
            super();
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);
            criteria.addContainsIgnoreCase(
                    Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER + SchoolCourse.COL_NUMBER, "bil");
            m_qualifyingTranscripts = getBroker().getGroupedCollectionByQuery(new BeanQuery(Transcript.class, criteria),
                    Transcript.COL_STUDENT_OID, 128);

        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String returnValue = null;
            SisStudent std = (SisStudent) entity.getBean();
            String beanPath = field.getBeanPath();
            if (isUpdateStudent()) {
                returnValue = m_qualifyingTranscripts.containsKey(std.getOid()) ? "Y" : "N";
                std.setFieldValueByBeanPath(beanPath, returnValue);
                getBroker().saveBeanForced(std);
            } else {
                returnValue = (String) std.getFieldValueByBeanPath(beanPath);
            }
            return returnValue;
        }
    }

    /**
     * The Class RetrieveStudentGrade.
     */
    protected class RetrieveCCRValues implements FieldRetriever {
        private static final String CALC_ID = "CCR-VALUES";
        private static final String CALC_PARAM_ASM_ENG = "ASM_ENG";
        private static final String CALC_PARAM_ASM_MATH = "ASM_MATH";
        private static final String CALC_PARAM_ELIG_ENG = "ELIG_ENG";
        private static final String CALC_PARAM_ELIG_MATH = "ELIG_MATH";
        private static final String EXPORT_FIELD_CCR_STATUS_ENG = "CCR Subj ASM Status - ENG";
        private static final String EXPORT_FIELD_CCR_STATUS_MATH = "CCR Subj ASM Status - MATH";
        private static final String PARCC_ASSESSMENT_DEFINITON_ID = "PARCCTest-15-16";
        static final private String PARCC_ALIAS_TEST_CODE = "PARCCTSTCODE";
        private static final String SAT_MATH_ASSESSMENT_DEFINITON_ID = "SAT - MATH";
        private final BigDecimal CHECK_SCORE_520 = new BigDecimal("520");
        private final BigDecimal CHECK_SCORE_750 = new BigDecimal("750");

        PlainDate m_dateEnd;
        PlainDate m_dateStart;
        String m_fieldParccTestCode;
        Map<String, Collection<StudentAssessment>> m_parccAsmMapMATH = new HashMap();
        Map<String, Collection<StudentAssessment>> m_parccAsmMapELA = new HashMap();
        Map<String, Collection<StudentAssessment>> m_satStudentAsmMap = new HashMap();

        /**
         * Instantiates a new retrieve student grade.
         *
         * @param subQueryStudentOid SubQuery
         */
        public RetrieveCCRValues(SubQuery subQueryStudentOid) {
            Calendar cal = Calendar.getInstance();
            cal.set(2021, Calendar.AUGUST, 1);
            m_dateStart = new PlainDate(cal.getTime());

            cal.set(2021, Calendar.DECEMBER, 31);
            m_dateEnd = new PlainDate(cal.getTime());

            AssessmentDefinition asd = getAssessmentDefinition(PARCC_ASSESSMENT_DEFINITON_ID);
            if (asd != null) {
                m_fieldParccTestCode = translateAlias(asd, PARCC_ALIAS_TEST_CODE);
            }
            X2Criteria criteriaMath = new X2Criteria();
            criteriaMath.addIn(StudentAssessment.COL_STUDENT_OID, subQueryStudentOid);
            if (m_fieldParccTestCode != null) {
                criteriaMath.addIn(m_fieldParccTestCode, Arrays.asList("ALG01", "ALG02", "GEO01"));
            }
            criteriaMath.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER
                    + AssessmentDefinition.COL_ID, PARCC_ASSESSMENT_DEFINITON_ID);
            m_parccAsmMapMATH =
                    getBroker().getGroupedCollectionByQuery(new QueryByCriteria(StudentAssessment.class, criteriaMath),
                            StudentAssessment.COL_STUDENT_OID, 1024);

            X2Criteria criteriaEla = new X2Criteria();
            criteriaEla.addIn(StudentAssessment.COL_STUDENT_OID, subQueryStudentOid);
            if (m_fieldParccTestCode != null) {
                criteriaEla.addIn(m_fieldParccTestCode, Arrays.asList("ELA10"));
            }
            criteriaEla.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER
                    + AssessmentDefinition.COL_ID, PARCC_ASSESSMENT_DEFINITON_ID);
            m_parccAsmMapELA =
                    getBroker().getGroupedCollectionByQuery(new QueryByCriteria(StudentAssessment.class, criteriaEla),
                            StudentAssessment.COL_STUDENT_OID, 1024);

            X2Criteria criteriaSatMath = new X2Criteria();
            criteriaSatMath.addIn(StudentAssessment.COL_STUDENT_OID, subQueryStudentOid);
            criteriaSatMath.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER
                    + AssessmentDefinition.COL_ID, SAT_MATH_ASSESSMENT_DEFINITON_ID);
            m_satStudentAsmMap =
                    getBroker().getGroupedCollectionByQuery(
                            new QueryByCriteria(StudentAssessment.class, criteriaSatMath),
                            StudentAssessment.COL_STUDENT_OID, 1024);
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
            String returnValue = null;
            SisStudent std = (SisStudent) entity.getBean();
            String param = (String) field.getParameter();
            String beanPath = field.getBeanPath();
            if (isUpdateStudent()) {
                Collection<StudentAssessment> parccAsmMath = m_parccAsmMapMATH.get(std.getOid());
                Collection<StudentAssessment> parccAsmELA = m_parccAsmMapELA.get(std.getOid());
                if (CALC_PARAM_ASM_MATH.equals(param)) {
                    if (parccAsmMath != null && !parccAsmMath.isEmpty()) {
                        boolean isWAsmFound = false;
                        for (StudentAssessment asmToCheck : parccAsmMath) {
                            if (!isWAsmFound) {
                                String schoolBeanPath =
                                        translateAlias(asmToCheck.getAssessmentDefinition(), PARCC_ALIAS_SCHOOL);
                                isWAsmFound = (asmToCheck.getPerformanceLevel() != null
                                        && "W".equals(asmToCheck.getPerformanceLevel()))
                                        || (schoolBeanPath != null
                                                && StringUtils.equals(
                                                        (String) asmToCheck.getFieldValueByBeanPath(schoolBeanPath),
                                                        "OOS"));
                            }
                            if ((asmToCheck.getPerformanceLevel() != null
                                    && !"W".equals(asmToCheck.getPerformanceLevel()))
                                    || (asmToCheck.getScaleScore() != null
                                            && asmToCheck.getScaleScore().compareTo(BigDecimal.ZERO) > 0)) {
                                returnValue = "01";
                                break;
                            }
                        }
                        if (StringUtils.isEmpty(returnValue) && isWAsmFound) {
                            returnValue = "00";
                        }
                    }
                    if (StringUtils.isEmpty(returnValue)) {
                        returnValue = "02";
                    }
                } else if (CALC_PARAM_ELIG_MATH.equals(param)) {
                    if (Arrays.asList("00", "02").contains(entity.getFieldValue(EXPORT_FIELD_CCR_STATUS_MATH))) {
                        returnValue = "00";
                    } else if (parccAsmMath != null && !parccAsmMath.isEmpty()) {
                        boolean isMatchFirstCheck = false;
                        boolean isMatchSecondCheck = false;
                        boolean isMatchThirdCheck = false;
                        for (StudentAssessment asmToCheck : parccAsmMath) {
                            if (!isMatchFirstCheck && asmToCheck.getDate().before(m_dateStart)
                                    && (Arrays.asList("4", "5").contains(asmToCheck.getPerformanceLevel()))
                                    || (asmToCheck.getScaleScore() != null
                                            && asmToCheck.getScaleScore()
                                                    .compareTo(CHECK_SCORE_750) > 0)) {
                                isMatchFirstCheck = true;
                                break;
                            }
                            if (!isMatchSecondCheck && DateUtils.isBetween(asmToCheck.getDate(), m_dateStart, m_dateEnd)
                                    && Arrays.asList("2", "3", "4").contains(asmToCheck.getPerformanceLevel())) {
                                isMatchSecondCheck = true;
                                break;
                            }
                            if (!isMatchThirdCheck && asmToCheck.getDate().after(m_dateEnd)
                                    && Arrays.asList("3", "4").contains(asmToCheck.getPerformanceLevel())) {
                                isMatchThirdCheck = true;
                                break;
                            }
                        }
                        if (isMatchFirstCheck || isMatchSecondCheck || isMatchThirdCheck) {
                            returnValue = "01";
                        } else if (StringUtils.isEmpty(returnValue)) {
                            if (StringUtils.isEmpty(returnValue) && m_satStudentAsmMap.containsKey(std.getOid())) {
                                for (StudentAssessment satAsmToCheck : m_satStudentAsmMap.get(std.getOid())) {
                                    if (satAsmToCheck.getRawScore() != null
                                            && satAsmToCheck.getRawScore().compareTo(CHECK_SCORE_520) >= 0) {
                                        returnValue = "01";
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    if (StringUtils.isEmpty(returnValue)) {
                        returnValue = "02";
                    }
                } else if (CALC_PARAM_ASM_ENG.equals(param)) {
                    if (parccAsmELA != null && !parccAsmELA.isEmpty()) {
                        boolean isWAsmFound = false;
                        for (StudentAssessment asmToCheck : parccAsmELA) {
                            if (!isWAsmFound) {
                                String schoolBeanPath =
                                        translateAlias(asmToCheck.getAssessmentDefinition(), PARCC_ALIAS_SCHOOL);
                                isWAsmFound = (asmToCheck.getPerformanceLevel() != null
                                        && "W".equals(asmToCheck.getPerformanceLevel()))
                                        || (schoolBeanPath != null
                                                && StringUtils.equals(
                                                        (String) asmToCheck.getFieldValueByBeanPath(schoolBeanPath),
                                                        "OOS"));
                            }
                            if ((asmToCheck.getPerformanceLevel() != null
                                    && !"W".equals(asmToCheck.getPerformanceLevel()))
                                    || (asmToCheck.getScaleScore() != null
                                            && asmToCheck.getScaleScore().compareTo(BigDecimal.ZERO) > 0)) {
                                returnValue = "01";
                                break;
                            }
                        }
                        if (StringUtils.isEmpty(returnValue) && isWAsmFound) {
                            returnValue = "00";
                        }
                    }
                    if (StringUtils.isEmpty(returnValue)) {
                        returnValue = "02";
                    }
                } else if (CALC_PARAM_ELIG_ENG.equals(param)) {
                    if (Arrays.asList("00", "02").contains(entity.getFieldValue(EXPORT_FIELD_CCR_STATUS_ENG))) {
                        returnValue = "00";
                    } else if (parccAsmELA != null && !parccAsmELA.isEmpty()) {
                        boolean isMatchFirstCheck = false;
                        boolean isMatchSecondCheck = false;
                        boolean isMatchThirdCheck = false;
                        for (StudentAssessment asmToCheck : parccAsmELA) {
                            if (!isMatchFirstCheck && asmToCheck.getDate().before(m_dateStart)
                                    && (Arrays.asList("4", "5").contains(asmToCheck.getPerformanceLevel()))
                                    || (asmToCheck.getScaleScore() != null
                                            && asmToCheck.getScaleScore()
                                                    .compareTo(CHECK_SCORE_750) > 0)) {
                                isMatchFirstCheck = true;
                                break;
                            }
                            if (!isMatchSecondCheck && DateUtils.isBetween(asmToCheck.getDate(), m_dateStart, m_dateEnd)
                                    && Arrays.asList("2", "3", "4").contains(asmToCheck.getPerformanceLevel())) {
                                isMatchSecondCheck = true;
                                break;
                            }
                            if (!isMatchThirdCheck && asmToCheck.getDate().after(m_dateEnd)
                                    && Arrays.asList("3", "4").contains(asmToCheck.getPerformanceLevel())) {
                                isMatchThirdCheck = true;
                                break;
                            }
                        }
                        if (isMatchFirstCheck || isMatchSecondCheck || isMatchThirdCheck) {
                            returnValue = "01";
                        } else if (StringUtils.isEmpty(returnValue)) {
                            if (StringUtils.isEmpty(returnValue) && m_satStudentAsmMap.containsKey(std.getOid())) {
                                for (StudentAssessment satAsmToCheck : m_satStudentAsmMap.get(std.getOid())) {
                                    if (satAsmToCheck.getRawScore() != null
                                            && satAsmToCheck.getRawScore().compareTo(CHECK_SCORE_520) >= 0) {
                                        returnValue = "01";
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    if (StringUtils.isEmpty(returnValue)) {
                        returnValue = "02";
                    }
                }
                std.setFieldValueByBeanPath(beanPath, returnValue);
                getBroker().saveBeanForced(std);
            } else {
                String value = (String) std.getFieldValueByBeanPath(beanPath);
                if (!StringUtils.isEmpty(value)) {
                    returnValue = lookupStateValue(SisStudent.class, beanPath, value);
                }
            }
            return returnValue;
        }
    }

    /**
     * The Class RetrieveStudentAlias.
     */
    protected abstract class RetrieveStudentAlias implements FieldRetriever {

        private static final String ALIAS_FREEZE_DATA_FOR_HS_COLLECTION = "all-std-FreezeDataforHSCollection";
        private String m_fieldStudentFreezeData;

        /**
         * Instantiates a new retrieve student alias.
         */
        public RetrieveStudentAlias() {
            m_fieldStudentFreezeData =
                    HighSchoolDataCollection.this.translateAliasToJavaName(ALIAS_FREEZE_DATA_FOR_HS_COLLECTION,
                            true);
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
            String value = null;
            HighSchoolDataCollection hsData = (HighSchoolDataCollection) data;
            String beanPath = getAliasBeanPath(hsData, entity, field);
            if (!StringUtils.isEmpty(beanPath)) {
                SisStudent student = (SisStudent) entity.getBean();
                value = getBusinessRuleValue(hsData, entity, field);
                Boolean stdHsFreeze = (Boolean) data.getPropertyAsJavaType(student, m_fieldStudentFreezeData);
                if (!StringUtils.isEmpty(value) && hsData.isUpdateStudent()
                        && (stdHsFreeze == null || !stdHsFreeze.booleanValue())) {
                    student.setFieldValueByBeanPath(beanPath, value);
                }
            }
            return value;
        }

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         */
        protected abstract String getAliasBeanPath(HighSchoolDataCollection data,
                                                   StateReportEntity entity,
                                                   FieldDefinition field);

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @throws X2BaseException exception
         */
        protected abstract String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                                       StateReportEntity entity,
                                                       FieldDefinition field)
                throws X2BaseException;

    }

    /**
     * The Class RetrieveStudentByAlias.
     */
    protected class RetrieveStudentByAlias extends RetrieveStudentAlias {

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getAliasBeanPath(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getAliasBeanPath(HighSchoolDataCollection data,
                                          StateReportEntity entity,
                                          FieldDefinition field) {
            return data.translateAliasToJavaName((String) field.getParameter(), false);
        }

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field) {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            String beanPath = getAliasBeanPath(hsData, entity, field);
            if (!StringUtils.isEmpty(beanPath)) {
                value = (String) student.getFieldValueByBeanPath(beanPath);
            }
            if (!StringUtils.isEmpty(value) && hsData.isUpdateStudent()) {
                student.setFieldValueByBeanPath(beanPath, value);
                getBroker().saveBeanForced(student);
            }
            return value;
        }

    }

    /**
     * The Class RetrieveStudentByAlias.
     */
    protected class RetrieveStudentByAliasYNValues implements FieldRetriever {

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
            String value = null;
            HighSchoolDataCollection hsData = (HighSchoolDataCollection) data;
            String beanPath = field.getBeanPath();
            if (!StringUtils.isEmpty(beanPath)) {
                SisStudent student = (SisStudent) entity.getBean();
                value = (String) student.getFieldValueByBeanPath(beanPath);
                if (StringUtils.isEmpty(value)) {
                    value = "N";
                }
                if (!StringUtils.isEmpty(value) && hsData.isUpdateStudent()) {
                    student.setFieldValueByBeanPath(beanPath, value);
                    getBroker().saveBeanForced(student);
                }
            }
            return value;
        }
    }

    /**
     * The Class RetrieveStudentByAssessment.
     */
    protected class RetrieveStudentByAssessment extends RetrieveStudentAlias {
        private String m_falseValue;
        private Set<String> m_qualifiedStudents = new HashSet();
        private String m_trueValue;

        /**
         * Instantiates a new retrieve student by assessment.
         *
         * @param studentSubQuery SubQuery
         * @param qualifyingCriteria List<KeyValuePair>
         * @param qualifier AssessmentQualifier
         * @param trueValue String
         * @param falseValue String
         */
        public RetrieveStudentByAssessment(SubQuery studentSubQuery,
                List<KeyValueTrio> qualifyingCriteria,
                AssessmentQualifier qualifier,
                String trueValue,
                String falseValue) {
            super();
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
            ComparisonType.addQualifyingCriteria(criteria, qualifyingCriteria);


            if (qualifier == null) {
                ComparisonType.populateQualifiedList(getBroker(), StudentAssessment.class, criteria,
                        StudentAssessment.COL_STUDENT_OID,
                        m_qualifiedStudents);
            } else {
                BeanQuery query = new BeanQuery(StudentAssessment.class, criteria);
                query.addOrderByAscending(StudentAssessment.COL_STUDENT_OID);
                String stdOid = null;
                List<StudentAssessment> asmList = new LinkedList();
                try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                    while (iterator.hasNext()) {
                        StudentAssessment bean = (StudentAssessment) iterator.next();
                        if (stdOid != null && !stdOid.equals(bean.getStudentOid())) {
                            if (qualifier.isQualified(asmList)) {
                                m_qualifiedStudents.add(stdOid);
                            }
                            asmList = new LinkedList();
                        }
                        stdOid = bean.getStudentOid();
                        asmList.add(bean);
                    }
                }
                if (!asmList.isEmpty() && qualifier.isQualified(asmList)) {
                    m_qualifiedStudents.add(stdOid);
                }
            }

            System.out.println("Assessment: " + m_qualifiedStudents.size() + " - " + qualifyingCriteria.toString());
            m_trueValue = trueValue;
            m_falseValue = falseValue;
        }

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getAliasBeanPath(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getAliasBeanPath(HighSchoolDataCollection data,
                                          StateReportEntity entity,
                                          FieldDefinition field) {
            return data.translateAliasToJavaName((String) field.getParameter(), false);
        }

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field) {
            SisStudent student = (SisStudent) entity.getBean();
            String value = m_falseValue;
            if (m_qualifiedStudents.contains(student.getOid())) {
                value = m_trueValue;
            }
            return value;
        }

    }

    /**
     * The Class RetrieveStudentByGPA.
     */
    protected class RetrieveStudentByGPA extends RetrieveStudentAlias {
        private final BigDecimal GPA_THRESHOLD = new BigDecimal("3.0");

        private String m_fieldGpa;

        /**
         * Instantiates a new retrieve student by GPA.
         *
         * @param subQueryStudentOid SubQuery
         */
        public RetrieveStudentByGPA(SubQuery subQueryStudentOid) {
            String gpdOid = (String) getParameter(INPUT_PARAM_GPA_DEFINITION_OID);
            if (!StringUtils.isEmpty(gpdOid)) {
                GradePointAverageDefinition gpd = (GradePointAverageDefinition) getBroker()
                        .getBeanByOid(GradePointAverageDefinition.class, gpdOid);
                if (gpd != null) {
                    SisDataFieldConfig field = gpd.getGpaDataFieldConfig();
                    if (field != null) {
                        m_fieldGpa = field.getDataField().getJavaName();
                    } else {
                        addSetupError("GPA data field not found", gpdOid);
                    }
                } else {
                    addSetupError("GPA Definition not found", gpdOid);
                }
            } else {
                addSetupError("GPA Definition Oid empty", "");
            }
        }

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getAliasBeanPath(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getAliasBeanPath(HighSchoolDataCollection data,
                                          StateReportEntity entity,
                                          FieldDefinition field) {
            return data.translateAliasToJavaName((String) field.getParameter(), false);
        }

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field)
                throws X2BaseException {
            String value = "N";
            SisStudent student = (SisStudent) entity.getBean();
            Object gpa = hsData.getPropertyAsJavaType(student, m_fieldGpa);
            if (gpa != null) {
                BigDecimal decimalGpa = null;
                if (gpa instanceof BigDecimal) {
                    decimalGpa = (BigDecimal) gpa;
                } else {
                    decimalGpa = new BigDecimal(gpa.toString());
                }
                if (decimalGpa.compareTo(GPA_THRESHOLD) >= 0) {
                    value = "Y";
                }
            }
            return value;
        }

    }

    /**
     * The Class RetrieveStudentBySchedule.
     */
    protected class RetrieveStudentBySchedule extends RetrieveStudentAlias {
        private Set<String> m_qualifiedStudents = new HashSet();

        /**
         * Instantiates a new retrieve student CTE concentrator.
         *
         * @param studentSubQuery SubQuery
         * @param sectionCriteria List<KeyValueTrio>
         */
        public RetrieveStudentBySchedule(SubQuery studentSubQuery, List<KeyValueTrio> sectionCriteria) {
            SubQuery sectionSubQuery = getSectionSubquery(sectionCriteria);
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentSchedule.COL_STUDENT_OID, studentSubQuery);
            criteria.addIn(StudentSchedule.COL_SECTION_OID, sectionSubQuery);
            ComparisonType.populateQualifiedList(getBroker(), StudentSchedule.class, criteria,
                    StudentSchedule.COL_STUDENT_OID,
                    m_qualifiedStudents);
            System.out.println("Schedule: " + m_qualifiedStudents.size() + " - " + sectionCriteria.toString());

            criteria = new X2Criteria();
            criteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);
            criteria.addIn(Transcript.COL_MASTER_SCHEDULE_OID, sectionSubQuery);
            ComparisonType.populateQualifiedList(getBroker(), Transcript.class, criteria,
                    Transcript.COL_STUDENT_OID,
                    m_qualifiedStudents);
            System.out
                    .println("Schedule/Transcript: " + m_qualifiedStudents.size() + " - "
                            + sectionCriteria.toString());
        }

        /**
         * Gets the section subquery.
         *
         * @param sectionCriteria List<KeyValueTrio>
         * @return Sub query
         */
        private SubQuery getSectionSubquery(List<KeyValueTrio> sectionCriteria) {
            X2Criteria criteria = new X2Criteria();
            ComparisonType.addQualifyingCriteria(criteria, sectionCriteria);
            return new SubQuery(MasterSchedule.class, X2BaseBean.COL_OID, criteria);
        }

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getAliasBeanPath(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getAliasBeanPath(HighSchoolDataCollection data,
                                          StateReportEntity entity,
                                          FieldDefinition field) {
            return data.translateAliasToJavaName((String) field.getParameter(), false);
        }

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field) {
            Student student = (Student) entity.getBean();
            if (CALC_ID_TRAN_MATH.equals(field.getCalcId())
                    && StringUtils.equals((String) student.getFieldValueByAlias(ALIAS_STD_CCR_11TH_MATH), "N")) {
                return "Y";
            }
            if (CALC_ID_TRAN_ELA.equals(field.getCalcId())
                    && StringUtils.equals((String) student.getFieldValueByAlias(ALIAS_STD_CCR_11TH_ELA), "N")) {
                return "Y";
            }
            return m_qualifiedStudents.contains(student.getOid()) ? "Y" : "N";
        }

    }
    /**
     * The Class RetrieveStudentBySchedule.
     */
    protected class RetrieveStudentByTranscriptExists extends RetrieveStudentAlias {
        private Set<String> m_qualifiedStudents = new HashSet();

        /**
         * Instantiates a new retrieve student CTE concentrator.
         *
         * @param studentSubQuery SubQuery
         * @param cskCriteria List<KeyValueTrio>
         */
        public RetrieveStudentByTranscriptExists(SubQuery studentSubQuery, List<KeyValueTrio> cskCriteria) {
            SubQuery cskSubQuery = getSchoolCourseSubquery(cskCriteria);
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);
            criteria.addIn(Transcript.COL_SCHOOL_COURSE_OID, cskSubQuery);
            ComparisonType.populateQualifiedList(getBroker(), Transcript.class, criteria,
                    Transcript.COL_STUDENT_OID,
                    m_qualifiedStudents);
            System.out.println("Schedule: " + m_qualifiedStudents.size() + " - " + cskCriteria.toString());
        }

        /**
         * Gets the section subquery.
         *
         * @param cskCriteria List<KeyValueTrio>
         * @return Sub query
         */
        private SubQuery getSchoolCourseSubquery(List<KeyValueTrio> cskCriteria) {
            X2Criteria criteria = new X2Criteria();
            ComparisonType.addQualifyingCriteria(criteria, cskCriteria);
            return new SubQuery(SchoolCourse.class, X2BaseBean.COL_OID, criteria);
        }

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getAliasBeanPath(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getAliasBeanPath(HighSchoolDataCollection data,
                                          StateReportEntity entity,
                                          FieldDefinition field) {
            return data.translateAliasToJavaName((String) field.getParameter(), false);
        }

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field) {
            Student student = (Student) entity.getBean();
            if (CALC_ID_TRAN_MATH.equals(field.getCalcId())
                    && StringUtils.equals((String) student.getFieldValueByAlias(ALIAS_STD_CCR_11TH_MATH), "N")) {
                return "Y";
            }
            if (CALC_ID_TRAN_ELA.equals(field.getCalcId())
                    && StringUtils.equals((String) student.getFieldValueByAlias(ALIAS_STD_CCR_11TH_ELA), "N")) {
                return "Y";
            }
            return m_qualifiedStudents.contains(student.getOid()) ? "Y" : "N";
        }

    }

    /**
     * The Class RetrieveStudentByTranscript.
     */
    protected class RetrieveStudentByTranscript extends RetrieveStudentAlias {
        private List<String> m_distinctFields;
        private List<String> m_groupFields;
        private int m_numRequired;
        private Map<String, Collection<Transcript>> m_qualifyingTranscripts;

        /**
         * Instantiates a new retrieve student by transcript.
         *
         * @param studentSubQuery SubQuery
         * @param qualifyingCriteria List<KeyValueTrio>
         * @param groupFields List<String>
         * @param distinctFields List<String>
         * @param count int
         */
        public RetrieveStudentByTranscript(SubQuery studentSubQuery,
                List<KeyValueTrio> qualifyingCriteria,
                List<String> groupFields,
                List<String> distinctFields,
                int count) {
            super();
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);
            ComparisonType.addQualifyingCriteria(criteria, qualifyingCriteria);
            m_qualifyingTranscripts =
                    getBroker().getGroupedCollectionByQuery(new BeanQuery(Transcript.class, criteria),
                            Transcript.COL_STUDENT_OID, 128);
            System.out.println(
                    "Transcript: " + m_qualifyingTranscripts.size() + " - " + qualifyingCriteria.toString());
            m_groupFields = groupFields;
            m_distinctFields = distinctFields;
            m_numRequired = count;
        }

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getAliasBeanPath(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getAliasBeanPath(HighSchoolDataCollection data,
                                          StateReportEntity entity,
                                          FieldDefinition field) {
            return data.translateAliasToJavaName((String) field.getParameter(), false);
        }

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field) {
            SisStudent student = (SisStudent) entity.getBean();
            Collection<Transcript> transcripts = m_qualifyingTranscripts.get(student.getOid());
            String value = "N";
            if (transcripts != null && !transcripts.isEmpty()) {
                if (m_numRequired <= 1) {
                    value = "Y";
                } else {
                    Map<String, Set<String>> distinctKeysByGroup = new HashMap();
                    BigDecimal sum = new BigDecimal(0);
                    for (Transcript transcript : transcripts) {
                        if (CALC_ID_RIGOR_SCIENCE.equals(field.getCalcId())
                                && transcript.getTotalCredit() != null) {
                            sum = sum.add(transcript.getTotalCredit());
                            if (sum.compareTo(new BigDecimal(4.0)) >= 0) {
                                return "Y";
                            }
                        } else if (CALC_ID_RIGOR_ADV_TECH.equals(field.getCalcId())
                                && transcript.getTotalCredit() != null) {
                            sum = sum.add(transcript.getTotalCredit());
                            if (sum.compareTo(new BigDecimal(2.0)) >= 0) {
                                return "Y";
                            }
                        } else {
                            Course course = null;
                            if (transcript.getEquivalentSchoolCourse() != null) {
                                course = transcript.getEquivalentSchoolCourse().getCourse();
                            } else {
                                if (transcript.getSchoolCourse() != null) {
                                    course = transcript.getSchoolCourse().getCourse();
                                }
                            }
                            if (course != null) {
                                String groupKey = getKey(course, m_groupFields);
                                String distinctKey = getKey(course, m_distinctFields);

                                if (distinctKeysByGroup.containsKey(groupKey)) {
                                    Set distinctKeys = distinctKeysByGroup.get(groupKey);
                                    distinctKeys.add(distinctKey);
                                    if (distinctKeys.size() >= m_numRequired) {
                                        value = "Y";
                                        break;
                                    }
                                } else {
                                    Set distinctKeys = new HashSet();
                                    distinctKeys.add(distinctKey);
                                    distinctKeysByGroup.put(groupKey, distinctKeys);
                                }
                            }
                        }

                    }
                }
            }
            return value;
        }

        /**
         * Gets the key.
         *
         * @param course Course
         * @param fields List<String>
         * @return String
         */
        private String getKey(Course course, List<String> fields) {
            String key = "key";
            if (fields != null && !fields.isEmpty()) {
                StringBuilder keyBuilder = new StringBuilder();
                for (String beanPath : fields) {
                    keyBuilder.append(course.getFieldValueByBeanPath(beanPath));
                    keyBuilder.append("|");
                }
                key = keyBuilder.toString();
            }
            return key;
        }
    }

    /**
     * The Class RetrieveStudentBridgeAssessment.
     */
    protected class RetrieveStudentBridgeAssessment extends RetrieveStudentAlias {
        private String m_asdId;
        private BridgeHelper m_helper;
        private String m_hsaFieldName;

        /**
         * Instantiates a new retrieve student bridge assessment.
         *
         * @param helper BridgeHelper
         * @param asdId String
         * @param hsaFieldName String
         */
        public RetrieveStudentBridgeAssessment(BridgeHelper helper, String asdId, String hsaFieldName) {
            m_helper = helper;
            m_asdId = asdId;
            m_hsaFieldName = hsaFieldName;
        }

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getAliasBeanPath(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getAliasBeanPath(HighSchoolDataCollection data,
                                          StateReportEntity entity,
                                          FieldDefinition field) {
            return data.translateAliasToJavaName((String) field.getParameter(), false);
        }

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentByAssessment#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field) {
            String value = "N";
            String hsaValue = entity.getFieldValue(m_hsaFieldName);
            SisStudent student = (SisStudent) entity.getBean();
            if ("10".equals(hsaValue) || "11".equals(hsaValue)) {
                value = "N";
            }
            if (m_helper.isQualified(m_asdId, student.getOid())) {
                value = BRIDGE_PLAN_COMPLETED;
            } else if ("31".equals(hsaValue)) {
                value = "E";
            }
            return value;
        }

    }

    /**
     * The Class RetrieveStudentCCR.
     */
    protected class RetrieveStudentCCR extends RetrieveStudentAlias {
        private int m_yog;
        private String m_beanPath;

        /**
         * Instantiates a new retrieve student CCR.
         *
         * @param yog int
         * @param beanPath String
         */
        public RetrieveStudentCCR(int yog, String beanPath) {
            super();
            m_yog = yog;
            m_beanPath = beanPath;
        }

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getAliasBeanPath(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getAliasBeanPath(HighSchoolDataCollection data,
                                          StateReportEntity entity,
                                          FieldDefinition field) {
            return data.translateAliasToJavaName((String) field.getParameter(), false);
        }

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field)
                throws X2BaseException {
            String value = "N";
            SisStudent student = (SisStudent) entity.getBean();
            if (student.getYog() == m_yog) {
                value = getPropertyAsBoolean(student, m_beanPath) ? "Y" : "N";
            }

            if (CALC_ID_CCR_12_MATH.equals(field.getCalcId())
                    && StringUtils.equals(entity.getFieldValue(FIELD_CCR_11_MATH), "Y")) {
                value = "Y";
            }
            if (CALC_ID_CCR_12_ELA.equals(field.getCalcId())
                    && StringUtils.equals(entity.getFieldValue(FIELD_CCR_11_ELA), "Y")) {
                value = "Y";
            }
            return value;
        }

    }

    /**
     * The Class RetrieveStudentCTEConcentrator.
     */
    protected class RetrieveStudentCIPCode extends RetrieveStudentAlias {
        static final private String ALIAS_CIP_CODE = "DOE CTE CIP";

        private Map<String, List<KeyValuePair<String, Integer>>> m_studentCodes = new HashMap();

        /**
         * Instantiates a new retrieve student CTE concentrator.
         *
         * @param studentSubQuery SubQuery
         * @param ctx DistrictSchoolYearContext
         */
        public RetrieveStudentCIPCode(SubQuery studentSubQuery, DistrictSchoolYearContext ctx) {
            String cteCIPCodeField = translateAliasToJavaName(ALIAS_CIP_CODE, true);
            if (!StringUtils.isEmpty(cteCIPCodeField)) {
                Map<String, Set<String>> processedSections = new HashMap();
                X2Criteria criteria = new X2Criteria();
                criteria.addIn(StudentSchedule.COL_STUDENT_OID, studentSubQuery);
                criteria.addEqualTo(StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        Schedule.COL_DISTRICT_CONTEXT_OID, ctx.getOid());
                criteria.addNotEmpty(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                        cteCIPCodeField, getBroker().getPersistenceKey());

                String[] columns = new String[] {StudentSchedule.COL_STUDENT_OID,
                        StudentSchedule.COL_SECTION_OID,
                        StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + cteCIPCodeField
                };
                ColumnQuery query = new ColumnQuery(StudentSchedule.class, columns, criteria);
                try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        String studentOid = (String) row[0];
                        String sectionOid = (String) row[1];
                        String cipCode = (String) row[2];
                        if (!StringUtils.isEmpty(cipCode)) {
                            cipCode = lookupStateValue(Course.class, cteCIPCodeField, cipCode);
                        }
                        if (!StringUtils.isEmpty(cipCode)) {
                            if (cipCode.length() > 6) {
                                cipCode = cipCode.substring(0, 6);
                            }
                            List<KeyValuePair<String, Integer>> codes = m_studentCodes.get(studentOid);
                            if (codes == null) {
                                codes = new LinkedList();
                                m_studentCodes.put(studentOid, codes);
                            }
                            int count = 0;
                            if (!codes.isEmpty()) {
                                Iterator<KeyValuePair<String, Integer>> codesIterator = codes.iterator();
                                while (codesIterator.hasNext()) {
                                    KeyValuePair<String, Integer> pair = codesIterator.next();
                                    if (pair.getKey().equals(cipCode)) {
                                        count += pair.getValue().intValue();
                                        codesIterator.remove();
                                    }
                                }
                            }
                            addProcessedSection(processedSections, studentOid, sectionOid);
                            codes.add(new KeyValuePair(cipCode, Integer.valueOf(count + 1)));
                        }
                    }
                }

                // Process matching transcripts
                criteria = new X2Criteria();
                criteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);
                criteria.addEqualTo(Transcript.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        Schedule.COL_DISTRICT_CONTEXT_OID, ctx.getOid());
                criteria.addNotEmpty(Transcript.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                        cteCIPCodeField, getBroker().getPersistenceKey());

                columns = new String[] {Transcript.COL_STUDENT_OID,
                        Transcript.COL_MASTER_SCHEDULE_OID,
                        Transcript.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + cteCIPCodeField
                };
                query = new ColumnQuery(Transcript.class, columns, criteria);
                try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        String studentOid = (String) row[0];
                        String sectionOid = (String) row[1];
                        String cipCode = (String) row[2];
                        if (!StringUtils.isEmpty(cipCode)) {
                            cipCode = lookupStateValue(Course.class, cteCIPCodeField, cipCode);
                        }
                        if (!StringUtils.isEmpty(cipCode)
                                && !isProcessedSection(processedSections, studentOid, sectionOid)) {
                            if (cipCode.length() > 6) {
                                cipCode = cipCode.substring(0, 6);
                            }
                            List<KeyValuePair<String, Integer>> codes = m_studentCodes.get(studentOid);
                            if (codes == null) {
                                codes = new LinkedList();
                                m_studentCodes.put(studentOid, codes);
                            }
                            int count = 0;
                            if (!codes.isEmpty()) {
                                Iterator<KeyValuePair<String, Integer>> codesIterator = codes.iterator();
                                while (codesIterator.hasNext()) {
                                    KeyValuePair<String, Integer> pair = codesIterator.next();
                                    if (pair.getKey().equals(cipCode)) {
                                        count += pair.getValue().intValue();
                                        codesIterator.remove();
                                    }
                                }
                            }
                            addProcessedSection(processedSections, studentOid, sectionOid);
                            codes.add(new KeyValuePair(cipCode, Integer.valueOf(count + 1)));
                        }
                    }
                }
            }
        }

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getAliasBeanPath(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getAliasBeanPath(HighSchoolDataCollection data,
                                          StateReportEntity entity,
                                          FieldDefinition field) {
            return data.translateAliasToJavaName((String) field.getParameter(), false);
        }

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field) {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();

            List<KeyValuePair<String, Integer>> codes = m_studentCodes.get(student.getOid());
            if (codes != null && !codes.isEmpty()) {
                if (codes.size() == 1) {
                    value = codes.iterator().next().getKey();
                } else {
                    int max = 0;
                    for (KeyValuePair<String, Integer> code : codes) {
                        if (code.getValue().intValue() > max) {
                            value = code.getKey();
                            max = code.getValue().intValue();
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Adds the processed section.
         *
         * @param processedSections Map<String,Set<String>>
         * @param studentOid String
         * @param sectionOid String
         */
        private void addProcessedSection(Map<String, Set<String>> processedSections,
                                         String studentOid,
                                         String sectionOid) {
            Set<String> sectionOids = processedSections.get(studentOid);
            if (sectionOids == null) {
                sectionOids = new HashSet();
                processedSections.put(studentOid, sectionOids);
            }
            sectionOids.add(sectionOid);
        }

        /**
         * Checks if is processed section.
         *
         * @param processedSections Map<String,Set<String>>
         * @param studentOid String
         * @param sectionOid String
         * @return true, if is processed section
         */
        private boolean isProcessedSection(Map<String, Set<String>> processedSections,
                                           String studentOid,
                                           String sectionOid) {
            boolean isProcessed = false;
            Set<String> sectionOids = processedSections.get(studentOid);
            if (sectionOids != null) {
                isProcessed = sectionOids.contains(sectionOid);
            }
            return isProcessed;
        }

    }

    /**
     * The Class RetrieveStudentGrade.
     */
    protected class RetrieveStudentGrade implements FieldRetriever {
        Map<String, String> m_mapStudentGradeLevel = new HashMap();

        /**
         * Instantiates a new retrieve student grade.
         *
         * @param subQueryStudentOid SubQuery
         */
        public RetrieveStudentGrade(SubQuery subQueryStudentOid) {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentContextAttributes.COL_STUDENT_OID, subQueryStudentOid);
            criteria.addEqualTo(StudentContextAttributes.COL_CONTEXT_OID, getCurrentContext().getOid());
            criteria.addNotEmpty(StudentContextAttributes.COL_GRADE_LEVEL, getBroker().getPersistenceKey());

            String[] columns =
                    new String[] {StudentContextAttributes.COL_STUDENT_OID,
                            StudentContextAttributes.COL_GRADE_LEVEL};
            ColumnQuery query = new ColumnQuery(StudentContextAttributes.class, columns, criteria);
            try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    m_mapStudentGradeLevel.put((String) row[0], (String) row[1]);
                }
            }
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
            String gradeLevel = ((SisStudent) entity.getBean()).getGradeLevel();
            if (m_mapStudentGradeLevel.containsKey(entity.getBean().getOid())) {
                gradeLevel = m_mapStudentGradeLevel.get(entity.getBean().getOid());
            }
            if (!StringUtils.isEmpty(gradeLevel)) {
                gradeLevel = lookupStateValue(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel);
            }
            return gradeLevel;
        }
    }

    /**
     * The Class RetrieveStudentHSCompletionStatus.
     */
    protected class RetrieveStudentHSCompletionStatus extends RetrieveStudentAlias {
        private GraduateHelper m_helper;

        /**
         * Instantiates a new retrieve student HS completion status.
         *
         * @param helper GraduateHelper
         */
        public RetrieveStudentHSCompletionStatus(GraduateHelper helper) {
            m_helper = helper;
        }

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getAliasBeanPath(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getAliasBeanPath(HighSchoolDataCollection data,
                                          StateReportEntity entity,
                                          FieldDefinition field) {
            return data.translateAliasToJavaName((String) field.getParameter(), false);
        }

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field)
                throws X2BaseException {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            boolean metLocalGradReq = m_helper.isGraduate(student.getOid());
            boolean metCollegeReq = m_helper.metCollegeRequirements(student.getOid());
            boolean cteConcentrator = isCteCompleter(entity);
            if (metLocalGradReq && metCollegeReq) {
                value = "01";
            }
            if (cteConcentrator) {
                value = "02";
            }
            if (metLocalGradReq && metCollegeReq && cteConcentrator) {
                value = "03";
            }
            if (metLocalGradReq && !metCollegeReq && !cteConcentrator) {
                value = "04";
            }
            if (value == null) {
                value = "00";
            }
            return value;
        }

        /**
         * Checks if is cte completer.
         *
         * @param entity SisStudent
         * @return true, if is cte completer
         * @throws X2BaseException exception
         */
        private boolean isCteCompleter(StateReportEntity entity) throws X2BaseException {

            return "Y".equals(entity.getFieldValue(FIELD_CTE_CONCENTRATOR));
        }
    }


    /**
     * The Class RetrieveStudentMetGradRequirement.
     */
    protected class RetrieveStudentMetGradRequirement extends RetrieveStudentAlias {
        private GraduateHelper m_helper;

        /**
         * Instantiates a new retrieve student met grad requirement.
         *
         * @param helper GraduateHelper
         */
        public RetrieveStudentMetGradRequirement(GraduateHelper helper) {
            m_helper = helper;
        }

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getAliasBeanPath(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getAliasBeanPath(HighSchoolDataCollection data,
                                          StateReportEntity entity,
                                          FieldDefinition field) {
            return data.translateAliasToJavaName((String) field.getParameter(), false);
        }

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            return m_helper.isGraduate(student.getOid()) ? "Y" : "N";
        }

    }
    /**
     * The Class RetrieveStudentMHSA.
     */
    protected class RetrieveStudentMHSA extends RetrieveStudentAlias {
        static final private String HSA_ALIAS_SUBJECT_AREA = "HSA_SUBJECT_AREA";
        static final private String HSA_ALIAS_PASS_FLAG = "HSA_PASS_FLAG";
        static final private String PARCC_ALIAS_TEST_CODE = "PARCCTSTCODE";
        static final private String PARCC_ASSESSMENT_DEFINITON_ID = "PARCCTest-15-16";

        final private BigDecimal SCORE_TEST_GOVT = BigDecimal.valueOf(394);

        protected String m_fieldParccTestCode;
        final private KeyValuePair m_aliasValues[] = {
                new KeyValuePair(CALC_PARAM_SUBJECT_MATH, "all-std-MHSAGraduationRequirementMath"),
                new KeyValuePair(CALC_PARAM_SUBJECT_ELA, "all-std-MHSAGraduationRequirementELA"),
                new KeyValuePair(CALC_PARAM_SUBJECT_SCIENCE, "all-std-MHSAGraduationRequirementScience"),
                new KeyValuePair(CALC_PARAM_SUBJECT_GOVERNMENT, "all-std-MHSAGraduationRequirementGovernment")
        };
        private Map<String, Collection<StudentAssessment>> m_governmentAssessmentsHSA = new HashMap();
        private Set<String> m_qualifiedHsaGovernment = new HashSet();
        private Set<String> m_qualifiedHsaMath = new HashSet();
        private Set<String> m_qualifiedHsaMisa = new HashSet();
        private Set<String> m_nonQualifiedHsaMisa = new HashSet();
        private Set<String> m_qualifiedScoreGovernment = new HashSet();
        private Set<String> m_qualifiedScoreScience = new HashSet();
        private Map<String, Collection<StudentAssessment>> m_parccAssessments = new HashMap();
        private Map<String, Collection<StudentAssessment>> m_parccAssessmentsScience = new HashMap();
        private Map<String, Collection<StudentAssessment>> m_parccAssessmentsGovt = new HashMap();
        private Set<String> m_wHsaMisa = new HashSet();

        /**
         * Instantiates a new retrieve student MHSA.
         *
         * @param studentSubQuery SubQuery
         * @param bridgeHelper BridgeHelper
         */
        public RetrieveStudentMHSA(SubQuery studentSubQuery) {
            super();
            loadParccTests(studentSubQuery);
            loadParccTestsForGov(studentSubQuery);
            loadHsaSets(studentSubQuery);
            loadParccTestsScience(studentSubQuery);
        }

        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field) {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            Collection<StudentAssessment> assessments = null;

            if (StringUtils.equals(entity.getFieldValue(FIELD_SPED_CERT), "Y")) {
                return "18";
            }

            switch ((String) field.getParameter()) {
                case CALC_PARAM_SUBJECT_MATH:
                    if (m_qualifiedHsaMath.contains(student.getOid())) {
                        value = "10";
                    } else {
                        assessments = m_parccAssessments.get(student.getOid());
                        value = getMHSAValueMATH(entity, assessments, "ALG01");
                    }
                    break;
                case CALC_PARAM_SUBJECT_ELA:
                    assessments = m_parccAssessments.get(student.getOid());
                    value = getMHSAValueELA(entity, assessments, "ELA10");
                    break;
                case CALC_PARAM_SUBJECT_SCIENCE:
                    value = getMHSAPassFailScience(entity, m_parccAssessmentsScience.get(student.getOid()));
                    if (value == null && m_parccAssessmentsScience.get(student.getOid()) != null) {
                        StudentAssessment asmWithPerfLevel = m_parccAssessmentsScience.get(student.getOid()).stream()
                                .filter(asm -> Arrays.asList("3", "4").contains(asm.getPerformanceLevel())).findAny()
                                .orElse(null);
                        StudentAssessment asmExists = m_parccAssessmentsScience.get(student.getOid()).stream()
                                .filter(asm -> asm.getPerformanceLevel() != null || (asm.getScaleScore() != null
                                        && asm.getScaleScore().compareTo(BigDecimal.ZERO) > 0))
                                .findAny()
                                .orElse(null);

                        if (asmWithPerfLevel != null) {
                            value = "10";

                        } else if (asmExists != null) {
                            value = "14";
                        } else if (m_parccAssessmentsScience.get(student.getOid()).stream()
                                .filter(asm -> "W".equals(asm.getPerformanceLevel())).findAny().orElse(null) != null) {
                            value = "18";
                        }
                    }
                    if (value == null) {
                        if (m_qualifiedHsaMisa.contains(student.getOid())) {
                            value = "10";
                            break;
                        } else if (m_nonQualifiedHsaMisa.contains(student.getOid())) {
                            value = "14";
                            break;
                        } else if (m_wHsaMisa.contains(student.getOid())) {
                            value = "18";
                        } else {
                            value = "30";
                        }
                    }
                    break;
                case CALC_PARAM_SUBJECT_GOVERNMENT:
                    /**
                     * Code "19" is not valid this year.
                     *
                     * if (m_stdSpecialCourseLevels.contains(student.getOid())) {
                     * value = "19";
                     * break;
                     * }
                     *
                     */
                    Collection<StudentAssessment> assessmentsParccGovt = m_parccAssessmentsGovt.get(student.getOid());
                    if (assessmentsParccGovt != null) {
                        boolean isAsmWithWfound = false;

                        for (StudentAssessment parccGovtAsm : assessmentsParccGovt) {
                            String schoolBeanPath =
                                    translateAlias(parccGovtAsm.getAssessmentDefinition(), PARCC_ALIAS_SCHOOL);
                            if (schoolBeanPath != null
                                    && StringUtils.equals((String) parccGovtAsm.getFieldValueByBeanPath(schoolBeanPath),
                                            "OOS")) {
                                return "15";
                            }
                            isAsmWithWfound = "W".equals(parccGovtAsm.getPerformanceLevel());
                            if (parccGovtAsm.getScaleScore() != null
                                    && parccGovtAsm.getScaleScore().compareTo(SCORE_TEST_GOVT) >= 0) {
                                value = "10";
                                break;
                            }
                        }
                        if (StringUtils.isEmpty(value) && isAsmWithWfound) {
                            value = "18";
                        }
                        if (StringUtils.isEmpty(value)) {
                            value = "14";
                        }
                    }
                    if (value == null || !"10".equals(value)) {
                        if (m_qualifiedHsaGovernment.contains(student.getOid())) {
                            value = "10";
                        } else if (m_governmentAssessmentsHSA.containsKey(student.getOid())) {
                            value = "14";
                            for (StudentAssessment asmToCheck : m_governmentAssessmentsHSA.get(student.getOid())) {
                                if ("W".equals(asmToCheck.getPerformanceLevel())) {
                                    value = "18";
                                    break;
                                }
                            }
                        }
                    }
                    if (StringUtils.isEmpty(value)) {
                        value = "30";
                    }
                    break;
                default:
                    break;
            }
            return value;
        }

        /**
         * Gets the alias bean path.
         *
         * @param data HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getAliasBeanPath(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getAliasBeanPath(HighSchoolDataCollection data,
                                          StateReportEntity entity,
                                          FieldDefinition field) {
            String value = null;
            for (KeyValuePair pair : m_aliasValues) {
                if (field.getParameter() != null && field.getParameter().equals(pair.getKey())) {
                    value = data.translateAliasToJavaName((String) pair.getValue(), false);
                }
            }
            return value;
        }

        /**
         * Find best score.
         *
         * @param assessments Collection<StudentAssessment>
         * @param testCode String
         * @return StudentAssessment
         */
        private StudentAssessment findBestScore(Collection<StudentAssessment> assessments, String testCode) {
            StudentAssessment asm = null;
            BigDecimal bestScore = BigDecimal.ZERO;
            if (assessments != null && !assessments.isEmpty()) {
                for (StudentAssessment assessment : assessments) {
                    String code = (String) assessment.getFieldValueByBeanPath(m_fieldParccTestCode);
                    if (!StringUtils.isEmpty(code) && code.equals(testCode)) {
                        if (assessment.getScaleScore() != null
                                && assessment.getScaleScore().compareTo(bestScore) >= 0) {
                            bestScore = assessment.getScaleScore();
                            asm = assessment;
                        }
                        if ((assessment.getScaleScore() == null
                                || BigDecimal.ZERO.compareTo(assessment.getScaleScore()) == 0)
                                && "W".equals(assessment.getPerformanceLevel())) {
                            asm = assessment;
                            break;
                        }
                    }
                }
            }
            return asm;
        }

        /**
         * Load hsa sets.
         *
         * @param studentSubQuery SubQuery
         */
        private void loadHsaSets(SubQuery studentSubQuery) {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
            ComparisonType.addQualifyingCriteria(criteria, Arrays.asList(new KeyValueTrio[] {
                    new KeyValueTrio(
                            StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                    AssessmentDefinition.COL_ID,
                            "HSA - ALGEBRA", ComparisonType.EQUALS),
                    new KeyValueTrio(StudentAssessment.COL_SCALE_SCORE, BigDecimal.valueOf(412.0),
                            ComparisonType.GREATER_THAN_OR_EQUALS)}));
            ComparisonType.populateQualifiedList(getBroker(), StudentAssessment.class, criteria,
                    StudentAssessment.COL_STUDENT_OID, m_qualifiedHsaMath);

            String beanPathPassFlag = translateAliasToJavaName(HSA_ALIAS_PASS_FLAG, true);
            String beanPathSubjectArea = translateAliasToJavaName(HSA_ALIAS_SUBJECT_AREA, true);
            if (!StringUtils.isEmpty(beanPathPassFlag) && !StringUtils.isEmpty(beanPathSubjectArea)) {
                criteria = new X2Criteria();
                criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
                ComparisonType.addQualifyingCriteria(criteria, Arrays.asList(new KeyValueTrio[] {
                        new KeyValueTrio(
                                StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                        AssessmentDefinition.COL_ID,
                                "HSA", ComparisonType.EQUALS),
                        new KeyValueTrio(StudentAssessment.COL_PERFORMANCE_LEVEL, "P", ComparisonType.EQUALS),
                        new KeyValueTrio(beanPathSubjectArea, "HSMISA", ComparisonType.EQUALS)}));
                ComparisonType.populateQualifiedList(getBroker(), StudentAssessment.class, criteria,
                        StudentAssessment.COL_STUDENT_OID, m_qualifiedHsaMisa);

                criteria = new X2Criteria();
                criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
                ComparisonType.addQualifyingCriteria(criteria, Arrays.asList(new KeyValueTrio[] {
                        new KeyValueTrio(
                                StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                        AssessmentDefinition.COL_ID,
                                "HSA", ComparisonType.EQUALS),
                        new KeyValueTrio(StudentAssessment.COL_PERFORMANCE_LEVEL, "W", ComparisonType.EQUALS),
                        new KeyValueTrio(beanPathSubjectArea, "HSMISA", ComparisonType.EQUALS)}));
                ComparisonType.populateQualifiedList(getBroker(), StudentAssessment.class, criteria,
                        StudentAssessment.COL_STUDENT_OID, m_wHsaMisa);

                criteria = new X2Criteria();
                criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
                ComparisonType.addQualifyingCriteria(criteria, Arrays.asList(new KeyValueTrio[] {
                        new KeyValueTrio(
                                StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                        AssessmentDefinition.COL_ID,
                                "HSA", ComparisonType.EQUALS),
                        new KeyValueTrio(beanPathPassFlag, "P", ComparisonType.NOT_EQUALS),
                        new KeyValueTrio(beanPathSubjectArea, "HSMISA", ComparisonType.EQUALS)}));
                ComparisonType.populateQualifiedList(getBroker(), StudentAssessment.class, criteria,
                        StudentAssessment.COL_STUDENT_OID, m_nonQualifiedHsaMisa);

                criteria = new X2Criteria();
                criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
                ComparisonType.addQualifyingCriteria(criteria, Arrays.asList(new KeyValueTrio[] {
                        new KeyValueTrio(
                                StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                        AssessmentDefinition.COL_ID,
                                "HSA", ComparisonType.EQUALS),
                        new KeyValueTrio(beanPathPassFlag, "P", ComparisonType.EQUALS),
                        new KeyValueTrio(beanPathSubjectArea, "HSGOV", ComparisonType.EQUALS)}));
                ComparisonType.populateQualifiedList(getBroker(), StudentAssessment.class, criteria,
                        StudentAssessment.COL_STUDENT_OID, m_qualifiedHsaGovernment);

                criteria = new X2Criteria();
                criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
                criteria.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER
                        + AssessmentDefinition.COL_ID, "HSA");
                criteria.addEqualTo(beanPathSubjectArea, "HSGOV");
                m_governmentAssessmentsHSA =
                        getBroker().getGroupedCollectionByQuery(new QueryByCriteria(StudentAssessment.class, criteria),
                                StudentAssessment.COL_STUDENT_OID, 1024);
            }
            if (!StringUtils.isEmpty(beanPathSubjectArea)) {
                criteria = new X2Criteria();
                criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
                ComparisonType.addQualifyingCriteria(criteria, Arrays.asList(new KeyValueTrio[] {
                        new KeyValueTrio(
                                StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                        AssessmentDefinition.COL_ID,
                                "HSA", ComparisonType.EQUALS),
                        new KeyValueTrio(StudentAssessment.COL_SCALE_SCORE, BigDecimal.valueOf(394.0),
                                ComparisonType.GREATER_THAN_OR_EQUALS),
                        new KeyValueTrio(beanPathSubjectArea, "HSGOV", ComparisonType.EQUALS)}));
                ComparisonType.populateQualifiedList(getBroker(), StudentAssessment.class, criteria,
                        StudentAssessment.COL_STUDENT_OID, m_qualifiedScoreGovernment);
            }

            criteria = new X2Criteria();
            criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
            ComparisonType.addQualifyingCriteria(criteria, Arrays.asList(new KeyValueTrio[] {
                    new KeyValueTrio(
                            StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                    AssessmentDefinition.COL_ID,
                            "HSA", ComparisonType.EQUALS),
                    new KeyValueTrio(StudentAssessment.COL_SCALE_SCORE, BigDecimal.valueOf(400.0),
                            ComparisonType.GREATER_THAN_OR_EQUALS),
                    new KeyValueTrio(beanPathSubjectArea, "HSMISA", ComparisonType.EQUALS)}));
            ComparisonType.populateQualifiedList(getBroker(), StudentAssessment.class, criteria,
                    StudentAssessment.COL_STUDENT_OID, m_qualifiedScoreScience);
        }

        /**
         * Load parcc tests.
         *
         * @param studentSubQuery SubQuery
         */
        private void loadParccTests(SubQuery studentSubQuery) {
            AssessmentDefinition asd = getAssessmentDefinition(PARCC_ASSESSMENT_DEFINITON_ID);
            if (asd != null) {
                m_fieldParccTestCode = translateAlias(asd, PARCC_ALIAS_TEST_CODE);
                if (!StringUtils.isEmpty(m_fieldParccTestCode)) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, asd.getOid());
                    criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
                    criteria.addIn(m_fieldParccTestCode, Arrays.asList(new String[] {"ALG01", "ELA10"}));

                    X2Criteria andCriteria = new X2Criteria();
                    andCriteria.addNotEmpty(StudentAssessment.COL_PERFORMANCE_LEVEL, getBroker().getPersistenceKey());
                    X2Criteria orScaleScoreCrit = new X2Criteria();
                    orScaleScoreCrit.addGreaterThan(StudentAssessment.COL_SCALE_SCORE, BigDecimal.ZERO);
                    andCriteria.addOrCriteria(orScaleScoreCrit);
                    X2Criteria orWaviedPerfomanceLevelCrit = new X2Criteria();
                    orWaviedPerfomanceLevelCrit.addNotEqualTo(StudentAssessment.COL_PERFORMANCE_LEVEL, "W");
                    andCriteria.addOrCriteria(orWaviedPerfomanceLevelCrit);

                    criteria.addAndCriteria(andCriteria);
                    m_parccAssessments =
                            getBroker().getGroupedCollectionByQuery(
                                    new BeanQuery(StudentAssessment.class, criteria),
                                    StudentAssessment.COL_STUDENT_OID, 128);
                } else {
                    addSetupError("Assessment Definition Alias Not Found", PARCC_ALIAS_TEST_CODE);
                }
            } else {
                addSetupError("Assessment Definition Not Found", PARCC_ASSESSMENT_DEFINITON_ID);
            }
        }

        /**
         * Load parcc tests.
         *
         * @param studentSubQuery SubQuery
         */
        private void loadParccTestsScience(SubQuery studentSubQuery) {
            AssessmentDefinition asd = getAssessmentDefinition(PARCC_ASSESSMENT_DEFINITON_ID);
            if (asd != null) {
                if (StringUtils.isEmpty(m_fieldParccTestCode)) {
                    m_fieldParccTestCode = translateAlias(asd, PARCC_ALIAS_TEST_CODE);
                }
                if (!StringUtils.isEmpty(m_fieldParccTestCode)) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, asd.getOid());
                    criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
                    criteria.addIn(m_fieldParccTestCode,
                            Arrays.asList(new String[] {"HSMISA", "BIOL", "LSMISA", "LSCI"}));

                    X2Criteria andCriteria = new X2Criteria();
                    andCriteria.addNotEmpty(StudentAssessment.COL_PERFORMANCE_LEVEL, getBroker().getPersistenceKey());
                    X2Criteria orScaleScoreCrit = new X2Criteria();
                    orScaleScoreCrit.addGreaterThan(StudentAssessment.COL_SCALE_SCORE, BigDecimal.ZERO);
                    andCriteria.addOrCriteria(orScaleScoreCrit);
                    X2Criteria orWaviedPerfomanceLevelCrit = new X2Criteria();
                    orWaviedPerfomanceLevelCrit.addNotEqualTo(StudentAssessment.COL_PERFORMANCE_LEVEL, "W");
                    andCriteria.addOrCriteria(orWaviedPerfomanceLevelCrit);
                    criteria.addAndCriteria(andCriteria);
                    m_parccAssessmentsScience =
                            getBroker().getGroupedCollectionByQuery(
                                    new BeanQuery(StudentAssessment.class, criteria),
                                    StudentAssessment.COL_STUDENT_OID, 128);
                } else {
                    addSetupError("Assessment Definition Alias Not Found", PARCC_ALIAS_TEST_CODE);
                }
            } else {
                addSetupError("Assessment Definition Not Found", PARCC_ASSESSMENT_DEFINITON_ID);
            }
        }

        /**
         * Load parcc tests.
         *
         * @param studentSubQuery SubQuery
         */
        private void loadParccTestsForGov(SubQuery studentSubQuery) {
            AssessmentDefinition asd = getAssessmentDefinition(PARCC_ASSESSMENT_DEFINITON_ID);
            if (asd != null) {
                m_fieldParccTestCode = translateAlias(asd, PARCC_ALIAS_TEST_CODE);
                if (!StringUtils.isEmpty(m_fieldParccTestCode)) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, asd.getOid());
                    criteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
                    criteria.addIn(m_fieldParccTestCode, Arrays.asList(new String[] {"GOVT"}));

                    X2Criteria andCriteria = new X2Criteria();
                    andCriteria.addNotEmpty(StudentAssessment.COL_PERFORMANCE_LEVEL, getBroker().getPersistenceKey());
                    X2Criteria orScaleScoreCrit = new X2Criteria();
                    orScaleScoreCrit.addGreaterThan(StudentAssessment.COL_SCALE_SCORE, BigDecimal.ZERO);
                    andCriteria.addOrCriteria(orScaleScoreCrit);
                    X2Criteria orWaviedPerfomanceLevelCrit = new X2Criteria();
                    orWaviedPerfomanceLevelCrit.addNotEqualTo(StudentAssessment.COL_PERFORMANCE_LEVEL, "W");
                    andCriteria.addOrCriteria(orWaviedPerfomanceLevelCrit);

                    m_parccAssessmentsGovt =
                            getBroker().getGroupedCollectionByQuery(
                                    new BeanQuery(StudentAssessment.class, criteria),
                                    StudentAssessment.COL_STUDENT_OID, 128);
                } else {
                    addSetupError("Assessment Definition Alias Not Found", PARCC_ALIAS_TEST_CODE);
                }
            } else {
                addSetupError("Assessment Definition Not Found", PARCC_ASSESSMENT_DEFINITON_ID);
            }
        }

        /**
         * Gets the MHSA pass fail.
         *
         * @param entity StateReportEntity
         * @param assessments Collection<StudentAssessment>
         * @return String
         */
        private String getMHSAPassFailScience(StateReportEntity entity,
                                              Collection<StudentAssessment> assessments) {
            String value = null;
            PlainDate latestDate = null;
            if (assessments != null && !assessments.isEmpty()) {
                for (StudentAssessment assessment : assessments) {
                    String schoolBeanPath =
                            translateAlias(assessment.getAssessmentDefinition(), PARCC_ALIAS_SCHOOL);
                    String code = (String) assessment.getFieldValueByBeanPath(m_fieldParccTestCode);
                    if (Arrays.asList("BIOL", "LSMISA", "HSMISA").contains(code) && schoolBeanPath != null
                            && StringUtils.equals((String) assessment.getFieldValueByBeanPath(schoolBeanPath),
                                    "OOS")) {
                        return "15";
                    }
                    if (assessment.getDate() != null
                            && (latestDate == null || latestDate.before(assessment.getDate()))) {
                        latestDate = assessment.getDate();
                    }
                }
            }
            return value;
        }

        /**
         * Gets the MHSA value.
         *
         * @param entity StateReportEntity
         * @param assessments Collection<StudentAssessment>
         * @param testCodeMain String
         * @return String
         */
        private String getMHSAValueELA(StateReportEntity entity,
                                       Collection<StudentAssessment> assessments,
                                       String testCodeMain) {
            String value = null;
            StudentAssessment asmMain = findBestScore(assessments, testCodeMain);
            if (asmMain == null && assessments != null) {
                asmMain = assessments.stream()
                        .filter(asmConsider -> asmConsider.getPerformanceLevel() != null
                                && Arrays.asList("2", "3", "4", "5").contains(asmConsider.getPerformanceLevel())
                                && testCodeMain
                                        .equals(asmConsider.getFieldValueByBeanPath(m_fieldParccTestCode)))
                        .findAny().orElse(null);
            }
            if (asmMain != null) {
                String schoolBeanPath = translateAlias(asmMain.getAssessmentDefinition(), PARCC_ALIAS_SCHOOL);
                if (schoolBeanPath != null
                        && StringUtils.equals((String) asmMain.getFieldValueByBeanPath(schoolBeanPath), "OOS")) {
                    return "15";
                }
            }
            if (asmMain != null) {
                String asmMainPerfLevel = asmMain.getPerformanceLevel();
                int asmYear = DateUtils.getYear(asmMain.getDate());
                if ((Arrays.asList("4", "5").contains(asmMainPerfLevel) && asmYear < 2021)
                        || (Arrays.asList("2", "3").contains(asmMainPerfLevel) && 2021 == asmYear)
                        || (Arrays.asList("3", "4").contains(asmMainPerfLevel) && asmYear > 2021)) {
                    value = "10";
                }
            }
            if (StringUtils.isEmpty(value)) {
                if (asmMain == null) {
                    if (assessments != null) {
                        for (StudentAssessment assessment : assessments) {
                            String code = (String) assessment.getFieldValueByBeanPath(m_fieldParccTestCode);
                            if (!StringUtils.isEmpty(code) && code.equals(testCodeMain)
                                    && ((assessment.getPerformanceLevel() != null
                                            && !"W".equals(assessment.getPerformanceLevel()))
                                            || (assessment.getScaleScore() != null
                                                    && assessment.getScaleScore().compareTo(BigDecimal.ZERO) > 0))) {
                                value = "14";
                                break;
                            }
                        }
                    }
                } else if (((asmMain.getPerformanceLevel() != null && !"W".equals(asmMain.getPerformanceLevel()))
                        || (asmMain.getScaleScore() != null
                                && asmMain.getScaleScore().compareTo(BigDecimal.ZERO) > 0))) {
                    value = "14";
                }
            }
            if (StringUtils.isEmpty(value)) {
                if (asmMain != null && "W".equals(asmMain.getPerformanceLevel())) {
                    value = "18";
                } else if (assessments != null) {
                    for (StudentAssessment assessment : assessments) {
                        String code = (String) assessment.getFieldValueByBeanPath(m_fieldParccTestCode);
                        if (!StringUtils.isEmpty(code) && code.equals(testCodeMain)
                                && "W".equals(assessment.getPerformanceLevel())) {
                            value = "18";
                            break;
                        }
                    }
                }
            }
            if (StringUtils.isEmpty(value)) {
                value = "30";
            }
            return value;
        }

        /**
         * Gets the MHSA value.
         *
         * @param entity StateReportEntity
         * @param assessments Collection<StudentAssessment>
         * @param testCodeMain String
         * @return String
         */
        private String getMHSAValueMATH(StateReportEntity entity,
                                        Collection<StudentAssessment> assessments,
                                        String testCodeMain) {
            String value = null;
            StudentAssessment asmMain = findBestScore(assessments, testCodeMain);
            if (asmMain == null && assessments != null) {
                asmMain = assessments.stream()
                        .filter(asmConsider -> asmConsider.getPerformanceLevel() != null
                                && Arrays.asList("2", "3", "4", "5").contains(asmConsider.getPerformanceLevel())
                                && testCodeMain
                                        .equals(asmConsider.getFieldValueByBeanPath(m_fieldParccTestCode)))
                        .findAny().orElse(null);
            }
            if (asmMain != null) {
                String schoolBeanPath = translateAlias(asmMain.getAssessmentDefinition(), PARCC_ALIAS_SCHOOL);
                if (schoolBeanPath != null
                        && StringUtils.equals((String) asmMain.getFieldValueByBeanPath(schoolBeanPath), "OOS")) {
                    return "15";
                }
            }
            if (asmMain != null) {
                String asmMainPerfLevel = asmMain.getPerformanceLevel();
                int asmYear = DateUtils.getYear(asmMain.getDate());
                if ((Arrays.asList("4", "5").contains(asmMainPerfLevel) && asmYear < 2021)
                        || (Arrays.asList("2", "3").contains(asmMainPerfLevel) && 2021 == asmYear)
                        || (Arrays.asList("3", "4").contains(asmMainPerfLevel) && asmYear > 2021)) {
                    value = "10";
                }
            }
            if (StringUtils.isEmpty(value)) {
                if (asmMain == null) {
                    if (assessments != null) {
                        for (StudentAssessment assessment : assessments) {
                            String code = (String) assessment.getFieldValueByBeanPath(m_fieldParccTestCode);
                            if (!StringUtils.isEmpty(code) && code.equals(testCodeMain)
                                    && ((assessment.getPerformanceLevel() != null
                                            && !"W".equals(assessment.getPerformanceLevel()))
                                            || (assessment.getScaleScore() != null
                                                    && assessment.getScaleScore().compareTo(BigDecimal.ZERO) > 0))) {
                                value = "14";
                                break;
                            }
                        }
                    }
                } else if (((asmMain.getPerformanceLevel() != null && !"W".equals(asmMain.getPerformanceLevel()))
                        || (asmMain.getScaleScore() != null
                                && asmMain.getScaleScore().compareTo(BigDecimal.ZERO) > 0))) {
                    value = "14";
                }
            }
            if (StringUtils.isEmpty(value)) {
                if (asmMain != null && "W".equals(asmMain.getPerformanceLevel())) {
                    value = "18";
                } else if (assessments != null) {
                    for (StudentAssessment assessment : assessments) {
                        String code = (String) assessment.getFieldValueByBeanPath(m_fieldParccTestCode);
                        if (!StringUtils.isEmpty(code) && code.equals(testCodeMain)
                                && "W".equals(assessment.getPerformanceLevel())) {
                            value = "18";
                            break;
                        }
                    }
                }
            }
            if (StringUtils.isEmpty(value)) {
                value = "30";
            }

            return value;
        }

    }

    /**
     * The Class RetrieveStudentTSA.
     */
    protected class RetrieveStudentTSA extends RetrieveStudentByAlias {
        private final List<String> COURSE_NUMBERS = Arrays.asList("805", "806", "807", "808");

        private Map<String, List<CourseGpa>> m_stdCourseGPAs = new HashMap<>();

        /**
         * The Class CourseGpa.
         */
        private class CourseGpa {
            private double m_gpa;
            private String m_number;

            /**
             * Instantiates a new course gpa.
             *
             * @param number String
             * @param grade String
             */
            public CourseGpa(String number, String grade) {
                this.m_number = number;
                this.m_gpa = gradeToGpa(grade);
            }

            /**
             * Grade to gpa.
             *
             * @param grade String
             * @return double
             */
            public double gradeToGpa(String grade) {
                if ("A".equals(grade)) {
                    return 4.0;
                } else if ("B".equals(grade)) {
                    return 3.0;
                } else if ("C".equals(grade)) {
                    return 2.0;
                } else if ("D".equals(grade)) {
                    return 1.0;
                }
                return 0;
            }
        }

        /**
         * Instantiates a new retrieve student TSA.
         *
         * @param stdSubQuery SubQuery
         */
        public RetrieveStudentTSA(SubQuery stdSubQuery) {
            loadCourseGrades(stdSubQuery);
        }

        /**
         * Adds the course grade.
         *
         * @param stdOid String
         * @param courseNumber String
         * @param finalGrade String
         */
        private void addCourseGrade(String stdOid, String courseNumber, String finalGrade) {
            List<CourseGpa> gpas = m_stdCourseGPAs.get(stdOid);
            if (gpas == null) {
                gpas = new ArrayList<>();
                m_stdCourseGPAs.put(stdOid, gpas);
            }
            CourseGpa courseGpa =
                    gpas.stream().filter(gpa -> courseNumber.equals(gpa.m_number)).findFirst().orElse(null);
            if (courseGpa == null) {
                gpas.add(new CourseGpa(courseNumber, finalGrade));
            } else {
                double gpa = courseGpa.gradeToGpa(finalGrade);
                if (gpa > courseGpa.m_gpa) {
                    courseGpa.m_gpa = gpa;
                }
            }
        }

        /**
         * Gets the std average gpa.
         *
         * @param stdOid String
         * @return Double
         */
        private Double getStdAverageGpa(String stdOid) {
            List<CourseGpa> gpas = m_stdCourseGPAs.get(stdOid);
            if (gpas != null && gpas.size() == COURSE_NUMBERS.size()) {
                // we've grades for all the courses
                return gpas.stream().mapToDouble(it -> it.m_gpa).average().getAsDouble();
            }
            return null;
        }

        /**
         * Calculate TSA result.
         *
         * @param std SisStudent
         * @return String
         */
        private String calculateTSAResult(SisStudent std) {
            Double averageGpa = getStdAverageGpa(std.getOid());
            if (averageGpa == null) {
                return null;
            }
            return averageGpa >= 3.0 ? "T" : "A";
        }

        /**
         * Load course grades.
         *
         * @param stdSubQuery SubQuery
         */
        private void loadCourseGrades(SubQuery stdSubQuery) {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(Transcript.COL_STUDENT_OID, stdSubQuery);
            criteria.addIn(Transcript.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                    Course.COL_NUMBER, COURSE_NUMBERS);

            String[] columns = new String[] {Transcript.COL_STUDENT_OID,
                    Transcript.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                            MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                            SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + Course.COL_NUMBER,
                    Transcript.COL_FINAL_GRADE
            };
            ColumnQuery query = new ColumnQuery(Transcript.class, columns, criteria);
            try (QueryIterator<?> iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    addCourseGrade((String) row[0], (String) row[1], (String) row[2]);
                }
            }
        }


        /**
         * Gets the business rule value.
         *
         * @param hsData HighSchoolDataCollection
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @see com.x2dev.procedures.statereporting.md.HighSchoolDataCollection.RetrieveStudentAlias#getBusinessRuleValue(com.x2dev.procedures.statereporting.md.HighSchoolDataCollection,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        protected String getBusinessRuleValue(HighSchoolDataCollection hsData,
                                              StateReportEntity entity,
                                              FieldDefinition field) {
            HighSchoolDataCollectionEntity hsdcEntity = (HighSchoolDataCollectionEntity) entity;
            SisStudent student = (SisStudent) hsdcEntity.getBean();
            TSAData tsaData = hsdcEntity.m_tsaData;
            if (!tsaData.m_hasCode && !tsaData.m_calculated) {
                String result = calculateTSAResult(student);
                tsaData.setCalculationResult(result);
            }
            String alias = (String) field.getParameter();
            return tsaData.getValueByAlias(alias);
        }
    }

    protected class RetrieveTranscriptsValues implements FieldRetriever {
        private static final String CALC_ID = "TRN-VALUES";
        private static final String CALC_PARAM_GRADE_FAIL = "GRADE_FAIL";
        private static final String CALC_PARAM_GRADE_GREDITS = "GRADE_GREDITS";
        private static final String CALC_PARAM_GRADE_TRACKER = "GRADE_TRACKER";
        private final DecimalFormat DECIMAL_FORMAT_CREDITS = new DecimalFormat("00.00");
        private final BigDecimal TRN_SUM_CREDITS_TRESHOLD = BigDecimal.valueOf(6.0d);

        private Map<String, Collection<Transcript>> m_qualifyingTranscripts;

        /**
         * Instantiates a new retrieve student by transcript.
         *
         * @param studentSubQuery SubQuery
         * @param qualifyingCriteria List<KeyValueTrio>
         * @param groupFields List<String>
         * @param distinctFields List<String>
         * @param count int
         */
        public RetrieveTranscriptsValues(SubQuery studentSubQuery) {
            super();
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);
            criteria.addEqualTo(Transcript.COL_GRADE_LEVEL, "09");
            criteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
            m_qualifyingTranscripts =
                    getBroker().getGroupedCollectionByQuery(new QueryByCriteria(Transcript.class, criteria),
                            Transcript.COL_STUDENT_OID, 1024);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String returnValue = null;
            SisStudent std = (SisStudent) entity.getBean();
            String param = (String) field.getParameter();
            String beanPath = field.getBeanPath();
            Collection<Transcript> transcripts = m_qualifyingTranscripts.get(std.getOid());
            if (CALC_PARAM_GRADE_TRACKER.equals(param)) {
                if (isUpdateStudent()) {
                    if (transcripts != null && !transcripts.isEmpty()) {
                        BigDecimal sumCredits = BigDecimal.ZERO;
                        if (transcripts != null && !transcripts.isEmpty()) {
                            for (Transcript trn : transcripts) {
                                if (trn.getTotalCredit() != null) {
                                    sumCredits = sumCredits.add(trn.getTotalCredit());
                                }
                            }
                        }
                        if (sumCredits.compareTo(TRN_SUM_CREDITS_TRESHOLD) >= 0) {
                            returnValue = "Y";
                        } else {
                            returnValue = "N";
                        }
                    } else {
                        returnValue = "N/A";
                    }
                    std.setFieldValueByBeanPath(beanPath, returnValue);
                    getBroker().saveBeanForced(std);
                } else {
                    String value = (String) std.getFieldValueByBeanPath(beanPath);
                    if (!StringUtils.isEmpty(value)) {
                        returnValue = lookupStateValue(SisStudent.class, beanPath, value);
                    }
                }
            } else if (CALC_PARAM_GRADE_FAIL.equals(param)) {
                if (isUpdateStudent()) {
                    if (transcripts != null && !transcripts.isEmpty()) {
                        int sumTranscripts = 0;
                        if (transcripts != null && !transcripts.isEmpty()) {
                            for (Transcript trn : transcripts) {
                                if ("F".equals(trn.getFinalGrade())
                                        && Arrays.asList("Math", "Science", "Social Studies", "English")
                                                .contains(trn.getSchoolCourse().getCourse().getDepartmentCode())) {
                                    sumTranscripts += 1;
                                }
                            }
                        }
                        returnValue = String.valueOf(sumTranscripts);
                    }
                    std.setFieldValueByBeanPath(beanPath, returnValue);
                    getBroker().saveBeanForced(std);
                } else {
                    returnValue = (String) std.getFieldValueByBeanPath(beanPath);
                }
            } else if (CALC_PARAM_GRADE_GREDITS.equals(param)) {
                if (isUpdateStudent()) {
                    if (transcripts != null && !transcripts.isEmpty()) {
                        BigDecimal sumCredits = BigDecimal.ZERO;
                        if (transcripts != null && !transcripts.isEmpty()) {
                            for (Transcript trn : transcripts) {
                                if (trn.getTotalCredit() != null) {
                                    sumCredits = sumCredits.add(trn.getTotalCredit());
                                }
                            }
                        }
                        returnValue = DECIMAL_FORMAT_CREDITS.format(sumCredits);
                        if (returnValue.startsWith("0")) {
                            returnValue = returnValue.substring(1);
                        }
                    } else {
                        returnValue = "";
                    }
                    std.setFieldValueByBeanPath(beanPath, returnValue);
                    getBroker().saveBeanForced(std);
                } else {
                    returnValue = (String) std.getFieldValueByBeanPath(beanPath);
                }
            }
            return returnValue;
        }
    }

    static final private String PARCC_ALIAS_SCHOOL = "PARCC_SCHOOL";

    private Set<String> m_graduateList;
    private Set<String> m_membershipList;
    private DistrictSchoolYearContext m_reportContext;
    private boolean m_updateStudentIndicator;

    /**
     * Gets the current context.
     *
     * @return District school year context
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getCurrentContext()
     */
    @Override
    public DistrictSchoolYearContext getCurrentContext() {
        return getReportContext();
    }

    /**
     * Gets the assessment definition.
     *
     * @param asdId String
     * @return Assessment definition
     */
    protected AssessmentDefinition getAssessmentDefinition(String asdId) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(AssessmentDefinition.COL_ID, asdId);
        BeanQuery query = new BeanQuery(AssessmentDefinition.class, criteria);
        return (AssessmentDefinition) getBroker().getBeanByQuery(query);
    }

    /**
     * Gets the graduate list.
     *
     * @return Sets the
     */
    protected Set<String> getGraduateList() {
        if (m_graduateList == null) {
            m_graduateList = new HashSet<String>(500);
            String[] columns = new String[] {StudentEnrollment.COL_STUDENT_OID};
            ColumnQuery query = new ColumnQuery(StudentEnrollment.class, columns, getGraduateCriteria());
            try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    m_graduateList.add((String) row[0]);
                }
            }
        }
        return m_graduateList;
    }

    /**
     * Gets the membership list.
     *
     * @return Sets the
     */
    protected Set<String> getMembershipList() {
        if (m_membershipList == null) {
            boolean entryIsMemberDay =
                    Boolean.valueOf(PreferenceManager.getPreferenceSet(getOrganization()).getPreferenceValue(
                            SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();

            boolean withdrawalIsMemberDay =
                    Boolean.valueOf(PreferenceManager.getPreferenceSet(getOrganization()).getPreferenceValue(
                            SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

            // Process enrollment records with one query
            List typesList = new ArrayList(3);
            typesList.add(StudentEnrollment.ENTRY);
            typesList.add(StudentEnrollment.WITHDRAWAL);
            X2Criteria enrollmentCriteria = new X2Criteria();
            addYogCriteria(enrollmentCriteria, StudentEnrollment.REL_STUDENT + ModelProperty.PATH_DELIMITER);
            // Limit to high schools
            enrollmentCriteria.addIn(
                    StudentEnrollment.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_SCHOOL_LEVEL_CODE,
                    refCodesForStateCode(SisSchool.class, SisSchool.COL_SCHOOL_LEVEL_CODE, "H"));
            enrollmentCriteria.addNotNull(StudentEnrollment.COL_STUDENT_OID);
            enrollmentCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, typesList);

            QueryByCriteria enrollmentQuery =
                    new QueryByCriteria(StudentEnrollment.class, enrollmentCriteria);
            enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
            enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
            enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);

            m_membershipList = new HashSet<String>(500);
            Set complete = new HashSet(500);
            Set recordsEvaluated = new HashSet(500);
            QueryIterator enrollmentIterator = null;
            try {
                enrollmentIterator = getBroker().getIteratorByQuery(enrollmentQuery);
                while (enrollmentIterator.hasNext()) {
                    StudentEnrollment enrollment = (StudentEnrollment) enrollmentIterator.next();
                    String studentOid = enrollment.getStudentOid();

                    if (studentOid != null &&
                            enrollment.getEnrollmentDate() != null &&
                            !complete.contains(studentOid)) {
                        // Determine how to check if this record indicates an active status:
                        // checkStatus == false -> ENTRY means active, all else is not.
                        // checkStatus == true -> ENTRY or STATUS_CHANGE must have "Active" or empty
                        // in
                        // status code.
                        boolean enrollmentIsActiveStatus = true;
                        enrollmentIsActiveStatus = StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType());

                        if (m_reportDate.after(enrollment.getEnrollmentDate())) {
                            if (enrollmentIsActiveStatus) {
                                m_membershipList.add(enrollment.getStudentOid());
                            } else {
                                m_membershipList.remove(enrollment.getStudentOid());
                            }
                        } else if (m_reportDate.before(enrollment.getEnrollmentDate())) {
                            // Only consider records after the as-of date if no records existed
                            // before
                            // it
                            if (!recordsEvaluated.contains(studentOid)) {
                                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                                    m_membershipList.remove(enrollment.getStudentOid());
                                } else {
                                    m_membershipList.add(enrollment.getStudentOid());
                                }
                            }

                            complete.add(studentOid);
                        } else // Enrollment date occurs on asOf date
                        {
                            if (enrollmentIsActiveStatus) {
                                if (entryIsMemberDay) {
                                    m_membershipList.add(enrollment.getStudentOid());
                                } else {
                                    if (!recordsEvaluated.contains(studentOid)) {
                                        m_membershipList.remove(enrollment.getStudentOid());
                                    }
                                }
                            } else {
                                if (withdrawalIsMemberDay) {
                                    if (!recordsEvaluated.contains(studentOid)) {
                                        m_membershipList.add(enrollment.getStudentOid());
                                    }
                                } else {
                                    m_membershipList.remove(enrollment.getStudentOid());
                                }
                            }

                            complete.add(studentOid);
                        }
                        recordsEvaluated.add(studentOid);
                    }
                }
            } finally {
                if (enrollmentIterator != null) {
                    enrollmentIterator.close();
                }
            }

            complete.clear();
            complete = null;
            recordsEvaluated.clear();
            recordsEvaluated = null;
        }
        return m_membershipList;
    }

    /**
     * Gets the property as boolean.
     *
     * @param bean X2BaseBean
     * @param beanPath String
     * @return boolean
     * @throws X2BaseException exception
     */
    boolean getPropertyAsBoolean(X2BaseBean bean, String beanPath) throws X2BaseException {
        boolean value = false;
        Object property = getPropertyAsJavaType(bean, beanPath);
        if (property != null && property instanceof Boolean) {
            value = ((Boolean) property).booleanValue();
        } else if (property != null && property instanceof String) {
            value = "Y".equals(property);
        }
        return value;

    }

    /**
     * Gets the reporting criteria.
     *
     * @return Criteria
     * @see com.x2dev.procedures.statereporting.md.MDStudentReportData#getReportingCriteria()
     */
    @Override
    protected Criteria getReportingCriteria() {
        // select all students with correct YOG and filter in entity
        int seniorYOG = getReportContext().getSchoolYear();
        int ninthYOG = seniorYOG + 3;
        X2Criteria criteria = new X2Criteria();
        addYogCriteria(criteria, "");
        criteria.addGreaterOrEqualThan(SisStudent.COL_YOG, Integer.valueOf(seniorYOG));
        criteria.addLessOrEqualThan(SisStudent.COL_YOG, Integer.valueOf(ninthYOG));

        X2Criteria graduatesCriteria = new X2Criteria();
        graduatesCriteria.addIn(X2BaseBean.COL_OID,
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, getGraduateCriteria()));
        criteria.addOrCriteria(graduatesCriteria);
        return criteria;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.md.MDStudentReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        Object parameter = getParameter(INPUT_PARAM_CLEAR_STUDENT);

        parameter = getParameter(INPUT_PARAM_UPDATE_STUDENT);
        m_updateStudentIndicator =
                parameter != null && parameter instanceof Boolean && ((Boolean) parameter).booleanValue() ? true
                        : false;

        if (getSetupErrors().size() == 0) {
            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("ALIAS-ONLY", new RetrieveStudentByAlias());

            calcs.put("ALIAS-ONLY-YN", new RetrieveStudentByAliasYNValues());

            SubQuery subQueryStudentOid = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getStudentCriteria());
            calcs.put("GRADE", new RetrieveStudentGrade(subQueryStudentOid));

            BridgeHelper bridgeHelper = new BridgeHelper();
            calcs.put("MHSA", new RetrieveStudentMHSA(subQueryStudentOid));

            bridgeHelper.initAssessmentDefinition(BRIDGE_ASD_ID_MATH, subQueryStudentOid,
                    Arrays.asList(new KeyValueTrio[] {
                            new KeyValueTrio(
                                    StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                            AssessmentDefinition.COL_ID,
                                    BRIDGE_ASD_ID_MATH, ComparisonType.EQUALS),
                            new KeyValueTrio(StudentAssessment.COL_PERFORMANCE_LEVEL, "P",
                                    ComparisonType.EQUALS)}));
            calcs.put("BRIDGE-MATH",
                    new RetrieveStudentBridgeAssessment(bridgeHelper, BRIDGE_ASD_ID_MATH, FIELD_MHSA_MATH));

            bridgeHelper.initAssessmentDefinition(BRIDGE_ASD_ID_ELA, subQueryStudentOid,
                    Arrays.asList(new KeyValueTrio[] {
                            new KeyValueTrio(
                                    StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                            AssessmentDefinition.COL_ID,
                                    BRIDGE_ASD_ID_ELA, ComparisonType.EQUALS),
                            new KeyValueTrio(StudentAssessment.COL_PERFORMANCE_LEVEL, "P",
                                    ComparisonType.EQUALS)}));
            calcs.put("BRIDGE-ELA",
                    new RetrieveStudentBridgeAssessment(bridgeHelper, BRIDGE_ASD_ID_ELA, FIELD_MHSA_ELA));

            bridgeHelper.initAssessmentDefinition(BRIDGE_ASD_ID_SCIENCE, subQueryStudentOid,
                    Arrays.asList(new KeyValueTrio[] {
                            new KeyValueTrio(
                                    StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                            AssessmentDefinition.COL_ID,
                                    BRIDGE_ASD_ID_SCIENCE, ComparisonType.EQUALS),
                            new KeyValueTrio(StudentAssessment.COL_PERFORMANCE_LEVEL, "P",
                                    ComparisonType.EQUALS)}));
            calcs.put("BRIDGE-SCIENCE",
                    new RetrieveStudentBridgeAssessment(bridgeHelper, BRIDGE_ASD_ID_SCIENCE, FIELD_MHSA_SCIENCE));

            bridgeHelper.initAssessmentDefinition(BRIDGE_ASD_ID_GOVERNMENT, subQueryStudentOid,
                    Arrays.asList(new KeyValueTrio[] {
                            new KeyValueTrio(
                                    StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                            AssessmentDefinition.COL_ID,
                                    BRIDGE_ASD_ID_GOVERNMENT, ComparisonType.EQUALS),
                            new KeyValueTrio(StudentAssessment.COL_PERFORMANCE_LEVEL, "P",
                                    ComparisonType.EQUALS)}));
            calcs.put("BRIDGE-GOVERNMENT",
                    new RetrieveStudentBridgeAssessment(bridgeHelper, BRIDGE_ASD_ID_GOVERNMENT, FIELD_MHSA_GOVERNMENT));

            GraduateHelper helper = new GraduateHelper(subQueryStudentOid);
            calcs.put("LOCAL-GRAD-REQ", new RetrieveStudentMetGradRequirement(helper));
            calcs.put("HS-COMPLETION", new RetrieveStudentHSCompletionStatus(helper));

            String cteConcentratorField = translateAliasToJavaName(ALIAS_CTE_CONCENTRATOR, true);
            if (!StringUtils.isEmpty(cteConcentratorField)) {
                calcs.put("CTE-CONCENTRATOR", new RetrieveStudentByTranscriptExists(subQueryStudentOid,
                        Arrays.asList(new KeyValueTrio[] {
                                new KeyValueTrio(SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                                        cteConcentratorField,
                                        Arrays.asList(new String[] {"Yes", "Y"}),
                                        ComparisonType.IN_LIST)})));
            }
            calcs.put("CTE-CIP-CODE", new RetrieveStudentCIPCode(subQueryStudentOid, getReportContext()));

            String departmentField = translateAliasToJavaName(ALIAS_CRS_DEPARTMENT, true);
            String rigorousIndicatorField = translateAliasToJavaName(ALIAS_CRS_RIGOROUS_INDICATOR, true);
            String advTechEdField = translateAliasToJavaName(ALIAS_CRS_ADV_TECH_ED, true);
            String worldLanguageField = translateAliasToJavaName(ALIAS_CRS_WORLD_LANGUAGE, true);
            if (!StringUtils.isEmpty(departmentField) && !StringUtils.isEmpty(rigorousIndicatorField)
                    && !StringUtils.isEmpty(worldLanguageField)) {
                calcs.put("RIGOR-FL", new RetrieveStudentByTranscript(subQueryStudentOid,
                        Arrays.asList(new KeyValueTrio[] {
                                new KeyValueTrio(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + departmentField,
                                        refCodesForStateCode(Course.class, departmentField, "World Language"),
                                        ComparisonType.IN_LIST),
                                new KeyValueTrio(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER
                                        + Course.COL_SCHOOL_LEVEL,
                                        refCodesForStateCode(Course.class, departmentField, "High"),
                                        ComparisonType.IN_LIST),
                                new KeyValueTrio(Transcript.COL_FINAL_GRADE,
                                        Arrays.asList(new String[] {"A", "B"}), ComparisonType.IN_LIST)}),
                        Arrays.asList(new String[] {worldLanguageField}),
                        Arrays.asList(new String[] {Course.COL_NUMBER}),
                        2));
                calcs.put("RIGOR-MATH", new RetrieveStudentByTranscript(subQueryStudentOid,
                        Arrays.asList(new KeyValueTrio[] {
                                new KeyValueTrio(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + departmentField,
                                        refCodesForStateCode(Course.class, departmentField, "Math"),
                                        ComparisonType.IN_LIST),
                                new KeyValueTrio(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + rigorousIndicatorField,
                                        BooleanAsStringConverter.TRUE, ComparisonType.EQUALS),
                                new KeyValueTrio(Transcript.COL_FINAL_GRADE,
                                        Arrays.asList(new String[] {"A", "B"}), ComparisonType.IN_LIST)}),
                        null,
                        Arrays.asList(new String[] {Course.COL_NUMBER}),
                        1));
                calcs.put("RIGOR-SCIENCE", new RetrieveStudentByTranscript(subQueryStudentOid,
                        Arrays.asList(new KeyValueTrio[] {
                                new KeyValueTrio(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + departmentField,
                                        refCodesForStateCode(Course.class, departmentField, "Science"),
                                        ComparisonType.IN_LIST),
                                new KeyValueTrio(Transcript.COL_FINAL_GRADE,
                                        Arrays.asList(new String[] {"A", "B"}), ComparisonType.IN_LIST)}),
                        null,
                        Arrays.asList(new String[] {Course.COL_NUMBER}),
                        2));
                calcs.put("RIGOR-ADV-TECH", new RetrieveStudentByTranscript(subQueryStudentOid,
                        Arrays.asList(new KeyValueTrio[] {
                                new KeyValueTrio(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + advTechEdField,
                                        BooleanAsStringConverter.TRUE, ComparisonType.EQUALS),
                                new KeyValueTrio(Transcript.COL_FINAL_GRADE,
                                        Arrays.asList(new String[] {"A", "B"}), ComparisonType.IN_LIST)}),
                        null,
                        Arrays.asList(
                                new String[] {Course.COL_NUMBER}),
                        2));
            }
            calcs.put("RIGOR-SAT", new RetrieveStudentByAssessment(subQueryStudentOid,
                    Arrays.asList(new KeyValueTrio[] {
                            new KeyValueTrio(
                                    StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                            AssessmentDefinition.COL_ID,
                                    "SAT", ComparisonType.EQUALS),
                            new KeyValueTrio(StudentAssessment.COL_RAW_SCORE, BigDecimal.valueOf(1000.0),
                                    ComparisonType.GREATER_THAN_OR_EQUALS)}),
                    null, "Y", "N"));
            calcs.put("RIGOR-ACT", new RetrieveStudentByAssessment(subQueryStudentOid,
                    Arrays.asList(new KeyValueTrio[] {
                            new KeyValueTrio(
                                    StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                            AssessmentDefinition.COL_ID,
                                    "ACT", ComparisonType.EQUALS),
                            new KeyValueTrio(StudentAssessment.COL_RAW_SCORE, BigDecimal.valueOf(20.0),
                                    ComparisonType.GREATER_THAN_OR_EQUALS)}),
                    null, "Y", "N"));
            calcs.put("RIGOR-GPA", new RetrieveStudentByGPA(subQueryStudentOid));

            calcs.put("PS-AP", new RetrieveStudentByAssessment(subQueryStudentOid,
                    Arrays.asList(new KeyValueTrio[] {
                            new KeyValueTrio(
                                    StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                            AssessmentDefinition.COL_ID,
                                    "AP %", ComparisonType.LIKE),
                            new KeyValueTrio(StudentAssessment.COL_RAW_SCORE, BigDecimal.valueOf(3.0),
                                    ComparisonType.GREATER_THAN_OR_EQUALS)}),
                    null,
                    "Y", "N"));
            calcs.put("PS-IB", new RetrieveStudentByAssessment(subQueryStudentOid,
                    Arrays.asList(new KeyValueTrio[] {
                            new KeyValueTrio(
                                    StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                            AssessmentDefinition.COL_ID,
                                    "IB %", ComparisonType.LIKE),
                            new KeyValueTrio(StudentAssessment.COL_RAW_SCORE, BigDecimal.valueOf(4.0),
                                    ComparisonType.GREATER_THAN_OR_EQUALS)}),
                    null,
                    "Y", "N"));
            calcs.put("PS-SAT", new RetrieveStudentByAssessment(subQueryStudentOid,
                    Arrays.asList(new KeyValueTrio[] {
                            new KeyValueTrio(
                                    StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                            AssessmentDefinition.COL_ID,
                                    "SAT", ComparisonType.EQUALS),}),
                    new AssessmentQualifierPSSAT(StudentAssessment.COL_SCORE05, StudentAssessment.COL_FIELD_A039),
                    "Y", "N"));
            calcs.put("PS-ACT", new RetrieveStudentByAssessment(subQueryStudentOid,
                    Arrays.asList(new KeyValueTrio[] {
                            new KeyValueTrio(
                                    StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                            AssessmentDefinition.COL_ID,
                                    "ACT", ComparisonType.EQUALS),
                            new KeyValueTrio(StudentAssessment.COL_RAW_SCORE, BigDecimal.valueOf(21.0),
                                    ComparisonType.GREATER_THAN_OR_EQUALS)}),
                    null, "Y", "N"));

            String dualEnrollmentField = translateAliasToJavaName(ALIAS_DUAL_ENROLLMENT, true);
            if (!StringUtils.isEmpty(dualEnrollmentField)) {
                calcs.put("DUAL-PART", new RetrieveStudentByTranscript(subQueryStudentOid,
                        Arrays.asList(new KeyValueTrio[] {
                                new KeyValueTrio(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + dualEnrollmentField,
                                        BooleanAsStringConverter.TRUE, ComparisonType.EQUALS)}),
                        null,
                        Arrays.asList(new String[] {Course.COL_NUMBER}),
                        1));
                calcs.put("DUAL-EARNED", new RetrieveStudentByTranscript(subQueryStudentOid,
                        Arrays.asList(new KeyValueTrio[] {
                                new KeyValueTrio(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + dualEnrollmentField,
                                        BooleanAsStringConverter.TRUE, ComparisonType.EQUALS),
                                new KeyValueTrio(Transcript.COL_TOTAL_CREDIT,
                                        BigDecimal.ZERO, ComparisonType.GREATER_THAN)}),
                        null,
                        Arrays.asList(new String[] {Course.COL_NUMBER}),
                        1));
            }
            String ccrMathBeanPath = translateAliasToJavaName("all-std-CCRMath", false);
            String ccrELABeanPath = translateAliasToJavaName("all-std-CCRELA", false);
            if (!StringUtils.isEmpty(ccrMathBeanPath) && !StringUtils.isEmpty(ccrELABeanPath)) {
                calcs.put("CCR-11-MATH",
                        new RetrieveStudentCCR(getReportContext().getSchoolYear() + 1, ccrMathBeanPath));
                calcs.put("CCR-11-ELA", new RetrieveStudentCCR(getReportContext().getSchoolYear() + 1, ccrELABeanPath));
                calcs.put("CCR-12-MATH",
                        new RetrieveStudentCCR(getReportContext().getSchoolYear(), ccrMathBeanPath));
                calcs.put("CCR-12-ELA", new RetrieveStudentCCR(getReportContext().getSchoolYear(), ccrELABeanPath));
            }

            String transitionCourseField = translateAliasToJavaName(ALIAS_CRS_TRANSITION, true);
            String departmentCourseField = translateAliasToJavaName(ALIAS_CRS_DEPARTMENT, true);
            if (!StringUtils.isEmpty(transitionCourseField) && !StringUtils.isEmpty(departmentCourseField)) {
                calcs.put("TRAN-MATH", new RetrieveStudentBySchedule(subQueryStudentOid,
                        Arrays.asList(new KeyValueTrio[] {
                                new KeyValueTrio(MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                                        Schedule.COL_DISTRICT_CONTEXT_OID,
                                        getReportContext().getOid(),
                                        ComparisonType.EQUALS),
                                new KeyValueTrio(MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                                        transitionCourseField,
                                        refCodesForStateCode(Course.class, transitionCourseField, "TRAN"),
                                        ComparisonType.IN_LIST),
                                new KeyValueTrio(MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                                        departmentCourseField,
                                        refCodesForStateCode(Course.class, departmentCourseField, "Math"),
                                        ComparisonType.IN_LIST)})));
                calcs.put("TRAN-ELA", new RetrieveStudentBySchedule(subQueryStudentOid,
                        Arrays.asList(new KeyValueTrio[] {
                                new KeyValueTrio(MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                                        Schedule.COL_DISTRICT_CONTEXT_OID,
                                        getReportContext().getOid(),
                                        ComparisonType.EQUALS),
                                new KeyValueTrio(MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                                        transitionCourseField,
                                        refCodesForStateCode(Course.class, transitionCourseField, "TRAN"),
                                        ComparisonType.IN_LIST),
                                new KeyValueTrio(MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                                        departmentCourseField,
                                        refCodesForStateCode(Course.class, departmentCourseField, "English"),
                                        ComparisonType.IN_LIST)})));
            }

            calcs.put(CALC_ID_TSA_RESULT, new RetrieveStudentTSA(subQueryStudentOid));
            calcs.put(RetrieveCCRValues.CALC_ID, new RetrieveCCRValues(subQueryStudentOid));
            calcs.put(RetrieveTranscriptsValues.CALC_ID, new RetrieveTranscriptsValues(subQueryStudentOid));
            calcs.put(RetrieveBiliteracyValues.CALC_ID, new RetrieveBiliteracyValues(subQueryStudentOid));

            super.addCalcs(calcs);
        }
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(getStudentQuery());
            setEntityClass(HighSchoolDataCollectionEntity.class);
        }

    }


    /**
     * Checks if is update student.
     *
     * @return true, if is update student
     */
    protected boolean isUpdateStudent() {
        return m_updateStudentIndicator;
    }

    /**
     * Ref codes for state code.
     *
     * @param beanClass Class
     * @param beanPath String
     * @param value String
     * @return Set
     */
    protected Set<String> refCodesForStateCode(Class beanClass, String beanPath, String value) {
        Set<String> codes = new HashSet();
        DataDictionaryField dictionaryField = getDataDictionaryField(beanClass, beanPath);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            X2Criteria rcdCriteria = new X2Criteria();
            rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, dictionaryField.getReferenceTableOid());
            rcdCriteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
            rcdCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, value);
            String[] columns = new String[] {ReferenceCode.COL_CODE};
            ColumnQuery rcdQuery = new ColumnQuery(ReferenceCode.class, columns, rcdCriteria);
            try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(rcdQuery)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    codes.add((String) row[0]);
                }
            }
        }
        if (codes.isEmpty()) {
            codes.add(value);
        }
        return codes;
    }


    /**
     * Translate alias.
     *
     * @param asd AssessmentDefinition
     * @param alias String
     * @return String
     */
    protected String translateAlias(AssessmentDefinition asd, String alias) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(asd, asd.getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field == null) {
            addSetupError("Assessment Definition " + asd.getId() + " alias not found", alias);
        }
        return field != null ? field.getJavaName() : null;
    }

    /**
     * Translate alias.
     *
     * @param asdId String
     * @param alias String
     * @return String
     */
    protected String translateAlias(String asdId, String alias) {
        AssessmentDefinition asd = getAssessmentDefinition(asdId);
        return asd == null ? null : translateAlias(asd, alias);
    }

    /**
     * Adds the yog criteria.
     *
     * @param criteria X2Criteria
     * @param prefix String
     */
    private void addYogCriteria(X2Criteria criteria, String prefix) {
        int seniorYOG = getReportContext().getSchoolYear();
        int ninthYOG = seniorYOG + 3;
        criteria.addGreaterOrEqualThan(prefix + SisStudent.COL_YOG, Integer.valueOf(seniorYOG));
        criteria.addLessOrEqualThan(prefix + SisStudent.COL_YOG, Integer.valueOf(ninthYOG));
    }

    /**
     * Gets the graduate criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getGraduateCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentEnrollment.COL_STATUS_CODE, "Graduate");
        criteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, getReportContext().getStartDate());
        criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, getReportContext().getEndDate());
        return criteria;
    }

    /**
     * Gets the report context.
     *
     * @return District school year context
     */
    private DistrictSchoolYearContext getReportContext() {
        if (m_reportContext == null) {
            Object parameter = getParameter(INPUT_PARAM_CONTEXT_OID);
            if (parameter != null && parameter instanceof String) {
                m_reportContext = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                        (String) parameter);
            }
            if (m_reportContext == null) {
                DistrictSchoolYearContext context = super.getCurrentContext();
                if (m_reportDate.before(context.getStartDate()) || m_reportDate.after(context.getEndDate())) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_reportDate);
                    criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_reportDate);
                    DistrictSchoolYearContext bean = (DistrictSchoolYearContext) getBroker()
                            .getBeanByQuery(new BeanQuery(DistrictSchoolYearContext.class, criteria));
                    if (bean != null) {
                        setCurrentContext(bean);
                        context = bean;
                    }
                }
                m_reportContext = context;
            }
        }
        return m_reportContext;
    }

}
