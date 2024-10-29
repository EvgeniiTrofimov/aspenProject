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

package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.*;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Procedure that looks at the student schedule and auto creates a PARCC Assessment
 * for those courses that are selected in the procedure input,
 * for the cycle indicated and align to the test code selected in the procedure input.
 *
 * @author Follett Software Company
 *
 */
public class MDFallAsmCreation extends ProcedureJavaSource {
    private static final String ALIAS_CRS_ALGEBRA_1 = "all-crs-Algebra1";
    private static final String ALIAS_CRS_ALGEBRA_2 = "all-crs-Algebra2";
    private static final String ALIAS_CRS_ELA_10 = "all-crs-English10";
    private static final String ALIAS_CRS_ELA_11 = "all-crs-English11";
    private static final String ALIAS_PARCC_RETEST = "PARCC_RETEST";
    private static final String ALIAS_PARCC_TESTCODE = "PARCCTSTCODE";
    private static final String ALIAS_PARCC_TESTDATE = "PARCCTSTDATE";
    private static final String ALIAS_PARCC_TESTFORMAT = "PARCCTSTFORMAT";
    private static final String ALIAS_PARCC_TESTYEAR = "PARCCTSTYEAR";
    private static final String ALIAS_PARCC_TESTPERIOD = "PARCCTSTPERIOD";
    private static final String ALIAS_STD_CCR_ELA = "all-std-CCRELA";

    private static final Collection EMPTY_LIST = new ArrayList();

    private static final List GRADES_LIST_9_12 = Arrays.asList("09", "10", "11", "12");
    private static final List GRADES_LIST_11_12 = Arrays.asList("11", "12");

    private static final String INPUT_PARAM_ASD_OID = "asdOid";
    private static final String INPUT_PARAM_ASM_DATE = "asmDate";
    private static final String INPUT_PARAM_ASM_DEF_ID = "asmDefinitionId";
    private static final String INPUT_PARAM_CTX = "schoolYearContext";
    private static final String INPUT_PARAM_CTX_BEAN_PATH = "ctxBeanPath";
    private static final String INPUT_PARAM_GRADES = "gradeList";
    private static final String INPUT_PARAM_HISTORICAL_ASD_OID = "asdHistoricalOid";
    private static final String INPUT_PARAM_SCHOOLS = "schoolOids";
    private static final String INPUT_PARAM_TEST_FORMAT = "testFormat";

    private static final String TEST_CODE_ALG01 = "ALG01";
    private static final String TEST_CODE_ALG02 = "ALG02";
    private static final String TEST_CODE_ELA10 = "ELA10";
    private static final String TEST_CODE_ELA11 = "ELA11";
    private static final List<String> TEST_CODES =
            Arrays.asList(TEST_CODE_ALG01, TEST_CODE_ALG02, TEST_CODE_ELA10, TEST_CODE_ELA11);

    private static final String TEST_PERIOD_FALL = "Fall";

    /**
     * The Class RuleSet.
     */
    private class RuleSet {
        private SisStudent m_student;
        private Collection<StudentAssessmentInfo> m_assessmentsALG01;
        private Collection<StudentAssessmentInfo> m_assessmentsALG02;
        private Collection<StudentAssessmentInfo> m_assessmentsELA10;

        /**
         * Instantiates a new rule set.
         *
         * @param student SisStudent
         */
        public RuleSet(SisStudent student) {
            m_student = student;
        }

        /**
         * Requires ALG 01.
         *
         * @return true, if successful
         */
        public boolean requiresALG01() {
            boolean value = false;
            if (GRADES_LIST_9_12.contains(m_student.getGradeLevel())) {
                Collection<StudentAssessmentInfo> assessments = getAssessmentsALG01();
                if (assessments != null && !assessments.isEmpty()) {
                    // skip if already two assessments
                    if (assessments.size() == 1 && !before2016(assessments.iterator().next())) {
                        BigDecimal score = bestScore(assessments);
                        if (BigDecimal.valueOf(725.0).compareTo(score) > 0) {
                            assessments = getAssessmentsELA10();
                            if (assessments != null && !assessments.isEmpty()) {
                                score = score.add(bestScore(assessments));
                            }
                            if (BigDecimal.valueOf(1450.0).compareTo(score) > 0) {
                                value = true;
                            }
                        }
                    }
                } else {
                    if (completedAlgebra1()) {
                        value = true;
                    }
                }
            }
            return value;
        }

        /**
         * Requires ALG 02.
         *
         * @return true, if successful
         */
        public boolean requiresALG02() {
            boolean value = true;
            Collection<StudentAssessmentInfo> assessmentsALG01 =
                    getStudentAssessmentsWithScore(m_student.getOid(), TEST_CODE_ALG01);
            if (assessmentsALG01 != null && !assessmentsALG01.isEmpty()) {
                for (StudentAssessmentInfo asmInfo : assessmentsALG01) {
                    if (GRADES_LIST_9_12.contains(asmInfo.getGradeLevelCode())
                            && asmInfo.getScaleScore().compareTo(BigDecimal.ZERO) > 0) {
                        value = false;
                        break;
                    }
                }
            }
            if (value) {
                Collection<StudentAssessmentInfo> assessmentsALG02 =
                        getStudentAssessmentsWithScore(m_student.getOid(), TEST_CODE_ALG02);
                if (assessmentsALG02 != null && !assessmentsALG02.isEmpty()) {
                    for (StudentAssessmentInfo asmInfo : assessmentsALG02) {
                        if (GRADES_LIST_9_12.contains(asmInfo.getGradeLevelCode())
                                && asmInfo.getScaleScore().compareTo(BigDecimal.ZERO) > 0) {
                            value = false;
                            break;
                        }
                    }
                    value = false;
                }
            }
            if (value) {
                value = completedAlgebra2();
            }
            return value;
        }

        /**
         * Requires ELA 10.
         *
         * @return true, if successful
         */
        public boolean requiresELA10() {
            boolean value = false;
            if (GRADES_LIST_11_12.contains(m_student.getGradeLevel())) {
                Collection<StudentAssessmentInfo> assessments = getAssessmentsELA10();
                if (assessments != null && !assessments.isEmpty()) {
                    // skip if already two assessments
                    if (assessments.size() == 1 && !before2016(assessments.iterator().next())) {
                        BigDecimal score = bestScore(assessments);
                        if (BigDecimal.valueOf(725.0).compareTo(score) > 0) {
                            assessments = getAssessmentsALG01();
                            if (assessments != null && !assessments.isEmpty()) {
                                score = score.add(bestScore(assessments));
                            }
                            if (BigDecimal.valueOf(1450.0).compareTo(score) > 0) {
                                value = true;
                            }
                        }
                    }
                } else {
                    if (completedEnglish10()) {
                        value = true;
                    }
                }
            }
            return value;
        }

        /**
         * Requires ELA 11.
         *
         * @return true, if successful
         */
        public boolean requiresELA11() {
            boolean value = false;
            if (GRADES_LIST_11_12.contains(m_student.getGradeLevel()) && completedEnglish11()) {
                String code = (String) m_student.getFieldValueByAlias(ALIAS_STD_CCR_ELA);
                if (StringUtils.isEmpty(code) || !BooleanAsStringConverter.TRUE.equals(code)) {
                    value = true;
                }
            }
            return value;
        }


        /**
         * Best score.
         *
         * @param assessments Collection<StudentAssessment>
         * @return BigDecimal
         */
        private BigDecimal bestScore(Collection<StudentAssessmentInfo> assessments) {
            BigDecimal bestScore = BigDecimal.ZERO;
            if (assessments != null && !assessments.isEmpty()) {
                for (StudentAssessmentInfo assessment : assessments) {
                    if (assessment.getScaleScore() != null && assessment.getScaleScore().compareTo(bestScore) > 0) {
                        bestScore = assessment.getScaleScore();
                    }
                }
            }
            return bestScore;
        }

        /**
         * Completed algebra 1.
         *
         * @return true, if successful
         */
        private boolean completedAlgebra1() {
            return MDFallAsmCreation.this.completedAlgebra1(m_student.getOid());
        }

        /**
         * Completed algebra 1.
         *
         * @return true, if successful
         */
        private boolean completedAlgebra2() {
            return MDFallAsmCreation.this.completedAlgebra2(m_student.getOid());
        }

        /**
         * Completed english 10.
         *
         * @return true, if successful
         */
        private boolean completedEnglish10() {
            return MDFallAsmCreation.this.completedEnglish10(m_student.getOid());
        }

        /**
         * Completed english 11.
         *
         * @return true, if successful
         */
        private boolean completedEnglish11() {
            return MDFallAsmCreation.this.completedEnglish11(m_student.getOid());
        }

        /**
         * Gets the assessments ALG 01.
         *
         * @return Collection
         */
        private Collection<StudentAssessmentInfo> getAssessmentsALG01() {
            if (m_assessmentsALG01 == null) {
                m_assessmentsALG01 = getStudentAssessmentsWithScore(m_student.getOid(), TEST_CODE_ALG01);
                if (m_assessmentsALG01 == null) {
                    m_assessmentsALG01 = EMPTY_LIST;
                }
            }
            return m_assessmentsALG01;
        }

        /**
         * Gets the assessments ALG 02.
         *
         * @return Collection
         */
        private Collection<StudentAssessmentInfo> getAssessmentsALG02() {
            if (m_assessmentsALG02 == null) {
                m_assessmentsALG02 = getStudentAssessmentsWithScore(m_student.getOid(), TEST_CODE_ALG02);
                if (m_assessmentsALG02 == null) {
                    m_assessmentsALG02 = EMPTY_LIST;
                }
            }
            return m_assessmentsALG02;
        }

        /**
         * Gets the assessments ELA 10.
         *
         * @return Collection
         */
        private Collection<StudentAssessmentInfo> getAssessmentsELA10() {
            if (m_assessmentsELA10 == null) {
                m_assessmentsELA10 = getStudentAssessmentsWithScore(m_student.getOid(), TEST_CODE_ELA10);
                if (m_assessmentsELA10 == null) {
                    m_assessmentsELA10 = EMPTY_LIST;
                }
            }
            return m_assessmentsELA10;
        }

    }

    /**
     * The Class StudentAssessmentInfo.
     */
    public class StudentAssessmentInfo {
        private String m_asmGradeLevel;
        private PlainDate m_date;
        private BigDecimal m_scaleScore;

        /**
         * Instantiates a new student assessment info.
         *
         * @param date PlainDate
         * @param scaleScore BigDecimal
         * @param asmGradeLevel
         */
        public StudentAssessmentInfo(PlainDate date, BigDecimal scaleScore, String asmGradeLevel) {
            m_date = date;
            m_scaleScore = scaleScore;
            m_asmGradeLevel = asmGradeLevel;
        }

        /**
         * Instantiates a new student assessment info.
         *
         * @param assessment StudentAssessment
         */
        public StudentAssessmentInfo(StudentAssessment assessment) {
            this(assessment.getDate(), assessment.getScaleScore(), assessment.getGradeLevelCode());
        }

        /**
         * Gets the date.
         *
         * @return Plain date
         */
        public PlainDate getDate() {
            return m_date;
        }

        public String getGradeLevelCode() {
            return m_asmGradeLevel;
        }

        /**
         * Gets the scale score.
         *
         * @return Big decimal
         */
        public BigDecimal getScaleScore() {
            return m_scaleScore;
        }
    }

    /**
     * Class members
     */
    private Set<String> m_algebra1Set;
    private Set<String> m_algebra2Set;
    private AssessmentDefinition m_asd;
    private PlainDate m_asmDate;
    private Map<String, Map<String, Collection<StudentAssessment>>> m_asmMap;
    private Map<String, Map<String, Collection<StudentAssessmentInfo>>> m_asmHistoricalMap = new HashMap();
    private String m_assessmentRetestField;
    private String m_assessmentTestCodeField;
    private String m_assessmentTestDateField;
    private String m_assessmentTestFormatField;
    private String m_assessmentTestPeriodField;
    private String m_assessmentTestYearField;
    private int m_countCreated;
    private int m_countSkipped;
    private String m_ctxForParccValue;
    private DateAsStringConverter m_dateConverter;
    private List<String> m_detailMessages = new LinkedList();
    private Set<String> m_ela10Set;
    private Set<String> m_ela11Set;
    private PlainDate m_endDate2016;
    private String m_testFormat;

    /**
     * Before 2016.
     *
     * @param assessment StudentAssessment
     * @return true, if successful
     */
    protected boolean before2016(StudentAssessmentInfo assessment) {
        boolean value = false;
        if (m_endDate2016 == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisDistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(2016));
            BeanQuery query = new BeanQuery(SisDistrictSchoolYearContext.class, criteria);
            SisDistrictSchoolYearContext ctx = (SisDistrictSchoolYearContext) getBroker().getBeanByQuery(query);
            if (ctx == null) {
                throw new IllegalStateException(" A district context for school year 2016 must exist");
            }
            m_endDate2016 = ctx.getEndDate();
        }
        PlainDate assessmentDate = assessment.getDate();
        if (assessmentDate != null && !m_endDate2016.before(assessmentDate)) {
            value = true;
        }
        return value;
    }

    /**
     * Completed algebra 1.
     *
     * @param oid String
     * @return true, if successful
     */
    protected boolean completedAlgebra1(String oid) {
        return m_algebra1Set.contains(oid);
    }

    /**
     * Completed algebra 2.
     *
     * @param oid String
     * @return true, if successful
     */
    protected boolean completedAlgebra2(String oid) {
        return m_algebra2Set.contains(oid);
    }

    /**
     * Completed english 10.
     *
     * @param oid String
     * @return true, if successful
     */
    protected boolean completedEnglish10(String oid) {
        return m_ela10Set.contains(oid);
    }

    /**
     * Completed english 11.
     *
     * @param oid String
     * @return true, if successful
     */
    protected boolean completedEnglish11(String oid) {
        return m_ela11Set.contains(oid);
    }

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        if (initializeASDById()) {
            X2Criteria criteria = getStudentCriteria();
            SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, criteria);
            populateASMMap(subQuery);
            m_algebra1Set = populateTranscriptSet(subQuery, ALIAS_CRS_ALGEBRA_1);
            m_algebra2Set = populateTranscriptSet(subQuery, ALIAS_CRS_ALGEBRA_2);
            m_ela10Set = populateTranscriptSet(subQuery, ALIAS_CRS_ELA_10);
            m_ela11Set = populateTranscriptSet(subQuery, ALIAS_CRS_ELA_11);
            if (m_algebra1Set != null && m_ela10Set != null && m_ela11Set != null && m_algebra2Set != null) {
                BeanQuery query = new BeanQuery(SisStudent.class, criteria);
                try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                    while (iterator.hasNext()) {
                        SisStudent student = (SisStudent) iterator.next();
                        RuleSet rules = new RuleSet(student);
                        if (rules.requiresALG01()) {
                            createNewAsm(student, getStudentAssessments(student.getOid(), TEST_CODE_ALG01),
                                    TEST_CODE_ALG01);
                        }
                        if (rules.requiresALG02()) {
                            createNewAsm(student, getStudentAssessments(student.getOid(), TEST_CODE_ALG02),
                                    TEST_CODE_ALG02);
                        }
                        if (rules.requiresELA10()) {
                            createNewAsm(student, getStudentAssessments(student.getOid(), TEST_CODE_ELA10),
                                    TEST_CODE_ELA10);
                        }
                        if (rules.requiresELA11()) {
                            createNewAsm(student, getStudentAssessments(student.getOid(), TEST_CODE_ELA11),
                                    TEST_CODE_ELA11);
                        }
                    }
                }
                if (m_countCreated > 0 || m_countSkipped > 0) {
                    logMessage("Student Assessments created is " + m_countCreated);
                    logMessage("Student Assessments already existing is  " + m_countSkipped);
                    logMessage("");
                }

                for (String detail : m_detailMessages) {
                    logMessage(detail);
                }
            }
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                getLocale(), true);

        DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) getBroker()
                .getBeanByOid(DistrictSchoolYearContext.class, (String) getParameter(INPUT_PARAM_CTX));
        m_ctxForParccValue = (String) ctx.getFieldValueByBeanPath((String) getParameter(INPUT_PARAM_CTX_BEAN_PATH));
        m_asmDate = (PlainDate) getParameter(INPUT_PARAM_ASM_DATE);

        if (m_asmDate.after(ctx.getEndDate())) {
            m_asmDate = ctx.getEndDate();
        } else if (m_asmDate.before(ctx.getStartDate())) {
            m_asmDate = ctx.getStartDate();
        }

        String rcdTestFormatOid = (String) getParameter(INPUT_PARAM_TEST_FORMAT);
        ReferenceCode rcdTestFormat = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, rcdTestFormatOid);
        m_testFormat = rcdTestFormat != null ? rcdTestFormat.getStateCode() : null;
    }

    /**
     * Check if it is needed to save candidate ASM to the DB.
     *
     * @param student SisStudent
     * @param asmList Collection<StudentAssessment>
     * @param testCode String
     */
    private void createNewAsm(SisStudent student, Collection<StudentAssessment> asmList, String testCode) {
        boolean isAsmAlreadyCreated = false;

        if (asmList != null) {
            for (StudentAssessment asm : asmList) {
                String testedTestCode = (String) asm.getFieldValueByBeanPath(m_assessmentTestCodeField);
                String testedTestPeriod = (String) asm.getFieldValueByBeanPath(m_assessmentTestPeriodField);
                String testedTestContextValue = (String) asm.getFieldValueByBeanPath(m_assessmentTestYearField);

                if (testCode.equals(testedTestCode) && TEST_PERIOD_FALL.equals(testedTestPeriod) &&
                        m_ctxForParccValue != null && m_ctxForParccValue.equals(testedTestContextValue)) {
                    isAsmAlreadyCreated = true;
                    ++m_countSkipped;
                    logDetail("Student Assessment already exists: Student: " + student.getNameView()
                            + ", PARCC Test Code: " + testCode + ", Period: " + TEST_PERIOD_FALL);
                    break;
                }
            }
        }

        if (!isAsmAlreadyCreated) {
            saveNewAsm(student, testCode);
        }
    }

    /**
     * Get Field Java Name according given alias.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @param isRequired boolean
     * @return String
     */
    private String getAsmJavaName(String alias, DataDictionary dataDictionary, boolean isRequired) {
        String javaName = null;

        DataDictionaryField dictField = dataDictionary.findDataDictionaryFieldByAlias(alias);
        if (dictField != null && dataDictionary.containsAlias(alias)) {
            javaName = dictField.getJavaName();
        } else if (isRequired) {
            logMessage("Assessment column for " + alias + " is not defined");
        }

        return javaName;
    }

    /**
     * Check if alias define correct field.
     *
     * @param dictionary DataDictionary
     * @param alias String
     * @return String
     */
    private String getFieldJavaNameByAliasForCode(DataDictionary dictionary, String alias) {
        DataDictionaryField ddField = null;
        String value = null;
        if (dictionary != null && (ddField = dictionary.findDataDictionaryFieldByAlias(alias)) != null) {
            if (alias.split("-")[1].toUpperCase().equals(ddField.getDataTable().getObjectPrefix())) {
                value = ddField.getJavaName();
            } else {
                logMessage("Alias " + alias + " is defined on the wrong table");
            }
        } else {
            logMessage("Alias " + alias + " is not defined");
        }

        return value;
    }

    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Map<String, SisSchool> getSchools() {
        Map<String, SisSchool> schools = new LinkedHashMap<String, SisSchool>();
        Object objSchools = getParameter(INPUT_PARAM_SCHOOLS);
        String schoolOids = objSchools == null ? "" : (String) objSchools;

        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

        if (!StringUtils.isEmpty(schoolOids)) {
            List<String> oids = Arrays.asList(schoolOids.split(","));
            schoolCriteria.addIn(X2BaseBean.COL_OID, oids);
        }

        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
        schoolQuery.addOrderByAscending(SisSchool.COL_SCHOOL_LEVEL_CODE);
        schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
        schools = getBroker().getGroupedCollectionByQuery(schoolQuery, X2BaseBean.COL_OID, 1024);

        return schools;
    }

    /**
     * Gets the student assessments.
     *
     * @param oid String
     * @param testCode String
     * @return Collection
     */
    private Collection<StudentAssessment> getStudentAssessments(String oid, String testCode) {
        Collection<StudentAssessment> assessments = null;
        Map<String, Collection<StudentAssessment>> mapStudent = m_asmMap.get(oid);
        if (mapStudent != null) {
            assessments = mapStudent.get(testCode);
        }
        return assessments;
    }

    /**
     * Gets the student assessments with score.
     *
     * @param oid String
     * @param testCode String
     * @return Collection
     */
    private Collection<StudentAssessmentInfo> getStudentAssessmentsWithScore(String oid, String testCode) {
        Collection<StudentAssessmentInfo> infos = new ArrayList();
        Collection<StudentAssessment> assessments = getStudentAssessments(oid, testCode);
        if (assessments != null && !assessments.isEmpty()) {
            for (StudentAssessment assessment : assessments) {
                if (assessment.getScaleScore() != null) {
                    infos.add(new StudentAssessmentInfo(assessment));
                }
            }
        }
        Map<String, Collection<StudentAssessmentInfo>> studentMap = m_asmHistoricalMap.get(oid);
        if (studentMap != null && !studentMap.isEmpty()) {
            Collection<StudentAssessmentInfo> studentInfos = studentMap.get(testCode);
            if (studentInfos != null && !studentInfos.isEmpty()) {
                infos.addAll(studentInfos);
            }
        }

        return infos;
    }

    /**
     * Gets the student criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStudentCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(SisStudent.COL_GRADE_LEVEL, inputGrades());
        criteria.addIn(SisStudent.COL_SCHOOL_OID, getSchools().keySet());
        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
        return criteria;
    }

    /**
     * Find proper ASD by asdId.
     *
     * @return true, if successful
     */
    private boolean initializeASDById() {
        boolean retValue = false;
        String asdOid = (String) getParameter(INPUT_PARAM_ASD_OID);
        if (!StringUtils.isEmpty(asdOid)) {
            m_asd = (AssessmentDefinition) getBroker().getBeanByOid(AssessmentDefinition.class, asdOid);
        } else {
            X2Criteria asdCriteria = new X2Criteria();
            asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, getParameter(INPUT_PARAM_ASM_DEF_ID));

            m_asd = (AssessmentDefinition) getBroker()
                    .getBeanByQuery(new QueryByCriteria(AssessmentDefinition.class, asdCriteria));
        }

        if (m_asd == null) {
            logMessage("Required assessment definition with ID = " + getParameter(INPUT_PARAM_ASM_DEF_ID)
                    + " could not be found");
        } else {
            DataDictionary dataDictionary =
                    DataDictionary.getDistrictDictionary(m_asd, getBroker().getPersistenceKey());

            if (dataDictionary == null) {
                logMessage("Extended Dictinary for PARCC could not be loaded");
            } else {
                m_assessmentRetestField = getAsmJavaName(ALIAS_PARCC_RETEST, dataDictionary, true);
                m_assessmentTestCodeField = getAsmJavaName(ALIAS_PARCC_TESTCODE, dataDictionary, true);
                m_assessmentTestDateField = getAsmJavaName(ALIAS_PARCC_TESTDATE, dataDictionary, false);
                m_assessmentTestFormatField = getAsmJavaName(ALIAS_PARCC_TESTFORMAT, dataDictionary, true);
                m_assessmentTestPeriodField = getAsmJavaName(ALIAS_PARCC_TESTPERIOD, dataDictionary, true);
                m_assessmentTestYearField = getAsmJavaName(ALIAS_PARCC_TESTYEAR, dataDictionary, false);
                if (!StringUtils.isEmpty(m_assessmentTestCodeField) &&
                        !StringUtils.isEmpty(m_assessmentTestPeriodField) &&
                        !StringUtils.isEmpty(m_assessmentTestFormatField)) {
                    retValue = true;
                }
            }
        }
        return retValue;
    }

    /**
     * Get a list of the grade codes selected.
     *
     * @return List
     */
    private List<String> inputGrades() {
        List<String> grades = new LinkedList();
        if (StringUtils.isEmpty((String) getParameter(INPUT_PARAM_GRADES))) {
            grades.addAll(GRADES_LIST_9_12);
        } else {
            String[] rcdOids = ((String) getParameter(INPUT_PARAM_GRADES)).split(",");
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(X2BaseBean.COL_OID, Arrays.asList(rcdOids));

            ReportQueryByCriteria query =
                    new ReportQueryByCriteria(ReferenceCode.class, new String[] {ReferenceCode.COL_CODE}, criteria);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    grades.add((String) row[0]);
                }
            } finally {
                iterator.close();
            }
        }
        return grades;
    }

    /**
     * Add detail message that will be appended after other messages.
     *
     * @param key String
     */
    private void logDetail(String key) {
        m_detailMessages.add(key);
    }

    /**
     * Populate transcript set.
     *
     * @param subQuery SubQuery
     * @param crsAlias String
     * @return Set
     */
    private Set<String> populateTranscriptSet(SubQuery subQuery, String crsAlias) {
        Set<String> set = null;
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        String field = getFieldJavaNameByAliasForCode(dictionary, crsAlias);

        if (!StringUtils.isEmpty(field)) {
            set = new HashSet();
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER + SchoolCourse.REL_COURSE
                    + ModelProperty.PATH_DELIMITER + field, BooleanAsStringConverter.TRUE);
            criteria.addNotEmpty(Transcript.COL_FINAL_GRADE, getBroker().getPersistenceKey());
            criteria.addIn(Transcript.COL_STUDENT_OID, subQuery);
            String[] columns = new String[] {Transcript.COL_STUDENT_OID};
            ColumnQuery query = new ColumnQuery(Transcript.class, columns, criteria);
            try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    set.add((String) row[0]);
                }
            }
        } else {
            logMessage("Required alias on Course " + crsAlias + " could not be found");
        }
        return set;
    }

    /**
     * Method to populate ASM map keyed on asmStdOid.
     *
     * @param studentOids SubQuery
     */
    private void populateASMMap(SubQuery studentOids) {
        X2Criteria asmCriteria = new X2Criteria();

        asmCriteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, m_asd.getOid());
        asmCriteria.addIn(StudentAssessment.COL_STUDENT_OID, studentOids);
        asmCriteria.addIn(m_assessmentTestCodeField, TEST_CODES);

        QueryByCriteria asmQuery = new QueryByCriteria(StudentAssessment.class, asmCriteria);

        m_asmMap = getBroker().getGroupedCollectionByQuery(asmQuery,
                new String[] {StudentAssessment.COL_STUDENT_OID, m_assessmentTestCodeField},
                new int[] {1024, 8});

        String asmHistoricalOids = (String) getParameter(INPUT_PARAM_HISTORICAL_ASD_OID);
        if (!StringUtils.isEmpty(asmHistoricalOids)) {
            Set<String> oids = new HashSet();
            oids.addAll(Arrays.asList(asmHistoricalOids.split("\\s*,\\s*")));
            oids.remove(m_asd.getOid());
            if (!oids.isEmpty()) {
                for (String oid : oids) {
                    AssessmentDefinition asd =
                            (AssessmentDefinition) getBroker().getBeanByOid(AssessmentDefinition.class, oid);
                    if (asd != null) {
                        DataDictionary dataDictionary =
                                DataDictionary.getDistrictDictionary(asd, getBroker().getPersistenceKey());
                        String testCodeField = getAsmJavaName(ALIAS_PARCC_TESTCODE, dataDictionary, true);
                        asmCriteria = new X2Criteria();

                        asmCriteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, oid);
                        asmCriteria.addIn(StudentAssessment.COL_STUDENT_OID, studentOids);
                        asmCriteria.addIn(testCodeField, TEST_CODES);

                        String[] columns = new String[] {StudentAssessment.COL_STUDENT_OID, testCodeField,
                                StudentAssessment.COL_DATE, StudentAssessment.COL_SCALE_SCORE,
                                StudentAssessment.COL_GRADE_LEVEL_CODE};
                        ColumnQuery query = new ColumnQuery(StudentAssessment.class, columns, asmCriteria);
                        try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                            while (iterator.hasNext()) {
                                Object[] row = (Object[]) iterator.next();
                                String studentOid = (String) row[0];
                                String testCode = (String) row[1];
                                if (row[2] != null && row[3] != null && row[4] != null) {
                                    PlainDate date = new PlainDate((java.util.Date) row[2]);
                                    BigDecimal scaleScore = (BigDecimal) row[3];
                                    String grade = (String) row[4];
                                    if (scaleScore != null && scaleScore.compareTo(BigDecimal.ZERO) > 0) {
                                        Map<String, Collection<StudentAssessmentInfo>> studentMap =
                                                m_asmHistoricalMap.get(studentOid);
                                        if (studentMap == null) {
                                            studentMap = new HashMap();
                                            m_asmHistoricalMap.put(studentOid, studentMap);
                                        }
                                        Collection<StudentAssessmentInfo> infos = studentMap.get(testCode);
                                        if (infos == null) {
                                            infos = new LinkedList();
                                            studentMap.put(testCode, infos);
                                        }
                                        infos.add(new StudentAssessmentInfo(date, scaleScore, grade));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Save ASM to DB.
     *
     * @param student SisStudent
     * @param testCode String
     */
    private void saveNewAsm(SisStudent student, String testCode) {
        StudentAssessment newAsm = X2BaseBean.newInstance(StudentAssessment.class, getBroker().getPersistenceKey());
        newAsm.setAssessmentDefinitionOid(m_asd.getOid());
        newAsm.setDate(m_asmDate);
        newAsm.setGradeLevelCode(student.getGradeLevel());
        newAsm.setStudentOid(student.getOid());
        newAsm.setSchoolOid(student.getSchoolOid());
        newAsm.setFieldValueByBeanPath(m_assessmentTestCodeField, testCode);
        newAsm.setFieldValueByBeanPath(m_assessmentTestFormatField, m_testFormat);
        newAsm.setFieldValueByBeanPath(m_assessmentTestPeriodField, TEST_PERIOD_FALL);
        if (!StringUtils.isEmpty(m_assessmentTestDateField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestDateField, m_dateConverter.getSystemString(m_asmDate));
        }
        if (!StringUtils.isEmpty(m_assessmentTestYearField)) {
            newAsm.setFieldValueByBeanPath(m_assessmentTestYearField, m_ctxForParccValue);
        }
        if (!StringUtils.isEmpty(m_assessmentRetestField)) {
            String value = getStudentAssessmentsWithScore(student.getOid(), testCode).isEmpty()
                    ? BooleanAsStringConverter.FALSE
                    : BooleanAsStringConverter.TRUE;
            newAsm.setFieldValueByBeanPath(m_assessmentRetestField, value);
        }

        getBroker().saveBeanForced(newAsm);

        ++m_countCreated;
        logDetail("Student Assessment was created for: Student: " + student.getNameView() + ", Test Code: "
                + testCode);

    }

}
