/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.ibm.icu.text.SimpleDateFormat;
import com.ibm.icu.util.Calendar;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLStudentAssessmentData.ASD_TYPES;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetProcedureASM.
 */
public class SetProcedureASM extends SetProcedure {

    private final static String ALIAS_TEST_SUBJECT_CONTENT = "asm-%asdId%-testSubjectContent";

    private final static String ASD_ID_PATTERN = "%asdId%";

    private final static String FILE_NAME_ASSESSMENT_DEFINITION_AICE = "assessment-definition-AICE.zip";
    private final static String FILE_NAME_ASSESSMENT_DEFINITION_AP = "assessment-definition-AP.zip";
    private final static String FILE_NAME_ASSESSMENT_DEFINITION_IB = "assessment-definition-IB.zip";

    private Collection<String> m_asdRefTablesNames =
            Arrays.asList("FL AP Course", "FL Test Subject Content Code", "FL Test Score Types");
    private Collection<String> m_asdRefTablesOids =
            Arrays.asList("rtbFlAPCourse", "rtbFlSubCont", "rtbFlScoreType");

    private SimpleDateFormat m_dateFormat = new SimpleDateFormat("MMddyyyy");

    private List<String> m_caiTestLevelCodes = Arrays.asList("A", "AS");
    private List<String> m_ibpTestLevelCodes = Arrays.asList("HL", "SL");

    private Random m_random = null;

    private Map<String, Map<String, Collection<StudentAssessment>>> m_studentAssessments = null;

    private FLStudentHelper m_studentHelper = null;

    private RuntimeParam[] m_caiTestDates = new RuntimeParam[] {
            new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "01"),
            new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "02"),
            new RuntimeParam(RuntimeParam.FISCAL_DATE, "11", "01"),
            new RuntimeParam(RuntimeParam.FISCAL_DATE, "11", "02")};

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        removeNonValidReferenceTables();

        try {
            importBundle(FILE_NAME_ASSESSMENT_DEFINITION_AICE);
            importBundle(FILE_NAME_ASSESSMENT_DEFINITION_AP);
            importBundle(FILE_NAME_ASSESSMENT_DEFINITION_IB);
            DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
        } catch (Exception e) {
            e.printStackTrace();
        }

        addAssessments();
        for (int i = 0; i < 5; i++) {
            runAndValidateExports();
            super.execute();
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
        super.initialize(FLExportConfiguration.FL_EXPORT.ASM, SisStudent.class);

        m_random = new Random(System.currentTimeMillis());

        m_studentHelper = getFLReportData().getStudentHelper();

        initializeRule7Fix();
        initializeRule20Fix();
        initializeRule24Fix();
        initializeRule26Fix();
        initializeRule27Fix();
        initializeRule28Fix();
        initializeRule29Fix();
        initializeRule31Fix();
        initializeRule32Fix();
        initializeRule33Fix();
        initializeRule34Fix();
    }

    private void addAssessments() throws X2BaseException {
        X2Criteria studentCriteria = getFLReportData().getStudentHelper().getStudentCriteria();
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        X2Criteria studentAssessmentCriteria = new X2Criteria();
        studentAssessmentCriteria
                .addIn(StudentAssessment.REL_STUDENT + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, studentSubQuery);
        studentAssessmentCriteria.addIn(
                StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER
                        + AssessmentDefinition.COL_ID,
                ASD_TYPES.getIds());

        studentAssessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, getFLReportData().getSurveyPeriod().getStartDate());
        studentAssessmentCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE, getFLReportData().getSurveyPeriod().getEndDate());

        QueryByCriteria studentAssessmentQuery =
                new QueryByCriteria(StudentAssessment.class, studentAssessmentCriteria, false);

        X2Criteria asdCriteria = new X2Criteria();
        asdCriteria.addIn(AssessmentDefinition.COL_ID, ASD_TYPES.getIds());
        SubQuery defOidsSubQuery = new SubQuery(AssessmentDefinition.class, X2BaseBean.COL_OID, asdCriteria);
        List<String> defOids = new ArrayList(getBroker().getSubQueryCollectionByQuery(defOidsSubQuery));

        int requiredNumOfAssessments = 10;
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
        if (getBroker().getCollectionByQuery(studentAssessmentQuery).size() < requiredNumOfAssessments) {
            List<SisStudent> students = new ArrayList<SisStudent>(getBroker().getCollectionByQuery(studentQuery));
            int addedAsmCounter = 0;
            while (addedAsmCounter < requiredNumOfAssessments) {
                SisStudent randomStudent = students.get(m_random.nextInt(students.size()));
                StudentAssessment newAsm = new StudentAssessment(getBroker().getPersistenceKey());
                newAsm.setStudentOid(randomStudent.getOid());
                newAsm.setSchoolOid(randomStudent.getSchoolOid());
                newAsm.setAssessmentDefinitionOid(defOids.get(m_random.nextInt(defOids.size())));
                PlainDate asmDate = new PlainDate(new Date(
                        ThreadLocalRandom.current().nextLong(getCurrentContext().getStartDate().getTime(),
                                getCurrentContext().getEndDate().getTime())));
                newAsm.setDate(asmDate);
                String asdId = newAsm.getAssessmentDefinition().getId();
                DataDictionary dictionary =
                        DataDictionary.getDistrictDictionary(newAsm.getExtendedDataDictionary(),
                                getBroker().getPersistenceKey());
                DataDictionaryField field =
                        dictionary
                                .findDataDictionaryFieldByAlias(ALIAS_TEST_SUBJECT_CONTENT.replaceAll(ASD_ID_PATTERN, asdId.toLowerCase()));
                newAsm.setFieldValueByBeanPath(field.getJavaName(), getRandomCodeForField(field));

                getModelBroker().saveBean(newAsm);

                addedAsmCounter++;
            }
        }

        DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
    }

    /**
     * Initialize rule 7 fix.
     */
    private void initializeRule7Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        for (ASD_TYPES asdType : ASD_TYPES.values()) {
            String testName = asdType.getTestName();
            final String asdId = asdType.getId();
            ruleWithFixes.add(new RuleWithFix(
                    ValidationRule.testIf(Restriction.equals("Test Name", testName))
                            .testThen(Restriction.and(Restriction.greaterThanOrEquals("Publication Year",
                                    Double.valueOf(1970)),
                                    Restriction.lessThanOrEquals("Publication Year",
                                            Double.valueOf(Calendar.getInstance().get(Calendar.YEAR))))),
                    new Fix() {
                        String pubYearAliasPattern = "asm-%asdId%-testPublicationYear";

                        @Override
                        protected void fixError(X2BaseBean bean) {
                            Collection<StudentAssessment> assessments = getStdAsmsById(bean.getOid(), asdId);
                            for (StudentAssessment asm : assessments) {

                                DataDictionary dictionary =
                                        DataDictionary.getDistrictDictionary(asm.getExtendedDataDictionary(),
                                                getBroker().getPersistenceKey());

                                String alias = pubYearAliasPattern.replaceAll(ASD_ID_PATTERN, asdId.toLowerCase());
                                DataDictionaryField ddf = dictionary.findDataDictionaryFieldByAlias(alias);
                                Integer randomYear = Integer.valueOf(ThreadLocalRandom.current().nextInt(1970,
                                        Calendar.getInstance().get(Calendar.YEAR) + 1));
                                asm.setFieldValueByBeanPath(ddf.getJavaName(), String.valueOf(randomYear));

                                getModelBroker().saveBean(asm, dictionary);
                            }
                        }
                    }));
        }

        addFixesByRuleNumber("7", ruleWithFixes);
    }

    /**
     * Initialize rule 20 fix.
     */
    private void initializeRule20Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        for (ASD_TYPES asdType : ASD_TYPES.values()) {
            final String asdId = asdType.getId();
            ruleWithFixes.add(new RuleWithFix(
                    ValidationRule.testIf(Restriction.equals("Test Score Type (2)", "ZZ"))
                            .testThen(Restriction.equals("Test Score (2)", "0000")),
                    new Fix() {
                        String aliasPattern = "asm-%asdId%-testScore2";

                        @Override
                        protected void fixError(X2BaseBean bean) {
                            Collection<StudentAssessment> assessments = getStdAsmsById(bean.getOid(), asdId);
                            for (StudentAssessment asm : assessments) {

                                DataDictionary dictionary =
                                        DataDictionary.getDistrictDictionary(asm.getExtendedDataDictionary(),
                                                getBroker().getPersistenceKey());

                                String alias = aliasPattern.replaceAll(ASD_ID_PATTERN, asdId.toLowerCase());
                                DataDictionaryField ddf = dictionary.findDataDictionaryFieldByAlias(alias);
                                asm.setFieldValueByBeanPath(ddf.getJavaName(), "0000");

                                getModelBroker().saveBean(asm, dictionary);
                            }
                        }
                    }));
        }

        addFixesByRuleNumber("20", ruleWithFixes);
    }

    /**
     * Initialize rule 24 fix.
     */
    private void initializeRule24Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        for (ASD_TYPES asdType : ASD_TYPES.values()) {
            final String asdId = asdType.getId();
            ruleWithFixes.add(new RuleWithFix(
                    ValidationRule.testIf(Restriction.pattern("Test Name", "^(APT|CAI|IBP)$"))
                            .testThen(Restriction.and(Restriction.equals("Test Score Type (1)", "SS"),
                                    Restriction.equals("Test Score Type (2)", "ZZ"))),
                    new Fix() {
                        String aliasPatternTst1 = "asm-%asdId%-testScoreType1";
                        String aliasPatternTst2 = "asm-%asdId%-testScoreType2";

                        @Override
                        protected void fixError(X2BaseBean bean) {
                            Collection<StudentAssessment> assessments = getStdAsmsById(bean.getOid(), asdId);
                            for (StudentAssessment asm : assessments) {

                                DataDictionary dictionary =
                                        DataDictionary.getDistrictDictionary(asm.getExtendedDataDictionary(),
                                                getBroker().getPersistenceKey());

                                String aliasTst1 = aliasPatternTst1.replaceAll(ASD_ID_PATTERN, asdId.toLowerCase());
                                String aliasTst2 = aliasPatternTst2.replaceAll(ASD_ID_PATTERN, asdId.toLowerCase());
                                String testSubjectContent = ALIAS_TEST_SUBJECT_CONTENT.replaceAll(ASD_ID_PATTERN, asdId.toLowerCase());
                                DataDictionaryField ddfTst1 = dictionary.findDataDictionaryFieldByAlias(aliasTst1);
                                DataDictionaryField ddfTst2 = dictionary.findDataDictionaryFieldByAlias(aliasTst2);
                                DataDictionaryField tsc = dictionary.findDataDictionaryFieldByAlias(testSubjectContent);
                                asm.setFieldValueByBeanPath(ddfTst1.getJavaName(), "SS");
                                asm.setFieldValueByBeanPath(ddfTst2.getJavaName(), "ZZ");
                                if (StringUtils.isEmpty((String) asm.getFieldValueByBeanPath(tsc.getJavaName()))) {
                                    asm.setFieldValueByBeanPath(tsc.getJavaName(), getRandomCodeForField(tsc));
                                }

                                getModelBroker().saveBean(asm, dictionary);
                            }
                        }
                    }));
        }

        addFixesByRuleNumber("24", ruleWithFixes);
    }

    /**
     * Initialize rule 26 fix.
     */
    private void initializeRule26Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Test Name", "CAI"))
                        .testThen(Restriction.greaterThanOrEqualsYearOfDateField("Publication Year",
                                "Test Date")),
                new Fix() {
                    String aliasPattern = "asm-%asdId%-testPublicationYear";
                    ASD_TYPES asdType = ASD_TYPES.AICE;

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Date testDate = null;
                        try {
                            testDate = m_dateFormat
                                    .parse(getHelper().getExportFormatRowFieldValue(getCurrentRow(), m_export, "Test Date"));
                        } catch (ParseException e) {
                            e.printStackTrace();
                        }
                        if (testDate != null) {
                            Collection<StudentAssessment> assessments = getStdAsmsById(bean.getOid(), asdType.getId());
                            for (StudentAssessment asm : assessments) {
                                DataDictionary dictionary =
                                        DataDictionary.getDistrictDictionary(asm.getExtendedDataDictionary(),
                                                getBroker().getPersistenceKey());

                                String alias = aliasPattern.replaceAll(ASD_ID_PATTERN, asdType.getId().toLowerCase());
                                DataDictionaryField ddf = dictionary.findDataDictionaryFieldByAlias(alias);

                                Calendar calTestDate = Calendar.getInstance();
                                calTestDate.setTime(testDate);

                                Calendar calNow = Calendar.getInstance();
                                calNow.setTime(testDate);

                                if (calTestDate.get(Calendar.YEAR) != calNow.get(Calendar.YEAR)) {
                                    asm.setFieldValueByBeanPath(ddf.getJavaName(),
                                            String.valueOf(ThreadLocalRandom.current().nextInt(calTestDate.get(Calendar.YEAR),
                                                    calNow.get(Calendar.YEAR))));
                                } else {
                                    asm.setFieldValueByBeanPath(ddf.getJavaName(), String.valueOf(calTestDate.get(Calendar.YEAR)));
                                }

                                getModelBroker().saveBean(asm, dictionary);
                            }
                        }
                    }
                }));

        addFixesByRuleNumber("26", ruleWithFixes);
    }

    /**
     * Initialize rule 27 fix.
     */
    private void initializeRule27Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Test Name", "CAI"))
                        .testThen(Restriction.or(
                                Restriction.equals("Test Date", m_caiTestDates[0]),
                                Restriction.equals("Test Date", m_caiTestDates[1]),
                                Restriction.equals("Test Date", m_caiTestDates[2]),
                                Restriction.equals("Test Date", m_caiTestDates[3]))),
                new Fix() {
                    ASD_TYPES asdType = ASD_TYPES.AICE;

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Date testDate = null;
                        try {
                            testDate = m_dateFormat
                                    .parse(getHelper().getExportFormatRowFieldValue(getCurrentRow(), m_export, "Test Date"));
                        } catch (ParseException e) {
                            e.printStackTrace();
                        }
                        if (testDate != null) {
                            Collection<StudentAssessment> assessments = getStdAsmsById(bean.getOid(), asdType.getId());
                            for (StudentAssessment asm : assessments) {
                                asm.setDate(
                                        new PlainDate((Date) m_caiTestDates[m_random.nextInt(m_caiTestDates.length)]
                                                .getRuntimeObject(getHelper())));

                                DataDictionary dictionary =
                                        DataDictionary.getDistrictDictionary(asm.getExtendedDataDictionary(),
                                                getBroker().getPersistenceKey());

                                getModelBroker().saveBean(asm, dictionary);
                            }
                        }
                    }
                }));

        addFixesByRuleNumber("27", ruleWithFixes);
    }

    /**
     * Initialize rule 28 fix.
     */
    private void initializeRule28Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        for (ASD_TYPES asdType : ASD_TYPES.values()) {
            final String asdId = asdType.getId();
            ruleWithFixes.add(new RuleWithFix(
                    ValidationRule.testIf(Restriction.pattern("Test Name", "^(APT|CAI|IBP)$"))
                            .testThen(Restriction.equals("Test Form", "Z")),
                    new Fix() {
                        String aliasPattern = "asm-%asdId%-testForm";

                        @Override
                        protected void fixError(X2BaseBean bean) {
                            Collection<StudentAssessment> assessments = getStdAsmsById(bean.getOid(), asdId);
                            for (StudentAssessment asm : assessments) {

                                DataDictionary dictionary =
                                        DataDictionary.getDistrictDictionary(asm.getExtendedDataDictionary(),
                                                getBroker().getPersistenceKey());

                                String alias = aliasPattern.replaceAll(ASD_ID_PATTERN, asdId.toLowerCase());
                                DataDictionaryField ddf = dictionary.findDataDictionaryFieldByAlias(alias);
                                asm.setFieldValueByBeanPath(ddf.getJavaName(), "Z");

                                getModelBroker().saveBean(asm, dictionary);
                            }
                        }
                    }));
        }

        addFixesByRuleNumber("28", ruleWithFixes);
    }

    /**
     * Initialize rule 29 fix.
     */
    private void initializeRule29Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        for (ASD_TYPES asdType : ASD_TYPES.values()) {
            final String asdId = asdType.getId();
            ruleWithFixes.add(new RuleWithFix(
                    ValidationRule.testIf(Restriction.equals("Test Name", "CAI"))
                            .testThen(Restriction.pattern("Test Level", "^(A|AS)$")),
                    new Fix() {
                        String aliasPattern = "asm-%asdId%-testLevel";

                        @Override
                        protected void fixError(X2BaseBean bean) {
                            Collection<StudentAssessment> assessments = getStdAsmsById(bean.getOid(), asdId);
                            for (StudentAssessment asm : assessments) {

                                DataDictionary dictionary =
                                        DataDictionary.getDistrictDictionary(asm.getExtendedDataDictionary(),
                                                getBroker().getPersistenceKey());

                                String alias = aliasPattern.replaceAll(ASD_ID_PATTERN, asdId.toLowerCase());
                                DataDictionaryField ddf = dictionary.findDataDictionaryFieldByAlias(alias);
                                asm.setFieldValueByBeanPath(ddf.getJavaName(),
                                        m_caiTestLevelCodes.get(m_random.nextInt(m_caiTestLevelCodes.size())));

                                getModelBroker().saveBean(asm, dictionary);
                            }
                        }
                    }));
        }

        addFixesByRuleNumber("29", ruleWithFixes);
    }

    /**
     * Initialize rule 31 fix.
     */
    private void initializeRule31Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Test Name", "APT"))
                        .testThen(Restriction.equals("Test Level", "ZZ")),
                new Fix() {
                    String aliasPattern = "asm-%asdId%-testLevel";
                    ASD_TYPES asdType = ASD_TYPES.AP;

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Collection<StudentAssessment> assessments = getStdAsmsById(bean.getOid(), asdType.getId());
                        for (StudentAssessment asm : assessments) {

                            DataDictionary dictionary =
                                    DataDictionary.getDistrictDictionary(asm.getExtendedDataDictionary(),
                                            getBroker().getPersistenceKey());

                            String alias = aliasPattern.replaceAll(ASD_ID_PATTERN, asdType.getId().toLowerCase());
                            DataDictionaryField ddf = dictionary.findDataDictionaryFieldByAlias(alias);
                            asm.setFieldValueByBeanPath(ddf.getJavaName(), "ZZ");

                            getModelBroker().saveBean(asm, dictionary);
                        }
                    }
                }));

        addFixesByRuleNumber("31", ruleWithFixes);
    }

    /**
     * Initialize rule 32 fix.
     */
    private void initializeRule32Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Test Name", "IBP"))
                        .testThen(Restriction.pattern("Test Level", "^(HL|SL)$")),
                new Fix() {
                    String aliasPattern = "asm-%asdId%-testLevel";
                    ASD_TYPES asdType = ASD_TYPES.IB;

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Collection<StudentAssessment> assessments = getStdAsmsById(bean.getOid(), asdType.getId());
                        for (StudentAssessment asm : assessments) {

                            DataDictionary dictionary =
                                    DataDictionary.getDistrictDictionary(asm.getExtendedDataDictionary(),
                                            getBroker().getPersistenceKey());

                            String alias = aliasPattern.replaceAll(ASD_ID_PATTERN, asdType.getId().toLowerCase());
                            DataDictionaryField ddf = dictionary.findDataDictionaryFieldByAlias(alias);
                            asm.setFieldValueByBeanPath(ddf.getJavaName(),
                                    m_ibpTestLevelCodes.get(m_random.nextInt(m_ibpTestLevelCodes.size())));

                            getModelBroker().saveBean(asm, dictionary);
                        }
                    }
                }));

        addFixesByRuleNumber("32", ruleWithFixes);
    }

    /**
     * Initialize rule 33 fix.
     */
    private void initializeRule33Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Name", "APT"))
                                        .testThen(
                                                Restriction.pattern("Test Score (1)", "^000[1-5]$"))),
                        "If Test Name is APT then Test Score (1) must be 0001, 0002, 0003, 0004, or 0005."),
                new Fix() {
                    String aliasPattern = "asm-%asdId%-testScore1";
                    ASD_TYPES asdType = ASD_TYPES.AP;

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Collection<StudentAssessment> assessments = getStdAsmsById(bean.getOid(), asdType.getId());
                        for (StudentAssessment asm : assessments) {

                            DataDictionary dictionary =
                                    DataDictionary.getDistrictDictionary(asm.getExtendedDataDictionary(),
                                            getBroker().getPersistenceKey());

                            String alias = aliasPattern.replaceAll(ASD_ID_PATTERN, asdType.getId().toLowerCase());
                            DataDictionaryField ddf = dictionary.findDataDictionaryFieldByAlias(alias);
                            asm.setFieldValueByBeanPath(ddf.getJavaName(), String.valueOf(ThreadLocalRandom.current().nextInt(1, 6)));

                            getModelBroker().saveBean(asm, dictionary);
                        }
                    }
                }));

        addFixesByRuleNumber("33", ruleWithFixes);
    }

    /**
     * Initialize rule 34 fix.
     */
    private void initializeRule34Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Test Name", "IBP"))
                        .testThen(Restriction.pattern("Test Score (1)", "^000[1-9]$")),
                new Fix() {
                    String aliasPattern = "asm-%asdId%-testScore1";
                    ASD_TYPES asdType = ASD_TYPES.IB;

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Collection<StudentAssessment> assessments = getStdAsmsById(bean.getOid(), asdType.getId());
                        for (StudentAssessment asm : assessments) {

                            DataDictionary dictionary =
                                    DataDictionary.getDistrictDictionary(asm.getExtendedDataDictionary(),
                                            getBroker().getPersistenceKey());

                            String alias = aliasPattern.replaceAll(ASD_ID_PATTERN, asdType.getId().toLowerCase());
                            DataDictionaryField ddf = dictionary.findDataDictionaryFieldByAlias(alias);
                            asm.setFieldValueByBeanPath(ddf.getJavaName(), String.valueOf(ThreadLocalRandom.current().nextInt(1, 10)));

                            getModelBroker().saveBean(asm, dictionary);
                        }
                    }
                }));

        addFixesByRuleNumber("34", ruleWithFixes);
    }

    /**
     * Gets the std asms by id.
     *
     * @param studentOid String
     * @param asdId String
     * @return Collection
     */
    private Collection<StudentAssessment> getStdAsmsById(String studentOid, String asdId) {
        if (m_studentAssessments == null) {
            X2Criteria studentCriteria = m_studentHelper.getStudentCriteria();
            SubQuery studentQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

            X2Criteria studentAssessmentCriteria = new X2Criteria();
            studentAssessmentCriteria
                    .addIn(StudentAssessment.REL_STUDENT + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                            studentQuery);
            studentAssessmentCriteria.addIn(
                    StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER
                            + AssessmentDefinition.COL_ID,
                    ASD_TYPES.getIds());

            studentAssessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE,
                    getFLReportData().getSurveyPeriod().getStartDate());
            studentAssessmentCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE,
                    getFLReportData().getSurveyPeriod().getEndDate());

            QueryByCriteria studentAssessmentQuery =
                    new QueryByCriteria(StudentAssessment.class, studentAssessmentCriteria, false);

            String[] columns = new String[] {StudentAssessment.COL_STUDENT_OID,
                    StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER
                            + AssessmentDefinition.COL_ID};
            m_studentAssessments =
                    getBroker().getGroupedCollectionByQuery(studentAssessmentQuery, columns, new int[] {1000, 10});
        }

        ArrayList<StudentAssessment> studentAssessments = new ArrayList<>();

        Map<String, Collection<StudentAssessment>> studentAssessmentsByIds = m_studentAssessments.get(studentOid);
        if (studentAssessmentsByIds != null && studentAssessmentsByIds.get(asdId) != null) {
            studentAssessments.addAll(studentAssessmentsByIds.get(asdId));
        }

        return studentAssessments;
    }

    private void removeNonValidReferenceTables() {
        X2Criteria nonValidRefTablesCriteria = new X2Criteria();
        nonValidRefTablesCriteria.addIn(ReferenceTable.COL_USER_NAME, m_asdRefTablesNames);
        nonValidRefTablesCriteria.addNotIn(X2BaseBean.COL_OID, m_asdRefTablesOids);
        QueryByCriteria query = new QueryByCriteria(ReferenceTable.class, nonValidRefTablesCriteria);
        getModelBroker().deleteByQuery(query);
    }
}
