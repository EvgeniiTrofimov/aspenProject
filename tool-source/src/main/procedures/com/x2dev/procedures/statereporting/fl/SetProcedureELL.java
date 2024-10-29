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

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateDistrictNumber;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Pattern;

/**
 * The Class SetProcedureSSC.
 */
public class SetProcedureELL extends SetProcedure {

    public static final String DDX_ID = "FL-PGM-ELL";

    protected static final String ALIAS_ELL_CODE = "pgm-ell-code";
    protected static final String ALIAS_STD_STATE_ID = "all-std-StateId";
    protected static final String ALIAS_ORG_DISTRICT_NUMBER = "all-org-DistrictNumber";
    protected static final String ALIAS_REEVALUATION_DATE = "pgm-reevaluation-date";
    protected static final String ALIAS_TEST_SC_T_LISTENING = "pgm-test-score-type-listening";
    protected static final String ALIAS_TEST_SC_T_READING = "pgm-test-score-type-reading";
    protected static final String ALIAS_TEST_SC_T_SPEAKING = "pgm-test-score-type-speaking";
    protected static final String ALIAS_TEST_SC_T_WRITING = "pgm-test-score-type-writing";
    protected static final String ALIAS_BASIS_OF_ENTRY = "pgm-basis-of-entry";
    protected static final String ALIAS_BASIS_OF_EXIT_FIRST = "pgm-basis-of-exit-first";
    protected static final String ALIAS_BASIS_OF_EXIT_SECOND = "pgm-basis-of-exit-second";
    protected static final String ALIAS_CLASSIFICATION_DATE = "pgm-classification-date";
    protected static final String ALIAS_EXT_OF_INSTRUCTION = "pgm-extension-of-instruction";
    protected static final String ALIAS_FUND_SRC = "pgm-ell-fund-src";
    protected static final String ALIAS_PROG_PARTICIPATION = "pgm-prog-participation";
    protected static final String ALIAS_RECLASSIFICATION_DATE = "pgm-reclassification-date";
    protected static final String ALIAS_RECLASS_EXIT_DATE = "pgm-reclassification-exit-date";
    protected static final String ALIAS_STUDENT_PLAN_DATE = "pgm-student-plan-date";
    protected static final String ALIAS_TEST_CONT_LISTENING = "pgm-test-cont-listening";
    protected static final String ALIAS_TEST_CONT_READING = "pgm-test-cont-reading";
    protected static final String ALIAS_TEST_CONT_SPEAKING = "pgm-test-cont-speaking";
    protected static final String ALIAS_TEST_DATE_LISTENING = "pgm-test-date-listening";
    protected static final String ALIAS_TEST_DATE_READING = "pgm-test-date-reading";
    protected static final String ALIAS_TEST_DATE_SPEAKING = "pgm-test-date-speaking";
    protected static final String ALIAS_TEST_DATE_WRITING = "pgm-test-date-writing";
    protected static final String ALIAS_TEST_FORM_LISTENING = "pgm-test-form-listening";
    protected static final String ALIAS_TEST_FORM_READING = "pgm-test-form-reading";
    protected static final String ALIAS_TEST_FORM_SPEAKING = "pgm-test-form-speaking";
    protected static final String ALIAS_TEST_FORM_WRITING = "pgm-test-form-writing";
    protected static final String ALIAS_TEST_LEVEL_LISTENING = "pgm-test-level-listening";
    protected static final String ALIAS_TEST_LEVEL_READING = "pgm-test-level-reading";
    protected static final String ALIAS_TEST_LEVEL_SPEAKING = "pgm-test-level-speaking";
    protected static final String ALIAS_TEST_LEVEL_WRITING = "pgm-test-level-writing";
    protected static final String ALIAS_TEST_NAME_LISTENING = "pgm-test-name-listening";
    protected static final String ALIAS_TEST_NAME_READING = "pgm-test-name-reading";
    protected static final String ALIAS_TEST_NAME_SPEAKING = "pgm-test-name-speaking";
    protected static final String ALIAS_TEST_NAME_WRITING = "pgm-test-name-writing";
    protected static final String ALIAS_TEST_SCORE_LISTENING = "pgm-test-score-listening";
    protected static final String ALIAS_TEST_SCORE_READING = "pgm-test-score-reading";
    protected static final String ALIAS_TEST_SCORE_SPEAKING = "pgm-test-score-speaking";
    protected static final String ALIAS_TEST_SCORE_WRITING = "pgm-test-score-writing";
    protected static final String ALIAS_TEST_SUBJECT_WRITING = "pgm-test-subject-writing";
    protected static final String ALIAS_TIER_PLACEMENT = "pgm-tier-placement";

    private DataDictionary m_dictionary;

    private DataDictionaryField m_fieldOrgDistrictNumber;
    private DataDictionaryField m_fieldPgmEllCode;
    private DataDictionaryField m_fieldReevaluationDate;
    private DataDictionaryField m_fieldTestScoreTypeListening;
    private DataDictionaryField m_fieldTestScoreTypeReading;
    private DataDictionaryField m_fieldTestScoreTypeSpeaking;
    private DataDictionaryField m_fieldTestScoreTypeWriting;
    private DataDictionaryField m_fieldBasisofEntry;
    private DataDictionaryField m_fieldBasisofExitFirst;
    private DataDictionaryField m_fieldBasisofExitSecond;
    private DataDictionaryField m_fieldClassificationDate;
    private DataDictionaryField m_fieldExtensionofInstruction;
    private DataDictionaryField m_fieldFundSrc;
    private DataDictionaryField m_fieldProgParticipation;
    private DataDictionaryField m_fieldReclassificationDate;
    private DataDictionaryField m_fieldReclassificationExitDate;
    private DataDictionaryField m_fieldStudentPlanDate;
    private DataDictionaryField m_fieldTestContListening;
    private DataDictionaryField m_fieldTestContReading;
    private DataDictionaryField m_fieldTestContSpeaking;
    private DataDictionaryField m_fieldTestDateListening;
    private DataDictionaryField m_fieldTestDateReading;
    private DataDictionaryField m_fieldTestDateSpeaking;
    private DataDictionaryField m_fieldTestDateWriting;
    private DataDictionaryField m_fieldTestFormListening;
    private DataDictionaryField m_fieldTestFormReading;
    private DataDictionaryField m_fieldTestFormSpeaking;
    private DataDictionaryField m_fieldTestFormWriting;
    private DataDictionaryField m_fieldTestLevelListening;
    private DataDictionaryField m_fieldTestLevelReading;
    private DataDictionaryField m_fieldTestLevelSpeaking;
    private DataDictionaryField m_fieldTestLevelWriting;
    private DataDictionaryField m_fieldTestNameListening;
    private DataDictionaryField m_fieldTestNameReading;
    private DataDictionaryField m_fieldTestNameSpeaking;
    private DataDictionaryField m_fieldTestNameWriting;
    private DataDictionaryField m_fieldTestScoreListening;
    private DataDictionaryField m_fieldTestScoreReading;
    private DataDictionaryField m_fieldTestScoreSpeaking;
    private DataDictionaryField m_fieldTestScoreWriting;
    private DataDictionaryField m_fieldTestSubjectWriting;
    private DataDictionaryField m_fieldTierPlacement;

    private FLStudentHelper m_studentHelper;
    private StudentProgramDataset m_pgmEllDataset;

    private List<String> m_ellCodes = Arrays.asList("LF", "LY", "LP");

    private List<String> m_entryBasisCodes = Arrays.asList("A", "R", "T");

    private Random m_random = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        // for (int i = 0; i < 3; i++) {
        runAndValidateExports();
        super.execute();
        // runAndValidateExports();
        // }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FLExportConfiguration.FL_EXPORT.ELL, SisStudent.class);

        m_studentHelper = getFLReportData().getStudentHelper();

        m_pgmEllDataset = m_studentHelper.getStudentProgramDataset(DDX_ID, getFLReportData().getSurveyPeriod());
        m_dictionary = m_pgmEllDataset.getDataDictionary();

        m_random = new Random(System.currentTimeMillis());

        m_fieldPgmEllCode = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_ELL_CODE);
        m_fieldOrgDistrictNumber = getFLReportData().translateAliasToDictionaryField(ALIAS_ORG_DISTRICT_NUMBER, true);
        m_fieldProgParticipation = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PROG_PARTICIPATION);
        m_fieldTierPlacement = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TIER_PLACEMENT);
        m_fieldStudentPlanDate = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_STUDENT_PLAN_DATE);
        m_fieldClassificationDate = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_CLASSIFICATION_DATE);
        m_fieldFundSrc = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_FUND_SRC);
        m_fieldReevaluationDate = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_REEVALUATION_DATE);
        m_fieldExtensionofInstruction = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_EXT_OF_INSTRUCTION);
        m_fieldReclassificationDate = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_RECLASSIFICATION_DATE);
        m_fieldReclassificationExitDate = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_RECLASS_EXIT_DATE);
        m_fieldBasisofEntry = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_BASIS_OF_ENTRY);
        m_fieldBasisofExitFirst = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_BASIS_OF_EXIT_FIRST);
        m_fieldBasisofExitSecond = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_BASIS_OF_EXIT_SECOND);
        m_fieldTestNameListening = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_NAME_LISTENING);
        m_fieldTestScoreTypeListening = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_SC_T_LISTENING);
        m_fieldTestContListening = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_CONT_LISTENING);
        m_fieldTestScoreListening = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_SCORE_LISTENING);
        m_fieldTestDateListening = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_DATE_LISTENING);
        m_fieldTestNameSpeaking = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_NAME_SPEAKING);
        m_fieldTestScoreTypeSpeaking = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_SC_T_SPEAKING);
        m_fieldTestContSpeaking = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_CONT_SPEAKING);
        m_fieldTestScoreSpeaking = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_SCORE_SPEAKING);
        m_fieldTestDateSpeaking = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_DATE_SPEAKING);
        m_fieldTestNameReading = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_NAME_READING);
        m_fieldTestScoreTypeReading = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_SC_T_READING);
        m_fieldTestContReading = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_CONT_READING);
        m_fieldTestScoreReading = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_SCORE_READING);
        m_fieldTestDateReading = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_DATE_READING);
        m_fieldTestNameWriting = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_NAME_WRITING);
        m_fieldTestScoreTypeWriting = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_SC_T_WRITING);
        m_fieldTestSubjectWriting = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_SUBJECT_WRITING);
        m_fieldTestScoreWriting = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_SCORE_WRITING);
        m_fieldTestDateWriting = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_DATE_WRITING);
        m_fieldTestFormListening = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_FORM_LISTENING);
        m_fieldTestLevelListening = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_LEVEL_LISTENING);
        m_fieldTestFormSpeaking = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_FORM_SPEAKING);
        m_fieldTestLevelSpeaking = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_LEVEL_SPEAKING);
        m_fieldTestFormReading = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_FORM_READING);
        m_fieldTestLevelReading = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_LEVEL_READING);
        m_fieldTestFormWriting = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_FORM_WRITING);
        m_fieldTestLevelWriting = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_LEVEL_WRITING);

        // Collection<SisStudent> students =
        // getModelBroker().getCollectionByQuery(getFLReportData().getQuery());
        // for (SisStudent student : students) {
        // Collection<StudentProgramParticipation> pgms =
        // m_pgmEllDataset.getPrograms(student.getOid());
        // for (StudentProgramParticipation pgm : pgms) {
        // if (pgm.getFieldValueByBeanPath(m_fieldPgmEllCode.getJavaName()) == null
        // || pgm.getFieldValueByBeanPath(m_fieldPgmEllCode.getJavaName()).equals("LP")) {
        // pgm.setFieldValueByBeanPath(m_fieldPgmEllCode.getJavaName(), "LY");
        // getModelBroker().saveBean(pgm);
        // }
        // }
        // }

        initializeRule3Fix();
        initializeRule1Fix();
        initializeRule2BFix();
        initializeRule7Fix();
        initializeRule8Fix();
        initializeRule11Fix();
        initializeRule14Fix();
        initializeRule18Fix();
        initializeRule23Fix();
        initializeRule27Fix();
        initializeRule29Fix();
        initializeRule2HFix();
        initializeRule2IFix();
        initializeRule30Fix();
        initializeRule31Fix();
        initializeRule32Fix();
        initializeRule34Fix();
        initializeRule37Fix();
        initializeRule39Fix();
        initializeRule40Fix();
        initializeRule41Fix();
        initializeRule43Fix();
        initializeRule46Fix();
        initializeRule48Fix();
        initializeRule49Fix();
        initializeRule50Fix();
        initializeRule52Fix();
        initializeRule53Fix();
        initializeRule55Fix();
        initializeRule57Fix();
        initializeRule58Fix();
        initializeRule59Fix();
        initializeRule75Fix();
        initializeRule77Fix();
        initializeRule80Fix();
        initializeRule82Fix();
        initializeRule86Fix();
    }

    private StudentProgramParticipation getStudentProgramParticipation(SisStudent student) {
        StudentProgramParticipation pgm = null;

        if (m_pgmEllDataset == null || student == null) {
            System.out.println();
        }
        List<StudentProgramParticipation> list = m_pgmEllDataset.getPrograms(student.getOid());
        if (list != null && !list.isEmpty()) {
            pgm = list.get(list.size() - 1);
        }

        return pgm;
    }

    /**
     * Initialize rule 1 fix.
     */
    private void initializeRule1Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateDistrictNumber("District Number CIS"),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        SisOrganization organization1 = student.getOrganization1();
                        organization1.setFieldValueByBeanPath(m_fieldOrgDistrictNumber.getJavaName(),
                                String.valueOf(ThreadLocalRandom.current().nextLong(10, 69)));
                        getModelBroker().saveBean(organization1);
                    }
                }));

        addFixesByRuleNumber("1", ruleWithFixes);
    }

    /**
     * Initialize rule 2B fix.
     */
    private void initializeRule2BFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                Restriction.or(Restriction.pattern("Basis of Exit First", "^(F|G|R)$"),
                                        Restriction.pattern("Basis of Exit Second", "^(F|G|R)$")))
                                .testThen(Restriction.lessThan("Exit Date",
                                        FLEnglishLanguageLearnersValidation.generateDate("07", "01", "2008"))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                Restriction.or(Restriction.pattern("Basis of Exit First", "^A$"),
                                        Restriction.pattern("Basis of Exit Second", "^A$")))
                                .testThen(Restriction.lessThan("Exit Date",
                                        FLEnglishLanguageLearnersValidation.generateDate("07", "01", "2009"))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                Restriction.or(Restriction.pattern("Basis of Exit First", "^[B-E]$"),
                                        Restriction.pattern("Basis of Exit Second", "^[B-E]$")))
                                .testThen(Restriction.lessThan("Exit Date",
                                        FLEnglishLanguageLearnersValidation.generateDate("04", "07", "2012")))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldBasisofExitFirst.getJavaName(), null);
                            pgm.setFieldValueByBeanPath(m_fieldBasisofExitSecond.getJavaName(), null);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("2B", ruleWithFixes);
    }

    /**
     * Initialize rule 3 fix.
     */
    private void initializeRule3Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.pattern("Student Number", "^\\d{9}(\\d|X)$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number", "^\\d{10}$"))
                                        .testThen(Restriction.pattern("Student Number",
                                                "^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|7[8-9]).{8}$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number", "^\\d{9}X$"))
                                        .testThen(Restriction.pattern("Student Number", "^(?!000).{10}$"))),
                        "The first nine positions of Student Number Identifier, Florida must be "
                                + "numeric. The tenth position of Student Number Identifier, Florida must either be "
                                + "an \"X\" or numeric. If the tenth position of Student Number Identifier, Florida is "
                                + "numeric, the first two digits must be a valid district number in the range 01-68, 71-"
                                + "75 or 78-79. If the tenth position of the Student Number Identifier, Florida is an "
                                + "\"X\", the first three positions may not all be zeroes."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        String currentId = (String) bean.getFieldValueByAlias(ALIAS_STD_STATE_ID);
                        if (currentId == null) {
                            currentId = String.valueOf(ThreadLocalRandom.current().nextLong(1000000000l, 9999999999l));
                        }
                        currentId = StringUtils.padLeft(currentId, 10, '0');
                        String randomDistrict = "";
                        if (currentId.substring(9).matches("\\d")
                                && !currentId.substring(0, 2).matches("(?!(00|69|7[067]|[89]))\\d{2}")) {
                            while (StringUtils.isEmpty(randomDistrict)
                                    || !randomDistrict.matches("(?!(00|69|7[067]|[89]))\\d{2}")) {
                                randomDistrict = String.valueOf(ThreadLocalRandom.current().nextInt(79));
                            }
                        } else if (currentId.substring(9).matches("X")
                                && currentId.substring(0, 3).matches(("(?!(00|69|7[067]|[89])|0{3})\\d{3}"))) {
                            while (randomDistrict == null
                                    || !randomDistrict.matches("(?!(00|69|7[067]|[89])|0{3})\\d{3}")) {
                                randomDistrict = String.valueOf(ThreadLocalRandom.current().nextInt(999));
                            }
                        }

                        if (!StringUtils.isEmpty(randomDistrict)) {
                            currentId = randomDistrict + currentId.substring(2);
                        }
                        bean.setFieldValueByAlias(ALIAS_STD_STATE_ID, currentId);
                        getModelBroker().saveBean(bean);
                    }
                }));

        addFixesByRuleNumber("3", ruleWithFixes);
    }

    /**
     * Initialize rule 7 fix.
     */
    private void initializeRule7Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                        .testThen(Restriction.pattern("Basis of Entry", "^(A|R|L|T)$"))),
                        "If Survey Period Code is 2, 3 or 5, English Language Learners: Basis of "
                                + "Entry code must be A, R, L, or T."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldBasisofEntry.getJavaName(), getRandomCodeForField(m_fieldBasisofEntry));
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("7", ruleWithFixes);
    }

    /**
     * Initialize rule 8 fix.
     */
    private void initializeRule8Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Student Plan Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                Restriction.equals("Student Plan Date", "00000000")))),
                        "If Survey Period Code is 2 or 3, English Language Learners: Student Plan "
                                + "Date must be numeric and a valid date less than or equal to the survey date "
                                + "unless zero filled."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldStudentPlanDate.getJavaName(), "00000000");
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("8", ruleWithFixes);
    }

    /**
     * Initialize rule 11 fix.
     */
    private void initializeRule11Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[2|3]$"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Reevaluation Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                Restriction.equals("Reevaluation Date", "00000000")))),
                        "If Survey Period Code is 2 or 3, English Language Learners: Reevaluation"
                                + "Date must be numeric and a valid date less than or equal to the survey date "
                                + "unless zero filled."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldReevaluationDate.getJavaName(), "00000000");
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("11", ruleWithFixes);
    }

    /**
     * Initialize rule 14 fix.
     */
    private void initializeRule14Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Test Date Listening",
                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                Restriction.equals("Test Date Listening", "00000000"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                            long startLong = getFLReportData().getCurrentContext().getStartDate().getTime();
                            long endLong = getFLReportData().getSurveyPeriod().getDateCertain().getTime();
                            Date randomDate = new Date(ThreadLocalRandom.current().nextLong(startLong, endLong));
                            String value = sdf.format(randomDate);
                            pgm.setFieldValueByBeanPath(m_fieldTestDateListening.getJavaName(), value);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("14", ruleWithFixes);
    }

    /**
     * Initialize rule 18 fix.
     */
    private void initializeRule18Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.greaterThanOrEqualsFieldValue("Student Plan Date",
                                                "Entry Date", Date.class))),
                        "The English Language Learners: Student Plan Date must be a valid date "
                                + "and be greater than or equal to the English Language Learners: Entry Date."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                            String value = sdf.format(pgm.getStartDate());

                            pgm.setFieldValueByBeanPath(m_fieldStudentPlanDate.getJavaName(), value);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("18", ruleWithFixes);
    }

    /**
     * Initialize rule 23 fix.
     */
    private void initializeRule23Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.or(
                                                Restriction.byExtFldRefTable("FL-PGM-ELL", "pgm-test-cont-listening",
                                                        "Test Cont Listening"),
                                                Restriction.equals("Test Cont Listening", "ZZ")))),
                        "The Test Subject Content Listening must be a valid code specified in "
                                + "Appendix L of the DOE Information Data Base Requirements Volume I-Automated "
                                + "Student Information System manual or it may be ZZ."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldTestContListening.getJavaName(), "ZZ");
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("23", ruleWithFixes);
    }

    /**
     * Initialize rule 27 fix.
     */
    private void initializeRule27Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Test Sc T Listening",
                        "^(RS|SS|NP|AL|ZZ)$",
                        "Test Score Type Listening must be RS, SS, NP, AL or ZZ."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            String randomCode = null;
                            while (randomCode == null || !randomCode.matches("^(RS|SS|NP|AL|ZZ)$")) {
                                randomCode = getRandomCodeForField(m_fieldTestScoreTypeListening);
                            }
                            pgm.setFieldValueByBeanPath(m_fieldTestScoreTypeListening.getJavaName(), randomCode);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("27", ruleWithFixes);
    }

    /**
     * Initialize rule 29 fix.
     */
    private void initializeRule29Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Date Listening", "00000000"))
                                        .testThen(Restriction.and(Restriction.equals("Test Name Listening", "ZZZ"),
                                                Restriction.equals("Test Sc T Listening", "ZZ"),
                                                Restriction.equals("Test Cont Listening", "ZZ"),
                                                Restriction.equals("Test Score Listening", "0000")))),
                        "If Test Date Listening is equal to zero, then Test Name Listening must be "
                                + "ZZZ, the Test Score Type Listening must be ZZ, the Test Subject Content "
                                + "Listening must be ZZ, and the Test Score Listening must be zero."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                            long startLong = getFLReportData().getCurrentContext().getStartDate().getTime();
                            long endLong = getFLReportData().getSurveyPeriod().getDateCertain().getTime();
                            Date randomDate = new Date(ThreadLocalRandom.current().nextLong(startLong, endLong));
                            String value = sdf.format(randomDate);
                            pgm.setFieldValueByBeanPath(m_fieldTestDateListening.getJavaName(), value);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("29", ruleWithFixes);
    }

    /**
     * Initialize rule 2H fix.
     */
    private void initializeRule2HFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Basis of Entry", "A"),
                                        Restriction.equals("Exit Date", "00000000"))
                                        .testThen(Restriction.and(Restriction.notEquals("Test Name Listening", "ZZZ"),
                                                Restriction.notEquals("Test Name Speaking", "ZZZ")))),
                        "If English Language Learners: Basis of Entry is code A and If English "
                                + "Language Learners: Exit Date is 00000000 then the Test Name: Listening and "
                                + "Test Name: Speaking must be other than ZZZ."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            String value = getRandomCodeForField(m_fieldBasisofEntry);
                            if (value != null) {
                                while (value == "A" || value == "R") {
                                    value = getRandomCodeForField(m_fieldBasisofEntry);
                                }
                            }

                            pgm.setFieldValueByBeanPath(m_fieldBasisofEntry.getJavaName(), value);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("2H", ruleWithFixes);
    }

    /**
     * Initialize rule 2I fix.
     */
    private void initializeRule2IFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Basis of Entry", "R"),
                                        Restriction.equals("Exit Date", "00000000"))
                                        .testThen(Restriction.and(Restriction.notEquals("Test Name Reading", "ZZZ"),
                                                Restriction.notEquals("Test Name Writing", "ZZZ")))),
                        "If English Language Learners: Basis of Entry is code R and If English "
                                + "Language Learners: Exit Date is 00000000 then the Test Name: Reading and Test "
                                + "Name: Writing must be other than ZZZ."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            String value = getRandomCodeForField(m_fieldBasisofEntry);
                            if (value != null) {
                                while (value == "A" || value == "R") {
                                    value = getRandomCodeForField(m_fieldBasisofEntry);
                                }
                            }

                            pgm.setFieldValueByBeanPath(m_fieldBasisofEntry.getJavaName(), value);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("2I", ruleWithFixes);
    }

    /**
     * Initialize rule 30 fix.
     */
    private void initializeRule30Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("Test Date Listening", Double.valueOf(0)))
                                        .testThen(Restriction.notEquals("Test Sc T Listening", "ZZ"))),
                        "If Test Date Listening is greater than zero, then Test Score Type Listening "
                                + "must be a valid code other than ZZ."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldTestScoreTypeListening.getJavaName(),
                                    getRandomCodeForField(m_fieldTestScoreTypeListening));
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("30", ruleWithFixes);
    }

    /**
     * Initialize rule 31 fix.
     */
    private void initializeRule31Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("Test Date Listening", Double.valueOf(0)))
                                        .testThen(Restriction.notEquals("Test Cont Listening", "ZZ"))),
                        "If Test Date Listening is greater than zero, then Test Subject Content "
                                + "Listening must be a valid code other than ZZ."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldTestContListening.getJavaName(),
                                    getRandomCodeForField(m_fieldTestContListening));
                            getModelBroker().saveBean(pgm);
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
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Test Date Speaking",
                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                Restriction.equals("Test Date Speaking", "00000000"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                            long startLong = getFLReportData().getCurrentContext().getStartDate().getTime();
                            long endLong = getFLReportData().getSurveyPeriod().getDateCertain().getTime();
                            Date randomDate = new Date(ThreadLocalRandom.current().nextLong(startLong, endLong));
                            String value = sdf.format(randomDate);
                            pgm.setFieldValueByBeanPath(m_fieldTestDateSpeaking.getJavaName(), value);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("32", ruleWithFixes);
    }

    /**
     * Initialize rule 34 fix.
     */
    private void initializeRule34Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Test Sc T Speaking",
                        "^[RS|SS|NP|AL|ZZ]$",
                        "Test Score Type Speaking must be RS, SS, NP, AL or ZZ."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            String randomCode = null;
                            while (randomCode == null || !randomCode.matches("^(RS|SS|NP|AL|ZZ)$")) {
                                randomCode = getRandomCodeForField(m_fieldTestScoreTypeListening);
                            }
                            pgm.setFieldValueByBeanPath(m_fieldTestScoreTypeSpeaking.getJavaName(),
                                    randomCode);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("34", ruleWithFixes);
    }

    /**
     * Initialize rule 37 fix.
     */
    private void initializeRule37Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Date Speaking", "00000000"))
                                        .testThen(Restriction.and(Restriction.equals("Test Name Speaking", "ZZZ"),
                                                Restriction.equals("Test Sc T Speaking", "ZZ"),
                                                Restriction.equals("Test Cont Speaking", "ZZ"),
                                                Restriction.equals("Test Score Speaking", "0000")))),
                        "If Test Date Speaking is equal to zero, then Test Name Speaking must be "
                                + "ZZZ, the Test Score Type Speaking must be ZZ, the Test Subject Content "
                                + "Speaking must be ZZ, and the Test Score Speaking must be zero."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                            long startLong = getFLReportData().getCurrentContext().getStartDate().getTime();
                            long endLong = getFLReportData().getSurveyPeriod().getDateCertain().getTime();
                            Date randomDate = new Date(ThreadLocalRandom.current().nextLong(startLong, endLong));
                            String value = sdf.format(randomDate);
                            pgm.setFieldValueByBeanPath(m_fieldTestDateSpeaking.getJavaName(), value);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("37", ruleWithFixes);
    }

    /**
     * Initialize rule 39 fix.
     */
    private void initializeRule39Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.greaterThan("Test Date Speaking", Double.valueOf(0)))
                        .testThen(Restriction.notEquals("Test Sc T Speaking", "ZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldTestScoreTypeSpeaking.getJavaName(),
                                    getRandomCodeForField(m_fieldTestScoreTypeSpeaking));
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("39", ruleWithFixes);
    }

    /**
     * Initialize rule 40 fix.
     */
    private void initializeRule40Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.greaterThan("Test Date Speaking", Double.valueOf(0)))
                        .testThen(Restriction.notEquals("Test Cont Speaking", "ZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldTestContSpeaking.getJavaName(),
                                    getRandomCodeForField(m_fieldTestContSpeaking));
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("40", ruleWithFixes);
    }

    /**
     * Initialize rule 41 fix.
     */
    private void initializeRule41Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Test Date Reading",
                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                Restriction.equals("Test Date Reading", "00000000"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                            long startLong = getFLReportData().getCurrentContext().getStartDate().getTime();
                            long endLong = getFLReportData().getSurveyPeriod().getDateCertain().getTime();
                            Date randomDate = new Date(ThreadLocalRandom.current().nextLong(startLong, endLong));
                            String value = sdf.format(randomDate);
                            pgm.setFieldValueByBeanPath(m_fieldTestDateReading.getJavaName(), value);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("41", ruleWithFixes);
    }

    /**
     * Initialize rule 43 fix.
     */
    private void initializeRule43Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Test Sc T Reading",
                        "^(RS|SS|NP|AL|ZZ)$",
                        "Test Score Type Reading must be RS, SS, NP, AL or ZZ."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            String programCode = (String) pgm.getFieldValueByBeanPath(m_fieldPgmEllCode.getJavaName());
                            if (!"FY".equals(programCode)) {
                                String randomCode = null;
                                while (randomCode == null || !randomCode.matches("^(RS|SS|NP|AL)$")) {
                                    randomCode = getRandomCodeForField(m_fieldTestScoreTypeReading);
                                }
                                pgm.setFieldValueByBeanPath(m_fieldTestScoreTypeReading.getJavaName(),
                                        randomCode);
                            } else {
                                pgm.setFieldValueByBeanPath(m_fieldTestScoreTypeReading.getJavaName(), "ZZ");
                            }
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("43", ruleWithFixes);
    }

    /**
     * Initialize rule 46 fix.
     */
    private void initializeRule46Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Test Date Reading", "00000000"))
                        .testThen(Restriction.and(Restriction.equals("Test Name Reading", "ZZZ"),
                                Restriction.equals("Test Sc T Reading", "ZZ"),
                                Restriction.equals("Test Cont Reading", "ZZ"),
                                Restriction.equals("Test Score Reading", "0000"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                            long startLong = getFLReportData().getCurrentContext().getStartDate().getTime();
                            long endLong = getFLReportData().getSurveyPeriod().getDateCertain().getTime();
                            Date randomDate = new Date(ThreadLocalRandom.current().nextLong(startLong, endLong));
                            String value = sdf.format(randomDate);
                            pgm.setFieldValueByBeanPath(m_fieldTestDateReading.getJavaName(), value);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("46", ruleWithFixes);
    }

    /**
     * Initialize rule 48 fix.
     */
    private void initializeRule48Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.greaterThan("Test Date Reading", Double.valueOf(0)))
                        .testThen(Restriction.notEquals("Test Sc T Reading", "ZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            String randomCode = null;
                            while (randomCode == null || randomCode.matches("^ZZ$")) {
                                randomCode = getRandomCodeForField(m_fieldTestScoreTypeReading);
                            }
                            String ellCode = (String) pgm.getFieldValueByBeanPath(m_fieldPgmEllCode.getJavaName());
                            if ("LY".equals(ellCode)) {
                                pgm.setFieldValueByBeanPath(m_fieldTestDateReading.getJavaName(), null);
                                pgm.setFieldValueByBeanPath(m_fieldTestScoreReading.getJavaName(), null);
                                pgm.setFieldValueByBeanPath(m_fieldTestNameReading.getJavaName(), null);
                                pgm.setFieldValueByBeanPath(m_fieldTestScoreTypeReading.getJavaName(), null);
                                pgm.setFieldValueByBeanPath(m_fieldTestContReading.getJavaName(), null);
                                getModelBroker().saveBean(pgm);
                            } else {
                                pgm.setFieldValueByBeanPath(m_fieldTestScoreTypeReading.getJavaName(), randomCode);
                            }
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("48", ruleWithFixes);
    }

    /**
     * Initialize rule 49 fix.
     */
    private void initializeRule49Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.greaterThan("Test Date Reading", Double.valueOf(0)))
                        .testThen(Restriction.notEquals("Test Cont Reading", "ZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            String randomCode = null;
                            while (randomCode == null || randomCode.matches("^ZZ$")) {
                                randomCode = getRandomCodeForField(m_fieldTestContReading);
                            }
                            pgm.setFieldValueByBeanPath(m_fieldTestContReading.getJavaName(), randomCode);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("49", ruleWithFixes);
    }

    /**
     * Initialize rule 50 fix.
     */
    private void initializeRule50Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Test Date Writing",
                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                Restriction.equals("Test Date Writing", "00000000"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                            long startLong = getFLReportData().getCurrentContext().getStartDate().getTime();
                            long endLong = getFLReportData().getSurveyPeriod().getDateCertain().getTime();
                            Date randomDate = new Date(ThreadLocalRandom.current().nextLong(startLong, endLong));
                            String value = sdf.format(randomDate);
                            pgm.setFieldValueByBeanPath(m_fieldTestDateWriting.getJavaName(), value);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("50", ruleWithFixes);
    }

    /**
     * Initialize rule 52 fix.
     */
    private void initializeRule52Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Test Sc T Writing",
                        "^[RS|SS|NP|AL|ZZ]$",
                        "Test Score Type Writing must be RS, SS, NP, AL or ZZ."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldTestScoreTypeWriting.getJavaName(), "ZZ");
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("52", ruleWithFixes);
    }

    /**
     * Initialize rule 53 fix.
     */
    private void initializeRule53Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.or(
                                Restriction.byExtFldRefTable("FL-PGM-ELL", "pgm-test-subject-writing",
                                        "Test Subject Writing"),
                                Restriction.equals("Test Subject Writing", "ZZ"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldTestSubjectWriting.getJavaName(),
                                    getRandomCodeForField(m_fieldTestSubjectWriting));
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("53", ruleWithFixes);
    }

    /**
     * Initialize rule 55 fix.
     */
    private void initializeRule55Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Test Date Writing", "00000000"))
                        .testThen(Restriction.and(Restriction.equals("Test Name Writing", "ZZZ"),
                                Restriction.equals("Test Sc T Writing", "ZZ"),
                                Restriction.equals("Test Subject Writing", "ZZ"),
                                Restriction.equals("Test Score Writing", "0000"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                            long startLong = getFLReportData().getCurrentContext().getStartDate().getTime();
                            long endLong = getFLReportData().getSurveyPeriod().getDateCertain().getTime();
                            Date randomDate = new Date(ThreadLocalRandom.current().nextLong(startLong, endLong));
                            String value = sdf.format(randomDate);
                            pgm.setFieldValueByBeanPath(m_fieldTestDateWriting.getJavaName(), value);
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("55", ruleWithFixes);
    }

    /**
     * Initialize rule 57 fix.
     */
    private void initializeRule57Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.greaterThan("Test Date Writing", Double.valueOf(0)))
                        .testThen(Restriction.notEquals("Test Sc T Writing", "ZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            String randomCode = null;
                            while (randomCode == null || randomCode.matches("^ZZ$")) {
                                randomCode = getRandomCodeForField(m_fieldTestScoreTypeWriting);
                            }
                            String ellCode = (String) pgm.getFieldValueByBeanPath(m_fieldPgmEllCode.getJavaName());
                            if ("LY".equals(ellCode)) {
                                pgm.setFieldValueByBeanPath(m_fieldTestDateWriting.getJavaName(), null);
                                pgm.setFieldValueByBeanPath(m_fieldTestScoreWriting.getJavaName(), null);
                                pgm.setFieldValueByBeanPath(m_fieldTestNameWriting.getJavaName(), null);
                                pgm.setFieldValueByBeanPath(m_fieldTestScoreTypeWriting.getJavaName(), null);
                                pgm.setFieldValueByBeanPath(m_fieldTestSubjectWriting.getJavaName(), null);
                                getModelBroker().saveBean(pgm);
                            } else {
                                pgm.setFieldValueByBeanPath(m_fieldTestScoreTypeWriting.getJavaName(), randomCode);
                            }
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("57", ruleWithFixes);
    }

    /**
     * Initialize rule 58 fix.
     */
    private void initializeRule58Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.greaterThan("Test Date Writing", Double.valueOf(0)))
                        .testThen(Restriction.notEquals("Test Subject Writing", "ZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldTestSubjectWriting.getJavaName(),
                                    getRandomCodeForField(m_fieldTestSubjectWriting));
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("58", ruleWithFixes);
    }

    /**
     * Initialize rule 59 fix.
     */
    private void initializeRule59Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Student ID Local",
                        "^[A-Za-z0-9\\s]{10}$",
                        "Student Number Identifier, Local may be any combination of letters, numbers and blanks. "
                                + "(All blanks are allowable.) It must be left-justified with trailing blanks."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        student.setLocalId(String.valueOf(ThreadLocalRandom.current().nextLong(1000000000l, 9999999999l)));
                        getModelBroker().saveBean(student);
                    }
                }));

        addFixesByRuleNumber("59", ruleWithFixes);
    }

    /**
     * Initialize rule 75 fix.
     */
    private void initializeRule75Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number CIS", "District Number, E"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Pattern.compile("^(LY|LP|LF)$"), "ELL"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldPgmEllCode.getJavaName(), m_random.nextBoolean() ? "LY" : "LF");
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("75", ruleWithFixes);
    }

    /**
     * Initialize rule 77 fix.
     */
    private void initializeRule77Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                        Restriction.equals("Basis of Exit First", "Z"),
                        Restriction.equals("Basis of Exit Second", "Z"))
                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair(Pattern.compile("^(LP|LY)$"), "ELL"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        String ellCode = (String) pgm.getFieldValueByBeanPath(m_fieldPgmEllCode.getJavaName());
                        if (!"LY".equals(ellCode)) {
                            pgm.setFieldValueByBeanPath(m_fieldBasisofEntry.getJavaName(), getRandomCodeForField(m_fieldBasisofEntry));
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("77", ruleWithFixes);
    }

    /**
     * Initialize rule 80 fix.
     */
    private void initializeRule80Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "2"),
                        Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Pattern.compile("^LY$"), "ELL"),
                                new KeyValuePair(Pattern.compile("^(0[1-9]|10|1[1-2])$"),
                                        "Grade Level")))
                        .testThen(Restriction.pattern("Tier Placement", "^[A-D]$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldTierPlacement.getJavaName(), getRandomCodeForField(m_fieldTierPlacement));
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("80", ruleWithFixes);
    }

    /**
     * Initialize rule 82 fix.
     */
    private void initializeRule82Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.notEquals("Classification Date", "00000000"))
                                        .testThen(Restriction.greaterThanOrEquals("Classification Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)))),
                        "English Language Learners: Classification Date must be greater than or "
                                + "equal to the English Language Learners: Home Language Survey Date, unless "
                                + "the ELL: Classification Date is zero filled."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldClassificationDate.getJavaName(), "00000000");
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("82", ruleWithFixes);
    }

    /**
     * Initialize rule 86 fix.
     */
    private void initializeRule86Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                        Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number CIS", "District Number, E"),
                                new KeyValuePair(Pattern.compile("^LY$"), "ELL")),
                        Restriction.pattern("Basis of Entry", "^(A|R|T)$"))
                        .testThen(Restriction.or(Restriction.notEquals("Test Name Listening", "ZZZ"),
                                Restriction.notEquals("Test Name Reading", "ZZZ"),
                                Restriction.notEquals("Test Name Writing", "ZZZ"),
                                Restriction.notEquals("Test Name Speaking", "ZZZ"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        StudentProgramParticipation pgm = getStudentProgramParticipation(student);
                        if (pgm != null) {
                            pgm.setFieldValueByBeanPath(m_fieldBasisofEntry.getJavaName(), m_entryBasisCodes.get(m_random.nextInt(2)));
                            pgm.setFieldValueByBeanPath(m_fieldTestNameListening.getJavaName(),
                                    getRandomCodeForField(m_fieldTestNameListening));
                            pgm.setFieldValueByBeanPath(m_fieldTestNameReading.getJavaName(),
                                    getRandomCodeForField(m_fieldTestNameReading));
                            pgm.setFieldValueByBeanPath(m_fieldTestNameSpeaking.getJavaName(),
                                    getRandomCodeForField(m_fieldTestNameSpeaking));
                            pgm.setFieldValueByBeanPath(m_fieldTestNameWriting.getJavaName(),
                                    getRandomCodeForField(m_fieldTestNameWriting));
                            getModelBroker().saveBean(pgm);
                        }
                    }
                }));

        addFixesByRuleNumber("86", ruleWithFixes);
    }
}
