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

package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentEnrollmentSpanInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetProcedureSEYS.
 */
public class SetProcedureSEYS extends SetProcedure {

    private static final String ALIAS_DIPLOMA_TYPE = "all-std-DiplomaType";
    private static final String ALIAS_NINTH_GRADE = "all-std-NinthGradeYear";
    private static final String ALIAS_PARAM_EXIT_OPT_TEST = "pgm-drp-exit-opt-test";
    private static final String ALIAS_STD_COMP_CERT = "all-std-CertificateOfCompletion";
    private static final String ALIAS_STD_GRADE_PROMOTION = "all-std-GradePromotionStatus";
    private static final String ALIAS_STD_GRAD_OPTION = "all-std-GraduationOption";
    private static final String DDX_ID = "FL-PGM-DROP";

    private DataDictionary m_dictionary = null;
    private DataDictionaryField m_fieldComplCert = null;
    private DataDictionaryField m_fieldDiplomaType = null;
    private DataDictionaryField m_fieldEnrollmentCode;
    private DataDictionaryField m_fieldExitOptTest;

    private DataDictionaryField m_fieldGradePromotionStatus = null;
    private DataDictionaryField m_fieldGradOption = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        super.execute();
        runAndValidateExports();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FLExportConfiguration.FL_EXPORT.SEYS, SisStudent.class);
        // includeExports(Arrays.asList(FL_EXPORT.DRP));
        getStudentHelper().setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                getCurrentContext().getStartDate());

        ModelProperty prop = new ModelProperty(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE,
                getFLReportData().getBroker().getPersistenceKey());
        m_fieldEnrollmentCode = getFLReportData().getDataDictionary().findDataDictionaryField(prop.getFieldId());

        m_fieldComplCert = getFLReportData().translateAliasToDictionaryField(ALIAS_STD_COMP_CERT, true);
        m_fieldDiplomaType = getFLReportData().translateAliasToDictionaryField(ALIAS_DIPLOMA_TYPE, true);
        m_fieldGradePromotionStatus = getFLReportData()
                .translateAliasToDictionaryField(ALIAS_STD_GRADE_PROMOTION, true);
        m_fieldGradOption = getFLReportData()
                .translateAliasToDictionaryField(ALIAS_STD_GRAD_OPTION, true);

        StudentProgramDataset pgmDatasetLunch =
                getFLReportData().getStudentHelper().getStudentProgramDataset(DDX_ID,
                        getFLReportData().getSurveyPeriod());

        m_fieldExitOptTest = getFLReportData().translateAliasToDictionaryField(
                pgmDatasetLunch.getDataDictionary(),
                ALIAS_PARAM_EXIT_OPT_TEST, true);

        X2Criteria ddxCriteria = new X2Criteria();
        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_ID);
        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        m_dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());

        initializeRule10Fix();
        initializeRule17Fix();
        initializeRule8Fix();
        initializeRule26Fix();
        initializeRule14Fix();
        initializeRule3UFix();
        initializeRule22Fix();
        initializeRule4HFix();
        initializeRule4IFix();
        initializeRule3CFix();
        initializeRule3FFix();
        initializeRule3KFix();
        initializeRule3SFix();
        initializeRule32Fix();
        initializeRule33Fix();
        initializeRule34Fix();
        initializeRule44Fix();
    }

    private StudentEnrollment getWithdrawalEnrollment(SisStudent student) {
        StudentEnrollment withdrawalEnr = null;

        try {
            if (getFLReportData().getStudentHelper().isStudentEligible(student)) {
                List<StudentEnrollmentSpan> spans = getFLReportData().getStudentHelper()
                        .getStudentEnrollmentSpans(student, true);
                StudentEnrollmentSpan lastSpan = null;
                for (StudentEnrollmentSpan span : spans) {
                    if (span.getFirstActiveDate() != null && !span.getFirstActiveDate()
                            .after(getFLReportData().getSurveyPeriod().getSnapshotDate())) {
                        if (lastSpan == null ||
                                !lastSpan.getFirstActiveDate().after(span.getFirstActiveDate())) {
                            lastSpan = span;
                        }
                    }
                    if (lastSpan != null) {
                        StudentEnrollmentSpanInfo info =
                                getFLReportData().getStudentHelper().new StudentEnrollmentSpanInfo(student,
                                        lastSpan);
                        if (info != null) {
                            withdrawalEnr = info.getWithdrawalEnrollment();
                        }
                    }
                    if (withdrawalEnr == null) {
                        withdrawalEnr = span.getFirstInactiveEnrollment();
                    }
                }
            }
        } catch (X2BaseException e) {
            e.printStackTrace();
        }
        return withdrawalEnr;
    }

    /**
     * Initialize rule 14 fix.
     */
    private void initializeRule10Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Completion Cert Type", "^(W08|W8A|W8B|W09|ZZZ)$",
                        "Certificate of Completion, Type must be W08, W8A, W8B, W09 or ZZZ"),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByBeanPath(m_fieldComplCert.getJavaName(),
                                getRandomCodeForFieldByPattern(m_fieldComplCert, "^(W08|W8A|W8B|W09|ZZZ)$"));
                        getModelBroker().saveBeanForced(bean);
                    }
                }));
        addFixesByRuleNumber("14", ruleWithFixes);
    }

    /**
     * Initialize rule 14 fix.
     */
    private void initializeRule14Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Grade Level", "^(PK|KG|0[1-8])$"))
                                .testThen(Restriction.equals("Year Entered 9 Grade", "00000000")),
                        ValidationRule.testIf(Restriction.pattern("Grade Level", "^(09|1[0-2])$"))
                                .testThen(Restriction.greaterThan("Year Entered 9 Grade", Double.valueOf(0)))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        if (student != null) {
                            String ninthGrade = null;
                            String gradeLevel = student.getGradeLevel();
                            Matcher matcher = Pattern.compile("^(PK|KG|0[1-8])$").matcher(gradeLevel);
                            if (!matcher.find()) {
                                ninthGrade = String.valueOf(
                                        ThreadLocalRandom.current().nextInt(2009, 2012));
                            }
                            bean.setFieldValueByAlias(ALIAS_NINTH_GRADE, ninthGrade);
                            getModelBroker().saveBeanForced(bean);
                        }
                    }
                }));
        addFixesByRuleNumber("14", ruleWithFixes);
    }

    /**
     * Initialize rule 17 fix.
     */
    private void initializeRule17Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Grade Level", "^(PK|KG|0[1-9]|1[0-1]|3[01])$",
                        "Grade Level code must be PK, KG, 01-12, 30 or 31."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // SisStudent student = (SisStudent) bean;
                        // if (student.getGradeLevel().contentEquals("K")) {
                        // student.setGradeLevel("KG");
                        // getModelBroker().saveBean(student);
                        // }
                        // if (student.getGradeLevel().contentEquals("P4")) {
                        // student.setGradeLevel("04");
                        // getModelBroker().saveBean(student);
                        // }
                        // if (student.getGradeLevel().contentEquals("P3")) {
                        // student.setGradeLevel("03");
                        // getModelBroker().saveBean(student);
                        // }
                        // // rule 3K fix
                        // if (student.getGradeLevel().contentEquals("12")) {
                        // student.setGradeLevel("11");
                        // getModelBroker().saveBean(student);
                        // }
                    }
                }));
        addFixesByRuleNumber("17", ruleWithFixes);
    }

    /**
     * Initialize rule 22 fix.
     */
    private void initializeRule22Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Withdrawal Reason", "DNE"))
                        .testThen(Restriction.pattern("Grade Promotion", "^(A|D|P|R)$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        student.setFieldValueByBeanPath(
                                m_fieldGradePromotionStatus.getJavaName(),
                                getRandomCodeForFieldByPattern(m_fieldGradePromotionStatus, "^(A|D|P|R)$"));
                        getModelBroker().saveBeanForced(student);
                    }
                }));
        addFixesByRuleNumber("22", ruleWithFixes);
    }

    /**
     * Initialize rule 26 fix.
     */
    private void initializeRule26Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("District Number", "71"),
                                Restriction.and(
                                        Restriction.pattern("School Number", "^0[34]00$"),
                                        Restriction.equals("District Number", "71")))
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Withdrawal Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                                        Restriction.lessThanOrEquals("Withdrawal Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")))),
                        ValidationRule.testIf(Restriction.and(Restriction.equals("District Number", "71"),
                                Restriction.pattern("School Number", "^0[567]00$")))
                                .testThen(Restriction.equals("Withdrawal Date", "00000000"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;

                        Date withdrawalDate =
                                (Date) (new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "02"))
                                        .getRuntimeObject(getHelper());
                        StudentEnrollment withdrawalEnr = getWithdrawalEnrollment(student);
                        if (withdrawalEnr != null) {
                            withdrawalEnr.setEnrollmentDate(new PlainDate(withdrawalDate));
                            getModelBroker().saveBeanForced(withdrawalEnr);
                        }
                    }
                }));
        addFixesByRuleNumber("26", ruleWithFixes);
    }

    /**
     * Initialize rule 32 fix.
     */
    private void initializeRule32Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Diploma Type",
                        "^(W06|W07|W10|W27|WD1|WFT|WFW|WGA|WGD|WRW|WXL|WXT|WXW)$"))
                        .testThen(Restriction.pattern("Grade Level", "^(09|1[0-2])$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByBeanPath(m_fieldDiplomaType.getJavaName(),
                                getRandomCodeForFieldByPattern(m_fieldDiplomaType,
                                        "^(?!W06|W07|W10|W27|WD1|WFT|WFW|WGA|WGD|WRW|WXL|WXT|WXW).{3}$"));
                        getModelBroker().saveBeanForced(bean);
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
                ValidationRule.testIf(Restriction.pattern("Completion Cert Type", "^(W08|W8A|W09|W8B)$"))
                        .testThen(Restriction.pattern("Grade Level", "^(09|1[0-2])$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByBeanPath(m_fieldComplCert.getJavaName(),
                                getRandomCodeForFieldByPattern(m_fieldComplCert,
                                        "^(?!W08|W8A|W09|W8B).*$"));
                        getModelBroker().saveBeanForced(bean);
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
                ValidationRule.testIf(Restriction.notEquals("School Number", "0600"),
                        Restriction.notEquals("District Number", "71"))
                        .testThen(Restriction.or(
                                Restriction.and(
                                        Restriction.notEquals("Diploma Type", "ZZZ"),
                                        Restriction.equals("Completion Cert Type", "ZZZ"),
                                        Restriction.equals("Withdrawal Reason", "ZZZ")),
                                Restriction.and(
                                        Restriction.equals("Diploma Type", "ZZZ"),
                                        Restriction.notEquals("Completion Cert Type", "ZZZ"),
                                        Restriction.equals("Withdrawal Reason", "ZZZ")),
                                Restriction.and(
                                        Restriction.equals("Diploma Type", "ZZZ"),
                                        Restriction.equals("Completion Cert Type", "ZZZ"),
                                        Restriction.notEquals("Withdrawal Reason", "ZZZ")))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        String wdrawReason = getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.SEYS,
                                "Withdrawal Reason");
                        if (wdrawReason != null && !wdrawReason.equals("ZZZ")) {
                            bean.setFieldValueByBeanPath(m_fieldDiplomaType.getJavaName(), "ZZZ");
                            bean.setFieldValueByBeanPath(m_fieldComplCert.getJavaName(), "ZZZ");
                        } else {
                            bean.setFieldValueByBeanPath(m_fieldDiplomaType.getJavaName(), "ZZZ");
                            bean.setFieldValueByBeanPath(m_fieldComplCert.getJavaName(), "ZZZ");
                            int choice = ThreadLocalRandom.current().nextInt(0, 2);
                            switch (choice) {
                                case 0:
                                    bean.setFieldValueByBeanPath(m_fieldDiplomaType.getJavaName(),
                                            getRandomCodeForFieldByPattern(m_fieldDiplomaType, "(?!ZZZ).{3}"));
                                    break;
                                case 1:
                                    bean.setFieldValueByBeanPath(m_fieldComplCert.getJavaName(),
                                            getRandomCodeForFieldByPattern(m_fieldComplCert, "(?!ZZZ).{3}"));
                                    break;

                                default:
                                    break;
                            }
                        }
                        getModelBroker().saveBeanForced(bean);
                    }
                }));
        addFixesByRuleNumber("34", ruleWithFixes);
    }

    /**
     * Initialize rule 3C fix.
     */
    private void initializeRule3CFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Diploma Type", "^(W43|W45|W52|W54|W55|W57|W58)$"))
                        .testThen(Restriction.pattern("Grade Level", "^(3[01])$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByBeanPath(m_fieldDiplomaType.getJavaName(),
                                getRandomCodeForFieldByPattern(m_fieldDiplomaType,
                                        "^(?!W43|W45|W52|W54|W55|W57|W58).{3}$"));
                        getModelBroker().saveBeanForced(bean);
                    }
                }));
        addFixesByRuleNumber("3C", ruleWithFixes);
    }

    /**
     * Initialize rule 3F fix.
     */
    private void initializeRule3FFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Withdrawal Reason", "WPO"))
                        .testThen(Restriction.equals("Grade Level", "12")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        /* No Fix needed */ }
                }));
        addFixesByRuleNumber("3F", ruleWithFixes);
    }

    /**
     * Initialize rule 3K fix.
     */
    private void initializeRule3KFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Grade Promotion", "P"),
                        Restriction.equals("Grade Level", "12"))
                        .testThen(Restriction.or(
                                Restriction.notEquals("Completion Cert Type", "ZZZ"),
                                Restriction.equals("Withdrawal Reason", "WPO"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        StudentEnrollment enr = getWithdrawalEnrollment((SisStudent) bean);
                        String type = enr.getEnrollmentType();
                        String reason = enr.getEnrollmentCode();
                        if ("W".equals(type) && !"WPO".equals(reason)) {
                            enr.setEnrollmentCode("WPO");
                            getModelBroker().saveBeanForced(enr);
                        }
                    }
                }));
        addFixesByRuleNumber("3K", ruleWithFixes);
    }

    /**
     * Initialize rule 3S fix.
     */
    private void initializeRule3SFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                        Restriction.or(
                                Restriction.notEquals("Completion Cert Type", "ZZZ"),
                                Restriction.notEquals("Diploma Type", "ZZZ"),
                                Restriction.equals("Withdrawal Reason", "WPO")))
                        .testThen(Restriction.equals("Grade Promotion", "P")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByBeanPath(m_fieldGradePromotionStatus.getJavaName(), "P");
                        getModelBroker().saveBeanForced(bean);
                    }
                }));
        addFixesByRuleNumber("3S", ruleWithFixes);
    }

    /**
     * Initialize rule 3U fix.
     */
    private void initializeRule3UFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Withdrawal Reason", "W26"))
                        .testThen(Restriction.pattern("Grade Level", "^(0[6-9]|1[0-2]|3[01])$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // SisStudent student = (SisStudent) bean;
                        // student.setGradeLevel("31");
                        // getModelBroker().saveBeanForced(student);
                    }
                }));
        addFixesByRuleNumber("3U", ruleWithFixes);
    }

    /**
     * Initialize rule 4H fix.
     */
    private void initializeRule4HFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Drp Exit Option Test", "^(F|P)$"))
                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number", "District Number, I/S"),
                                new KeyValuePair("District Number", "District Number, E"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.equals("Graduation Option", "8"), null))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {

                            getFLReportData().getStudentHelper().getStudentProgram(bean.getOid(), DDX_ID,
                                    getFLReportData().getSurveyPeriod());
                            // pgm.setFieldValueByBeanPath(m_fieldExitOptTest.getJavaName(), "Z");
                            // getBroker().saveBean(pgm, m_dictionary);
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("4H", ruleWithFixes);
    }

    /**
     * Initialize rule 4I fix.
     */
    private void initializeRule4IFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Drp Exit Option Test", "^(F|P)$"))
                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number", "District Number, I/S"),
                                new KeyValuePair("District Number", "District Number, E"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.equals("Graduation Option", "8"), null))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByBeanPath(m_fieldGradOption.getJavaName(), "8");
                        getModelBroker().saveBeanForced(bean);
                    }
                }));
        addFixesByRuleNumber("4I", ruleWithFixes);
    }

    /**
     * Initialize rule 44 fix.
     */
    private void initializeRule44Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new RuleSet(
                        ValidationRule.testIf(Restriction.validateNotMatchInExport("EXPDATA-FL-EXCEPT",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number", "District Number"),
                                new KeyValuePair("School Number", "School Number"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                .testThen(Restriction.pattern("Diploma Type", "^(?!W[FW|RW|XW])\\S{3}$")),
                        ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number", "District Number"),
                                new KeyValuePair("School Number", "School Number"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.equals("Except Primary", "L"), null),
                                new KeyValuePair(Restriction.equals("Exceptionality Other",
                                        "ZZZZZZZZZ"), null)))
                                .testThen(Restriction.pattern("Diploma Type", "^(?!W[FW|RW|XW])\\S{3}$"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByBeanPath(m_fieldDiplomaType.getJavaName(),
                                getRandomCodeForFieldByPattern(m_fieldDiplomaType,
                                        "^(?!W[FW|RW|XW])\\S{3}$"));
                        getModelBroker().saveBeanForced(bean);
                    }
                }));
        addFixesByRuleNumber("44", ruleWithFixes);
    }

    /**
     * Initialize rule 8 fix.
     */
    private void initializeRule8Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Grade Promotion", "^(A|D|P|R|N|Z)$",
                        "Grade Promotion Status must be A, D, P, R, N or Z."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        student.setFieldValueByBeanPath(
                                m_fieldGradePromotionStatus.getJavaName(),
                                getRandomCodeForFieldByPattern(m_fieldGradePromotionStatus, "^(A|D|P|R|N|Z)$"));
                        // student.setGradeLevel("PK");
                        getModelBroker().saveBeanForced(student);
                    }

                }));
        addFixesByRuleNumber("8", ruleWithFixes);
    }

}
