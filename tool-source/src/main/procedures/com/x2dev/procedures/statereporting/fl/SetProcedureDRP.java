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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

/**
 * @author Follett Software Company
 * @copyright 2018
 */
public class SetProcedureDRP extends SetProcedure {

    private static final String ALIAS_FUND_SOURCE = "pgm-drp-fund-src";

    private static final String ALIAS_PGM_ACTIONS_TAKEN = "pgm-drp-actions-taken";
    private static final String ALIAS_PGM_CODE = "pgm-dropout-code";
    private static final String ALIAS_PGM_EXIT_OP_TEST = "pgm-drp-exit-opt-test";
    private static final String ALIAS_PGM_POS_INFLUENCES = "pgm-drp-pos-influences";
    private static final String ALIAS_PGM_PRETEST_MATH = "pgm-pretest-math";
    private static final String ALIAS_PGM_PRETEST_READING = "pgm-pretest-reading";
    private static final String ALIAS_PGM_PRIMARY_REASON = "pgm-drp-primary-reason";
    private static final String ALIAS_PGM_PROGRESS_MATH = "pgm-progress-math";
    private static final String ALIAS_PGM_PROGRESS_READING = "pgm-progress-reading";
    private static final String ALIAS_PGM_SECONDARY_REASON = "pgm-drp-secondary-reason";
    private static final String ALIAS_PGM_TERM = "pgm-term-code";

    private static final String ALIAS_STD_STATE_ID = "all-std-StateId";

    private static final String PROGRAM_CODE_DROPOUT = "DROPOUT";

    private DateAsStringConverter m_dateConverter;

    private DataDictionary m_dictionary = null;

    private DataDictionaryField m_fieldActionsTaken = null;
    private DataDictionaryField m_fieldCode = null;
    private DataDictionaryField m_fieldExitOpTest = null;
    private DataDictionaryField m_fieldFundSource = null;
    private DataDictionaryField m_fieldPosInfluences = null;
    private DataDictionaryField m_fieldPretestMath = null;
    private DataDictionaryField m_fieldPretestReading = null;
    private DataDictionaryField m_fieldPrimaryReason = null;
    private DataDictionaryField m_fieldProgressMath = null;
    private DataDictionaryField m_fieldProgressReading = null;
    private DataDictionaryField m_fieldSecondaryReason = null;
    private DataDictionaryField m_fieldTerm = null;

    private RuntimeParam m_fiscalDateStart = null;
    private RuntimeParam m_fiscalDateEnd = null;

    private Random m_random;

    /**
     * @see com.x2dev.procedures.statereporting.fl.SetProcedure#execute()
     */
    @Override
    protected void execute() throws Exception {
        addPrograms();
        for (int i = 0; i < 6; i++) {
            runAndValidateExports();
            super.execute();
        }
        runAndValidateExports();
    }

    /**
     * @see com.x2dev.procedures.statereporting.fl.SetProcedure#initialize(com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
     *      java.lang.Class)
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FL_EXPORT.DRP, SisStudent.class);

        m_dateConverter = (DateAsStringConverter) ConverterFactory
                .getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true);

        m_dictionary = ((FLDropoutPreventionProgramData) getFLReportData()).m_pgmDataset.getDataDictionary();

        m_random = new Random(System.currentTimeMillis());

        m_fiscalDateStart = new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01");
        m_fiscalDateEnd = new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31");

        m_fieldFundSource = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_FUND_SOURCE);
        m_fieldActionsTaken = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_ACTIONS_TAKEN);
        m_fieldCode = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_CODE);
        m_fieldExitOpTest = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_EXIT_OP_TEST);
        m_fieldPosInfluences = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_POS_INFLUENCES);
        m_fieldPretestMath = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_PRETEST_MATH);
        m_fieldPretestReading = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_PRETEST_READING);
        m_fieldPrimaryReason = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_PRIMARY_REASON);
        m_fieldProgressMath = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_PROGRESS_MATH);
        m_fieldProgressReading = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_PROGRESS_READING);
        m_fieldSecondaryReason = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_SECONDARY_REASON);
        m_fieldTerm = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_TERM);

        initializeRule3Fix();
        initializeRule6Fix();
        initializeRule7Fix();
        initializeRule9Fix();
        initializeRule16Fix();
        initializeRule23Fix();
        initializeRule26Fix();
        initializeRule51Fix();
    }

    /**
     * Adds the programs.
     *
     * @throws X2BaseException exception
     */
    private void addPrograms() throws X2BaseException {
        int numOfNeededStudents = 10;
        X2Criteria currentStudentsCriteria = getFLReportData().getStudentHelper().getStudentCriteria();
        SubQuery curStudentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, currentStudentsCriteria);
        Collection<String> currentStudents = getModelBroker().getSubQueryCollectionByQuery(curStudentsSubQuery);
        if (currentStudents.size() < numOfNeededStudents) {
            FLStudentHelper helper = new FLStudentHelper(getFLReportData());
            helper.getStudentCriteria().addNotIn(X2BaseBean.COL_OID, currentStudents);
            ArrayList<SisStudent> studentsWithoutDrop =
                    new ArrayList<SisStudent>(getModelBroker().getCollectionByQuery(helper.getStudentQuery(true)));
            for (int i = 0; i <= numOfNeededStudents; i++) {
                SisStudent randomStudent = studentsWithoutDrop.get(m_random.nextInt(studentsWithoutDrop.size()));
                StudentProgramParticipation pgm = getNewProgram(randomStudent);
                getModelBroker().saveBean(pgm);
            }

        }
    }

    /**
     * Gets the new program.
     *
     * @param student SisStudent
     * @return Student program participation
     * @throws X2BaseException exception
     */
    private StudentProgramParticipation getNewProgram(SisStudent student) throws X2BaseException {
        StudentProgramParticipation newPgm =
                new StudentProgramParticipation(getBroker().getPersistenceKey());
        newPgm.setStudentOid(student.getOid());
        newPgm.setExtendedDataDictionaryOid(m_dictionary.getExtendedDictionaryOid());

        PlainDate programStartDate = null;
        StudentEnrollment nearestEnrollment =
                getFLReportData().getStudentHelper().getEnrollmentForDate(student.getOid(), getCurrentContext().getStartDate(), "EW");
        if (nearestEnrollment == null) {
            programStartDate = getCurrentContext().getStartDate();
        } else {
            programStartDate = new PlainDate(new Date(
                    ThreadLocalRandom.current().nextLong(nearestEnrollment.getEnrollmentDate().getTime(),
                            getFLReportData().getSurveyPeriod().getEndDate().getTime())));
        }

        newPgm.setStartDate(programStartDate);
        newPgm.setProgramCode(PROGRAM_CODE_DROPOUT);

        newPgm.setFieldValueByBeanPath(m_fieldCode.getJavaName(), getRandomCodeForField(m_fieldCode));
        newPgm.setFieldValueByBeanPath(m_fieldActionsTaken.getJavaName(), getRandomCodeForField(m_fieldActionsTaken));
        newPgm.setFieldValueByBeanPath(m_fieldExitOpTest.getJavaName(), getRandomCodeForField(m_fieldExitOpTest));
        newPgm.setFieldValueByBeanPath(m_fieldPosInfluences.getJavaName(), getRandomCodeForField(m_fieldPosInfluences));
        newPgm.setFieldValueByBeanPath(m_fieldPretestMath.getJavaName(), getRandomCodeForField(m_fieldPretestMath));
        newPgm.setFieldValueByBeanPath(m_fieldPretestReading.getJavaName(), getRandomCodeForField(m_fieldPretestReading));
        newPgm.setFieldValueByBeanPath(m_fieldPrimaryReason.getJavaName(), getRandomCodeForField(m_fieldPrimaryReason));
        newPgm.setFieldValueByBeanPath(m_fieldProgressMath.getJavaName(), getRandomCodeForField(m_fieldProgressMath));
        newPgm.setFieldValueByBeanPath(m_fieldProgressReading.getJavaName(), getRandomCodeForField(m_fieldProgressReading));
        newPgm.setFieldValueByBeanPath(m_fieldSecondaryReason.getJavaName(), getRandomCodeForField(m_fieldSecondaryReason));
        newPgm.setFieldValueByBeanPath(m_fieldTerm.getJavaName(), getRandomCodeForField(m_fieldTerm));

        return newPgm;
    }

    /**
     * Initialize rule 3 fix.
     */
    private void initializeRule3Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Student Number",
                        "^(?!(00|69|7[067]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w$",
                        "Student Number, must be numeric, tenth position must either be an X or numeric."
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-68, 71-75 or 78-79."
                                + "If the tenth position is an X, the first three positions may not all be zeroes."),
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
     * Initialize rule 6 fix.
     */
    private void initializeRule6Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Dropout/Juvenile Pgm", "^[URAENPDJW]$"))
                        .testThen(Restriction.and(Restriction.greaterThanOrEquals("Enrollment Date",
                                (Date) m_dateConverter.parseSystemString("1989-07-01"), "MMddyyyy"),
                                Restriction.lessThanOrEquals("Enrollment Date",
                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")))),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        System.out.println(
                                "6:" + getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.DRP, "Enrollment Date"));
                        List<StudentProgramParticipation> programs =
                                ((FLDropoutPreventionProgramData) getFLReportData()).m_pgmDataset.getPrograms(bean.getOid());

                        Date tillDate = (Date) (new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")).getRuntimeObject(getHelper());
                        for (StudentProgramParticipation pgm : programs) {
                            if (pgm.getEndDate() == null || pgm.getEndDate().after(tillDate)) {
                                pgm.setEndDate(new PlainDate(tillDate));
                            }
                            getModelBroker().saveBean(pgm);
                        }
                    }

                }));

        addFixesByRuleNumber("6", ruleWithFixes);
    }

    /**
     * Initialize rule 7 fix.
     */
    private void initializeRule7Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression(
                        "Dropout/Juvenile Pgm", "^[URAENPDJW]$",
                        "Dropout Prevention/Juvenile Justice Programs must be A, D, E, J, N, P, R, U, or W."),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentProgramParticipation> programs =
                                ((FLDropoutPreventionProgramData) getFLReportData()).m_pgmDataset.getPrograms(bean.getOid());

                        for (StudentProgramParticipation pgm : programs) {
                            pgm.setFieldValueByBeanPath(m_fieldCode.getJavaName(), getRandomCodeForField(m_fieldCode));
                            getModelBroker().saveBean(pgm);
                        }

                    }

                }));

        addFixesByRuleNumber("7", ruleWithFixes);
    }

    /**
     * Initialize rule 9 fix.
     */
    private void initializeRule9Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Fund Source", "^(D|Z)$", "Fund Source code must be D or Z."),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentProgramParticipation> programs =
                                ((FLDropoutPreventionProgramData) getFLReportData()).m_pgmDataset.getPrograms(bean.getOid());

                        for (StudentProgramParticipation pgm : programs) {
                            pgm.setFieldValueByBeanPath(m_fieldFundSource.getJavaName(), m_random.nextBoolean() ? "D" : "Z");
                            getModelBroker().saveBean(pgm);
                        }

                    }

                }));

        addFixesByRuleNumber("9", ruleWithFixes);
    }

    /**
     * Initialize rule 16 fix.
     */
    private void initializeRule16Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                        .testThen(Restriction.and(
                                Restriction.greaterThanOrEquals("Withdrawal Date",
                                        m_fiscalDateStart),
                                Restriction.lessThanOrEquals("Withdrawal Date",
                                        m_fiscalDateEnd))),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentProgramParticipation> programs =
                                ((FLDropoutPreventionProgramData) getFLReportData()).m_pgmDataset.getPrograms(bean.getOid());

                        Date startDate = (Date) m_fiscalDateStart.getRuntimeObject(getHelper());
                        Date endDate = (Date) m_fiscalDateEnd.getRuntimeObject(getHelper());
                        for (StudentProgramParticipation pgm : programs) {
                            Date randomDateBetween = new Date(ThreadLocalRandom.current().nextLong(startDate.getTime(), endDate.getTime()));
                            pgm.setEndDate(new PlainDate(randomDateBetween));
                            getModelBroker().saveBean(pgm);
                        }

                    }

                }));

        addFixesByRuleNumber("16", ruleWithFixes);
    }

    /**
     * Initialize rule 23 fix.
     */
    private void initializeRule23Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                        .testThen(Restriction.greaterThanOrEqualsFieldValue("Withdrawal Date",
                                "Enrollment Date", Date.class)),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        System.out.println(
                                "23-1:" + getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.DRP, "Withdrawal Date"));
                        System.out.println(
                                "23-2:" + getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.DRP, "Enrollment Date"));

                        List<StudentProgramParticipation> programs =
                                ((FLDropoutPreventionProgramData) getFLReportData()).m_pgmDataset.getPrograms(bean.getOid());

                        for (StudentProgramParticipation pgm : programs) {
                            if (pgm.getEndDate() != null) {
                                PlainDate startDate = pgm.getStartDate();
                                pgm.setStartDate(pgm.getEndDate());
                                pgm.setEndDate(startDate);
                                getModelBroker().saveBean(pgm);
                            }
                        }
                    }

                }));

        addFixesByRuleNumber("23", ruleWithFixes);
    }

    /**
     * Initialize rule 26 fix.
     */
    private void initializeRule26Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Term", "^(3|S)$", "Term must be 3 or S."),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentProgramParticipation> programs =
                                ((FLDropoutPreventionProgramData) getFLReportData()).m_pgmDataset.getPrograms(bean.getOid());

                        for (StudentProgramParticipation pgm : programs) {
                            pgm.setFieldValueByBeanPath(m_fieldTerm.getJavaName(), m_random.nextBoolean() ? "3" : "S");
                            getModelBroker().saveBean(pgm);
                        }

                    }

                }));

        addFixesByRuleNumber("26", ruleWithFixes);
    }

    /**
     * Initialize rule 51 fix.
     */
    private void initializeRule51Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Dropout/Juvenile Pgm", "D"))
                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number", "District Number, I/S"),
                                new KeyValuePair("School Number", "School Number"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.pattern("Grade Level", "^(PK|KG)$"), null))),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentProgramParticipation> programs =
                                ((FLDropoutPreventionProgramData) getFLReportData()).m_pgmDataset.getPrograms(bean.getOid());

                        for (StudentProgramParticipation pgm : programs) {
                            if ("D".equals(pgm.getFieldValueByBeanPath(m_fieldCode.getJavaName()))) {
                                String randomCode = null;
                                while (randomCode == null || "D".equals(randomCode)) {
                                    randomCode = getRandomCodeForField(m_fieldCode);
                                }
                                pgm.setFieldValueByBeanPath(m_fieldCode.getJavaName(), randomCode);
                                getModelBroker().saveBean(pgm);
                            }
                        }

                    }

                }));

        addFixesByRuleNumber("51", ruleWithFixes);
    }
}
