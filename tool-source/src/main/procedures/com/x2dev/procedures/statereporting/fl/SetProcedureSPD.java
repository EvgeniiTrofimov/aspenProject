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

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffPdPlan;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

/**
 * @author Follett Software Company
 * @copyright 2018
 */
public class SetProcedureSPD extends SetProcedure {

    private static String ALIAS_SDP_COMPONENT_NUMBER = "all-sdp-ComponentNumber";
    private static String ALIAS_SPD_DISTRICT_NUMBER = "all-sdp-DistrictNumber";
    private static String ALIAS_SPD_EVAL_METHOD_STAFF = "all-sdp-EvaluationMethodStaff";
    private static String ALIAS_SPD_EVAL_METHOD_STUDENT = "all-sdp-EvaluationMethodStudent";
    private static String ALIAS_SPD_IMPL_METHOD = "all-sdp-ImplementationMethod";
    private static String ALIAS_SPD_LEARNING_METHOD = "all-sdp-LearningMethod";
    private static String ALIAS_SPD_PART_HOURS = "all-sdp-ParticipationHours";
    private static String ALIAS_SPD_PRIMARY_PURPOSE = "all-sdp-PrimaryPurpose";

    private Random m_random = null;

    private DataDictionaryField m_fieldCompNum = null;
    private DataDictionaryField m_fieldDistrNum = null;
    private DataDictionaryField m_fieldEvalMethodStf = null;
    private DataDictionaryField m_fieldEvalMethodStd = null;
    private DataDictionaryField m_fieldImplMethod = null;
    private DataDictionaryField m_fieldLearnMethod = null;
    private DataDictionaryField m_fieldPartHours = null;
    private DataDictionaryField m_fieldPrimePurpose = null;

    private int[][] m_twoFourRanges = {{2, 18}, {100, 107}, {200, 212},
            {300, 309}, {400, 425}, {500, 521},
            {600, 603}, {700, 706}, {800, 806}};

    /**
     * @see com.x2dev.procedures.statereporting.fl.SetProcedure#execute()
     */
    @Override
    protected void execute() throws Exception {
        addPDPlans();
        runAndValidateExports();
        super.execute();
        runAndValidateExports();
    }

    /**
     * @see com.x2dev.procedures.statereporting.fl.SetProcedure#initialize(com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
     *      java.lang.Class)
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FL_EXPORT.SPD, SisStaff.class);

        m_fieldCompNum = getFLReportData().translateAliasToDictionaryField(ALIAS_SDP_COMPONENT_NUMBER, true);
        m_fieldDistrNum = getFLReportData().translateAliasToDictionaryField(ALIAS_SPD_DISTRICT_NUMBER, true);
        m_fieldEvalMethodStf = getFLReportData().translateAliasToDictionaryField(ALIAS_SPD_EVAL_METHOD_STAFF, true);
        m_fieldEvalMethodStd = getFLReportData().translateAliasToDictionaryField(ALIAS_SPD_EVAL_METHOD_STUDENT, true);
        m_fieldImplMethod = getFLReportData().translateAliasToDictionaryField(ALIAS_SPD_IMPL_METHOD, true);
        m_fieldLearnMethod = getFLReportData().translateAliasToDictionaryField(ALIAS_SPD_LEARNING_METHOD, true);
        m_fieldPartHours = getFLReportData().translateAliasToDictionaryField(ALIAS_SPD_PART_HOURS, true);
        m_fieldPrimePurpose = getFLReportData().translateAliasToDictionaryField(ALIAS_SPD_PRIMARY_PURPOSE, true);

        m_random = new Random(System.currentTimeMillis());

        initializeRule3Fix();
        initializeRule8Fix();
        initializeRule12Fix();
        initializeRule17Fix();
        initializeRule30Fix();
    }

    protected void addPDPlans() {
        try {
            Collection<StaffPdPlan> plans = new ArrayList<StaffPdPlan>();
            int neededNumOfPdPlans = 10;
            ArrayList<SisStaff> staffs =
                    new ArrayList<>(getModelBroker().getCollectionByQuery(getFLReportData().getQuery()));
            for (SisStaff staff : staffs) {
                Collection<StaffPdPlan> staffPlans = getFLReportData().getStaffHelper().getStaffPdPlans(staff);
                if (staffPlans != null) {
                    plans.addAll(staffPlans);
                }
            }
            if (plans.size() < neededNumOfPdPlans) {
                while (plans.size() < 20) {
                    SisStaff randomStaff = staffs.get(m_random.nextInt(staffs.size()));
                    StaffPdPlan newPlan = new StaffPdPlan(getModelBroker().getPersistenceKey());
                    Date renewalDate =
                            new Date(ThreadLocalRandom.current().nextLong(
                                    getFLReportData().getCurrentContext().getStartDate().getTime(),
                                    getFLReportData().getCurrentContext().getEndDate().getTime()));
                    newPlan.setRenewalDate(new PlainDate(renewalDate));
                    newPlan.setFieldValueByBeanPath(m_fieldEvalMethodStd.getJavaName(),
                            getRandomCodeForField(m_fieldEvalMethodStd));
                    newPlan.setFieldValueByBeanPath(m_fieldEvalMethodStf.getJavaName(),
                            getRandomCodeForField(m_fieldEvalMethodStf));
                    newPlan.setFieldValueByBeanPath(m_fieldImplMethod.getJavaName(),
                            getRandomCodeForField(m_fieldImplMethod));
                    newPlan.setFieldValueByBeanPath(m_fieldLearnMethod.getJavaName(),
                            getRandomCodeForField(m_fieldLearnMethod));
                    newPlan.setFieldValueByBeanPath(m_fieldPrimePurpose.getJavaName(),
                            getRandomCodeForField(m_fieldPrimePurpose));
                    newPlan.setStaffOid(randomStaff.getOid());

                    getModelBroker().saveBeanForced(newPlan);

                    plans.add(newPlan);
                }
            }
        } catch (X2BaseException e) {
            e.printStackTrace();
        }
    }

    private void initializeRule3Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("SSN", "^.{9}.*$"))
                        .testThen(Restriction.pattern("SSN", "^.{9}\\s$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        /* No Fix needed */
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule
                        .testIf(Restriction.pattern("Staff ID Local", "^(?!CS\\d{7}\\s{0,1}).{1,10}$"))
                        .testThen(Restriction.and(
                                Restriction.pattern("SSN", "^\\d{1,10}$"),
                                Restriction.greaterThan("SSN", Double.valueOf(0)),
                                Restriction.notEquals("SSN", "999999999"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        /* No Fix needed */
                    }
                }));
        addFixesByRuleNumber("3", ruleWithFixes);
    }

    private void initializeRule8Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.and(
                                Restriction.pattern("Participation Hours", "^\\d+$"),
                                Restriction.greaterThan("Participation Hours", Double.valueOf(0)))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            SisStaff staff = (SisStaff) bean;
                            Collection<StaffPdPlan> plans = getFLReportData().getStaffHelper().getStaffPdPlans(staff);
                            for (StaffPdPlan plan : plans) {
                                plan.setFieldValueByBeanPath(m_fieldPartHours.getJavaName(), "40");
                                getModelBroker().saveBeanForced(plan);
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("8", ruleWithFixes);
    }

    private void initializeRule12Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Component Number",
                        "^[1-9](000|00[2-9]|01[0-7]|10[0-6]|20[0-9]|21[0-1]|30[0-8]|4[0-1][0-9]|42[0-4]|5[0-1][0-9]|520|60[0-2]|70[0-5]|80[0-5])(00[1-9]|[0-9][0-9][0-9])$",
                        "Position one of the Professional Development, Component Number must be "
                                + "1-9. Positions two, three and four must be 000, 002-017, 100-106, 200-211, 300-308, "
                                + "400-424, 500-520, 600-602, 700-705 or 800-805. Positions five, six and seven must "
                                + "be 001-999."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            SisStaff staff = (SisStaff) bean;
                            Collection<StaffPdPlan> plans = getFLReportData().getStaffHelper().getStaffPdPlans(staff);
                            for (StaffPdPlan plan : plans) {
                                String componentNum =
                                        (String) plan.getFieldValueByBeanPath(m_fieldCompNum.getJavaName());
                                if (componentNum == null || !componentNum.matches(
                                        "^[1-9](000|00[2-9]|01[0-7]|10[0-6]|20[0-9]|21[0-1]|30[0-8]|4[0-1][0-9]|42[0-4]|5[0-1][0-9]|520|60[0-2]|70[0-5]|80[0-5])(00[1-9]|[0-9][1-9][0-9])$")) {

                                    String position1 = String.valueOf(ThreadLocalRandom.current().nextInt(1, 10));
                                    int rangeNum = m_random.nextInt(9);

                                    String position2_4 = String.format("%03d", Integer
                                            .valueOf(ThreadLocalRandom.current().nextInt(m_twoFourRanges[rangeNum][0],
                                                    m_twoFourRanges[rangeNum][1])));

                                    String position5_7 =
                                            String.format("%03d",
                                                    Integer.valueOf(ThreadLocalRandom.current().nextInt(1, 999)));

                                    String compNumber = position1 + position2_4 + position5_7;
                                    plan.setFieldValueByBeanPath(m_fieldCompNum.getJavaName(), compNumber);
                                    getModelBroker().saveBeanForced(plan);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("12", ruleWithFixes);
    }

    private void initializeRule17Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Staff ID Local",
                        "^[A-Za-z0-9\\s]{10}$",
                        "The Staff Number Identifier, Local may be any combination of letters, "
                                + "numbers and blanks. All blanks are not allowable. It must be left-justified with "
                                + "trailing blanks."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        /* No Fix needed */
                    }
                }));
        addFixesByRuleNumber("17", ruleWithFixes);
    }

    private void initializeRule30Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STF",
                                new KeyValuePair("SSN", "SSN"),
                                new KeyValuePair("District Number", "District Number"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        /* No Fix needed */
                    }
                }));
        addFixesByRuleNumber("30", ruleWithFixes);
    }
}
