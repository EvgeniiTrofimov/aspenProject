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
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * The Class SetProcedureSAJA.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class SetProcedureSAJA extends SetProcedure {

    private static final String ALIAS_SFP_PRIMARY_POSITION = "all-sfp-PrimaryPosition";
    private static final String ALIAS_SFP_FUND_SOURCE_1 = "all-sfp-FundSource1";
    private static final String ALIAS_SFP_FUND_SOURCE_2 = "all-sfp-FundSource2";
    private static final String ALIAS_SFP_FUND_SOURCE_3 = "all-sfp-FundSource3";

    private DataDictionaryField m_fieldSfpPrimaryPosition;

    private DataDictionaryField m_fieldSfpFundSource1;
    private DataDictionaryField m_fieldSfpFundSource2;
    private DataDictionaryField m_fieldSfpFundSource3;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.procedures.statereporting.fl.SetProcedure#execute()
     */
    @Override
    protected void execute() throws Exception {
        runAndValidateExports();
        super.execute();
        runAndValidateExports();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws com.x2dev.utils.X2BaseException {
        super.initialize(FL_EXPORT.SAJA, SisStaff.class);

        m_fieldSfpPrimaryPosition = getFLReportData().translateAliasToDictionaryField(ALIAS_SFP_PRIMARY_POSITION, true);

        m_fieldSfpFundSource1 = getFLReportData().translateAliasToDictionaryField(ALIAS_SFP_FUND_SOURCE_1, true);
        m_fieldSfpFundSource2 = getFLReportData().translateAliasToDictionaryField(ALIAS_SFP_FUND_SOURCE_2, true);
        m_fieldSfpFundSource3 = getFLReportData().translateAliasToDictionaryField(ALIAS_SFP_FUND_SOURCE_3, true);

        initializeRule5Fix();
        initializeRule30Fix();
        initializeRule31Fix();
        initializeRule34Fix();
    }

    /**
     * Initialize rule 5 fix.
     */
    private void initializeRule5Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.byActiveSchool("School Number")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            Collection<StaffPosition> staffPositions =
                                    getFLReportData().getStaffHelper().getStaffPositions((SisStaff) bean);
                            if (staffPositions != null && staffPositions.size() > 0) {
                                StaffPosition position = staffPositions.iterator().next();
                                position.setFieldValueByBeanPath(m_fieldSfpPrimaryPosition.getJavaName(), BooleanAsStringConverter.TRUE);
                                getModelBroker().saveBeanForced(position);
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("5", ruleWithFixes);
    }

    /**
     * Initialize rule 30 fix.
     */
    private void initializeRule30Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.notEquals("Job Code Fund Source", "000000000000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            Collection<StaffPosition> staffPositions =
                                    getFLReportData().getStaffHelper().getStaffPositions((SisStaff) bean);
                            if (staffPositions != null) {
                                for (StaffPosition sfp : staffPositions) {
                                    sfp.setFieldValueByBeanPath(m_fieldSfpFundSource1.getJavaName(),
                                            getRandomCodeForField(m_fieldSfpFundSource1));
                                    sfp.setFieldValueByBeanPath(m_fieldSfpFundSource2.getJavaName(),
                                            getRandomCodeForField(m_fieldSfpFundSource2));
                                    sfp.setFieldValueByBeanPath(m_fieldSfpFundSource3.getJavaName(),
                                            getRandomCodeForField(m_fieldSfpFundSource3));
                                    getModelBroker().saveBeanForced(sfp);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
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
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.validateJobCodeFundSource("Job Code Fund Source")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            Set<String> fundSourceCodes = new HashSet<>();
                            while (fundSourceCodes.size() < 3) {
                                fundSourceCodes.add(getRandomCodeForField(m_fieldSfpFundSource1));
                            }
                            String[] codes = new String[3];
                            fundSourceCodes.toArray(codes);
                            Collection<StaffPosition> staffPositions =
                                    getFLReportData().getStaffHelper().getStaffPositions((SisStaff) bean);
                            if (staffPositions != null) {
                                for (StaffPosition sfp : staffPositions) {
                                    sfp.setFieldValueByBeanPath(m_fieldSfpFundSource1.getJavaName(), codes[0]);
                                    sfp.setFieldValueByBeanPath(m_fieldSfpFundSource2.getJavaName(), codes[1]);
                                    sfp.setFieldValueByBeanPath(m_fieldSfpFundSource3.getJavaName(), codes[2]);
                                    getModelBroker().saveBeanForced(sfp);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("31", ruleWithFixes);
    }

    /**
     * Initialize rule 34 fix.
     */
    private void initializeRule34Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Job Code Additional", "^(51080|52080|53080|54080|55080|59080)$"))
                                .testThen(Restriction.greaterThanOrEquals("Job Code FTE", Double.valueOf(0))),
                        ValidationRule
                                .testIf(Restriction.pattern("Job Code Additional", "^(?!51080|52080|53080|54080|55080|59080).*$"))
                                .testThen(Restriction.greaterThan("Job Code FTE", Double.valueOf(0)))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            Set<String> fundSourceCodes = new HashSet<>();
                            while (fundSourceCodes.size() < 3) {
                                fundSourceCodes.add(getRandomCodeForField(m_fieldSfpFundSource1));
                            }
                            String[] codes = new String[3];
                            fundSourceCodes.toArray(codes);
                            Collection<StaffPosition> staffPositions =
                                    getFLReportData().getStaffHelper().getStaffPositions((SisStaff) bean);
                            if (staffPositions != null) {
                                for (StaffPosition sfp : staffPositions) {
                                    sfp.setFte(new BigDecimal(1));
                                    getModelBroker().saveBeanForced(sfp);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("34", ruleWithFixes);
    }
}
