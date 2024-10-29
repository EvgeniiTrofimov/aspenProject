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
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.ibm.icu.text.SimpleDateFormat;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLStaffExtractValidation.ValidateEmploymentDate;
import com.x2dev.procedures.statereporting.fl.FLStaffExtractValidation.ValidatePersonnelEvaluationDetails;
import com.x2dev.procedures.statereporting.fl.FLStaffExtractValidation.ValidatePersonnelEvaluationTotals;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * @author Follett Software Company
 * @copyright 2018
 */
public class SetProcedureSTF extends SetProcedure {

    private static String ALIAS_STF_EMPL_DATE_CONT = "all-stf-EmploymentDateContinuous";
    private static String ALIAS_STF_EMPL_TYPE = "all-stf-EmployeeType";
    private static String ALIAS_STF_EXP_TYPE = "all-stf-ExperienceType";
    private static String ALIAS_STF_PERF_MEAS = "all-stf-PersonnelEvaluationStdPerformanceMeasures";
    private static String ALIAS_STF_PERS_EV_LEADERSHIP = "all-stf-PersonnelEvaluationLeadership";
    private static String ALIAS_STF_PERS_EV_PERFORMANCE = "all-stf-PersonnelEvaluationStdPerformance";
    private static String ALIAS_STF_PERS_EV_PRACTICE = "all-stf-PersonnelEvaluationPractice";
    private static String ALIAS_STF_PERS_EV_RESPONSIBILITIES = "all-stf-PersonnelEvaluationResponsibilities";
    private static String ALIAS_STF_SEPARATION_DATE = "all-stf-SeparationDate";
    private static String ALIAS_STF_SEPARATION_REASON = "all-stf-SeparationReason";

    private static String REF_TABLE_OID_JOB_CODES = "rtbStaffJob";
    private static String REF_TABLE_OID_FL_JOB_CODES = "rtbFlJobCodes";

    private static final PlainDate ZERO_DATE = new PlainDate(new Date(0L));

    private String[] m_empTypeCodes = {"RF", "RP", "TF", "TP", "CF", "CP", "ST"};

    private DataDictionaryField m_fieldJobCode = null;
    private DataDictionaryField m_fieldSeparationReason = null;

    private Random m_random = null;

    private static String EVAL_LEADERSHIP = String.valueOf(34);
    private static String EVAL_PERFORMANCE = String.valueOf(33);
    private static String EVAL_PRACTICE = String.valueOf(34);
    private static String EVAL_RESPONSIBILITIES = String.valueOf(33);

    private SimpleDateFormat m_toAliasDateFormat = null;

    /**
     * @see com.x2dev.procedures.statereporting.fl.SetProcedure#execute()
     */
    @Override
    protected void execute() throws Exception {
        reassignJobCodesRefTable();
        DataDictionaryCache.clearAllDictionaries();
        runAndValidateExports();
        super.execute();
        runAndValidateExports();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FL_EXPORT.STF, SisStaff.class);

        m_toAliasDateFormat = new SimpleDateFormat(DateAsStringConverter.STRING_DATE_FORMAT);

        m_random = new Random(System.currentTimeMillis());

        m_fieldJobCode = getFLReportData().getDataDictionaryField(StaffPosition.class, StaffPosition.COL_JOB_CODE);
        m_fieldSeparationReason = getFLReportData().translateAliasToDictionaryField(ALIAS_STF_SEPARATION_REASON, true);

        initializeRule12_51_52Fix();
        initializeRule13_53Fix();
        initializeRule6Fix();
        initializeRule14Fix();
        initializeRule16Fix();
        initializeRule17Fix();
        initializeRule1QFix();
        initializeRule1RFix();
        initializeRule1TFix();
        initializeRule1UFix();
        initializeRule24Fix();
        initializeRule2EFix();
        initializeRule2VFix();
        initializeRule1V_1WFix();
        initializeRule38Fix();
    }

    private void reassignJobCodesRefTable() {
        X2Criteria fddCriteria = new X2Criteria();
        fddCriteria.addEqualTo(DataFieldConfig.COL_REFERENCE_TABLE_OID, REF_TABLE_OID_JOB_CODES);
        QueryByCriteria query = new QueryByCriteria(DataFieldConfig.class, fddCriteria);

        Collection<DataFieldConfig> configs = getBroker().getCollectionByQuery(query);
        for (DataFieldConfig config : configs) {
            config.setReferenceTableOid(REF_TABLE_OID_FL_JOB_CODES);
            getModelBroker().saveBeanForced(config);
        }
    }

    private void initializeRule12_51_52Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateEmploymentDate("Employment Date CP"),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        PlainDate current = FLStaffExtractValidation.parseDate(new ArrayList(), "Employment Date CP", getHelper(),
                                FL_EXPORT.STF, getCurrentRow());

                        PlainDate ce =
                                FLStaffExtractValidation.parseDate(new ArrayList(), "Employment Date CE", getHelper(),
                                        FL_EXPORT.STF, getCurrentRow());
                        if (ce == null) {
                            String formattedDate = m_toAliasDateFormat.format(current);
                            staff.setFieldValueByAlias(ALIAS_STF_EMPL_DATE_CONT, formattedDate);
                            getModelBroker().saveBeanForced(staff);
                        }
                        PlainDate op =
                                FLStaffExtractValidation.parseDate(new ArrayList(), "Employment Date OP", getHelper(),
                                        FL_EXPORT.STF, getCurrentRow());
                        if (op == null) {
                            staff.setHireDate(getHelper().getContext().getStartDate());
                            getModelBroker().saveBeanForced(staff);
                        }

                        PlainDate separation =
                                FLStaffExtractValidation.parseDate(new ArrayList(), "Separation Date", getHelper(),
                                        FL_EXPORT.STF, getCurrentRow());
                        if (separation != null && separation.before(getHelper().getContext().getStartDate())) {
                            if (!ZERO_DATE.equals(current)) {
                                long startDate = getFLReportData().getCurrentContext().getStartDate().getTime();
                                long endDate = getFLReportData().getCurrentContext().getEndDate().getTime();
                                Date newSeparationDate = new Date(ThreadLocalRandom.current().nextLong(startDate, endDate));
                                String formattedDate = m_toAliasDateFormat.format(newSeparationDate);
                                staff.setFieldValueByAlias(ALIAS_STF_SEPARATION_DATE, formattedDate);

                                getModelBroker().saveBeanForced(staff);
                            }
                        }
                        if (current != null && ce != null && current.before(ce)) {
                            String formattedDate = m_toAliasDateFormat.format(current);
                            staff.setFieldValueByAlias(ALIAS_STF_EMPL_DATE_CONT, formattedDate);
                            getModelBroker().saveBeanForced(staff);
                        }
                    }
                }));

        addFixesByRuleNumber("12_51_52", ruleWithFixes);
    }

    private void initializeRule13_53Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateEmploymentDate("Employment Date CE"),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        PlainDate current = FLStaffExtractValidation.parseDate(new ArrayList(), "Employment Date CE", getHelper(),
                                FL_EXPORT.STF, getCurrentRow());

                        PlainDate ce =
                                FLStaffExtractValidation.parseDate(new ArrayList(), "Employment Date CE", getHelper(),
                                        FL_EXPORT.STF, getCurrentRow());
                        if (ce == null) {
                            String formattedDate = m_toAliasDateFormat.format(current);
                            staff.setFieldValueByAlias(ALIAS_STF_EMPL_DATE_CONT, formattedDate);
                            getModelBroker().saveBeanForced(staff);
                        }
                        PlainDate op =
                                FLStaffExtractValidation.parseDate(new ArrayList(), "Employment Date OP", getHelper(),
                                        FL_EXPORT.STF, getCurrentRow());
                        if (op == null) {
                            staff.setHireDate(getHelper().getContext().getStartDate());
                            getModelBroker().saveBeanForced(staff);
                        }

                        PlainDate separation =
                                FLStaffExtractValidation.parseDate(new ArrayList(), "Separation Date", getHelper(),
                                        FL_EXPORT.STF, getCurrentRow());
                        if (separation != null && separation.before(getHelper().getContext().getStartDate())) {
                            if (!ZERO_DATE.equals(current)) {
                                long startDate = getFLReportData().getCurrentContext().getStartDate().getTime();
                                long endDate = getFLReportData().getCurrentContext().getEndDate().getTime();
                                Date newSeparationDate = new Date(ThreadLocalRandom.current().nextLong(startDate, endDate));
                                String formattedDate = m_toAliasDateFormat.format(newSeparationDate);
                                staff.setFieldValueByAlias(ALIAS_STF_SEPARATION_DATE, formattedDate);

                                getModelBroker().saveBeanForced(staff);
                            }
                        }
                        if (current != null && ce != null && current.before(ce)) {
                            String formattedDate = m_toAliasDateFormat.format(current);
                            staff.setFieldValueByAlias(ALIAS_STF_EMPL_DATE_CONT, formattedDate);
                            getModelBroker().saveBeanForced(staff);
                        }
                    }
                }));

        addFixesByRuleNumber("13_53", ruleWithFixes);
    }

    private void initializeRule6Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Certificate Number",
                        "^(?!60{2}[2-4]0{6})([0-5][0-9]{9}|600[0-4][0-9]{6}|9999999999$)",
                        "Must be numeric, and in the range 0000000000 - 6001999999, 6002000001 - 6002999999, 6003000001 - 6003999999, 6004000001 - 6004999999 or 9999999999"),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        staff.setFieldValueByAlias(ALIAS_STF_SEPARATION_REASON, getRandomCodeForField(m_fieldSeparationReason));
                        getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("6", ruleWithFixes);
    }

    private void initializeRule14Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateEmploymentDate("Employment Date OP", false),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        staff.setFieldValueByAlias(ALIAS_STF_SEPARATION_DATE, m_toAliasDateFormat.format(staff.getHireDate()));
                        getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("14", ruleWithFixes);
    }

    private void initializeRule15Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Separation Reason", "^[A-P]|Z$",
                        "Separation Reason code must be A-P or Z. This edit does not apply to Survey 8."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        staff.setFieldValueByAlias(ALIAS_STF_SEPARATION_REASON, getRandomCodeForField(m_fieldSeparationReason));
                        getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("15", ruleWithFixes);
    }

    private void initializeRule16Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Separation Reason", "^[A-P]|Z$",
                        "Separation Reason code must be A-P or Z. This edit does not apply to Survey 8."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        staff.setFieldValueByAlias(ALIAS_STF_SEPARATION_REASON, getRandomCodeForField(m_fieldSeparationReason));
                        getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("16", ruleWithFixes);
    }

    private void initializeRule17Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Job Code Primary", "^\\d+$",
                        "Job Code, Primary must equal one of the codes on the Job Code Assignments table."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            SisStaff staff = (SisStaff) bean;
                            StaffPosition primary = getFLReportData().getStaffHelper().getPrimaryStaffPosition(staff);
                            if (primary != null) {
                                primary.setJobCode(getRandomCodeForField(m_fieldJobCode));
                                getModelBroker().saveBeanForced(primary);
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));

        addFixesByRuleNumber("17", ruleWithFixes);
    }

    private void initializeRule1QFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidatePersonnelEvaluationDetails("PE Leadership",
                        new String[] {"73026"}, 33, 67),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        staff.setFieldValueByAlias(ALIAS_STF_PERS_EV_LEADERSHIP,
                                String.valueOf(Integer.valueOf(ThreadLocalRandom.current().nextInt(33, 68))));
                        getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("1Q", ruleWithFixes);
    }

    private void initializeRule1RFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidatePersonnelEvaluationDetails("PE Practice",
                        new String[] {"51080", "52015", "52080", "53080", "54080", "55052", "55080", "59080"}, 33, 67),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        staff.setFieldValueByAlias(ALIAS_STF_PERS_EV_PRACTICE,
                                String.valueOf(Integer.valueOf(ThreadLocalRandom.current().nextInt(33, 68))));
                        getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("1R", ruleWithFixes);
    }

    private void initializeRule1TFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidatePersonnelEvaluationDetails("PE Std Performance",
                        new String[] {"73026", "51080", "52015", "52080", "53080", "54080", "55052", "55080", "59080"}, 33, 67),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        staff.setFieldValueByAlias(ALIAS_STF_PERS_EV_PERFORMANCE,
                                String.valueOf(Integer.valueOf(ThreadLocalRandom.current().nextInt(33, 68))));
                        getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("1T", ruleWithFixes);
    }

    private void initializeRule1V_1WFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidatePersonnelEvaluationTotals(),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        int practice = FLStaffExtractValidation
                                .parseInt(getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.STF, "PE Practice"));
                        int leadership = FLStaffExtractValidation
                                .parseInt(getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.STF, "PE Leadership"));
                        int responsibilities =
                                FLStaffExtractValidation.parseInt(
                                        getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.STF, "PE Responsibilities"));
                        int stdPerformance =
                                FLStaffExtractValidation.parseInt(
                                        getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.STF, "PE Std Performance"));

                        if (leadership >= 33 && (leadership + responsibilities + stdPerformance) != 100) {
                            staff.setFieldValueByAlias(ALIAS_STF_PERS_EV_LEADERSHIP, EVAL_LEADERSHIP);
                            staff.setFieldValueByAlias(ALIAS_STF_PERS_EV_RESPONSIBILITIES, EVAL_RESPONSIBILITIES);
                            staff.setFieldValueByAlias(ALIAS_STF_PERS_EV_PERFORMANCE, EVAL_PERFORMANCE);
                        }

                        if (practice >= 33 && (practice + responsibilities + stdPerformance) != 100) {
                            staff.setFieldValueByAlias(ALIAS_STF_PERS_EV_PRACTICE, EVAL_PRACTICE);
                            staff.setFieldValueByAlias(ALIAS_STF_PERS_EV_RESPONSIBILITIES, EVAL_RESPONSIBILITIES);
                            staff.setFieldValueByAlias(ALIAS_STF_PERS_EV_PERFORMANCE, EVAL_PERFORMANCE);
                        }

                        staff.setFieldValueByAlias(ALIAS_STF_PERF_MEAS,
                                String.valueOf((char) ThreadLocalRandom.current().nextInt('B', 'K')));
                        getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("1V_1W", ruleWithFixes);
    }

    private void initializeRule1UFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidatePersonnelEvaluationDetails("Performance Measures",
                        new String[] {"73026", "51080", "52015", "52080", "53080", "54080", "55052", "55080", "59080"}, "^[B-K]$"),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        staff.setFieldValueByAlias(ALIAS_STF_PERF_MEAS,
                                String.valueOf((char) ThreadLocalRandom.current().nextInt('B', 'K')));
                        getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("1U", ruleWithFixes);
    }

    private void initializeRule24Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Employee Type", "^RF|RP|TF|TP|CF|CP|ST$",
                        "Employee Type code must be RF, RP, TF, TP, or ST."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        staff.setFieldValueByAlias(ALIAS_STF_EMPL_TYPE, m_empTypeCodes[m_random.nextInt(7)]);
                        getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("24", ruleWithFixes);
    }

    private void initializeRule2EFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(
                        Restriction.notEquals("Survey Period", "8"),
                        Restriction.greaterThanOrEquals("Separation Date",
                                new RuntimeParam(RuntimeParam.FISCAL_BEGIN_DATE)),
                        Restriction.lessThanOrEquals("Separation Date",
                                new RuntimeParam(RuntimeParam.FISCAL_END_DATE))))
                        .testThen(Restriction.notEquals("Separation Reason", "Z")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        staff.setFieldValueByAlias(ALIAS_STF_SEPARATION_REASON, getRandomCodeForField(m_fieldSeparationReason));
                        getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("2E", ruleWithFixes);
    }

    private void initializeRule2VFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(
                        Restriction.notEquals("Survey Period", "8"),
                        Restriction.pattern("Separation Reason", "^[A-P]$")))
                        .testThen(Restriction.pattern("Employee Type", "RF|RP")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        staff.setFieldValueByAlias(ALIAS_STF_EMPL_TYPE, m_random.nextBoolean() ? "RF" : "RP");
                        getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("2V", ruleWithFixes);
    }

    private void initializeRule38Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(
                        Restriction.pattern("Survey Period", "^2|3$"),
                        Restriction.pattern("Job Code Primary",
                                "^(?!.*(51080|52080|53080|54080|55080|59080)).*$"),
                        Restriction.pattern("Employee Type", "^RF|TF$")))
                        .testThen(
                                Restriction.and(
                                        Restriction.validateMatchInExport("EXPDATA-FL-SXP",
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("SSN", "SSN"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Pattern.compile("^C$"), "Experience Type")),
                                        Restriction.validateMatchInExport("EXPDATA-FL-SXP",
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("SSN", "SSN"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Pattern.compile("^D$"), "Experience Type")),
                                        Restriction.validateMatchInExport("EXPDATA-FL-SXP",
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("SSN", "SSN"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Pattern.compile("^F$"), "Experience Type")))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff staff = (SisStaff) bean;

                        staff.setFieldValueByAlias(ALIAS_STF_EXP_TYPE + "C", "10");
                        staff.setFieldValueByAlias(ALIAS_STF_EXP_TYPE + "D", "20");
                        staff.setFieldValueByAlias(ALIAS_STF_EXP_TYPE + "F", "30");
                        // getModelBroker().saveBeanForced(staff);
                    }
                }));

        addFixesByRuleNumber("38", ruleWithFixes);
    }

}
