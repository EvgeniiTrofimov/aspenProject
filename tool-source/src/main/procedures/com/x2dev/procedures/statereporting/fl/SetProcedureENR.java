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
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.InvalidPreferenceException;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.ibm.icu.util.Calendar;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.EnrollmentSnapshot;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.GradeMatcher;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionAnd;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidateMatchInExport;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationResult;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.ConductManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ThreadLocalRandom;

/**
 * @author Follett Software Company
 * @copyright 2018
 */
public class SetProcedureENR extends SetProcedure {

    private static class ValidateEnrMatchInEnrExport extends ValidateMatchInExport {
        /**
         * Instantiates a new validate sdra match in except export.
         *
         * @param procedure_id String
         * @param keyFieldNamePair KeyValuePair<String,String>
         * @param relatedFieldNamePairs KeyValuePair<Object,String>[]
         */
        public ValidateEnrMatchInEnrExport(String procedure_id, KeyValuePair<String, String> keyFieldNamePair,
                KeyValuePair<Object, String>... relatedFieldNamePairs) {
            super(procedure_id, keyFieldNamePair, relatedFieldNamePairs);
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param currentExport FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidateMatchInExport#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport currentExport,
                                                    ExportFormatRow row) {

            String entryDateValue = helper.getExportFormatRowFieldValue(row, currentExport, "Entry Date");
            try {
                PlainDate entryDate = new PlainDate(DEFAULT_DATE_FORMAT.parse(entryDateValue));
                for (KeyValuePair<Object, String> fieldNamePair : m_relatedFieldNamePairs) {
                    if (fieldNamePair.getKey() instanceof RestrictionAnd) {
                        RestrictionAnd restrictionAnd = (RestrictionAnd) fieldNamePair.getKey();
                        Restriction[] ands = restrictionAnd.getRestrictions();
                        if (ands.length > 1 && ands[1] instanceof RestrictionCompare) {
                            RestrictionCompare cmp = (RestrictionCompare) ands[1];
                            cmp.setComparableValue(entryDate);
                        }
                    }
                }
                return super.getValidationResult(helper, currentExport, row);
            } catch (Exception ex) {
                return ValidationResult.FieldNotValid("Entry Date", entryDateValue);
            }
        }
    }

    private static final String ALIAS_ENR_EDU_CHOICE = "all-enr-EducationalChoice";

    private static final String ALIAS_ENR_PRIOR_SKL_COUNTY = "all-enr-PriorSchoolCounty";
    private static final String ALIAS_ENR_PRIOR_SKL_COUNTRY = "all-enr-PriorSchoolCountry";
    private static final String ALIAS_ENR_PRIOR_SKL_STATE = "all-enr-PriorSchoolState";

    private static final String ALIAS_STD_DISTRICT_NUMBER_ZONED = "all-std-ZonedDistrict";
    private static final String ALIAS_STD_SCHOOL_NUMBER_ZONED = "all-std-ZonedSchool";
    private static final String ALIAS_STD_STATE_ID_ALIAS = "all-std-StateIdAlias";

    private static final String ALIAS_STD_STATE_ID = "all-std-StateId";

    private static final String DEFAULT_DATE_FORMAT_MASK = "MMddyyyy";
    private static final SimpleDateFormat DEFAULT_DATE_FORMAT = new SimpleDateFormat(DEFAULT_DATE_FORMAT_MASK);

    private List<String> m_entryEnrCodes = null;
    private List<String> m_wdrawEnrCodes = null;
    private DataDictionaryField m_fieldEduChoice = null;
    private DataDictionaryField m_fieldPriorSklState = null;
    private DataDictionaryField m_fieldPriorSklCountry = null;
    private DataDictionaryField m_fieldPriorSklCounty = null;
    private Random m_random = null;

    /**
     * @see com.x2dev.procedures.statereporting.fl.SetProcedure#execute()
     */
    @Override
    protected void execute() throws Exception {
        // for (int i = 0; i < 3; i++) {
        adjustStdGradeLevels();
        // runAndValidateExports();
        super.execute();
        // }
        runAndValidateExports();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FL_EXPORT.ENR, SisStudent.class);

        changeRefTableOfFields();

        m_random = new Random(System.currentTimeMillis());

        X2Criteria entryEnrCodesCriteria = new X2Criteria();
        entryEnrCodesCriteria.addEqualTo(
                ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                "rtbFlEntCodes");
        SubQuery entrySubQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, entryEnrCodesCriteria);
        m_entryEnrCodes = new ArrayList<String>(getModelBroker().getSubQueryCollectionByQuery(entrySubQuery));

        X2Criteria wdrawEnrCodesCriteria = new X2Criteria();
        wdrawEnrCodesCriteria.addEqualTo(
                ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                "rtbFlWdrCodes");
        SubQuery wdrawSubQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, wdrawEnrCodesCriteria);
        m_wdrawEnrCodes = new ArrayList<String>(getModelBroker().getSubQueryCollectionByQuery(wdrawSubQuery));

        m_fieldEduChoice = getFLReportData().getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_ENR_EDU_CHOICE);
        m_fieldPriorSklState =
                getFLReportData().getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_ENR_PRIOR_SKL_STATE);
        m_fieldPriorSklCountry =
                getFLReportData().getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_ENR_PRIOR_SKL_COUNTRY);
        m_fieldPriorSklCounty =
                getFLReportData().getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_ENR_PRIOR_SKL_COUNTY);

        // fixWrongSpans();

        initializeRule3Fix();
        initializeRule4Fix();
        initializeRule4GFix();
        initializeRule9Fix();
        initializeRule12Fix();
        initializeRule13Fix();
        initializeRule14Fix();
        initializeRule15Fix();
        initializeRule16Fix();
        initializeRule17Fix();
        initializeRule18Fix();
        initializeRule19Fix();
        initializeRule20Fix();
        initializeRule21Fix();
        initializeRule22Fix();
        initializeRule23Fix();
        // initializeRule24Fix();
        initializeRule26Fix();
        initializeRule27Fix();
        initializeRule28Fix();
        initializeRule29Fix();
        initializeRule2AFix();
        initializeRule2BFix();
        initializeRule2CFix();
        initializeRule2DFix();
        initializeRule2EFix();
        initializeRule2FFix();
        initializeRule30Fix();
        initializeRule31Fix();
        initializeRule33Fix();
        initializeRule35Fix();
        initializeRule36Fix();
        initializeRule38Fix();
        initializeRule42Fix();
        initializeRule43Fix();
        initializeRule46Fix();
        initializeRule47Fix();
        initializeRule48Fix();
        initializeRule49Fix();
        initializeRule4DFix();
        initializeRule51Fix();
        initializeRule52Fix();
        initializeRule75Fix();
        initializeRule77Fix();
        initializeRule79Fix();
        initializeRule80Fix();
        initializeRule81Fix();
    }

    private void adjustStdGradeLevels() {
        Collection<SisStudent> students = getModelBroker().getCollectionByQuery(getFLReportData().getQuery());
        try {
            FLStudentHelper studentHelper = new FLStudentHelper(getFLReportData());
            GradeMatcher gradeMatcher = studentHelper.new GradeMatcher();

            for (SisStudent student : students) {
                if (student.getOid().equals("STD00000055IOg")) {
                    System.out.println();
                }
                String gradeLevel = student.getGradeLevel();
                if (gradeMatcher.getReferenceCode(gradeLevel) == null) {
                    EnrollmentSnapshot snapshot = studentHelper.new EnrollmentSnapshot(student,
                            getFLReportData().getSurveyPeriod().getStartDate());
                    ReferenceCode gradeCode = gradeMatcher.getReferenceCode(snapshot.getYog());
                    if (gradeCode != null) {
                        student.setGradeLevel(gradeCode.getCode());
                    }
                }
                getModelBroker().saveBean(student);
            }
        } catch (X2BaseException e) {
            e.printStackTrace();
        }
    }

    /**
     * Initialize rule 3 fix.
     */
    private void initializeRule3Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Student Number Alias",
                        "^(?!(00|69|7[067]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w| {10}$",
                        "Student Number, must be numeric, tenth position must either be an X or numeric."
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-68, 71-75 or 78-79. "
                                + "If the tenth position is an X, the first three positions may not all be zeroes."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
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
                        getModelBroker().saveBeanForced(bean);
                    }
                }));

        addFixesByRuleNumber("3", ruleWithFixes);
    }

    /**
     * Initialize rule 4 fix.
     */
    private void initializeRule4Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern("Student Number Alias", "^\\d{9}(X|\\d)$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        String currentId = (String) bean.getFieldValueByAlias(ALIAS_STD_STATE_ID_ALIAS);
                        if (StringUtils.isEmpty(currentId)) {
                            currentId = (String) bean.getFieldValueByAlias(ALIAS_STD_STATE_ID);
                        }
                        if (currentId == null) {
                            throw new X2RuntimeException();
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

                        bean.setFieldValueByAlias(ALIAS_STD_STATE_ID_ALIAS, currentId);
                        getModelBroker().saveBeanForced(bean);

                        // System.out.println(bean.getOid());
                        // System.out.println("4-2");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Student Number Alias", "^\\d{10}$"))
                        .testThen(Restriction.pattern("Student Number Alias",
                                "^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|7[89])\\d{8}$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("4-2");
                    }
                }));

        addFixesByRuleNumber("4", ruleWithFixes);
    }

    /**
     * Initialize rule 4G fix.
     */
    private void initializeRule4GFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(Restriction.equals("Survey Period", "5"),
                        Restriction.equals("Term", "S")))
                        .testThen(Restriction
                                .and(Restriction.greaterThan("Days Present Summer", Double.valueOf(0)),
                                        Restriction.equals("Days Present Annual", "000"),
                                        Restriction.equals("Days Absent Annual", "000"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                span.getFirstActiveEnrollment();
                                String randomCode = null;
                                while (randomCode == null || randomCode.equals("S")) {
                                    randomCode = m_entryEnrCodes.get(m_random.nextInt(m_entryEnrCodes.size()));
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("4G", ruleWithFixes);
    }

    /**
     * Initialize rule 9 fix.
     */
    private void initializeRule9Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Grade Level", "^PK|KG|10|11|12|0[1-9]$",
                        "Grade Level code must be PK, KG, or 01-12."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("9-1");
                    }
                }));

        addFixesByRuleNumber("9", ruleWithFixes);
    }

    /**
     * Initialize rule 12 fix.
     */
    private void initializeRule12Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.or(
                        Restriction.equals("Survey Period", "2"),
                        Restriction.notEquals("Term", "Y"))).testThen(Restriction.pattern("Entry Code PK-12",
                                "^E01|E02|E2A|E03|E3A|E04|E4A|E05|E09|R01|R02|R03$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("12-1");

                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                enr.setEnrollmentCode(m_entryEnrCodes.get(m_random.nextInt(m_entryEnrCodes.size())));
                                getModelBroker().saveBeanForced(enr);
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(
                        Restriction.pattern("Survey Period", "^3|5$"),
                        Restriction.equals("Term", "Y")))
                        .testThen(Restriction.equals("Entry Code PK-12", "ZZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("12-2");
                    }
                }));

        addFixesByRuleNumber("12", ruleWithFixes);
    }

    /**
     * Initialize rule 13 fix.
     */
    private void initializeRule13Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "2")).testThen(
                        Restriction.and(
                                Restriction.byDateFormat("Entry Date"),
                                Restriction.greaterThanOrEquals("Entry Date",
                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                                Restriction.lessThanOrEquals("Entry Date",
                                        new RuntimeParam(RuntimeParam.PERIOD_END_DATE)))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {

                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                Date startDate =
                                        (Date) (new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01"))
                                                .getRuntimeObject(getHelper());
                                Date endDate = (Date) (new RuntimeParam(RuntimeParam.PERIOD_END_DATE))
                                        .getRuntimeObject(getHelper());
                                Date randomDateBetween =
                                        new Date(ThreadLocalRandom.current().nextLong(startDate.getTime(),
                                                endDate.getTime()));
                                enr.setEnrollmentDate(new PlainDate(randomDateBetween));
                                getModelBroker().saveBeanForced(enr);
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("13-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(
                        Restriction.pattern("Survey Period", "^3|5$"),
                        Restriction.equals("Term", "Y")))
                        .testThen(Restriction.equals("Entry Date", "00000000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("13-2");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(
                        Restriction.equals("Survey Period", "5"),
                        Restriction.notEquals("Term", "Y"))).testThen(
                                Restriction.and(
                                        Restriction.byDateFormat("Entry Date"),
                                        Restriction.greaterThanOrEquals("Entry Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                                        Restriction.lessThanOrEquals("Entry Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                Date startDate =
                                        (Date) (new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01"))
                                                .getRuntimeObject(getHelper());
                                Date endDate =
                                        (Date) (new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31"))
                                                .getRuntimeObject(getHelper());
                                Date randomDateBetween =
                                        new Date(ThreadLocalRandom.current().nextLong(startDate.getTime(),
                                                endDate.getTime()));
                                enr.setEnrollmentDate(new PlainDate(randomDateBetween));
                                getModelBroker().saveBeanForced(enr);
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("13-3");
                    }
                }));

        addFixesByRuleNumber("13", ruleWithFixes);
    }

    /**
     * Initialize rule 14 fix.
     */
    private void initializeRule14Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Prior School County",
                        "^0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|99$",
                        "Prior School/Location: District/County must be numeric and in the range"
                                + " 01-68, 71-75 or 99."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                enr.setFieldValueByAlias(ALIAS_ENR_PRIOR_SKL_COUNTY,
                                        String.valueOf(ThreadLocalRandom.current().nextInt(01, 68)));
                                getModelBroker().saveBeanForced(enr);
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("14-1");
                    }
                }));

        addFixesByRuleNumber("14", ruleWithFixes);
    }

    /**
     * Initialize rule 15 fix.
     */
    private void initializeRule15Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue()).testThen(
                        Restriction.or(
                                Restriction.byAliasFldRefTable("all-enr-PriorSchoolState",
                                        "Prior School State"),
                                Restriction.equals("Prior School State", "ZZ"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                enr.setFieldValueByBeanPath(m_fieldPriorSklState.getJavaName(),
                                        getRandomCodeForField(m_fieldPriorSklState));
                                getModelBroker().saveBeanForced(enr);
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("15");
                    }
                }));

        addFixesByRuleNumber("15", ruleWithFixes);
    }

    /**
     * Initialize rule 16 fix.
     */
    private void initializeRule16Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.notEquals("Term", "Y")).testThen(
                        Restriction.byAliasFldRefTable("all-enr-PriorSchoolCountry",
                                "Prior School Country")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                enr.setFieldValueByBeanPath(m_fieldPriorSklCountry.getJavaName(),
                                        getRandomCodeForField(m_fieldPriorSklCountry));
                                getModelBroker().saveBeanForced(enr);
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("16-1");
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Term", "Y")).testThen(
                        Restriction.equals("Prior School Country", "ZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("16-2");
                    }
                }));

        addFixesByRuleNumber("16", ruleWithFixes);
    }

    /**
     * Initialize rule 17 fix.
     */
    private void initializeRule17Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue()).testThen(
                        Restriction.or(
                                Restriction.pattern("Withdrawal Code",
                                        "^DNE|W01|W02|W3A|W3B|W0[4-8]|W8A|W8B|W09|W1[0-3]|W15|W18|W2[1-7]|WFT|WFW|WRW|WGA|WGD|WXL|WXT|WXW|WD1|WPO$"),
                                Restriction.and(Restriction.notEquals("Survey Period", "5"),
                                        Restriction.equals("Withdrawal Code", "ZZZ")))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                String randomCode = null;
                                while (randomCode == null) {
                                    randomCode = m_wdrawEnrCodes.get(m_random.nextInt(m_wdrawEnrCodes.size()));
                                    if (!randomCode.matches(
                                            "^DNE|W01|W02|W3A|W3B|W0[4-8]|W8A|W8B|W09|W1[0-3]|W15|W18|W2[1-7]|WFT|WFW|WRW|WGA|WGD|WXL|WXT|WXW|WD1|WPO$")) {
                                        randomCode = null;
                                    }
                                }
                                StudentEnrollment enr = span.getFirstInactiveEnrollment();
                                if (enr != null) {
                                    enr.setEnrollmentCode(randomCode);
                                    getModelBroker().saveBeanForced(enr);
                                } else {
                                    if (!"Y".equals(span.getFirstActiveEnrollment().getEnrollmentType())) {
                                        StudentEnrollment newWdrawEnr =
                                                new StudentEnrollment(getBroker().getPersistenceKey());
                                        PlainDate endDate = getFLReportData().getSurveyPeriod().getEndDate();
                                        newWdrawEnr.setEnrollmentType("W");
                                        newWdrawEnr.setEnrollmentCode(randomCode);
                                        newWdrawEnr.setEnrollmentDate(endDate);
                                        newWdrawEnr.setStatusCode("Inactive");
                                        newWdrawEnr.setSchoolOid(span.getFirstActiveEnrollment().getSchoolOid());
                                        newWdrawEnr.setStudentOid(span.getFirstActiveEnrollment().getStudentOid());
                                        newWdrawEnr.setYog(span.getFirstActiveEnrollment().getYog());

                                        getFLReportData().getStudentHelper().getStudentEnrollments((SisStudent) bean);
                                        getModelBroker().saveBeanForced(newWdrawEnr);

                                        break;
                                    }
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("17-1");
                    }
                }));

        addFixesByRuleNumber("17", ruleWithFixes);
    }

    /**
     * Initialize rule 18 fix.
     */
    private void initializeRule18Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.or(Restriction.equals("Survey Period", "5"),
                        Restriction.notEquals("Withdrawal Code", "ZZZ"))).testThen(
                                Restriction.and(
                                        Restriction.byDateFormat("Withdrawal Date"),
                                        Restriction.greaterThanOrEquals("Withdrawal Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                                        Restriction.lessThanOrEquals("Withdrawal Date",
                                                new RuntimeParam(RuntimeParam.PERIOD_END_DATE)))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (int i = 0; i < spans.size(); i++) {
                                boolean isLastSpan = i == spans.size() - 1;

                                Date startDate = spans.get(i).getFirstActiveEnrollment().getEnrollmentDate();
                                Date endDate = (Date) (new RuntimeParam(RuntimeParam.PERIOD_END_DATE))
                                        .getRuntimeObject(getHelper());

                                if (!isLastSpan) {
                                    Date nextSpanStartDate =
                                            spans.get(i + 1).getFirstActiveEnrollment().getEnrollmentDate();
                                    if (endDate.after(nextSpanStartDate)) {
                                        endDate = nextSpanStartDate;
                                    }
                                }

                                try {
                                    Date randomDateBetween = !endDate.after(startDate) ? startDate
                                            : new Date(ThreadLocalRandom.current().nextLong(startDate.getTime(),
                                                    endDate.getTime()));

                                    StudentEnrollment enr = spans.get(i).getFirstInactiveEnrollment();
                                    if (enr != null) {
                                        enr.setEnrollmentDate(new PlainDate(randomDateBetween));
                                        getModelBroker().saveBeanForced(enr);
                                    }
                                } catch (Exception e) {
                                    // System.out.println();
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("18-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(Restriction.notEquals("Survey Period", "5"),
                        Restriction.equals("Withdrawal Code", "ZZZ"))).testThen(
                                Restriction.equals("Withdrawal Date", "00000000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstInactiveEnrollment();
                                if (enr != null && enr.getEnrollmentCode() != null) {
                                    enr.setEnrollmentCode(
                                            m_wdrawEnrCodes.get(m_random.nextInt(m_wdrawEnrCodes.size())));
                                    getModelBroker().saveBeanForced(enr);
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("18-2");
                    }
                }));

        addFixesByRuleNumber("18", ruleWithFixes);
    }

    /**
     * Initialize rule 19 fix.
     */
    private void initializeRule19Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(Restriction.notEquals("Term", "Y"),
                        Restriction.pattern("Survey Period", "^3|5$"))).testThen(
                                Restriction.pattern("Days Present Annual",
                                        "^\\d+$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("19-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "2")).testThen(
                        Restriction.equals("Days Present Annual",
                                "000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("19-2");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(Restriction.pattern("Survey Period", "^3|5$"),
                        Restriction.equals("Term", "Y"))).testThen(
                                Restriction.equals("Days Present Annual", "000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("19-3");
                    }
                }));

        addFixesByRuleNumber("19", ruleWithFixes);
    }

    /**
     * Initialize rule 20 fix.
     */
    private void initializeRule20Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(Restriction.notEquals("Term", "Y"),
                        Restriction.pattern("Survey Period", "^3|5$"))).testThen(
                                Restriction.pattern("Days Absent Annual",
                                        "^\\d+$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("20-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "2")).testThen(
                        Restriction.equals("Days Absent Annual",
                                "000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("20-2");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(Restriction.pattern("Survey Period", "^3|5$"),
                        Restriction.equals("Term", "Y"))).testThen(
                                Restriction.equals("Days Absent Annual", "000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("20-3");
                    }
                }));

        addFixesByRuleNumber("20", ruleWithFixes);
    }

    /**
     * Initialize rule 21 fix.
     */
    private void initializeRule21Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(Restriction.notEquals("Term", "Y"),
                        Restriction.equals("Survey Period", "5"))).testThen(
                                Restriction.pattern("Days Present Summer",
                                        "^\\d+$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("21-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$")).testThen(
                        Restriction.equals("Days Present Summer",
                                "000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("21-2");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(Restriction.equals("Survey Period", "5"),
                        Restriction.equals("Term", "Y"))).testThen(
                                Restriction.equals("Days Present Summer", "000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("21-3");
                    }
                }));

        addFixesByRuleNumber("21", ruleWithFixes);
    }

    /**
     * Initialize rule 22 fix.
     */
    private void initializeRule22Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(Restriction.notEquals("Term", "Y"),
                        Restriction.equals("Survey Period", "5"))).testThen(
                                Restriction.pattern("Days Absent Summer",
                                        "^\\d+$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("22-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$")).testThen(
                        Restriction.equals("Days Absent Summer",
                                "000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("22-2");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(Restriction.equals("Survey Period", "5"),
                        Restriction.equals("Term", "Y"))).testThen(
                                Restriction.equals("Days Absent Summer", "000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("22-3");
                    }
                }));

        addFixesByRuleNumber("22", ruleWithFixes);
    }

    /**
     * Initialize rule 23 fix.
     */
    private void initializeRule23Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "5")).testThen(
                        Restriction
                                .greaterThanOrEqualsFieldValue("Withdrawal Date", "Entry Date",
                                        Date.class)),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("23-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$")).testThen(
                        Restriction.or(
                                Restriction
                                        .greaterThanOrEqualsFieldValue("Withdrawal Date", "Entry Date",
                                                Date.class),
                                Restriction.equals("Withdrawal Date", "00000000"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("23-2");
                    }
                }));

        addFixesByRuleNumber("23", ruleWithFixes);
    }

    // /**
    // * Initialize rule 24 fix.
    // */
    // private void initializeRule24Fix() {
    // ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
    // ruleWithFixes.add(new RuleWithFix(
    // ValidationRule.testIf(Restriction.alwaysTrue())
    // .testThen(Restriction.uniqueValue("District Number, E", "School Number, E",
    // "Student Number", "Survey Period", "Fiscal Year", "Entry Date")),
    // new Fix() {
    // @Override
    // protected void fixError(X2BaseBean bean) {
    // //System.out.println(bean.getOid());
    // //System.out.println("24-1");
    // }
    // }));
    //
    // addFixesByRuleNumber("24", ruleWithFixes);
    // }

    /**
     * Initialize rule 26 fix.
     */
    private void initializeRule26Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E01"))
                        .testThen(Restriction.and(
                                Restriction.equalsFieldValue("Prior School County", "District Number, E",
                                        String.class),
                                Restriction.equals("Prior School State", "FL"),
                                Restriction.equals("Prior School Country", "US"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (StudentEnrollment enrollment : getEnrollments((SisStudent) bean)) {
                            if ("E01".equals(enrollment.getEnrollmentCode())) {
                                enrollment.setFieldValueByAlias(ALIAS_ENR_PRIOR_SKL_COUNTY,
                                        getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.ENR,
                                                "District Number, E"));
                                enrollment.setFieldValueByAlias(ALIAS_ENR_PRIOR_SKL_STATE, "FL");
                                enrollment.setFieldValueByAlias(ALIAS_ENR_PRIOR_SKL_COUNTRY, "US");
                                getModelBroker().saveBeanForced(enrollment);
                            }
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("26-1");
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
                ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E02"))
                        .testThen(Restriction.and(
                                Restriction.notEqualsFieldValue("Prior School County", "District Number, E",
                                        String.class),
                                Restriction.notEquals("Prior School County", "99"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (StudentEnrollment enrollment : getEnrollments((SisStudent) bean)) {
                            if (enrollment.getEnrollmentCode() != null
                                    && enrollment.getEnrollmentCode().matches("^E02$")) {
                                String districtNumber =
                                        getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.ENR,
                                                "District Number, E");
                                String randomSchoolCounty = null;
                                while (randomSchoolCounty == null || randomSchoolCounty.equals("99")
                                        || districtNumber.equals(randomSchoolCounty)) {
                                    randomSchoolCounty = getRandomCodeForField(m_fieldPriorSklCounty);
                                }
                                enrollment.setFieldValueByBeanPath(m_fieldPriorSklCounty.getJavaName(),
                                        randomSchoolCounty);
                                getModelBroker().saveBeanForced(enrollment);
                            }
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("27-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E2A"))
                        .testThen(Restriction.equals("Prior School County", "99")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Collection<StudentEnrollment> enrollments = getEnrollments((SisStudent) bean);
                        for (StudentEnrollment enrollment : enrollments) {
                            if ("E2A".equals(enrollment.getEnrollmentCode())) {
                                enrollment.setEnrollmentCode("E02");
                                getModelBroker().saveBeanForced(enrollment);
                            }
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("27-2");
                    }
                }));

        addFixesByRuleNumber("27", ruleWithFixes);
    }

    /**
     * Initialize rule 28 fix.
     */
    private void initializeRule28Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(
                        Restriction.equals("Prior School County", "99"),
                        Restriction.equals("Prior School State", "ZZ")))
                        .testThen(
                                Restriction.notEquals("Prior School Country", "US")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {


                        System.out.println(bean.getOid());
                        System.out.println("28-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Prior School Country", "US"))
                        .testThen(
                                Restriction.or(
                                        Restriction.notEquals("Prior School County", "99"),
                                        Restriction.notEquals("Prior School State", "ZZ"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Collection<StudentEnrollment> enrollments = getEnrollments((SisStudent) bean);
                        for (StudentEnrollment enrollment : enrollments) {
                            String randomSchoolCounty = null;
                            while (randomSchoolCounty == null || randomSchoolCounty.equals("99")) {
                                randomSchoolCounty = getRandomCodeForField(m_fieldPriorSklCounty);
                            }
                            enrollment.setFieldValueByBeanPath(m_fieldPriorSklCounty.getJavaName(), randomSchoolCounty);
                            getModelBroker().saveBeanForced(enrollment);
                        }

                        System.out.println(bean.getOid());
                        System.out.println("28-2");
                    }
                }));

        addFixesByRuleNumber("28", ruleWithFixes);
    }

    /**
     * Initialize rule 29 fix.
     */
    private void initializeRule29Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Entry Code PK-12", "^R01|R02|R03$"))
                        .testThen(
                                Restriction.equalsFieldValue("Prior School County", "District Number, E",
                                        String.class)),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (StudentEnrollment enrollment : getEnrollments((SisStudent) bean)) {
                            if (enrollment.getEnrollmentCode() != null
                                    && enrollment.getEnrollmentCode().matches("^R01|R02|R03$")) {
                                enrollment.setFieldValueByAlias(ALIAS_ENR_PRIOR_SKL_COUNTY,
                                        getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.ENR,
                                                "District Number, E"));
                                getModelBroker().saveBeanForced(enrollment);
                            }
                            if (!enrollment.getEnrollmentType().equals("E") && enrollment.getEnrollmentCode() != null
                                    && !enrollment.getEnrollmentCode().matches("^[E]|[R].*$")) {
                                enrollment.setFieldValueByAlias(ALIAS_ENR_PRIOR_SKL_COUNTY,
                                        getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.ENR,
                                                "District Number, E"));
                                getModelBroker().saveBeanForced(enrollment);
                            }
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("29");
                    }
                }));

        addFixesByRuleNumber("29", ruleWithFixes);
    }

    /**
     * Initialize rule 2A fix.
     */
    private void initializeRule2AFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E03"))
                        .testThen(Restriction.notEquals("Prior School County", "99")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Collection<StudentEnrollment> enrollments = getEnrollments((SisStudent) bean);
                        for (StudentEnrollment enrollment : enrollments) {
                            String randomSchoolCounty = null;
                            while (randomSchoolCounty == null || randomSchoolCounty.equals("99")) {
                                randomSchoolCounty = getRandomCodeForField(m_fieldPriorSklCounty);
                            }
                            enrollment.setFieldValueByBeanPath(m_fieldPriorSklCounty.getJavaName(), randomSchoolCounty);
                            getModelBroker().saveBeanForced(enrollment);
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("2A-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E3A"))
                        .testThen(Restriction.equals("Prior School County", "99")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Collection<StudentEnrollment> enrollments = getEnrollments((SisStudent) bean);
                        for (StudentEnrollment enrollment : enrollments) {
                            if ("E3A".equals(enrollment.getEnrollmentCode())) {
                                enrollment.setEnrollmentCode("E03");
                                getModelBroker().saveBeanForced(enrollment);
                            }
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("2A-2");
                    }
                }));

        addFixesByRuleNumber("2A", ruleWithFixes);
    }

    /**
     * Initialize rule 2B fix.
     */
    private void initializeRule2BFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E04"))
                        .testThen(Restriction.notEquals("Prior School County", "99")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Collection<StudentEnrollment> enrollments = getEnrollments((SisStudent) bean);
                        for (StudentEnrollment enrollment : enrollments) {
                            String randomSchoolCounty = null;
                            while (randomSchoolCounty == null || randomSchoolCounty.equals("99")) {
                                randomSchoolCounty = getRandomCodeForField(m_fieldPriorSklCounty);
                            }
                            enrollment.setFieldValueByBeanPath(m_fieldPriorSklCounty.getJavaName(), randomSchoolCounty);
                            getModelBroker().saveBeanForced(enrollment);
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("2B-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E4A"))
                        .testThen(
                                Restriction.equals("Prior School County", "99")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                enr.setEnrollmentCode("E04");
                                getModelBroker().saveBeanForced(enr);
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("2B-2");
                    }
                }));

        addFixesByRuleNumber("2B", ruleWithFixes);
    }

    /**
     * Initialize rule 2C fix.
     */
    private void initializeRule2CFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E09"))
                        .testThen(Restriction.equals("Prior School State", "ZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                if (enr.getEnrollmentCode() != null && enr.getEnrollmentCode().equals("E09")) {
                                    String randomEntryCode = null;
                                    while (randomEntryCode == null || randomEntryCode.equals("E09")) {
                                        randomEntryCode = m_entryEnrCodes.get(m_random.nextInt(m_entryEnrCodes.size()));
                                    }
                                    enr.setEnrollmentCode(randomEntryCode);
                                    getModelBroker().saveBeanForced(enr);
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("2C-1");
                    }
                }));

        addFixesByRuleNumber("2C", ruleWithFixes);
    }

    /**
     * Initialize rule 2D fix.
     */
    private void initializeRule2DFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.byAliasFldRefTable("all-enr-PriorSchoolState",
                        "Prior School State")).testThen(
                                Restriction.equals("Prior School Country", "US")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            String term =
                                    getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.ENR, "Term");
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            if ("Y".equals(term)) {
                                for (StudentEnrollmentSpan span : spans) {
                                    StudentEnrollment enr = span.getFirstActiveEnrollment();
                                    enr.setFieldValueByBeanPath(m_fieldPriorSklState.getJavaName(), null);
                                    enr.setFieldValueByBeanPath(m_fieldPriorSklCounty.getJavaName(), "99");
                                    getModelBroker().saveBeanForced(enr);
                                }
                            } else {
                                for (StudentEnrollmentSpan span : spans) {
                                    StudentEnrollment enr = span.getFirstActiveEnrollment();
                                    enr.setFieldValueByBeanPath(m_fieldPriorSklCountry.getJavaName(), "US");
                                    getModelBroker().saveBeanForced(enr);
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("2D-1");
                    }
                }));

        addFixesByRuleNumber("2D", ruleWithFixes);
    }

    /**
     * Initialize rule 2E fix.
     */
    private void initializeRule2EFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Educational Choice", "A")).testThen(
                        Restriction.pattern("District Number, Z", "^(0[1-9]|[1-5][0-9]|6[0-7])$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByAlias(ALIAS_STD_DISTRICT_NUMBER_ZONED,
                                String.format("%02d", Integer.valueOf(ThreadLocalRandom.current().nextInt(1, 67))));
                        getModelBroker().saveBeanForced(bean);
                        // System.out.println(bean.getOid());
                        // System.out.println("2E-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.notEquals("Educational Choice", "A")).testThen(
                        Restriction.pattern("District Number, Z", "^00$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            Collection<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                Collection<StudentEnrollment> enrollments = span.getEnrollments();
                                boolean enrollmentChanged = false;
                                for (StudentEnrollment enrollment : enrollments) {
                                    if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) ||
                                            StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) {
                                        enrollment.setFieldValueByBeanPath(m_fieldEduChoice.getJavaName(), "A");
                                        getModelBroker().saveBeanForced(enrollment);
                                        enrollmentChanged = true;
                                    }
                                }
                                if (!enrollmentChanged) {
                                    bean.setFieldValueByAlias(ALIAS_STD_DISTRICT_NUMBER_ZONED, null);
                                    getModelBroker().saveBeanForced(bean);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("2E-2");
                    }
                }));

        addFixesByRuleNumber("2E", ruleWithFixes);
    }

    /**
     * Initialize rule 2F fix.
     */
    private void initializeRule2FFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Educational Choice", "A")).testThen(
                        Restriction.byActiveSchool("School Number, Z")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByAlias(ALIAS_STD_SCHOOL_NUMBER_ZONED,
                                getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.ENR,
                                        "School Number, E"));
                        getModelBroker().saveBeanForced(bean);
                        // System.out.println(bean.getOid());
                        // System.out.println("2F-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.notEquals("Educational Choice", "A")).testThen(
                        Restriction.pattern("School Number, Z", "^0000$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            Collection<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                Collection<StudentEnrollment> enrollments = span.getEnrollments();
                                boolean enrollmentChanged = false;
                                for (StudentEnrollment enrollment : enrollments) {
                                    if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) ||
                                            StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) {
                                        enrollment.setFieldValueByBeanPath(m_fieldEduChoice.getJavaName(), "A");
                                        getModelBroker().saveBeanForced(enrollment);
                                        enrollmentChanged = true;
                                    }
                                }
                                if (!enrollmentChanged) {
                                    bean.setFieldValueByAlias(ALIAS_STD_SCHOOL_NUMBER_ZONED, null);
                                    getModelBroker().saveBeanForced(bean);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                        // System.out.println(bean.getOid());
                        // System.out.println("2F-2");
                    }
                }));

        addFixesByRuleNumber("2F", ruleWithFixes);
    }

    /**
     * Initialize rule 30 fix.
     */
    private void initializeRule30Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "2"))
                        .testThen(Restriction.equals("Term", "Z")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("30-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "3"))
                        .testThen(Restriction.pattern("Term", "^3|Y$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentScheduleInfo> schInfoList =
                                    ((FLPriorSchoolStatusData) getFLReportData()).m_studentScheduleHelper
                                            .getStudentScheduleInfo((SisStudent) bean);
                            if (schInfoList.isEmpty()) {
                                List<StudentEnrollmentSpan> spans =
                                        getFLReportData().getStudentHelper()
                                                .getStudentEnrollmentSpans((SisStudent) bean, true);
                                for (StudentEnrollmentSpan span : spans) {
                                    StudentEnrollment enr = span.getFirstInactiveEnrollment();
                                    if (enr != null) {
                                        enr.setEnrollmentCode("DNE");
                                        getModelBroker().saveBeanForced(enr);
                                    } else {
                                        StudentEnrollment newWdrawEnr =
                                                new StudentEnrollment(getBroker().getPersistenceKey());
                                        PlainDate startDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                                        PlainDate endDate = getFLReportData().getSurveyPeriod().getEndDate();
                                        Date randomDateBetween =
                                                new Date(ThreadLocalRandom.current().nextLong(startDate.getTime(),
                                                        endDate.getTime()));
                                        newWdrawEnr.setEnrollmentType("W");
                                        newWdrawEnr.setEnrollmentCode("DNE");
                                        newWdrawEnr.setEnrollmentDate(new PlainDate(randomDateBetween));
                                        newWdrawEnr.setStatusCode("Inactive");
                                        newWdrawEnr.setSchoolOid(span.getFirstActiveEnrollment().getSchoolOid());
                                        newWdrawEnr.setStudentOid(span.getFirstActiveEnrollment().getStudentOid());
                                        newWdrawEnr.setYog(span.getFirstActiveEnrollment().getYog());

                                        getFLReportData().getStudentHelper().getStudentEnrollments((SisStudent) bean);
                                        getModelBroker().saveBeanForced(newWdrawEnr);

                                        break;
                                    }
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("30-2");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                        .testThen(Restriction.pattern("Term", "^3|S|Y$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentScheduleInfo> schInfoList =
                                    ((FLPriorSchoolStatusData) getFLReportData()).m_studentScheduleHelper
                                            .getStudentScheduleInfo((SisStudent) bean);
                            Collection<StudentEnrollment> enrollments =
                                    getFLReportData().getStudentHelper().getStudentEnrollments((SisStudent) bean);
                            if (enrollments != null) {
                                for (StudentEnrollment enrollment : enrollments) {
                                    if ("DNE".equals(enrollment.getEnrollmentCode())) {
                                        return;
                                    }
                                }
                                if (schInfoList.isEmpty()) {
                                    if ("STD0000005TP0u".equals(bean.getOid())) {
                                        // System.out.println();
                                    }
                                    List<StudentEnrollmentSpan> spans =
                                            getFLReportData().getStudentHelper()
                                                    .getStudentEnrollmentSpans((SisStudent) bean, true);
                                    for (StudentEnrollmentSpan span : spans) {
                                        StudentEnrollment enr = span.getFirstInactiveEnrollment();
                                        if (enr != null) {
                                            enr.setEnrollmentCode("DNE");
                                            getModelBroker().saveBeanForced(enr);
                                        } else {
                                            StudentEnrollment newWdrawEnr =
                                                    new StudentEnrollment(getBroker().getPersistenceKey());
                                            PlainDate startDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                                            PlainDate endDate = getFLReportData().getSurveyPeriod().getEndDate();
                                            Date randomDateBetween =
                                                    new Date(ThreadLocalRandom.current().nextLong(startDate.getTime(),
                                                            endDate.getTime()));
                                            newWdrawEnr.setEnrollmentType("W");
                                            newWdrawEnr.setEnrollmentCode("DNE");
                                            newWdrawEnr.setEnrollmentDate(new PlainDate(randomDateBetween));
                                            newWdrawEnr.setStatusCode("Inactive");
                                            newWdrawEnr.setSchoolOid(span.getFirstActiveEnrollment().getSchoolOid());
                                            newWdrawEnr.setStudentOid(span.getFirstActiveEnrollment().getStudentOid());
                                            newWdrawEnr.setYog(span.getFirstActiveEnrollment().getYog());

                                            getFLReportData().getStudentHelper()
                                                    .getStudentEnrollments((SisStudent) bean);
                                            getModelBroker().saveBeanForced(newWdrawEnr);

                                            break;
                                        }
                                    }
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("30-3");
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
                ValidationRule.testIf(Restriction.and(
                        Restriction.pattern("Survey Period", "^2|3$"),
                        Restriction.equals("Withdrawal Code", "ZZZ")))
                        .testThen(Restriction.uniqueValue("Student Number", "Survey Period", "Withdrawal Code")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            List<StudentScheduleSpan> scheduleSpans =
                                    getFLReportData().getStudentHelper().getStudentScheduleSpans((SisStudent) bean);
                            Set<String> scheduledSchoolsOids = new HashSet<>();
                            for (StudentScheduleSpan schSpan : scheduleSpans) {
                                scheduledSchoolsOids.add(schSpan.getSection().getSchoolCourse().getSchoolOid());
                            }
                            for (StudentEnrollmentSpan span : spans) {
                                if (!scheduledSchoolsOids.contains(span.getSchool().getOid())) {
                                    Collection<StudentEnrollment> enrollments = span.getEnrollments();
                                    for (StudentEnrollment enr : enrollments) {
                                        getModelBroker().deleteBean(enr);
                                    }
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("31-1");
                    }
                }));

        addFixesByRuleNumber("31", ruleWithFixes);
    }

    /**
     * Initialize rule 33 fix.
     */
    private void initializeRule33Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(
                        Restriction.equals("Entry Code PK-12", "E09"),
                        Restriction.notEquals("Term", "Y")))
                        .testThen(Restriction.notEquals("Prior School Country", "US")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                if (enr.getEnrollmentCode() != null && enr.getEnrollmentCode().equals("E09")) {
                                    String randomEntryCode = null;
                                    while (randomEntryCode == null || randomEntryCode.equals("E09")) {
                                        randomEntryCode = m_entryEnrCodes.get(m_random.nextInt(m_entryEnrCodes.size()));
                                    }
                                    enr.setEnrollmentCode(randomEntryCode);
                                    getModelBroker().saveBeanForced(enr);
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        System.out.println(bean.getOid());
                        System.out.println("33-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(
                        Restriction.notEquals("Prior School Country", "US"),
                        Restriction.notEquals("Term", "Y")))
                        .testThen(Restriction.equals("Entry Code PK-12", "E09")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                if (enr != null) {
                                    enr.setFieldValueByBeanPath(m_fieldPriorSklCountry.getJavaName(), "US");
                                    getModelBroker().saveBeanForced(enr);
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        System.out.println(bean.getOid());
                        System.out.println("33-2");
                    }
                }));

        addFixesByRuleNumber("33", ruleWithFixes);
    }

    /**
     * Initialize rule 35 fix.
     */
    private void initializeRule35Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(
                        Restriction.pattern("Withdrawal Code",
                                "^W06|W07|W08|W8A|W8B|W09|W10|W27|WFT|WFW|WRW|WGA|WGD|WXL|WXT|WXW|WD1|WPO$"))
                        .testThen(Restriction.pattern("Grade Level", "^9|1[0-2]$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstInactiveEnrollment();
                                if (enr != null) {
                                    String randomCode = null;
                                    while (randomCode == null || (randomCode
                                            .matches(
                                                    "^W06|W07|W08|W8A|W8B|W09|W10|W27|WFT|WFW|WRW|WGA|WGD|WXL|WXT|WXW|WD1|WPO$"))
                                            // to avoid error 35
                                            && !randomCode.matches(
                                                    "^DNE|W01|W02|W3A|W3B|W0[4-8]|W8A|W8B|W09|W1[0-3]|W15|W18|W2[1-7]|WFT|WFW|WRW|WGA|WGD|WXL|WXT|WXW|WD1|WPO$")) {
                                        randomCode = m_wdrawEnrCodes.get(m_random.nextInt(m_wdrawEnrCodes.size()));
                                    }
                                    enr.setEnrollmentCode(randomCode);
                                    getModelBroker().saveBeanForced(enr);
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("35");
                    }
                }));

        addFixesByRuleNumber("35", ruleWithFixes);
    }

    /**
     * Initialize rule 36 fix.
     */
    private void initializeRule36Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Educational Choice", "^A|B|C|E|F|M|Z$",
                        "Educational Choice must be A, B, C, E, F, M or Z"),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                List<StudentEnrollment> enrollments = span.getEnrollments();
                                for (StudentEnrollment enr : enrollments) {
                                    if (enr.getEnrollmentType() != null && enr.getEnrollmentType().matches("E|S")) {
                                        String currentEduChoice =
                                                (String) enr.getFieldValueByBeanPath(m_fieldEduChoice.getJavaName());
                                        if (!currentEduChoice.matches("^A|B|C|E|F|M|Z$")) {
                                            enr.setFieldValueByBeanPath(m_fieldEduChoice.getJavaName(),
                                                    getRandomCodeForField(m_fieldEduChoice));
                                            getModelBroker().saveBeanForced(enr);
                                        }
                                    }
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("36");
                    }
                }));

        addFixesByRuleNumber("36", ruleWithFixes);
    }

    /**
     * Initialize rule 38 fix.
     */
    private void initializeRule38Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(
                        Restriction.pattern("Prior School County", "^0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]$"))
                        .testThen(Restriction.equals("Prior School State", "FL")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                // may be adjusted previously by 33-1
                                enr.setFieldValueByBeanPath(m_fieldPriorSklState.getJavaName(), "FL");
                                getModelBroker().saveBeanForced(enr);
                                // System.out.println(bean.getOid());
                                // System.out.println("38");
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }
                    }
                }));

        addFixesByRuleNumber("38", ruleWithFixes);
    }

    /**
     * Initialize rule 42 fix.
     */
    private void initializeRule42Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Prior School County", "99"))
                        .testThen(Restriction.notEquals("Prior School State", "FL")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                enr.setFieldValueByAlias(ALIAS_ENR_PRIOR_SKL_COUNTY,
                                        String.valueOf(ThreadLocalRandom.current().nextInt(01, 68)));
                                getModelBroker().saveBeanForced(enr);
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("42-1");
                    }
                }));

        addFixesByRuleNumber("42", ruleWithFixes);
    }

    /**
     * Initialize rule 43 fix.
     */
    private void initializeRule43Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "2"))
                        .testThen(Restriction.equals("Days Absent Unexcuse", "000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("43-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.notEquals("Survey Period", "2"))
                        .testThen(Restriction.or(
                                Restriction.and(
                                        Restriction.equals("Term", "Y"),
                                        Restriction.equals("Days Absent Unexcuse", "000")),
                                Restriction.pattern("Days Absent Unexcuse", "^\\d{3}$"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println("Term: " +
                        // getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.ENR,
                        // "Term"));
                        // System.out.println("Days Absent Unexcuse: "
                        // + getHelper().getExportFormatRowFieldValue(getCurrentRow(),
                        // FL_EXPORT.ENR, "Days Absent Unexcuse"));

                        // System.out.println(bean.getOid());
                        // System.out.println("43-2");
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
                ValidationRule.testIf(Restriction.equals("Grade Level", "PK"))
                        .testThen(Restriction.notEquals("Educational Choice", "C")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                List<StudentEnrollment> enrollments = span.getEnrollments();
                                for (StudentEnrollment enr : enrollments) {
                                    if (enr.getEnrollmentType() != null && enr.getEnrollmentType().matches("E|S")) {
                                        String currentEduChoice =
                                                (String) enr.getFieldValueByBeanPath(m_fieldEduChoice.getJavaName());
                                        if (currentEduChoice.matches("^C$")) {
                                            String randomEduChoice = null;
                                            while (randomEduChoice == null || randomEduChoice.matches("^C$")) {
                                                randomEduChoice = getRandomCodeForField(m_fieldEduChoice);
                                            }
                                            enr.setFieldValueByBeanPath(m_fieldEduChoice.getJavaName(),
                                                    randomEduChoice);
                                            getModelBroker().saveBeanForced(enr);
                                        }
                                    }
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("46-1");
                    }
                }));

        addFixesByRuleNumber("46", ruleWithFixes);
    }

    /**
     * Initialize rule 47 fix.
     */
    private void initializeRule47Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Withdrawal Code", "W05"))
                        .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(16),
                                null, true, false, "Withdrawal Date")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        String wdrawDateString =
                                getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.ENR,
                                        "Withdrawal Date");
                        if (!StringUtils.isEmpty(wdrawDateString)) {
                            try {
                                Date wdrawDate = DEFAULT_DATE_FORMAT.parse(wdrawDateString);
                                PlainDate randomDateInRange = getDobOnDateForRange(new PlainDate(wdrawDate), true,
                                        false, Integer.valueOf(16), Integer.valueOf(18));
                                SisStudent student = (SisStudent) bean;
                                SisPerson person = student.getPerson();
                                person.setDob(randomDateInRange);
                                getModelBroker().saveBean(person);
                            } catch (ParseException e) {
                                e.printStackTrace();
                            }
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("47-1");
                    }
                }));

        addFixesByRuleNumber("47", ruleWithFixes);
    }

    /**
     * Initialize rule 48 fix.
     */
    private void initializeRule48Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Withdrawal Code", "W25"))
                        .testThen(Restriction.pattern("Grade Level", "^PK|KG$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstInactiveEnrollment();
                                if (enr != null && enr.getEnrollmentCode() != null
                                        && enr.getEnrollmentCode().equals("W25")) {
                                    String randomCode = null;
                                    while (randomCode == null || randomCode.equals("W25")) {
                                        randomCode = m_wdrawEnrCodes.get(m_random.nextInt(m_wdrawEnrCodes.size()));
                                    }
                                    enr.setEnrollmentCode(randomCode);
                                    getModelBroker().saveBeanForced(enr);
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("48-1");
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
                ValidationRule.testIf(Restriction.equals("Withdrawal Code", "W25"))
                        .testThen(Restriction.studentAgeInRange("Student ID Local", null,
                                Integer.valueOf(6), false, false,
                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "02", "01"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Date tillDate = (Date) (new RuntimeParam(RuntimeParam.FISCAL_DATE, "02", "01")
                                .getRuntimeObject(getHelper()));

                        PlainDate randomDateInRange = getDobOnDateForRange(new PlainDate(tillDate), true,
                                false, Integer.valueOf(5), Integer.valueOf(6));
                        SisStudent student = (SisStudent) bean;
                        SisPerson person = student.getPerson();
                        person.setDob(randomDateInRange);
                        getModelBroker().saveBean(person);

                        // System.out.println(bean.getOid());
                        // System.out.println("49-1");
                    }
                }));

        addFixesByRuleNumber("49", ruleWithFixes);
    }

    /**
     * Initialize rule 4D fix.
     */
    private void initializeRule4DFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Prior School State", "FL"))
                        .testThen(Restriction.notEquals("Prior School County", "99")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // System.out.println(bean.getOid());
                        // System.out.println("4D-1");
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.notEquals("Prior School State", "FL"))
                        .testThen(Restriction.equals("Prior School County", "99")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                enr.setFieldValueByBeanPath(m_fieldPriorSklState.getJavaName(), "FL");
                                getModelBroker().saveBeanForced(enr);
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("4D-2");
                    }
                }));

        addFixesByRuleNumber("4D", ruleWithFixes);
    }

    /**
     * Initialize rule 51 fix.
     */
    private void initializeRule51Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Withdrawal Code", "W21"))
                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number, E", "District Number"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.equals("Action Code", "E"), null))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Date randomDateBetween =
                                new Date(ThreadLocalRandom.current().nextLong(
                                        getFLReportData().getSurveyPeriod().getStartDate().getTime(),
                                        getFLReportData().getSurveyPeriod().getEndDate().getTime()));
                        try {
                            ConductIncident incident = new ConductIncident(getBroker().getPersistenceKey());
                            incident.setStudentOid(bean.getOid());
                            incident.setSchoolOid(((SisStudent) bean).getSchoolOid());
                            incident.setIncidentDate(new PlainDate(randomDateBetween));
                            incident.setIncidentId(ConductManager.generateConductIncidentId(getBroker(),
                                    ((SisStudent) bean).getOrganization1(), false));
                            // getModelBroker().saveBeanForced(incident);

                            ConductAction action = new ConductAction(getBroker().getPersistenceKey());
                            action.setStudentOid(bean.getOid());
                            action.setSchoolOid(((SisStudent) bean).getSchoolOid());
                            action.setActionCode("E");
                            action.setActionStartDate(new PlainDate(randomDateBetween));
                            action.setActionEndDate(new PlainDate(randomDateBetween));
                            action.setIncidentOid(incident.getOid());
                            // getModelBroker().saveBeanForced(action);

                            // System.out.println(bean.getOid());
                            // System.out.println("51-1");
                        } catch (InvalidPreferenceException e) {
                            e.printStackTrace();
                        }
                    }
                }));

        addFixesByRuleNumber("51", ruleWithFixes);
    }

    /**
     * Initialize rule 52 fix.
     */
    private void initializeRule52Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        // ruleWithFixes.add(new RuleWithFix(
        // ValidationRule.testIf(Restriction.pattern("Entry Code PK-12", "^R01|R02|R03$"))
        // .testThen(new ValidateEnrMatchInEnrExport("EXPDATA-FL-ENR",
        // new KeyValuePair("Student Number", "Student Number"),
        // new KeyValuePair("District Number, E", "District Number, E"),
        // new KeyValuePair("Survey Period", "Survey Period"),
        // new KeyValuePair("Fiscal Year", "Fiscal Year"),
        // new KeyValuePair(Restriction.pattern("Entry Code PK-12",
        // "^E0[1-5]|E2A|E3A|E4A|E09$"),
        // null),
        // new KeyValuePair(
        // Restriction.lessThanOrEqualsFieldValueWithMatched("Withdrawal Date", "Entry Date",
        // Date.class),
        // null))),
        // new Fix() {
        // @Override
        // protected void fixError(X2BaseBean bean) {
        //
        // // System.out.println(bean.getOid());
        // // System.out.println("52-1");
        // }
        // }));

        addFixesByRuleNumber("52", ruleWithFixes);
    }

    /**
     * Initialize rule 75 fix.
     */
    private void initializeRule75Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^3|5$"))
                        .testThen(Restriction.sumLessThanOrEquals(Double.valueOf(180), "Days Absent Annual",
                                "Days Present Annual")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {


                        // System.out.println(bean.getOid());
                        // System.out.println("75");
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
                ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E05"))
                        .testThen(Restriction.pattern("Grade Level", "^PK|KG$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstActiveEnrollment();
                                if (enr.getEnrollmentCode() != null && enr.getEnrollmentCode().equals("E05")) {
                                    String randomEntryCode = null;
                                    while (randomEntryCode == null || randomEntryCode.equals("E05")) {
                                        randomEntryCode = m_entryEnrCodes.get(m_random.nextInt(m_entryEnrCodes.size()));
                                    }
                                    enr.setEnrollmentCode(randomEntryCode);
                                    getModelBroker().saveBeanForced(enr);
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("77");
                    }
                }));

        addFixesByRuleNumber("77", ruleWithFixes);
    }

    /**
     * Initialize rule 79 fix.
     */
    private void initializeRule79Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Withdrawal Code", "W26"))
                        .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(16),
                                null, true, false, "Withdrawal Date")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        String wdrawDateString =
                                getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.ENR,
                                        "Withdrawal Date");
                        if (!StringUtils.isEmpty(wdrawDateString)) {
                            try {
                                Date wdrawDate = DEFAULT_DATE_FORMAT.parse(wdrawDateString);
                                PlainDate randomDateInRange = getDobOnDateForRange(new PlainDate(wdrawDate), true,
                                        false, Integer.valueOf(16), Integer.valueOf(18));
                                SisStudent student = (SisStudent) bean;
                                SisPerson person = student.getPerson();
                                person.setDob(randomDateInRange);
                                getModelBroker().saveBean(person);
                            } catch (ParseException e) {
                                e.printStackTrace();
                            }
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("79");
                    }
                }));

        addFixesByRuleNumber("79", ruleWithFixes);
    }

    /**
     * Initialize rule 80 fix.
     */
    private void initializeRule80Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.and(
                        Restriction.pattern("Survey Period", "^2|3$"),
                        Restriction.equals("Withdrawal Code", "W02")))
                        .testThen(new ValidateEnrMatchInEnrExport("EXPDATA-FL-ENR",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number, E", "District Number, E"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(
                                        Restriction.and(
                                                Restriction.equals("Entry Code PK-12", "R02"),
                                                Restriction.greaterThanOrEquals("Withdrawal Date", null)),
                                        null))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<StudentEnrollmentSpan> spans =
                                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans((SisStudent) bean,
                                            true);
                            for (StudentEnrollmentSpan span : spans) {
                                StudentEnrollment enr = span.getFirstInactiveEnrollment();
                                if (enr != null && enr.getEnrollmentCode() != null
                                        && enr.getEnrollmentCode().equals("W02")) {
                                    String randomCode = null;
                                    while (randomCode == null || randomCode.equals("W02")) {
                                        randomCode = m_wdrawEnrCodes.get(m_random.nextInt(m_wdrawEnrCodes.size()));
                                    }
                                    enr.setEnrollmentCode(randomCode);
                                    getModelBroker().saveBeanForced(enr);
                                }
                            }
                        } catch (X2BaseException e1) {
                            e1.printStackTrace();
                        }

                        // System.out.println(bean.getOid());
                        // System.out.println("80");
                    }
                }));

        addFixesByRuleNumber("80", ruleWithFixes);
    }

    /**
     * Initialize rule 81 fix.
     */
    private void initializeRule81Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                        .testThen(Restriction.sumLessThanOrEquals(Double.valueOf(70), "Days Absent Summer",
                                "Days Present Summer")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {


                        // System.out.println(bean.getOid());
                        // System.out.println("81");
                    }
                }));

        addFixesByRuleNumber("81", ruleWithFixes);
    }

    private void fixWrongSpans() {
        try {
            getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                    getCurrentContext().getStartDate());
            getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            Collection<SisStudent> students = getModelBroker().getCollectionByQuery(getFLReportData().getQuery());
            for (SisStudent student : students) {
                List<StudentEnrollmentSpan> spans;
                spans = getStudentHelper().getStudentEnrollmentSpans(student, true);
                if (spans != null && !spans.isEmpty()) {
                    TreeSet<StudentEnrollmentSpan> orderedByEnrNumSpans =
                            new TreeSet<>(new Comparator<StudentEnrollmentSpan>() {
                                @Override
                                public int compare(StudentEnrollmentSpan o1, StudentEnrollmentSpan o2) {
                                    return o1.getEnrollments().size() - o2.getEnrollments().size();
                                }
                            });
                    orderedByEnrNumSpans.addAll(spans);
                    Set<StudentEnrollment> firstActiveEnrollments = new HashSet<>();
                    for (StudentEnrollmentSpan span : orderedByEnrNumSpans) {
                        if (!firstActiveEnrollments.add(span.getFirstActiveEnrollment())) {
                            getModelBroker().deleteBean(span.getFirstActiveEnrollment());
                        }
                    }
                }
            }
            reloadHelper();
            getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                    getCurrentContext().getStartDate());
            getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            for (SisStudent student : students) {
                List<StudentEnrollmentSpan> spans = getStudentHelper().getStudentEnrollmentSpans(student, true);
                for (StudentEnrollmentSpan span : spans) {
                    String randomCode = m_entryEnrCodes.get(m_random.nextInt(m_entryEnrCodes.size()));
                    StudentEnrollment enr = span.getFirstActiveEnrollment();
                    enr.setEnrollmentCode(randomCode);
                    getModelBroker().saveBeanForced(enr);
                }
            }
            reloadHelper();
        } catch (X2BaseException e) {
            e.printStackTrace();
        }
    }

    private void changeRefTableOfFields() {
        DataDictionaryField fieldEnrCode =
                getFLReportData().getDataDictionaryField(StudentEnrollment.class,
                        StudentEnrollment.COL_ENROLLMENT_CODE);
        DataFieldConfig fddEnrCode = fieldEnrCode.getDataFieldConfig();
        fddEnrCode.setReferenceTableOid("rtbFlEnrCodes");
        getModelBroker().saveBeanForced(fddEnrCode);
        DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
    }

    /**
     * Gets the dob on date for range.
     *
     * @param date PlainDate
     * @param inclusiveFrom boolean
     * @param inclusiveTo boolean
     * @param from Integer
     * @param to Integer
     * @return Plain date
     */
    private static PlainDate getDobOnDateForRange(PlainDate date,
                                                  boolean inclusiveFrom,
                                                  boolean inclusiveTo,
                                                  Integer from,
                                                  Integer to) {
        if (to == null) {
            to = Integer.valueOf(from.intValue() + 10);
        }

        if (!inclusiveFrom) {
            from = Integer.valueOf(from.intValue() + 1);
        }

        if (!inclusiveTo) {
            to = Integer.valueOf(to.intValue() - 1);
        }

        Calendar cal = Calendar.getInstance();
        cal.setTime(date);

        cal.add(Calendar.YEAR, -to.intValue() - 1);
        cal.add(Calendar.DATE, 1);
        Date rangeStartDate = cal.getTime();

        cal.add(Calendar.YEAR, to.intValue() - from.intValue() + 1);
        cal.add(Calendar.DATE, -2);
        Date rangeEndDate = cal.getTime();

        return new PlainDate(
                new Date(ThreadLocalRandom.current().nextLong(rangeStartDate.getTime(), rangeEndDate.getTime())));
    }

    private Collection<StudentEnrollment> getEnrollments(SisStudent student) {
        Collection<StudentEnrollment> enrollments = new ArrayList<StudentEnrollment>();
        try {
            List<StudentEnrollmentSpan> spans =
                    getFLReportData().getStudentHelper().getStudentEnrollmentSpans(student, true);

            for (StudentEnrollmentSpan span : spans) {
                enrollments.addAll(span.getEnrollments());
            }
        } catch (X2BaseException e1) {
            e1.printStackTrace();
        }
        return enrollments;
    }
}
