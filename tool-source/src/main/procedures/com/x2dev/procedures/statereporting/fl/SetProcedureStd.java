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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.ibm.icu.util.Calendar;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.EnrollmentSnapshot;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.GradeMatcher;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ThreadLocalRandom;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetProcedureStd.
 */
public class SetProcedureStd extends SetProcedure {

    private static final String ALIAS_ADR_COUNTY = "all-adr-County";
    private static final String ALIAS_ELL_CODE = "pgm-ell-code";
    private static final String ALIAS_ELL_SURVEY_DATE = "pgm-survey-date";
    private static final String ALIAS_ENR_RESIDENT_STATUS = "all-enr-ResidentStatus";

    private static final String ALIAS_STD_BIRTH_COUNTRY = "all-std-BirthCountry";
    private static final String ALIAS_STD_NATIVE_LANGUAGE = "all-std-NativeLanguage";
    private static final String ALIAS_STD_PRIMARY_LANGUAGE = "all-std-PrimaryLanguage";
    private static final String ALIAS_STD_STATE_ID = "all-std-StateId";
    private static final String ALIAS_STD_STATE_ID_ALIAS = "all-std-StateIdAlias";

    private static final String DDX_ID_ELL = "FL-PGM-ELL";

    private static final String PROGRAM_CODE_ELL = "ELL";

    private SimpleDateFormat m_dateFormat = new SimpleDateFormat(DateAsStringConverter.STRING_DATE_FORMAT);
    private DataDictionary m_dictionary = null;
    private DataDictionary m_dictionaryEll = null;

    private PlainDate m_endDate = null;
    private DataDictionaryField m_fieldAdrCounty = null;
    private DataDictionaryField m_fieldEllCode;
    private DataDictionaryField m_fieldNativeLang = null;
    private DataDictionaryField m_fieldStdBirthCountry = null;
    private DataDictionaryField m_fieldStdPrimLang = null;
    private GradeMatcher m_gradeMatcher;
    private String[] m_residentStatuses = new String[] {"0", "2"};
    private PlainDate m_startDate = null;
    private FLStudentHelper m_studentHelper = null;

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
    public static PlainDate getDobOnDateForRange(PlainDate date,
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

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        super.execute();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FLExportConfiguration.FL_EXPORT.STD, SisStudent.class);

        m_startDate = getCurrentContext().getStartDate();
        m_endDate = getCurrentContext().getEndDate();

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_fieldAdrCounty = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_ADR_COUNTY);
        m_fieldStdPrimLang = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_STD_PRIMARY_LANGUAGE);
        m_fieldStdBirthCountry = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_STD_BIRTH_COUNTRY);
        StudentProgramDataset pgmDatasetEll =
                getFLReportData().getStudentHelper().getStudentProgramDataset(DDX_ID_ELL,
                        getFLReportData().getSurveyPeriod());

        m_fieldEllCode =
                getFLReportData().translateAliasToDictionaryField(pgmDatasetEll.getDataDictionary(), ALIAS_ELL_CODE,
                        true);
        m_fieldNativeLang = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_STD_NATIVE_LANGUAGE);

        m_studentHelper = getFLReportData().getStudentHelper();
        m_gradeMatcher = m_studentHelper.new GradeMatcher();

        X2Criteria ddxCriteria = new X2Criteria();
        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_ID_ELL);
        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        m_dictionaryEll = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());

        initializeRule4Fix();
        initializeRule7Fix();
        initializeRule11Fix();
        initializeRule18Fix();
        initializeRule29Fix();
        initializeRule34Fix();
        initializeRule39Fix();
        initializeRule45Fix();
        initializeRule46Fix();
        initializeRule48Fix();
        initializeRule49Fix();
        initializeRule63Fix();
    }

    /**
     * Initialize rule 4 fix.
     */
    private void initializeRule4Fix() {
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

        addFixesByRuleNumber("4", ruleWithFixes);
    }

    /**
     * Initialize rule 7 fix.
     */
    private void initializeRule7Fix() {
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
                        getModelBroker().saveBean(bean);
                    }
                }));

        addFixesByRuleNumber("7", ruleWithFixes);
    }

    /**
     * Initialize rule 11 fix.
     */
    private void initializeRule11Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(5|9)$"))
                        .testThen(Restriction.patternForFields(
                                "^(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|A[0-9][0-9][0-9])$",
                                "Institution Number 1", "Institution Number 2",
                                "Institution Number 3")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        String gradeLevel = null;
                        EnrollmentSnapshot snapshot = m_studentHelper.new EnrollmentSnapshot((SisStudent) bean,
                                getFLReportData().getSurveyPeriod().getStartDate());
                        ReferenceCode gradeCode = m_gradeMatcher.getReferenceCode(snapshot.getYog());
                        if (gradeCode != null) {
                            gradeLevel = gradeCode.getCode();
                        }
                        ((SisStudent) bean).setGradeLevel(gradeLevel);
                        getModelBroker().saveBean(bean);
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^([1-4]|7)$"))
                        .testThen(Restriction.patternForFields(
                                "^0000$",
                                "Institution Number 1", "Institution Number 2",
                                "Institution Number 3")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        String gradeLevel = null;
                        EnrollmentSnapshot snapshot = m_studentHelper.new EnrollmentSnapshot((SisStudent) bean,
                                getFLReportData().getSurveyPeriod().getStartDate());
                        ReferenceCode gradeCode = m_gradeMatcher.getReferenceCode(snapshot.getYog());
                        if (gradeCode != null) {
                            gradeLevel = gradeCode.getCode();
                        }
                        ((SisStudent) bean).setGradeLevel(gradeLevel);
                        getModelBroker().saveBean(bean);
                    }
                }));

        addFixesByRuleNumber("11", ruleWithFixes);
    }

    /**
     * Initialize rule 18 fix.
     */
    private void initializeRule18Fix() {
        Map<String, int[]> gradesAgeRange = new HashMap<>();
        gradesAgeRange.put("PK", new int[] {9, 11});
        gradesAgeRange.put("KG", new int[] {11, 12});
        gradesAgeRange.put("1", new int[] {12, 13});
        gradesAgeRange.put("2", new int[] {13, 14});
        gradesAgeRange.put("3", new int[] {14, 15});
        gradesAgeRange.put("4", new int[] {15, 16});
        gradesAgeRange.put("5", new int[] {16, 18});
        gradesAgeRange.put("6", new int[] {18, 19});
        gradesAgeRange.put("7", new int[] {19, 20});
        gradesAgeRange.put("8", new int[] {20, 27});
        gradesAgeRange.put("9", new int[] {27, 28});
        gradesAgeRange.put("10", new int[] {28, 29});
        gradesAgeRange.put("11", new int[] {29, 30});
        gradesAgeRange.put("12", new int[] {30, 40});

        Map<String, RuntimeParam> periodDate = new HashMap<>();
        periodDate.put("^[1-4]$", new RuntimeParam(RuntimeParam.DATE_CERTAIN));
        periodDate.put("5", new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3));
        periodDate.put("6", new RuntimeParam(RuntimeParam.PERIOD_END_DATE));
        periodDate.put("^[89]$", new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2));

        ArrayList<RuleWithFix> rulesWithFix = new ArrayList<>();
        for (Entry<String, RuntimeParam> periodDateEntry : periodDate.entrySet()) {
            String period = periodDateEntry.getKey();
            final RuntimeParam date = periodDateEntry.getValue();
            for (Entry<String, int[]> gradeAgesEntry : gradesAgeRange.entrySet()) {
                final String grade = gradeAgesEntry.getKey();
                final int from = gradeAgesEntry.getValue()[0];
                final int to = gradeAgesEntry.getValue()[1];

                rulesWithFix.add(new RuleWithFix(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", period),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", grade))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(from),
                                        Integer.valueOf(to),
                                        true, false, date)),
                        new Fix() {
                            @Override
                            protected void fixError(X2BaseBean bean) {
                                SisStudent student = (SisStudent) bean;
                                SisPerson person = student.getPerson();
                                PlainDate randomDateInRange =
                                        getDobOnDateForRange((PlainDate) date.getRuntimeObject(getHelper()), true,
                                                false,
                                                Integer.valueOf(from), Integer.valueOf(to));
                                person.setDob(randomDateInRange);
                                getModelBroker().saveBean(person);
                            }
                        }));
            }
        }

        addFixesByRuleNumber("18", rulesWithFix);
    }

    /**
     * Initialize rule 29 fix.
     */
    private void initializeRule29Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2]|3[01])$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        String gradeLevel = null;
                        EnrollmentSnapshot snapshot = m_studentHelper.new EnrollmentSnapshot((SisStudent) bean,
                                getFLReportData().getSurveyPeriod().getStartDate());
                        ReferenceCode gradeCode = m_gradeMatcher.getReferenceCode(snapshot.getYog());
                        if (gradeCode != null) {
                            gradeLevel = gradeCode.getCode();
                        }
                        ((SisStudent) bean).setGradeLevel(gradeLevel);
                        getModelBroker().saveBean(bean);
                    }
                }));

        addFixesByRuleNumber("29", ruleWithFixes);
    }

    /**
     * Initialize rule 34 fix.
     */
    private void initializeRule34Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                        Restriction.notEquals("School Number", "9995"))
                        .testThen(Restriction.pattern("Resident County", "^(?!00$)[0-5][0-9]|6[0-7]|99$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        SisPerson person = student.getPerson();
                        SisAddress address = person.getPhysicalAddress();
                        if (address != null) {
                            String randomResidentCounty = null;
                            while (randomResidentCounty == null
                                    || !randomResidentCounty.matches("^(?!00$)[0-5][0-9]|6[0-7]|99$")) {
                                randomResidentCounty = getRandomCodeForField(m_fieldAdrCounty);
                            }
                            address.setFieldValueByAlias(ALIAS_ADR_COUNTY, randomResidentCounty);
                        } else {
                            System.out.println("person oid " + person.getOid() + " address is null");
                        }

                        getModelBroker().saveBean(address);
                    }
                }));
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                        Restriction.equals("School Number", "9995"))
                        .testThen(Restriction.equals("Resident County", "00")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        SisPerson person = student.getPerson();
                        SisAddress address = person.getPhysicalAddress();
                        if (address != null) {
                            address.setFieldValueByAlias(ALIAS_ADR_COUNTY, "00");
                        } else {
                            System.out.println("person oid " + person.getOid() + " address is null");
                        }

                        getModelBroker().saveBean(address);
                    }
                }));

        addFixesByRuleNumber("34", ruleWithFixes);
    }

    /**
     * Initialize rule 39 fix.
     */
    private void initializeRule39Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                        Restriction.equals("Resident County", "99"),
                        Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"))
                        .testThen(Restriction.pattern("Resident Status", "^(0|2)$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        StudentEnrollment stdEnrollment = m_studentHelper.getEnrollmentForDate(bean.getOid(),
                                getFLReportData().getSurveyPeriod().getSnapshotDate(), StudentEnrollment.ENTRY);
                        stdEnrollment.setFieldValueByAlias(ALIAS_ENR_RESIDENT_STATUS,
                                m_residentStatuses[ThreadLocalRandom.current().nextInt(0, 2)]);
                        getModelBroker().saveBean(stdEnrollment);
                    }
                }));

        addFixesByRuleNumber("39", ruleWithFixes);
    }

    /**
     * Initialize rule 45 fix.
     */
    private void initializeRule45Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                        Restriction.pattern("School Number", "^(?!9992|9993|9995)\\S{4}$"))
                        .testThen(Restriction.byAliasFldRefTable("all-std-PrimaryLanguage",
                                "Primary Language")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByAlias(ALIAS_STD_PRIMARY_LANGUAGE,
                                getRandomCodeForField(m_fieldStdPrimLang));
                        getModelBroker().saveBean(bean);
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                        Restriction.pattern("School Number", "^(9992|9993|9995)$"))
                        .testThen(Restriction.equals("Primary Language", "ZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByAlias(ALIAS_STD_PRIMARY_LANGUAGE, "ZZ");
                        getModelBroker().saveBean(bean);
                    }
                }));

        addFixesByRuleNumber("45", ruleWithFixes);
    }

    /**
     * Initialize rule 46 fix.
     */
    private void initializeRule46Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                        Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                        Restriction.pattern("School Number", "^(?!9992|9993|9995)\\S{4}$"))
                        .testThen(Restriction.and(
                                Restriction.byAliasFldRefTable("all-std-BirthCountry", "Country of Birth"),
                                Restriction.notEquals("Country of Birth", "ZZ"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByAlias(ALIAS_STD_BIRTH_COUNTRY,
                                getRandomCodeForField(m_fieldStdBirthCountry));
                        getModelBroker().saveBean(bean);
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                        Restriction.pattern("Grade Level", "^3[01]$"))
                        .testThen(Restriction.or(
                                Restriction.byAliasFldRefTable("all-std-BirthCountry", "Country of Birth"),
                                Restriction.equals("Country of Birth", "ZZ"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByAlias(ALIAS_STD_BIRTH_COUNTRY,
                                getRandomCodeForField(m_fieldStdBirthCountry));
                        getModelBroker().saveBean(bean);
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                        Restriction.pattern("School Number", "^(9992|9993|9995)$"))
                        .testThen(Restriction.equals("Country of Birth", "ZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByAlias(ALIAS_STD_BIRTH_COUNTRY, "ZZ");
                        getModelBroker().saveBean(bean);
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
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                        Restriction.equals("Grade Level", "^3[01]$"),
                        Restriction.pattern("School Number", "^(9992|9993|9995|9997)$"))
                        .testThen(Restriction.equals("ELL Survey Date", "00000000")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            StudentProgramParticipation pgm =
                                    getFLReportData().getStudentHelper().getStudentProgram(bean.getOid(), DDX_ID_ELL,
                                            getFLReportData().getSurveyPeriod());
                            if (pgm != null) {
                                pgm.setFieldValueByAlias(ALIAS_ELL_SURVEY_DATE, null, m_dictionaryEll);
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                        Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                        Restriction.pattern("School Number", "^(?!9992|9993|9995|9997)\\S{4}$"))
                        .testThen(Restriction.byDateFormat("MMddyyyy", "ELL Survey Date")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            StudentProgramParticipation pgm =
                                    getFLReportData().getStudentHelper().getStudentProgram(bean.getOid(), DDX_ID_ELL,
                                            getFLReportData().getSurveyPeriod());
                            if (pgm == null) {
                                pgm = new StudentProgramParticipation(getBroker().getPersistenceKey());
                                pgm.setStudentOid(bean.getOid());
                                pgm.setExtendedDataDictionaryOid(m_dictionaryEll.getExtendedDictionaryOid());

                                PlainDate programStartDate = null;
                                StudentEnrollment nearestEnrollment =
                                        getFLReportData().getStudentHelper().getEnrollmentForDate(bean.getOid(),
                                                m_startDate,
                                                "EW");
                                if (nearestEnrollment == null) {
                                    programStartDate = m_startDate;
                                } else {
                                    if (!nearestEnrollment.getEnrollmentDate().before(m_startDate)) {
                                        programStartDate = nearestEnrollment.getEnrollmentDate();
                                    } else {
                                        programStartDate = new PlainDate(new Date(
                                                ThreadLocalRandom.current().nextLong(
                                                        nearestEnrollment.getEnrollmentDate().getTime(),
                                                        m_startDate.getTime())));
                                    }
                                }

                                pgm.setStartDate(programStartDate);
                                pgm.setProgramCode(PROGRAM_CODE_ELL);
                            }
                            String dateString = m_dateFormat.format(
                                    new Date(ThreadLocalRandom.current().nextLong(m_startDate.getTime(),
                                            m_endDate.getTime())));
                            pgm.setFieldValueByAlias(ALIAS_ELL_SURVEY_DATE, dateString, m_dictionaryEll);
                            pgm.setFieldValueByAlias(ALIAS_ELL_CODE, getRandomCodeForField(m_fieldEllCode),
                                    m_dictionaryEll);
                            getBroker().saveBean(pgm, m_dictionaryEll);
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[13]$"),
                        Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                        Restriction.pattern("School Number", "^(?!9992|9993|9995|9997)\\S{4}$"),
                        Restriction.or(Restriction.pattern("District Number, E", "^0[1-9]|[1-6][0-8]|7[1-5]$"),
                                Restriction.and(Restriction.equals("District Number, E", "71"),
                                        Restriction.equals("District Number, I/S", "71"))))
                        .testThen(Restriction.lessThanOrEquals("Birth Date",
                                new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        SisPerson person = student.getPerson();
                        person.setDob(new PlainDate(new Date(ThreadLocalRandom.current().nextLong(m_startDate.getTime(),
                                ((PlainDate) new RuntimeParam(RuntimeParam.DATE_CERTAIN).getRuntimeObject(getHelper()))
                                        .getTime()))));
                        getModelBroker().saveBean(person);
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[13]$"),
                        Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                        Restriction.pattern("School Number", "^(?!9992|9993|9995|9997)\\S{4}$"),
                        Restriction.or(Restriction.pattern("District Number, E", "^0[1-9]|[1-6][0-9]|7[1-5]$"),
                                Restriction.and(Restriction.notEquals("District Number, E", "71"),
                                        Restriction.equals("District Number, I/S", "71"))))
                        .testThen(Restriction.lessThanOrEquals("Birth Date",
                                new RuntimeParam(RuntimeParam.DATE_CERTAIN_PLUS_90))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        SisPerson person = student.getPerson();
                        person.setDob(new PlainDate(new Date(ThreadLocalRandom.current().nextLong(m_startDate.getTime(),
                                ((PlainDate) new RuntimeParam(RuntimeParam.DATE_CERTAIN_PLUS_90)
                                        .getRuntimeObject(getHelper()))
                                                .getTime()))));
                        getModelBroker().saveBean(person);
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "4"),
                        Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                        Restriction.pattern("School Number", "^(?!9992|9993|9995|9997)\\S{4}$"))
                        .testThen(Restriction.lessThanOrEquals("Birth Date",
                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        SisPerson person = student.getPerson();
                        person.setDob(new PlainDate(new Date(ThreadLocalRandom.current().nextLong(m_startDate.getTime(),
                                ((PlainDate) new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30")
                                        .getRuntimeObject(getHelper()))
                                                .getTime()))));
                        getModelBroker().saveBean(person);
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                        Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                        Restriction.pattern("School Number", "^(?!9992|9993|9995|9997)\\S{4}$"))
                        .testThen(Restriction.lessThanOrEquals("Birth Date",
                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        SisPerson person = student.getPerson();
                        person.setDob(new PlainDate(new Date(ThreadLocalRandom.current().nextLong(m_startDate.getTime(),
                                ((PlainDate) new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")
                                        .getRuntimeObject(getHelper()))
                                                .getTime()))));
                        getModelBroker().saveBean(person);
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "9"),
                        Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                        Restriction.pattern("School Number", "^(?!9992|9993|9995|9997)\\S{4}$"))
                        .testThen(Restriction.lessThanOrEquals("Birth Date",
                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "01", "01"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        SisPerson person = student.getPerson();
                        person.setDob(new PlainDate(new Date(ThreadLocalRandom.current().nextLong(m_startDate.getTime(),
                                ((PlainDate) new RuntimeParam(RuntimeParam.FISCAL_DATE, "01", "01")
                                        .getRuntimeObject(getHelper()))
                                                .getTime()))));
                        getModelBroker().saveBean(person);
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
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                        Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                        Restriction.pattern("School Number", "^(?!9992|9993|9995)\\S{4}$"))
                        .testThen(Restriction.notEquals("Native Language", "ZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByAlias(ALIAS_STD_NATIVE_LANGUAGE, getRandomCodeForField(m_fieldNativeLang));
                        getModelBroker().saveBean(bean);
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                        Restriction.pattern("School Number", "^[9992|9993|9995]$"))
                        .testThen(Restriction.equals("Native Language", "ZZ")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        bean.setFieldValueByAlias(ALIAS_STD_NATIVE_LANGUAGE, "ZZ");
                        getModelBroker().saveBean(bean);
                    }
                }));

        addFixesByRuleNumber("49", ruleWithFixes);
    }

    /**
     * Initialize rule 63 fix.
     */
    private void initializeRule63Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Grade Level", "^(KG|0[1-9]|1[0-2])$"),
                        Restriction.pattern("School Number", "^(?!9992|9993|9995)\\S{4}$"))
                        .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(5), null,
                                true, false, new RuntimeParam(RuntimeParam.FISCAL_DATE, "09", "01"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        SisPerson person = student.getPerson();
                        PlainDate randomDateInRange =
                                getDobOnDateForRange(
                                        new PlainDate(
                                                ((Date) new RuntimeParam(RuntimeParam.FISCAL_DATE, "09", "01")
                                                        .getRuntimeObject(getHelper()))),
                                        true,
                                        false,
                                        Integer.valueOf(5), Integer.valueOf(6));
                        person.setDob(randomDateInRange);
                        getModelBroker().saveBean(person);
                    }
                }));

        addFixesByRuleNumber("63", ruleWithFixes);
    }
}
