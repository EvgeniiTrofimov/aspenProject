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

import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.InvalidPreferenceException;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentConductDataset;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.ConductManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The Class SetProcedureSESIR.
 */
public class SetProcedureSESIR extends SetProcedure {

    private static final String ALIAS_ALCOHOL_RELATED = "all-cnd-AlcoholRelated";
    private static final String ALIAS_CONTEXT = "all-cnd-Context";
    private static final String ALIAS_DRUG_RELATED = "all-cnd-DrugRelated";
    private static final String ALIAS_DRUG_TYPE = "all-cnd-DrugType";
    private static final String ALIAS_HOMICIDE_VICTIMS = "all-cnd-HomicideVictims";
    private static final String ALIAS_INCIDENT_TYPE = "all-cnd-IncidentType";
    private static final String ALIAS_INJURY_RELATED = "all-cnd-InjuryRelated";
    private static final String ALIAS_LAW_ENFORSMENT = "all-cnd-ReportedToLawEnforcement";
    private static final String ALIAS_OFFENDER_TYPE = "all-cnd-OffenderType";
    private static final String ALIAS_WEAPON_RELATED = "all-cnd-WeaponRelated";
    private static final String ALIAS_WEAPON_TYPE = "all-cnd-WeaponType";

    private StudentConductDataset m_conductDataset = null;
    private DataDictionary m_dictionary = null;
    private DataDictionaryField m_fieldAlcoholRelated;
    private DataDictionaryField m_fieldContext;
    private DataDictionaryField m_fieldDrugRelated;
    private DataDictionaryField m_fieldDrugType;
    private DataDictionaryField m_fieldHomicideVictims;
    private DataDictionaryField m_fieldIncidentType;
    private DataDictionaryField m_fieldInjuryRelated;
    private DataDictionaryField m_fieldOffenderType;
    private DataDictionaryField m_fieldReportedToLawEnforcement;
    private DataDictionaryField m_fieldWeaponRelated;
    private DataDictionaryField m_fieldWeaponType;

    private List<List<ConductIncident>> m_incidentList = null;
    private Random m_random = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        addIncidents();
        runAndValidateExports();
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
        super.initialize(FLExportConfiguration.FL_EXPORT.SESIR, Organization.class);
        includeExports(Arrays.asList(FL_EXPORT.SDRA));
        m_random = new Random(System.currentTimeMillis());

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_fieldContext = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_CONTEXT);
        m_fieldIncidentType = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_INCIDENT_TYPE);
        m_fieldWeaponType = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_WEAPON_TYPE);
        m_fieldWeaponRelated = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_WEAPON_RELATED);
        m_fieldAlcoholRelated = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_ALCOHOL_RELATED);
        m_fieldDrugType = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_DRUG_TYPE);
        m_fieldDrugRelated = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_DRUG_RELATED);
        m_fieldHomicideVictims = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_HOMICIDE_VICTIMS);
        m_fieldInjuryRelated = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_INJURY_RELATED);
        m_fieldReportedToLawEnforcement = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_LAW_ENFORSMENT);
        m_fieldOffenderType = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_OFFENDER_TYPE);

        m_conductDataset =
                getFLReportData().getStudentHelper().getStudentConductDataset(getCurrentContext().getStartDate(),
                        getFLReportData().getSurveyPeriod().getEndDate());
        m_incidentList = new ArrayList(m_conductDataset.getConductIncidentsByIdMap().values());

        initializeRule6Fix();
        initializeRule7Fix();
        initializeRule8Fix();
        initializeRule14Fix();
        initializeRule18Fix();
        initializeRule19Fix();
        initializeRule24Fix();
        initializeRule29Fix();
        initializeRule30Fix();
        initializeRule32Fix();
        initializeRule36Fix();
        initializeRule42Fix();
        initializeRule60Fix();
        initializeRule26Fix();
    }

    private void addIncidents() throws X2BaseException {

        int numOfIncidents = 10;

        int currIncidents = m_conductDataset.getConductIncidentsByIdMap().size();
        if (currIncidents < numOfIncidents) {
            try {
                FLStudentHelper helper = new FLStudentHelper(getFLReportData());

                helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
                helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                        getCurrentContext().getStartDate());
                helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getCurrentContext().getEndDate());

                ArrayList<SisStudent> newStudents =
                        new ArrayList<SisStudent>(getBroker().getCollectionByQuery(helper.getStudentQuery(false)));
                for (int i = 0; i <= numOfIncidents; i++) {
                    SisStudent randomStudent = newStudents.get(m_random.nextInt(newStudents.size()));
                    ConductIncident inc = getNewIncident(randomStudent);
                    getModelBroker().saveBean(inc);
                }
            } catch (X2BaseException e) {
                e.printStackTrace();
            }

        }
    }

    private ConductIncident getNewIncident(SisStudent student) throws X2BaseException {
        ConductIncident newIncident =
                new ConductIncident(getBroker().getPersistenceKey());
        newIncident.setSchoolOid(student.getSchoolOid());
        newIncident.setStudentOid(student.getOid());
        newIncident.setFieldValueByBeanPath(m_fieldIncidentType.getJavaName(), getRandomCodeForField(m_fieldIncidentType));
        try {
            newIncident.setIncidentId(ConductManager.generateConductIncidentId(getBroker(),
                    student.getOrganization1(), false));
        } catch (InvalidPreferenceException e) {
            e.printStackTrace();
        }
        newIncident.setIncidentLocation("" + ThreadLocalRandom.current().nextInt(1, 4));
        newIncident.setIncidentDate(
                new PlainDate((Date) new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "02")
                        .getRuntimeObject(getHelper())));
        newIncident.setFieldValueByBeanPath(m_fieldContext.getJavaName(), getRandomCodeForField(m_fieldContext));
        newIncident.setFieldValueByBeanPath(m_fieldReportedToLawEnforcement.getJavaName(),
                getRandomCodeForField(m_fieldReportedToLawEnforcement));
        String weapRealated = getRandomCodeForField(m_fieldWeaponRelated);
        newIncident.setFieldValueByBeanPath(m_fieldWeaponRelated.getJavaName(), getRandomCodeForField(m_fieldWeaponRelated));
        if (weapRealated.matches("[1-4]")) {
            newIncident.setFieldValueByBeanPath(m_fieldWeaponType.getJavaName(), getRandomCodeForField(m_fieldWeaponType));
        }

        return newIncident;
    }

    /**
     * Initialize rule 14 fix.
     */
    private void initializeRule14Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression(
                        "Drug-Related", "^(Y|N|Z)$",
                        "Incident, Drug-Related code must be Y, N or Z."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                String value =
                                        (String) inc.getFieldValueByBeanPath(m_fieldIncidentType.getJavaName());
                                if (value.contentEquals("DRD") || value.contentEquals("DRU")) {
                                    inc.setFieldValueByBeanPath(m_fieldDrugRelated.getJavaName(), "Y");
                                    getModelBroker().saveBeanForced(inc);
                                }
                            }
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
                new ValidateRegularExpression(
                        "Incident, Context", "^[1-3]$",
                        "Incident, Context code must be 1, 2 or 3."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                inc.setFieldValueByBeanPath(m_fieldContext.getJavaName(),
                                        "" + ThreadLocalRandom.current().nextInt(1, 4));
                                getModelBroker().saveBean(inc);
                            }
                        }

                    }
                }));
        addFixesByRuleNumber("18", ruleWithFixes);
    }

    /**
     * Initialize rule 19 fix.
     */
    private void initializeRule19Fix() {
        final String pattern12 = "^(K|O|U)$";
        final String pattern34 = "^(F|H|M|R)$";
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Weapon-Related", "^[1-2]$"))
                                .testThen(Restriction.pattern("Weapon, Description", "^(K|O|U)$")),
                        ValidationRule.testIf(Restriction.pattern("Weapon-Related", "^[3-4]$"))
                                .testThen(Restriction.pattern("Weapon, Description", "^(F|H|M|R)$"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            for (ConductIncident cnd : cndList) {
                                String weaponRealated = (String) cnd.getFieldValueByBeanPath(m_fieldWeaponRelated.getJavaName());
                                if (weaponRealated != null && weaponRealated.matches("^[1-2]$")) {
                                    String randomCode = getRandomCodeForFieldByPattern(m_fieldWeaponType, pattern12);
                                    cnd.setFieldValueByBeanPath(m_fieldWeaponType.getJavaName(), randomCode);
                                    getModelBroker().saveBeanForced(cnd);
                                }
                                if (weaponRealated != null && weaponRealated.matches("^[3-4]$")) {
                                    String randomCode = getRandomCodeForFieldByPattern(m_fieldWeaponType, pattern34);
                                    cnd.setFieldValueByBeanPath(m_fieldWeaponType.getJavaName(), randomCode);
                                    getModelBroker().saveBeanForced(cnd);
                                }
                            }
                        }
                    }
                }));
        addFixesByRuleNumber("19", ruleWithFixes);
    }

    /**
     * Initialize rule 24 fix.
     */
    private void initializeRule24Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^ALC$"))
                        .testThen(Restriction.pattern("Alcohol-Related", "^Y$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                String value =
                                        (String) inc.getFieldValueByBeanPath(m_fieldIncidentType.getJavaName());
                                if (value.contentEquals("ALC") || inc.getIncidentCode().contentEquals("ALC")) {
                                    inc.setFieldValueByBeanPath(m_fieldAlcoholRelated.getJavaName(), "Y");
                                    getModelBroker().saveBean(inc);
                                }
                            }
                        }
                    }
                }));

        addFixesByRuleNumber("24", ruleWithFixes);
    }

    /**
     * Initialize rule 26 fix.
     */
    private void initializeRule26Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Weapon, Description", "^(K|O|U)$"))
                                .testThen(Restriction.pattern("Weapon-Related", "^(1|2)$")),
                        ValidationRule.testIf(Restriction.pattern("Weapon, Description", "^(F|H|M|R)$"))
                                .testThen(Restriction.pattern("Weapon-Related", "^(3|4)$"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                String value =
                                        (String) inc.getFieldValueByBeanPath(m_fieldWeaponType.getJavaName());
                                if (value != null) {
                                    Matcher matcher = Pattern.compile("^(F|H|M|R)$").matcher(value);
                                    if (matcher.find()) {
                                        inc.setFieldValueByBeanPath(m_fieldWeaponRelated.getJavaName(),
                                                String.valueOf(ThreadLocalRandom.current().nextInt(3, 5)));
                                        getModelBroker().saveBean(inc);
                                    }
                                    matcher = Pattern.compile("^(K|O|U)$").matcher(value);
                                    if (matcher.find()) {
                                        inc.setFieldValueByBeanPath(m_fieldWeaponRelated.getJavaName(),
                                                String.valueOf(ThreadLocalRandom.current().nextInt(1, 3)));
                                        getModelBroker().saveBean(inc);
                                    }
                                }
                            }
                        }
                    }
                }));

        addFixesByRuleNumber("26", ruleWithFixes);
    }

    /**
     * Initialize rule 29 fix.
     */
    private void initializeRule29Fix() {
        final String pattern = "^(M|N|O)$";
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Drug Description", pattern))
                        .testThen(Restriction.pattern("Drug-Related", "^Y$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                String value =
                                        (String) inc.getFieldValueByBeanPath(m_fieldDrugType.getJavaName());
                                if (value != null) {
                                    if (value.matches(pattern)) {
                                        inc.setFieldValueByBeanPath(m_fieldDrugRelated.getJavaName(), "Y");
                                        getModelBroker().saveBeanForced(inc);
                                    }
                                }
                            }
                        }
                    }
                }));

        addFixesByRuleNumber("29", ruleWithFixes);
    }

    /**
     * Initialize rule 30 fix.
     */
    private void initializeRule30Fix() {
        final String pattern = "^(M|N|O)$";
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(DRD|DRU)$"))
                        .testThen(Restriction.pattern("Drug Description", pattern)),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                String value =
                                        (String) inc.getFieldValueByBeanPath(m_fieldIncidentType.getJavaName());
                                if (value.contentEquals("DRD") || value.contentEquals("DRU")) {
                                    inc.setFieldValueByBeanPath(m_fieldDrugType.getJavaName(),
                                            getRandomCodeForFieldByPattern(m_fieldDrugType, pattern));
                                    getModelBroker().saveBeanForced(inc);
                                }
                            }
                        }
                    }
                }));

        addFixesByRuleNumber("30", ruleWithFixes);
    }

    /**
     * Initialize rule 32 fix.
     */
    private void initializeRule32Fix() {
        final String pattern = "^(E|F|O|S|Z)$";
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression(
                        "Victims of Homicide", pattern,
                        "Victims of Homicide code must be E, F, O, S, or Z."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                String value =
                                        (String) inc.getFieldValueByBeanPath(m_fieldHomicideVictims.getJavaName());
                                if (value != null) {
                                    if (!value.matches(pattern)) {
                                        inc.setFieldValueByBeanPath(m_fieldHomicideVictims.getJavaName(),
                                                getRandomCodeForFieldByPattern(m_fieldHomicideVictims, pattern));
                                        getModelBroker().saveBeanForced(inc);
                                    }
                                }
                            }
                        }
                    }
                }));

        addFixesByRuleNumber("32", ruleWithFixes);
    }

    /**
     * Initialize rule 36 fix.
     */
    private void initializeRule36Fix() {
        final String pattern = "^(A|B|Z)$";
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression(
                        "Injury-Related", "^(A|B|Z)$",
                        "Incident, Injury-Related code must be A, B or Z."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                String value =
                                        (String) inc.getFieldValueByBeanPath(m_fieldInjuryRelated.getJavaName());
                                if (value != null && !value.matches(pattern)) {
                                    inc.setFieldValueByBeanPath(m_fieldInjuryRelated.getJavaName(),
                                            getRandomCodeForFieldByPattern(m_fieldInjuryRelated, pattern));
                                    getModelBroker().saveBeanForced(inc);
                                }
                            }
                        }
                    }
                }));

        addFixesByRuleNumber("36", ruleWithFixes);
    }

    /**
     * Initialize rule 42 fix.
     */
    private void initializeRule42Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Weapon-Related", "^[1-4]$"))
                        .testThen(Restriction.pattern("Rep Law Enforcement", "^Y$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                String value =
                                        (String) inc.getFieldValueByBeanPath(m_fieldWeaponRelated.getJavaName());
                                if (value != null) {
                                    Matcher matcher = Pattern.compile("^[1-4]$").matcher(value);
                                    if (matcher.find()) {
                                        inc.setFieldValueByBeanPath(m_fieldReportedToLawEnforcement.getJavaName(), "Y");
                                        getModelBroker().saveBean(inc);
                                    }
                                }
                            }
                        }
                    }
                }));

        addFixesByRuleNumber("42", ruleWithFixes);
    }

    /**
     * Initialize rule 60 fix.
     */
    private void initializeRule60Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Weapon-Related", "^[1-4]$"))
                        .testThen(Restriction.pattern("Rep Law Enforcement", "^Y$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                String value =
                                        (String) inc.getFieldValueByBeanPath(m_fieldWeaponRelated.getJavaName());
                                if (value != null) {
                                    Matcher matcher = Pattern.compile("^[1-4]$").matcher(value);
                                    if (matcher.find()) {
                                        inc.setFieldValueByBeanPath(m_fieldReportedToLawEnforcement.getJavaName(), "Y");
                                        getModelBroker().saveBean(inc);
                                    }
                                }
                            }
                        }
                    }
                }));

        addFixesByRuleNumber("60", ruleWithFixes);
    }

    /**
     * Initialize rule 6 fix.
     */
    private void initializeRule6Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression(
                        "Incident, Identifier", "^(?!00000000)\\S+$",
                        "Incident, Identifier must be alphanumeric, may not be zero, and must not "
                                + "contain blanks."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                try {
                                    inc.setIncidentId(ConductManager.generateConductIncidentId(getBroker(),
                                            ((SisStudent) bean).getOrganization1(), false));
                                } catch (InvalidPreferenceException e) {
                                    e.printStackTrace();
                                }
                                getModelBroker().saveBean(inc);
                            }
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
                ValidationRule.testIf(Restriction.alwaysTrue()).testThen(Restriction.and(
                        Restriction.byDateFormat("Incident, Date"),
                        Restriction.greaterThanOrEquals("Incident, Date",
                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                        Restriction.lessThanOrEquals("Incident, Date",
                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")),
                        Restriction.lessThanOrEquals("Incident, Date",
                                new RuntimeParam(RuntimeParam.PERIOD_END_DATE)))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                inc.setIncidentDate(
                                        new PlainDate((Date) new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "02")
                                                .getRuntimeObject(getHelper())));
                                getModelBroker().saveBean(inc);
                            }
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
                new ValidateRegularExpression(
                        "Incident, Location", "^[1-3]$",
                        "Incident, Location must be 1, 2 or 3."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        for (List<ConductIncident> cndList : m_incidentList) {
                            ConductIncident inc = cndList.get(0);
                            if (inc != null) {
                                inc.setIncidentLocation("" + ThreadLocalRandom.current().nextInt(1, 4));
                                getModelBroker().saveBean(inc);
                            }
                        }

                    }
                }));
        addFixesByRuleNumber("8", ruleWithFixes);
    }
}
