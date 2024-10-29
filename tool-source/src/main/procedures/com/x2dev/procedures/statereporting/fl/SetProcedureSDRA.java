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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetProcedureSDRA.
 */
public class SetProcedureSDRA extends SetProcedure {

    private static final String ALIAS_LUNCH_STATUS_CODE = "pgm-lunch-status";
    private static final String ALIAS_STD_STATE_ID = "all-std-StateId";
    private static final String DDX_ID_LUNCH = "FL-PGM-LUNCH";
    private static final String PROGRAM_CODE_LUNCH = "LUNCH";
    private Map<String, List<String>> m_codesForFields = null;
    private DataDictionary m_dictionaryLunch = null;
    private DataDictionaryField m_fieldLunchStatusCode;
    private PlainDate m_startDate = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        // DataDictionaryField actField =
        // getFLReportData().getDataDictionary().findDataDictionaryField(ConductAction.class.getName(),
        // ConductAction.COL_ACTION_CODE);
        // adjustRefTableForField(actField, ACTION_CODES_REF_TABLE_OID);
        // DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
        runAndValidateExports();
        super.execute();
        // runAndValidateExports();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FLExportConfiguration.FL_EXPORT.SDRA, SisStudent.class);

        m_startDate = getCurrentContext().getStartDate();

        StudentProgramDataset pgmDatasetLunch =
                getFLReportData().getStudentHelper().getStudentProgramDataset(DDX_ID_LUNCH,
                        getFLReportData().getSurveyPeriod());

        m_fieldLunchStatusCode =
                getFLReportData().translateAliasToDictionaryField(pgmDatasetLunch.getDataDictionary(),
                        ALIAS_LUNCH_STATUS_CODE,
                        true);

        X2Criteria ddxCriteria = new X2Criteria();
        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_ID_LUNCH);
        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        m_dictionaryLunch = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());

        initializeRule10Fix();
        initializeRule2KFix();
        initializeRule3Fix();
        initializeRule27Fix();
        initializeRule35Fix();
        initializeRule41Fix();
    }

    /**
     * Gets the reference codes for field.
     *
     * @param field DataDictionaryField
     * @return List
     */
    private List<String> getRefCodesForField(DataDictionaryField field) {
        if (m_codesForFields == null) {
            m_codesForFields = new HashMap<>();
        }
        List<String> codesForField = m_codesForFields.get(field.getJavaName());
        if (codesForField == null) {
            codesForField = new ArrayList<String>();
            ReferenceTable refTable = field.getReferenceTable();
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes();
                for (ReferenceCode code : codes) {
                    codesForField.add(code.getCode());
                }
                m_codesForFields.put(field.getJavaName(), codesForField);
            }
        }
        return codesForField;
    }

    /**
     * Initialize rule 10 fix.
     */
    private void initializeRule10Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.and(
                                Restriction.byDateFormat("Incident Date"),
                                Restriction.greaterThanOrEquals("Incident Date",
                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                                Restriction.lessThanOrEquals("Incident Date",
                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")),
                                Restriction.lessThanOrEquals("Incident Date",
                                        new RuntimeParam(RuntimeParam.PERIOD_END_DATE)))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        X2Criteria incidentCriteria = new X2Criteria();
                        incidentCriteria.addEqualTo(ConductIncident.COL_STUDENT_OID, student.getOid());
                        QueryByCriteria incidentQuery =
                                new QueryByCriteria(ConductIncident.class, incidentCriteria, true);
                        QueryIterator iterator = getBroker().getIteratorByQuery(incidentQuery);
                        while (iterator.hasNext()) {
                            ConductIncident inc = (ConductIncident) iterator.next();
                            inc.setIncidentDate(
                                    new PlainDate((Date) new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "02")
                                            .getRuntimeObject(getHelper())));
                            getBroker().saveBeanForced(inc);
                        }
                    }
                }));
        addFixesByRuleNumber("10", ruleWithFixes);
    }

    /**
     * Initialize rule 27 fix.
     */
    private void initializeRule27Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Lunch Status", "^(0|1|3|4|C|D|E|F|N|R)$",
                        "Lunch Status Must be 0, 1, 3, 4, C, D, E, F, N or R."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            StudentProgramParticipation pgm =
                                    getFLReportData().getStudentHelper().getStudentProgram(bean.getOid(), DDX_ID_LUNCH,
                                            getFLReportData().getSurveyPeriod());
                            if (pgm == null) {
                                pgm = new StudentProgramParticipation(getBroker().getPersistenceKey());
                                pgm.setStudentOid(bean.getOid());
                                pgm.setExtendedDataDictionaryOid(m_dictionaryLunch.getExtendedDictionaryOid());

                                PlainDate programStartDate = null;
                                StudentEnrollment nearestEnrollment =
                                        getFLReportData().getStudentHelper().getEnrollmentForDate(bean.getOid(),
                                                m_startDate,
                                                "EW");
                                if (nearestEnrollment == null
                                        || !nearestEnrollment.getEnrollmentDate().before(m_startDate)) {
                                    programStartDate = m_startDate;
                                } else {
                                    programStartDate = new PlainDate(new Date(
                                            ThreadLocalRandom.current().nextLong(
                                                    nearestEnrollment.getEnrollmentDate().getTime(),
                                                    m_startDate.getTime())));
                                }
                                pgm.setStartDate(programStartDate);
                                pgm.setProgramCode(PROGRAM_CODE_LUNCH);
                            }
                            m_fieldLunchStatusCode =
                                    m_dictionaryLunch.findDataDictionaryFieldByAlias(ALIAS_LUNCH_STATUS_CODE);
                            List<String> lunchStatusRefCodes = getRefCodesForField(m_fieldLunchStatusCode);
                            pgm.setFieldValueByBeanPath(m_fieldLunchStatusCode.getJavaName(),
                                    lunchStatusRefCodes.get(0));
                            getBroker().saveBean(pgm, m_dictionaryLunch);
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("27", ruleWithFixes);
    }

    /**
     * Initialize rule 2B fix.
     */
    private void initializeRule2BFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Zero T Expulsions", "^(Y|N)$"))
                        .testThen(Restriction.pattern("Action Code", "^(E|F|U)$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        X2Criteria actionCriteria = new X2Criteria();
                        actionCriteria.addEqualTo(ConductAction.COL_STUDENT_OID, student.getOid());
                        QueryByCriteria actionQuery = new QueryByCriteria(ConductAction.class, actionCriteria, true);
                        QueryIterator iterator = getBroker().getIteratorByQuery(actionQuery);
                        while (iterator.hasNext()) {
                            ConductAction act = (ConductAction) iterator.next();
                            act.setActionCode("X Expulsion");
                            act.setActionPenaltyTime(new BigDecimal(1));
                            getBroker().saveBeanForced(act);
                        }
                    }
                }));
        addFixesByRuleNumber("2B", ruleWithFixes);
    }

    /**
     * Initialize rule 2K fix.
     */
    private void initializeRule2KFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Zero T Expulsions", "^(Y|N)$"))
                        .testThen(Restriction.pattern("Action Code", "^(E|F|U)$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        X2Criteria actionCriteria = new X2Criteria();
                        actionCriteria.addEqualTo(ConductAction.COL_STUDENT_OID, student.getOid());
                        QueryByCriteria actionQuery = new QueryByCriteria(ConductAction.class, actionCriteria, true);
                        QueryIterator iterator = getBroker().getIteratorByQuery(actionQuery);
                        while (iterator.hasNext()) {
                            ConductAction act = (ConductAction) iterator.next();
                            act.setActionCode("X Expulsion");
                            act.setActionPenaltyTime(new BigDecimal(1));
                            getBroker().saveBeanForced(act);
                        }
                    }
                }));
        addFixesByRuleNumber("2K", ruleWithFixes);
    }

    /**
     * Initialize rule 3 fix.
     */
    private void initializeRule3Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Student Number",
                        "^(?!(00|7[67]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w$",
                        "Student Number, must be numeric, tenth position must either be an X or numeric."
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-75 or 78-79. "
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
     * Initialize rule 35 fix.
     */
    private void initializeRule35Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Action Code", "E"))
                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-ENR",
                                new KeyValuePair("District Number", "District Number, E"),
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.equals("Withdrawal Code", "W21"), null))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.SDRA, "Student Number");
                    }
                }));
        addFixesByRuleNumber("35", ruleWithFixes);
    }

    /**
     * Initialize rule 41 fix.
     */
    private void initializeRule41Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Action Code", "E"))
                        .testThen(Restriction.and(
                                Restriction.greaterThan("Action Duration", Double.valueOf(0)),
                                Restriction.lessThanOrEquals("Action Duration", Double.valueOf(210)))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        X2Criteria actionCriteria = new X2Criteria();
                        actionCriteria.addEqualTo(ConductAction.COL_STUDENT_OID, student.getOid());
                        QueryByCriteria actionQuery = new QueryByCriteria(ConductAction.class, actionCriteria, true);
                        QueryIterator iterator = getBroker().getIteratorByQuery(actionQuery);
                        while (iterator.hasNext()) {
                            ConductAction act = (ConductAction) iterator.next();
                            act.setActionPenaltyTime(new BigDecimal(1));
                            getBroker().saveBeanForced(act);
                        }
                    }
                }));
        addFixesByRuleNumber("41", ruleWithFixes);
    }
}
