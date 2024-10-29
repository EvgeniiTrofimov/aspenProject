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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLCTEStudentCourseScheduleData.RetrieveCteFields;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Random;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetProcedureCTESSC.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class SetProcedureCTESSC extends SetProcedure {

    private static final String ALIAS_CTE_COMPLETION_POINT = "pgm-completion-point";
    private static final String ALIAS_CTE_INTERNSHIP_PARTICIPANT = "pgm-internship-participant";
    private static final String ALIAS_CTE_MOD_OCCUP_COMPL_POINT = "pgm-mocp";

    private static final String ALIAS_STD_SPED_FUNDING_TYPE = "all-std-SpecialEdFundingType";

    private DataDictionaryField m_fieldCompletionPoint = null;
    private DataDictionaryField m_fieldInternshipPart = null;
    private DataDictionaryField m_fieldModOccupComplPoint = null;

    private StudentProgramDataset m_pgmCteDataset = null;
    private Random m_random = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.procedures.statereporting.fl.SetProcedure#execute()
     */
    @Override
    protected void execute() throws Exception {
        super.execute();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.fl.SetProcedure#initialize(com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
     *      java.lang.Class)
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FL_EXPORT.CTESSC, SisStudent.class);

        m_random = new Random(System.currentTimeMillis());

        m_pgmCteDataset = getFLReportData().getStudentHelper().getStudentProgramDataset(RetrieveCteFields.DDX_ID,
                getFLReportData().getSurveyPeriod());

        m_fieldInternshipPart = m_pgmCteDataset.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CTE_INTERNSHIP_PARTICIPANT);
        m_fieldCompletionPoint = m_pgmCteDataset.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CTE_COMPLETION_POINT);
        m_fieldModOccupComplPoint = m_pgmCteDataset.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CTE_MOD_OCCUP_COMPL_POINT);

        initializeRule16Fix();
        initializeRule31Fix();
        initializeRule36Fix();
        initializeRule63Fix();
    }

    /**
     * Initialize rule 16 fix.
     */
    private void initializeRule16Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Internship",
                        "^[Y|N]$",
                        "The code for Internship Participant must be Y or N."),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            StudentProgramParticipation pgm =
                                    getFLReportData().getStudentHelper().getStudentProgram(bean.getOid(), RetrieveCteFields.DDX_ID,
                                            getFLReportData().getSurveyPeriod());
                            pgm.setFieldValueByBeanPath(m_fieldInternshipPart.getJavaName(),
                                    m_random.nextBoolean() ? BooleanAsStringConverter.TRUE : BooleanAsStringConverter.FALSE);
                            getModelBroker().saveBean(pgm);
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }

                }));

        addFixesByRuleNumber("16", ruleWithFixes);
    }

    /**
     * Initialize rule 31 fix.
     */
    private void initializeRule31Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.notEquals("Mod Occup Comp Point", "Z"))
                        .testThen(Restriction.equalsFieldValue("Mod Occup Comp Point",
                                "CTE Completion Point", String.class)),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            StudentProgramParticipation pgm =
                                    getFLReportData().getStudentHelper().getStudentProgram(bean.getOid(), RetrieveCteFields.DDX_ID,
                                            getFLReportData().getSurveyPeriod());
                            pgm.setFieldValueByBeanPath(m_fieldCompletionPoint.getJavaName(),
                                    pgm.getFieldValueByBeanPath(m_fieldModOccupComplPoint.getJavaName()));
                            getModelBroker().saveBean(pgm);
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }

                }));

        addFixesByRuleNumber("31", ruleWithFixes);
    }

    /**
     * Initialize rule 36 fix.
     */
    private void initializeRule36Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.notEquals("Mod Occup Comp Point", "Z"))
                        .testThen(Restriction.equals("Except CTE Course", "E")),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentScheduleInfo> infos = ((FLCTEStudentCourseScheduleData) getFLReportData()).m_studentScheduleHelper
                                .getStudentScheduleInfo((SisStudent) bean);
                        Collection<String> sectionOids = new ArrayList<String>();
                        for (StudentScheduleInfo info : infos) {
                            sectionOids.add(info.getSection().getOid());
                        }
                        Collection<StudentSchedule> studentSchedules = getStudentSchedules(bean.getOid(), sectionOids);
                        for (StudentSchedule stdSchedule : studentSchedules) {
                            stdSchedule.setFieldValueByAlias(FLStudentHelper.ALIAS_CTE_EXCEPTIONAL_SETTING_SSC, "E");
                            getModelBroker().saveBean(stdSchedule);
                        }
                        Collection<StudentScheduleChange> studentSchedulesChanges = getStudentScheduleChanges(bean.getOid(), sectionOids);
                        for (StudentScheduleChange stdScheduleChange : studentSchedulesChanges) {
                            stdScheduleChange.setFieldValueByAlias(FLStudentHelper.ALIAS_CTE_EXCEPTIONAL_SETTING_SCC, "E");
                            getModelBroker().saveBean(stdScheduleChange);
                        }
                    }

                }));

        addFixesByRuleNumber("36", ruleWithFixes);
    }

    /**
     * Initialize rule 63 fix.
     */
    private void initializeRule63Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Course Number", "^(?!8502000|00000000)\\d+$"))
                        .testThen(
                                Restriction.pattern("FEFP Program Number", "^(300|102|103|112|113|254|255)$")),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        DataDictionaryField field =
                                getFLReportData().translateAliasToDictionaryField(ALIAS_STD_SPED_FUNDING_TYPE, false);
                        bean.setFieldValueByBeanPath(field.getJavaName(), m_random.nextBoolean() ? "Support Level 4" : "Support Level 5");
                        getModelBroker().saveBean(bean);
                    }

                }));

        addFixesByRuleNumber("63", ruleWithFixes);
    }

    /**
     * Gets the student schedules.
     *
     * @param studentOid String
     * @param sectionOids Collection<String>
     * @return Collection
     */
    private Collection<StudentSchedule> getStudentSchedules(String studentOid, Collection<String> sectionOids) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, studentOid);
        criteria.addIn(StudentSchedule.COL_SECTION_OID, sectionOids);
        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, criteria, true);
        return getBroker().getCollectionByQuery(query);
    }

    /**
     * Gets the student schedule changes.
     *
     * @param studentOid String
     * @param sectionOids Collection<String>
     * @return Collection
     */
    private Collection<StudentScheduleChange> getStudentScheduleChanges(String studentOid, Collection<String> sectionOids) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentScheduleChange.COL_STUDENT_OID, studentOid);
        criteria.addIn(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, sectionOids);
        QueryByCriteria query = new QueryByCriteria(StudentScheduleChange.class, criteria, true);
        return getBroker().getCollectionByQuery(query);
    }
}
