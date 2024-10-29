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
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.ibm.icu.text.SimpleDateFormat;
import com.x2dev.procedures.statereporting.fl.FLCompensatoryProjectEvaluationData.RetrieveMigrantFields;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetProcedureCPE.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class SetProcedureCPE extends SetProcedure {

    private static final String ALIAS_ARRIVAL_DATE = "pgm-qualarrival-date";
    private static final String ALIAS_CONTINUATION = "pgm-continuation";
    private static final String ALIAS_MIGRANT_STATUS = "pgm-migrant-status";
    private static final String ALIAS_MODEL = "pgm-model";
    private static final String ALIAS_PRIORITY = "pgm-priority";
    private static final String ALIAS_PROJECT_TYPE = "pgm-project-type";
    private static final String ALIAS_REFERRED_SERVICES = "pgm-referred-services";
    private static final String ALIAS_SUBJECT_AREA = "pgm-subject-area";
    private static final String ALIAS_TERM_CODE = "pgm-term-code";

    private static final String PROGRAM_CODE_MIGRANT = "MIGRANT";

    private DataDictionary m_dictionary = null;

    private DataDictionaryField m_fieldArrivalDate = null;
    private DataDictionaryField m_fieldContinuation = null;
    private DataDictionaryField m_fieldMigrantStatus = null;
    private DataDictionaryField m_fieldModel = null;
    private DataDictionaryField m_fieldPriority = null;
    private DataDictionaryField m_fieldProjectType = null;
    private DataDictionaryField m_fieldReferredServices = null;
    private DataDictionaryField m_fieldSubjectArea = null;
    private DataDictionaryField m_fieldTermCode = null;

    private Random m_random = null;
    private StudentProgramDataset m_pgmDataset;
    private SimpleDateFormat m_dateFormat = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.procedures.statereporting.fl.SetProcedure#execute()
     */
    @Override
    protected void execute() throws Exception {
        addPrograms();

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
        super.initialize(FL_EXPORT.CPE, SisStudent.class);

        m_dateFormat = new SimpleDateFormat("MMddyyyy");

        X2Criteria ddxCriteria = new X2Criteria();
        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, RetrieveMigrantFields.DDX_ID);
        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        m_dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());

        m_fieldArrivalDate = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_ARRIVAL_DATE);
        m_fieldContinuation = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_CONTINUATION);
        m_fieldMigrantStatus = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_MIGRANT_STATUS);
        m_fieldModel = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_MODEL);
        m_fieldPriority = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PRIORITY);
        m_fieldProjectType = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PROJECT_TYPE);
        m_fieldReferredServices = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_REFERRED_SERVICES);
        m_fieldSubjectArea = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_SUBJECT_AREA);
        m_fieldTermCode = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TERM_CODE);

        m_pgmDataset = getFLReportData().getStudentHelper().getStudentProgramDataset(
                RetrieveMigrantFields.DDX_ID,
                getFLReportData().getSurveyPeriod());

        m_random = new Random(System.currentTimeMillis());

        initializeRule20Fix();
        initializeRule24Fix();
        initializeRule29Fix();
        initializeRule2CFix();
        initializeRule2EFix();
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
            ArrayList<SisStudent> studentsWithoutMigrant =
                    new ArrayList<SisStudent>(getModelBroker().getCollectionByQuery(helper.getStudentQuery(true)));
            for (int i = 0; i <= numOfNeededStudents; i++) {
                SisStudent randomStudent = studentsWithoutMigrant.get(m_random.nextInt(studentsWithoutMigrant.size()));
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
        newPgm.setProgramCode(PROGRAM_CODE_MIGRANT);

        newPgm.setFieldValueByBeanPath(m_fieldMigrantStatus.getJavaName(), getRandomCodeForField(m_fieldMigrantStatus));
        newPgm.setFieldValueByBeanPath(m_fieldContinuation.getJavaName(), getRandomCodeForField(m_fieldContinuation));
        newPgm.setFieldValueByBeanPath(m_fieldTermCode.getJavaName(), getRandomCodeForField(m_fieldTermCode));
        newPgm.setFieldValueByBeanPath(m_fieldProjectType.getJavaName(), getRandomCodeForField(m_fieldProjectType));
        newPgm.setFieldValueByBeanPath(m_fieldSubjectArea.getJavaName(), getRandomCodeForField(m_fieldSubjectArea));
        newPgm.setFieldValueByBeanPath(m_fieldModel.getJavaName(), getRandomCodeForField(m_fieldModel));

        newPgm.setFieldValueByBeanPath(m_fieldPriority.getJavaName(),
                m_random.nextBoolean() ? BooleanAsStringConverter.TRUE : BooleanAsStringConverter.FALSE);
        newPgm.setFieldValueByBeanPath(m_fieldReferredServices.getJavaName(),
                m_random.nextBoolean() ? BooleanAsStringConverter.TRUE : BooleanAsStringConverter.FALSE);

        newPgm.setFieldValueByBeanPath(m_fieldArrivalDate.getJavaName(),
                m_dateFormat
                        .format(new Date(ThreadLocalRandom.current().nextLong(getFLReportData().getSurveyPeriod().getStartDate().getTime(),
                                getFLReportData().getSurveyPeriod().getEndDate().getTime()))));

        return newPgm;
    }

    /**
     * Initialize rule 20 fix.
     */
    private void initializeRule20Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Term", "^(3|S)$", "Term must be 3 or S."),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentProgramParticipation> programs = m_pgmDataset.getPrograms(bean.getOid());
                        for (StudentProgramParticipation program : programs) {
                            program.setFieldValueByBeanPath(m_fieldTermCode.getJavaName(), m_random.nextBoolean() ? "3" : "S");
                            getModelBroker().saveBean(program);
                        }
                    }

                }));

        addFixesByRuleNumber("20", ruleWithFixes);
    }

    /**
     * Initialize rule 24 fix.
     */
    private void initializeRule24Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Support Services", "^(A|D|H|N|O|R|S|T|X|Z)$",
                        "Federal/State Project - Support Service must be A, D, H, N, O, R, S, T, X, or Z."),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentProgramParticipation> programs = m_pgmDataset.getPrograms(bean.getOid());
                        for (StudentProgramParticipation program : programs) {
                            program.setFieldValueByBeanPath(m_fieldTermCode.getJavaName(), m_random.nextBoolean() ? "3" : "S");
                            getModelBroker().saveBean(program);
                        }
                    }

                }));

        addFixesByRuleNumber("24", ruleWithFixes);
    }

    /**
     * Initialize rule 29 fix.
     */
    private void initializeRule29Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Fed/St Project Type", "1"))
                        .testThen(Restriction.notEquals("Fed/St Model", "00")),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentProgramParticipation> programs = m_pgmDataset.getPrograms(bean.getOid());
                        for (StudentProgramParticipation program : programs) {
                            program.setFieldValueByBeanPath(m_fieldModel.getJavaName(), getRandomCodeForField(m_fieldModel));
                            getModelBroker().saveBean(program);
                        }
                    }

                }));

        addFixesByRuleNumber("29", ruleWithFixes);
    }

    /**
     * Initialize rule 2C fix.
     */
    private void initializeRule2CFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Fed/St Model", "^(0[019]|1[0-4])$",
                        "Federal/State Model must equal 00, 01, 09, 10, 11, 12, 13, 14."),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentProgramParticipation> programs = m_pgmDataset.getPrograms(bean.getOid());
                        for (StudentProgramParticipation program : programs) {
                            program.setFieldValueByBeanPath(m_fieldModel.getJavaName(),
                                    String.valueOf(ThreadLocalRandom.current().nextInt(10, 15)));
                            getModelBroker().saveBean(program);
                        }
                    }

                }));

        addFixesByRuleNumber("2C", ruleWithFixes);
    }

    /**
     * Initialize rule 2E fix.
     */
    private void initializeRule2EFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Fed/St Project Type", "8"))
                        .testThen(Restriction.equals("Fed/St Model", "14")),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentProgramParticipation> programs = m_pgmDataset.getPrograms(bean.getOid());
                        for (StudentProgramParticipation program : programs) {
                            program.setFieldValueByBeanPath(m_fieldModel.getJavaName(), "14");
                            getModelBroker().saveBean(program);
                        }
                    }

                }));

        addFixesByRuleNumber("2E", ruleWithFixes);
    }
}
