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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetProcedureExcept.
 */
public class SetProcedureExcept extends SetProcedure {

    private static final String ALIAS_DISMISSAL_DATE = "pgm-dismissal-date";
    private static final String ALIAS_ELIG_DETERM_DATE = "pgm-eligibility-determination";
    private static final String ALIAS_EVAL_COMPLETION_DATE = "pgm-eval-completion-date";
    private static final String ALIAS_EXCEPT_OTHER = "pgm-other";
    private static final String ALIAS_EXCEPT_PRIMARY = "pgm-primary";
    private static final String ALIAS_GIFTED_ELIGIB = "pgm-gifted-eligibility";
    private static final String ALIAS_IDEA_EDU_ENV = "pgm-idea-edu-environments";
    private static final String ALIAS_PLACEMENT_DATE = "pgm-placement-date";
    private static final String ALIAS_PLACEMENT_STATUS = "pgm-placement-status";
    private static final String ALIAS_PLAN_DATE = "pgm-plan-date";

    private static final int DAY_DISMISSAL_END = 16;
    private static final int DAY_DISMISSAL_START = 1;

    private static final String DDX_ID = "FL-PGM-EXCEPT";

    private static final String PROGRAM_CODE_EXCEPT = "EXCEPT";

    private ModelBroker m_broker = null;
    private SimpleDateFormat m_dateFormat = new SimpleDateFormat(DateAsStringConverter.STRING_DATE_FORMAT);
    private DataDictionary m_dictionary = null;
    private Calendar m_dismEndCal = null;
    private Calendar m_dismStartCal = null;
    private PlainDate m_endDate = null;
    private String[] m_exceptAliasesForRandomSet = new String[] {ALIAS_EXCEPT_OTHER, ALIAS_EXCEPT_PRIMARY};
    private List m_oftenValidPlacementCodes = Arrays.asList("P", "T");
    private PlainDate m_oneDayAfterStart = null;
    private QueryByCriteria m_pgmsQuery = null;
    private PlainDate m_startDate = null;
    private PlainDate m_tenDaysAfterStart = null;
    private PlainDate m_twoDaysAfterStart = null;

    private DataDictionaryField m_fieldIdeaEduEnv = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        addRecords();
        fixErrors();
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
        super.initialize(FL_EXPORT.EXCEPT, SisStudent.class);

        m_startDate = getCurrentContext().getStartDate();
        Calendar daysAfterCalendar = Calendar.getInstance();
        daysAfterCalendar.setTime(m_startDate);
        daysAfterCalendar.add(Calendar.DATE, 1);
        m_oneDayAfterStart = new PlainDate(daysAfterCalendar.getTime());
        daysAfterCalendar.add(Calendar.DATE, 1);
        m_twoDaysAfterStart = new PlainDate(daysAfterCalendar.getTime());
        daysAfterCalendar.add(Calendar.DATE, 8);
        m_tenDaysAfterStart = new PlainDate(daysAfterCalendar.getTime());
        m_endDate = getCurrentContext().getEndDate();

        m_broker = new ModelBroker(getPrivilegeSet());

        X2Criteria criteria = new X2Criteria();
        // Date range criteria
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, getCurrentContext().getEndDate());
        X2Criteria endDate1Criteria = new X2Criteria();
        endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE,
                getBroker().getPersistenceKey());
        X2Criteria endDate2Criteria = new X2Criteria();
        endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                getCurrentContext().getStartDate());
        endDate1Criteria.addOrCriteria(endDate2Criteria);
        criteria.addAndCriteria(endDate1Criteria);

        // Extended data dictionary
        criteria.addEqualTo(StudentProgramParticipation.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER
                + ExtendedDataDictionary.COL_ID, DDX_ID);

        m_pgmsQuery = new QueryByCriteria(StudentProgramParticipation.class, criteria);

        X2Criteria ddxCriteria = new X2Criteria();
        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_ID);
        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        m_dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());

        m_dismStartCal = Calendar.getInstance();
        m_dismStartCal.set(Calendar.MONTH, Calendar.JULY);
        m_dismStartCal.set(Calendar.DATE, DAY_DISMISSAL_START);

        m_dismEndCal = Calendar.getInstance();
        m_dismEndCal.set(Calendar.MONTH, Calendar.AUGUST);
        m_dismEndCal.set(Calendar.DATE, DAY_DISMISSAL_END);

        StudentProgramDataset pgmExceptionalStudentDataset = getStudentHelper().getStudentProgramDataset(DDX_ID,
                getFLReportData().getSurveyPeriod());
        DataDictionary pgmExceptionalStudentDataDictionary = pgmExceptionalStudentDataset.getDataDictionary();

        m_fieldIdeaEduEnv =
                getFLReportData().translateAliasToDictionaryField(pgmExceptionalStudentDataDictionary, ALIAS_IDEA_EDU_ENV, true);

        initializeRule31Fix();
        initializeRule38Fix();
        initializeRule39Fix();
    }

    /**
     * Initialize rule 31 fix.
     */
    private void initializeRule31Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(
                        Restriction.equals("Survey Period", "2"),
                        Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(3),
                                Integer.valueOf(5), true, true),
                        Restriction.or(
                                Restriction.pattern("Except Primary", "^((?!L|Z).)*$"),
                                Restriction.pattern("Exceptionality Other", "^((?!L|Z).)*$")))
                        .testThen(Restriction.pattern("IDEA Educational Env",
                                "^[A|B|J|K|L|M|S]$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            StudentProgramParticipation pgm =
                                    getFLReportData().getStudentHelper().getStudentProgram(bean.getOid(), DDX_ID,
                                            getFLReportData().getSurveyPeriod());
                            String randomCode = null;
                            while (randomCode == null || !randomCode.matches("^[A|B|J|K|L|M|S]$")) {
                                randomCode = getRandomCodeForField(m_fieldIdeaEduEnv);
                            }
                            pgm.setFieldValueByBeanPath(m_fieldIdeaEduEnv.getJavaName(), randomCode);
                            getModelBroker().saveBeanForced(pgm);
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));

        addFixesByRuleNumber("31", ruleWithFixes);
    }

    /**
     * Initialize rule 38 fix.
     */
    private void initializeRule38Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(
                        Restriction.pattern("Survey Period", "^[1-4]$"),
                        Restriction.or(
                                Restriction.equals("Except Primary", "U"),
                                Restriction.equals("Exceptionality Other", "U")),
                        Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair(Pattern.compile("^((?!A|B).)*$"),
                                        "Year-Round Indicator")))
                        .testThen(
                                Restriction.studentAgeInRange("Student ID Local", null, Integer.valueOf(3),
                                        false, false)),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        SisPerson person = student.getPerson();
                        person.setDob(SetProcedureStd.getDobOnDateForRange(new PlainDate(new Date()), false, false, Integer.valueOf(1),
                                Integer.valueOf(3)));
                        getModelBroker().saveBeanForced(person);
                    }
                }));

        addFixesByRuleNumber("38", ruleWithFixes);
    }

    /**
     * Initialize rule 39 fix.
     */
    private void initializeRule39Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(
                        Restriction.pattern("Survey Period", "^[1-4]$"),
                        Restriction.or(
                                Restriction.equals("Except Primary", "T"),
                                Restriction.equals("Exceptionality Other", "T")),
                        Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair(Pattern.compile("^((?!A|B).)*$"),
                                        "Year-Round Indicator")))
                        .testThen(
                                Restriction.studentAgeInRange("Student ID Local", null, Integer.valueOf(6),
                                        false, false)),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        SisPerson person = student.getPerson();
                        person.setDob(SetProcedureStd.getDobOnDateForRange(new PlainDate(new Date()), false, false, Integer.valueOf(1),
                                Integer.valueOf(6)));
                        getModelBroker().saveBeanForced(person);
                    }
                }));

        addFixesByRuleNumber("39", ruleWithFixes);
    }

    /**
     * Adds the records.
     *
     * @throws X2BaseException exception
     */
    private void addRecords() throws X2BaseException {
        Collection<SisStudent> existingStudentsWithPrograms = getBroker().getCollectionByQuery(getFLReportData().getQuery());
        if (existingStudentsWithPrograms.size() < 10) {
            FLStudentHelper helper = new FLStudentHelper(getFLReportData());
            Collection<SisStudent> students =
                    getBroker().getCollectionByQuery(helper.getStudentQuery(false));
            int couter = 0;
            for (SisStudent student : students) {
                if (!existingStudentsWithPrograms.contains(student)) {
                    StudentProgramParticipation newPgm =
                            new StudentProgramParticipation(getBroker().getPersistenceKey());
                    newPgm.setStudentOid(student.getOid());
                    newPgm.setExtendedDataDictionaryOid(m_dictionary.getExtendedDictionaryOid());

                    PlainDate programStartDate = null;
                    StudentEnrollment nearestEnrollment =
                            getFLReportData().getStudentHelper().getEnrollmentForDate(student.getOid(), m_startDate, "EW");
                    if (nearestEnrollment == null) {
                        programStartDate = m_startDate;
                    } else {
                        programStartDate = new PlainDate(new Date(
                                ThreadLocalRandom.current().nextLong(nearestEnrollment.getEnrollmentDate().getTime(),
                                        m_startDate.getTime())));
                    }

                    newPgm.setStartDate(programStartDate);
                    newPgm.setProgramCode(PROGRAM_CODE_EXCEPT);
                    getBroker().saveBean(newPgm, m_dictionary);
                    couter++;
                }
                if (couter == 10) {
                    break;
                }
            }
        }
    }

    /**
     * Fix errors.
     *
     * @throws ParseException exception
     */
    private void fixErrors() throws ParseException {
        QueryIterator pgmsIterator = getBroker().getIteratorByQuery(m_pgmsQuery);

        try {
            while (pgmsIterator.hasNext()) {
                StudentProgramParticipation pgm = (StudentProgramParticipation) pgmsIterator.next();

                // Fix errors for validation rule 9
                String planDateString = m_dateFormat.format(
                        new Date(ThreadLocalRandom.current().nextLong(m_startDate.getTime(), m_endDate.getTime())));
                pgm.setFieldValueByAlias(ALIAS_PLAN_DATE, planDateString, m_dictionary);

                // Fix errors validation rule 2L
                String primary = (String) pgm.getFieldValueByAlias(ALIAS_EXCEPT_PRIMARY, m_dictionary);
                if (StringUtils.isEmpty(primary) || "Z".equals(primary)) {
                    DataDictionaryField fieldPrimary =
                            m_dictionary.findDataDictionaryFieldByAlias(ALIAS_EXCEPT_PRIMARY);
                    pgm.setFieldValueByAlias(ALIAS_EXCEPT_PRIMARY, getRandomCodeForField(fieldPrimary), m_dictionary);
                }

                // Fix errors for validation rule 28
                String giftedEligibility = (String) pgm.getFieldValueByAlias(ALIAS_GIFTED_ELIGIB, m_dictionary);
                if (!StringUtils.isEmpty(giftedEligibility) && giftedEligibility.matches("^[A|B]$")) {
                    String other = (String) pgm.getFieldValueByAlias(ALIAS_EXCEPT_OTHER, m_dictionary);
                    String expectedOtherPrimaryCode = "L";
                    if (!(expectedOtherPrimaryCode.equals(other) || expectedOtherPrimaryCode.equals(primary))) {
                        int aliasToSetIndex = ThreadLocalRandom.current().nextInt(0, 2);
                        String aliasToSet = m_exceptAliasesForRandomSet[aliasToSetIndex];
                        pgm.setFieldValueByAlias(aliasToSet, expectedOtherPrimaryCode, m_dictionary);

                    }
                }

                // Fix errors for validation rule 44
                PlainDate dismissalDate = (PlainDate) pgm.getFieldValueByAlias(ALIAS_DISMISSAL_DATE, m_dictionary);
                if (dismissalDate != null) {
                    PlainDate disDateRangeStart = getDismissalDateStart(dismissalDate);
                    PlainDate disDateRangeEnd = getDismissalDateEnd(dismissalDate);

                    if (dismissalDate.before(disDateRangeStart) || dismissalDate.after(disDateRangeEnd)) {
                        String dismissalDateString = m_dateFormat.format(
                                new Date(ThreadLocalRandom.current().nextLong(disDateRangeStart.getTime(),
                                        disDateRangeEnd.getTime())));
                        pgm.setFieldValueByAlias(ALIAS_DISMISSAL_DATE, dismissalDateString, m_dictionary);
                    }
                }

                // Fix errors for validation rule 2D
                String placementStatus = (String) pgm.getFieldValueByAlias(ALIAS_PLACEMENT_STATUS, m_dictionary);
                if (StringUtils.isEmpty(placementStatus) || !m_oftenValidPlacementCodes.contains(placementStatus)) {
                    DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PLACEMENT_STATUS);
                    String codeToSet = null;
                    while (!m_oftenValidPlacementCodes.contains(codeToSet)) {
                        codeToSet = getRandomCodeForField(field);
                    }
                    pgm.setFieldValueByAlias(ALIAS_PLACEMENT_STATUS, codeToSet, m_dictionary);
                }

                // Fix errors for validation rule 2B
                String placementDate =
                        (String) pgm.getFieldValueByAlias(ALIAS_PLACEMENT_DATE, m_dictionary);
                if (placementDate == null) {
                    placementDate = m_dateFormat.format(
                            new Date(ThreadLocalRandom.current().nextLong(m_twoDaysAfterStart.getTime(),
                                    m_tenDaysAfterStart.getTime())));
                    pgm.setFieldValueByAlias(ALIAS_PLACEMENT_DATE, placementDate, m_dictionary);
                }

                // Fix initialization errors 41
                String determDate = (String) pgm.getFieldValueByAlias(ALIAS_ELIG_DETERM_DATE, m_dictionary);
                if (determDate == null && placementDate != null) {
                    determDate = m_dateFormat.format(
                            new Date(ThreadLocalRandom.current().nextLong(m_oneDayAfterStart.getTime(),
                                    m_dateFormat.parse(placementDate).getTime())));
                    pgm.setFieldValueByAlias(ALIAS_ELIG_DETERM_DATE, determDate, m_dictionary);
                    determDate = (String) pgm.getFieldValueByAlias(ALIAS_ELIG_DETERM_DATE, m_dictionary);
                }

                // Fix initialization errors 42
                String completionDate =
                        (String) pgm.getFieldValueByAlias(ALIAS_EVAL_COMPLETION_DATE, m_dictionary);
                if (completionDate == null && determDate != null) {
                    completionDate = m_dateFormat.format(
                            new Date(ThreadLocalRandom.current().nextLong(m_startDate.getTime(),
                                    m_dateFormat.parse(determDate).getTime())));
                    pgm.setFieldValueByAlias(ALIAS_EVAL_COMPLETION_DATE, completionDate, m_dictionary);
                }

                m_broker.saveBean(pgm, m_dictionary);
            }
        } finally {
            pgmsIterator.close();
        }
    }

    /**
     * Gets the dismissal date start.
     *
     * @param dismissalDate PlainDate
     * @return Plain date
     */
    private PlainDate getDismissalDateStart(PlainDate dismissalDate) {
        m_dismStartCal.set(Calendar.YEAR, getYear(dismissalDate));
        return new PlainDate(m_dismStartCal.getTime());
    }

    /**
     * Gets the dismissal date end.
     *
     * @param dismissalDate PlainDate
     * @return Plain date
     */
    private PlainDate getDismissalDateEnd(PlainDate dismissalDate) {
        m_dismEndCal.set(Calendar.YEAR, getYear(dismissalDate));
        return new PlainDate(m_dismEndCal.getTime());
    }

    /**
     * Gets the year.
     *
     * @param date PlainDate
     * @return int
     */
    private int getYear(PlainDate date) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        return calendar.get(Calendar.YEAR);
    }
}
