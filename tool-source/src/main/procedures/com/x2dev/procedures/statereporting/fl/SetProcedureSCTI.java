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
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

/**
 * @author Follett Software Company
 * @copyright 2018
 */
public class SetProcedureSCTI extends SetProcedure {
    private static final String ALIAS_COURSE_NUMBER = "all-crs-StateId";
    private static final String ALIAS_COURSE_SAR = "all-crs-StateSAR";
    private static final String ALIAS_COURSE_SSAR = "all-crs-SubstitSSAR";
    private static final String ALIAS_COURSE_FLAG = "all-crs-Flag";
    private static final String ALIAS_COURSE_NUMBER_SUBS = "all-crs-NumbSubstit";
    private static final String ALIAS_COURSE_ONLINE_CRS = "all-crs-OnlineCourse";

    protected DataDictionaryField m_fieldCrsFlag;
    protected DataDictionaryField m_fieldCrsNo;
    protected DataDictionaryField m_fieldCrsNoSubst;
    protected DataDictionaryField m_fieldCrsOnline;
    protected DataDictionaryField m_fieldCrsSAR;
    protected DataDictionaryField m_fieldCrsSSAR;

    /**
     * @see com.x2dev.procedures.statereporting.fl.SetProcedure#execute()
     */
    @Override
    protected void execute() throws Exception {
        // for (int i = 0; i < 2; i++) {
        runAndValidateExports();
        // super.execute();
        // }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FL_EXPORT.SCTI, SisStudent.class);

        m_fieldCrsNo = getFLReportData().translateAliasToDictionaryField(ALIAS_COURSE_NUMBER, true);
        m_fieldCrsNoSubst = getFLReportData().translateAliasToDictionaryField(ALIAS_COURSE_NUMBER_SUBS, true);
        m_fieldCrsSAR = getFLReportData().translateAliasToDictionaryField(ALIAS_COURSE_SAR, true);
        m_fieldCrsSSAR = getFLReportData().translateAliasToDictionaryField(ALIAS_COURSE_SSAR, true);
        m_fieldCrsFlag = getFLReportData().translateAliasToDictionaryField(ALIAS_COURSE_FLAG, true);
        m_fieldCrsOnline = getFLReportData().translateAliasToDictionaryField(ALIAS_COURSE_ONLINE_CRS, true);

        // updateCourseIds();

        initializeRule7Fix();
        initializeRule10Fix();
        initializeRule12Fix();
        initializeRule13Fix();
        initializeRule14Fix();
        initializeRule15Fix();
        initializeRule18Fix();
        initializeRule23Fix();
        initializeRule26Fix();
        initializeRule27Fix();
        initializeRule28Fix();
        initializeRule29Fix();
        initializeRule35Fix();
        initializeRule36Fix();
        initializeRule38Fix();
        initializeRule81Fix();
    }

    // private void updateCourseIds() {
    // X2Criteria criteria = new X2Criteria();
    // QueryByCriteria query = new QueryByCriteria(Course.class, criteria);
    // Collection<Course> courses = getBroker().getCollectionByQuery(query);
    // Set<String> setCourseIds = new HashSet<>();
    // int counter = 0;
    // for (Course course : courses) {
    // counter++;
    // System.out.println(counter);
    // String courseId = (String) course.getFieldValueByBeanPath(m_fieldCrsNo.getJavaName());
    // if (courseId == null || !courseId.matches("^\\S{7}$")) {
    // course.setFieldValueByBeanPath(m_fieldCrsNo.getJavaName(),
    // getRandomCodeForField(m_fieldCrsNo));
    // getModelBroker().saveBeanForced(course);
    // } else {
    // setCourseIds.add(courseId);
    // }
    // }
    // }

    /**
     * Initialize rule 7 fix.
     */
    private void initializeRule7Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.lessThanOrEquals("School Year CT", Double.valueOf(0))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {/* No Fix needed */}
                }));
        addFixesByRuleNumber("7", ruleWithFixes);
    }

    /**
     * Initialize rule 10 fix.
     */
    private void initializeRule10Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Course Number", "^\\S{1,7}$",
                        "Course Number must be alphanumeric and contain no blanks."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // try {
                        // List<Transcript> transcripts =
                        // getFLReportData().getStudentHelper().getStudentTranscripts((SisStudent)
                        // bean);
                        // for (Transcript trn : transcripts) {
                        // SchoolCourse schoolCourse = trn.getSchoolCourse();
                        // if (schoolCourse == null) {
                        // continue;
                        // }
                        // course.setFieldValueByBeanPath(beanPath, fieldValue);
                        // }
                        // } catch (X2BaseException e) {
                        // e.printStackTrace();
                        // }
                    }
                }));
        addFixesByRuleNumber("10", ruleWithFixes);
    }

    /**
     * Initialize rule 12 fix.
     */
    private void initializeRule12Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("School Number WCrE", "^(?!P000|P\\d{3})\\S{1,4}$"))
                        .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {/* No Fix needed */}
                }));
        addFixesByRuleNumber("12", ruleWithFixes);
    }

    /**
     * Initialize rule 13 fix.
     */
    private void initializeRule13Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equalsFieldValue("School Year CT",
                        "School Year RS", String.class))
                        .testThen(Restriction.pattern("Course State SAR",
                                "^(EN|MA|AH|WH|EC|AG|VO|PF|PE|EL|FL|PA|A1|BI|GE|EQ|SS|CE|EX|LA|LM|NC|SV)$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<Transcript> transcripts =
                                    getFLReportData().getStudentHelper().getStudentTranscripts((SisStudent) bean);
                            for (Transcript trn : transcripts) {
                                SchoolCourse schoolCourse = trn.getSchoolCourse();
                                if (schoolCourse == null) {
                                    continue;
                                }
                                Course course = schoolCourse.getCourse();
                                String currentSAR =
                                        (String) course.getFieldValueByBeanPath(m_fieldCrsSAR.getJavaName());
                                if (StringUtils.isEmpty(currentSAR)
                                        || !currentSAR
                                                .matches(
                                                        "^(EN|MA|AH|WH|EC|AG|VO|PF|PE|EL|FL|PA|A1|BI|GE|EQ|SS|CE|EX|LA|LM|NC|SV)$")) {
                                    String randomSAR = null;
                                    while (randomSAR == null
                                            || !randomSAR
                                                    .matches(
                                                            "^(EN|MA|AH|WH|EC|AG|VO|PF|PE|EL|FL|PA|A1|BI|GE|EQ|SS|CE|EX|LA|LM|NC|SV)$")) {
                                        randomSAR = getRandomCodeForField(m_fieldCrsSAR);
                                    }
                                    course.setFieldValueByBeanPath(m_fieldCrsSAR.getJavaName(), randomSAR);
                                    getModelBroker().saveBeanForced(course);
                                    System.out.println(course.getOid());
                                    System.out.println("fix 13");
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
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
                ValidationRule.testIf(Restriction.equalsFieldValue("School Year CT",
                        "School Year RS", String.class))
                        .testThen(Restriction.pattern("Course, Flag",
                                "^([G-I]|[79NPTWX\\*\\s]){1,4}$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<Transcript> transcripts =
                                    getFLReportData().getStudentHelper().getStudentTranscripts((SisStudent) bean);
                            for (Transcript trn : transcripts) {
                                SchoolCourse schoolCourse = trn.getSchoolCourse();
                                if (schoolCourse == null) {
                                    continue;
                                }
                                Course course = schoolCourse.getCourse();
                                String currentFlag =
                                        (String) course.getFieldValueByBeanPath(m_fieldCrsFlag.getJavaName());
                                if (currentFlag == null || !currentFlag.matches("^([G-I]|[79NPTWX\\*\\s]){1,4}$")) {
                                    String randomCode = null;
                                    while (randomCode == null
                                            || !randomCode.matches("^([G-I]|[79NPTWX\\*\\s]){1,4}$")) {
                                        randomCode = getRandomCodeForField(m_fieldCrsFlag);
                                    }
                                    course.setFieldValueByBeanPath(m_fieldCrsFlag.getJavaName(), randomCode);
                                    getModelBroker().saveBeanForced(course);
                                    System.out.println(course.getOid());
                                    System.out.println("fix 14");
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
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
                new ValidateRegularExpression("Course, Flag",
                        "^(?!.*(7.*7|9.*9|G.*G|H.*H|I.*I|N.*N|P.*P|T.*T|W.*W|X.*X|\\*.*\\*).*).+$",
                        "For each Student Course Transcript Information record, no Course Flag, except blanks, may be repeated."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<Transcript> transcripts =
                                    getFLReportData().getStudentHelper().getStudentTranscripts((SisStudent) bean);
                            for (Transcript trn : transcripts) {
                                SchoolCourse schoolCourse = trn.getSchoolCourse();
                                if (schoolCourse == null) {
                                    continue;
                                }
                                Course course = schoolCourse.getCourse();
                                String currentFlag =
                                        (String) course.getFieldValueByBeanPath(m_fieldCrsFlag.getJavaName());
                                if (currentFlag == null || !currentFlag.matches("^([G-I]|[79NPTWX\\*\\s]){1,4}$")) {
                                    String randomCode = null;
                                    while (randomCode == null
                                            || !randomCode.matches("^([G-I]|[79NPTWX\\*\\s]){1,4}$")) {
                                        randomCode = getRandomCodeForField(m_fieldCrsFlag);
                                    }
                                    course.setFieldValueByBeanPath(m_fieldCrsFlag.getJavaName(), randomCode);
                                    getModelBroker().saveBeanForced(course);
                                    System.out.println(course.getOid());
                                    System.out.println("fix 14");
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                        System.out.println("fix 15");
                    }
                }));
        addFixesByRuleNumber("15", ruleWithFixes);
    }

    /**
     * Initialize rule 18 fix.
     */
    private void initializeRule18Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Course Grade",
                        "^(\\s{2}([A-D]|F|I|N|U|P|S|E|W|Z)|\\s(A[+-]?|B[+-]?|C[+-]?|D[+-]?|IP|WP|FL|NG|WF))$",
                        "The Course Grade code must be A+, A, A-, B+, B, B-, C+, C, C-, D+, D, D-, F, I, N, U, P, S, SB, T, E, WP, FL, NG, W or WF "
                                + "and must be right justified with leading blanks."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<Transcript> transcripts =
                                    getFLReportData().getStudentHelper().getStudentTranscripts((SisStudent) bean);
                            for (Transcript trn : transcripts) {
                                String finalGrade = trn.getFinalGrade();
                                if (StringUtils.isEmpty(finalGrade)) {
                                    trn.setFinalGrade(String.valueOf(ThreadLocalRandom.current().nextInt(60, 100)));
                                    getModelBroker().saveBeanForced(trn);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("18", ruleWithFixes);
    }

    /**
     * Initialize rule 23 fix.
     */
    private void initializeRule23Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Student ID Local",
                        "^[A-Za-z0-9\\s]{10}$",
                        "Student Number Identifier, Local may be any combination of letters, numbers and blanks. "
                                + "(All blanks are allowable.) It must be left-justified with trailing blanks."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        /* No Fix needed */
                    }
                }));
        addFixesByRuleNumber("23", ruleWithFixes);
    }

    /**
     * Initialize rule 26 fix.
     */
    private void initializeRule26Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Course Numb Substit", "^\\s*\\S+$"))
                        .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Numb Substit")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<Transcript> transcripts =
                                    getFLReportData().getStudentHelper().getStudentTranscripts((SisStudent) bean);
                            for (Transcript trn : transcripts) {
                                SchoolCourse schoolCourse = trn.getSchoolCourse();
                                if (schoolCourse == null) {
                                    continue;
                                }
                                Course course = schoolCourse.getCourse();
                                String currentNoSubst =
                                        (String) course.getFieldValueByBeanPath(m_fieldCrsNoSubst.getJavaName());
                                if (StringUtils.isEmpty(currentNoSubst)) {
                                    course.setFieldValueByBeanPath(m_fieldCrsNoSubst.getJavaName(),
                                            course.getFieldValueByBeanPath(m_fieldCrsNo.getJavaName()));
                                    getModelBroker().saveBeanForced(course);
                                    System.out.println("fix 26");
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
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
                ValidationRule.testIf(Restriction.pattern("Course Numb Substit", "^\\s*\\S+$"))
                        .testThen(Restriction.equals("Course, Flag", "*")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<Transcript> transcripts =
                                    getFLReportData().getStudentHelper().getStudentTranscripts((SisStudent) bean);
                            for (Transcript trn : transcripts) {
                                SchoolCourse schoolCourse = trn.getSchoolCourse();
                                if (schoolCourse == null) {
                                    continue;
                                }
                                Course course = schoolCourse.getCourse();
                                String courseNumSub =
                                        (String) course.getFieldValueByBeanPath(m_fieldCrsNoSubst.getJavaName());
                                if (courseNumSub.matches("^\\s*\\S+$")) {
                                    String courseFlag =
                                            (String) course.getFieldValueByBeanPath(m_fieldCrsFlag.getJavaName());
                                    if (courseFlag == null) {
                                        while (courseFlag == null || courseFlag.equals("*")) {
                                            courseFlag = "";
                                        }
                                    }
                                    if (!courseFlag.contains("*")) {
                                        courseFlag = courseFlag + "*";
                                        course.setFieldValueByBeanPath(m_fieldCrsFlag.getJavaName(), courseFlag);
                                        getModelBroker().saveBeanForced(course);
                                        System.out.println("fix 27");
                                    }
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("27", ruleWithFixes);
    }

    /**
     * Initialize Course Numb Substit fix.
     */
    private void initializeRule28Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Course, Flag", "*"))
                        .testThen(Restriction.pattern("Course Numb Substit", "^\\s*\\S+$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<Transcript> transcripts =
                                    getFLReportData().getStudentHelper().getStudentTranscripts((SisStudent) bean);
                            for (Transcript trn : transcripts) {
                                SchoolCourse schoolCourse = trn.getSchoolCourse();
                                if (schoolCourse == null) {
                                    continue;
                                }
                                Course course = schoolCourse.getCourse();
                                String currentNoSubst =
                                        (String) course.getFieldValueByBeanPath(m_fieldCrsNoSubst.getJavaName());
                                String currentFlag =
                                        (String) course.getFieldValueByBeanPath(m_fieldCrsFlag.getJavaName());
                                if (StringUtils.isEmpty(currentNoSubst) && "*".equals(currentFlag)) {
                                    course.setFieldValueByBeanPath(m_fieldCrsNoSubst.getJavaName(),
                                            getRandomCodeForField(m_fieldCrsNo));
                                    getModelBroker().saveBeanForced(course);
                                    System.out.println("fix 28");
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
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
                ValidationRule.testIf(Restriction.equalsFieldValue("School Year CT", "School Year RS",
                        String.class))
                        .testThen(Restriction.pattern("Course Substit SSAR",
                                "^(EN|MA|AH|WH|EC|AG|VO|PF|PE|EL|FL|PA|A1|BI|GE|EQ|SS|CE|EX|LA|LM|NC|SV)$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<Transcript> transcripts =
                                    getFLReportData().getStudentHelper().getStudentTranscripts((SisStudent) bean);
                            for (Transcript trn : transcripts) {
                                SchoolCourse schoolCourse = trn.getSchoolCourse();
                                if (schoolCourse == null) {
                                    continue;
                                }
                                Course course = schoolCourse.getCourse();
                                String currentSSAR =
                                        (String) course.getFieldValueByBeanPath(m_fieldCrsSSAR.getJavaName());
                                if (StringUtils.isEmpty(currentSSAR) || !currentSSAR
                                        .matches(
                                                "^(EN|MA|AH|WH|EC|AG|VO|PF|PE|EL|FL|PA|A1|BI|GE|EQ|SS|CE|EX|LA|LM|NC|SV)$")) {
                                    String randomSSAR = null;
                                    while (randomSSAR == null
                                            || !randomSSAR
                                                    .matches(
                                                            "^(EN|MA|AH|WH|EC|AG|VO|PF|PE|EL|FL|PA|A1|BI|GE|EQ|SS|CE|EX|LA|LM|NC|SV)$")) {
                                        randomSSAR = getRandomCodeForField(m_fieldCrsSSAR);
                                    }
                                    course.setFieldValueByBeanPath(m_fieldCrsSSAR.getJavaName(), randomSSAR);
                                    getModelBroker().saveBeanForced(course);
                                    System.out.println(course.getOid());
                                    System.out.println("fix 29");
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("29", ruleWithFixes);
    }

    /**
     * Initialize rule 35 fix.
     */
    private void initializeRule35Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Online Course", "^(Y|N|I|J|O)$",
                        "Online Course must be Y, N, I, J or O."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<Transcript> transcripts =
                                    getFLReportData().getStudentHelper().getStudentTranscripts((SisStudent) bean);
                            for (Transcript trn : transcripts) {
                                SchoolCourse schoolCourse = trn.getSchoolCourse();
                                if (schoolCourse == null) {
                                    continue;
                                }
                                Course course = schoolCourse.getCourse();
                                String currentOnline =
                                        (String) course.getFieldValueByBeanPath(m_fieldCrsOnline.getJavaName());
                                if (StringUtils.isEmpty(currentOnline)) {
                                    String randomCode = null;
                                    while (randomCode == null || !randomCode.matches("^(Y|N|I|J|O)$")) {
                                        randomCode = getRandomCodeForField(m_fieldCrsOnline);
                                    }
                                    course.setFieldValueByBeanPath(m_fieldCrsOnline.getJavaName(), randomCode);
                                    getModelBroker().saveBeanForced(course);
                                    System.out.println("fix 35");
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
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
                ValidationRule.testIf(Restriction.pattern("School Number WCrE", "^(?!P\\S{1,3})\\S{1,4}$"))
                        .testThen(Restriction.pattern("Course Number",
                                "^(?!5022000|22000([0-4]\\d|50)|22003([0-6]\\d|70))\\S{1,7}$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {/* No Fix needed */}
                }));
        addFixesByRuleNumber("36", ruleWithFixes);
    }

    /**
     * Initialize rule 38 fix.
     */
    private void initializeRule38Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Online Course", "^(J|O)$"))
                        .testThen(Restriction.equals("Course Number", "0200985")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<Transcript> transcripts =
                                    getFLReportData().getStudentHelper().getStudentTranscripts((SisStudent) bean);
                            for (Transcript trn : transcripts) {
                                SchoolCourse schoolCourse = trn.getSchoolCourse();
                                if (schoolCourse == null) {
                                    continue;
                                }
                                Course course = schoolCourse.getCourse();
                                String onlineCourse =
                                        (String) course.getFieldValueByBeanPath(m_fieldCrsOnline.getJavaName());
                                if (onlineCourse.matches("^(J|O)$")) {
                                    course.setFieldValueByBeanPath(m_fieldCrsNo.getJavaName(), "0200985");
                                    getModelBroker().saveBeanForced(course);
                                    System.out.println("fix 38");
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("38", ruleWithFixes);
    }

    /**
     * Initialize rule 81 fix.
     */
    private void initializeRule81Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Grade Level", "^(0[1-9])$"),
                        Restriction.greaterThan("Credit Earned Course", Double.valueOf(0)))
                        .testThen(Restriction.equals("Course, Flag", "9")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<Transcript> transcripts =
                                    getFLReportData().getStudentHelper().getStudentTranscripts((SisStudent) bean);
                            for (Transcript trn : transcripts) {
                                trn.setTotalCredit(new BigDecimal(0));
                                getModelBroker().saveBeanForced(trn);
                                System.out.println("fix 81");
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("81", ruleWithFixes);
    }
}
