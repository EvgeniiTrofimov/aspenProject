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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetProcedureSSC.
 */
public class SetProcedureSSC extends SetProcedure {

    protected static final String ALIAS_MST_STATE_ID = "all-mst-StateId";
    protected static final String ALIAS_STD_STATE_ID = "all-std-StateId";
    protected static final String ALIAS_COURSE_NUMBER = "all-crs-StateId";
    protected static final String ALIAS_DUAL_ENROLLMENT_MST = "all-mst-DualEnrollmentIndicator";
    protected static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";
    protected static final String ALIAS_CTE_PROGRAM_CODE_CRS = "all-crs-CTEProgramCode";
    protected static final String ALIAS_VIRTUAL_COURSE_LOCATION = "all-mst-VirtualCourseLocation";
    protected static final String ALIAS_VIRTUAL_INSTRUCTION_PROVIDER = "all-mst-VirtualInstructionProvider";
    protected static final String ALIAS_ELL_MODEL_MST = "all-mst-ELLInstructionalModel";

    private DataDictionaryField m_fieldDualEnrInd;
    private DataDictionaryField m_fieldStateId;
    private DataDictionaryField m_fieldSchoolNumber;
    private DataDictionaryField m_fieldCteProgramCode;
    private DataDictionaryField m_fieldVirtualLocation;
    private DataDictionaryField m_fieldVirtualInstructionProvider;
    private DataDictionaryField m_fieldELLModel;
    private DataDictionaryField m_fieldMstNumber;

    private Set<MasterSchedule> m_notScheduledMst = new HashSet<>();
    private int m_notScheduledRecords = 0;

    private FLStudentHelper m_studentHelper;
    private FLScheduleHelper m_scheduleHelper;
    private StudentScheduleHelper m_studentScheduleHelper;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        // for (int i = 0; i < 3; i++) {
        // super.execute();
        runAndValidateExports();
        // }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FLExportConfiguration.FL_EXPORT.SSC, SisStudent.class);
        includeExports(Arrays.asList(FL_EXPORT.STD, FL_EXPORT.MTC));
        m_fieldSchoolNumber = getFLReportData().translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER, true);
        m_fieldStateId = getFLReportData().getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_COURSE_NUMBER);
        m_fieldCteProgramCode = getFLReportData().translateAliasToDictionaryField(ALIAS_CTE_PROGRAM_CODE_CRS, false);
        m_fieldVirtualLocation =
                getFLReportData().translateAliasToDictionaryField(ALIAS_VIRTUAL_COURSE_LOCATION, false);
        m_fieldVirtualInstructionProvider =
                getFLReportData().translateAliasToDictionaryField(ALIAS_VIRTUAL_INSTRUCTION_PROVIDER, false);
        m_fieldELLModel = getFLReportData().translateAliasToDictionaryField(ALIAS_ELL_MODEL_MST, false);
        m_fieldDualEnrInd = getFLReportData().translateAliasToDictionaryField(ALIAS_DUAL_ENROLLMENT_MST, false);
        m_fieldMstNumber = getFLReportData().translateAliasToDictionaryField(ALIAS_MST_STATE_ID, false);

        m_studentHelper = getFLReportData().getStudentHelper();
        m_scheduleHelper =
                new FLScheduleHelper(getFLReportData(), getFLReportData().getSurveyPeriod().getStartDate(),
                        getFLReportData().getSurveyPeriod().getEndDate());
        m_studentScheduleHelper = m_studentHelper.new StudentScheduleHelper(m_scheduleHelper,
                getFLReportData().getSurveyPeriod().getStartDate(), getFLReportData().getSurveyPeriod().getEndDate());

        initializeRule3Fix();
        initializeRule12Fix();
        initializeRule15Fix();
        initializeRule21Fix();
        initializeRule2AFix();
        initializeRule2BFix();
        initializeRule41Fix();
        initializeRule4HFix();
        initializeRule48Fix();
        initializeRule49Fix();
        initializeRule51Fix();
        initializeRule54Fix();
        initializeRule5KFix();
        initializeRule5MFix();
        initializeRule5CFix();
        initializeRule5VFix();
        initializeRule5WFix();
    }

    private List<StudentScheduleInfo> getStudentSchInfos(SisStudent student) {
        List<StudentScheduleInfo> records = null;
        try {
            if (getStudentHelper().isStudentEligible(student)) {
                records = ((FLStudentCourseScheduleData) getFLReportData()).m_studentScheduleHelper
                        .getStudentScheduleInfo(student);
                // remove duplicate sections
                Set<String> mstOids = new HashSet();
                Iterator<StudentScheduleInfo> iterator = records.iterator();
                while (iterator.hasNext()) {
                    StudentScheduleInfo item = iterator.next();
                    String mstOid = item.getSection().getOid();
                    if (mstOids.contains(mstOid)) {
                        iterator.remove();
                    } else {
                        mstOids.add(mstOid);
                    }
                }
            }
        } catch (X2BaseException e) {
            e.printStackTrace();
        }
        return records;
    }

    /**
     * Initialize rule 3 fix.
     */
    private void initializeRule3Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.pattern("Student Number", "^\\d{9}(\\d|X)$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number", "^\\d{10}$"))
                                        .testThen(Restriction.pattern("Student Number",
                                                "^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|7[8-9]).{8}$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number", "^\\d{9}X$"))
                                        .testThen(Restriction.pattern("Student Number", "^(?!000).{10}$"))),
                        "The first nine positions of Student Number Identifier, Florida must be "
                                + "numeric. The tenth position of Student Number Identifier, Florida must either be "
                                + "an \"X\" or numeric. If the tenth position of Student Number Identifier, Florida is "
                                + "numeric, the first two digits must be a valid district number in the range 01-68, 71-"
                                + "75 or 78-79. If the tenth position of the Student Number Identifier, Florida is an "
                                + "\"X\", the first three positions may not all be zeroes."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // String currentId = (String)
                        // bean.getFieldValueByAlias(ALIAS_STD_STATE_ID);
                        // if (currentId == null) {
                        // currentId =
                        // String.valueOf(ThreadLocalRandom.current().nextLong(1000000000l,
                        // 9999999999l));
                        // }
                        // currentId = StringUtils.padLeft(currentId, 10, '0');
                        // String randomDistrict = "";
                        // if (currentId.substring(9).matches("\\d")
                        // && !currentId.substring(0, 2).matches("(?!(00|69|7[067]|[89]))\\d{2}")) {
                        // while (StringUtils.isEmpty(randomDistrict)
                        // || !randomDistrict.matches("(?!(00|69|7[067]|[89]))\\d{2}")) {
                        // randomDistrict = String.valueOf(ThreadLocalRandom.current().nextInt(79));
                        // }
                        // } else if (currentId.substring(9).matches("X")
                        // && currentId.substring(0,
                        // 3).matches(("(?!(00|69|7[067]|[89])|0{3})\\d{3}"))) {
                        // while (randomDistrict == null
                        // || !randomDistrict.matches("(?!(00|69|7[067]|[89])|0{3})\\d{3}")) {
                        // randomDistrict =
                        // String.valueOf(ThreadLocalRandom.current().nextInt(999));
                        // }
                        // }
                        //
                        // if (!StringUtils.isEmpty(randomDistrict)) {
                        // currentId = randomDistrict + currentId.substring(2);
                        // }
                        // bean.setFieldValueByAlias(ALIAS_STD_STATE_ID, currentId);
                        // getModelBroker().saveBean(bean);
                    }
                }));

        addFixesByRuleNumber("3", ruleWithFixes);
    }

    /**
     * Initialize rule 12 fix.
     */
    private void initializeRule12Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Instruct School", "^(?!P\\S{1,3})\\S{1,4}$"),
                                Restriction.pattern("Grade Level", "^(0[1-9]|1[0-2])$"))
                                .testThen(Restriction.pattern("Course Number",
                                        "^(?!\\d{1,4}980|\\d{1,4}990)\\d{1,7}|0500980$")),
                        ValidationRule.testIf(Restriction.pattern("Instruct School", "^(?!P\\S{1,3})\\S{1,4}$"),
                                Restriction.pattern("Grade Level", "^(30|31)$"))
                                .testThen(Restriction.pattern("Course Number", "^(?![A-Z]0{6})\\d{1,7}$")),
                        ValidationRule.testIf(Restriction.pattern("Instruct School", "^(?!P\\S{1,3})\\S{1,4}$"))
                                .testThen(Restriction.pattern("Course Number", "^(?!00\\S{1,5})\\S{1,7}$"))),
                        "If the School Number Current Instruction/Service does not begin with P the Course Number must not be a local use only transfer"
                                + " course number unless Course Number = 0500980. If the School Number Current Instruction/Service does not begin with P "
                                + "then the Course Number must not begin with 00. Local use only transfer course numbers for Grade Levels 09-12 are numeric course numbers "
                                + "that end in 980 or 990. Local use only transfer course numbers for Grade Levels 30-31 are alphanumeric course numbers "
                                + "that begin with one alpha character and end with six zeroes."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // try {
                        // SisStudent student = (SisStudent) bean;
                        // List<StudentScheduleInfo> studentScheduleInfo =
                        // m_studentScheduleHelper.getStudentScheduleInfo(student);
                        //
                        // for (StudentScheduleInfo info : studentScheduleInfo) {
                        // SisSchool school = info.getSchool();
                        // String schoolNumber =
                        // getFLReportData().getFieldValue(school, m_fieldSchoolNumber).toString();
                        //
                        // Course course = info.getSection().getSchoolCourse().getCourse();
                        //
                        // String gradeLevel = info.getStudentInfo()
                        // .getGradeLevel(getFLReportData().getSurveyPeriod().getDateCertain());
                        //
                        // if (!schoolNumber.startsWith("P")) {
                        // String newValue = getRandomCodeForField(m_fieldStateId);
                        // while (newValue.startsWith("00")) {
                        // newValue = getRandomCodeForField(m_fieldStateId);
                        // }
                        //
                        // // course.setFieldValueByAlias(ALIAS_COURSE_NUMBER, newValue);
                        // course.setFieldValueByBeanPath(m_fieldStateId.getJavaName(), newValue);
                        // List<ValidationError> errors = getModelBroker().saveBean(course);
                        // }
                        // }
                        // } catch (X2BaseException e) {
                        // e.printStackTrace();
                        // }
                    }
                }));

        addFixesByRuleNumber("12", ruleWithFixes);
    }

    /**
     * Initialize rule 15 fix.
     */
    private void initializeRule15Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Dual Enrollment", "^(A|B|C|E|Z)$",
                        "Dual Enrollment Indicator must be A, B, C, E, or Z."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        List<StudentScheduleInfo> schInfos = getStudentSchInfos(student);
                        try {
                            for (StudentScheduleInfo schInfo : schInfos) {
                                String dualEnrInd = schInfo.getDualEnrollmentIndicator();
                                if (dualEnrInd == null || !dualEnrInd.matches("^(A|B|C|E|Z)$")) {
                                    MasterSchedule section = schInfo.getSection();
                                    section.setFieldValueByBeanPath(m_fieldDualEnrInd.getJavaName(),
                                            getRandomCodeForFieldByPattern(m_fieldDualEnrInd, "^(A|B|C|E|Z)$"));
                                    getModelBroker().saveBeanForced(section);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));

        addFixesByRuleNumber("15", ruleWithFixes);
    }

    /**
     * Initialize rule 2A fix.
     */
    private void initializeRule2AFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Dual Enrollment", "A"),
                        Restriction.pattern("Instruct School", "^(?!(?!P000)P\\d{3})\\S{1,4}$"))
                        .testThen(Restriction.pattern("Course Number", "^[A-Z]\\d{1,6}$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        List<StudentScheduleInfo> schInfos = getStudentSchInfos(student);
                        try {
                            for (StudentScheduleInfo schInfo : schInfos) {
                                String dualEnrInd = schInfo.getDualEnrollmentIndicator();
                                String sectionSchool = (String) getFLReportData().getFieldValue(schInfo.getSchool(),
                                        m_fieldSchoolNumber);
                                if (dualEnrInd != null && dualEnrInd.matches("^A$")
                                        && !sectionSchool.matches("^(?!(?!P000)P\\d{3})\\S{1,4}$")) {
                                    MasterSchedule section = schInfo.getSection();
                                    section.setFieldValueByBeanPath(m_fieldDualEnrInd.getJavaName(),
                                            getRandomCodeForFieldByPattern(m_fieldDualEnrInd, "^(B|C|E|Z)$"));
                                    getModelBroker().saveBeanForced(section);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));

        addFixesByRuleNumber("2A", ruleWithFixes);
    }

    /**
     * Initialize rule 20 fix.
     */
    private void initializeRule20Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Dual Enrollment", "^(A|B|C|E)$"))
                        .testThen(Restriction.pattern("FEFP Program Number", "(102|103|112|113)")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {/* No Fix needed */}
                }));

        addFixesByRuleNumber("20", ruleWithFixes);
    }

    /**
     * Initialize rule 21 fix.
     */
    private void initializeRule21Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Dual Enrollment", "A"))
                        .testThen(Restriction.pattern("Instruct School",
                                "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3})$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        List<StudentScheduleInfo> schInfos = getStudentSchInfos(student);
                        try {
                            for (StudentScheduleInfo schInfo : schInfos) {
                                String dualEnrInd = schInfo.getDualEnrollmentIndicator();
                                String sectionSchool = (String) getFLReportData().getFieldValue(schInfo.getSchool(),
                                        m_fieldSchoolNumber);
                                if (dualEnrInd != null && dualEnrInd.matches("^A$")
                                        && !sectionSchool.matches(
                                                "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3})$")) {
                                    MasterSchedule section = schInfo.getSection();
                                    section.setFieldValueByBeanPath(m_fieldDualEnrInd.getJavaName(),
                                            getRandomCodeForFieldByPattern(m_fieldDualEnrInd, "^(B|C|E|Z)$"));
                                    getModelBroker().saveBeanForced(section);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));

        addFixesByRuleNumber("21", ruleWithFixes);
    }

    /**
     * Initialize rule 2B fix.
     */
    private void initializeRule2BFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression(
                        "Course Grade",
                        "^(\\s{2}([A-D]|F|I|N|U|P|S|E|W|Z)|\\s(A[+-]|B[+-]|C[+-]|D[+-]|IP|WP|FL|NG|WF))$",
                        "The Course Grade code must be A+, A, A-, B+, B, B-, C+, C, C-, D+, D, D-, F, I, IP, N, U, P, S, E, WP, FL, NG, W, WF or Z "
                                + "and must be right justified with leading blanks."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // try {
                        // SisStudent student = (SisStudent) bean;
                        //
                        // List<StudentScheduleSpan> spans =
                        // getStudentHelper().getStudentScheduleSpans(student);
                        // for (StudentScheduleSpan span : spans) {
                        // if (span.getTranscript() != null) {
                        // Transcript transcript = span.getTranscript();
                        // transcript.setFinalGrade(" A+");
                        // getModelBroker().saveBean(transcript);
                        // }
                        // }
                        // } catch (Exception e) {
                        // e.printStackTrace();
                        // }
                    }
                }));

        addFixesByRuleNumber("2B", ruleWithFixes);
    }

    /**
     * Initialize rule 41 fix.
     */
    private void initializeRule41Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.or(
                                        Restriction.byAliasFldRefTable("all-crs-CTEProgramCode", "CTE Program Code"),
                                        Restriction.equals("CTE Program Code", "0000000")))),
                        "The Career and Technical Education/Adult General Education Program Code must be a program number from the "
                                + "Career and Technical Education/Adult General Education Program Edit file (F61730) or must be zero-filled."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // try {
                        // SisStudent student = (SisStudent) bean;
                        // List<StudentScheduleInfo> studentScheduleInfo =
                        // m_studentScheduleHelper.getStudentScheduleInfo(student);
                        //
                        // for (StudentScheduleInfo info : studentScheduleInfo) {
                        // Course course = info.getSection().getSchoolCourse().getCourse();
                        // course.setFieldValueByBeanPath(m_fieldCteProgramCode.getJavaName(),
                        // "0000000");
                        // List<ValidationError> errors = getModelBroker().saveBean(course);
                        // }
                        // } catch (Exception e) {
                        // e.printStackTrace();
                        // }
                    }
                }));

        addFixesByRuleNumber("41", ruleWithFixes);
    }

    /**
     * Initialize rule 48 fix.
     */
    private void initializeRule48Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Dual Enrollment", "E"))
                        .testThen(Restriction.pattern("Instruct School",
                                "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3})$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        List<StudentScheduleInfo> schInfos = getStudentSchInfos(student);
                        try {
                            for (StudentScheduleInfo schInfo : schInfos) {
                                String dualEnrInd = schInfo.getDualEnrollmentIndicator();
                                String sectionSchool = (String) getFLReportData().getFieldValue(schInfo.getSchool(),
                                        m_fieldSchoolNumber);
                                if (dualEnrInd != null && dualEnrInd.matches("^E$")
                                        && !sectionSchool.matches(
                                                "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3})$")) {
                                    MasterSchedule section = schInfo.getSection();
                                    section.setFieldValueByBeanPath(m_fieldDualEnrInd.getJavaName(),
                                            getRandomCodeForFieldByPattern(m_fieldDualEnrInd, "^(C|E|Z)$"));
                                    getModelBroker().saveBeanForced(section);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
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
                new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Period Number", "9800"))
                                .testThen(Restriction.equals("Days Per Week", "0")),
                        ValidationRule.testIf(Restriction.notEquals("Period Number", "9800"))
                                .testThen(Restriction.pattern("Days Per Week", "^[1-7]$"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        String currentMstId = getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.SSC,
                                "Section Number");
                        String courseId = getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.SSC,
                                "Course Number");
                        SisStudent student = (SisStudent) bean;
                        List<StudentScheduleInfo> schInfos = getStudentSchInfos(student);
                        for (StudentScheduleInfo schInfo : schInfos) {
                            MasterSchedule mst = schInfo.getSection();
                            if (currentMstId
                                    .endsWith((String) mst.getFieldValueByBeanPath(m_fieldMstNumber.getJavaName()))) {
                                Course course = mst.getSchoolCourse().getCourse();
                                String curCourseId =
                                        (String) course.getFieldValueByBeanPath(m_fieldStateId.getJavaName());
                                if (curCourseId.equals(courseId)) {
                                    m_notScheduledMst.add(mst);
                                }
                            }
                        }
                        ++m_notScheduledRecords;
                        System.out.println("check if it's a valid course without days, msts: "
                                + m_notScheduledMst.size() + ", records: "
                                + m_notScheduledRecords);
                        try {
                            List<StudentScheduleInfo> studentScheduleInfo =
                                    m_studentScheduleHelper.getStudentScheduleInfo(student);

                            for (StudentScheduleInfo info : studentScheduleInfo) {
                                MasterSchedule masterSchedule = info.getSection();
                                masterSchedule.setFieldValueByBeanPath(m_fieldVirtualInstructionProvider.getJavaName(),
                                        null);
                                getModelBroker().saveBeanForced(masterSchedule);
                            }
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                }));

        addFixesByRuleNumber("49", ruleWithFixes);
    }

    /**
     * Initialize rule 4H fix.
     */
    private void initializeRule4HFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.pattern("Location of Student", "^(T|Z)$"))),
                        "If School Number Current Instruction/Service"
                                + " equals 7001, 7004, 7006, or 7023;"
                                + " or if District Number Current Instruction/Service equals 71;"
                                + " or District Number, Current Instruction/Service=50 and School Number, Current Instruction/Service = 7079;"
                                + " or Charter School Status is not Z and School Function/Setting = V on the Master School Identification file, "
                                + "then Location of Student must be N or S. All other schools must be T or Z."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        // try {
                        // SisStudent student = (SisStudent) bean;
                        // List<StudentScheduleInfo> studentScheduleInfo =
                        // m_studentScheduleHelper.getStudentScheduleInfo(student);
                        //
                        // for (StudentScheduleInfo info : studentScheduleInfo) {
                        // MasterSchedule masterSchedule = info.getSection();
                        // masterSchedule.setFieldValueByBeanPath(m_fieldVirtualLocation.getJavaName(),
                        // "Z");
                        // List<ValidationError> errors = getModelBroker().saveBean(masterSchedule);
                        // }
                        // } catch (Exception e) {
                        // e.printStackTrace();
                        // }
                    }
                }));

        addFixesByRuleNumber("4H", ruleWithFixes);
    }

    /**
     * Initialize rule 51 fix.
     */
    private void initializeRule51Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("Grade Level", "^(0[7-9]|1[0-2])$"))
                                .testThen(Restriction.greaterThan("Class Minutes", Double.valueOf(0))),
                        ValidationRule.testIf(Restriction.equals("Period Number", "9800"))
                                .testThen(Restriction.equals("Class Minutes", "0"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        System.out.println("check if it's a valid course without minutes");
                    }
                }));

        addFixesByRuleNumber("51", ruleWithFixes);
    }

    /**
     * Initialize rule 54 fix.
     */
    private void initializeRule54Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Virtual Instruction", "^(071|308|309|311)$"))
                                .testThen(Restriction.pattern("Grade Level", "^(0[6-9]|1[0-2])$")),
                        ValidationRule.testIf(Restriction.equals("Virtual Instruction", "301"))
                                .testThen(Restriction.pattern("Grade Level", "^(KG|0[1-5])$")),
                        ValidationRule.testIf(Restriction.pattern("Virtual Instruction", "^(302|313)$"))
                                .testThen(Restriction.pattern("Grade Level", "^(KG|0[1-9]|1[0-2])$"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        String gradeLevel =
                                getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.SSC, "Grade Level");
                        String virtInstr = getHelper().getExportFormatRowFieldValue(getCurrentRow(), FL_EXPORT.SSC,
                                "Virtual Instruction");
                        String valueToSet = null;
                        if (gradeLevel.matches("^(0[6-9]|1[0-2])$")) {
                            valueToSet =
                                    getRandomCodeForField(m_fieldVirtualInstructionProvider, "^(071|308|309|311)$");
                        } else if (gradeLevel.matches("^(KG|0[1-5])$")) {
                            valueToSet = "301";
                        } else if (gradeLevel.matches("^(KG|0[1-9]|1[0-2])$")) {
                            valueToSet = getRandomCodeForField(m_fieldVirtualInstructionProvider, "^(302|313)$");
                        }

                        try {
                            SisStudent student = (SisStudent) bean;
                            List<StudentScheduleInfo> studentScheduleInfo =
                                    m_studentScheduleHelper.getStudentScheduleInfo(student);

                            for (StudentScheduleInfo info : studentScheduleInfo) {
                                MasterSchedule masterSchedule = info.getSection();
                                if (virtInstr
                                        .equals(masterSchedule.getFieldValueByBeanPath(
                                                m_fieldVirtualInstructionProvider.getJavaName()))) {
                                    masterSchedule.setFieldValueByBeanPath(
                                            m_fieldVirtualInstructionProvider.getJavaName(), valueToSet);
                                    getModelBroker().saveBeanForced(masterSchedule);
                                }
                            }
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                }));

        addFixesByRuleNumber("54", ruleWithFixes);
    }

    /**
     * Initialize rule 5K fix.
     */
    private void initializeRule5KFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.notEquals("School Number", "3518"),
                                Restriction.pattern("Instruct School", "^(?!7001|7004|7006|7023)\\S{1,4}$"))
                                .testThen(Restriction.or(Restriction.equals("Scheduled Monday", "Y"),
                                        Restriction.equals("Scheduled Tuesday", "Y"),
                                        Restriction.equals("Scheduled Wednesday", "Y"),
                                        Restriction.equals("Scheduled Thursday", "Y"),
                                        Restriction.equals("Scheduled Friday", "Y"),
                                        Restriction.equals("Scheduled Saturday", "Y")))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        System.out.println("check if it's a valid course without sheduled days");
                    }
                }));

        addFixesByRuleNumber("5K", ruleWithFixes);
    }

    /**
     * Initialize rule 5M fix.
     */
    private void initializeRule5MFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.equals("Scheduled Friday", "Y"))
                                .testThen(Restriction.pattern("Scheduled Certain", "^(N|Z)$"))),
                        "If Survey Period code is 2 or 3 and Day of Week, Scheduled Friday is Y, then Day of Week Scheduled, Date Certain must be N or Z."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            SisStudent student = (SisStudent) bean;
                            List<StudentScheduleInfo> studentScheduleInfo =
                                    m_studentScheduleHelper.getStudentScheduleInfo(student);

                            for (StudentScheduleInfo info : studentScheduleInfo) {
                                info.getSection();
                            }
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                }));

        addFixesByRuleNumber("5M", ruleWithFixes);
    }

    /**
     * Initialize rule 5C fix.
     */
    private void initializeRule5CFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("FEFP Program Number", "130"),
                                Restriction.notEquals("Instruct School", "3518"))
                                .testThen(Restriction.pattern("ELL Instruct Model", "^(C|E|I|O|S|T)$"))),
                        "If FEFP Program Number is 130, English Language Learners: Instructional Model code must be C, E, I, O, S, or T "
                                + "unless School Number, Current Enrollment equals 3518."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        List<StudentScheduleInfo> schInfos = getStudentSchInfos(student);
                        try {
                            for (StudentScheduleInfo schInfo : schInfos) {
                                String fefpProgramNumber = schInfo.getStudentInfo().getFefpProgram();
                                String sectionSchool = (String) getFLReportData().getFieldValue(schInfo.getSchool(),
                                        m_fieldSchoolNumber);
                                String ellInstModel = schInfo.getEllInstructionalModel();
                                if ("130".equals(fefpProgramNumber) && !"3518".equals(sectionSchool)
                                        && (ellInstModel == null || !ellInstModel.matches("^(C|E|I|O|S|T)$"))) {
                                    MasterSchedule section = schInfo.getSection();
                                    String sectionEllModel =
                                            (String) section.getFieldValueByBeanPath(m_fieldELLModel.getJavaName());
                                    if (sectionEllModel == null || !sectionEllModel.matches("^(C|E|I|O|S|T)$")) {
                                        section.setFieldValueByBeanPath(m_fieldELLModel.getJavaName(),
                                                getRandomCodeForFieldByPattern(m_fieldELLModel, "^(C|E|I|O|S|T)$"));
                                        getModelBroker().saveBeanForced(section);
                                        System.out.println(section + " saved");
                                    }
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));

        addFixesByRuleNumber("5C", ruleWithFixes);
    }

    /**
     * Initialize rule 5V fix.
     */
    private void initializeRule5VFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|4)$"),
                        Restriction.pattern("ELL Instruct Model", "^(S|C)$"))
                        .testThen(Restriction.pattern("Course Number",
                                "^(12\\d{5}|20\\d{5}|21\\d{5}|82\\d{5})$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        List<StudentScheduleInfo> schInfos = getStudentSchInfos(student);
                        for (StudentScheduleInfo schInfo : schInfos) {
                            Course course = schInfo.getCourse();
                            if (course != null) {
                                String courseNumber =
                                        (String) course.getFieldValueByBeanPath(m_fieldStateId.getJavaName());
                                if (courseNumber != null
                                        && !courseNumber.matches("^(12\\d{5}|20\\d{5}|21\\d{5}|82\\d{5})$")) {
                                    course.setFieldValueByBeanPath(m_fieldStateId.getJavaName(),
                                            getRandomCodeForFieldByPattern(m_fieldStateId,
                                                    "^(12\\d{5}|20\\d{5}|21\\d{5}|82\\d{5})$"));
                                    getModelBroker().saveBeanForced(course);
                                }

                                // adjust all master schedules that relate to this course
                                X2Criteria criteria = new X2Criteria();
                                criteria.addEqualTo(
                                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER
                                                + SchoolCourse.REL_COURSE
                                                + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                                        course.getOid());
                                QueryByCriteria query = new QueryByCriteria(MasterSchedule.class, criteria);
                                Collection<MasterSchedule> msts = getModelBroker().getCollectionByQuery(query);
                                for (MasterSchedule mst : msts) {
                                    String ellInstModel =
                                            (String) mst.getFieldValueByBeanPath(m_fieldELLModel.getJavaName());
                                    if (ellInstModel == null || !ellInstModel.matches("^(S|C)$")) {
                                        mst.setFieldValueByBeanPath(m_fieldELLModel.getJavaName(),
                                                ThreadLocalRandom.current().nextBoolean() ? "S" : "C");
                                        getModelBroker().saveBeanForced(mst);
                                    }
                                }
                            }
                        }
                    }
                }));

        addFixesByRuleNumber("5V", ruleWithFixes);
    }

    /**
     * Initialize rule 5W fix.
     */
    private void initializeRule5WFix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|4)$"),
                        Restriction.pattern("ELL Instruct Model", "^(E|I)$"))
                        .testThen(Restriction.pattern("Course Number", "^10\\d{5}|5010\\d{3}$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        List<StudentScheduleInfo> schInfos = getStudentSchInfos(student);
                        for (StudentScheduleInfo schInfo : schInfos) {
                            Course course = schInfo.getCourse();
                            if (course != null) {
                                String courseNumber =
                                        (String) course.getFieldValueByBeanPath(m_fieldStateId.getJavaName());
                                if (courseNumber != null && !courseNumber.matches("^10\\d{5}|5010\\d{3}$")) {
                                    course.setFieldValueByBeanPath(m_fieldStateId.getJavaName(),
                                            getRandomCodeForFieldByPattern(m_fieldStateId, "^10\\d{5}|5010\\d{3}$"));
                                    getModelBroker().saveBeanForced(course);
                                }

                                // adjust all master schedules that relate to this course
                                X2Criteria criteria = new X2Criteria();
                                criteria.addEqualTo(
                                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER
                                                + SchoolCourse.REL_COURSE
                                                + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                                        course.getOid());
                                QueryByCriteria query = new QueryByCriteria(MasterSchedule.class, criteria);
                                Collection<MasterSchedule> msts = getModelBroker().getCollectionByQuery(query);
                                for (MasterSchedule mst : msts) {
                                    String ellInstModel =
                                            (String) mst.getFieldValueByBeanPath(m_fieldELLModel.getJavaName());
                                    if (ellInstModel == null || !ellInstModel.matches("^(E|I)$")) {
                                        mst.setFieldValueByBeanPath(m_fieldELLModel.getJavaName(),
                                                ThreadLocalRandom.current().nextBoolean() ? "E" : "I");
                                        getModelBroker().saveBeanForced(mst);
                                    }
                                }
                            }
                        }
                    }
                }));

        addFixesByRuleNumber("5W", ruleWithFixes);
    }
}
