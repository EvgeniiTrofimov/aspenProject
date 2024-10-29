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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLCTEStudentCourseScheduleData.RetrieveCteFields;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetProcedureCAPEIC.
 */
public class SetProcedureCAPEIC extends SetProcedure {

    private static final String ALIAS_CRS_CTE_PROGRAM_CODE = "all-crs-CTEProgramCode";
    private static final String ALIAS_CRS_STATE_ID = "all-crs-StateId";

    private static final String ALIAS_ORG_DISTRICT_NUMBER = "all-org-DistrictNumber";
    private static final String ALIAS_ORG_STATE_ID = "all-org-StateId";

    private static final String ALIAS_PGM_CAPE_DISCTRICT = "pgm-cape-district";
    private static final String ALIAS_PGM_CAPE_ID = "pgm-cape-identifier";
    private static final String ALIAS_PGM_CAPE_INDUSTRY_CERT = "pgm-cape-industry-cert";
    private static final String ALIAS_PGM_CAPE_INDUSTRY_CERT_ID = "pgm-cape-industry-cert-id";
    private static final String ALIAS_PGM_CAPE_SCHOOL = "pgm-cape-school";

    private static final String ALIAS_SKL_STATE_ID = "all-skl-StateId";

    private static final String DDX_ID = "FL-PGM-CAPE";

    private static final String PROGRAM_CODE_CAPE = "CAPE";

    private DataDictionary m_dictionary = null;

    private DataDictionaryField m_fieldCapeDistrict;
    private DataDictionaryField m_fieldCapeId;
    private DataDictionaryField m_fieldCapeIndustryCert;
    private DataDictionaryField m_fieldCapeIndustryCertId;
    private DataDictionaryField m_fieldCapeSchool;

    private DataDictionaryField m_fieldCteProgramCode;

    private DataDictionaryField m_fieldStateId;

    private Random m_random = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
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
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FLExportConfiguration.FL_EXPORT.CAPEIC, SisStudent.class);

        m_random = new Random(System.currentTimeMillis());

        X2Criteria ddxCriteria = new X2Criteria();
        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_ID);
        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        m_dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());

        m_fieldCapeDistrict = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_CAPE_DISCTRICT);
        m_fieldCapeId = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_CAPE_ID);
        m_fieldCapeIndustryCert = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_CAPE_INDUSTRY_CERT);
        m_fieldCapeIndustryCertId = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_CAPE_INDUSTRY_CERT_ID);
        m_fieldCapeSchool = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_CAPE_SCHOOL);

        m_fieldStateId = getFLReportData().getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRS_STATE_ID);

        m_fieldCteProgramCode = getFLReportData().getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRS_CTE_PROGRAM_CODE);

        getFLReportData().getStudentHelper().getStudentScheduleCriteria()
                .addNotEmpty(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                        m_fieldCteProgramCode.getJavaName(), getBroker().getPersistenceKey());
        getFLReportData().getStudentHelper().getStudentScheduleChangeCriteria()
                .addNotEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                        m_fieldCteProgramCode.getJavaName(), getBroker().getPersistenceKey());

        initializeRule2Fix();
        initializeRule6Fix();
        initializeRule8Fix();
        initializeRule9Fix();
        initializeRule10Fix();
        initializeRule11Fix();
        initializeRule41Fix();
    }

    private void addPrograms() throws X2BaseException {
        int numOfNeededStudents = 10;
        X2Criteria currentStudentsCriteria = getFLReportData().getStudentHelper().getStudentCriteria();
        SubQuery curStudentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, currentStudentsCriteria);
        Collection<String> currentStudents = getModelBroker().getSubQueryCollectionByQuery(curStudentsSubQuery);
        if (currentStudents.size() < numOfNeededStudents) {
            FLStudentHelper helper = new FLStudentHelper(getFLReportData());
            helper.getStudentCriteria().addNotIn(X2BaseBean.COL_OID, currentStudents);
            helper.getStudentCriteria().addIn(X2BaseBean.COL_OID,
                    helper.getStudentProgramDataset(
                            RetrieveCteFields.DDX_ID,
                            getFLReportData().getSurveyPeriod()).getStudentSubQuery());
            ArrayList<SisStudent> studentsWithoutCape =
                    new ArrayList<SisStudent>(getModelBroker().getCollectionByQuery(helper.getStudentQuery(true)));
            for (int i = 0; i <= numOfNeededStudents; i++) {
                SisStudent randomStudent = studentsWithoutCape.get(m_random.nextInt(studentsWithoutCape.size()));
                StudentProgramParticipation pgm = getNewProgram(randomStudent);
                getModelBroker().saveBean(pgm);
            }

        }
    }

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
        newPgm.setProgramCode(PROGRAM_CODE_CAPE);

        newPgm.setFieldValueByBeanPath(m_fieldCapeDistrict.getJavaName(), getRandomCodeForField(m_fieldCapeDistrict));
        newPgm.setFieldValueByBeanPath(m_fieldCapeId.getJavaName(), getRandomCodeForField(m_fieldCapeId));
        newPgm.setFieldValueByBeanPath(m_fieldCapeSchool.getJavaName(), getRandomCodeForField(m_fieldCapeSchool));

        return newPgm;
    }

    /**
     * Initialize rule 2 fix.
     */
    private void initializeRule2Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("School Number",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N99[89])$",
                        "School Number, Current Enrollment must be numeric in the range 0001 to 9899, excluding 9001, or it must be N998 or N999."),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisSchool school = ((SisStudent) bean).getSchool();
                        school.setFieldValueByAlias(ALIAS_SKL_STATE_ID,
                                String.format("%03d", Integer.valueOf(ThreadLocalRandom.current().nextInt(1, 9899))));
                        getModelBroker().saveBean(school);
                    }

                }));

        addFixesByRuleNumber("2", ruleWithFixes);
    }

    /**
     * Initialize rule 6 fix.
     */
    private void initializeRule6Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("District Number CIS",
                        "^0[1-9]|[1-6][0-8]|7[1-5]$",
                        "District Number, Current Instruction/Service must be numeric in the range 01-68 or 71-75 "
                                + "and must be correct for the district submitting the data."),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        Organization organization = ((SisStudent) bean).getOrganization1();
                        organization.setFieldValueByAlias(ALIAS_ORG_DISTRICT_NUMBER, organization.getFieldValueByAlias(ALIAS_ORG_STATE_ID));
                        getModelBroker().saveBean(organization);
                    }

                }));

        addFixesByRuleNumber("6", ruleWithFixes);
    }

    /**
     * Initialize rule 8 fix.
     */
    private void initializeRule8Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-industry-cert-id",
                                "Industry Cert Id")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            StudentProgramParticipation pgm = getFLReportData().getStudentHelper().getStudentProgram(bean.getOid(), DDX_ID,
                                    getFLReportData().getSurveyPeriod());
                            pgm.setFieldValueByBeanPath(m_fieldCapeIndustryCertId.getJavaName(),
                                    getRandomCodeForField(m_fieldCapeIndustryCertId));
                            getModelBroker().saveBean(pgm);
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }

                }));

        addFixesByRuleNumber("8", ruleWithFixes);
    }

    /**
     * Initialize rule 9 fix.
     */
    private void initializeRule9Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.notEquals("Industry Cert Id", "CAPEI001"))
                        .testThen(Restriction.pattern("Industry Cert Outcom", "^(A|C|F|I|P|Z)$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            StudentProgramParticipation pgm = getFLReportData().getStudentHelper().getStudentProgram(bean.getOid(), DDX_ID,
                                    getFLReportData().getSurveyPeriod());
                            pgm.setFieldValueByBeanPath(m_fieldCapeIndustryCert.getJavaName(),
                                    getRandomCodeForField(m_fieldCapeIndustryCert));
                            getModelBroker().saveBean(pgm);
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }

                }));

        addFixesByRuleNumber("9", ruleWithFixes);
    }

    /**
     * Initialize rule 10 fix.
     */
    private void initializeRule10Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("School Number CIS", "^(?!C|P|U\\S{1,3})\\S{1,4}$"),
                        Restriction.pattern("Grade Level", "^(KG|0[1-5])$"))
                        .testThen(Restriction.equals("Course Number", "0000000")),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentScheduleInfo> scheduleInfos =
                                ((FLIndustryCertificationData) getFLReportData()).m_studentScheduleHelper
                                        .getStudentScheduleInfo((SisStudent) bean);
                        for (StudentScheduleInfo info : scheduleInfos) {
                            Course course = info.getCourse();
                            course.setFieldValueByAlias(ALIAS_CRS_STATE_ID, null);
                            getModelBroker().saveBean(course);
                        }
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("School Number CIS", "^(?!C|P|U\\S{1,3})\\S{1,4}$"),
                        Restriction.pattern("Grade Level", "^(0[6-8])$"),
                        Restriction.pattern("Industry Cert Id", "^\\S{5}8\\S{2}$"))
                        .testThen(Restriction.or(
                                Restriction.pattern("Course Number", "0000000"),
                                Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        System.out.println("2");
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("School Number CIS", "^(?!C|P|U\\S{1,3})\\S{1,4}$"),
                        Restriction.pattern("Grade Level", "^(0[6-8])$"),
                        Restriction.pattern("Industry Cert Id", "^\\S{5}(?!8)\\S{3}$"))
                        .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentScheduleInfo> scheduleInfos =
                                ((FLIndustryCertificationData) getFLReportData()).m_studentScheduleHelper
                                        .getStudentScheduleInfo((SisStudent) bean);
                        for (StudentScheduleInfo info : scheduleInfos) {
                            Course course = info.getCourse();
                            course.setFieldValueByBeanPath(m_fieldStateId.getJavaName(), getRandomCodeForField(m_fieldStateId));

                            getModelBroker().saveBean(course);
                        }
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("School Number CIS", "^(?!C|P|U\\S{1,3})\\S{1,4}$"),
                        Restriction.pattern("Grade Level", "^(09|1[0-2])$"))
                        .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<StudentScheduleInfo> scheduleInfos =
                                ((FLIndustryCertificationData) getFLReportData()).m_studentScheduleHelper
                                        .getStudentScheduleInfo((SisStudent) bean);
                        for (StudentScheduleInfo info : scheduleInfos) {
                            Course course = info.getCourse();
                            course.setFieldValueByBeanPath(m_fieldStateId.getJavaName(), getRandomCodeForField(m_fieldStateId));
                            getModelBroker().saveBean(course);
                        }
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("School Number CIS", "^(C|U)\\S{1,3}$"))
                        .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        System.out.println("5");
                    }
                }));

        addFixesByRuleNumber("10", ruleWithFixes);
    }

    /**
     * Initialize rule 11 fix.
     */
    private void initializeRule11Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Course Number", "0000000"),
                        Restriction.pattern("Grade Level", "^(KG|0[1-8])$"))
                        .testThen(Restriction.equals("Cape Program Code", "0000000")),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            StudentProgramParticipation pgm = getFLReportData().getStudentHelper().getStudentProgram(bean.getOid(), DDX_ID,
                                    getFLReportData().getSurveyPeriod());
                            pgm.setFieldValueByBeanPath(m_fieldCapeDistrict.getJavaName(), "0000000");
                            getModelBroker().saveBean(pgm);
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Course Number",
                        "^(?!0000000|1006300|2001310|2001340|2003310|2102360|2102365|2102370|3027010|3027020|2000350|2000360)\\d\\S{6}$"))
                        .testThen(Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-district",
                                "Cape Program Code")),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        System.out.println("1");
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Course Number", "^[A-Z]{3}0\\S{3}$"),
                        Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-district", "Course Number"))
                        .testThen(Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-district",
                                "Cape Program Code")),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        System.out.println("2");
                    }
                }));

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Course Number", "^[A-Z]\\S{6}$"),
                        Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-district", "Course Number"))
                        .testThen(Restriction.or(
                                Restriction.equalsFieldValue("Cape Program Code", "Course Number",
                                        String.class),
                                Restriction.equals("Cape Program Code", "0000000"))),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        System.out.println("3");
                    }
                }));

        addFixesByRuleNumber("11", ruleWithFixes);
    }

    /**
     * Initialize rule 41 fix.
     */
    private void initializeRule41Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();

        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Course Number",
                        "^(?!0000000|1006300|2001310|2001340|2003310|2102360|2102365|2102370|3027010|3027020|2000350|2000360)\\d\\S{6}$"),
                        Restriction.equalsFieldValue("Cource School Year", "Fiscal Year", String.class)
                // ,
                // Restriction.byExtFldRefTable("FL-PGM-CAPE",
                // "pgm-cape-district", "Cape Program Code")
                )
                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-CTESSC",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number CIS", "Instruct District"),
                                new KeyValuePair("School Number CIS", "Instruct School"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Cape Program Code", "CTE Program Code"),
                                new KeyValuePair("Course Number", "Course Number"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"))),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        System.out.println("41");
                    }
                }));

        addFixesByRuleNumber("41", ruleWithFixes);
    }
}
