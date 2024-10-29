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
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolRoom;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffCertification;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Random;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class SetProcedureMTC.
 */
public class SetProcedureMTC extends SetProcedure {

    private static final String ALIAS_CERT_STATUS = "all-mtc-CertificationStatus";
    private static final String ALIAS_CLASSROOM_ID = "all-rms-ClassroomIdentificationNo";
    private static final String ALIAS_SCH_METHOD = "all-mtc-SchedulingMethod";


    private DataDictionaryField m_fieldCertificationStatus;
    private DataDictionaryField m_fieldClassroomIdentificationNo;
    private DataDictionaryField m_fieldScheduleTermCode;
    private DataDictionaryField m_fieldSchedulingMethod;

    private Random m_random = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        // addSchedules();
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
        super.initialize(FLExportConfiguration.FL_EXPORT.MTC, SisStaff.class);

        m_random = new Random(System.currentTimeMillis());

        m_fieldCertificationStatus = getFLReportData().translateAliasToDictionaryField(ALIAS_CERT_STATUS, true);
        m_fieldClassroomIdentificationNo = getFLReportData().translateAliasToDictionaryField(ALIAS_CLASSROOM_ID, true);
        m_fieldSchedulingMethod = getFLReportData().translateAliasToDictionaryField(ALIAS_SCH_METHOD, true);
        m_fieldScheduleTermCode = getFLReportData().getDataDictionary()
                .findDataDictionaryField(ScheduleTerm.class.getName(), ScheduleTerm.COL_CODE);

        initializeRule15Fix();
        initializeRule17Fix();
        initializeRule23Fix();
        initializeRule26Fix();
        initializeRule27Fix();
        initializeRule28Fix();
        initializeRule63Fix();
        initializeRule67Fix();
    }

    /**
     * Adds the schedules.
     *
     * @throws X2BaseException exception
     */
    private void addSchedules() throws X2BaseException {

        int numOfNeededSchedules = 1;

        X2Criteria scheduleCriteria = getFLReportData().getStaffHelper().getTeacherScheduleCriteria();
        QueryByCriteria scheduleQuery = new QueryByCriteria(ScheduleTeacher.class, scheduleCriteria);
        Map<String, List<ScheduleTeacher>> currentStudents = getModelBroker().getGroupedCollectionByQuery(scheduleQuery,
                ScheduleTeacher.COL_STAFF_OID, 500);
        if (currentStudents.size() < numOfNeededSchedules) {
            try {
                FLStaffHelper helper = new FLStaffHelper(getFLReportData());
                helper.setStaffSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
                helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                        getCurrentContext().getStartDate());
                helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getCurrentContext().getEndDate());
                ArrayList<SisStaff> newStaff =
                        new ArrayList<SisStaff>(getBroker().getCollectionByQuery(helper.getStaffQuery(false)));
                for (int i = 0; i <= numOfNeededSchedules; i++) {
                    SisStaff randomStaff = newStaff.get(m_random.nextInt(newStaff.size()));
                    ScheduleTeacher sch = getNewSchedule(randomStaff);
                    getModelBroker().saveBeanForced(sch);
                }
            } catch (X2BaseException e) {
                e.printStackTrace();
            }

        }
    }

    /**
     * Gets the new schedule.
     *
     * @param stf SisStaff
     * @return Schedule teacher
     * @throws X2BaseException exception
     */
    private ScheduleTeacher getNewSchedule(SisStaff stf) throws X2BaseException {

        SchoolCourse course =
                new SchoolCourse(getBroker().getPersistenceKey());
        course.setMasterType(SchoolCourse.MASTER_TYPE_CLASS);
        getModelBroker().saveBeanForced(course);

        ScheduleTerm term =
                new ScheduleTerm(getBroker().getPersistenceKey());
        getModelBroker().saveBeanForced(term);

        ScheduleTermDate termDate =
                new ScheduleTermDate(getBroker().getPersistenceKey());
        termDate.setStartDate(new PlainDate((Date) new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "02")
                .getRuntimeObject(getHelper())));
        termDate.setEndDate(new PlainDate((Date) new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "02")
                .getRuntimeObject(getHelper())));
        termDate.setScheduleTermOid(term.getOid());
        getModelBroker().saveBeanForced(termDate);

        MasterSchedule masterSch =
                new MasterSchedule(getBroker().getPersistenceKey());
        masterSch.setPrimaryStaffOid(stf.getOid());
        masterSch.setPrimaryRoomOid(getNewSchoolRoomOid(stf));
        masterSch.setTermView(term.getOid());
        masterSch.setStaffView(stf.getOid());
        masterSch.setCourseView(course.getOid());
        getModelBroker().saveBeanForced(masterSch);

        ScheduleTeacher newSchedule =
                new ScheduleTeacher(getBroker().getPersistenceKey());
        newSchedule.setStaffOid(stf.getOid());
        newSchedule.setSectionOid(masterSch.getOid());
        newSchedule.setScheduleTermOid(term.getOid());

        return newSchedule;
    }

    /**
     * Gets the new school room oid.
     *
     * @param stf SisStaff
     * @return String
     */
    private String getNewSchoolRoomOid(SisStaff stf) {
        SchoolRoom newRoom =
                new SchoolRoom(getBroker().getPersistenceKey());
        newRoom.setSchoolOid(stf.getSchoolOid());
        newRoom.setFieldValueByBeanPath(m_fieldClassroomIdentificationNo.getJavaName(),
                "00256A0123400456ABCDE");

        getModelBroker().saveBeanForced(newRoom);

        return newRoom.getOid();
    }

    /**
     * Gets the schedules.
     *
     * @param staff SisStaff
     * @return List
     */
    private List<ScheduleTeacher> getSchedules(SisStaff staff) {
        List<ScheduleTeacher> schedules = null;
        if (staff != null) {
            try {
                schedules = getFLReportData().getStaffHelper().getTeacherSchedules(staff.getOid());
            } catch (X2BaseException e) {
                e.printStackTrace();
            }
        }
        return schedules;
    }

    /**
     * Initialize rule 15 fix.
     */
    private void initializeRule15Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                        Restriction.equals("Term", "3"))
                        .testThen(Restriction.equals("Survey Indicator", "Y")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<ScheduleTeacher> schedules = getSchedules((SisStaff) bean);
                        if (schedules != null && !schedules.isEmpty()) {
                            for (ScheduleTeacher schedule : schedules) {
                                ScheduleTerm term = schedule.getSection().getScheduleTerm();
                                term.setFieldValueByBeanPath(m_fieldScheduleTermCode.getJavaName(), "X");
                                getModelBroker().saveBeanForced(term);

                            }
                        }
                    }
                }));
        addFixesByRuleNumber("15", ruleWithFixes);
    }

    /**
     * Initialize rule 17 fix.
     */
    private void initializeRule17Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.equals("Survey Period", "3"),
                        Restriction.equals("Term", "2"))
                        .testThen(Restriction.equals("Survey Indicator", "Y")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<ScheduleTeacher> schedules = getSchedules((SisStaff) bean);
                        if (schedules != null && !schedules.isEmpty()) {
                            for (ScheduleTeacher schedule : schedules) {
                                ScheduleTerm term = schedule.getSection().getScheduleTerm();
                                term.setFieldValueByBeanPath(m_fieldScheduleTermCode.getJavaName(), "X");
                                getModelBroker().saveBeanForced(term);

                            }
                        }
                    }
                }));
        addFixesByRuleNumber("17", ruleWithFixes);
    }

    /**
     * Initialize rule 23 fix.
     */
    private void initializeRule23Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Certification Status", "^(A|B|H|I|O|M|S|N|V|P)$",
                        "Certification/Licensure/Qualification Status code must be A, B, H, I, O, M, S, N, V or P."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<ScheduleTeacher> schedules = getSchedules((SisStaff) bean);
                        if (schedules != null && !schedules.isEmpty()) {
                            for (ScheduleTeacher schedule : schedules) {
                                schedule.setFieldValueByBeanPath(m_fieldCertificationStatus.getJavaName(), "A");
                                getModelBroker().saveBeanForced(schedule);
                            }
                        }
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
                new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"))
                                .testThen(Restriction.pattern("Classroom Id Number",
                                        "^(\\d{5}\\w\\d{10}(\\d|\\w){5})|(\\d{5}O\\d{10}.{5})$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[14]$"))
                                .testThen(Restriction.pattern("Classroom Id Number",
                                        "^(.{0,20}[\\sZ0].{0,20})$"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff stf = (SisStaff) bean;
                        List<ScheduleTeacher> schedules = getSchedules(stf);
                        if (schedules != null && !schedules.isEmpty()) {
                            for (ScheduleTeacher schedule : schedules) {
                                MasterSchedule masterSch = schedule.getSection();
                                SchoolRoom room = masterSch.getPrimaryRoom();
                                if (room == null) {
                                    masterSch.setPrimaryRoomOid(getNewSchoolRoomOid(stf));
                                    getModelBroker().saveBeanForced(masterSch);

                                } else {
                                    room.setFieldValueByBeanPath(m_fieldClassroomIdentificationNo.getJavaName(),
                                            "00256A0123400456ABCDE");
                                    getModelBroker().saveBeanForced(room);
                                }
                            }
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
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"))
                        .testThen(Restriction.pattern("Classroom Id Number",
                                "^(.{5}[A|B|C|F|O|S].{15})$")),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff stf = (SisStaff) bean;
                        List<ScheduleTeacher> schedules = getSchedules(stf);
                        if (schedules != null && !schedules.isEmpty()) {
                            for (ScheduleTeacher schedule : schedules) {
                                MasterSchedule masterSch = schedule.getSection();
                                SchoolRoom room = masterSch.getPrimaryRoom();
                                if (room == null) {
                                    masterSch.setPrimaryRoomOid(getNewSchoolRoomOid(stf));
                                    getModelBroker().saveBeanForced(masterSch);

                                } else {
                                    room.setFieldValueByBeanPath(m_fieldClassroomIdentificationNo.getJavaName(),
                                            "00256A0123400456ABCDE");
                                    getModelBroker().saveBeanForced(room);
                                }
                            }
                        }

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
                new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"))
                                .testThen(Restriction.pattern("Scheduling Method",
                                        "^(A|B|C|G|I|M|S|W)$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[14]$"))
                                .testThen(Restriction.equals("Scheduling Method", "Z"))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        String schedulingMethod = "Z";
                        String surveyPeriodCode = getFLReportData().getSurveyPeriodCode();
                        if (surveyPeriodCode.contentEquals("2") || surveyPeriodCode.contentEquals("3")) {
                            schedulingMethod = "B";
                        }
                        List<ScheduleTeacher> schedules = getSchedules((SisStaff) bean);
                        if (schedules != null && !schedules.isEmpty()) {
                            for (ScheduleTeacher schedule : schedules) {
                                schedule.setFieldValueByBeanPath(m_fieldSchedulingMethod.getJavaName(),
                                        schedulingMethod);
                                getModelBroker().saveBeanForced(schedule);
                            }
                        }
                    }
                }));
        addFixesByRuleNumber("28", ruleWithFixes);
    }

    /**
     * Initialize rule 63 fix.
     */
    private void initializeRule63Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Certification Number", "^9999999999|000\\d{7}$",
                        "Florida Educators Certificate Number must be 0000000000 or in the range 0000000001 through 0000999998, "
                                + "0001000000 through 0009999999, 0000999999 or 9999999999."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStaff stf = (SisStaff) bean;
                        Collection<StaffCertification> certifications;
                        try {
                            certifications = getFLReportData()
                                    .getStaffHelper().getStaffCertifications(stf);
                            if (certifications != null && !certifications.isEmpty()) {
                                for (StaffCertification item : certifications) {
                                    item.setCertificationNumber("9999999999");
                                    getModelBroker().saveBeanForced(item);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }
                }));
        addFixesByRuleNumber("63", ruleWithFixes);
    }


    /**
     * Initialize rule 67 fix.
     */
    private void initializeRule67Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                        Restriction.equals("Scheduling Method", "A"))
                        .testThen(Restriction.and(
                                Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Section Number", "Section Number"),
                                        new KeyValuePair("Scheduling Method", "Scheduling Method")),
                                Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Scheduling Method", "Scheduling Method")))),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        List<ScheduleTeacher> schedules = getSchedules((SisStaff) bean);
                        if (schedules != null && !schedules.isEmpty()) {
                            for (ScheduleTeacher schedule : schedules) {
                                schedule.setFieldValueByBeanPath(m_fieldSchedulingMethod.getJavaName(), "B");
                                getModelBroker().saveBeanForced(schedule);
                            }
                        }
                    }
                }));
        addFixesByRuleNumber("67", ruleWithFixes);
    }

}
