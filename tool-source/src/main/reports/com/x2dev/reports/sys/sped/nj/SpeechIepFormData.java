/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.nj;

import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepGoalObjective;
import com.x2dev.sis.model.beans.IepMeeting.TypeCode;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class is used by the State of New Jersey for printing the Speech-IEP Reports.
 * 
 * @author Follett Software Company
 *
 */
public class SpeechIepFormData extends BaseFormReportJavaSource {

    private IepData m_currentIep = null;

    /**
     * The variables below are used for setting the sub report streams.
     */
    private Report m_reportAssessment = null;
    private Report m_reportGoals = null;
    private Report m_reportObjectives = null;
    private Report m_reportPresentLevels = null;
    private Report m_reportRights = null;
    private Report m_reportServices = null;

    /**
     * The variables below are used for setting the grids of each sub report.
     */
    private ReportDataGrid m_assessmentGrid = null;
    private ReportDataGrid m_goalsGrid = null;
    private ReportDataGrid m_objectivesGrid = null;
    private ReportDataGrid m_presentLevelsGrid = null;
    private ReportDataGrid m_rightsGrid = null;
    private ReportDataGrid m_servicesGrid = null;

    /**
     * The variables below are the alias variables that are used in the printed report.
     */
    private static final String ALIAS_ACCOMMODATION_MODIFICATION = "iep-accomodation-modification";
    private static final String ALIAS_ASSESSMENT_TYPE = "iep-assess-assess-procedu";
    private static final String ALIAS_DEVELOPMENT_SRC = "iep-development-src";
    private static final String ALIAS_DISTRICT_ASSESSMENT = "iep-assess-dist-assess";
    private static final String ALIAS_DISTRICT_ASSESSMENT_MODIFICATION = "iep-assess-modif-accomo";
    private static final String ALIAS_EDUCATIONAL_NEEDS = "iep-education-needs";
    private static final String ALIAS_MODIFICATIONS = "iep-state-modi-acc";
    private static final String ALIAS_OBJECTIVE_BENCHMARK = "iep-acad-short-term-obj";
    private static final String ALIAS_OBJECTIVE_CRITERIA = "iep-acad-obj-criteria";
    private static final String ALIAS_OBJECTIVE_EVAL_PROC = "iep-acad-obj-eval-proc";
    private static final String ALIAS_PARENT_NOTIFIED = "iep-transfer-notif-par";
    private static final String ALIAS_PARENT_NOTIFIED_DATE = "iep-transfer-par-date";
    private static final String ALIAS_PRESENT_LEVEL = "iep-present-level";
    private static final String ALIAS_REL_SERVICE_GROUP_SIZE = "iep-nj-svc-grp-size";
    private static final String ALIAS_SPECIAL_FACTORS = "iep-sped-special-factors";
    private static final String ALIAS_STATE_COURSE1 = "iep-state-course1";
    private static final String ALIAS_STATE_COURSE2 = "iep-state-course2";
    private static final String ALIAS_STATE_COURSE3 = "iep-state-course3";
    private static final String ALIAS_STUDENT_CONTACT_NAME = "iep-transfer-par-name";
    private static final String ALIAS_STUDENT_NOTIFIED = "iep-transfer-notif-std";
    private static final String ALIAS_STUDENT_NOTIFIED_DATE = "iep-transfer-std-date";
    private static final String ALIAS_TRANSFER_OF_RIGHTS_OPTION = "iep-transfer-option-type";

    /**
     * The variables below are used for setting the data source fields of the printed report..
     */
    private static final String DATASOURCE_ASSESSMENT = "DATASOURCE_ASSESSMENT";
    private static final String DATASOURCE_GOALS = "DATASOURCE_GOALS";
    private static final String DATASOURCE_PRESENT_LEVELS = "DATASOURCE_PRESENT_LEVELS";
    private static final String DATASOURCE_RIGHTS = "DATASOURCE_RIGHTS";
    private static final String DATASOURCE_SERVICES = "DATASOURCE_SERVICES";

    private static final String DASHED_STRING = "__________________________________";
    private static final String EMPTY_STRING = "";

    /**
     * The variables below are used for setting the fields in the sub reports.
     */
    private static final String FIELD_ACCOMMODATION_MODIFICATION = "accommodationModification";
    private static final String FIELD_DIST_ASSESSMENT_MODIFICATIONS = "distAssessmentModif";
    private static final String FIELD_STATE_ASSESSMENT_MODIFICATIONS = "stateAssessmentModif";
    private static final String FIELD_ASSESSMENT_TYPE = "assessmentType";
    private static final String FIELD_DATA_GRID_GOAL_OBJECTIVES = "goalObjectives";
    private static final String FIELD_DEVELOPMENT_SRC = "developmentSrc";
    private static final String FIELD_DISTRICT_ASSESSMENT = "distAssessment";
    private static final String FIELD_EDUCATIONAL_NEEDS = "educationalNeeds";
    private static final String FIELD_GOAL_DESCRIPTION = "goalDescription";
    private static final String FIELD_GOAL_FOCUS = "goalFocus";
    private static final String FIELD_GOAL_ID = "goalId";
    private static final String FIELD_GOAL_OID = "goalOid";
    private static final String FIELD_HSPA = "hspa";
    private static final String FIELD_HSPA_SRA_ARTS = "hspaSraArts";
    private static final String FIELD_HSPA_SRA_MATHS = "hspaSraMaths";
    private static final String FIELD_HSPA_SRA_SCIENCE = "hspaSraScience";
    private static final String FIELD_OBJECTIVE_BENCHMARK = "shortTermBenchMark";
    private static final String FIELD_OBJECTIVE_CRITERIA = "criteria";
    private static final String FIELD_OBJECTIVE_EVALUATION_PROC = "evalProc";
    private static final String FIELD_OPTION_SELECTED = "optionSelected";
    private static final String FIELD_OPTION1_LINE1 = "option1Line1";
    private static final String FIELD_OPTION1_LINE2 = "option1Line2";
    private static final String FIELD_OPTION1_LINE3 = "option1Line3";
    private static final String FIELD_OPTION1_LINE4 = "option1Line4";
    private static final String FIELD_OPTION1_LINE5 = "option1Line5";
    private static final String FIELD_OPTION1_LINE6 = "option1Line6";
    private static final String FIELD_OPTION2_LINE1 = "option2Line1";
    private static final String FIELD_OPTION2_LINE2 = "option2Line2";
    private static final String FIELD_PARENT_NOTIFIED = "parentNotified";
    private static final String FIELD_PRESENT_LEVEL = "presentLevel";
    // Has the start and end date in the format: Start Date + " - " End Date
    private static final String FIELD_SERVICE_DATE = "serviceDate";
    private static final String FIELD_SERVICE_DURATION = "duration";
    private static final String FIELD_SERVICE_FREQUENCY = "frequency";
    private static final String FIELD_SERVICE_LOCATION = "location";
    private static final String FIELD_SERVICE_OID = "svcOid";
    private static final String FIELD_SERVICE_TYPE = "serviceType";
    private static final String FIELD_SPECIAL_FACTORS = "specialFactors";
    private static final String FIELD_SRA = "sra";
    private static final String FIELD_STATE_GRADE3_ARTS = "stateGrade3Arts";
    private static final String FIELD_STATE_GRADE3_MATHS = "stateGrade3Maths";
    private static final String FIELD_STATE_GRADE4_ARTS = "stateGrade4Arts";
    private static final String FIELD_STATE_GRADE4_MATHS = "stateGrade4Maths";
    private static final String FIELD_STATE_GRADE4_SCIENCE = "stateGrade4Science";
    private static final String FIELD_STATE_GRADE5_ARTS = "stateGrade5Arts";
    private static final String FIELD_STATE_GRADE5_MATHS = "stateGrade5Maths";
    private static final String FIELD_STATE_GRADE6_ARTS = "stateGrade6Arts";
    private static final String FIELD_STATE_GRADE6_MATHS = "stateGrade6Maths";
    private static final String FIELD_STATE_GRADE7_ARTS = "stateGrade7Arts";
    private static final String FIELD_STATE_GRADE7_MATHS = "stateGrade7Maths";
    private static final String FIELD_STATE_GRADE8_ARTS = "stateGrade8Arts";
    private static final String FIELD_STATE_GRADE8_MATHS = "stateGrade8Maths";
    private static final String FIELD_STATE_GRADE8_SCIENCE = "stateGrade8Science";
    private static final String FIELD_STUDENT_NOTIFIED = "studentNotified";

    /**
     * The variables below are used for setting the parameters of the printed report.
     */
    private static final String PARAM_CASE_MANAGER_NAME = "CASE_MANAGER_NAME";
    private static final String PARAM_CONTACT_NAME = "CONTACT_NAME";
    private static final String PARAM_COUNSELOR_NAME = "COUNSELOR_NAME";
    private static final String PARAM_DISTRICT_REP_NAME = "DISTRICT_REP_NAME";
    private static final String PARAM_GENERAL_ED_TEACHER_NAME = "GENERAL_ED_TEACHER_NAME";
    private static final String PARAM_LDT_NAME = "LDT_NAME";
    private static final String PARAM_MEETING_TYPE = "MEETING_TYPE";
    private static final String PARAM_PARENT1_NAME = "PARENT1_NAME";
    private static final String PARAM_PARENT2_NAME = "PARENT2_NAME";
    private static final String PARAM_PSYCHOLOGIST_NAME = "PSYCHOLOGIST_NAME";
    private static final String PARAM_SERVICE_NAME_SPEECH = "Speech";
    private static final String PARAM_SPECIALIST_NAME = "SPECIALIST_NAME";
    private static final String PARAM_SPED_TEACHER_NAME = "SPED_TEACHER_NAME";
    private static final String PARAM_SOCIAL_WORKER_NAME = "SOCIAL_WORKER_NAME";
    private static final String PARAM_STATUS_CODE = "STATUS_CODE";
    private static final String PARAM_STUDENT_NAME = "STUDENT_NAME";
    private static final String PARAM_SUBREPORT_ASSESSMENT = "SUB_REPORT_ASSESSMENT";
    private static final String PARAM_SUBREPORT_GOALS = "SUB_REPORT_GOALS";
    private static final String PARAM_SUBREPORT_OBJECTIVES = "SUB_REPORT_OBJECTIVES";
    private static final String PARAM_SUBREPORT_PRESENT_LEVELS = "SUB_REPORT_PRESENT_LEVELS";
    private static final String PARAM_SUBREPORT_RIGHTS = "SUB_REPORT_RIGHTS";
    private static final String PARAM_SUBREPORT_SERVICES = "SUB_REPORT_SERVICES";

    private static final String REFCODE_COUNSELOR = "Counselor";
    private static final String REFCODE_DISTRICT_REP = "Dist Representative";
    private static final String REFCODE_GENERAL_ED_TEACHER = "General Ed Teacher";
    private static final String REFCODE_GRADE3 = "Grade 3";
    private static final String REFCODE_GRADE4 = "Grade 4";
    private static final String REFCODE_GRADE5 = "Grade 5";
    private static final String REFCODE_GRADE6 = "Grade 6";
    private static final String REFCODE_GRADE7 = "Grade 7";
    private static final String REFCODE_GRADE8 = "Grade 8";
    private static final String REFCODE_HSPA = "HSPA";
    private static final String REFCODE_LDT = "LDT-C";
    private static final String REFCODE_PARENT = "Parent/Guardian";
    private static final String REFCODE_PSYCHOLOGIST = "School Psychologist";
    private static final String REFCODE_SPECIALIST = "Specialist";
    private static final String REFCODE_SPED_TEACHER = "SpedTeacher/Provider";
    private static final String REFCODE_SOCIAL_WORKER = "School Social Worker";
    private static final String REFCODE_SRA = "SRA";

    /**
     * The variables below represent the report ids of the sub reports.
     */
    private static final String REPORT_ID_ASSESSMENTS = "SYS-SPED-NJ-SIEP5";
    private static final String REPORT_ID_GOALS_OBJECTIVES = "SYS-SPED-NJ-SIEP3";
    private static final String REPORT_ID_OBJECTIVES = "SYS-SPED-NJ-SIEP4";
    private static final String REPORT_ID_PRESENT_LEVELS = "SYS-SPED-NJ-SIEP1";
    private static final String REPORT_ID_RIGHTS = "SYS-SPED-NJ-SIEP2";
    private static final String REPORT_ID_SERVICES = "SYS-SPED-NJ-SIEP6";

    private static final String TRANSFER_OF_RIGHTS_OPTION2 = "Option 2";


    /**
     * Prepares the data source that will be used by the Jasper design. This method is called after
     * <code>initialize(UserDataContainer)</code> and before <code>releaseResources()</code>.
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        loadAssessments();
        loadGoalsAndObjectives();
        loadPresentLevels();
        loadRights();
        loadSpedRelServices();
        setCoverPageNames();
        setCoverMeetingTypeAndStatusCode();

        SimpleFormDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        return dataSource;
    }

    /**
     * This method sets the meeting type and status code on the cover page of the printed IEP.
     */
    private void setCoverMeetingTypeAndStatusCode() {
        TypeCode typeCode = m_currentIep.getMeetingTypeCodeEnum();
        String meetingTypeCode = "";
        if (typeCode != null) {
            meetingTypeCode = typeCode.name();
        }
        addParameter(PARAM_MEETING_TYPE, meetingTypeCode);
        StatusCode statusCode = m_currentIep.getStatusCodeEnum();
        String status = "";
        if (statusCode != null) {
            status = statusCode.name();
        }
        addParameter(PARAM_STATUS_CODE, status);

    }

    /**
     * This method sets parameters for the names of all people on the cover page
     * of the IEP report form.
     */
    private void setCoverPageNames() {
        String primaryContactName = "";
        String studentName = "";
        String caseManagerName = "";

        SisStudent student = m_currentIep.getStudent();
        if (student != null) {
            studentName = student.getPerson().getFirstName() + " " + student.getPerson().getLastName();
            StudentContact primaryContact = student.getPrimaryContact();
            if (primaryContact != null) {
                primaryContactName =
                        primaryContact.getPerson().getFirstName() + " " + primaryContact.getPerson().getLastName();
            }
        }

        SisStaff caseManager = m_currentIep.getStaff();
        if (caseManager != null) {
            caseManagerName = caseManager.getPerson().getFirstName() + " " + caseManager.getPerson().getLastName();
        }

        setEmptyNamesOnCoverPage();

        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();
        IepTeamMember parent = null;
        for (IepTeamMember member : teamMembers) {
            String role = member.getMemberRoleCode();
            Person person = member.getPerson();
            String name = "";
            if (person != null) {
                name = person.getFirstName() + " " + person.getLastName();
            }
            if (role != null && REFCODE_COUNSELOR.equalsIgnoreCase(role)) {
                addParameter(PARAM_COUNSELOR_NAME, name);
            } else if (role != null && REFCODE_DISTRICT_REP.equalsIgnoreCase(role)) {
                addParameter(PARAM_DISTRICT_REP_NAME, name);
            } else if (role != null && REFCODE_GENERAL_ED_TEACHER.equalsIgnoreCase(role)) {
                addParameter(PARAM_GENERAL_ED_TEACHER_NAME, name);
            } else if (role != null && REFCODE_LDT.equalsIgnoreCase(role)) {
                addParameter(PARAM_LDT_NAME, name);
            } else if (role != null && REFCODE_PARENT.equalsIgnoreCase(role)) {
                parent = member;
                Person secondaryContact = parent.getPerson();
                if (secondaryContact != null) {
                    addParameter(PARAM_PARENT1_NAME, primaryContactName);
                    String secondaryContactName =
                            secondaryContact.getFirstName() + " " + secondaryContact.getLastName();
                    if (secondaryContactName != null && primaryContactName != null
                            && !secondaryContactName.equalsIgnoreCase(primaryContactName)) {
                        addParameter(PARAM_PARENT2_NAME, secondaryContactName);
                    }
                }
            } else if (role != null && REFCODE_PSYCHOLOGIST.equalsIgnoreCase(role)) {
                addParameter(PARAM_PSYCHOLOGIST_NAME, name);
            } else if (role != null && REFCODE_SOCIAL_WORKER.equalsIgnoreCase(role)) {
                addParameter(PARAM_SOCIAL_WORKER_NAME, name);
            } else if (role != null && REFCODE_SPECIALIST.equalsIgnoreCase(role)) {
                addParameter(PARAM_SPECIALIST_NAME, name);
            } else if (role != null && REFCODE_SPED_TEACHER.equalsIgnoreCase(role)) {
                addParameter(PARAM_SPED_TEACHER_NAME, name);
            }

        }
        addParameter(PARAM_STUDENT_NAME, studentName);
        addParameter(PARAM_CONTACT_NAME, primaryContactName);
        addParameter(PARAM_CASE_MANAGER_NAME, caseManagerName);

    }

    /**
     * This method initializes names on the cover page of the printed IEP report.
     */
    private void setEmptyNamesOnCoverPage() {
        addParameter(PARAM_CASE_MANAGER_NAME, EMPTY_STRING);
        addParameter(PARAM_CONTACT_NAME, EMPTY_STRING);
        addParameter(PARAM_COUNSELOR_NAME, EMPTY_STRING);
        addParameter(PARAM_DISTRICT_REP_NAME, EMPTY_STRING);
        addParameter(PARAM_GENERAL_ED_TEACHER_NAME, EMPTY_STRING);
        addParameter(PARAM_LDT_NAME, EMPTY_STRING);
        addParameter(PARAM_PARENT1_NAME, EMPTY_STRING);
        addParameter(PARAM_PARENT2_NAME, EMPTY_STRING);
        addParameter(PARAM_PSYCHOLOGIST_NAME, EMPTY_STRING);
        addParameter(PARAM_SOCIAL_WORKER_NAME, EMPTY_STRING);
        addParameter(PARAM_SPECIALIST_NAME, EMPTY_STRING);
        addParameter(PARAM_SPED_TEACHER_NAME, EMPTY_STRING);
        addParameter(PARAM_STUDENT_NAME, EMPTY_STRING);
    }

    /**
     * This method loads the present levels sub report.
     */
    private void loadPresentLevels() {
        getPresentLevelsSubRptCriteria();
        getPresentLevelsCriteria();
    }

    /**
     * This method sets the input stream for the present levels sub report.
     *
     * @return void
     */
    private void getPresentLevelsSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_PRESENT_LEVELS);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportPresentLevels = (Report) getBroker().getBeanByQuery(query);
        addParameter(PARAM_SUBREPORT_PRESENT_LEVELS,
                new ByteArrayInputStream(m_reportPresentLevels.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the present levels sub report.
     *
     * @return void
     */
    private void getPresentLevelsCriteria() {
        String developmentSrc = null;
        String presentLevel = null;
        String accommodationModification = null;
        String educationalNeeds = null;
        String specialFactors = null;

        developmentSrc = (String) m_currentIep.getFieldValueByAlias(ALIAS_DEVELOPMENT_SRC, getDictionary());
        presentLevel = (String) m_currentIep.getFieldValueByAlias(ALIAS_PRESENT_LEVEL, getDictionary());
        accommodationModification =
                (String) m_currentIep.getFieldValueByAlias(ALIAS_ACCOMMODATION_MODIFICATION, getDictionary());
        educationalNeeds = (String) m_currentIep.getFieldValueByAlias(ALIAS_EDUCATIONAL_NEEDS, getDictionary());
        specialFactors = (String) m_currentIep.getFieldValueByAlias(ALIAS_SPECIAL_FACTORS, getDictionary());

        m_presentLevelsGrid = new ReportDataGrid();
        m_presentLevelsGrid.append();
        m_presentLevelsGrid.set(FIELD_DEVELOPMENT_SRC, developmentSrc);
        m_presentLevelsGrid.set(FIELD_PRESENT_LEVEL, presentLevel);
        m_presentLevelsGrid.set(FIELD_ACCOMMODATION_MODIFICATION, accommodationModification);
        m_presentLevelsGrid.set(FIELD_EDUCATIONAL_NEEDS, educationalNeeds);
        m_presentLevelsGrid.set(FIELD_SPECIAL_FACTORS, specialFactors);
        m_presentLevelsGrid.beforeTop();

        addParameter(DATASOURCE_PRESENT_LEVELS, m_presentLevelsGrid);
    }

    /**
     * This method loads the rights sub report.
     */
    private void loadRights() {
        getRightsRptCriteria();
        getRightsCriteria();
    }

    /**
     * This method sets the input stream for the rights sub report.
     *
     * @return void
     */
    private void getRightsRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_RIGHTS);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportRights = (Report) getBroker().getBeanByQuery(query);
        addParameter(PARAM_SUBREPORT_RIGHTS, new ByteArrayInputStream(m_reportRights.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the rights sub report.
     *
     * @return void
     */
    private void getRightsCriteria() {
        String option1Line1 = null;
        String option1Line2 = null;
        String option1Line3 = null;
        String option1Line4 = null;
        String option1Line5 = null;
        String option1Line6 = null;
        String option2Line1 = null;
        String option2Line2 = null;


        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yy");
        SimpleDateFormat inputFormat = new SimpleDateFormat("yyyy-MM-dd");
        SisStudent student = m_currentIep.getStudent();
        String stdFullName = new StringBuilder(student.getPerson().getFirstName()).append(" ")
                .append(student.getPerson().getLastName()).toString();
        PlainDate dob = student.getPerson().getDob();
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(dob);
        calendar.add(Calendar.YEAR, 18);
        String dobPlus18Yrs = dateFormat.format(calendar.getTime());

        option1Line1 = new StringBuilder("On ")
                .append(dobPlus18Yrs)
                .append(", ")
                .append(stdFullName)
                .append("  will turn age 18 and become an adult student. " +
                        "The following rights will transfer to ")
                .append(stdFullName)
                .append(":").toString();

        option1Line2 = new StringBuilder(">> The school district must receive written permission from ")
                .append(stdFullName)
                .append(" before it conducts any assessments as part of an evaluation " +
                        "or reevaluation and before implementing an IEP for the first " +
                        "time.")
                .toString();

        option1Line3 = new StringBuilder(">> The school must send a written notice to ")
                .append(stdFullName)
                .append(" whenever it wishes to change or refuses to change the " +
                        "evaluation, eligibility, individualized education program (IEP), " +
                        "placement, or the provision of a free appropriate public  " +
                        "education (FAPE).")
                .toString();

        option1Line4 = new StringBuilder(">> You, the parent(s) may not have access to ")
                .append(stdFullName)
                .append("\'s educational records without his/her consent, unless he/she continues to be financially " +
                        "dependent on you.")
                .toString();

        option1Line5 = new StringBuilder(">> Any time ")
                .append(stdFullName)
                .append(" disagrees with his/her special education program, he/she is the only " +
                        "one who can request mediation or a due process hearing to resolve any " +
                        "disputes arising in those areas.")
                .toString();

        option1Line6 = new StringBuilder("If ")
                .append(stdFullName)
                .append(" wishes, he/she may write a letter to the school giving you, the " +
                        "parent(s), the right to continue to act on his/her behalf in these " +
                        "matters.")
                .toString();

        String studentNotified = "";
        String parentNotified = "";
        String studentNotificationDate = DASHED_STRING;
        String parentNotificationDate = DASHED_STRING;
        String contactName = (String) m_currentIep.getFieldValueByAlias(ALIAS_STUDENT_CONTACT_NAME, getDictionary());
        String optionSelected =
                (String) m_currentIep.getFieldValueByAlias(ALIAS_TRANSFER_OF_RIGHTS_OPTION, getDictionary());

        if (TRANSFER_OF_RIGHTS_OPTION2.equalsIgnoreCase(optionSelected)) {
            studentNotified = (String) m_currentIep.getFieldValueByAlias(ALIAS_STUDENT_NOTIFIED, getDictionary());
            parentNotified = (String) m_currentIep.getFieldValueByAlias(ALIAS_PARENT_NOTIFIED, getDictionary());
            try {
                String notificationDate =
                        (String) m_currentIep.getFieldValueByAlias(ALIAS_STUDENT_NOTIFIED_DATE, getDictionary());
                Date tempDate = null;
                if (notificationDate != null) {
                    tempDate = inputFormat.parse(notificationDate);
                    studentNotificationDate = dateFormat.format(tempDate);
                }

                notificationDate =
                        (String) m_currentIep.getFieldValueByAlias(ALIAS_PARENT_NOTIFIED_DATE, getDictionary());
                tempDate = null;
                if (notificationDate != null) {
                    tempDate = inputFormat.parse(notificationDate);
                    parentNotificationDate = dateFormat.format(tempDate);
                } else {
                    parentNotificationDate = DASHED_STRING;
                }
            } catch (ParseException e) {
                AppGlobals.getLog().log(Level.SEVERE, e.getLocalizedMessage(), e);
            }
        } else {
            studentNotified = "";
            parentNotified = "";
            contactName = DASHED_STRING;
            studentNotificationDate = DASHED_STRING;
            parentNotificationDate = DASHED_STRING;
        }

        option2Line1 = new StringBuilder().append(stdFullName).append(" was informed in writing on ")
                .append(studentNotificationDate)
                .append(" of the rights that will transfer to him/her at age eighteen.").toString();

        option2Line2 = new StringBuilder().append(contactName).append(" was/were informed in writing on ")
                .append(parentNotificationDate).append(" of the rights that will transfer at age eighteen.").toString();

        m_rightsGrid = new ReportDataGrid();
        m_rightsGrid.append();
        m_rightsGrid.set(FIELD_OPTION1_LINE1, option1Line1);
        m_rightsGrid.set(FIELD_OPTION1_LINE2, option1Line2);
        m_rightsGrid.set(FIELD_OPTION1_LINE3, option1Line3);
        m_rightsGrid.set(FIELD_OPTION1_LINE4, option1Line4);
        m_rightsGrid.set(FIELD_OPTION1_LINE5, option1Line5);
        m_rightsGrid.set(FIELD_OPTION1_LINE6, option1Line6);
        m_rightsGrid.set(FIELD_OPTION2_LINE1, option2Line1);
        m_rightsGrid.set(FIELD_OPTION2_LINE2, option2Line2);
        m_rightsGrid.set(FIELD_STUDENT_NOTIFIED, studentNotified);
        m_rightsGrid.set(FIELD_PARENT_NOTIFIED, parentNotified);
        m_rightsGrid.set(FIELD_OPTION_SELECTED, optionSelected);

        m_rightsGrid.beforeTop();

        addParameter(DATASOURCE_RIGHTS, m_rightsGrid);
    }

    /**
     * This method loads the goals and objectives sub report.
     */
    private void loadGoalsAndObjectives() {
        getGoalsAndObjectivesSubRptCriteria();
        getGoalsAndObjectivesCriteria();
    }

    /**
     * This method sets the input stream for the goals sub report.
     * It then sets the input stream for the objectives sub report.
     *
     * @return void
     */
    private void getGoalsAndObjectivesSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_GOALS_OBJECTIVES);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportGoals = (Report) getBroker().getBeanByQuery(query);
        addParameter(PARAM_SUBREPORT_GOALS, new ByteArrayInputStream(m_reportGoals.getCompiledFormat()));
        getObjectivesSubRptCriteria();
    }

    /**
     * This method sets the criteria for the goals and objectives.
     *
     * @return void
     */
    private void getGoalsAndObjectivesCriteria() {
        Criteria goalsCriteria = new Criteria();
        goalsCriteria.addEqualTo(IepGoal.COL_IEP_DATA_OID, m_currentIep.getOid());
        QueryByCriteria goalsQuery = new QueryByCriteria(IepGoal.class, goalsCriteria);
        goalsQuery.addOrderByAscending(IepGoal.COL_ID); // Goal number
        goalsQuery.addOrderByAscending(IepGoal.COL_FOCUS);
        Collection<IepGoal> goals = getBroker().getCollectionByQuery(goalsQuery);

        m_goalsGrid = new ReportDataGrid();
        if (goals.isEmpty()) {
            goals.add(X2BaseBean.newInstance(IepGoal.class, getBroker().getPersistenceKey()));
        }
        for (IepGoal goal : goals) {
            String goalId = goal.getId();
            String goalDescription = goal.getGoal();

            Collection<IepGoalObjective> objectives = goal.getIepGoalObjectives(getBroker());
            m_objectivesGrid = new ReportDataGrid();
            for (IepGoalObjective objective : objectives) {
                String shortTermBenchmark =
                        (String) objective.getFieldValueByAlias(ALIAS_OBJECTIVE_BENCHMARK, getDictionary());
                String criteria = (String) objective.getFieldValueByAlias(ALIAS_OBJECTIVE_CRITERIA, getDictionary());
                String evalProc = (String) objective.getFieldValueByAlias(ALIAS_OBJECTIVE_EVAL_PROC, getDictionary());

                m_objectivesGrid.append();
                m_objectivesGrid.set(FIELD_OBJECTIVE_BENCHMARK, shortTermBenchmark);
                m_objectivesGrid.set(FIELD_OBJECTIVE_CRITERIA, criteria);
                m_objectivesGrid.set(FIELD_OBJECTIVE_EVALUATION_PROC, evalProc);
            }
            if (objectives.size() < 4) {
                for (int i = 0; i < (4 - objectives.size()); i++) {
                    m_objectivesGrid.append();
                    m_objectivesGrid.set(FIELD_OBJECTIVE_BENCHMARK, EMPTY_STRING);
                    m_objectivesGrid.set(FIELD_OBJECTIVE_CRITERIA, EMPTY_STRING);
                    m_objectivesGrid.set(FIELD_OBJECTIVE_EVALUATION_PROC, EMPTY_STRING);
                }
            }
            m_objectivesGrid.beforeTop();

            m_goalsGrid.append();
            m_goalsGrid.set(FIELD_GOAL_ID, goalId);
            m_goalsGrid.set(FIELD_GOAL_OID, goal.getOid());
            m_goalsGrid.set(FIELD_GOAL_FOCUS, goal.getFocus());
            m_goalsGrid.set(FIELD_GOAL_DESCRIPTION, goalDescription);
            m_goalsGrid.set(FIELD_DATA_GRID_GOAL_OBJECTIVES, m_objectivesGrid);
        }
        m_goalsGrid.beforeTop();
        addParameter(DATASOURCE_GOALS, m_goalsGrid);
    }

    /**
     * This method is called when the goals sub report is loaded.
     * This method sets the objectives input stream.
     *
     * @return void
     */
    private void getObjectivesSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_OBJECTIVES);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportObjectives = (Report) getBroker().getBeanByQuery(query);
        addParameter(PARAM_SUBREPORT_OBJECTIVES, new ByteArrayInputStream(m_reportObjectives.getCompiledFormat()));
    }

    /**
     * This method loads the sped and related services sub report.
     */
    private void loadSpedRelServices() {
        getSvcsSubRptCriteria();
        getSvcsCriteria();
    }

    /**
     * This method sets the input stream for the services sub report.
     *
     * @return void
     */
    private void getSvcsSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_SERVICES);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportServices = (Report) getBroker().getBeanByQuery(query);
        addParameter(PARAM_SUBREPORT_SERVICES, new ByteArrayInputStream(m_reportServices.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the services.
     *
     * @return void
     */
    private void getSvcsCriteria() {
        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yy");
        m_servicesGrid = new ReportDataGrid();
        // Related Services service mode
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepService.COL_IEP_DATA_OID, m_currentIep.getOid());
        criteria.addEqualTo(IepService.COL_SERVICE_MODE, PARAM_SERVICE_NAME_SPEECH);
        QueryByCriteria servicesQuery = new QueryByCriteria(IepService.class, criteria);
        servicesQuery.addOrderByAscending(IepService.COL_SERVICE_TYPE);
        Collection<IepService> relatedServices = getBroker().getCollectionByQuery(servicesQuery);
        if (relatedServices.isEmpty()) {
            relatedServices.add(X2BaseBean.newInstance(IepService.class, getBroker().getPersistenceKey()));
        }
        for (IepService service : relatedServices) {
            m_servicesGrid.append();
            String groupSize = (String) service.getFieldValueByAlias(ALIAS_REL_SERVICE_GROUP_SIZE, getDictionary());
            String serviceType = service.getServiceType();
            if (groupSize != null && !"".equals(groupSize.trim())) {
                serviceType = serviceType + " (Size:" + groupSize + ")";
            }
            m_servicesGrid.set(FIELD_SERVICE_TYPE, serviceType);
            m_servicesGrid.set(FIELD_SERVICE_OID, service.getOid());
            if (service.getStartDate() != null && service.getEndDate() != null) {
                m_servicesGrid.set(FIELD_SERVICE_DATE, dateFormat.format(service.getStartDate())
                        + " - " + dateFormat.format(service.getEndDate()));
                if (service.getCycle() != null) {
                    m_servicesGrid.set(FIELD_SERVICE_FREQUENCY,
                            service.getFrequency().toString() + "/" + service.getCycle());
                } else {
                    m_servicesGrid.set(FIELD_SERVICE_FREQUENCY, service.getFrequency().toString());
                }
            } else {
                m_servicesGrid.set(FIELD_SERVICE_DATE, EMPTY_STRING);
                m_servicesGrid.set(FIELD_SERVICE_FREQUENCY, EMPTY_STRING);
            }
            m_servicesGrid.set(FIELD_SERVICE_LOCATION, service.getSettingCode());
            if (service.getStudent() != null) {
                m_servicesGrid.set(FIELD_SERVICE_DURATION, Integer.toString(service.getDuration())); // returns
                                                                                                     // int
            }
        }

        m_servicesGrid.beforeTop();
        addParameter(DATASOURCE_SERVICES, m_servicesGrid);
    }

    /**
     * This method loads the assessment sub report.
     */
    private void loadAssessments() {
        getAssessmentsSubRptCriteria();
        getAssessmentsCriteria();
    }

    /**
     * This method sets the input stream for the assessments sub report.
     *
     * @return void
     */
    private void getAssessmentsSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_ASSESSMENTS);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportAssessment = (Report) getBroker().getBeanByQuery(query);
        addParameter(PARAM_SUBREPORT_ASSESSMENT, new ByteArrayInputStream(m_reportAssessment.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the assessments sub report.
     *
     * @return void
     */
    private void getAssessmentsCriteria() {
        Collection<IepAccommodation> accommodations = m_currentIep.getAccommodations();
        m_assessmentGrid = new ReportDataGrid();
        m_assessmentGrid.append();

        String distAssessment = (String) m_currentIep.getFieldValueByAlias(ALIAS_DISTRICT_ASSESSMENT, getDictionary());
        String distAssessmentModif =
                (String) m_currentIep.getFieldValueByAlias(ALIAS_DISTRICT_ASSESSMENT_MODIFICATION, getDictionary());

        String assessmentType = (String) m_currentIep.getFieldValueByAlias(ALIAS_ASSESSMENT_TYPE, getDictionary());
        m_assessmentGrid.set(FIELD_DISTRICT_ASSESSMENT, distAssessment);
        m_assessmentGrid.set(FIELD_DIST_ASSESSMENT_MODIFICATIONS, distAssessmentModif);
        m_assessmentGrid.set(FIELD_ASSESSMENT_TYPE, assessmentType);


        if (!accommodations.isEmpty()) {
            Iterator<IepAccommodation> iter = accommodations.iterator();
            IepAccommodation accommodation = iter.next();
            String grade = accommodation.getCategory();
            String isStateArts = (String) accommodation.getFieldValueByAlias(ALIAS_STATE_COURSE1, getDictionary());
            String isStateMaths = (String) accommodation.getFieldValueByAlias(ALIAS_STATE_COURSE2, getDictionary());
            String isStateScience = (String) accommodation.getFieldValueByAlias(ALIAS_STATE_COURSE3, getDictionary());
            String modifications = (String) accommodation.getFieldValueByAlias(ALIAS_MODIFICATIONS, getDictionary());

            if (REFCODE_GRADE3.equalsIgnoreCase(grade)) {
                m_assessmentGrid.set(FIELD_STATE_GRADE3_ARTS, isStateArts);
                m_assessmentGrid.set(FIELD_STATE_GRADE3_MATHS, isStateMaths);
            } else if (REFCODE_GRADE4.equalsIgnoreCase(grade)) {
                m_assessmentGrid.set(FIELD_STATE_GRADE4_ARTS, isStateArts);
                m_assessmentGrid.set(FIELD_STATE_GRADE4_MATHS, isStateMaths);
                m_assessmentGrid.set(FIELD_STATE_GRADE4_SCIENCE, isStateScience);
            } else if (REFCODE_GRADE5.equalsIgnoreCase(grade)) {
                m_assessmentGrid.set(FIELD_STATE_GRADE5_ARTS, isStateArts);
                m_assessmentGrid.set(FIELD_STATE_GRADE5_MATHS, isStateMaths);
            } else if (REFCODE_GRADE6.equalsIgnoreCase(grade)) {
                m_assessmentGrid.set(FIELD_STATE_GRADE6_ARTS, isStateArts);
                m_assessmentGrid.set(FIELD_STATE_GRADE6_MATHS, isStateMaths);
            } else if (REFCODE_GRADE7.equalsIgnoreCase(grade)) {
                m_assessmentGrid.set(FIELD_STATE_GRADE7_ARTS, isStateArts);
                m_assessmentGrid.set(FIELD_STATE_GRADE7_MATHS, isStateMaths);
            } else if (REFCODE_GRADE8.equalsIgnoreCase(grade)) {
                m_assessmentGrid.set(FIELD_STATE_GRADE8_ARTS, isStateArts);
                m_assessmentGrid.set(FIELD_STATE_GRADE8_MATHS, isStateMaths);
                m_assessmentGrid.set(FIELD_STATE_GRADE8_SCIENCE, isStateScience);
            } else if (REFCODE_HSPA.equalsIgnoreCase(grade) || REFCODE_SRA.equalsIgnoreCase(grade)) {
                if (REFCODE_HSPA.equalsIgnoreCase(grade)) {
                    m_assessmentGrid.set(FIELD_HSPA, grade);
                    m_assessmentGrid.set(FIELD_SRA, EMPTY_STRING);
                } else {
                    m_assessmentGrid.set(FIELD_HSPA, EMPTY_STRING);
                    m_assessmentGrid.set(FIELD_SRA, grade);
                }
                m_assessmentGrid.set(FIELD_HSPA_SRA_ARTS, isStateArts);
                m_assessmentGrid.set(FIELD_HSPA_SRA_MATHS, isStateMaths);
                m_assessmentGrid.set(FIELD_HSPA_SRA_SCIENCE, isStateScience);
            }
            m_assessmentGrid.set(FIELD_STATE_ASSESSMENT_MODIFICATIONS, modifications);
        } else {
            m_assessmentGrid.set(FIELD_STATE_GRADE3_ARTS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE3_MATHS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE4_ARTS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE4_MATHS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE4_SCIENCE, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE5_ARTS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE5_MATHS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE6_ARTS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE6_MATHS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE7_ARTS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE7_MATHS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE8_ARTS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE8_MATHS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_GRADE8_SCIENCE, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_HSPA, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_SRA, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_HSPA_SRA_ARTS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_HSPA_SRA_MATHS, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_HSPA_SRA_SCIENCE, EMPTY_STRING);
            m_assessmentGrid.set(FIELD_STATE_ASSESSMENT_MODIFICATIONS, EMPTY_STRING);
        }
        m_assessmentGrid.beforeTop();

        addParameter(DATASOURCE_ASSESSMENT, m_assessmentGrid);
    }

    /**
     * The following methods are called, in order, during the life-cycle of a ReportJavaSource
     * instance:
     * <ol>
     * <li><code>saveState(UserDataContainer)</code>
     * <li><code>initialize()</code>
     * <li><code>gatherData()</code>
     * <li><code>releaseResources()</code>
     * </ol>
     *
     * This method is provided as a convenient way for subclasses to initialize member variables
     * before the <code>run()</code> method is called. The default implementation does nothing.
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        } else if (m_currentIep != null) {
            setFormOwner(m_currentIep);
            setFormStorage(m_currentIep);
            setDictionary(DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey()));
            addFormParameters();
        }
    }

    /**
     * This method is provided as a way for subclasses to save session state information. The
     * default implementation does nothing.
     *
     * @param userData UserDataContainer
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);
        m_currentIep = userData.getCurrentRecord(IepData.class);
    }

}
