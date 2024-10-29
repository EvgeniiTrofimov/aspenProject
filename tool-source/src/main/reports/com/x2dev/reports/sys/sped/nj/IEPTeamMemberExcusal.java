/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.nj;

import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepOtherService;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;

/**
 * New Jersey SPED form for Excusing an IEP Team Member.
 * 
 * @author Follett Software Company
 *
 */
public class IEPTeamMemberExcusal extends BaseFormReportJavaSource {
    /**
     * Instances
     */

    private StringBuilder agencyNames = new StringBuilder();
    private StringBuilder attendees = new StringBuilder();
    private IepData m_currentIep = null;
    private DataDictionary m_dictionary = null;
    private StringBuilder excusedTeamMembers = new StringBuilder();
    private ReportDataGrid m_grid;
    private PlainDate m_responseDate = new PlainDate();

    /**
     * Aliases
     */
    private static final String ALIAS_DET_ELIG_FOR_SPED = "sped-det-continuing-eligiblity";
    private static final String ALIAS_DEVELOP_INIT_IEP = "sped-develop-iep";
    private static final String ALIAS_EVALUATIONS_LIST = "sped-attached-evaluations-list";
    private static final String ALIAS_INT_EVAL_AND_DET_ELIG = "sped-interpret-eval-det-elig";
    private static final String ALIAS_INTERAGENCY_NAME = "iep-agency";
    private static final String ALIAS_INTERPRET_ASSESSMENT = "sped-interpret-assessment-data";
    private static final String ALIAS_OTHER = "sped-other-reason";
    private static final String ALIAS_OTHER_CONTACT_PERSON = "sped-other-contact-person";
    private static final String ALIAS_OTHER_DESC = "sped-other-description";
    private static final String ALIAS_OTHER_EVALUATIONS_LIST = "sped-other-evaluation-details";
    private static final String ALIAS_PLAN_FOR_TRANSITION = "sped-plan-transition-adultlife";
    private static final String ALIAS_PLAN_REEVAL = "sped-plan-reevaluation";
    private static final String ALIAS_REVIEW_REVISE_IEP = "sped-review-revise-iep";
    private static final String ALIAS_WRITTEN_INPUT = "sped-written-input-excused-mbr";

    /**
     * Fields
     */
    private static final String FIELD_ADDRESS_LINE1 = "addressLine1";
    private static final String FIELD_ADDRESS_LINE2 = "addressLine2";
    private static final String FIELD_FULL_NAME = "fullName";

    /**
     * Params
     */
    private static final String PARAM_CATEGORY_AGE = "CATEGORY_AGE";
    private static final String PARAM_CATEGORY_AREA = "CATEGORY_AREA";
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_DET_ELIG_FOR_SPED = "DET_ELIG_FOR_SPED";
    private static final String PARAM_DEVELOP_INIT_IEP = "DEVELOP_INIT_IEP";
    private static final String PARAM_EVALUATIONS_LIST = "EVALUATIONS_LIST";
    private static final String PARAM_EXCUSED_TEAM_MBRS = "EXCUSED_TEAM_MBRS";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_INT_EVAL_AND_DET_ELIG = "INT_EVAL_AND_DET_ELIG";
    private static final String PARAM_INTERPRET_ASSESSMENT = "INTERPRET_ASSESSMENT";
    private static final String PARAM_INVITED_TEAM_MBRS = "INVITED_TEAM_MBRS";
    private static final String PARAM_MEETING_DATE = "MEETING_DATE";
    private static final String PARAM_MEETING_LOCATION = "MEETING_LOCATION";
    private static final String PARAM_MEETING_TIME = "MEETING_TIME";
    private static final String PARAM_OTHER = "OTHER";
    private static final String PARAM_OTHER_CONTACT_PERSON = "OTHER_CONTACT_PERSON";
    private static final String PARAM_OTHER_DESC = "OTHER_DESC";
    private static final String PARAM_PARTICIPATING_AGENCIES = "PARTICIPATING_AGENCIES";
    private static final String PARAM_PLAN_FOR_TRANSITION = "PLAN_FOR_TRANSITION";
    private static final String PARAM_PLAN_REEVAL = "PLAN_REEVAL";
    private static final String PARAM_RESPONSE_DATE = "RESPONSE_DATE";
    private static final String PARAM_REVIEW_REVISE_IEP = "REVIEW_REVISE_IEP";
    private static final String PARAM_SERVICE_NAME_INTERAGENCY = "InterAgency";
    private static final String PARAM_STUDENT_NAME = "STUDENT_NAME";
    private static final String PARAM_WRITTEN_INPUT = "WRITTEN_INPUT";

    /**
     * Other Constants
     */
    private static final String AGE_CATEGORY_U18 = "AgeCategoryU18";
    private static final String AGE_CATEGORY_O18 = "AgeCategoryO18";
    private static final String AREA_CATEGORY_DISCUSSED = "Discussed";
    private static final String AREA_CATEGORY_NOT_DISCUSSED = "NotDiscussed";
    private static final String FORM_ID_AREA_DISCUSSED = "EXCUSE-AREA-DISCUSS";
    private static final String FORM_ID_AREA_NOT_DISCUSSED = "EXCUSE-AREA-NDISCUSS";
    private static final String REFCODE_PARENT = "Parent/Guardian";
    private static final String STRING_COMMA = ", ";
    private static final String STRING_EMPTY = "";
    private static final String STRING_OTHER = "Other";

    private final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("MM/dd/yyyy");
    private final SimpleDateFormat TIME_FORMATTER = new SimpleDateFormat("HH:mm");

    /**
     * This method builds the entire report.
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        ExtendedDictionaryAttributes extendDictionary = m_currentIep.getExtendedDataDictionary();
        m_dictionary = DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());
        m_grid = new ReportDataGrid();

        FormInstance formInstance = getFormInstance();
        addParameter(PARAM_FORM_DATE, DATE_FORMATTER.format(new Date()));
        if (formInstance != null) {
            addParameter(PARAM_FORM_DATE, DATE_FORMATTER.format(new Date(formInstance.getCreatedTime())));
        }
        getChairPersonDetails();

        SisStudent student = m_currentIep.getStudent();
        PlainDate dateOfBirth = student.getPerson().getDob();
        IepMeeting meeting = (IepMeeting) getFormStorage();
        PlainDate meetingDate = meeting.getDate();

        int age = 15; // Some default age value which is less than 18 to print the form for parents.
        addParameter(PARAM_MEETING_DATE, "");
        addParameter(PARAM_MEETING_TIME, "");
        addParameter(PARAM_RESPONSE_DATE, "");
        if (meetingDate != null) {
            age = Person.getAgeAsOfDate(dateOfBirth, meetingDate);
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(meetingDate);
            calendar.add(Calendar.DATE, -2);
            m_responseDate = DateUtils.getPriorWeekDay(new PlainDate(calendar.getTime()));
            addParameter(PARAM_MEETING_DATE, DATE_FORMATTER.format(meetingDate));
            addParameter(PARAM_MEETING_TIME, TIME_FORMATTER.format(meeting.getTime()));
            addParameter(PARAM_RESPONSE_DATE, DATE_FORMATTER.format(m_responseDate));
        }

        addParameter(PARAM_CATEGORY_AGE, (age < 18) ? AGE_CATEGORY_U18 : AGE_CATEGORY_O18);
        addParameter(PARAM_INVITED_TEAM_MBRS, getInvitedTeamMembers(meeting));
        addParameter(PARAM_PARTICIPATING_AGENCIES, getAgencyRepresentatives());
        addParameter(PARAM_EXCUSED_TEAM_MBRS, getExcusedTeamMembers(meeting));
        addParameter(PARAM_MEETING_LOCATION, meeting.getLocation());
        addParameter(PARAM_EVALUATIONS_LIST, getEvaluationsList(meeting));
        addParameter(PARAM_STUDENT_NAME, student.getPerson().getFirstName() + " " + student.getPerson().getLastName());
        addParameter(PARAM_INT_EVAL_AND_DET_ELIG,
                meeting.getFieldValueByAlias(ALIAS_INT_EVAL_AND_DET_ELIG, m_dictionary));
        addParameter(PARAM_DEVELOP_INIT_IEP, meeting.getFieldValueByAlias(ALIAS_DEVELOP_INIT_IEP, m_dictionary));
        addParameter(PARAM_REVIEW_REVISE_IEP, meeting.getFieldValueByAlias(ALIAS_REVIEW_REVISE_IEP, m_dictionary));
        addParameter(PARAM_PLAN_FOR_TRANSITION, meeting.getFieldValueByAlias(ALIAS_PLAN_FOR_TRANSITION, m_dictionary));
        addParameter(PARAM_PLAN_REEVAL, meeting.getFieldValueByAlias(ALIAS_PLAN_REEVAL, m_dictionary));
        addParameter(PARAM_INTERPRET_ASSESSMENT,
                meeting.getFieldValueByAlias(ALIAS_INTERPRET_ASSESSMENT, m_dictionary));
        addParameter(PARAM_DET_ELIG_FOR_SPED, meeting.getFieldValueByAlias(ALIAS_DET_ELIG_FOR_SPED, m_dictionary));
        addParameter(PARAM_OTHER_CONTACT_PERSON,
                meeting.getFieldValueByAlias(ALIAS_OTHER_CONTACT_PERSON, m_dictionary));

        Object otherPurpose = meeting.getFieldValueByAlias(ALIAS_OTHER, m_dictionary);
        addParameter(PARAM_OTHER_DESC, "");
        if (otherPurpose != null) {
            addParameter(PARAM_OTHER, otherPurpose);
            Object otherPurposeDesc = meeting.getFieldValueByAlias(ALIAS_OTHER_DESC, m_dictionary);
            if (otherPurposeDesc != null) {
                addParameter(PARAM_OTHER_DESC, otherPurposeDesc);
            }
        }

        String formId = getFormDefinition().getId();
        if (FORM_ID_AREA_DISCUSSED.equals(formId)) {
            addParameter(PARAM_CATEGORY_AREA, AREA_CATEGORY_DISCUSSED);
            addParameter(PARAM_WRITTEN_INPUT, meeting.getFieldValueByAlias(ALIAS_WRITTEN_INPUT, m_dictionary));
        } else if (FORM_ID_AREA_NOT_DISCUSSED.equals(formId)) {
            addParameter(PARAM_CATEGORY_AREA, AREA_CATEGORY_NOT_DISCUSSED);
        }

        if (age < 18) {
            loadUnder18FormForParents();
        } else {
            loadOver18FormForStudent(student);
        }
        if (m_grid.rowCount() == 0) {
            m_grid.append();
        }
        m_grid.beforeTop();
        return m_grid;
    }

    /**
     * This method builds the Under-18 version of the form for Parents.
     */
    private void loadUnder18FormForParents() {
        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();
        for (IepTeamMember teamMember : teamMembers) {
            String title = teamMember.getMemberRoleCode();
            if (title != null && REFCODE_PARENT.equalsIgnoreCase(title)) {
                setReportFields(teamMember.getPerson());
            }
        }
    }

    /**
     * This method builds the Over-18 version of the form for Students.
     *
     * @param student SisStudent
     */
    private void loadOver18FormForStudent(SisStudent student) {
        setReportFields(student.getPerson());
    }

    /**
     * This method sets all the report fields related to a Person.
     *
     * @param person void
     */
    private void setReportFields(Person person) {
        m_grid.append();
        m_grid.set(FIELD_FULL_NAME, person.getFirstName() + " " + person.getLastName());
        m_grid.set(FIELD_ADDRESS_LINE1, STRING_EMPTY);
        m_grid.set(FIELD_ADDRESS_LINE2, STRING_EMPTY);

        Address mailingAddress = person.getMailingAddress();
        if (mailingAddress != null) {
            m_grid.set(FIELD_ADDRESS_LINE1, mailingAddress.getAddressLine01());
            m_grid.set(FIELD_ADDRESS_LINE2, mailingAddress.getAddressLine03());
        }
    }

    /**
     * This method returns the list of all team members invited to a meeting.
     *
     * @param meeting IepMeeting
     * @return String
     */
    private String getInvitedTeamMembers(IepMeeting meeting) {
        Collection<IepMeetingAttendance> meetingAttendances = meeting.getMeetingAttendance();
        for (IepMeetingAttendance meetingAttendance : meetingAttendances) {
            if (meetingAttendance.getExcusedIndicator()) {
                continue;
            }
            IepTeamMember invitedTeamMember = meetingAttendance.getTeamMember();
            Person person = invitedTeamMember.getPerson();
            attendees.append(person.getFirstName() + " " + person.getLastName() + " ("
                    + invitedTeamMember.getMemberRoleCode() + ")\n");
        }
        return attendees.toString();
    }

    /**
     * This method returns the list of all team members excused from a meeting.
     *
     * @param meeting IepMeeting
     * @return String
     */
    private String getExcusedTeamMembers(IepMeeting meeting) {
        Collection<IepMeetingAttendance> meetingAttendances = meeting.getMeetingAttendance();
        for (IepMeetingAttendance meetingAttendance : meetingAttendances) {
            if (meetingAttendance.getExcusedIndicator()) {
                IepTeamMember invitedTeamMember = meetingAttendance.getTeamMember();
                Person person = invitedTeamMember.getPerson();
                excusedTeamMembers.append(person.getFirstName() + " " + person.getLastName() + " ("
                        + invitedTeamMember.getMemberRoleCode() + "), ");
            }
        }
        return (excusedTeamMembers.length() >= 2 ? excusedTeamMembers.deleteCharAt(excusedTeamMembers.length() - 2)
                : excusedTeamMembers).toString();
    }

    /**
     * This method returns the list of all the agency representatives.
     *
     * @return String
     */
    private String getAgencyRepresentatives() {
        Collection<IepOtherService> otherInterAgencySvcs = m_currentIep.getIepOtherServices(getBroker());
        for (IepOtherService otherInterAgencySvc : otherInterAgencySvcs) {
            String serviceType = otherInterAgencySvc.getServiceType();
            if (PARAM_SERVICE_NAME_INTERAGENCY.equals(serviceType)) {
                agencyNames
                        .append((String) otherInterAgencySvc.getFieldValueByAlias(ALIAS_INTERAGENCY_NAME, m_dictionary))
                        .append(STRING_COMMA);
            }
        }
        return (agencyNames.length() >= 2 ? agencyNames.deleteCharAt(agencyNames.length() - 2) : agencyNames)
                .toString();
    }

    /**
     * This method returns the evaluations list for a meeting.
     *
     * @param meeting IepMeeting
     * @return String
     */
    private String getEvaluationsList(IepMeeting meeting) {
        String evaluations = (String) meeting.getFieldValueByAlias(ALIAS_EVALUATIONS_LIST, m_dictionary);
        if (evaluations != null && evaluations.contains(STRING_OTHER)) {
            evaluations = evaluations + "\nif, Other: "
                    + (String) meeting.getFieldValueByAlias(ALIAS_OTHER_EVALUATIONS_LIST, m_dictionary);
        }
        return evaluations;
    }

    /**
     * This method sets the chair person's details for the signature line of the SPED form. If the
     * team members does not
     * include a chair person, then the case manager signs the SPED form.
     *
     * @return void
     */
    private void getChairPersonDetails() {
        addParameter(PARAM_CHAIR_PERSON, "");
        SisPerson chairPerson = null;
        Collection<IepTeamMember> teamMbrs = m_currentIep.getTeamMembers();
        for (IepTeamMember teamMbr : teamMbrs) {
            if (teamMbr.getChairpersonIndicator()) {
                chairPerson = teamMbr.getPerson();
                break;
            }
        }

        if (chairPerson == null) {
            SisStaff caseManager = m_currentIep.getStaff();
            if (caseManager != null) {
                chairPerson = caseManager.getPerson();
            }
        }

        if (chairPerson != null) {
            addParameter(PARAM_CHAIR_PERSON, chairPerson.getFirstName() + " " + chairPerson.getLastName());
        }
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
