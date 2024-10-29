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
 * New Jersey SPED form for Invitation to Disciplinary Action Meeting.
 *
 * @author Follett Software Company
 */
public class InviteToDisciplinaryActionMtg extends BaseFormReportJavaSource {
    /**
     * Instances
     */
    private StringBuilder attendees = new StringBuilder();
    private IepData m_currentIep = null;
    private ReportDataGrid m_grid;
    private PlainDate m_responseDate = new PlainDate();

    /**
     * Aliases
     */
    private static final String ALIAS_BEH_INTERVENTION_PLAN = "sped-review-beh-int-plan";
    private static final String ALIAS_DET_IAES = "sped-interim-alt-edu-setting";
    private static final String ALIAS_FUNC_BEH_ASSESSMENT = "sped-func-beh-assessment";
    private static final String ALIAS_OTHER = "sped-other-reason";
    private static final String ALIAS_OTHER_CONTACT_PERSON = "sped-other-contact-person";
    private static final String ALIAS_OTHER_DESC = "sped-other-description";
    private static final String ALIAS_MANIFESTATION = "sped-manifestation-deter";

    /**
     * Fields
     */
    private static final String FIELD_FULL_NAME = "fullName";
    private static final String FIELD_ADDRESS_LINE1 = "addressLine1";
    private static final String FIELD_ADDRESS_LINE2 = "addressLine2";

    /**
     * Params
     */
    private static final String PARAM_AGE_CATEGORY = "AGE_CATEGORY";
    private static final String PARAM_BEH_INTERVENTION_PLAN = "BEH_INTERVENTION_PLAN";
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_DET_IAES = "DET_IAES";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_FUNC_BEH_ASSESSMENT = "FUNC_BEH_ASSESSMENT";
    private static final String PARAM_INVITED_TEAM_MBRS = "INVITED_TEAM_MBRS";
    private static final String PARAM_MEETING_DATE = "MEETING_DATE";
    private static final String PARAM_MEETING_TIME = "MEETING_TIME";
    private static final String PARAM_MEETING_LOCATION = "MEETING_LOCATION";
    private static final String PARAM_MANIFESTATION = "MANIFESTATION";
    private static final String PARAM_OTHER = "OTHER";
    private static final String PARAM_OTHER_DESC = "OTHER_DESC";
    private static final String PARAM_OTHER_CONTACT_PERSON = "OTHER_CONTACT_PERSON";
    private static final String PARAM_RESPONSE_DATE = "RESPONSE_DATE";
    private static final String PARAM_STUDENT_NAME = "STUDENT_NAME";

    /**
     * Other Constants
     */
    private static final String AGE_CATEGORY_U18 = "AgeCategoryU18";
    private static final String AGE_CATEGORY_O18 = "AgeCategoryO18";
    private static final String EMPTY_STRING = "";
    private static final String REFCODE_PARENT = "Parent/Guardian";

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
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());

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

        addParameter(PARAM_AGE_CATEGORY, (age < 18) ? AGE_CATEGORY_U18 : AGE_CATEGORY_O18);
        addParameter(PARAM_STUDENT_NAME, student.getPerson().getFirstName() + " " + student.getPerson().getLastName());
        addParameter(PARAM_INVITED_TEAM_MBRS, getInvitedTeamMembers(meeting));
        addParameter(PARAM_MEETING_LOCATION, meeting.getLocation());
        addParameter(PARAM_MANIFESTATION, meeting.getFieldValueByAlias(ALIAS_MANIFESTATION, dictionary));
        addParameter(PARAM_FUNC_BEH_ASSESSMENT, meeting.getFieldValueByAlias(ALIAS_FUNC_BEH_ASSESSMENT, dictionary));
        addParameter(PARAM_BEH_INTERVENTION_PLAN,
                meeting.getFieldValueByAlias(ALIAS_BEH_INTERVENTION_PLAN, dictionary));
        addParameter(PARAM_DET_IAES, meeting.getFieldValueByAlias(ALIAS_DET_IAES, dictionary));
        addParameter(PARAM_OTHER_CONTACT_PERSON, meeting.getFieldValueByAlias(ALIAS_OTHER_CONTACT_PERSON, dictionary));

        Object otherPurpose = meeting.getFieldValueByAlias(ALIAS_OTHER, dictionary);
        addParameter(PARAM_OTHER_DESC, "");
        if (otherPurpose != null) {
            addParameter(PARAM_OTHER, otherPurpose);
            Object otherPurposeDesc = meeting.getFieldValueByAlias(ALIAS_OTHER_DESC, dictionary);
            if (otherPurposeDesc != null) {
                addParameter(PARAM_OTHER_DESC, otherPurposeDesc);
            }
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
     * This method loads the Under-18 version of the form for Parents.
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
     * This method loads the Over-18 version of the form for Students.
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
        m_grid.set(FIELD_ADDRESS_LINE1, EMPTY_STRING);
        m_grid.set(FIELD_ADDRESS_LINE2, EMPTY_STRING);

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
