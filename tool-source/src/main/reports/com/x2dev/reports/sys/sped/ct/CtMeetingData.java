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
package com.x2dev.reports.sys.sped.ct;

import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The Class CtMeetingData.
 */
public class CtMeetingData extends BaseFormReportJavaSource {
    // Member variables
    private Map<IepMeetingAttendance, Map> m_attendanceData = new HashMap();
    private IepData m_currentIep;
    private SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    private List<IepMeetingAttendance> m_invitedAttendees = new ArrayList<IepMeetingAttendance>();
    private IepMeeting m_meeting;
    private SimpleDateFormat m_timeFormat = new SimpleDateFormat("hh:mm a");

    private String ALIAS_PARENT = "Parent";

    // Class parameters
    private String PARAM_KEY = "key";
    private String PARAM_NAME = "name";
    private String PARAM_RECIPIENT = "validRecipient";

    // Report parameters
    private String PARAM_IEP_DATA = "iepData";
    private String PARAM_MEETING = "meeting";
    private String PARAM_MEETING_DATE = "meetingDate";
    private String PARAM_MEETING_TIME = "meetingTime";
    private String PARAM_RECIPIENT_NAME = "recipientName";
    private String PARAM_RECIPIENT_ADDRESS_01 = "recipientAddress01";
    private String PARAM_RECIPIENT_CITY = "recipientCity";
    private String PARAM_RECIPIENT_STATE = "recipientState";
    private String PARAM_RECIPIENT_ZIP = "recipientZip";
    private String PARAM_STUDENT_DOB = "studentDob";
    private String PARAM_STUDENT_NAME = "studentName";

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        for (IepMeetingAttendance attendee : m_invitedAttendees) {
            Map<String, Object> attendeeData = m_attendanceData.get(attendee);
            if (Boolean.TRUE.equals(attendeeData.get(PARAM_RECIPIENT)))
            // if (attendee.getPrintInvitationIndicator()) TODO: use if all attendees should receive
            // letter
            {
                grid.append();
                grid.set(PARAM_IEP_DATA, m_currentIep);
                grid.set(PARAM_MEETING, m_meeting);
                addMeetingData(grid);
                addStudentData(grid);
                addInvitationRecipient(grid, attendee);
                addTeamMembers(grid);
            }
        }

        // if there are no recipients, use an empty grid to print a blank report
        if (grid.rowCount() == 0) {
            grid.append();
        }
        grid.beforeTop();

        return grid;
    }

    /**
     * Initialize report, m_currentIep (IepData), m_meeting (IepMeeting), and attendanceData by
     * calling loadTeamMembers.
     */
    @Override
    protected void initialize() {
        super.initialize();
        m_currentIep = (IepData) getFormOwner();
        m_meeting = (IepMeeting) getFormStorage();
        Collection<IepMeetingAttendance> attendance = m_meeting.getMeetingAttendance();
        loadTeamMembers(attendance);
    }

    /**
     * Add meeting invitation recipient data to grid for display on report.
     *
     * @param grid ReportDataGrid
     * @param attendee IepMeetingAttendance
     */
    private void addInvitationRecipient(ReportDataGrid grid, IepMeetingAttendance attendee) {
        if (attendee.getTeamMember() != null) {
            grid.set(PARAM_RECIPIENT_NAME, attendee.getTeamMember().getNameView());

            if (attendee.getTeamMember().getPerson() != null) {

                SisPerson person = attendee.getTeamMember().getPerson();
                SisAddress address = person.getPhysicalAddress();

                if (address == null) {
                    address = person.getMailingAddress();
                }

                if (address != null) {
                    grid.set(PARAM_RECIPIENT_ADDRESS_01, address.getAddressLine01());
                    grid.set(PARAM_RECIPIENT_CITY, address.getCity());
                    grid.set(PARAM_RECIPIENT_STATE, address.getState());
                    grid.set(PARAM_RECIPIENT_ZIP, address.getPostalCode());
                }
            }
        }

    }

    /**
     * Add meeting data to grid for display on report.
     *
     * @param grid ReportDataGrid
     */
    private void addMeetingData(ReportDataGrid grid) {
        grid.set(PARAM_MEETING_DATE, m_dateFormat.format(m_meeting.getDate()));
        grid.set(PARAM_MEETING_TIME, m_timeFormat.format(m_meeting.getTime()));
    }

    /**
     * Add Student data to grid for display on report.
     *
     * @param grid ReportDataGrid
     */
    private void addStudentData(ReportDataGrid grid) {
        if (m_currentIep.getStudent() != null) {
            SisStudent student = m_currentIep.getStudent();
            grid.set(PARAM_STUDENT_NAME, student.getNameView());

            if (student.getPerson() != null) {
                PlainDate dateOfBirth = student.getPerson().getDob();
                grid.set(PARAM_STUDENT_DOB, m_dateFormat.format(dateOfBirth));
            }
        }
    }

    /**
     * Add team member data to grid for display on report.
     *
     * @param grid ReportDataGrid
     */
    private void addTeamMembers(ReportDataGrid grid) {
        for (IepMeetingAttendance attendee : m_invitedAttendees) {
            Map<String, Object> attendeeData = m_attendanceData.get(attendee);
            grid.set((String) attendeeData.get(PARAM_KEY), (String) attendeeData.get(PARAM_NAME));
        }
    }

    /**
     * Loads team member key, name, and indicate whether they should be a letter recipient for each
     * attendee
     * in attendance. If attendee's role is already filled or is undefined, role is defined as
     * other.
     *
     * @param attendance Collection<IepMeetingAttendance>
     */
    private void loadTeamMembers(Collection<IepMeetingAttendance> attendance) {
        int otherCount = 1;

        for (IepMeetingAttendance attendee : attendance) {
            if (attendee.getPrintInvitationIndicator()) {
                m_invitedAttendees.add(attendee);
                Map<String, Object> attendeeData = new HashMap();
                List<String> roleFilled = new ArrayList<String>();

                String role = attendee.getTeamMember().getMemberRoleCode();
                String personName = attendee.getTeamMember().getNameView();
                attendeeData.put(PARAM_RECIPIENT, Boolean.FALSE);

                if (role.matches("Administrator|Regular Ed\\. Teacher|Special Ed\\. Teacher|Student") &&
                        !roleFilled.contains(role)) {
                    attendeeData.put(PARAM_KEY, role);
                    attendeeData.put(PARAM_NAME, personName);
                    roleFilled.add(role);
                } else {
                    attendeeData.put(PARAM_KEY, "Other " + otherCount);
                    attendeeData.put(PARAM_NAME, personName + " / " + role);
                    if (ALIAS_PARENT.equals(role)) {
                        attendeeData.put(PARAM_RECIPIENT, Boolean.TRUE);
                    }
                    otherCount++;
                }

                m_attendanceData.put(attendee, attendeeData);
            }
        }
    }

}
