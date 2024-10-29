/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ga;

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class GA_SPED_Meeting.
 */
public class GA_SPED_Meeting extends BaseFormReportJavaSource {

    /*
     * ID of the subreports: Required Members and Additional Members.
     */
    public static final String REQUIRED_MEMBERS_SUBREPORT_ID = "SYS-SPED-GA-IEP-MTG1";
    public static final String ADDITIONAL_MEMBERS_SUBREPORT_ID = "SYS-SPED-GA-IEP-MTG2";

    /*
     * Parameter containing the data for the Required Members and Additional Members sub-reports.
     */
    public static final String PARAM_REQUIRED_MEMBERS_SUBREPORT_DATA = "requiredMembersData";
    public static final String PARAM_ADDITIONAL_MEMBERS_SUBREPORT_DATA = "additionalMembersData";

    /*
     * Parameter containing the format for the Required Members and Additional Members sub-reports.
     */
    public static final String PARAM_REQUIRED_MEMBERS_SUBREPORT_FORMAT = "requiredMembersFormat";
    public static final String PARAM_ADDITIONAL_MEMBERS_SUBREPORT_FORMAT = "additionalMembersFormat";

    /*
     * Subreport constants
     */
    private static final String SUBREPORT_TEAM_MEMBER_FIELD = "teamMember";
    private static final String SUBREPORT_PRESENT_INDICATOR_FIELD = "presentIndicator";

    /*
     * Member variables
     */
    private IepData m_currentIep;
    private DataDictionary m_dictionary;
    private IepMeeting m_meeting;
    private ReportDataGrid m_meetingAdditionalMembersGrid;
    private ReportDataGrid m_meetingRequiredMembersGrid;
    private Map<String, String> m_meetingDetails;
    private Map<String, Object> m_meetingPurposes;
    private SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    private SimpleDateFormat m_timeFormat = new SimpleDateFormat("h:mm a");
    Collection<IepMeetingAttendance> m_requiredAttendance = new ArrayList<IepMeetingAttendance>();
    Collection<IepMeetingAttendance> m_additionalAttendance = new ArrayList<IepMeetingAttendance>();

    /*
     * Aliases used
     */
    private static final String ALIAS_MEETING_PURPOSE = "img-meeting-purpose";


    /*
     * Parameters used in iReport
     */
    private static final String PARAM_FORM_DATE = "formDate";
    private static final String PARAM_MEETING_DATE = "meetingDate";
    private static final String PARAM_MEETING_TIME = "meetingTime";
    private static final String PARAM_MEETING_LOCATION = "meetingLocation";
    private static final String PARAM_PURPOSE_ELIGIBILITY = "purposeEligibility";
    private static final String PARAM_PURPOSE_PLACEMENT = "purposePlacement";
    private static final String PARAM_PURPOSE_DEVELOP_IEP = "purposeDevelopIep";
    private static final String PARAM_PURPOSE_REVIEW_IEP = "purposeReviewIep";
    private static final String PARAM_PURPOSE_TRANSITION = "purposeTransition";
    private static final String PARAM_PURPOSE_REEVALUATION = "purposeReevaluation";
    private static final String PARAM_PURPOSE_REVIEW_RESULTS = "purposeReviewResults";
    private static final String PARAM_PURPOSE_BEHAVIOR = "purposeBehavior";
    private static final String PARAM_PURPOSE_OTHER = "purposeOther";
    private static final String PARAM_PURPOSE_OTHER_DETAIL = "purposeOtherDetail";
    private static final String PARAM_RECIPIENT_NAME = "recipientName";

    /*
     * Aliases used from IEP Meeting table
     */
    private static final String ALIAS_PURPOSE_OTHER = "img-purpose-other";
    private static final String ALIAS_REQUIRED_MEMBER = "itm-required-member";

    /*
     * Codes used for IEP meeting purpose
     */
    private static final String CODE_STATE_ELIBILITY = "01";
    private static final String CODE_STATE_PLACEMENT = "02";
    private static final String CODE_STATE_DEVELOP_IEP = "03";
    private static final String CODE_STATE_REVIEW_IEP = "04";
    private static final String CODE_STATE_TRANSITION = "05";
    private static final String CODE_STATE_REEVALUATION = "06";
    private static final String CODE_STATE_REVIEW_RESULTS = "07";
    private static final String CODE_STATE_BEHAVIOR = "08";
    private static final String CODE_STATE_OTHER = "09";

    /*
     * Minimum number of rows for the team members
     */
    private final int TEAM_MEMBERS_NUMBER = 6;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        if (getParameter(PARAM_BLANK).equals(Boolean.FALSE)) {
            if (m_meeting != null) {
                // Add support for the required and additional members sub-report
                Report requiredMembersSubreport = ReportUtils.getReport(REQUIRED_MEMBERS_SUBREPORT_ID, getBroker());
                Report additionalMembersSubreport = ReportUtils.getReport(ADDITIONAL_MEMBERS_SUBREPORT_ID, getBroker());

                // Iterate through the attendance again to append the grid
                for (IepMeetingAttendance attendee : m_meeting.getMeetingAttendance()) {
                    IepTeamMember teamMember = attendee.getTeamMember();
                    String recipientName = null;
                    if (teamMember != null) {
                        recipientName = teamMember.getNameView();
                    }

                    if (attendee.getPrintInvitationIndicator()) {
                        grid.append();
                        grid.set(PARAM_FORM_DATE, m_dateFormat.format(new Date()));
                        grid.set(PARAM_RECIPIENT_NAME, recipientName);

                        if (!m_meetingDetails.isEmpty()) {
                            addMeetingDetails(grid, m_meetingDetails);
                        }
                        if (!m_meetingPurposes.isEmpty()) {
                            addMappedDetails(grid, m_meetingPurposes);
                        }
                        if (requiredMembersSubreport != null) {
                            grid.set(PARAM_REQUIRED_MEMBERS_SUBREPORT_FORMAT,
                                    new ByteArrayInputStream(requiredMembersSubreport.getCompiledFormat()));
                        }
                        grid.set(PARAM_REQUIRED_MEMBERS_SUBREPORT_DATA, m_meetingRequiredMembersGrid);
                        if (additionalMembersSubreport != null) {
                            grid.set(PARAM_ADDITIONAL_MEMBERS_SUBREPORT_FORMAT,
                                    new ByteArrayInputStream(additionalMembersSubreport.getCompiledFormat()));
                        }
                        grid.set(PARAM_ADDITIONAL_MEMBERS_SUBREPORT_DATA, m_meetingAdditionalMembersGrid);
                    }
                }
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
        ExtendedDictionaryAttributes extendDictionary = m_currentIep.getExtendedDataDictionary();
        m_dictionary = DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());
        m_meeting = (IepMeeting) getFormStorage();
        m_meetingDetails = loadMeetingParameters(m_meeting);
        m_meetingPurposes = loadMeetingPurposes(m_currentIep, m_meeting);

        loadIepMeetingAttendance(m_meeting);
        m_meetingRequiredMembersGrid = loadMeetingAttendanceGrid(m_requiredAttendance, true);
        m_meetingAdditionalMembersGrid = loadMeetingAttendanceGrid(m_additionalAttendance, false);
    }

    /**
     * Load the iep meeting attendance for the required and additional attendance based on the given
     * iep meeting.
     *
     * @param latestMeeting IepMeeting
     */
    private void loadIepMeetingAttendance(IepMeeting latestMeeting) {
        // Iterate through all meeting attendance
        for (IepMeetingAttendance attendee : latestMeeting.getMeetingAttendance()) {
            // Check if the attendee is a requiredMember or not
            IepTeamMember teamMember = attendee.getTeamMember();
            if (teamMember != null) {
                String isRequiredMember =
                        (String) teamMember.getFieldValueByAlias(ALIAS_REQUIRED_MEMBER, getDictionary());
                if (isRequiredMember != null && isRequiredMember.equals("1")) {
                    m_requiredAttendance.add(attendee);
                } else {
                    m_additionalAttendance.add(attendee);
                }
            }
        }

        // Make both required members and additional members the same height/size if possible
        if (!m_requiredAttendance.isEmpty()) {
            if (m_requiredAttendance.size() < m_additionalAttendance.size()) {
                for (int i = m_requiredAttendance.size(); i < m_additionalAttendance.size(); i++) {
                    m_requiredAttendance.add(new IepMeetingAttendance(getBroker().getPersistenceKey()));
                }
            } else if (m_requiredAttendance.size() > m_additionalAttendance.size()) {
                for (int i = m_additionalAttendance.size(); i < m_requiredAttendance.size(); i++) {
                    m_additionalAttendance.add(new IepMeetingAttendance(getBroker().getPersistenceKey()));
                }
            }
        } else {
            if (!m_additionalAttendance.isEmpty() && m_additionalAttendance.size() < TEAM_MEMBERS_NUMBER) {
                for (int i = m_additionalAttendance.size(); i < TEAM_MEMBERS_NUMBER; i++) {
                    m_additionalAttendance.add(new IepMeetingAttendance(getBroker().getPersistenceKey()));
                }
            }
        }
    }

    /**
     * Get the report data grid for the subreports.
     *
     * @param meetingAttendances Collection<IepMeetingAttendance>
     * @param isRequiredMember boolean
     * @return reportDataGrid
     */
    private ReportDataGrid loadMeetingAttendanceGrid(Collection<IepMeetingAttendance> meetingAttendances,
                                                     boolean isRequiredMember) {
        ReportDataGrid iepMeetingAttendanceGrid = new ReportDataGrid();

        for (IepMeetingAttendance meetingAttendance : meetingAttendances) {
            iepMeetingAttendanceGrid.append();
            iepMeetingAttendanceGrid.set(SUBREPORT_TEAM_MEMBER_FIELD, meetingAttendance.getTeamMember());

            if (isRequiredMember) {
                iepMeetingAttendanceGrid.set(SUBREPORT_PRESENT_INDICATOR_FIELD,
                        Boolean.valueOf(meetingAttendance.getPresentIndicator()));
            }
        }

        iepMeetingAttendanceGrid.beforeTop();

        return iepMeetingAttendanceGrid;
    }

    /**
     * Given the latest meeting, getting the meeting purposes. Add each meeting purpose to
     * map with a value of Boolean.TRUE
     *
     * @param iepData IepData
     * @param latestMeeting IepMeeting
     * @return meetingPurposesMap
     */
    private Map<String, Object> loadMeetingPurposes(IepData iepData, IepMeeting latestMeeting) {
        Map<String, Object> meetingPurposesMap = new HashMap<String, Object>();
        String[] meetingPurposes = null;
        String meetingPurposesString = (String) latestMeeting.getFieldValueByAlias(ALIAS_MEETING_PURPOSE, m_dictionary);
        if (!StringUtils.isEmpty(meetingPurposesString)) {
            meetingPurposes = meetingPurposesString.split(",");
        }
        /*
         * Populate reference code map for values of table Meeting Purposes
         */
        Map<String, ReferenceCode> refCodeMap = new HashMap<String, ReferenceCode>();
        Criteria refCodeCriteria = new Criteria();
        refCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbGaIepMtgPrp");
        QueryByCriteria refCodesQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);
        refCodeMap = getBroker().getMapByQuery(refCodesQuery, ReferenceCode.COL_CODE, 64);

        if (meetingPurposes != null) {
            for (String meetingPurpose : meetingPurposes) {
                ReferenceCode meetingPurposeCode = refCodeMap.get(meetingPurpose.trim());
                if (meetingPurposeCode != null) {
                    String meetingPurposeSystemCode = meetingPurposeCode.getSystemCode();

                    if (CODE_STATE_BEHAVIOR.equals(meetingPurposeSystemCode)) {
                        meetingPurposesMap.put(PARAM_PURPOSE_BEHAVIOR, Boolean.TRUE);
                    } else if (CODE_STATE_DEVELOP_IEP.equals(meetingPurposeSystemCode)) {
                        meetingPurposesMap.put(PARAM_PURPOSE_DEVELOP_IEP, Boolean.TRUE);
                    } else if (CODE_STATE_ELIBILITY.equals(meetingPurposeSystemCode)) {
                        meetingPurposesMap.put(PARAM_PURPOSE_ELIGIBILITY, Boolean.TRUE);
                    } else if (CODE_STATE_OTHER.equals(meetingPurposeSystemCode)) {
                        meetingPurposesMap.put(PARAM_PURPOSE_OTHER, Boolean.TRUE);
                        meetingPurposesMap.put(PARAM_PURPOSE_OTHER_DETAIL,
                                m_meeting.getFieldValueByAlias(ALIAS_PURPOSE_OTHER, m_dictionary));
                    } else if (CODE_STATE_PLACEMENT.equals(meetingPurposeSystemCode)) {
                        meetingPurposesMap.put(PARAM_PURPOSE_PLACEMENT, Boolean.TRUE);
                    } else if (CODE_STATE_REEVALUATION.equals(meetingPurposeSystemCode)) {
                        meetingPurposesMap.put(PARAM_PURPOSE_REEVALUATION, Boolean.TRUE);
                    } else if (CODE_STATE_REVIEW_IEP.equals(meetingPurposeSystemCode)) {
                        meetingPurposesMap.put(PARAM_PURPOSE_REVIEW_IEP, Boolean.TRUE);
                    } else if (CODE_STATE_REVIEW_RESULTS.equals(meetingPurposeSystemCode)) {
                        meetingPurposesMap.put(PARAM_PURPOSE_REVIEW_RESULTS, Boolean.TRUE);
                    } else if (CODE_STATE_TRANSITION.equals(meetingPurposeSystemCode)) {
                        meetingPurposesMap.put(PARAM_PURPOSE_TRANSITION, Boolean.TRUE);
                    }
                }
            }
        }
        return meetingPurposesMap;
    }

    /**
     * Add the mapped details to the grid.
     *
     * @param grid ReportDataGrid
     * @param mappedDetails Map<String,Object>
     */
    private void addMappedDetails(ReportDataGrid grid, Map<String, Object> mappedDetails) {
        Iterator iterator = mappedDetails.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry pair = (Map.Entry) iterator.next();
            String parameter = (String) pair.getKey();
            Object detail = pair.getValue();

            grid.set(parameter, detail);
        }
    }

    /**
     * Add meeting details as parameters to the grid.
     *
     * @param grid ReportDataGrid
     * @param meetingDetails Map<String,String>
     */
    private void addMeetingDetails(ReportDataGrid grid, Map<String, String> meetingDetails) {
        grid.set(PARAM_MEETING_DATE, meetingDetails.get(PARAM_MEETING_DATE));
        grid.set(PARAM_MEETING_TIME, meetingDetails.get(PARAM_MEETING_TIME));
        grid.set(PARAM_MEETING_LOCATION, meetingDetails.get(PARAM_MEETING_LOCATION));
    }

    /**
     * Given the latest meeting, store the meeting details in map which is returned.
     * Meeting details consist of meeting date, time and location.
     *
     * @param latestMeeting IepMeeting
     * @return meetingDetails
     */
    private Map<String, String> loadMeetingParameters(IepMeeting latestMeeting) {
        HashMap<String, String> meetingDetails = new HashMap<String, String>();

        /*
         * Correctly format the latest meeting date, then add it as a parameter to the report
         */
        String latestMeetingDate = null;
        if (latestMeeting.getDate() != null) {
            latestMeetingDate = m_dateFormat.format(latestMeeting.getDate());
        }
        meetingDetails.put(PARAM_MEETING_DATE, latestMeetingDate);

        /*
         * Correctly format the latest meeting time, then add it as a parameter to the report
         */
        String latestMeetingTime = null;
        if (latestMeeting.getTime() != null) {
            latestMeetingTime = m_timeFormat.format(latestMeeting.getTime());
        }
        meetingDetails.put(PARAM_MEETING_TIME, latestMeetingTime);

        /*
         * Add the latest meeting location as a parameter to the report
         */
        meetingDetails.put(PARAM_MEETING_LOCATION, latestMeeting.getLocation());

        return meetingDetails;
    }

}
