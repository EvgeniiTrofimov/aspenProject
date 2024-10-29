/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.procedures.sys.sped.ri.RiSpedHelper;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Rhode Island IEP meeting invitation form. This class prepares a
 * ReportDataGrid that contains a row for each section of the report. Each row contains a format and
 * a java source for the corresponding section. The following are the sections prepared:
 * <p>
 * <table border="1">
 * <tr>
 * <th>Section</th>
 * <th>Java source description</th>
 * </tr>
 * <tr>
 * <td>Invitation letter</td>
 * <td>SimpleBeanDataSource on the meeting bean</td>
 * </tr>
 * <tr>
 * <td>Attendance sheet</td>
 * <td>QueryIteratorDataSource on attendance beans; this is to
 * populate the attendance sheet grid</td>
 * </tr>
 * <tr>
 * <td>Waiver form</td>
 * <td>SimpleBeanDataSource on the meeting bean</td>
 * </tr>
 * </table>
 * <p>
 * A letter and attendance sheet is printed for each team member who is both invited and designated
 * to receive an invitation. A waiver is printed for each team member who is either not invited or
 * excused.
 * <p>
 * Special handling exists for printing blank invitations. In this case, a single copy of each
 * format is included. The attendance sheet is configured to print several blank lines on the
 * team members table.
 *
 * @author X2 Development Corporation
 */
public class MeetingInvitationData extends BaseFormReportJavaSource {
    private static final String EMPTY = "";

    public static final String ALIAS_ADDTL_ATTENDEE_NAME = "all-ima-additionalName";
    public static final String ALIAS_PARENT_WAIVER = "img-parent-waives-notice";
    
    // RI-specific form parameter
    public static final String PARAM_AS_OF_GRADE = "asOfGrade";
    public static final String PARAM_AS_OF_SCHOOL = "asOfSchool";
    public static final String PARAM_AS_OF_SCHOOL_ADR1 = "asOfSchoolAdr1";
    public static final String PARAM_AS_OF_SCHOOL_ADR2 = "asOfSchoolAdr2";
    public static final String PARAM_AS_OF_SCHOOL_ADR3 = "asOfSchoolAdr3";
    public static final String PARAM_AS_OF_SCHOOL_PHONE = "asOfSchoolPhn";
    public static final String PARAM_FORM_DATE = "formDate";
    public static final String PARAM_PARENT_WAIVER = "parentWaiver";
    public static final String PARAM_PURPOSE_FORMAT = "purposeFormat";
    public static final String PARAM_PURPOSE_LIST = "purposeList";
    public static final String PARAM_RECIPIENT_KEY = "recipient";
    public static final String PARAM_RECIPIENT_VAL = "Parent/Guardian/Adult Student";

    // Constants for the letter (N3I) portion of the report
    public static final String TO_PARAMETER = "to";

    // Constants for the waiver (N3W) portion of the report
    public static final String TEAM_MEMBER_PARAMETER = "teamMember";
    public static final String WAIVER_TYPE_PARAMETER = "waiverType";

    public static final String WAIVER_TYPE_EXCUSED = "excused";
    public static final String WAIVER_TYPE_NOT_INVITED = "notInvited";

    // ReportDataGrid column constants for the main report
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";
    private static final String COL_PAGE_NUMBER = "pageNumber";
    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";
    private static final String COL_PARAMETER_MAP = "parameters";

    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-RI-MTG-SUB1";
    private static final String PAGE_2_FORMAT_ID = "SYS-SPED-RI-MTG-SUB2";
    private static final String PAGE_3_FORMAT_ID = "SYS-SPED-RI-MTG-SUB4";
    private static final String PAGE_MEETING_PURPOSE = "SYS-SPED-RI-MTG-PURP";
    private static final String PAGE_WAIVER_FORMAT_ID = "SYS-SPED-RI-MTG-SUB3";

    private static final String PURPOSE_OTHER = "Other";

    private int m_currentPageNumber = 0;
    private Map m_subReports = null;
    private RiSpedHelper m_rihlp = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {

        ReportDataGrid grid = new ReportDataGrid();
        m_rihlp = new RiSpedHelper(getSecondaryBroker(), getOrganization());
        loadSubReports();

        IepData iep = (IepData) getFormOwner();
        IepMeeting meeting = (IepMeeting) getFormStorage();

        PlainDate startDate = (getFormInstance() != null && getFormInstance().getCreatedTime() > 0
                ? new PlainDate(getFormInstance().getCreatedTime())
                : new PlainDate());

        SisStudent student = iep.getStudent();
        SisSchool currentSchool = student.getSchool();
        String currentGrade = student.getGradeLevel();
        List<String> enrollmentTypes =
                new ArrayList<String>(Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.YOG_CHANGE));
        if (iep.getStatusCodeEnum() != StatusCode.ACTIVE && iep.getStatusCodeEnum() != StatusCode.DRAFT) {

            StudentEnrollment lastEnrollment =
                    m_rihlp.getNearLastStudentEnrollment(student, startDate, enrollmentTypes);
            currentGrade = m_rihlp.getGradeLevel(startDate, lastEnrollment);
            currentSchool = lastEnrollment.getSchool();
        }

        addParameter(PARAM_PARENT_WAIVER, meeting.getFieldValueByAlias(ALIAS_PARENT_WAIVER, getDictionary()));
        addParameter(PARAM_AS_OF_GRADE, currentGrade);

        addParameter(PARAM_AS_OF_SCHOOL, currentSchool.getName());
        if (null != currentSchool.getAddress()) {
            addParameter(PARAM_AS_OF_SCHOOL_ADR1, currentSchool
                    .getAddress().getAddressLine01());

            String addressLine2 = StringUtils.coalesce(currentSchool.getAddress()
                    .getAddressLine02(), EMPTY);
            addParameter(PARAM_AS_OF_SCHOOL_ADR2, addressLine2);
            addParameter(PARAM_AS_OF_SCHOOL_ADR3, currentSchool
                    .getAddress().getAddressLine03());
            addParameter(PARAM_AS_OF_SCHOOL_PHONE, StringUtils.coalesce(currentSchool
                    .getAddress().getPhone01(), EMPTY));
        }


        FormInstance formInstance = getFormInstance();
        long timestamp = 0;
        if (formInstance != null) {
            timestamp = formInstance.getCreatedTime();
        }
        Date formDate = new Date();
        if (timestamp > 0) {
            formDate = new Date(timestamp);
        }

        addParameter(PARAM_FORM_DATE, formDate);

        addParameter(PARAM_RECIPIENT_KEY, PARAM_RECIPIENT_VAL);

        if (isBlank()) {
            preparePage1(grid, new IepMeetingAttendance(getBroker().getPersistenceKey()));
            preparePage2(grid);
            prepareWaiver(grid, new IepTeamMember(getBroker().getPersistenceKey()), new HashMap(0));
        } else {
            /*
             * Add invitations
             */
            X2Criteria attendanceCriteria = new X2Criteria();
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.FALSE);

            QueryByCriteria attendanceQuery = new QueryByCriteria(IepMeetingAttendance.class, attendanceCriteria);
            attendanceQuery
                    .addOrderByAscending(IepMeetingAttendance.REL_TEAM_MEMBER + "." +
                            IepTeamMember.COL_NAME_VIEW);

            QueryIterator attendance = getBroker().getIteratorByQuery(attendanceQuery);
            addMeetingPurposeToAttendance();
            try {
                while (attendance.hasNext()) {
                    IepMeetingAttendance attendanceBean = (IepMeetingAttendance) attendance.next();

                    /*
                     * We check the print invitation indicator here instead of filtering it out of
                     * the query above since the query is also used below to generate waivers
                     */
                    if (attendanceBean.getPrintInvitationIndicator()) {
                        preparePage1(grid, attendanceBean);
                        preparePage2(grid);
                        if (!StringUtils.isEmpty(meeting.getDetails())) {
                            preparePage3(grid, attendanceBean);
                        }
                    }
                }
            } finally {
                attendance.close();
            }

            /*
             * Add waivers
             */

            Criteria excusalCriteria = new Criteria();
            excusalCriteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
            excusalCriteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.TRUE);

            QueryByCriteria excusalQuery = new QueryByCriteria(IepMeetingAttendance.class, excusalCriteria);

            Map<String, IepMeetingAttendance> excusalMap =
                    getBroker().getMapByQuery(excusalQuery, IepMeetingAttendance.COL_TEAM_MEMBER_OID, 16);

            attendanceCriteria = new X2Criteria();
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.FALSE);
            attendanceCriteria.addNotNull(IepMeetingAttendance.COL_TEAM_MEMBER_OID);

            Criteria waiverCriteria = new Criteria();
            waiverCriteria.addEqualTo(IepTeamMember.COL_IEP_DATA_OID, meeting.getIepDataOid());
            waiverCriteria.addNotIn(X2BaseBean.COL_OID, new SubQuery(IepMeetingAttendance.class,
                    IepMeetingAttendance.COL_TEAM_MEMBER_OID, attendanceCriteria));

            QueryByCriteria waiverQuery = new QueryByCriteria(IepTeamMember.class, waiverCriteria);
            waiverQuery.addOrderByAscending(IepTeamMember.COL_NAME_VIEW);

            QueryIterator waivedTeamMembers = getBroker().getIteratorByQuery(waiverQuery);
            try {
                while (waivedTeamMembers.hasNext()) {
                    IepTeamMember teamMember = (IepTeamMember) waivedTeamMembers.next();
                    prepareWaiver(grid, teamMember, excusalMap);
                }
            } finally {
                waivedTeamMembers.close();
            }
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     * @return byte[]
     */
    private byte[] getSubreportFormat(String pageId) {
        Report report = (Report) m_subReports.get(pageId);
        return report.getCompiledFormat();
    }

    /**
     * Gets the attendee name.
     *
     * @param attendance IepMeetingAttendance
     * @return String
     */
    private String getAttendeeName(IepMeetingAttendance attendance) {
        String addtlAttName = null;
        IepTeamMember teamMember = attendance.getTeamMember();
        if (teamMember != null) {
            addtlAttName = teamMember.getNameView();
        } else {
            addtlAttName = (String) attendance.getFieldValueByAlias(ALIAS_ADDTL_ATTENDEE_NAME);
        }
        return addtlAttName == null ? EMPTY : addtlAttName;
    }

    /**
     * Loads each subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {PAGE_1_FORMAT_ID,
                PAGE_2_FORMAT_ID,
                PAGE_3_FORMAT_ID,
                PAGE_MEETING_PURPOSE,
                PAGE_WAIVER_FORMAT_ID}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

    /**
     * Prepares the first page (N3).
     *
     * @param grid ReportDataGrid
     * @param attendanceBean IepMeetingAttendance
     */
    private void preparePage1(ReportDataGrid grid, IepMeetingAttendance attendanceBean) {
        JRDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        HashMap<String, Object> parameterMap = new HashMap<String, Object>();
        parameterMap.put(TO_PARAMETER, attendanceBean.getTeamMember());
        parameterMap.putAll(getParameters());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_1_FORMAT_ID));
        grid.set(COL_PARAMETER_MAP, parameterMap);
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "N3");
    }

    /**
     * Class intended to keep meeting purpose data.
     */
    public static class MeetingPurpose {

        private String purpose;

        /**
         * Instantiates a new meeting purpose.
         */
        public MeetingPurpose() {

        }

        /**
         * Instantiates a new meeting purpose.
         *
         * @param aPurpose String
         */
        public MeetingPurpose(String aPurpose) {
            purpose = aPurpose;
        }

        /**
         * Gets the purpose.
         *
         * @return String
         */
        public String getPurpose() {
            return purpose;
        }

        /**
         * Sets the purpose.
         *
         * @param aPurpose void
         */
        public void setPurpose(String aPurpose) {
            purpose = aPurpose;
        }
    }


    /**
     * Appends meeting purpose data to meeting details.
     */
    private void addMeetingPurposeToAttendance() {
        IepMeeting meeting = (IepMeeting) getFormStorage();
        String meetingPurpose = meeting.getFieldD008();
        if (meetingPurpose != null && meetingPurpose.length() > 0) {
            String[] meetingPurposesString = meetingPurpose.split(",");
            String meetingPurposeOther = "";
            List<MeetingPurpose> meetingPurposes = new ArrayList<MeetingInvitationData.MeetingPurpose>();
            for (String aPurpose : meetingPurposesString) {
                if (aPurpose.contains(PURPOSE_OTHER)) {
                    meetingPurposeOther = PURPOSE_OTHER + "          " + meeting.getFieldD002();
                } else {
                    meetingPurposes.add(new MeetingPurpose(aPurpose));
                }
            }
            // Add 'Other' purpose at last position of purposes
            if (!meetingPurposeOther.isEmpty()) {
                meetingPurposes.add(new MeetingPurpose(meetingPurposeOther));
            }

            addParameter(PARAM_PURPOSE_LIST, meetingPurposes);
        }
        addParameter(PARAM_PURPOSE_FORMAT, getSubreportFormat(PAGE_MEETING_PURPOSE));
    }

    /**
     * Prepares the second page (N3 A).
     *
     * @param grid ReportDataGrid
     */
    private void preparePage2(ReportDataGrid grid) {
        IepMeeting meeting = (IepMeeting) getFormStorage();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
        criteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.FALSE);

        QueryByCriteria query = new QueryByCriteria(IepMeetingAttendance.class, criteria);

        List<IepMeetingAttendance> attendance;
        if (isBlank()) {
            attendance = new ArrayList<IepMeetingAttendance>(5);

            IepData iep = (IepData) getFormOwner();

            IepMeetingAttendance blankAttendance = new IepMeetingAttendance(getBroker().getPersistenceKey());
            blankAttendance.setStudentOid(iep.getStudentOid());
            blankAttendance.setIepDataOid(iep.getOid());

            for (int i = 0; i < 10; i++) {
                attendance.add(blankAttendance);
            }
        } else {
            attendance = new ArrayList(getBroker().getCollectionByQuery(query));
            Collections.sort(attendance, new Comparator<IepMeetingAttendance>() {

                @Override
                public int compare(IepMeetingAttendance o1, IepMeetingAttendance o2) {
                    String attendeeName1 = getAttendeeName(o1);
                    String attendeeName2 = getAttendeeName(o2);
                    int result = attendeeName1.compareTo(attendeeName2);
                    if (StringUtils.isEmpty(attendeeName2)) {
                        result = -1;
                    }
                    return result;
                }

            });
        }

        JRDataSource dataSource = new BeanCollectionDataSource(attendance,
                getDictionary(),
                getLocale());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_2_FORMAT_ID));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "N3 A");
    }

    /**
     * Prepares the first page (N3).
     *
     * @param grid ReportDataGrid
     * @param attendanceBean IepMeetingAttendance
     */
    private void preparePage3(ReportDataGrid grid, IepMeetingAttendance attendanceBean) {
        JRDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        HashMap<String, Object> parameterMap = new HashMap<String, Object>();
        parameterMap.put(TO_PARAMETER, attendanceBean.getTeamMember());
        parameterMap.putAll(getParameters());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_3_FORMAT_ID));
        grid.set(COL_PARAMETER_MAP, parameterMap);
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "N3");
    }

    /**
     * Prepares a waiver page.
     *
     * @param grid ReportDataGrid
     * @param teamMember IepTeamMember
     * @param excusalMap Map<String,IepMeetingAttendance>
     */
    private void prepareWaiver(ReportDataGrid grid,
                               IepTeamMember teamMember,
                               Map<String, IepMeetingAttendance> excusalMap) {
        HashMap<String, Object> parameterMap = new HashMap<String, Object>();
        parameterMap.put(TEAM_MEMBER_PARAMETER, teamMember);
        parameterMap.put(WAIVER_TYPE_PARAMETER,
                excusalMap.containsKey(teamMember.getOid()) ? WAIVER_TYPE_EXCUSED : WAIVER_TYPE_NOT_INVITED);
        parameterMap.putAll(getParameters());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_WAIVER_FORMAT_ID));
        grid.set(COL_PARAMETER_MAP, parameterMap);
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "N3W");
    }

}
