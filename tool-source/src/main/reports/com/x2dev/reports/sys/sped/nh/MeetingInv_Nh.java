/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.nh;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the New Hampshire IEP meeting invitation form. This class prepares a
 * ReportDataGrid that contains a row for each section of the report. Each row contains a format and
 * a java source for the corresponding section. The following are the sections prepared:
 * <p>
 * <table border="1">
 * <tr>
 * <th>Section</th>
 * <th>Java source description</th>
 * </tr>
 * <tr>
 * <td>Notification of SPED Meeting</td>Provides a attendanceBean
 * <td></td>
 * </tr>
 * <tr>
 * <td>Excusal form</td>
 * <td>SimpleBeanDataSource containing a meetingAttendance bean</td>
 * </tr>
 * </table>
 * <p>
 * An excusal form is printed for each team member who is excused (partially or fully).
 * <p>
 * Special handling exists for printing blank notifications. In this case, a single copy of each
 * format is included. The attendance sheet is configured to print several blank lines on the
 * team members table.
 *
 * @author X2 Development Corporation
 */
public class MeetingInv_Nh extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    // TODO: relocate aliases to NewHampshireAliases.java
    /**
     * Alias: verbal confirmation date
     */
    public static final String ALIAS_VERBAL_CONF = "mtg-verbal-conf";

    /**
     * Alias: written notice date
     */
    public static final String ALIAS_WRITTEN_NOTICE = "mtg-written-notice";

    /**
     * ReportDataGrid Column ID: datasource
     */
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";

    /**
     * ReportDataGrid Column ID: format column
     */
    private static final String COL_SUBREPORT_FORMAT = "format";

    /**
     * ReportDataGrid Column ID: page identifier
     */
    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";

    /**
     * ReportDataGrid Column ID: page number
     */
    private static final String COL_PAGE_NUMBER = "pageNumber";

    /**
     * ReportDataGrid Column ID: parameter map
     */
    private static final String COL_PARAMETER_MAP = "parameters";

    /**
     * Default format used to store dates
     */
    private static final String DEFAULT_DATE_FORMAT = "yyyy-MM-dd";

    /**
     * Format ID: Excusal sheet
     */
    private static final String FORMAT_ID_EXCUSAL = "SYS-SPED-NH-N3W";

    /**
     * Format ID: Attendance sheet
     */
    private static final String FORMAT_ID_PAGE_1 = "SYS-SPED-NH-N3A";

    /**
     * Parameter: Attendance record for an IEP meeting
     */
    public static final String PARAM_ATTENDANCE_RECORD = "attendanceRecord";

    /**
     * Parameter: Student's selected contact to be invited to the meeting
     */
    public static final String PARAM_CONTACT = "contact";

    /**
     * Parameter: SPED Team member
     */
    public static final String PARAM_TEAM_MEMBER = "teamMember";

    /**
     * Parameter: Verbal confirmation date
     */
    public static final String PARAM_VERBAL_CONF = "verbalConfDate";

    /**
     * Parameter: Written notice date
     */
    public static final String PARAM_WRITTEN_NOTICE = "writtenNoticeDate";

    private int m_currentPageNumber = 0;
    private Map m_subReports = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        loadSubReports();

        IepMeeting meeting = (IepMeeting) getFormStorage();

        prepareContacts();

        if (isBlank()) {
            preparePage1(grid);
            prepareExcusal(grid, new IepMeetingAttendance(getBroker().getPersistenceKey()));
        } else {
            /*
             * Build the parent invitation
             */
            preparePage1(grid);


            /*
             * Add meeting excusal forms
             */

            // Criteria to identify team members who are excused from an IEP meeting (fully or
            // partially)
            Criteria excusalCriteria = new Criteria();
            excusalCriteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
            excusalCriteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.valueOf(true));

            QueryByCriteria excusedQuery = new QueryByCriteria(IepMeetingAttendance.class, excusalCriteria);
            QueryIterator excusedAttendanceRecords = getBroker().getIteratorByQuery(excusedQuery);

            try {
                while (excusedAttendanceRecords.hasNext()) {
                    IepMeetingAttendance attendanceRecord = (IepMeetingAttendance) excusedAttendanceRecords.next();
                    if (attendanceRecord != null) {
                        prepareExcusal(grid, attendanceRecord);
                    }
                }
            } finally {
                excusedAttendanceRecords.close();
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
     * Loads each subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {FORMAT_ID_PAGE_1,
                FORMAT_ID_EXCUSAL}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

    /**
     * Adds a blank attendance entry to the passed collection.
     *
     * @param collection Collection<X2BaseBean>
     */
    private void prepareBlankAttendance(Collection<X2BaseBean> collection) {
        IepData iep = (IepData) getFormOwner();

        IepMeetingAttendance blankAttendance = new IepMeetingAttendance(getBroker().getPersistenceKey());
        blankAttendance.setStudentOid(iep.getStudentOid());
        blankAttendance.setIepDataOid(iep.getOid());

        for (int i = 0; i < 10; i++) {
            collection.add(blankAttendance);
        }
    }

    /**
     * Retrieves the student's contacts, and stores the first contact (sorted by emergency priority)
     * as a report parameter.
     */
    private void prepareContacts() {
        // Retrieve a reference to the form owner IEP
        IepData iep = (IepData) getFormOwner();

        // Query for the related student's contacts
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentContact.COL_STUDENT_OID, iep.getStudentOid());
        BeanQuery contactQuery = new BeanQuery(StudentContact.class, criteria, false);
        contactQuery.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);

        addParameter(PARAM_CONTACT, getBroker().getBeanByQuery(contactQuery));
    }

    /**
     * Prepares an excusal page.
     *
     * @param grid ReportDataGrid
     * @param attendanceRecord IepMeetingAttendance
     */
    private void prepareExcusal(ReportDataGrid grid, IepMeetingAttendance attendanceRecord) {
        HashMap<String, Object> parameterMap = new HashMap<String, Object>();

        parameterMap.put(PARAM_TEAM_MEMBER, attendanceRecord.getTeamMember());
        parameterMap.put(PARAM_ATTENDANCE_RECORD, attendanceRecord);

        parameterMap.putAll(getParameters());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_EXCUSAL));
        grid.set(COL_PARAMETER_MAP, parameterMap);
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "N3W");
    }

    /**
     * Prepares the first page (Notification of SPED Team Meeting).
     *
     * @param grid ReportDataGrid
     */
    private void preparePage1(ReportDataGrid grid) {
        IepMeeting meeting = (IepMeeting) getFormStorage();

        HashMap<String, Object> parameterMap = new HashMap<String, Object>();
        parameterMap.putAll(getParameters());

        Collection<X2BaseBean> attendance;
        if (isBlank()) {
            attendance = new ArrayList<X2BaseBean>(5);

            prepareBlankAttendance(attendance);
        } else {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());

            QueryByCriteria query = new QueryByCriteria(IepMeetingAttendance.class, criteria);
            query.addOrderByAscending(IepMeetingAttendance.REL_TEAM_MEMBER + "." + IepTeamMember.COL_NAME_VIEW);

            attendance = getBroker().getCollectionByQuery(query);
            if (attendance.size() == 0) {
                prepareBlankAttendance(attendance);
            }

            parameterMap.putAll(preparePage1Dates(meeting));
        }

        JRDataSource dataSource = new BeanCollectionDataSource(attendance,
                getDictionary(),
                getLocale());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(FORMAT_ID_PAGE_1));
        grid.set(COL_PARAMETER_MAP, parameterMap);
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "N3 A");
    }

    /**
     * Sets report parameters corresponding to dates stored via aliases. Retrieved here (in the Java
     * source) to permit re-formatting of date values. Returns a map containing the two parameters.
     *
     * @param meeting IepMeeting
     * @return HashMap<String, Object>
     */
    private HashMap<String, Object> preparePage1Dates(IepMeeting meeting) {
        HashMap<String, Object> parameterMap = new HashMap<String, Object>();

        SimpleDateFormat dateFormatter = new SimpleDateFormat(DEFAULT_DATE_FORMAT);

        Date verbalDate = null;
        Date writtenDate = null;

        try {
            String rawVerbalDate = (String) meeting.getFieldValueByAlias(ALIAS_VERBAL_CONF,
                    getDictionary());
            if (rawVerbalDate != null) {
                verbalDate = dateFormatter.parse(rawVerbalDate);
            }


            String rawWrittenDate = (String) meeting.getFieldValueByAlias(ALIAS_WRITTEN_NOTICE,
                    getDictionary());
            if (rawWrittenDate != null) {
                writtenDate = dateFormatter.parse(rawWrittenDate);
            }
        } catch (java.text.ParseException e) {
            // Date incorrectly stored. Catch the error so the rest of the report can print.
        }

        parameterMap.put(PARAM_VERBAL_CONF, verbalDate);
        parameterMap.put(PARAM_WRITTEN_NOTICE, writtenDate);

        return parameterMap;
    }
}
