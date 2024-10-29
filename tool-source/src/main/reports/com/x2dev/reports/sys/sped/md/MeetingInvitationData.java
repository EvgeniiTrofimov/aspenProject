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
package com.x2dev.reports.sys.sped.md;

import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Maryland IEP meeting invitation form. This class prepares a
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
 * <p>
 * A letter and attendance sheet is printed for each team member who is both invited and designated
 * to receive an invitation. A waiver is printed for each team member who is either not invited or
 * excused.
 *
 * @author X2 Development Corporation
 */
public class MeetingInvitationData extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    // Constants for the letter (N3I) portion of the report
    public static final String TO_PARAMETER = "to";

    // ReportDataGrid column constants for the main report
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";
    private static final String COL_PAGE_NUMBER = "pageNumber";
    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";
    private static final String COL_PARAMETER_MAP = "parameters";

    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-MD-INV-P1";
    private static final String PAGE_2_FORMAT_ID = "SYS-SPED-MD-INV-P2";

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

        preparePage1(grid);
        preparePage2(grid);

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
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {PAGE_1_FORMAT_ID,
                PAGE_2_FORMAT_ID}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

    /**
     * Prepares the first page (N3).
     *
     * @param grid ReportDataGrid
     */
    private void preparePage1(ReportDataGrid grid) {
        JRDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_1_FORMAT_ID));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "P1");
    }

    /**
     * Prepares the second page (N3 A).
     *
     * @param grid ReportDataGrid
     */
    private void preparePage2(ReportDataGrid grid) {
        IepMeeting meeting = (IepMeeting) getFormStorage();

        Collection<IepMeetingAttendance> attendance = null;

        if (isBlank()) {
            int blankEntries = 5;

            attendance = new ArrayList<IepMeetingAttendance>(blankEntries);

            for (int i = 0; i < blankEntries; i++) {
                IepMeetingAttendance attendanceEntry = new IepMeetingAttendance(getBroker().getPersistenceKey());
                attendanceEntry.setIepMeetingOid(meeting.getOid());
                attendanceEntry.setIepDataOid(meeting.getIepDataOid());

                attendance.add(attendanceEntry);
            }
        } else {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
            criteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.valueOf(false));

            QueryByCriteria query = new QueryByCriteria(IepMeetingAttendance.class, criteria);
            query.addOrderByAscending(IepMeetingAttendance.REL_TEAM_MEMBER + "." + IepTeamMember.COL_NAME_VIEW);

            attendance = getBroker().getCollectionByQuery(query);
        }

        JRDataSource dataSource = new BeanCollectionDataSource(attendance,
                getDictionary(),
                getLocale());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_2_FORMAT_ID));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "P2");
    }
}
