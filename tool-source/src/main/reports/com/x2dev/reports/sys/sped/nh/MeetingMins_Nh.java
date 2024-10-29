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

import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.business.sped.NewHampshireAliases;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the New Hampshire IEP Meeting Minutes form. This class prepares a
 * ReportDataGrid that contains a row for each section of the report. Each row contains a format and
 * a java source for the corresponding section. The following are the sections prepared:
 * <p>
 * <table border="1">
 * <tr>
 * <th>Section</th>
 * <th>Java source description</th>
 * </tr>
 * <tr>
 * <td>Meeting Determination</td>Queries for the related meeting, and provides a datasource
 * containing the attendees
 * <td></td>
 * </tr>
 * <tr>
 * <td>Meeting Summary</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * </table>
 *
 * @author X2 Development Corporation
 */
public class MeetingMins_Nh extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * ReportDataGrid Column ID: page number (String)
     */
    private static final String COL_PAGE_NUMBER = "pageNumber";

    /**
     * ReportDataGrid Column ID: page identifier (String)
     */
    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";

    /**
     * ReportDataGrid Column ID: parameter map
     */
    private static final String COL_PARAMETER_MAP = "parameters";

    /**
     * ReportDataGrid Column ID: datasource
     */
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";

    /**
     * ReportDataGrid Column ID: format
     */
    private static final String COL_SUBREPORT_FORMAT = "format";

    /**
     * Format ID: Meeting Minutes Page 1: Attendance and Determination
     */
    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-NH-M1";

    /**
     * Format ID: Meeting Minutes Page 2: Meeting Summary
     */
    private static final String PAGE_2_FORMAT_ID = "SYS-SPED-NH-M2";

    /**
     * Parameter: IEP Meeting determination (String)
     */
    private static final String PARAM_DETERMINATION = "meetingDetermination";

    /**
     * Parameter: IEP Meeting object (IepMeeting.class)
     */
    private static final String PARAM_MEETING = "iepMeeting";

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

        if (isBlank()) {
            preparePage1(grid, new IepMeeting(getBroker().getPersistenceKey()));
            preparePage2(grid);
        } else {
            FormInstance refForm = getFormInstance();

            // Retrieve the aliased field values
            String meetingDetermination =
                    (String) refForm.getFormValueByAlias(NewHampshireAliases.MEETING_DETERMINATION, getBroker());
            String meetingOid = (String) refForm.getFormValueByAlias(NewHampshireAliases.MEETING_OID, getBroker());

            // Retrieve the corresponding IepMeeting bean
            IepMeeting meeting = (IepMeeting) getBroker().getBeanByOid(IepMeeting.class, meetingOid);

            addParameter(PARAM_MEETING, meeting);
            addParameter(PARAM_DETERMINATION, meetingDetermination);

            preparePage1(grid, meeting);
            preparePage2(grid);
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
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {PAGE_1_FORMAT_ID,
                PAGE_2_FORMAT_ID}));

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
     * Prepares the first page (Meeting Determination)
     * Queries for the related meeting, and (if found) passes a collection of attendees for
     * that meeting as a data source.
     *
     * @param grid ReportDataGrid
     * @param relatedMeeting IepMeeting
     */
    private void preparePage1(ReportDataGrid grid, IepMeeting relatedMeeting) {
        Collection<X2BaseBean> attendance;
        if (isBlank() || relatedMeeting == null) {
            attendance = new ArrayList<X2BaseBean>(5);
            prepareBlankAttendance(attendance);
        } else {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, relatedMeeting.getOid());

            QueryByCriteria query = new QueryByCriteria(IepMeetingAttendance.class, criteria);
            query.addOrderByAscending(IepMeetingAttendance.REL_TEAM_MEMBER + "." + IepTeamMember.COL_NAME_VIEW);

            attendance = getBroker().getCollectionByQuery(query);
            if (attendance.size() == 0) {
                prepareBlankAttendance(attendance);
            }
        }

        JRDataSource dataSource = new BeanCollectionDataSource(attendance,
                getDictionary(),
                getLocale());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_1_FORMAT_ID));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "M1");
    }

    /**
     * Prepares a meeting summary page using a <code>SimpleFormDataSource</code>.
     *
     * @param grid ReportDataGrid
     */
    private void preparePage2(ReportDataGrid grid) {
        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_2_FORMAT_ID));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "M2");
    }
}
