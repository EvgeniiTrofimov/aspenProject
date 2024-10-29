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
package com.x2dev.reports.sys.sped.md;

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Wicomico County special education meeting sign in sheet.
 *
 * @author X2 Development Corporation
 */
public class MeetingSignInSheetData extends MultiPageFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    // ReportDataGrid column constants for the main report
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";
    private static final String COL_PAGE_NUMBER = "pageNumber";
    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";
    private static final String COL_PARAMETER_MAP = "parameters";

    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-MD-MTG-S";
    private static final String PAGE_2_FORMAT_ID = "SYS-SPED-MD-MTG-RFR";

    /**
     * Prepare page.
     *
     * @param grid ReportDataGrid
     * @see
     *      com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#preparePage(com.follett.
     *      fsc.core.k12.tools.reports.ReportDataGrid)
     */
    @Override
    protected void preparePage(ReportDataGrid grid) {
        if (getCurrentPageNumber() == 1) {
            Criteria attendanceCriteria = new Criteria();
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, getFormStorage().getOid());
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.valueOf(false));

            QueryByCriteria attendanceQuery = new QueryByCriteria(IepMeetingAttendance.class, attendanceCriteria);
            attendanceQuery
                    .addOrderByAscending(IepMeetingAttendance.REL_TEAM_MEMBER + "." + IepTeamMember.COL_NAME_VIEW);

            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey());

            JRDataSource dataSource = new QueryIteratorDataSource(getBroker().getIteratorByQuery(attendanceQuery),
                    dictionary, true, getLocale());

            grid.append();
            grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_1_FORMAT_ID));
            grid.set(COL_PARAMETER_MAP, getParameters());
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(getCurrentPageNumber()));
            grid.set(COL_PAGE_IDENTIFIER, "1");
        } else {
            super.preparePage(grid);
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#getFormatIds()
     */
    @Override
    protected String[] getFormatIds() {
        return new String[] {PAGE_1_FORMAT_ID, PAGE_2_FORMAT_ID};
    }
}
