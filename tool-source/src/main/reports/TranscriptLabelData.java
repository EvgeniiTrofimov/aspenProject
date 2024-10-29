/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.tools.reports.GradeReportJavaSource;
import com.x2dev.sis.tools.reports.TranscriptReportGrid;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Transcript Label" report.
 *
 * @author X2 Development Corporation
 */
public class TranscriptLabelData extends GradeReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    // Report parameters
    private static final String ABSENT_TOTAL = "absentTotal";
    private static final String DATE_PARAM = "date";
    private static final String TARDY_TOTAL = "tardyTotal";
    private static final String DISMISSED_TOTAL = "dismissedTotal";

    // Private variables
    private Map m_absentTotals;
    private Map m_dismissalTotals;
    private Map m_tardyTotals;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.GradeReportJavaSource#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        String[] studentSortOrder = getUserSortOrderAsStringArray((String) getParameter(STUDENT_SORT_PARAM));

        TranscriptReportGrid grid = new TranscriptReportGrid(m_transcriptCriteria,
                studentSortOrder,
                m_convertNumeric,
                m_convertReference,
                m_localizeReference,
                getOrganization(),
                getBroker());

        iterateGrid(grid);

        addDefaultParameters();

        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.reports.GradeReportJavaSource#addToInitialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        populateAttendanceTotals();
        addParameter(ABSENT_TOTAL, m_absentTotals);
        addParameter(TARDY_TOTAL, m_tardyTotals);
        addParameter(DISMISSED_TOTAL, m_dismissalTotals);
    }

    /**
     * Queries for attendance data and populates summary maps.
     */
    private void populateAttendanceTotals() {
        m_absentTotals = new HashMap(1000);
        m_tardyTotals = new HashMap(1000);
        m_dismissalTotals = new HashMap(1000);

        Criteria attendanceCriteria = new Criteria();
        PlainDate date = (PlainDate) getParameter(DATE_PARAM);

        attendanceCriteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());
        attendanceCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, date);
        attendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_context.getStartDate());

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, attendanceCriteria);
        query.addOrderByAscending(StudentAttendance.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);

        QueryIterator attendance = null;

        try {
            attendance = getBroker().getIteratorByQuery(query);
            int absent = 0;
            int tardy = 0;
            int dismissal = 0;
            String lastOid = null;

            while (attendance.hasNext()) {
                StudentAttendance currentAttendance = (StudentAttendance) attendance.next();
                if (lastOid == null || !currentAttendance.getStudent().getOid().equals(lastOid)) {
                    if (lastOid != null) {
                        m_absentTotals.put(lastOid, Integer.valueOf(absent));
                        m_tardyTotals.put(lastOid, Integer.valueOf(tardy));
                        m_dismissalTotals.put(lastOid, Integer.valueOf(dismissal));
                    }

                    absent = 0;
                    tardy = 0;
                    dismissal = 0;
                }

                if (currentAttendance.getAbsentIndicator()) {
                    absent++;
                }
                if (currentAttendance.getTardyIndicator()) {
                    tardy++;
                }
                if (currentAttendance.getDismissedIndicator()) {
                    dismissal++;
                }

                lastOid = currentAttendance.getStudent().getOid();
            }

            // Enter in last data
            m_absentTotals.put(lastOid, Integer.valueOf(absent));
            m_tardyTotals.put(lastOid, Integer.valueOf(tardy));
            m_dismissalTotals.put(lastOid, Integer.valueOf(dismissal));
        } finally {
            if (attendance != null) {
                attendance.close();
            }
        }
    }
}
