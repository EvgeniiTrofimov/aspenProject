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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.StaffPostAttendance;
import com.x2dev.utils.types.PlainDate;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Class Attendance Post History" report. This report is
 * intended for use in the school view, Class History. It finds all attendance
 * posts within a certain date range for a specific class section.
 * 
 * @author X2 Development Corporation
 */
public class ClassAttendancePostHistoryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Parameter used to specify the end date for the date range used to
     * narrow which attendance posts are returned for the specific class section
     */
    public static final String END_DATE_INPUT = "endDate";

    /**
     * Parameter used to specify the section oid used to narrow which
     * attendance posts to look at
     */
    public static final String SECTION_OID_INPUT = "sectionOid";

    /**
     * Parameter used to specify the sort order for the query
     */
    public static final String SORT_INPUT = "sort";

    /**
     * Parameter used to specify whether to use ascending or descending sort
     */
    public static final String SORT_ORDER_INPUT = "sortOrder";

    /**
     * Parameter used to specify the start date for the date range used to
     * narrow which attendance posts are returned for the specific class section
     */
    public static final String START_DATE_INPUT = "startDate";


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        // Grab input parameters
        PlainDate startDate = (PlainDate) getParameter(START_DATE_INPUT);
        PlainDate endDate = (PlainDate) getParameter(END_DATE_INPUT);
        String sectionOid = (String) getParameter(SECTION_OID_INPUT);
        int sort = ((Integer) getParameter(SORT_INPUT)).intValue();
        boolean sortOrder = ((Boolean) getParameter(SORT_ORDER_INPUT)).booleanValue();

        // Build Class Attendance Post Verification query
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StaffPostAttendance.COL_ATTENDANCE_TYPE,
                Integer.valueOf(StaffPostAttendance.PERIOD_ATTENDANCE));
        criteria.addEqualTo(StaffPostAttendance.COL_MASTER_SCHEDULE_OID, sectionOid);
        criteria.addGreaterOrEqualThan(StaffPostAttendance.COL_DATE, startDate);
        criteria.addLessOrEqualThan(StaffPostAttendance.COL_DATE, endDate);

        QueryByCriteria query = createQueryByCriteria(StaffPostAttendance.class, criteria);

        // Date, Post Count, or Period # sort
        // In either ascending or descending order
        switch (sort) {
            case 0:
                query.addOrderBy(StaffPostAttendance.COL_DATE, sortOrder);
                break;
            case 1:
                query.addOrderBy(StaffPostAttendance.COL_POST_COUNT, sortOrder);
                query.addOrderBy(StaffPostAttendance.COL_DATE, sortOrder);
                break;
            case 2:
                query.addOrderBy(StaffPostAttendance.COL_PERIOD_VIEW, sortOrder);
                query.addOrderBy(StaffPostAttendance.COL_DATE, sortOrder);
                break;
        }

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }
}
