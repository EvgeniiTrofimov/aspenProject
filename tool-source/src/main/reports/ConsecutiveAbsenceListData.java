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

import com.follett.fsc.core.framework.persistence.CollectionCriteriaHelper;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SecondaryStudentDataSource;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.ContextList;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the Consecutive Absence List report. This report lists students
 * with consecutive absence streaks greater than the given input parameter for count.
 *
 * @author X2 Development Corporation
 */
public class ConsecutiveAbsenceListData extends SecondaryStudentDataSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "attendance count" report parameter. The value is an Integer.
     */
    public static final String CONSECUTIVE_COUNT_PARAM = "count";

    /**
     * Report parameter name for the end of the date range. This value is a PlainDate object.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Report parameter name for the excluded excused parameter. The value is a Boolean.
     */
    public static final String EXCLUDE_EXCUSED_PARAM = "excludeExcused";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Report parameter name for the start of the date range. This value is a PlainDate object.
     */
    public static final String START_DATE_PARAM = "startDate";

    /*
     * Grid fields
     */
    public static final String FIELD_CONSECUTIVE = "consecutive";
    public static final String FIELD_END_LOOKUP = "dateRangeEndLookup";
    public static final String FIELD_HOMEROOM = "homeroom";
    public static final String FIELD_NAME = "name";
    public static final String FIELD_START_LOOKUP = "dateRangeStartLookup";
    public static final String FIELD_YOG = "yog";

    private Map m_calendarLookup;
    private ApplicationContext m_context;
    private int m_count;
    private ReportDataGrid m_grid;
    private String m_staffOid;
    private HashSet<String> m_stdOids = null;
    private CollectionCriteriaHelper m_studentCollectionCriteriaHelper;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        PlainDate startDate = (PlainDate) getParameter(START_DATE_PARAM);
        PlainDate endDate = (PlainDate) getParameter(END_DATE_PARAM);

        getBroker().beginTransaction();
        /*
         * Build the criteria based on the (possible) school and date range
         */
        X2Criteria criteria = new X2Criteria();

        if (m_staffOid != null && m_context != null && m_context.equals(ApplicationContext.STAFF)) {
            /*
             * For the staff view, use the list of student OIDs gathered from the current list
             */
            m_studentCollectionCriteriaHelper = new CollectionCriteriaHelper(m_stdOids, getBroker());
            m_studentCollectionCriteriaHelper.applyToCriteria(StudentAttendance.COL_STUDENT_OID, criteria);
        } else if (isSchoolContext()) {
            if (hasSpecifiedCriteria()) {
                criteria = StudentManager.getStudentObjectCriteria(getSchool(), StudentAttendance.COL_STUDENT_OID,
                        StudentAttendance.REL_STUDENT, StudentAttendance.COL_SCHOOL_OID,
                        ((Boolean) getParameter(INCLUDE_SECONDARY_STUDENTS)).booleanValue());
            } else {
                criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());
            }
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(StudentAttendance.class));
        }

        if (((Boolean) getParameter(EXCLUDE_EXCUSED_PARAM)).booleanValue()) {
            criteria.addEqualTo(StudentAttendance.COL_EXCUSED_INDICATOR, Boolean.valueOf(false));
        }

        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.valueOf(true));
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, startDate);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, endDate);

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        query.addOrderByAscending(StudentAttendance.COL_STUDENT_OID);
        query.addOrderByAscending(StudentAttendance.COL_DATE);

        Collection results = getBroker().getCollectionByQuery(query);

        formatConsecutiveAbsenceData(results);

        m_grid.beforeTop();

        if (m_studentCollectionCriteriaHelper != null) {
            m_studentCollectionCriteriaHelper.cleanup();
        }
        getBroker().commitTransaction(); // end transaction to dispose of temp table created in
                                         // buildCriteria

        return m_grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        EnrollmentManager enrollManager =
                new EnrollmentManager(getBroker(), getPrivilegeSet(), userData.getOrganization());
        PlainDate enrollStart = getCurrentContext().getStartDate();
        PlainDate enrollEnd = (PlainDate) getParameter(END_DATE_PARAM);

        m_calendarLookup = enrollManager.getCalendarLookup((SisSchool) getSchool(), enrollStart, enrollEnd);

        m_context =
                userData.getSessionNavConfig() == null ? null : userData.getSessionNavConfig().getApplicationContext();
        m_staffOid = userData.getStaffOid();

        /*
         * If this is the staff view, get a collection of student OIDs that the staff person has
         * in their current attendance list
         */
        if (m_staffOid != null && m_context != null && m_context.equals(ApplicationContext.STAFF)) {
            m_stdOids = new HashSet<String>();
            ContextList list = userData.getCurrentList();
            Criteria criteria = list.getCustomCriteria();
            ReportQueryByCriteria query =
                    new ReportQueryByCriteria(SisStudent.class, new String[] {X2BaseBean.COL_OID}, criteria);
            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    m_stdOids.add((String) row[0]);
                }
            } finally {
                iterator.close();
            }

        }

    }

    /**
     * Determines which records to add to the grid from the given results by comparing it to
     * the user input and then adds them and sorts the grid when finished.
     *
     * @param results Collection<StudentAttendance>
     */
    private void formatConsecutiveAbsenceData(Collection<StudentAttendance> results) {
        m_grid = new ReportDataGrid(6);
        m_count = ((Integer) getParameter(CONSECUTIVE_COUNT_PARAM)).intValue();

        StudentAttendance lastStudentRecord = null;
        PlainDate lastAttendanceDate = null;
        // The date in which a consecutive absence streak starts
        PlainDate dateFrom = null;
        int consecutiveAbsent = 0;

        for (StudentAttendance record : results) {
            // Checks to see if the student being looked at is different than the last record
            if (lastStudentRecord == null || !lastStudentRecord.getStudent().equals(record.getStudent())) {
                if (lastStudentRecord != null && consecutiveAbsent >= m_count) {
                    setGrid(lastStudentRecord, consecutiveAbsent, dateFrom, lastAttendanceDate);
                }

                dateFrom = record.getDate();

                consecutiveAbsent = 1;
            } else // If there is a repeated student in results.
            {
                if (DateUtils.isDayBefore(lastAttendanceDate, record.getDate(),
                        ((Map<String, Set<PlainDate>>) m_calendarLookup).get(record.getStudent().getCalendarCode()))) {
                    consecutiveAbsent++;
                } else {
                    /*
                     * This is for the special case when a student has a non-consecutive absence
                     * record, following a streak of absence records that meet the user input
                     * requirements. In this case, we will store the streak as it's own row for the
                     * report and reset the count to check for more consecutive absences. (Hence
                     * there can be more than one row in the report for the same student if he/she
                     * has two seperate absence streaks.
                     */
                    if (consecutiveAbsent >= m_count) {
                        setGrid(lastStudentRecord, consecutiveAbsent, dateFrom, lastAttendanceDate);
                    }

                    dateFrom = record.getDate();
                    consecutiveAbsent = 1;
                }
            }

            lastStudentRecord = record;
            lastAttendanceDate = record.getDate();
        }

        // Sets data for the last student of the results set
        if (lastStudentRecord != null && consecutiveAbsent >= m_count) {
            setGrid(lastStudentRecord, consecutiveAbsent, dateFrom, lastAttendanceDate);
        }

        sortGrid();
    }

    /**
     * Builds the sort based on user input.
     */
    private void sortGrid() {
        ArrayList<String> sortColumns = new ArrayList<String>(3);
        ArrayList<Boolean> sortOrders = new ArrayList<Boolean>(3);
        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        switch (sort) {
            case 1: // YOG
                sortColumns.add(FIELD_YOG);
                sortOrders.add(Boolean.valueOf(true));
                break;

            case 2: // Hrm
                sortColumns.add(FIELD_HOMEROOM);
                sortOrders.add(Boolean.valueOf(true));
                break;

            case 3: // Consecutive
                sortColumns.add(FIELD_CONSECUTIVE);
                sortOrders.add(Boolean.valueOf(false));
                break;
        }
        sortColumns.add(FIELD_NAME);
        sortOrders.add(Boolean.valueOf(true));
        sortColumns.add(FIELD_START_LOOKUP);
        sortOrders.add(Boolean.valueOf(true));

        m_grid.sort(sortColumns, sortOrders, false);
    }

    /**
     * Sets the grid with the given data.
     *
     * @param record StudentAttendance
     * @param consecutiveAbsent int
     * @param dateFrom PlainDate
     * @param dateUntil PlainDate
     */
    private void setGrid(StudentAttendance record, int consecutiveAbsent, PlainDate dateFrom, PlainDate dateUntil) {
        m_grid.append();
        m_grid.set(FIELD_NAME, record.getStudent().getNameView());
        m_grid.set(FIELD_HOMEROOM, record.getStudent().getHomeroom());
        m_grid.set(FIELD_YOG, Integer.valueOf(record.getStudent().getYog()));
        m_grid.set(FIELD_CONSECUTIVE, Integer.valueOf(consecutiveAbsent));
        m_grid.set(FIELD_START_LOOKUP, dateFrom);
        m_grid.set(FIELD_END_LOOKUP, dateUntil);
    }
}
