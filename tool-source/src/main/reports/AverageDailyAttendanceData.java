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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SecondaryStudentDataSource;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source for the "Average Daily Attendance" report.
 *
 * @author X2 Development Corporation
 */
public class AverageDailyAttendanceData extends SecondaryStudentDataSource {
    private static final long serialVersionUID = 1L;
    private static final String END_DATE_PARAM = "endDate";
    private static final String START_DATE_PARAM = "startDate";

    // Field constants
    private static final String FIELD_DAYS_ENROLLED = "daysEnrolled";
    private static final String FIELD_DAYS_EXCUSED = "daysExcused";
    private static final String FIELD_DAYS_NOT_ENROLLED = "daysNotEnrolled";
    private static final String FIELD_DAYS_NOT_EXCUSED = "daysNotExcused";
    private static final String FIELD_DAYS_IN_SESSION = "daysInSession";
    private static final String FIELD_DAYS_PRESENT = "daysPresent";
    private static final String FIELD_ENROLLED_ON_LAST_DAY = "enrolledOnLastDay";
    private static final String FIELD_ENROLLMENT_BEAN = "enrollment";
    private static final String FIELD_ENROLLMENT_DATE = "enrDate";
    private static final String FIELD_REPORT_STATUS = "reportStatus";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_WITHDRAWAL_DATE = "wdrDate";

    // Paramater constants
    private static final String PARAMETER_DAY_COUNT = "dayCount";

    public static final int INITIAL_STUDENT_CAPACITY = 3000;

    // Enrollment report status constants
    private static final int REPORT_STATUS_ADDED = 100;
    private static final int REPORT_STATUS_ADDED_DROPPED = 200;
    private static final int REPORT_STATUS_DROPPED = 300;
    private static final int REPORT_STATUS_DROPPED_ADDED = 400;
    private static final int REPORT_STATUS_WITHDREW_PRIOR = 500;

    private Map m_attendanceData = null;
    private Calendar m_calendar = null;
    private Map m_calendarData = null;
    private CalendarManager m_calendarManager = null;
    private Map m_enrollmentData = null;
    private EnrollmentManager m_enrollmentManager = null;
    private PlainDate m_endDate = null;
    private Set m_priorEnrollments = null;
    private PlainDate m_startDate = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_startDate = (PlainDate) getParameter(START_DATE_PARAM);
        m_endDate = (PlainDate) getParameter(END_DATE_PARAM);

        ReportDataGrid grid = new ReportDataGrid(INITIAL_STUDENT_CAPACITY, 15);

        loadEnrollmentData();
        loadAttendanceData();

        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, getStudentCriteria());
        if (includeSecondaryStudents()) {
            studentQuery.setDistinct(true);
        }
        studentQuery.addOrderByAscending(SisStudent.COL_GRADE_LEVEL);
        studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        PlainDate contextStartDate = getOrganization().getCurrentContext().getStartDate();

        QueryIterator students = null;
        try {
            students = getBroker().getIteratorByQuery(studentQuery);
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                // include students who have been a member of this school from the beginning of the
                // year until the report end date
                if (m_enrollmentManager.getMembershipDays(student, contextStartDate, m_endDate, false) > 0) {
                    grid.append();
                    grid.set(FIELD_STUDENT, student);

                    populateEnrollmentCounts(grid);
                    populateAttendanceCounts(grid);
                }

            }
        } finally {
            if (students != null) {
                students.close();
            }
        }

        populateSchoolDayCount();

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_calendar = Calendar.getInstance(getLocale());
        m_calendarManager = new CalendarManager(getBroker());
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();

        m_attendanceData.clear();
        m_attendanceData = null;

        m_calendarData.clear();
        m_calendarData = null;

        m_enrollmentData.clear();
        m_enrollmentData = null;

        m_priorEnrollments.clear();
        m_priorEnrollments = null;
    }

    /**
     * Returns a Criteria that finds all enrollment records containing schools being reported
     * for the report date range.
     *
     * @param schoolOids Collection
     * @param startDate
     * @return Criteria
     */
    private Criteria getEnrollmentCriteria(Collection schoolOids, PlainDate startDate) {
        Criteria criteria = new Criteria();
        criteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_endDate);
        criteria.addIn(StudentEnrollment.COL_SCHOOL_OID, schoolOids);

        X2Criteria typeCriteria = new X2Criteria();
        typeCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        typeCriteria.addOrEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);

        criteria.addAndCriteria(typeCriteria);

        return criteria;
    }

    /**
     * Returns a Criteria that finds all students for this report.
     * <ul>
     * <li>The student's school OID is that of the school being reported, OR
     * <li>An enrollment record exists during the date range for the student containing the current
     * school OID being reported OR
     * <li>Any secondary students for the school if records for secondary students are included
     * <ul>
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        Criteria studentCriteria = new Criteria();
        studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        studentCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                SisStudent.COL_ENROLLMENT_STATUS));

        Collection<String> schoolOids = new ArrayList<String>();
        schoolOids.add(getSchool().getOid());

        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addIn(X2BaseBean.COL_OID, new SubQuery(StudentEnrollment.class,
                StudentEnrollment.COL_STUDENT_OID,
                getEnrollmentCriteria(schoolOids, getOrganization().getCurrentContext().getStartDate())));

        studentCriteria.addOrCriteria(enrollmentCriteria);

        /*
         * Check if secondary students are included.
         */
        if (includeSecondaryStudents()) {
            studentCriteria.addOrCriteria(StudentManager.buildSecondaryStudentCriteria(getSchool()));
        }

        return studentCriteria;
    }

    /**
     * Loads the map of attendance beans grouped by student OID.
     */
    private void loadAttendanceData() {
        Criteria criteria = new Criteria();
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_startDate);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_endDate);

        if (isSchoolContext()) {
            if (hasSpecifiedCriteria()) {
                criteria.addAndCriteria(getStudentObjectCriteria(StudentAttendance.COL_STUDENT_OID,
                        StudentAttendance.REL_STUDENT,
                        StudentAttendance.COL_SCHOOL_OID));
            } else {
                criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());
            }
        }

        QueryByCriteria attendanceQuery = new QueryByCriteria(StudentAttendance.class, criteria);
        attendanceQuery.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);

        m_attendanceData = getBroker().getGroupedCollectionByQuery(attendanceQuery,
                StudentAttendance.COL_STUDENT_OID, INITIAL_STUDENT_CAPACITY);
    }

    /**
     * Loads enrollment data required by this report including the map of calendar in session days
     * and the map of enrollment data grouped by student OID.
     */
    private void loadEnrollmentData() {
        DistrictSchoolYearContext context = m_calendarManager.getDistrictContext(m_endDate, getOrganization().getOid());
        if (context != null) {
            m_calendarData = m_enrollmentManager.getCalendarLookup((SisSchool) getSchool(), m_startDate, m_endDate,
                    context.getOid());
        } else {
            m_calendarData = m_enrollmentManager.getCalendarLookup((SisSchool) getSchool(), m_startDate, m_endDate,
                    getCurrentContext().getOid());
        }

        // Get the set of OIDs identifying students enrolled on the start date
        m_priorEnrollments = m_enrollmentManager.getMembershipAsOf(m_startDate, (SisSchool) getSchool());

        QueryByCriteria query =
                new QueryByCriteria(StudentEnrollment.class, getEnrollmentCriteria(getSchoolOids(), m_startDate));
        query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
        query.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);

        m_enrollmentData =
                getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID,
                        INITIAL_STUDENT_CAPACITY);
    }

    /**
     * Adds a report parameter containing the number of school days to be displayed on the header
     * of the report. The calendar used to determine the count is the calendar associated with the
     * most students on the report. Since only one count appears, it is possible that this number
     * may not apply to some students associated with different calendars.
     */
    private void populateSchoolDayCount() {
        int dayCount = 0;

        String calendarId = null;

        String countColumn = "count(" + SisStudent.COL_CALENDAR_CODE + ") as cnt ";

        ReportQueryByCriteria calendarIdQuery = new ReportQueryByCriteria(SisStudent.class,
                new String[] {SisStudent.COL_CALENDAR_CODE, countColumn},
                getStudentCriteria());
        calendarIdQuery.addGroupBy(SisStudent.COL_CALENDAR_CODE);
        // This works with OJB because the column is converted to a number in the SQL order by
        // clause
        calendarIdQuery.addOrderByDescending(countColumn);

        ReportQueryIterator calendarIdIterator = null;
        try {
            calendarIdIterator = getBroker().getReportQueryIteratorByQuery(calendarIdQuery);
            if (calendarIdIterator.hasNext()) {
                Object[] row = (Object[]) calendarIdIterator.next();
                calendarId = (String) row[0];
            }
        } finally {
            if (calendarIdIterator != null) {
                calendarIdIterator.close();
            }
        }

        if (calendarId != null) {
            Criteria dayCountCriteria = new Criteria();
            dayCountCriteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." +
                    SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
            dayCountCriteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." +
                    SchoolCalendar.COL_CALENDAR_ID, calendarId);
            dayCountCriteria.addGreaterOrEqualThan(SisSchoolCalendarDate.COL_DATE, m_startDate);
            dayCountCriteria.addLessOrEqualThan(SisSchoolCalendarDate.COL_DATE, m_endDate);
            dayCountCriteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

            QueryByCriteria query = new QueryByCriteria(SisSchoolCalendarDate.class, dayCountCriteria);

            dayCount = getBroker().getCount(query);
        }

        addParameter(PARAMETER_DAY_COUNT, Integer.valueOf(dayCount));
    }

    /**
     * Sets the counts of days present, excused, and unexcused for the current student in the passed
     * grid.
     *
     * @param grid ReportDataGrid
     */
    private void populateAttendanceCounts(ReportDataGrid grid) {
        SisStudent student = (SisStudent) grid.get(FIELD_STUDENT);

        Set sessionDates = (Set) m_calendarData.get(student.getCalendarCode());
        if (sessionDates == null) {
            sessionDates = new HashSet();
        }

        double notPresent = 0;
        double notPresentExcused = 0;

        List attendanceList = (List) m_attendanceData.get(student.getOid());
        if (attendanceList != null) {
            Iterator attendanceIterator = attendanceList.iterator();
            while (attendanceIterator.hasNext()) {
                StudentAttendance attendance = (StudentAttendance) attendanceIterator.next();

                if (sessionDates.contains(attendance.getDate())) {
                    if (attendance.getAbsentIndicator()) {
                        double portionAbsent =
                                attendance.getPortionAbsent() != null ? attendance.getPortionAbsent().doubleValue() : 0;

                        notPresent += portionAbsent;

                        if (attendance.getExcusedIndicator()) {
                            notPresentExcused += portionAbsent;
                        }
                    }
                }
            }
        }

        Integer daysEnrolled = (Integer) grid.get(FIELD_DAYS_ENROLLED);

        grid.set(FIELD_DAYS_PRESENT, Double.valueOf(daysEnrolled.doubleValue() - notPresent));
        grid.set(FIELD_DAYS_EXCUSED, Double.valueOf(notPresentExcused));
        grid.set(FIELD_DAYS_NOT_EXCUSED, Double.valueOf(notPresent - notPresentExcused));
    }

    /**
     * Sets the counts of days enrolled and days not enrolled for the current student in the passed
     * grid.
     *
     * @param grid ReportDataGrid
     */
    private void populateEnrollmentCounts(ReportDataGrid grid) {
        SisStudent student = (SisStudent) grid.get(FIELD_STUDENT);

        int reportStatus = 0;
        boolean enrolled = m_priorEnrollments.contains(student.getOid());
        if (!enrolled) {
            reportStatus = REPORT_STATUS_WITHDREW_PRIOR;
        }

        int daysEnrolled = 0;
        int daysInSession = 0;

        HashSet entryDates = new HashSet(5);
        HashSet withdrawalDates = new HashSet(5);
        Set sessionDates = (Set) m_calendarData.get(student.getCalendarCode());
        if (sessionDates == null) {
            sessionDates = new HashSet();
        }

        List enrollments = (List) m_enrollmentData.get(student.getOid());
        if (enrollments != null) {
            StudentEnrollment entry = null;
            StudentEnrollment withdrawal = null;
            Iterator enrollmentIterator = enrollments.iterator();
            while (enrollmentIterator.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) enrollmentIterator.next();
                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                    entryDates.add(enrollment.getEnrollmentDate());
                    if (reportStatus == REPORT_STATUS_DROPPED || reportStatus == REPORT_STATUS_ADDED_DROPPED) {
                        reportStatus = REPORT_STATUS_DROPPED_ADDED;
                    } else {
                        reportStatus = REPORT_STATUS_ADDED;
                    }
                    entry = enrollment;
                } else if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                    withdrawalDates.add(enrollment.getEnrollmentDate());
                    if (reportStatus == REPORT_STATUS_ADDED || reportStatus == REPORT_STATUS_DROPPED_ADDED) {
                        reportStatus = REPORT_STATUS_ADDED_DROPPED;
                    } else {
                        reportStatus = REPORT_STATUS_DROPPED;
                    }
                    withdrawal = enrollment;
                }
            }

            PlainDate entryDate = null;
            if (entry != null) {
                entryDate = entry.getEnrollmentDate();
            }
            PlainDate withdrawalDate = null;
            if (withdrawal != null) {
                withdrawalDate = withdrawal.getEnrollmentDate();
            }

            if (entryDate != null) {
                grid.set(FIELD_ENROLLMENT_DATE, entryDate);
                grid.set(FIELD_ENROLLMENT_BEAN, entry);
            }

            if (withdrawalDate != null) {
                grid.set(FIELD_WITHDRAWAL_DATE, withdrawalDate);
                if (entryDate == null || !withdrawalDate.before(entryDate)) {
                    grid.set(FIELD_ENROLLMENT_BEAN, withdrawal);
                }
            }
        }

        m_calendar.setTimeInMillis(m_startDate.getTime());
        while (!m_calendar.getTime().after(m_endDate)) {
            boolean entryDate = entryDates.contains(m_calendar.getTime());
            boolean withdrawalDate = withdrawalDates.contains(m_calendar.getTime());

            boolean inSession = sessionDates.contains(m_calendar.getTime());

            if (entryDate) {
                if (m_enrollmentManager.getEntryIsMemberDay() && inSession) {
                    daysEnrolled++;
                }
                enrolled = true;
            }
            if (withdrawalDate) {
                if (m_enrollmentManager.getWithdrawalIsMemberDay() && inSession) {
                    daysEnrolled++;
                }
                enrolled = false;
            }
            if (enrolled && !entryDate && !withdrawalDate && inSession) {
                daysEnrolled++;
            }

            if (inSession) {
                daysInSession++;
            }

            m_calendar.add(Calendar.DAY_OF_YEAR, 1);
        }

        grid.set(FIELD_DAYS_ENROLLED, Integer.valueOf(daysEnrolled));
        grid.set(FIELD_DAYS_IN_SESSION, Integer.valueOf(daysInSession));
        grid.set(FIELD_DAYS_NOT_ENROLLED, Integer.valueOf(daysInSession - daysEnrolled));
        grid.set(FIELD_ENROLLED_ON_LAST_DAY, Boolean.valueOf(enrolled));
        grid.set(FIELD_REPORT_STATUS, Integer.valueOf(reportStatus));
    }
}
