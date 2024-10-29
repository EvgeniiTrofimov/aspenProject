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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.Selection;
import com.follett.fsc.core.k12.beans.SelectionObject;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SecondaryStudentDataSource;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source for the Monthly Register report.
 *
 * @author X2 Development Corporation
 */
public class MonthlyRegisterData extends SecondaryStudentDataSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * School year context
     */
    public static final String CONTEXT_OID_PARAM = "contextOid";

    /**
     * Last date in the report date range
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * First YOG in the YOG range
     */
    public static final String YOG_FIRST_PARAM = "firstYog";

    /**
     * First date in the report date range
     */
    public static final String START_DATE_PARAM = "startDate";

    /**
     * Boolean for whether to display only the totals of the report.
     */
    public static final String TOTALS_ONLY_PARAM = "totalsOnly";

    /**
     * Last YOG in the YOG range
     */
    public static final String YOG_LAST_PARAM = "lastYog";

    private static final int INITIAL_COLUMN_SIZE = 35;
    private static final int INITIAL_ROW_SIZE = 1500;

    // Attendance codes
    private static final String CODE_ABSENT = "A";
    private static final String CODE_TARDY = "T";
    private static final String CODE_DISMISSED = "D";

    private static final String COLUMN_ENROLLMENT_TYPE = "enrollment_"; // Prefix; followed by a
                                                                        // cell #
    private static final String COLUMN_MONTH = "month";
    private static final String COLUMN_STUDENT = "student";
    private static final String COLUMN_STUDENT_OID = "studentOid";

    // Columns for monthly totals
    private static final String COLUMN_MONTHLY_ABSENT = "ma";
    private static final String COLUMN_MONTHLY_DAYS_IN_SESSION = "mds";
    private static final String COLUMN_MONTHLY_DISMISSED = "md";
    private static final String COLUMN_MONTHLY_NONMEMBER = "mn";
    private static final String COLUMN_MONTHLY_PRESENT = "mp";
    private static final String COLUMN_MONTHLY_TARDY = "mt";

    // Columns for year-to-date totals
    private static final String COLUMN_YTD_ABSENT = "ya";
    private static final String COLUMN_YTD_DAYS_IN_SESSION = "yds";
    private static final String COLUMN_YTD_DISMISSED = "yd";
    private static final String COLUMN_YTD_NONMEMBER = "yn";
    private static final String COLUMN_YTD_PRESENT = "yp";
    private static final String COLUMN_YTD_TARDY = "yt";

    private static final int DATE_COLUMNS = 25;

    // Columns for parameters used in the design
    private static final String PARAM_MONTH_DAY_LOOKUP = "days";
    private static final String PARAM_MONTH_NAME_LOOKUP = "monthNames";
    private static final String PARAM_TOTALS_ONLY = "totalsOnly";

    // Symbols
    private static final String SYMBOL_NONMEMBER = "--";
    private static final String SYMBOL_NONSESSION = "X";

    private Calendar m_calendar;
    private Map m_cellToDay;
    private Map m_dayToCell;
    private ReportDataGrid m_grid;
    private SimpleDateFormat m_lookupFormat;
    private PlainDate m_reportEndDate;
    private PlainDate m_reportStartDate;
    private PlainDate m_schoolEndDate;
    private PlainDate m_schoolStartDate;
    private Integer m_yogFirst;
    private Integer m_yogLast;

    /**
     * Constructs a ReportDataGrid containing monthly register data. The grid contains a row for
     * each student for each month in the date range, and the following columns:
     * <ul>
     * <li>The month/year stored as a String in the following format: YYYYMM
     * <li>Student object
     * <li>Student OID
     * <li>Columns 1-31, corresponding to the day of the month, containing the register detail
     * <li>Enrollment columns enrollment_1 - enrollment_31, containing null, "E", or "W"
     * <li>Monthly totals columns, corresponding to constants prefixed with COLUMN_MONTHLY_
     * <li>Year-to-date totals columns, corresponding to constants prefixed with COLUMN_YTD_
     * </ul>
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        if (m_reportStartDate != null && m_reportEndDate != null &&
                m_schoolStartDate != null && m_schoolEndDate != null) {
            // Force the report start & end dates within the school year
            if (m_reportStartDate.before(m_schoolStartDate)) {
                m_reportStartDate = m_schoolStartDate;
            }
            if (m_reportEndDate.after(m_schoolEndDate)) {
                m_reportEndDate = m_schoolEndDate;
            }

            if (!m_reportEndDate.before(m_reportStartDate)) {
                List monthRanges = getMonthRanges();

                loadCellLookups(monthRanges);

                Iterator ranges = monthRanges.iterator();
                while (ranges.hasNext()) {
                    KeyValuePair range = (KeyValuePair) ranges.next();

                    String monthId = getMonthId((PlainDate) range.getKey());

                    populateStudents(monthId);
                    populateAttendance(range);
                    populateEnrollment(range);
                }

                populateMembershipStatus();
                populateNonSessionDays();
                populateTotals();

                truncateGrid();

                addParameter(PARAM_MONTH_NAME_LOOKUP, getMonthNames());
                addParameter(PARAM_MONTH_DAY_LOOKUP, m_cellToDay);
            }

        }

        m_grid.beforeTop();

        // Add report parameters
        Boolean totalsOnly = (Boolean) getParameter(TOTALS_ONLY_PARAM);
        addParameter(PARAM_TOTALS_ONLY, totalsOnly);

        return m_grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_calendar = Calendar.getInstance(getLocale());

        m_grid = new ReportDataGrid(INITIAL_ROW_SIZE, INITIAL_COLUMN_SIZE);

        m_lookupFormat = new SimpleDateFormat("yyyyMMdd");

        m_reportStartDate = (PlainDate) getParameter(START_DATE_PARAM);
        m_reportEndDate = (PlainDate) getParameter(END_DATE_PARAM);

        String contextOid = (String) getParameter(CONTEXT_OID_PARAM);

        Criteria schoolScheduleCriteria = new Criteria();
        schoolScheduleCriteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, contextOid);
        schoolScheduleCriteria.addEqualTo(SchoolScheduleContext.COL_SCHOOL_OID, getSchool().getOid());

        QueryByCriteria query = new QueryByCriteria(SchoolScheduleContext.class, schoolScheduleCriteria);
        Schedule schedule = null;

        Collection schoolScheduleContexts = getBroker().getCollectionByQuery(query);

        Iterator iterator = schoolScheduleContexts.iterator();
        while (iterator.hasNext() && schedule == null) {
            SchoolScheduleContext scheduleContext = (SchoolScheduleContext) iterator.next();
            if (scheduleContext.getActiveSchedule() != null) {
                schedule = scheduleContext.getActiveSchedule();
            }
        }

        if (schedule != null) {
            m_schoolStartDate = schedule.getStartDate();
            m_schoolEndDate = schedule.getEndDate();
        }

        Integer firstYog = (Integer) getParameter(YOG_FIRST_PARAM);
        Integer lastYog = (Integer) getParameter(YOG_LAST_PARAM);

        if (firstYog != null) {
            m_yogFirst = firstYog;
        } else {
            m_yogFirst = Integer.valueOf(0);
        }

        if (lastYog != null) {
            m_yogLast = lastYog;
        } else {
            m_yogLast = Integer.valueOf(9999);
        }
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();

        if (m_cellToDay != null) {
            m_cellToDay.clear();
            m_cellToDay = null;
        }

        if (m_dayToCell != null) {
            m_dayToCell.clear();
            m_dayToCell = null;
        }

        if (m_grid != null) {
            m_grid.clear();
            m_grid = null;
        }
    }

    /**
     * Returns the ID of the cell corresponding to the passed month ID and day of month, null if
     * one does not exist.
     *
     * @param monthId String
     * @param dayOfMonth int
     * @return String
     */
    private String getCellId(String monthId, int dayOfMonth) {
        return (String) m_dayToCell.get(monthId + "_" + dayOfMonth);
    }

    /**
     * Returns the date corresponding to the passed month ID and cell number, null if one does not
     * exist.
     *
     * @param monthId String
     * @param cell int
     * @return Date
     */
    private Date getCellDate(String monthId, int cell) {
        /*
         * Parse the month ID to get the cell day. The month ID is a String formatted YYYYMM where
         * MM is a number from 0 - 11
         */
        String year = monthId.substring(0, 4);
        String month = StringUtils.padLeft(String.valueOf(Integer.parseInt(monthId.substring(4)) + 1), 2, '0');
        String day = getCellDay(monthId, cell);

        String date = year + month + day;
        Date dateParsed;
        try {
            dateParsed = m_lookupFormat.parse(date);
        } catch (ParseException e) {
            dateParsed = null;
        }

        return dateParsed;
    }

    /**
     * Returns the day of month corresponding to the passed month ID and cell number, null if one
     * does not exist.
     *
     * @param monthId String
     * @param cell int
     * @return String
     */
    private String getCellDay(String monthId, int cell) {
        return (String) m_cellToDay.get(monthId + "_" + cell);
    }

    /**
     * Returns the enrollment criteria to use for retrieving data. The Criteria returned can be used
     * to retrieve enrollment records for the school being reported within the school start date and
     * the report end date.
     *
     * @return Criteria
     */
    private Criteria getEnrollmentCriteria() {
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_schoolStartDate);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportEndDate);

        return enrollmentCriteria;
    }

    /**
     * Returns the enrollment subquery to use for retrieving student and attendnace data. The
     * subquery returned is a report query containing one column: student OID. It can be used in
     * an in clause to retrieve all students with enrollment records within the school start date
     * and the report end date.
     *
     * @return ReportQueryByCriteria
     */
    private ReportQueryByCriteria getEnrollmentSubquery() {
        ReportQueryByCriteria enrollmentQuery =
                new ReportQueryByCriteria(StudentEnrollment.class,
                        new String[] {StudentEnrollment.COL_STUDENT_OID},
                        getEnrollmentCriteria());

        return enrollmentQuery;
    }

    /**
     * Returns the month in the following format: YYYYMM.
     *
     * @param date Date
     * @return String
     */
    private String getMonthId(Date date) {
        m_calendar.setTime(date);

        StringBuilder month = new StringBuilder(6);

        month.append(m_calendar.get(Calendar.YEAR));
        String calendarMonth = Integer.toString(m_calendar.get(Calendar.MONTH));
        month.append(StringUtils.padLeft(calendarMonth, 2, '0'));

        return month.toString();
    }

    /**
     * Returns the month name lookup map.
     *
     * @return an ArrayList of month names ranging from 0-11
     */
    private Map getMonthNames() {
        Map monthNames = new HashMap(12);
        for (int i = 0; i < 12; i++) {
            monthNames.put(StringUtils.padLeft(Integer.toString(i), 2, '0'),
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(getLocale(),
                            "label.calendar.month." + i));
        }

        return monthNames;
    }

    /**
     * Returns a List of KeyValuePair objects containing the date ranges for each month included
     * in the report.
     *
     * @return List
     */
    private List getMonthRanges() {
        List ranges = new LinkedList();

        // Create a separate calendar for calculating date ranges
        Calendar rangeCalendar = Calendar.getInstance(getLocale());

        String schoolStartMonthId = getMonthId(m_schoolStartDate);

        m_calendar.setTime(m_schoolStartDate);
        String lastMonthId = "";
        while (!m_calendar.getTime().after(m_reportEndDate)) {
            String monthId = getMonthId(m_calendar.getTime());
            if (!monthId.equals(lastMonthId)) {
                /*
                 * If we are in the first month of school, set the first date to the first day of
                 * school
                 */
                PlainDate firstDate;
                if (monthId.equals(schoolStartMonthId)) {
                    m_calendar.setTime(m_schoolStartDate);
                    firstDate = m_schoolStartDate;
                } else {
                    rangeCalendar.setTime(m_calendar.getTime());
                    rangeCalendar.set(Calendar.DAY_OF_MONTH, 1);
                    firstDate = new PlainDate(rangeCalendar.getTime());
                }

                /*
                 * If we are in the last month of school, set the last date to the last day of
                 * school
                 */
                rangeCalendar.setTime(m_calendar.getTime());
                rangeCalendar.set(Calendar.DAY_OF_MONTH, 1);
                rangeCalendar.add(Calendar.MONTH, 1);
                rangeCalendar.add(Calendar.DAY_OF_YEAR, -1);
                PlainDate lastDate = new PlainDate(rangeCalendar.getTime());

                KeyValuePair range = new KeyValuePair(firstDate, lastDate);
                ranges.add(range);
            }

            lastMonthId = monthId;
            m_calendar.add(Calendar.DAY_OF_YEAR, 1);
        }

        return ranges;
    }

    /**
     * Returns a Selection of the students that were members of the school on the report start date.
     *
     * @return Selection
     */
    private Selection getSelection() {
        Selection selection = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());

        EnrollmentManager emanager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        Collection selectedStudents = emanager.getMembershipAsOf(m_reportStartDate, (SisSchool) getSchool());
        if (selectedStudents == null) {
            selectedStudents = new LinkedList();
        }

        Iterator selectedIterator = selectedStudents.iterator();
        while (selectedIterator.hasNext()) {
            SelectionObject selectedObject =
                    X2BaseBean.newInstance(SelectionObject.class, getBroker().getPersistenceKey());
            selectedObject.setObjectOid((String) selectedIterator.next());
            selection.addToSelectionObjects(selectedObject);
        }

        selection.setTimestamp(System.currentTimeMillis());
        getBroker().saveBeanForced(selection);

        return selection;
    }

    /**
     * Loads the cell lookup maps. Two maps are populated by this method: one for looking up a
     * cell number based on a date, and the other for looking up a date based on a cell number.
     * <p>
     * The passed list of KeyValuePair date ranges is used to generate the Maps.
     *
     * @param ranges List
     */
    private void loadCellLookups(List ranges) {
        // Map for resolving a cell to a date
        m_cellToDay = new HashMap(200);

        // Map for resolving a date to a cell
        m_dayToCell = new HashMap(200);

        Iterator rangeIterator = ranges.iterator();
        while (rangeIterator.hasNext()) {
            KeyValuePair dateRange = (KeyValuePair) rangeIterator.next();

            PlainDate startDate = (PlainDate) dateRange.getKey();
            PlainDate endDate = (PlainDate) dateRange.getValue();

            String monthId = getMonthId(startDate);

            // Initialize the cell number to the first valid weekday column
            m_calendar.setTime(startDate);
            while (m_calendar.get(Calendar.DAY_OF_WEEK) == Calendar.SATURDAY ||
                    m_calendar.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY) {
                m_calendar.add(Calendar.DAY_OF_MONTH, 1);
            }

            // Shift the cell number based on the day of the week
            int cell = m_calendar.get(Calendar.DAY_OF_WEEK) - 2;

            while (!m_calendar.getTime().after(endDate)) {
                if (m_calendar.get(Calendar.DAY_OF_WEEK) != Calendar.SATURDAY &&
                        m_calendar.get(Calendar.DAY_OF_WEEK) != Calendar.SUNDAY) {
                    cell++;

                    String dayOfMonth = Integer.toString(m_calendar.get(Calendar.DAY_OF_MONTH));

                    m_cellToDay.put(monthId + "_" + cell, dayOfMonth);
                    m_dayToCell.put(monthId + "_" + dayOfMonth, Integer.toString(cell));
                }

                m_calendar.add(Calendar.DAY_OF_MONTH, 1);
            }
        }
    }

    /**
     * Populates the grid with attendance data for the passed date range .
     *
     * @param range A KeyValuePair where the key is the first date in the range, and the value
     *        is the last
     */
    private void populateAttendance(KeyValuePair range) {
        m_grid.beforeTop();

        String monthId = getMonthId((PlainDate) range.getKey());

        /*
         * Get all attendance for students who were members of the current school at any time during
         * the date range
         */
        Criteria attendanceCriteria = new Criteria();
        attendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, range.getKey());
        attendanceCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, range.getValue());
        attendanceCriteria.addGreaterOrEqualThan(StudentAttendance.REL_STUDENT + "." +
                SisStudent.COL_YOG, m_yogFirst);
        attendanceCriteria.addLessOrEqualThan(StudentAttendance.REL_STUDENT + "." +
                SisStudent.COL_YOG, m_yogLast);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addIn(StudentAttendance.COL_STUDENT_OID, getEnrollmentSubquery());
        studentCriteria.addOrEqualTo(StudentAttendance.REL_STUDENT + "." +
                SisStudent.COL_SCHOOL_OID, getSchool().getOid());

        attendanceCriteria.addAndCriteria(studentCriteria);

        /*
         * Adds specified attendance criteria.
         */
        if (hasSpecifiedCriteria()) {
            attendanceCriteria.addAndCriteria(getStudentObjectCriteria(StudentAttendance.COL_STUDENT_OID,
                    StudentAttendance.REL_STUDENT,
                    StudentAttendance.COL_SCHOOL_OID));
        } else {
            attendanceCriteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());
        }

        QueryByCriteria attendanceQuery =
                new QueryByCriteria(StudentAttendance.class, attendanceCriteria);
        attendanceQuery.addOrderByAscending(StudentAttendance.REL_STUDENT + "." + SisStudent.COL_YOG);
        attendanceQuery.addOrderByAscending(StudentAttendance.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);

        QueryIterator attendanceIterator = null;
        try {
            attendanceIterator = getBroker().getIteratorByQuery(attendanceQuery);
            while (attendanceIterator.hasNext()) {
                StudentAttendance attendance = (StudentAttendance) attendanceIterator.next();

                if (attendance.getDate() != null) {
                    m_calendar.setTime(attendance.getDate());

                    StringBuilder attendanceString = new StringBuilder(3);
                    attendanceString.append(attendance.getAbsentIndicator() ? CODE_ABSENT : "");
                    attendanceString.append(attendance.getTardyIndicator() ? CODE_TARDY : "");
                    attendanceString.append(attendance.getDismissedIndicator() ? CODE_DISMISSED : "");

                    Map locateCriteria = new HashMap(2);
                    locateCriteria.put(COLUMN_MONTH, monthId);
                    locateCriteria.put(COLUMN_STUDENT_OID, attendance.getStudentOid());

                    if (m_grid.locate(locateCriteria)) {
                        String cell = getCellId(monthId, m_calendar.get(Calendar.DAY_OF_MONTH));
                        if (cell != null) {
                            m_grid.set(cell, attendanceString.toString());
                        }
                    }
                }
            }
        } finally {
            if (attendanceIterator != null) {
                attendanceIterator.close();
            }
        }
    }

    /**
     * Populates the grid with non session days based on the school calendar.
     */
    private void populateNonSessionDays() {
        /*
         * Load non-session days into memory for fast lookup. The structure used is a Map of Sets.
         * The map is keyed on calendar ID. The set contains all non-session dates. This allows
         * non-session days to be identified by a simple contains() operation.
         */
        HashMap calendars = new HashMap(10);

        Criteria calendarCriteria = new Criteria();
        calendarCriteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." +
                SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        calendarCriteria.addGreaterOrEqualThan(SisSchoolCalendarDate.COL_DATE, m_schoolStartDate);
        calendarCriteria.addLessOrEqualThan(SisSchoolCalendarDate.COL_DATE, m_schoolEndDate);

        QueryByCriteria calendarQuery = new QueryByCriteria(SisSchoolCalendarDate.class, calendarCriteria);

        QueryIterator calendarDateIterator = null;
        try {
            calendarDateIterator = getBroker().getIteratorByQuery(calendarQuery);
            while (calendarDateIterator.hasNext()) {
                SisSchoolCalendarDate calendarDate = (SisSchoolCalendarDate) calendarDateIterator.next();
                SchoolCalendar schoolCalendar = calendarDate.getSchoolCalendar();

                if (!calendarDate.getInSessionIndicator() && schoolCalendar != null &&
                        schoolCalendar.getCalendarId() != null) {
                    HashSet dates = (HashSet) calendars.get(schoolCalendar.getCalendarId());
                    if (dates == null) {
                        dates = new HashSet(50);
                    }
                    dates.add(calendarDate.getDate());
                    calendars.put(schoolCalendar.getCalendarId(), dates);
                }
            }
        } finally {
            if (calendarDateIterator != null) {
                calendarDateIterator.close();
            }
        }

        /*
         * Set the non-session days on the grid. A date cell is labeled as non-session if
         * at least one of the following are true:
         *
         * 1) The date's corresponding SchoolCalendarDate bean has an in-session flag = false
         * 2) The date is before the first day of school
         * 3) The date is after the last day of school
         */
        m_grid.beforeTop();
        while (m_grid.next()) {
            SisStudent student = (SisStudent) m_grid.get(COLUMN_STUDENT);
            String calendarId = student.getCalendarCode();
            String monthId = (String) m_grid.get(COLUMN_MONTH);

            if (calendarId != null) {
                HashSet nonSessionDays = (HashSet) calendars.get(calendarId);
                if (nonSessionDays != null) {
                    for (int i = 1; i <= DATE_COLUMNS; i++) {
                        Date cellDate = getCellDate(monthId, i);
                        if (cellDate != null && (nonSessionDays.contains(cellDate) ||
                                cellDate.before(m_schoolStartDate) ||
                                cellDate.after(m_schoolEndDate))) {
                            m_grid.set(Integer.toString(i), SYMBOL_NONSESSION);
                        }
                    }
                }
            }
        }
    }

    /**
     * Populates the grid with enrollment data for the passed date range.
     *
     * @param range A KeyValuePair where the key is the first date in the range, and the value
     *        is the last
     */
    private void populateEnrollment(KeyValuePair range) {
        m_grid.beforeTop();

        String monthId = getMonthId((PlainDate) range.getKey());

        /*
         * Get all enrollment records for all schools
         */
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addIn(StudentEnrollment.COL_SCHOOL_OID, getSchoolOids());
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, range.getKey());
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, range.getValue());

        QueryByCriteria enrollmentQuery =
                new QueryByCriteria(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.REL_STUDENT + "." + SisStudent.COL_YOG);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.REL_STUDENT + "." + X2BaseBean.COL_OID);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);

        QueryIterator enrollmentIterator = null;
        try {
            enrollmentIterator = getBroker().getIteratorByQuery(enrollmentQuery);
            while (enrollmentIterator.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) enrollmentIterator.next();

                if (enrollment.getEnrollmentDate() != null &&
                        (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) ||
                                StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()) ||
                                StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType()))) {
                    m_calendar.setTime(enrollment.getEnrollmentDate());

                    Map locateCriteria = new HashMap(2);
                    locateCriteria.put(COLUMN_MONTH, monthId);
                    locateCriteria.put(COLUMN_STUDENT_OID, enrollment.getStudentOid());

                    if (m_grid.locate(locateCriteria)) {
                        String cell = getCellId(monthId, m_calendar.get(Calendar.DAY_OF_MONTH));
                        m_grid.set(cell, enrollment.getEnrollmentCode());

                        // Associate an enrollment type with the cell
                        m_grid.set(COLUMN_ENROLLMENT_TYPE + cell, enrollment);
                    }
                }
            }
        } finally {
            if (enrollmentIterator != null) {
                enrollmentIterator.close();
            }
        }
    }

    /**
     * Updates the grid with membership status for each cell. Cells corresponding to non-member
     * days are marked with the non-member symbol.
     */
    private void populateMembershipStatus() {
        // Create a Map containing the initial membership status.
        EnrollmentManager eManager =
                new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        Set membership = eManager.getMembershipAsOf(m_schoolStartDate, (SisSchool) getSchool());

        // Scan through the grid and update each cell with membership status
        m_grid.beforeTop();
        while (m_grid.next()) {
            String studentOid = (String) m_grid.get(COLUMN_STUDENT_OID);
            boolean isMember = membership.contains(studentOid);

            String monthId = (String) m_grid.get(COLUMN_MONTH);

            for (int i = 1; i <= DATE_COLUMNS; i++) {
                // Only update membership in a cell if the cell corresponds to a valid day
                if (getCellDay(monthId, i) != null) {
                    StudentEnrollment enrollment = (StudentEnrollment) m_grid.get(COLUMN_ENROLLMENT_TYPE + i);
                    String enrollmentType = null;
                    String enrollmentStatus = null;
                    if (enrollment != null) {
                        enrollmentType = enrollment.getEnrollmentType();
                        enrollmentStatus = enrollment.getStatusCode();
                    }

                    if (enrollmentType == null && !isMember) {
                        m_grid.set(Integer.toString(i), SYMBOL_NONMEMBER);
                    }

                    // Save the membership status for use in the next cell
                    if (StudentEnrollment.WITHDRAWAL.equals(enrollmentType)) {
                        isMember = false;
                    } else if (StudentEnrollment.ENTRY.equals(enrollmentType) &&
                            StudentManager.isActiveStudent(getOrganization(), enrollmentStatus)) {
                        isMember = true;
                    } else if (StudentEnrollment.STATUS_CHANGE.equals(enrollmentType) &&
                            StudentManager.isActiveStudent(getOrganization(), enrollmentStatus)) {
                        isMember = true;
                    } else if (StudentEnrollment.STATUS_CHANGE.equals(enrollmentType) &&
                            !StudentManager.isActiveStudent(getOrganization(), enrollmentStatus)) {
                        isMember = false;
                    }
                }
            }

            // Save the membership status at the end of this row for use in the next month
            if (isMember) {
                membership.add(studentOid);
            } else {
                membership.remove(studentOid);
            }
        }
    }

    /**
     * Populates the grid with student data.
     *
     * @param monthId String
     */
    private void populateStudents(String monthId) {
        m_grid.beforeTop();

        /*
         * Get all students who were members of the current school at any time during the date range
         */
        X2Criteria studentCriteria = new X2Criteria();
        Selection selection = getSelection();

        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, selection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX
                + X2BaseBean.COL_OID);

        studentCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));

        X2Criteria enrollmentCriteria = new X2Criteria();
        enrollmentCriteria.addIn(X2BaseBean.COL_OID, getEnrollmentSubquery());
        studentCriteria.addOrCriteria(enrollmentCriteria);

        studentCriteria.addGreaterOrEqualThan(SisStudent.COL_YOG, m_yogFirst);
        studentCriteria.addLessOrEqualThan(SisStudent.COL_YOG, m_yogLast);

        /*
         * Check if secondary students are included.
         */
        if (includeSecondaryStudents()) {
            studentCriteria.addOrCriteria(StudentManager.buildSecondaryStudentCriteria(getSchool()));
        }

        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
        if (includeSecondaryStudents()) {
            studentQuery.setDistinct(true);
        }
        studentQuery.addOrderByAscending(SisStudent.COL_YOG);
        studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        QueryIterator students = null;
        try {
            students = getBroker().getIteratorByQuery(studentQuery);
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                m_grid.append();
                m_grid.set(COLUMN_STUDENT, student);
                m_grid.set(COLUMN_STUDENT_OID, student.getOid());
                m_grid.set(COLUMN_MONTH, monthId);
            }
        } finally {
            if (students != null) {
                students.close();
            }
        }
    }

    /**
     * Computes and saves per-line totals to the grid.
     */
    private void populateTotals() {
        boolean entryIsMember = Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();

        boolean withdrawalIsMember = Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

        // Map to store YTD total accumulations
        Map ytdTotals = new HashMap(500);

        m_grid.beforeTop();
        while (m_grid.next()) {
            int absent = 0;
            int tardy = 0;
            int dismissed = 0;
            int nonmember = 0;
            int daysInSession = 0;

            String monthId = (String) m_grid.get(COLUMN_MONTH);

            for (int i = 1; i <= DATE_COLUMNS; i++) {
                String detail = (String) m_grid.get(Integer.toString(i));

                boolean inSession = (getCellDay(monthId, i) != null &&
                        !SYMBOL_NONSESSION.equals(detail));

                StudentEnrollment enrollment = (StudentEnrollment) m_grid.get(COLUMN_ENROLLMENT_TYPE + i);
                String enrollmentType = null;
                if (enrollment != null) {
                    enrollmentType = enrollment.getEnrollmentType();
                }

                if (StudentEnrollment.WITHDRAWAL.equals(enrollmentType) && inSession) {
                    // add one to non-member days based on preference
                    if (!withdrawalIsMember) {
                        nonmember++;
                    }
                } else if (StudentEnrollment.ENTRY.equals(enrollmentType) && inSession) {
                    // add one to non-member days based on preference
                    if (!entryIsMember) {
                        nonmember++;
                    }
                } else {
                    if (detail != null && inSession) {
                        absent += (detail.indexOf(CODE_ABSENT) != -1 ? 1 : 0);
                        dismissed += (detail.indexOf(CODE_DISMISSED) != -1 ? 1 : 0);
                        tardy += (detail.indexOf(CODE_TARDY) != -1 ? 1 : 0);
                    }

                    if (detail != null) {
                        nonmember += (detail.equals(SYMBOL_NONMEMBER) ? 1 : 0);
                    }
                }

                if (inSession) {
                    daysInSession++;
                }
            }

            // Update the YTD totals
            String studentOid = (String) m_grid.get(COLUMN_STUDENT_OID);
            YTDTotals totals = (YTDTotals) ytdTotals.get(studentOid);
            if (totals == null) {
                totals = new YTDTotals();
            }
            totals.m_absent += absent;
            totals.m_daysInSession += daysInSession;
            totals.m_dismissed += dismissed;
            totals.m_nonmember += nonmember;
            totals.m_tardy += tardy;

            ytdTotals.put(studentOid, totals);

            // Set the total values on the grid
            m_grid.set(COLUMN_MONTHLY_ABSENT, Integer.valueOf(absent));
            m_grid.set(COLUMN_MONTHLY_DAYS_IN_SESSION, Integer.valueOf(daysInSession));
            m_grid.set(COLUMN_MONTHLY_DISMISSED, Integer.valueOf(dismissed));
            m_grid.set(COLUMN_MONTHLY_NONMEMBER, Integer.valueOf(nonmember));
            m_grid.set(COLUMN_MONTHLY_PRESENT, Integer.valueOf(daysInSession - absent - nonmember));
            m_grid.set(COLUMN_MONTHLY_TARDY, Integer.valueOf(tardy));

            m_grid.set(COLUMN_YTD_ABSENT, Integer.valueOf(totals.m_absent));
            m_grid.set(COLUMN_YTD_DAYS_IN_SESSION, Integer.valueOf(totals.m_daysInSession));
            m_grid.set(COLUMN_YTD_DISMISSED, Integer.valueOf(totals.m_dismissed));
            m_grid.set(COLUMN_YTD_NONMEMBER, Integer.valueOf(totals.m_nonmember));
            m_grid.set(COLUMN_YTD_PRESENT, Integer.valueOf(totals.m_daysInSession - totals.m_absent - totals.m_nonmember));
            m_grid.set(COLUMN_YTD_TARDY, Integer.valueOf(totals.m_tardy));
        }
    }

    /**
     * Removes months outside of the the report date range from the grid. The grid is populated with
     * attendance data from the first day of school up until the report end date for the purposes
     * of calculating year-to-date totals. This method removes months between the first day of
     * school and the report start date from the grid.
     */
    private void truncateGrid() {
        String reportStartMonth = getMonthId(m_reportStartDate);

        /*
         * Determine the range of rows to delete. We must determine the range to delete first
         * because it is not safe to remove rows while iterating.
         */
        int deleteRangeStart = -1;
        int deleteRangeEnd = -1;

        m_grid.beforeTop();
        while (m_grid.next()) {
            String monthId = (String) m_grid.get(COLUMN_MONTH);

            /*
             * We use a lexicographical comparison rather than converting to integers because the
             * monthId for Nov. 2004 is 200410 and the monthId for Jan. 2005 is 20050.
             * Lexicographically 200410 is before 20050 but not numerically!
             */
            if (monthId.compareTo(reportStartMonth) < 0) {
                if (deleteRangeStart == -1) {
                    deleteRangeStart = m_grid.currentRowNumber();
                }
                deleteRangeEnd = m_grid.currentRowNumber();
            }
        }

        // Delete the rows within the range, inclusively
        if (deleteRangeStart != -1 && deleteRangeEnd != -1) {
            for (int i = deleteRangeEnd; i >= deleteRangeStart; i--) {
                m_grid.deleteRow(i);
            }
        }
    }

    /**
     * Simple structure for storing year-to-date totals.
     */
    private class YTDTotals {
        public int m_absent = 0;
        public int m_daysInSession = 0;
        public int m_dismissed = 0;
        public int m_nonmember = 0;
        public int m_tardy = 0;

        /**
         * Constructs a YTDTotals.
         */
        YTDTotals() {
            // Increase the visibility of the constructor to improve performance.
        }
    }
}
