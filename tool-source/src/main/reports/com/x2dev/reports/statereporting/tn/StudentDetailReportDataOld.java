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

package com.x2dev.reports.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.Selection;
import com.follett.fsc.core.k12.beans.SelectionObject;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SecondaryStudentDataSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.types.PlainDate;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Time;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source for the Student Detail report.
 *
 * @author X2 Development Corporation
 */
public class StudentDetailReportDataOld extends SecondaryStudentDataSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 
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

    // Attendance codes
    private static final String CODE_ABSENT = "A";
    private static final String CODE_TARDY = "T";
    private static final String CODE_DISMISSED = "D";

    private static final String COLUMN_CALENDAR_CODE = "calendarCode";
    private static final String COLUMN_ENROLLMENT_TYPE = "enrollment_";
    private static final String COLUMN_REPORTING_PERIOD = "reportingPeriod";
    private static final String COLUMN_SCHOOL = "school";
    private static final String COLUMN_STUDENT = "student";
    private static final String COLUMN_STUDENT_CODE = "studentCode";
    private static final String COLUMN_STUDENT_OID = "studentOid";
    private static final String STUDENT_SORT_PARAM = "studentSort";

    // Columns for period totals
    private static final String COLUMN_PERIOD_ABSENT = "pa";
    private static final String COLUMN_PERIOD_ADA = "ada";
    private static final String COLUMN_PERIOD_ADM = "adm";
    private static final String COLUMN_PERIOD_DAYS_IN_SESSION = "pds";
    private static final String COLUMN_PERIOD_DISMISSED = "pd";
    private static final String COLUMN_PERIOD_NONMEMBER = "pn";
    private static final String COLUMN_PERIOD_PRESENT = "pp";
    private static final String COLUMN_PERIOD_TARDY = "pt";

    // Columns for parameters used in the design
    private static final String PARAM_DATABASE_PLATFORM = "databasePlatform";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_PERIOD_DAY_LOOKUP = "days";
    private static final String PARAM_REPORTING_PERIOD = "reportingPeriod";
    private static final String PARAM_SQL_COURSE_CLASS_TYPE = "sqlFieldCourseClassType";
    private static final String PARAM_SQL_SCHEDULE_CHANGE_OVERRIDE_DATE = "sqlFieldScheduleChangeOverrideDate";
    private static final String PARAM_SQL_SCHOOL_CALENDAR_MINUTES_REQUIRED = "sqlFieldSchoolCalendarMinutesRequired";
    private static final String PARAM_START_DATE = "startDate";
    private static final String PARAM_STUDENT_EIS_STATE_CODE = "studentEisStateCode";
    private static final String PARAM_TOTALS_ONLY = "totalsOnly";
    private static final String STRIP_CHARACTER = "0";

    private static final int DATE_COLUMNS = 20;
    private static final int INITIAL_COLUMN_SIZE = 35;
    private static final int INITIAL_ROW_SIZE = 1500;
    private static final long MILLISECONDS_PER_MINUTE = 60000;
    private static final int REPORT_PERIOD_LENGTH = 20;
    private static final long serialVersionUID = 1L;

    private static final String DATE_FORMAT = "yyyy-MM-dd";
    private static final String DISABLE_BEAN_PATH = "-9999";
    private static final String FULLY_SCHEDULED = "_Scheduled";
    private static final String GRADE_LEVEL_P3 = "P3";
    private static final String GRADE_LEVEL_P4 = "P4";
    private static final String GRADE_LEVEL_PK = "PK";
    // Commented out per Dave Gray. See notes on method. JDK 1-14-15
    // private static final String NONSESSION_DATES = "nonsessionDates";
    private static final String NOT_APPLICALBE = "NA";
    private static final String STRING_NO = "No";
    private static final String STRING_TRUE = "1";
    private static final String STRING_YES = "Yes";

    // Symbols
    private static final String SYMBOL_NONMEMBER = "--";
    private static final String SYMBOL_NONSESSION = "X";

    private String m_activeCode;
    private Map<String, String> m_attendance;
    private String m_beanPathStudentEisStateCode;
    private Calendar m_calendar;
    private Map<String, Set<PlainDate>> m_calendars;
    private Map<String, String> m_cellToDay;
    private Map<String, Map<String, String>> m_cellToHeader;
    private String m_databasePlatform;
    private Map<String, KeyValuePair> m_periodDateRange;
    private Map<String, String> m_dailyScheduled;
    private Map<String, String> m_dayToCell;
    private Map<String, StudentEnrollment> m_enrollmentStatusChanges;
    private Collection<String> m_excludeGrades;
    private ReportDataGrid m_grid;
    private SimpleDateFormat m_lookupFormat;
    private Set<String> m_membership;
    // Commented out per Dave Gray. See notes on method. JDK 1-14-15
    // private Map<String, Map<Integer, String>> m_nonsession;
    // private PlainDate m_reportEndDate;
    private String m_reportingPeriod;
    // private PlainDate m_reportStartDate;
    private PlainDate m_schoolEndDate;
    private PlainDate m_schoolStartDate;
    private String m_schoolYearContext;
    private String m_sqlCourseClassType;
    private String m_sqlScheduleChangeOverrideDate;
    private String m_sqlSchoolCalendarMinutesRequired;

    private int m_studentAbsent;
    private int m_studentDaysInSession;
    private int m_studentDismissed;
    private int m_studentNonmember;
    private int m_studentTardy;

    /**
     * Constructs a ReportDataGrid containing reporting period register data. The grid contains a
     * row for
     * each student for the selected reporting period, and the following columns:
     * <ul>
     * <li>The reporting period stored as a String in the following format: 01
     * <li>Student object
     * <li>Student OID
     * <li>Columns 1-31, corresponding to the day of the month, containing the register detail
     * <li>Enrollment columns enrollment_1 - enrollment_31, containing null, "E", or "W"
     * <li>Period totals columns, corresponding to constants prefixed with COLUMN_PERIOD_
     * <li>Scheduled columns prefixed by the corresponding day of the month show if the student is
     * fully scheduled
     * </ul>
     *
     * @return JRDataSource
     * @throws ParseException exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws ParseException {
        Map<String, PlainDate> calendarEndDate = new HashMap<String, PlainDate>();
        Map<String, PlainDate> calendarStartDate = new HashMap<String, PlainDate>();

        // This report is designed to only work for a single school at a time due to a sorting issue
        // for secondary
        // students where students who were secondarily enrolled at the school would appear at the
        // bottom of the report
        // for every school. This was caused by ordering the query by the student's primary school
        // in order to
        // accommodate running for all schools. By dropping the all schools option, the sort was
        // eliminated and
        // the students sort correctly within the given school regardless of primary or secondary
        // status.
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
        criteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_schoolYearContext);

        QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SchoolCalendar schoolCalendar = (SchoolCalendar) iterator.next();
                String calendarId = schoolCalendar.getCalendarId();
                String periodKey = calendarId + StringUtils.stripStart(m_reportingPeriod, STRIP_CHARACTER);

                KeyValuePair<PlainDate, PlainDate> dateRange = m_periodDateRange.get(periodKey);
                PlainDate startDate = dateRange.getKey();
                PlainDate endDate = dateRange.getValue();

                loadCalendarCycleDates(schoolCalendar);
                loadDailyScheduled(startDate, endDate);
                loadMembership(startDate);

                // Commented out per Dave Gray. See notes on method. JDK 1-14-15
                // loadNonSessionDays(schoolCalendar);
                populateStudents(startDate, endDate);
                populateGrid();

                calendarEndDate.put(calendarId, endDate);
                calendarStartDate.put(calendarId, startDate);
            }
        } finally {
            iterator.close();
        }

        m_grid.beforeTop();

        // Add report parameters
        Boolean totalsOnly = (Boolean) getParameter(TOTALS_ONLY_PARAM);
        addParameter(PARAM_START_DATE, calendarStartDate);
        addParameter(PARAM_END_DATE, calendarEndDate);
        addParameter(PARAM_PERIOD_DAY_LOOKUP, m_cellToHeader);
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
        m_activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_attendance = new HashMap<String, String>();
        m_sqlCourseClassType = (String) getParameter(PARAM_SQL_COURSE_CLASS_TYPE);
        m_sqlScheduleChangeOverrideDate = (String) getParameter(PARAM_SQL_SCHEDULE_CHANGE_OVERRIDE_DATE);
        m_sqlSchoolCalendarMinutesRequired = (String) getParameter(PARAM_SQL_SCHOOL_CALENDAR_MINUTES_REQUIRED);
        m_beanPathStudentEisStateCode = (String) getParameter(PARAM_STUDENT_EIS_STATE_CODE);
        m_calendar = Calendar.getInstance(getLocale());
        m_calendars = new HashMap<String, Set<PlainDate>>();
        m_cellToDay = new HashMap(200);
        m_cellToHeader = new HashMap(200);
        m_databasePlatform = (String) getParameter(PARAM_DATABASE_PLATFORM);
        m_periodDateRange = new HashMap<String, KeyValuePair>();
        m_dailyScheduled = new HashMap<String, String>();
        m_dayToCell = new HashMap(200);
        m_enrollmentStatusChanges = new HashMap<String, StudentEnrollment>();
        m_grid = new ReportDataGrid(INITIAL_ROW_SIZE, INITIAL_COLUMN_SIZE);
        m_lookupFormat = new SimpleDateFormat("dd");
        m_membership = new HashSet<String>();
        // Commented out per Dave Gray. See notes on method. JDK 1-14-15
        // m_nonsession = new HashMap<String, Map<Integer, String>>();
        m_reportingPeriod = (String) getParameter(PARAM_REPORTING_PERIOD);
        // Per Dave Gray - making the report to run for the current school year only, but leaving
        // old code in place
        // in case a district needs to be able to run for prior years in the future. JDK 1-20-15.
        // m_schoolYearContext = (String) getParameter(CONTEXT_OID_PARAM);
        m_schoolYearContext = getOrganization().getCurrentContextOid();
        m_studentAbsent = 0;
        m_studentTardy = 0;
        m_studentDismissed = 0;
        m_studentNonmember = 0;
        m_studentDaysInSession = 0;

        // Grades to be excluded
        m_excludeGrades = new ArrayList();
        m_excludeGrades.add(GRADE_LEVEL_PK);
        m_excludeGrades.add(GRADE_LEVEL_P3);
        m_excludeGrades.add(GRADE_LEVEL_P4);

        loadSchoolDates();
        loadReportingPeriodDateRanges();

        // Must run after loadReportingPeriodDateRanges
        loadAttendance();
        loadEnrollmentChanges();
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

        if (m_cellToHeader != null) {
            m_cellToHeader.clear();
            m_cellToHeader = null;
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
     * Calculates totals for multiple fields.
     *
     * @param entryIsMember boolean
     * @param withdrawalIsMember boolean
     * @param periodId String
     * @param i int
     */
    private void calculateTotals(boolean entryIsMember, boolean withdrawalIsMember, String periodId, int i) {

        String detail = (String) m_grid.get(Integer.toString(i));
        boolean inSession = (getCellDay(periodId, i) != null && !SYMBOL_NONSESSION.equals(detail));

        StudentEnrollment enrollment = (StudentEnrollment) m_grid.get(COLUMN_ENROLLMENT_TYPE + i);
        String enrollmentType = null;
        if (enrollment != null) {
            enrollmentType = enrollment.getEnrollmentType();
        }

        if (StudentEnrollment.WITHDRAWAL.equals(enrollmentType) && inSession) {
            // add one to non-member days based on preference
            if (!withdrawalIsMember) {
                m_studentNonmember++;
            }
        } else if (StudentEnrollment.ENTRY.equals(enrollmentType) && inSession) {
            // add one to non-member days based on preference
            if (!entryIsMember) {
                m_studentNonmember++;
            }
        } else {
            if (detail != null && inSession) {
                m_studentAbsent += (detail.indexOf(CODE_ABSENT) != -1 ? 1 : 0);
                m_studentDismissed += (detail.indexOf(CODE_DISMISSED) != -1 ? 1 : 0);
                m_studentTardy += (detail.indexOf(CODE_TARDY) != -1 ? 1 : 0);
            }

            if (detail != null) {
                m_studentNonmember += (detail.equals(SYMBOL_NONMEMBER) ? 1 : 0);
            }
        }

        if (inSession) {
            m_studentDaysInSession++;
        }
    }

    /**
     * Returns the day corresponding to the passed reporting period and cell number, null if one
     * does not exist.
     *
     * @param periodId String
     * @param cell int
     * @return String
     */
    private String getCellDay(String periodId, int cell) {
        return m_cellToDay.get(periodId + "_" + cell);
    }

    /**
     * Returns the enrollment criteria to use for retrieving data. The Criteria returned can be used
     * to retrieve enrollment records for the school being reported within the school start date and
     * the report end date.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Criteria
     */
    private Criteria getEnrollmentCriteria(PlainDate startDate, PlainDate endDate) {
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);

        return enrollmentCriteria;
    }

    /**
     * Returns the enrollment subquery to use for retrieving student and attendnace data. The
     * subquery returned is a report query containing one column: student OID. It can be used in
     * an in clause to retrieve all students with enrollment records within the school start date
     * and the report end date.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return ReportQueryByCriteria
     */
    private ReportQueryByCriteria getEnrollmentSubquery(PlainDate startDate, PlainDate endDate) {
        SubQuery enrollmentQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID,
                        getEnrollmentCriteria(startDate, endDate));

        return enrollmentQuery;
    }

    /**
     * Gets the selected field from the passed bean.
     *
     * @param bean X2BaseBean
     * @param path String
     * @return String
     */
    private String getFieldValue(X2BaseBean bean, String path) {
        String value = "";
        if (bean != null && path != null && !path.equals(DISABLE_BEAN_PATH)) {
            value = (String) bean.getFieldValueByBeanPath(path);
        }

        return value;
    }

    /**
     * Returns true if the school day is in session.
     *
     * @param schoolOid String
     * @param calendarId String
     * @param i int
     * @return boolean
     * @throws ParseException exception
     */
    private boolean getInSession(String schoolOid, String calendarId, int i) throws ParseException {
        // Default needs to be true b/c ADM only evaluates all in session days. Defaulting to false
        // would cause days not marked as being in session to be flagged as not fully scheduled.
        boolean inSession = true;

        if (calendarId != null) {
            String key = schoolOid + calendarId;
            Set nonSessionDays = m_calendars.get(key);
            if (nonSessionDays != null) {
                SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
                Date cellDate;

                String date = m_cellToDay.get(m_reportingPeriod + "_" + i);

                if (date != null) {
                    try {
                        cellDate = sdf.parse(date);
                    } catch (ParseException e) {
                        e.printStackTrace();
                        throw e;
                    }

                    if (cellDate != null && (nonSessionDays.contains(cellDate) ||
                            cellDate.before(m_schoolStartDate) ||
                            cellDate.after(m_schoolEndDate))) {
                        inSession = false;
                    }
                } else {
                    inSession = false;
                }
            }
        }

        return inSession;
    }

    /**
     * Returns the student's membership status after evaluating enrollment records for the given
     * day.
     *
     * @param student SisStudent
     * @param periodId String
     * @param i int
     * @return boolean
     */
    private boolean getMembershipStatus(SisStudent student, String periodId, int i) {
        String dateVal = getCellDay(periodId, i);
        String studentOid = student.getOid();

        boolean isMember = m_membership.contains(studentOid);
        boolean wasMember = m_membership.contains(studentOid);

        // Since m_enrollment only contains changes to enrollment within the report's selected date
        // range, isMember will
        // only be modified when those enrollment records match the date of the cell being
        // processed.
        if (dateVal != null) {
            String key = studentOid + dateVal;

            StudentEnrollment enrollment = m_enrollmentStatusChanges.get(key);
            String enrollmentType = null;
            String enrollmentStatus = null;

            if (enrollment != null) {
                enrollmentType = enrollment.getEnrollmentType();
                enrollmentStatus = enrollment.getStatusCode();
            }

            // Save the membership status for use in the next cell
            if (StudentEnrollment.WITHDRAWAL.equals(enrollmentType)) {
                isMember = false;
            } else if (StudentEnrollment.ENTRY.equals(enrollmentType) && m_activeCode.equals(enrollmentStatus)) {
                isMember = true;
            } else if (StudentEnrollment.STATUS_CHANGE.equals(enrollmentType)
                    && m_activeCode.equals(enrollmentStatus)) {
                isMember = true;
            } else if (StudentEnrollment.STATUS_CHANGE.equals(enrollmentType)
                    && !m_activeCode.equals(enrollmentStatus)) {
                isMember = false;
            }

            if (isMember != wasMember) {
                if (isMember) {
                    m_membership.add(studentOid);
                } else {
                    m_membership.remove(studentOid);
                }
            }
        }

        return isMember;
    }

    /**
     * Returns a Selection of the students that were members of the school on the report start date.
     *
     * @param startDate PlainDate
     * @return Selection
     */
    private Selection getSelection(PlainDate startDate) {
        Collection selectedStudents = new LinkedList<String>();
        Selection selection = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());
        EnrollmentManager emanager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        selectedStudents.addAll(emanager.getMembershipAsOf(startDate, (SisSchool) getSchool()));
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
     * Returns the time difference in minutes between two times.
     *
     * @param startTime Time
     * @param endTime Time
     * @return int
     */
    private int getTimeDifference(Time startTime, Time endTime) {
        Long minutesPresent = Long.valueOf(0);

        if (startTime != null && endTime != null) {
            minutesPresent = Long.valueOf((endTime.getTime() - startTime.getTime()) / MILLISECONDS_PER_MINUTE);
        }

        return minutesPresent.intValue();
    }

    /**
     * Loads the max and min calendar dates to m_reportStartDate and m_reportEndDate for the given
     * reporting period.
     *
     * @param schoolCalendar SchoolCalendar
     */
    private void loadCalendarCycleDates(SchoolCalendar schoolCalendar) {
        int cell = 1;
        Map<String, String> cellToHeader = new HashMap<String, String>();
        String periodKey = schoolCalendar.getCalendarId() + StringUtils.stripStart(m_reportingPeriod, STRIP_CHARACTER);

        KeyValuePair<PlainDate, PlainDate> dateRange = m_periodDateRange.get(periodKey);
        PlainDate startDate = dateRange.getKey();
        PlainDate endDate = dateRange.getValue();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, schoolCalendar.getOid());
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_schoolYearContext);
        criteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, STRING_TRUE);
        criteria.addGreaterOrEqualThan(SisSchoolCalendarDate.COL_DATE, startDate);
        criteria.addLessOrEqualThan(SisSchoolCalendarDate.COL_DATE, endDate);

        QueryByCriteria query = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
        query.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                SisSchoolCalendarDate calendarDate = (SisSchoolCalendarDate) iterator.next();
                PlainDate calDate = calendarDate.getDate();

                cellToHeader.put(m_reportingPeriod + "_" + cell, m_lookupFormat.format(calDate));
                m_cellToDay.put(m_reportingPeriod + "_" + cell, calDate.toString());
                m_dayToCell.put(m_reportingPeriod + "_" + calDate.toString(), Integer.toString(cell));
                cell++;
            }
        } finally {
            iterator.close();
        }

        m_cellToHeader.put(schoolCalendar.getCalendarId(), cellToHeader);
    }

    /**
     * Loads whether a student is fully scheduled for each day in the funding period.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void loadDailyScheduled(PlainDate startDate, PlainDate endDate) {
        String lastKey = null;
        int lastMinutesRequired = 0;
        int minutesScheduled = 0;

        // 1 = MySql or 2 = SQLServer
        String top1M = m_databasePlatform.equals("2") ? "TOP 1000000000 " : "";

        StringBuilder sql = new StringBuilder(512);
        sql.append("SELECT SSC_STD_OID, SCH_SKL_OID, MST_OID, CSD_DATE," + m_sqlSchoolCalendarMinutesRequired
                + ", MST_DESCRIPTION, BPE_START_TIME, BPE_END_TIME ");
        sql.append("FROM ( ");
        sql.append("    SELECT " + top1M + "SSC_STD_OID, BASE.SCH_SKL_OID, MST_OID, CSD_DATE,"
                + m_sqlSchoolCalendarMinutesRequired + ", MST_DESCRIPTION, BPE_START_TIME, BPE_END_TIME ");
        sql.append("    FROM  ");
        sql.append("    ( ");
        sql.append("        SELECT " + top1M
                + "SSC_OID, SSC_STD_OID, STD_CALENDAR_CODE, CAS_CALENDAR_ID, SSC_SCH_OID, SSC_MST_OID, ");
        sql.append("          SCH_SKL_OID, MST_OID," + m_sqlSchoolCalendarMinutesRequired
                + ", MST_DESCRIPTION, MST_CSK_OID, SCH_OID, CAS_OID ");
        sql.append("        FROM STUDENT_SCHEDULE ");
        sql.append("        JOIN STUDENT ON SSC_STD_OID = STD_OID     ");
        sql.append("        JOIN SCHEDULE_MASTER ON SSC_MST_OID = MST_OID ");
        sql.append("        JOIN SCHEDULE ON SCH_OID = MST_SCH_OID ");
        sql.append("        JOIN SCHOOL ON SCH_SKL_OID = SKL_OID ");
        sql.append("        JOIN CALENDAR_SCHOOL ON SKL_OID = CAS_SKL_OID ");
        sql.append("        AND STD_CALENDAR_CODE = CAS_CALENDAR_ID ");
        sql.append("        AND SCH_CTX_OID = CAS_CTX_OID ");
        sql.append("        WHERE SCH_SKL_OID = '" + getSchool().getOid() + "' ");
        sql.append("        AND NOT EXISTS ");
        sql.append("        ( ");
        sql.append("         SELECT 1");
        sql.append("         FROM STUDENT_SCHEDULE_CHANGE     ");
        sql.append("         WHERE STD_OID = SCC_STD_OID     ");
        sql.append("         AND SCH_OID = SCC_SCH_OID     ");
        sql.append("         AND MST_OID = SCC_MST_OID ");
        sql.append("         )  ");
        sql.append("        ORDER BY SSC_SCH_OID, SSC_MST_OID ");
        sql.append("    ) BASE ");
        sql.append("    JOIN SCHEDULE_DAY ON BASE.SCH_OID = DAY_SCH_OID ");
        sql.append("    JOIN COURSE_SCHOOL ON BASE.MST_CSK_OID = CSK_OID ");
        sql.append("    JOIN COURSE ON CSK_CRS_OID = CRS_OID ");
        sql.append("    JOIN ");
        sql.append("    ( ");
        sql.append("        SELECT " + top1M + "MTM_MST_OID, MTM_OID ");
        sql.append("        FROM SCHEDULE_MASTER_TERM ");
        sql.append("        WHERE EXISTS ");
        sql.append("        ( ");
        sql.append("            SELECT 1    ");
        sql.append("            FROM SCHEDULE_TERM     ");
        sql.append("            JOIN SCHEDULE_TERM_DATE ON TRM_OID = TMD_TRM_OID     ");
        sql.append("            JOIN SCHEDULE     ON TRM_SCH_OID = SCH_OID     ");
        sql.append("            WHERE TMD_START_DATE <= ?     ");
        sql.append("            AND TMD_END_DATE >= ? ");
        sql.append("            AND MTM_TRM_OID = TRM_OID   ");
        sql.append("        ) ");
        sql.append("        ORDER BY MTM_MST_OID, MTM_OID ");
        sql.append("    ) AS TERM ON BASE.MST_OID = TERM.MTM_MST_OID ");
        sql.append("    JOIN SCHEDULE_MASTER_MATRIX ON MTM_OID = MMX_MTM_OID ");
        sql.append(
                "    JOIN SCHEDULE_MATRIX ON MTX_OID = MMX_MTX_OID AND SCH_OID = MTX_SCH_OID AND DAY_OID = MTX_DAY_OID ");
        sql.append("    JOIN SCHEDULE_PERIOD ON MTX_PER_OID = PER_OID ");
        sql.append("    JOIN SCHEDULE_BELL ON BASE.SCH_OID = BEL_SCH_OID ");
        sql.append("    JOIN SCHEDULE_BELL_PERIOD ON BEL_OID = BPE_BEL_OID AND PER_OID = BPE_PER_OID ");
        sql.append("    JOIN ");
        sql.append("    ( ");
        sql.append("        SELECT " + top1M
                + "CSD_BEL_OID, CSD_CAS_OID, CSD_SCHEDULE_DAY_NUMBER, CSD_DATE, CAS_CALENDAR_ID ");
        sql.append("        FROM CALENDAR_SCHOOL_DATE ");
        sql.append("        JOIN CALENDAR_SCHOOL ");
        sql.append("        ON CSD_CAS_OID = CAS_OID ");
        sql.append("        WHERE CSD_DATE >= ? AND CSD_DATE <= ? ");
        sql.append("        ORDER BY CSD_BEL_OID, CSD_CAS_OID, CSD_SCHEDULE_DAY_NUMBER, CSD_DATE ");
        sql.append("    ) CSD ");
        sql.append("         ON BEL_OID = CSD_BEL_OID ");
        sql.append("         AND CAS_OID = CSD_CAS_OID     ");
        sql.append("         AND DAY_NUMBER = CSD_SCHEDULE_DAY_NUMBER ");
        sql.append("         AND STD_CALENDAR_CODE = CSD.CAS_CALENDAR_ID ");
        sql.append("    WHERE ");
        sql.append("    " + m_sqlCourseClassType + " IN ('C', 'T') ");
        sql.append("    ORDER BY SCH_SKL_OID, SSC_STD_OID, CSD_DATE," + m_sqlSchoolCalendarMinutesRequired
                + ", BPE_START_TIME ");
        sql.append(" ) AS BASELINE ");
        sql.append(" ");
        sql.append(" UNION ALL ");
        sql.append(" ");
        sql.append(" SELECT SCC_STD_OID, SCH_SKL_OID, MST_OID, CSD_DATE," + m_sqlSchoolCalendarMinutesRequired
                + ", MST_DESCRIPTION, BPE_START_TIME, BPE_END_TIME ");
        sql.append(" FROM ");
        sql.append(" (");
        sql.append("     SELECT " + top1M + "SCC_STD_OID, SCH_SKL_OID, MST_OID, CSD_DATE,"
                + m_sqlSchoolCalendarMinutesRequired + ", MST_DESCRIPTION, BPE_START_TIME, BPE_END_TIME ");
        sql.append("     FROM ");
        sql.append("     ( ");
        sql.append("          SELECT " + top1M
                + "ADDS.SCC_OID, ADDS.SCC_STD_OID, STD_CALENDAR_CODE, CAS_CALENDAR_ID, ADDS.SCC_SCH_OID, ADDS.SCC_MST_OID, ");
        sql.append("          COALESCE(ADDS." + m_sqlScheduleChangeOverrideDate
                + ", ADDS.SCC_DATE) AS ADD_DATE, DROP_DATE, SCH_SKL_OID, MST_OID," + m_sqlSchoolCalendarMinutesRequired
                + ", ");
        sql.append("          MST_DESCRIPTION, MST_CSK_OID, SCH_OID, CAS_OID ");
        sql.append("          FROM STUDENT_SCHEDULE_CHANGE AS ADDS     ");
        sql.append("          LEFT JOIN ");
        sql.append("          (         ");
        sql.append("            SELECT " + top1M + "SCC_OID, SCC_STD_OID, SCC_SCH_OID, SCC_MST_OID, COALESCE("
                + m_sqlScheduleChangeOverrideDate + ", SCC_DATE) AS DROP_DATE         ");
        sql.append("            FROM STUDENT_SCHEDULE_CHANGE         ");
        sql.append("            WHERE SCC_CHANGE_TYPE_CODE = 'Drop'     ");
        sql.append("            ORDER BY SCC_STD_OID, SCC_SCH_OID, SCC_MST_OID ");
        sql.append("        ) AS DROPS     ");
        sql.append("        ON ADDS.SCC_STD_OID = DROPS.SCC_STD_OID     ");
        sql.append("        AND ADDS.SCC_SCH_OID = DROPS.SCC_SCH_OID     ");
        sql.append("        AND ADDS.SCC_MST_OID = DROPS.SCC_MST_OID ");
        sql.append("        JOIN STUDENT ON ADDS.SCC_STD_OID = STD_OID ");
        sql.append("        JOIN SCHEDULE_MASTER ON ADDS.SCC_MST_OID = MST_OID ");
        sql.append("        JOIN SCHEDULE ON SCH_OID = MST_SCH_OID ");
        sql.append("        JOIN SCHOOL ON SCH_SKL_OID = SKL_OID ");
        sql.append("        JOIN CALENDAR_SCHOOL ON SKL_OID = CAS_SKL_OID ");
        sql.append("        AND STD_CALENDAR_CODE = CAS_CALENDAR_ID ");
        sql.append("        WHERE ADDS.SCC_CHANGE_TYPE_CODE = 'Add' ");
        sql.append("        AND SCH_SKL_OID = '" + getSchool().getOid() + "' ");
        sql.append("        ORDER BY SCC_SCH_OID, SCC_MST_OID, ADD_DATE, DROP_DATE ");
        sql.append("    ) BASE ");
        sql.append("    JOIN SCHEDULE_BELL ON BASE.SCC_SCH_OID = BEL_SCH_OID ");
        sql.append("    JOIN SCHEDULE_DAY ON BASE.SCC_SCH_OID = DAY_SCH_OID ");
        sql.append("    JOIN COURSE_SCHOOL ON MST_CSK_OID = CSK_OID ");
        sql.append("    JOIN COURSE ON CSK_CRS_OID = CRS_OID ");
        sql.append("    JOIN ");
        sql.append("    ( ");
        sql.append("        SELECT " + top1M + "MTM_MST_OID, MTM_OID ");
        sql.append("        FROM SCHEDULE_MASTER_TERM ");
        sql.append("        WHERE EXISTS ");
        sql.append("        ( ");
        sql.append("            SELECT 1    ");
        sql.append("            FROM SCHEDULE_TERM     ");
        sql.append("            JOIN SCHEDULE_TERM_DATE ON TRM_OID = TMD_TRM_OID     ");
        sql.append("            JOIN SCHEDULE     ON TRM_SCH_OID = SCH_OID     ");
        sql.append("            WHERE TMD_START_DATE <= ?     ");
        sql.append("            AND TMD_END_DATE >= ? ");
        sql.append("            AND MTM_TRM_OID = TRM_OID   ");
        sql.append("        ) ");
        sql.append("        ORDER BY MTM_MST_OID, MTM_OID ");
        sql.append("    ) AS TERM ON BASE.MST_OID = TERM.MTM_MST_OID ");
        sql.append("    JOIN SCHEDULE_MASTER_MATRIX ON MTM_OID = MMX_MTM_OID ");
        sql.append(
                "    JOIN SCHEDULE_MATRIX ON MTX_OID = MMX_MTX_OID AND SCH_OID = MTX_SCH_OID AND DAY_OID = MTX_DAY_OID ");
        sql.append("    JOIN SCHEDULE_PERIOD ON MTX_PER_OID = PER_OID ");
        sql.append("    JOIN SCHEDULE_BELL_PERIOD ON BEL_OID = BPE_BEL_OID AND PER_OID = BPE_PER_OID ");
        sql.append("    JOIN ");
        sql.append("    ( ");
        sql.append("        SELECT " + top1M
                + "CSD_BEL_OID, CSD_CAS_OID, CSD_SCHEDULE_DAY_NUMBER, CSD_DATE, CAS_CALENDAR_ID ");
        sql.append("        FROM CALENDAR_SCHOOL_DATE ");
        sql.append("        JOIN CALENDAR_SCHOOL ");
        sql.append("        ON CSD_CAS_OID = CAS_OID ");
        sql.append("        WHERE CSD_DATE >= ? AND CSD_DATE <= ? ");
        sql.append("        ORDER BY CSD_BEL_OID, CSD_CAS_OID, CSD_SCHEDULE_DAY_NUMBER, CSD_DATE ");
        sql.append("    ) CSD ");
        sql.append("         ON BEL_OID = CSD_BEL_OID ");
        sql.append("         AND CAS_OID = CSD_CAS_OID     ");
        sql.append("         AND DAY_NUMBER = CSD_SCHEDULE_DAY_NUMBER     ");
        sql.append("         AND STD_CALENDAR_CODE = CSD.CAS_CALENDAR_ID ");
        sql.append("         AND CSD_DATE >= ADD_DATE     ");
        sql.append("         AND (CSD_DATE < DROP_DATE OR DROP_DATE IS NULL)     ");
        sql.append("    WHERE ");
        sql.append("    " + m_sqlCourseClassType + " IN ('C', 'T') ");
        sql.append("    ORDER BY SCC_STD_OID, SCH_SKL_OID, MST_OID, CSD_DATE," + m_sqlSchoolCalendarMinutesRequired
                + ", MST_DESCRIPTION, BPE_START_TIME, BPE_END_TIME ");
        sql.append(") AS SCHEDULE_CHANGES ");
        sql.append("ORDER BY SSC_STD_OID, CSD_DATE ");

        Connection connection = getBroker().borrowConnection();
        PreparedStatement statement = null;
        ResultSet results = null;

        try {
            statement = connection.prepareStatement(sql.toString());

            statement.setDate(1, endDate);
            statement.setDate(2, startDate);
            statement.setDate(3, startDate);
            statement.setDate(4, endDate);
            statement.setDate(5, endDate);
            statement.setDate(6, startDate);
            statement.setDate(7, startDate);
            statement.setDate(8, endDate);

            results = statement.executeQuery();

            while (results.next()) {
                String studentOid = results.getString(1);
                Date date = results.getDate(4);
                int minutesRequired = Integer.parseInt(results.getString(5));
                Time startTime = results.getTime(7);
                Time endTime = results.getTime(8);


                /*
                 * AppGlobals.getLog().severe("### studentOid: " + studentOid);
                 * AppGlobals.getLog().severe("### mstOid: " + results.getString(3));
                 * AppGlobals.getLog().severe("### description: " + results.getString(6));
                 * AppGlobals.getLog().severe("### date: " + date);
                 * AppGlobals.getLog().severe("### minutesRequired: " + minutesRequired);
                 * AppGlobals.getLog().severe("### startTime: " + startTime);
                 * AppGlobals.getLog().severe("### endTime: " + endTime);
                 */

                String cell = m_dayToCell.get(m_reportingPeriod + "_" + date);
                String key = studentOid + cell;

                /*
                 * AppGlobals.getLog().severe("### cell: " + cell);
                 * AppGlobals.getLog().severe("### key: " + key);
                 * AppGlobals.getLog().severe("### lastKey: " + lastKey);
                 * AppGlobals.getLog().severe("### minutesScheduled: " + minutesScheduled);
                 * AppGlobals.getLog().severe("### lastMinutesRequired: " + lastMinutesRequired);
                 */


                if (lastKey != null && key != null && !key.equals(lastKey)) {
                    String fullyScheduled = "";

                    if (minutesScheduled < lastMinutesRequired) {
                        fullyScheduled = STRING_NO;
                    } else if (minutesScheduled >= lastMinutesRequired) {
                        fullyScheduled = STRING_YES;
                    }

                    // AppGlobals.getLog().severe("### fullyScheduled" + fullyScheduled);

                    m_dailyScheduled.put(lastKey, fullyScheduled);

                    lastMinutesRequired = 0;
                    minutesScheduled = 0;
                }

                minutesScheduled += getTimeDifference(startTime, endTime);
                lastMinutesRequired = minutesRequired;
                lastKey = key;
            }

            String fullyScheduled = "";
            if (minutesScheduled < lastMinutesRequired) {
                fullyScheduled = STRING_NO;
            } else if (minutesScheduled >= lastMinutesRequired) {
                fullyScheduled = STRING_YES;
            }

            m_dailyScheduled.put(lastKey, fullyScheduled);
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            try {
                if (results != null) {
                    results.close();
                }

                if (statement != null) {
                    statement.close();
                }
            } catch (Exception e) {
                AppGlobals.getLog().log(Level.WARNING, e.getMessage(), e);
            }

            getBroker().returnConnection();
        }
    }

    /**
     * Loads enrollment changes that fall within the date range of the report to a map keyed on
     * student oid + date.
     */
    private void loadEnrollmentChanges() {
        /*
         * Since the loadMembership() method loads the membership status as of the first day of the
         * report's date range,
         * we only need to load changes to the enrollment records that occur within the funding
         * period.
         */
        X2Criteria enrollmentCriteria = new X2Criteria();
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_schoolStartDate);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_schoolEndDate);
        enrollmentCriteria.addNotIn(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_GRADE_LEVEL,
                m_excludeGrades);
        enrollmentCriteria.addIn(StudentEnrollment.COL_SCHOOL_OID, getSchoolOids());

        QueryByCriteria enrollmentQuery = new QueryByCriteria(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + X2BaseBean.COL_OID);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);

        QueryIterator enrollmentIterator = null;
        try {
            enrollmentIterator = getBroker().getIteratorByQuery(enrollmentQuery);
            while (enrollmentIterator.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) enrollmentIterator.next();
                PlainDate date = enrollment.getEnrollmentDate();

                if (date != null) {
                    String key = enrollment.getStudentOid() + date;
                    m_enrollmentStatusChanges.put(key, enrollment);
                }
            }
        } finally {
            if (enrollmentIterator != null) {
                enrollmentIterator.close();
            }
        }
    }

    /**
     * Loads the student oids of students who are members as of the start of school to a set.
     *
     * @param startDate PlainDate
     */
    private void loadMembership(PlainDate startDate) {
        // Create a Map containing the initial membership status.
        EnrollmentManager eManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());

        if (startDate != null) {
            m_membership = eManager.getMembershipAsOf(startDate, (SisSchool) getSchool());
        }
    }

    /**
     * Loads the date ranges for each reporting period / calendar to a map keyed on calendar ID +
     * period.
     */
    private void loadReportingPeriodDateRanges() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                getSchool().getOid());
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_schoolYearContext);
        criteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
        query.addOrderBy(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID,
                true);
        query.addOrderBy(SisSchoolCalendarDate.COL_DATE, true);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            int dayCounter = 1;
            int periodCounter = 1;
            int previousPeriod = 1;

            String previousCalendarId = null;
            PlainDate periodStartDate = null;
            PlainDate previousDate = null;
            PlainDate previousStartDate = null;

            while (iterator.hasNext()) {
                SisSchoolCalendarDate schoolCalendarDate = (SisSchoolCalendarDate) iterator.next();
                PlainDate date = schoolCalendarDate.getDate();
                String calendarId = schoolCalendarDate.getSchoolCalendar().getCalendarId();
                boolean hasNext = iterator.hasNext();
                boolean calendarsMatch = calendarId.equals(previousCalendarId);

                if (dayCounter > REPORT_PERIOD_LENGTH || !calendarsMatch) {
                    dayCounter = 1;

                    if (!calendarsMatch) {
                        periodCounter = 1;
                    } else {
                        periodCounter++;
                    }
                }

                if (dayCounter == 1) {
                    periodStartDate = date;
                }

                /*
                 * AppGlobals.getLog().severe("### date: " + date);
                 * AppGlobals.getLog().severe("### previousDate: " + previousDate);
                 * AppGlobals.getLog().severe("### periodCounter: " + periodCounter);
                 * AppGlobals.getLog().severe("### dayCounter: " + dayCounter);
                 * AppGlobals.getLog().severe("### calendarId: " + calendarId);
                 * AppGlobals.getLog().severe("### previousCalendarId: " + previousCalendarId);
                 * AppGlobals.getLog().severe("### periodStartDate: " + periodStartDate);
                 * AppGlobals.getLog().severe("### previousStartDate: " + previousStartDate);
                 */

                if (previousCalendarId != null && (previousPeriod != periodCounter || !calendarsMatch || !hasNext)) {
                    KeyValuePair dateRange = null;
                    PlainDate startDate = null;
                    PlainDate endDate = null;
                    String periodKey = null;
                    String period = String.valueOf(periodCounter);

                    if (!hasNext) {
                        periodKey = calendarId + period;
                        startDate = periodStartDate;
                        endDate = date;
                    } else {
                        periodKey = previousCalendarId + previousPeriod;
                        startDate = previousStartDate;
                        endDate = previousDate;
                    }

                    /*
                     * AppGlobals.getLog().severe("### hasNext: " + hasNext);
                     * AppGlobals.getLog().severe("### periodKey: " + periodKey);
                     * AppGlobals.getLog().severe("### startDate: " + startDate);
                     * AppGlobals.getLog().severe("### endDate: " + endDate);
                     */

                    dateRange = new KeyValuePair<PlainDate, PlainDate>(startDate, endDate);
                    m_periodDateRange.put(periodKey, dateRange);

                    // AppGlobals.getLog().severe("### m_periodDateRange: " + m_periodDateRange);
                }

                dayCounter++;
                previousCalendarId = calendarId;
                previousDate = date;
                previousPeriod = periodCounter;
                previousStartDate = periodStartDate;
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Sets the start and end date variables.
     *//*
       * private void setStartEndDates(String period, PlainDate startDate, PlainDate endDate)
       * {
       * if (m_reportingPeriod != null && StringUtils.stripStart(m_reportingPeriod,
       * STRIP_CHARACTER).equals(period))
       * {
       * m_reportStartDate = startDate;
       * m_reportEndDate = endDate;
       * 
       * if (m_reportStartDate != null && m_reportEndDate != null && m_schoolStartDate != null &&
       * m_schoolEndDate != null)
       * {
       * // Force the report start & end dates within the school year
       * if (m_reportStartDate.before(m_schoolStartDate))
       * {
       * m_reportStartDate = m_schoolStartDate;
       * }
       * if (m_reportEndDate.after(m_schoolEndDate))
       * {
       * m_reportEndDate = m_schoolEndDate;
       * }
       * }
       * }
       * }
       */

    // Code is temporarily commented out as Dave Gray requested this be temporarily removed to see
    // if any
    // districts really need this feature. Please leave code so that it can be easily re-enabled if
    // needed.
    // JDK 1-14-15
    /**
     * Loads a nested map of nonSession days keyed on school oid + calendar id.
     */
    /*
     * private void loadNonSessionDays(SchoolCalendar schoolCalendar) throws ParseException
     * {
     * String lastKey = "";
     * 
     * Load non-session days into memory for fast lookup. The structure used is a Map of Sets.
     * The map is keyed on calendar ID. The set contains all non-session dates. This allows
     * non-session days to be identified by a simple contains() operation.
     * 
     * Criteria calendarCriteria = new Criteria();
     * calendarCriteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
     * SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
     * calendarCriteria.addEqualTo(SisSchoolCalendarDate.COL_SCHOOL_CALENDAR_OID,
     * schoolCalendar.getOid());
     * calendarCriteria.addGreaterOrEqualThan(SisSchoolCalendarDate.COL_DATE, m_reportStartDate);
     * calendarCriteria.addLessOrEqualThan(SisSchoolCalendarDate.COL_DATE, m_reportEndDate);
     * calendarCriteria.addNotEqualTo(DistrictCalendar.COL_IN_SESSION_INDICATOR, STRING_TRUE);
     * 
     * QueryByCriteria calendarQuery = new QueryByCriteria(SisSchoolCalendarDate.class,
     * calendarCriteria);
     * 
     * calendarQuery.addOrderByAscending(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER
     * +
     * SchoolCalendar.COL_SCHOOL_OID);
     * calendarQuery.addOrderByAscending(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER
     * +
     * SchoolCalendar.COL_CALENDAR_ID);
     * calendarQuery.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);
     * 
     * QueryIterator calendarDateIterator = null;
     * try
     * {
     * int i = 1;
     * calendarDateIterator = getBroker().getIteratorByQuery(calendarQuery);
     * while (calendarDateIterator.hasNext())
     * {
     * SisSchoolCalendarDate calendarDate = (SisSchoolCalendarDate) calendarDateIterator.next();
     * 
     * if (schoolCalendar.getSchoolOid() != null && schoolCalendar != null &&
     * schoolCalendar.getCalendarId() != null)
     * {
     * String key = schoolCalendar.getSchoolOid() + schoolCalendar.getCalendarId();
     * 
     * if (!lastKey.equals(key))
     * {
     * i = 1;
     * }
     * 
     * Set dates = m_calendars.get(key);
     * Map<Integer, String> nonsessionDates = m_nonsession.get(key);
     * if (dates == null)
     * {
     * dates = new HashSet(50);
     * }
     * 
     * if (nonsessionDates == null)
     * {
     * nonsessionDates = new HashMap<Integer, String>();
     * }
     * 
     * PlainDate date = calendarDate.getDate();
     * m_calendar.setTime(date);
     * 
     * // Exclude weekends from non-session dates
     * if(m_calendar.get(Calendar.DAY_OF_WEEK) != Calendar.SATURDAY &&
     * m_calendar.get(Calendar.DAY_OF_WEEK) != Calendar.SUNDAY)
     * {
     * 
     * // Map keyed on school + calendar ID containing a set of non-session dates. The set gets used
     * // in the getInSession method to get if the date is an in-session date.
     * dates.add(date);
     * nonsessionDates.put(Integer.valueOf(i), m_lookupFormat.format(date));
     * m_calendars.put(key, dates);
     * 
     * // Nested Map of non-session dates keyed on school + calendar ID. Inner map is formatted for
     * // display in iReport and keyed on cell numbers
     * m_nonsession.put(key, nonsessionDates);
     * 
     * i++;
     * }
     * 
     * lastKey = key;
     * }
     * }
     * 
     * addParameter(NONSESSION_DATES, m_nonsession);
     * }
     * finally
     * {
     * if (calendarDateIterator != null)
     * {
     * calendarDateIterator.close();
     * }
     * }
     * }
     */

    /**
     * Loads the school start and end dates to m_schoolStartDate and m_schoolEndDate
     */
    private void loadSchoolDates() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Schedule.COL_DISTRICT_CONTEXT_OID, m_schoolYearContext);
        criteria.addEqualTo(Schedule.COL_SCHOOL_OID, getSchool().getOid());

        QueryByCriteria query = new QueryByCriteria(Schedule.class, criteria);
        Schedule schedule = (Schedule) getBroker().getBeanByQuery(query);

        if (schedule != null) {
            m_schoolStartDate = schedule.getStartDate();
            m_schoolEndDate = schedule.getEndDate();
        }
    }

    /**
     * Populates student attendance info to a map keyed on student oid + date.
     */
    private void loadAttendance() {
        /*
         * Get all attendance for students who were members of the current school at any time during
         * the date range
         */
        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addIn(StudentAttendance.COL_STUDENT_OID,
                getEnrollmentSubquery(m_schoolStartDate, m_schoolEndDate));
        studentCriteria.addOrEqualTo(StudentAttendance.REL_STUDENT + PATH_DELIMITER +
                SisStudent.COL_SCHOOL_OID, getSchool().getOid());

        Criteria attendanceCriteria = new Criteria();
        attendanceCriteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());
        attendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_schoolStartDate);
        attendanceCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_schoolEndDate);
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

        QueryByCriteria attendanceQuery = new QueryByCriteria(StudentAttendance.class, attendanceCriteria);
        attendanceQuery.addOrderByAscending(StudentAttendance.REL_STUDENT + PATH_DELIMITER +
                SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
        attendanceQuery.addOrderByAscending(StudentAttendance.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);

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

                    String key = attendance.getStudentOid() + attendance.getDate();

                    m_attendance.put(key, attendanceString.toString());
                }
            }
        } finally {
            if (attendanceIterator != null) {
                attendanceIterator.close();
            }
        }
    }

    /**
     * Populates the grid.
     *
     * @throws ParseException exception
     */
    private void populateGrid() throws ParseException {
        boolean entryIsMember = Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();

        boolean withdrawalIsMember = Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

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
            double averageDailyMembership = 0.00;

            SisStudent student = (SisStudent) m_grid.get(COLUMN_STUDENT);
            String calendarId = student.getCalendarCode();
            String periodId = (String) m_grid.get(COLUMN_REPORTING_PERIOD);
            String studentOid = (String) m_grid.get(COLUMN_STUDENT_OID);



            m_studentAbsent = 0;
            m_studentTardy = 0;
            m_studentDismissed = 0;
            m_studentNonmember = 0;
            m_studentDaysInSession = 0;

            boolean isMember = false;

            for (int i = 1; i <= DATE_COLUMNS; i++) {
                String fullyScheduled = "";

                String fullyScheduledKey = studentOid + i;
                String scheduled = m_dailyScheduled.get(fullyScheduledKey);

                // Sets membership status
                isMember = getMembershipStatus(student, periodId, i);
                setMembershipStatus(isMember, periodId, i);

                // Sets attendance
                setAttendance(studentOid, periodId, i);

                // Updates non-session days
                boolean inSession = getInSession(student.getSchoolOid(), calendarId, i);
                setNonSessionDays(inSession, i);

                // Calculates the totals for absences, tardies, days member, etc
                calculateTotals(entryIsMember, withdrawalIsMember, periodId, i);

                if (isMember && inSession) {
                    if (scheduled == null) {
                        fullyScheduled = STRING_NO;
                    } else {
                        fullyScheduled = scheduled;

                        if (scheduled.equals(STRING_YES)) {
                            averageDailyMembership++;
                        }
                    }
                } else {
                    fullyScheduled = NOT_APPLICALBE;
                }

                m_grid.set(i + FULLY_SCHEDULED, fullyScheduled);
            }

            int daysPresent = m_studentDaysInSession - m_studentAbsent - m_studentNonmember;
            double averageDaysAttendance = 0.00;

            if (daysPresent > 0) {
                averageDaysAttendance = Integer.valueOf(daysPresent).doubleValue() /
                        Integer.valueOf(m_studentDaysInSession).doubleValue();

                // Rounds to 2 decimal places
                averageDaysAttendance = roundToTwoDecimals(averageDaysAttendance);
            }

            // Rounds to 2 decimal places
            averageDailyMembership = roundToTwoDecimals(averageDailyMembership / m_studentDaysInSession);

            // Set the total values on the grid
            m_grid.set(COLUMN_PERIOD_ABSENT, Integer.valueOf(m_studentAbsent));
            m_grid.set(COLUMN_PERIOD_ADA, Double.valueOf(averageDaysAttendance));
            m_grid.set(COLUMN_PERIOD_ADM, Double.valueOf(averageDailyMembership));
            m_grid.set(COLUMN_PERIOD_DAYS_IN_SESSION, Integer.valueOf(m_studentDaysInSession));
            m_grid.set(COLUMN_PERIOD_DISMISSED, Integer.valueOf(m_studentDismissed));
            m_grid.set(COLUMN_PERIOD_NONMEMBER, Integer.valueOf(m_studentNonmember));
            m_grid.set(COLUMN_PERIOD_PRESENT, Integer.valueOf(daysPresent));
            m_grid.set(COLUMN_PERIOD_TARDY, Integer.valueOf(m_studentTardy));

            // Save the membership status at the end of this row for use in the next month
            if (isMember) {
                m_membership.add(studentOid);
            } else {
                m_membership.remove(studentOid);
            }
        }
    }

    /**
     * Populates the grid with student data.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void populateStudents(PlainDate startDate, PlainDate endDate) {
        m_grid.beforeTop();

        /*
         * Get all students who were members of the current school at any time during the date range
         */
        X2Criteria studentCriteria = new X2Criteria();
        Selection selection = getSelection(startDate);

        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, selection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX
                + X2BaseBean.COL_OID);

        studentCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        /*
         * Records coming from the getSelection() method which uses enrollment manager cannot easily
         * be filtered since
         * this class doesn't inherit from enrollment manager. Those will be filtered in the loop
         * below.
         */
        studentCriteria.addNotIn(SisStudent.COL_GRADE_LEVEL, m_excludeGrades);

        X2Criteria enrollmentCriteria = new X2Criteria();
        enrollmentCriteria.addIn(X2BaseBean.COL_OID, getEnrollmentSubquery(startDate, endDate));
        studentCriteria.addOrCriteria(enrollmentCriteria);

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

        // Apply the student sort order
        String[] studentSortOrder = getUserSortOrderAsStringArray((String) getParameter(STUDENT_SORT_PARAM));
        for (int i = 0; i < studentSortOrder.length; i++) {
            studentQuery.addOrderByAscending(studentSortOrder[i]);
        }

        // School chosen by the user at run time
        School selectedSchool = getSchool();

        QueryIterator students = null;
        try {
            students = getBroker().getIteratorByQuery(studentQuery);

            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                if (!m_excludeGrades.contains(student.getGradeLevel())) {
                    School school = selectedSchool;
                    if (school == null) {
                        school = student.getSchool();
                    }

                    String studentOid = student.getOid();
                    if (!m_grid.locate(COLUMN_STUDENT_OID, studentOid)) {
                        m_grid.append();

                        m_grid.set(COLUMN_SCHOOL, school);
                        m_grid.set(COLUMN_CALENDAR_CODE, student.getCalendarCode());
                        m_grid.set(COLUMN_STUDENT, student);

                        m_grid.set(COLUMN_STUDENT_OID, studentOid);
                        m_grid.set(COLUMN_STUDENT_CODE, getFieldValue(student, m_beanPathStudentEisStateCode));
                        m_grid.set(COLUMN_REPORTING_PERIOD, m_reportingPeriod);
                    }
                }
            }
        } finally {
            if (students != null) {
                students.close();
            }
        }
    }

    /**
     * Round value to nearest two decimal places.
     *
     * @param val double
     * @return double
     */
    private double roundToTwoDecimals(double val) {
        val = (double) Math.round(val * 100) / 100;

        return val;
    }

    /**
     * Sets non-session days on the grid.
     *
     * @param studentOid String
     * @param periodId String
     * @param i int
     */
    private void setAttendance(String studentOid, String periodId, int i) {
        String calDate = getCellDay(periodId, i);
        String key = studentOid + calDate;
        String attendance = m_attendance.get(key);

        if (attendance != null) {
            m_grid.set(Integer.toString(i), attendance);
        }
    }

    /**
     * Sets the membership status on the grid for the student for the passed day. Returns the
     * membership status
     *
     * @param isMember boolean
     * @param periodId String
     * @param i int
     */
    private void setMembershipStatus(boolean isMember, String periodId, int i) {
        // Only update membership in a cell if the cell corresponds to a valid day
        if (getCellDay(periodId, i) != null) {
            if (!isMember) {
                m_grid.set(Integer.toString(i), SYMBOL_NONMEMBER);
            }
        }
    }

    /**
     * Sets non-session days on the grid.
     *
     * @param inSession boolean
     * @param i int
     */
    private void setNonSessionDays(boolean inSession, int i) {
        if (!inSession) {
            m_grid.set(Integer.toString(i), SYMBOL_NONSESSION);
        }
    }
}
