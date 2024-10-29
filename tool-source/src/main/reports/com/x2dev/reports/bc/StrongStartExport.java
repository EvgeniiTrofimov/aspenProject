/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * BC Strong Start Extract.
 *
 * @author X2 Development Corporation
 */
public class StrongStartExport extends ExportJavaSource {

    /**
     *
     * Subclass for StateReportData used to expose StudentHistoryHelper.
     */
    private class AttendanceData extends StateReportData {
        /*
         * Parameter for a random calendar from a school.
         */
        private static final String CALENDAR_ANY = "*";
        private static final String CUSTOM_CRITERIA = "Custom Criteria";

        private StudentHistoryHelper m_helper;
        private Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars =
                new HashMap<String, Map<String, Set<PlainDate>>>();
        private Map<String, List<StudentAttendance>> m_studentAttendanceMap;

        /**
         * Instantiates a new attendance data.
         */
        /*
         * General constructor
         */
        public AttendanceData() {
            super();
        }

        /**
         * Set up the student history helper.
         */
        @Override
        public void initialize() {
            m_helper = new StudentHistoryHelper(this);
            /*
             * This uses an undocumented feature of StudentHistoryHelper to use a completely custom
             * criteria for students
             */
            m_helper.setStudentSelectionMode(CUSTOM_CRITERIA);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.FALSE);
        }

        /**
         * Get the calendar in-session days for a particular student and enrollment span.
         *
         * @param school SisSchool
         * @param student SisStudent
         * @param span StudentEnrollmentSpan
         * @return Set<PlainDate>
         */
        Set<PlainDate> getCalendarDays(SisSchool school, SisStudent student, StudentEnrollmentSpan span) {
            Set<PlainDate> spanDates = new HashSet<PlainDate>();

            PlainDate startDate = span.getFirstActiveEnrollment().getEnrollmentDate();
            if (startDate == null) {
                startDate = m_startDate;
            }

            PlainDate endDate = span.getFirstInactiveEnrollment() == null ? null
                    : span.getFirstInactiveEnrollment().getEnrollmentDate();
            if (endDate == null) {
                endDate = m_endDate;
            }

            Set<PlainDate> dates = getCalendarDaysSet(school, student.getCalendarCode());
            if (dates == null || dates.size() == 0) {
                dates = getCalendarDaysSet(school, CALENDAR_ANY);
            }

            if (dates != null) {
                for (PlainDate date : dates) {
                    if (!date.before(startDate) && !date.after(endDate)) {
                        spanDates.add(date);
                    }
                }
            }

            return spanDates;
        }

        /**
         * Get the attendance information for a student.
         *
         * @param studentOid String
         * @return List<StudentAttendance>
         */
        List<StudentAttendance> getStudentAttendances(String studentOid) {
            if (m_studentAttendanceMap == null) {
                X2Criteria studentAttendanceCriteria = new X2Criteria();
                studentAttendanceCriteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);
                studentAttendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_startDate);
                studentAttendanceCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_endDate);
                studentAttendanceCriteria.addIn(StudentAttendance.COL_SCHOOL_OID, m_schoolOids);

                QueryByCriteria studentAttendanceQuery =
                        new QueryByCriteria(StudentAttendance.class, studentAttendanceCriteria);
                studentAttendanceQuery.addOrderBy(StudentAttendance.COL_STUDENT_OID, true);
                studentAttendanceQuery.addOrderBy(StudentAttendance.COL_DATE, false);

                m_studentAttendanceMap = getBroker().getGroupedCollectionByQuery(studentAttendanceQuery,
                        StudentAttendance.COL_STUDENT_OID, 500);

            }

            // Return the requests students attendance records.
            return m_studentAttendanceMap.get(studentOid);
        }

        /**
         * Get the student criteria from the StudentHistoryHelper.
         *
         * @return Criteria
         */
        Criteria getStudentCriteria() {
            return m_helper.getStudentCriteria();
        }

        /**
         * Get the enrollment span from the StudentHistoryHelper.
         *
         * @param student Student
         * @return List<StudentEnrollmentSpan>
         */
        List<StudentEnrollmentSpan> getStudentEnrollmentSpans(Student student) {
            return m_helper.getStudentEnrollmentSpans(student, true);
        }

        /**
         * Returns a set of days-in-session for the given school and calendar ID combination.
         *
         * @param school SisSchool
         * @param calendar String
         * @return Sets the
         */
        private Set<PlainDate> getCalendarDaysSet(SisSchool school, String calendar) {
            Set<PlainDate> calendarDates = null;

            Map<String, Set<PlainDate>> calendarData = null;
            if (school != null && !m_schoolsToCalendars.containsKey(school.getOid())) {
                calendarData = getCalendarLookup(school, m_startDate, m_endDate);
                m_schoolsToCalendars.put(school.getOid(), calendarData);
            }

            if (school != null) {
                calendarData = m_schoolsToCalendars.get(school.getOid());
                // Get any calendar after checking the calendars map is not empty
                if (CALENDAR_ANY.equals(calendar) && !calendarData.isEmpty()) {
                    calendarDates = calendarData.values().iterator().next();
                } else {
                    calendarDates = calendarData.get(calendar);
                }
            }

            return calendarDates;
        }

        /**
         * Returns a structure for looking up calendar in-session days for a given school. The
         * return
         * value is a Map of Sets. The key to the map is calendar ID. The value is a Set of date
         * objects
         * identifying in-session days for the given school/calendar ID.
         *
         * @param school SisSchool
         * @param startDate The first date in the range of dates to load
         * @param endDate The last date in the range of dates to load
         * @return A Map of Set objects (containing PlainDate objects) keyed on String objects
         */
        private Map getCalendarLookup(SisSchool school, PlainDate startDate, PlainDate endDate) {
            HashMap calendarData = new HashMap(5);

            Criteria criteria = new Criteria();
            criteria.addLessOrEqualThan(SisSchoolCalendarDate.COL_DATE, endDate);
            criteria.addGreaterOrEqualThan(SisSchoolCalendarDate.COL_DATE, startDate);
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, school.getOid());

            QueryByCriteria calendarQuery = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
            QueryIterator calendars = getBroker().getIteratorByQuery(calendarQuery);
            try {
                while (calendars.hasNext()) {
                    SisSchoolCalendarDate calendarDate = (SisSchoolCalendarDate) calendars.next();
                    String calendarId = calendarDate.getSchoolCalendar().getCalendarId();

                    HashSet dates = (HashSet) calendarData.get(calendarId);
                    if (dates == null) {
                        dates = new HashSet(200);
                        calendarData.put(calendarId, dates);
                    }

                    if (calendarDate.getInSessionIndicator()) {
                        dates.add(calendarDate.getDate());
                    }
                }
            } finally {
                calendars.close();
            }

            return calendarData;
        }
    }

    /*
     * Name for the report date parameters. The corresponding values is a PlainDate object.
     */
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_START_DATE = "startDate";
    private static final String PARAM_END_DATE = "endDate";
    private static final String SCHOOL_OIDS_PARAM = "schoolOids";

    /*
     * Data Extract File Layout
     */
    private static final String FIELD_ATTENDANCE = "Attendance";
    private static final String FIELD_SEQ_NUMBER = "Sequence Number";
    private static final String FIELD_MINISTRY_NUMBER = "School";
    private static final String FIELD_STUD_PEN = "PEN";

    /*
     * Other constants
     */
    private static final String EARLY_LEARNING_LEVEL = "Early Learning";
    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyyMMdd");

    // Evaluated report parameters / control fields
    PlainDate m_endDate;
    PlainDate m_reportDate;
    Collection<String> m_schoolOids;
    PlainDate m_startDate;

    private List<String> m_columns;
    private AttendanceData m_data;
    private String m_seqNumber;
    private int m_totalRecords;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid();

        QueryByCriteria query = new QueryByCriteria(Student.class, buildCriteria());
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                /*
                 * Aspen doesn't do positive attendance, so what we need to do is find the
                 * in-session dates
                 * for the student within the report date and remove dates where the student was
                 * absent. We are
                 * using the StudentHistoryHelper, you can use getCalendarDays
                 */
                List<StudentEnrollmentSpan> enrollmentSpans = m_data.getStudentEnrollmentSpans(student);
                for (StudentEnrollmentSpan span : enrollmentSpans) {
                    SisSchool school = span.getSchool();

                    if (school != null && m_schoolOids.contains(school.getOid())) {
                        Set<PlainDate> inSessionDays =
                                new TreeSet<PlainDate>(m_data.getCalendarDays(school, student, span));
                        Set<PlainDate> studentAbsenceDays = getStudentAbsenceDates(student.getOid());
                        inSessionDays.removeAll(studentAbsenceDays);

                        for (PlainDate attendanceDate : inSessionDays) {
                            grid.append();
                            grid.set(FIELD_SEQ_NUMBER, m_seqNumber);
                            grid.set(FIELD_STUD_PEN, student.getStateId());
                            grid.set(FIELD_MINISTRY_NUMBER, school.getSchoolId());
                            grid.set(FIELD_ATTENDANCE, DATE_FORMAT.format(attendanceDate));
                        }
                    }
                }
            }
        } finally {
            students.close();
        }

        m_totalRecords = grid.getRows().size();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getHeader() {
        StringBuilder headerBuilder = new StringBuilder();

        // Control header
        headerBuilder.append(m_seqNumber)
                .append(",")
                .append(DATE_FORMAT.format(m_reportDate))
                .append(",")
                .append(DATE_FORMAT.format(m_startDate))
                .append(",")
                .append(DATE_FORMAT.format(m_endDate))
                .append(",")
                .append(m_totalRecords)
                .append("\n");

        return headerBuilder.toString();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        setIncludeHeaderRow(false);
        setUseValueWrappers(false);

        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);

        m_schoolOids = getSchoolOids();

        m_data = new AttendanceData();
        m_data.setBroker(getBroker());
        m_data.setCurrentContext(getCurrentContext());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        m_data.setSchoolContext(isSchoolContext());
        m_data.setSchool(getSchool());
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();

        m_columns = new ArrayList<String>(4);
        m_columns.add(FIELD_SEQ_NUMBER);
        m_columns.add(FIELD_STUD_PEN);
        m_columns.add(FIELD_MINISTRY_NUMBER);
        m_columns.add(FIELD_ATTENDANCE);

        incrementSequenceNumber();
    }

    /**
     * Get the criteria for the students exported.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        Criteria studentCriteria = m_data.getStudentCriteria();

        X2Criteria enrollmentCriteria = new X2Criteria();
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_startDate);
        enrollmentCriteria.addIn(StudentEnrollment.COL_SCHOOL_OID, m_schoolOids);

        SubQuery enrollmentSubQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);
        X2Criteria enrCriteria = new X2Criteria();
        enrCriteria.addIn(X2BaseBean.COL_OID, enrollmentSubQuery);

        X2Criteria activeCriteria = new X2Criteria();
        activeCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
        activeCriteria.addIn(Student.COL_SCHOOL_OID, m_schoolOids);

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addOrCriteria(enrCriteria);
        orCriteria.addOrCriteria(activeCriteria);

        studentCriteria.addAndCriteria(orCriteria);

        return studentCriteria;
    }

    /**
     * Returns collection with export school oids.
     *
     * @return Collection<String>
     */
    private Collection<String> getSchoolOids() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisSchool.COL_SCHOOL_TYPE_CODE, EARLY_LEARNING_LEVEL);

        if (getSchool() != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            String oids = (String) getParameter(SCHOOL_OIDS_PARAM);
            if (!StringUtils.isEmpty(oids)) {
                Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);
                criteria.addIn(X2BaseBean.COL_OID, oidList);
            } else {
                criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }
        }

        return getBroker().getSubQueryCollectionByQuery(new SubQuery(SisSchool.class, X2BaseBean.COL_OID, criteria));
    }

    /**
     * Return a set of absence dates for the student.
     *
     * @param studentOid String
     * @return Set<PlainDate>
     */
    private Set<PlainDate> getStudentAbsenceDates(String studentOid) {
        List<StudentAttendance> attendances = m_data.getStudentAttendances(studentOid);

        Set<PlainDate> studentAbsenceDates = new HashSet<PlainDate>();
        if (attendances != null) {
            for (StudentAttendance attendance : attendances) {
                studentAbsenceDates.add(attendance.getDate());
            }
        }

        return studentAbsenceDates;
    }

    /**
     * Increment the sequence number and store in the comment.
     */
    private void incrementSequenceNumber() {
        Tool tool = getJob().getTool();
        if (ImportExportDefinition.class.isInstance(tool)) {
            ImportExportDefinition export = (ImportExportDefinition) tool;

            if (StringUtils.isEmpty(export.getComment())) {
                m_seqNumber = "1";
                export.setComment("SEQ:" + m_seqNumber);
            } else {
                Pattern pattern = Pattern.compile("SEQ:(\\d*)");
                Matcher matcher = pattern.matcher(export.getComment());
                if (matcher.find()) {
                    int iStart = matcher.start();
                    int iEnd = matcher.end();
                    String seq = matcher.group(1);

                    try {
                        int iSeq = Integer.parseInt(seq);
                        m_seqNumber = Integer.toString(iSeq + 1);
                    } catch (NumberFormatException e) {
                        m_seqNumber = "1";
                    }

                    String comment = export.getComment().substring(0, iStart) + "SEQ:" + m_seqNumber
                            + export.getComment().substring(iEnd);
                    export.setComment(comment);
                } else {
                    m_seqNumber = "1";
                    export.setComment(export.getComment() + "\nSEQ:" + m_seqNumber);
                }
            }

            getBroker().saveBeanForced(export);
        }
    }
}
