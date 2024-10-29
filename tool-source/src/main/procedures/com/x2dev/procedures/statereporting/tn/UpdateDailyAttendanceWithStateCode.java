/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2020 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */


package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Updates the Daily Attendance record's absence flag for Tardy and Dismissed records by comparing
 * the number of minutes
 * the student is present in class to the number of required minutes (listed on the school calendar
 * record). Also
 * updates the DOE PRESENT MINUTES with the number of minutes the student was present and the DOE
 * ATTENDANCE REPORTED
 * field with the state reporting absence code.
 *
 * @author X2 Development Corporation
 */
public class UpdateDailyAttendanceWithStateCode extends ProcedureJavaSource {
    private static final String DATE_FORMAT = "MM/dd/yyyy";
    private static final List<String> DIST_LEAN_ATT_CODES = Arrays.asList("N", "H", "I", "Y", "D", "Z");
    private static final String DOE_ATTENDANCE_REPORTED = "DOE ATTENDANCE REPORTED";
    private static final String DOE_PRESENT_MINUTES = "DOE PRESENT MINUTES";
    private static final String DOE_STUDENT_DAY_LENGTH = "DOE STUDENT DAY LENGTH";
    private static final long serialVersionUID = 1L;

    // Input parameters
    private static final String END_DATE = "endDate";
    private static final String START_DATE = "startDate";
    private static final String TOTAL_MINUTES_REQUIRED = "totalMinutesRequired";

    private int m_updateCount;

    private Map<String, Collection<StudentTransportation>> m_studentTransportation;
    private Map<String, SisSchoolCalendarDate> m_calendarDates;
    private Map<String, RefAttendanceStudent> m_distanceLearningRatByCodeMap;
    private Map<String, Integer> m_minutesRequired;
    private Map<String, PlainTime> m_scheduleStart;
    private Map<PlainDate, Collection<String>> m_studentOids;

    private int m_totalMinutesRequired;
    private PlainDate m_endDate;
    private PlainDate m_startDate;
    private String m_studentOid;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);

        Criteria criteria = new Criteria();
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_startDate);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_endDate);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());
        }

        if (m_studentOid != null) {
            criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, m_studentOid);
        }

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        query.addOrderByAscending(StudentAttendance.COL_SCHOOL_OID);
        query.addOrderByAscending(StudentAttendance.COL_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisSchool lastSchool = null;
            PlainDate lastDate = null;

            while (iterator.hasNext()) {
                StudentAttendance studentAtt = (StudentAttendance) iterator.next();
                SisStudent student = studentAtt.getStudent();
                String calendarName = studentAtt.getStudent().getCalendarCode();
                PlainDate date = studentAtt.getDate();
                SisSchool school = student.getSchool();
                String schoolOid = student.getSchoolOid();
                String key = schoolOid + calendarName + date;
                SisSchoolCalendarDate calendarDate = m_calendarDates.get(key);
                if (calendarDate != null) {
                    if ((lastSchool != null && lastSchool.getOid() != school.getOid()) ||
                            (lastDate != null && !lastDate.equals(date))) {
                        logUpdateCount(sdf, lastSchool, lastDate);
                    }

                    // Calculates minutes present and sets the fields on the attendance record
                    boolean success = setAttendanceFields(studentAtt, calendarDate);

                    if (!success) {
                        throw new Exception("Setting attendance fields failed. Please contact Aspen Support.");
                    }

                    lastSchool = school;
                    lastDate = date;
                }
            }

            logUpdateCount(sdf, lastSchool, lastDate);
        } finally {
            iterator.close();
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_updateCount = 0;
        m_calendarDates = new HashMap<String, SisSchoolCalendarDate>();
        m_minutesRequired = new HashMap<String, Integer>();
        m_studentOids = new HashMap<PlainDate, Collection<String>>();
        m_studentTransportation = new HashMap<String, Collection<StudentTransportation>>();
        m_scheduleStart = new HashMap<String, PlainTime>();
        m_endDate = (PlainDate) getParameter(END_DATE);
        m_startDate = (PlainDate) getParameter(START_DATE);

        String totalMinutesRequiredParam = (String) getParameter(TOTAL_MINUTES_REQUIRED);
        m_totalMinutesRequired = Integer.parseInt(totalMinutesRequiredParam.trim());

        loadSchoolCalendarDates();
        loadStudents();
        loadStudentTransportation();
        loadStartTimes();
        loadMinutesRequired();
        initializeDistanceLearningCodes();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState()
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        if (userData != null && userData.getCurrentRecord(SisStudent.class) != null) {
            m_studentOid = userData.getCurrentRecord(SisStudent.class).getOid();
        }
    }

    /**
     * Returns the time difference in minutes between two times.
     *
     * @param timeIn PlainTime
     * @param min Integer
     * @return PlainTime
     */
    private PlainTime addTime(PlainTime timeIn, Integer min) {
        PlainTime newTime = null;

        if (timeIn != null && min != null) {
            long minuteLong = min.longValue();
            long longTime = timeIn.getTime();
            minuteLong = minuteLong * (1000 * 60);

            long longTimeNew = longTime + minuteLong;
            newTime = new PlainTime(longTimeNew);
        }

        return newTime;
    }

    /**
     * Prints a log message displaying the number of records that were updated for each school.
     *
     * @param sdf SimpleDateFormat
     * @param school SisSchool
     * @param date PlainDate
     */
    private void logUpdateCount(SimpleDateFormat sdf, SisSchool school, PlainDate date) {
        String messageEnding = "";

        if (school != null && school.getName() != null) {
            messageEnding = " at " + school.getName() + " on " + sdf.format(date) + ".";
        } else {
            messageEnding = ".";
        }

        logMessage(m_updateCount + " attendance record(s) updated" + messageEnding);
        m_updateCount = 0;
    }

    /**
     * Returns the current date and time.
     *
     * @param student SisStudent
     * @return String
     */
    private String getCurrentDate(SisStudent student) {
        Organization organization = student.getSchool().getParentOrganization();

        TimeZone timeZone = OrganizationManager.getTimeZone(organization);
        Calendar calendar = Calendar.getInstance(timeZone);

        DateFormat dateFormat = new SimpleDateFormat("M/d/yyyy h:mm:ss a");
        dateFormat.setCalendar(calendar);
        String changeDate = dateFormat.format(calendar.getTime());

        return changeDate;
    }

    /**
     * Gets the distance learning codes.
     *
     * @return Sets the
     */
    private void initializeDistanceLearningCodes() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(RefAttendanceStudent.COL_CODE_TYPE,
                Integer.valueOf(RefAttendanceStudent.TYPE_OTHER_CODE));
        criteria.addIn(RefAttendanceStudent.COL_STATE_CODE, DIST_LEAN_ATT_CODES);
        QueryByCriteria ratQuery = new QueryByCriteria(RefAttendanceStudent.class, criteria);
        m_distanceLearningRatByCodeMap =
                getBroker().getMapByQuery(ratQuery, RefAttendanceStudent.COL_ATTENDANCE_CODE, 128);

    }

    /**
     * Returns the time difference in minutes between two times.
     *
     * @param timeA PlainTime
     * @param timeB PlainTime
     * @return int
     */
    private int getTimeDifference(PlainTime timeA, PlainTime timeB) {
        Long minutesPresent = Long.valueOf(0);

        if (timeA != null && timeB != null) {
            minutesPresent = Long.valueOf(timeA.getTimeInMinutes() - timeB.getTimeInMinutes());
        }

        return minutesPresent.intValue();
    }

    /**
     * Returns the number of minutes a student is present on a given day.
     *
     * @param att StudentAttendance
     * @param calendarDate SisSchoolCalendarDate
     * @return int
     */
    private int getMinutesPresent(StudentAttendance att, SisSchoolCalendarDate calendarDate) {
        Collection<StudentAttendanceTime> dailyTimes = att.getAttendanceTimes(getBroker());

        int minutesPresent = 0;
        SisStudent student = att.getStudent();
        String schoolOid = student.getSchoolOid();
        String key = schoolOid + calendarDate.getSchoolCalendarOid();

        Integer minutesRequired = m_minutesRequired.get(key);

        PlainTime startTime = m_scheduleStart.get(calendarDate.getBellScheduleOid());
        PlainTime previousTime = m_scheduleStart.get(calendarDate.getBellScheduleOid());
        PlainTime endTime = addTime(startTime, minutesRequired);

        Iterator iterator = dailyTimes.iterator();
        while (iterator.hasNext()) {
            StudentAttendanceTime attTime = (StudentAttendanceTime) iterator.next();
            boolean arrivalIndicator = attTime.getArrivalIndicator();
            PlainTime time = attTime.getTime();

            if (arrivalIndicator && !iterator.hasNext()) {
                minutesPresent += getTimeDifference(endTime, time);
            } else if (arrivalIndicator && iterator.hasNext()) {
                previousTime = time;
            } else {
                minutesPresent += getTimeDifference(time, previousTime);
            }
        }

        return minutesPresent;
    }

    /**
     * Returns whether or not a student has an active transportation record on the given date .
     *
     * @param activeTransportation boolean
     * @param date PlainDate
     * @param studentOid String
     * @return boolean
     */
    private boolean getTransportationStatus(boolean activeTransportation, PlainDate date, String studentOid) {
        Collection<StudentTransportation> studentTrans = m_studentTransportation.get(studentOid);

        if (studentTrans != null) {
            for (StudentTransportation trans : studentTrans) {
                if (trans.getStartDate().compareTo(date) <= 0 && (trans.getEndDate() == null ||
                        trans.getEndDate().compareTo(date) >= 0)) {
                    activeTransportation = true;
                }
            }
        }

        return activeTransportation;
    }

    /**
     * Loads the required time in school to a map keyed off of schoolOid + calendarOid
     *
     * NOTE: Purposely not limiting query to school context due to the possibility of dually
     * enrolled students. If
     * this procedure is run for School B, and a student is primarily enrolled in School A but is
     * dually enrolled
     * in School B, if this procedure is only run for School B, limiting this method to the calendar
     * to only
     * School B will cause the lookup to fail when we try to get the calendar code off of the
     * Student record
     * (which will be pointed to the Calendar record for School A).
     */
    private void loadMinutesRequired() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());

        if (isSchoolContext()) {
            criteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        }

        QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                SchoolCalendar calendar = (SchoolCalendar) iterator.next();
                int minutesRequired = 0;
                String studentDayLength = (String) calendar.getFieldValueByAlias(DOE_STUDENT_DAY_LENGTH);

                if (studentDayLength != null) {
                    // Parsing double first b/c of problems I was seeing with the data at Newport
                    // City, TN
                    double dVal = Double.parseDouble(studentDayLength);
                    minutesRequired = Integer.valueOf((int) dVal).intValue();
                }

                String key = calendar.getSchoolOid() + calendar.getOid();
                m_minutesRequired.put(key, Integer.valueOf(minutesRequired));
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads the school calendar dates to a map keyed on school oid + calendarID + date.
     *
     * NOTE: Purposely not limiting query to school context due to the possibility of dually
     * enrolled students. If
     * this procedure is run for School B, and a student is primarily enrolled in School A but is
     * dually enrolled
     * in School B, if this procedure is only run for School B, limiting this method to the calendar
     * to only
     * School B will cause the lookup to fail when we try to get the calendar code off of the
     * Student record
     * (which will be pointed to the Calendar record for School A).
     */
    private void loadSchoolCalendarDates() {
        X2Criteria calendarCriteria = new X2Criteria();
        calendarCriteria.addEqualTo(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());
        calendarCriteria.addGreaterOrEqualThan(SisSchoolCalendarDate.COL_DATE, m_startDate);
        calendarCriteria.addLessOrEqualThan(SisSchoolCalendarDate.COL_DATE, m_endDate);
        calendarCriteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
        calendarCriteria.addNotEmpty(SisSchoolCalendarDate.COL_BELL_SCHEDULE_OID, getBroker().getPersistenceKey());

        if (isSchoolContext()) {
            calendarCriteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        }

        QueryByCriteria query = new QueryByCriteria(SisSchoolCalendarDate.class, calendarCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                SisSchoolCalendarDate scd = (SisSchoolCalendarDate) iterator.next();

                if (scd.getSchoolCalendar() != null) {
                    String schoolOid = scd.getSchoolCalendar().getSchoolOid();
                    PlainDate date = scd.getDate();
                    String key = schoolOid + scd.getSchoolCalendar().getCalendarId() + date;

                    m_calendarDates.put(key, scd);
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a map of start times keyed on bell schedule oids.
     */
    private void loadStartTimes() {
        Criteria criteria = new Criteria();

        if (isSchoolContext()) {
            criteria.addEqualTo(ScheduleBellPeriod.REL_BELL_SCHEDULE + PATH_DELIMITER + ScheduleBell.REL_SCHEDULE +
                    PATH_DELIMITER + Schedule.COL_SCHOOL_OID, getSchool().getOid());
        }

        QueryByCriteria query = new QueryByCriteria(ScheduleBellPeriod.class, criteria);
        query.addOrderByAscending(ScheduleBellPeriod.COL_PERIOD_NUMBER);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ScheduleBellPeriod bellPeriod = (ScheduleBellPeriod) iterator.next();
                PlainTime time = bellPeriod.getStartTime();
                String bellScheduleOid = bellPeriod.getBellScheduleOid();

                if (m_scheduleStart.get(bellScheduleOid) == null) {
                    m_scheduleStart.put(bellScheduleOid, time);
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a map of dates to studentOids.
     */
    private void loadStudents() {
        Collection<String> studentOids = new ArrayList<String>();
        PlainDate date = null;
        PlainDate lastKey = null;

        Criteria criteria = new Criteria();
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_startDate);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_endDate);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());
        }

        if (m_studentOid != null) {
            criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, m_studentOid);
        }

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        query.addOrderByAscending(StudentAttendance.COL_SCHOOL_OID);
        query.addOrderByAscending(StudentAttendance.COL_DATE);
        query.addOrderByAscending(StudentAttendance.COL_STUDENT_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentAttendance studentAtt = (StudentAttendance) iterator.next();

                if (studentAtt.getStudentOid() != null && studentAtt.getDate() != null) {
                    date = studentAtt.getDate();
                    String studentOid = studentAtt.getStudentOid();

                    PlainDate key = date;

                    if (lastKey != null && !lastKey.equals(key)) {
                        m_studentOids.put(lastKey, studentOids);
                        studentOids = new ArrayList<String>();
                    }

                    studentOids.add(studentOid);

                    lastKey = key;
                }
            }

            // Adds final studentOids collection to map
            m_studentOids.put(lastKey, studentOids);
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads student transportation records to a map keyed off of studentOid + date.
     */
    private void loadStudentTransportation() {
        X2Criteria andCriteria = new X2Criteria();
        X2Criteria criteria = new X2Criteria();
        X2Criteria orCriteria = new X2Criteria();

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentTransportation.COL_SCHOOL_OID, getSchool().getOid());
        }

        if (m_studentOid != null) {
            criteria.addEqualTo(StudentTransportation.COL_STUDENT_OID, m_studentOid);
        }

        orCriteria.addGreaterOrEqualThan(StudentTransportation.COL_END_DATE, m_endDate);
        orCriteria.addLessOrEqualThan(StudentTransportation.COL_START_DATE, m_endDate);
        andCriteria.addEmpty(StudentTransportation.COL_END_DATE, getBroker().getPersistenceKey());
        andCriteria.addOrCriteria(orCriteria);
        criteria.addLessOrEqualThan(StudentTransportation.COL_START_DATE, m_startDate);
        criteria.addAndCriteria(andCriteria);

        QueryByCriteria query = new QueryByCriteria(StudentTransportation.class, criteria);
        m_studentTransportation =
                getBroker().getGroupedCollectionByQuery(query, StudentTransportation.COL_STUDENT_OID, 100);
    }

    /**
     * Sets the Daily Attendance fields.
     *
     * @param studentAtt StudentAttendance
     * @param calendarDate SisSchoolCalendarDate
     * @return boolean
     */
    private boolean setAttendanceFields(StudentAttendance studentAtt, SisSchoolCalendarDate calendarDate) {
        boolean absent = studentAtt.getAbsentIndicator();
        boolean activeTransportation = false;
        boolean appendComment = false;
        boolean dismissed = studentAtt.getDismissedIndicator();
        boolean excused = studentAtt.getExcusedIndicator();
        boolean previousValue = false;
        boolean success = false;
        boolean tardy = studentAtt.getTardyIndicator();

        PlainDate date = calendarDate.getDate();
        SisStudent student = studentAtt.getStudent();

        String previousComment = studentAtt.getComment();
        String newComment = "";
        String stateReportingCode = (String) studentAtt.getFieldValueByAlias(DOE_ATTENDANCE_REPORTED);
        String studentOid = studentAtt.getStudentOid();

        int minutesPresent = getMinutesPresent(studentAtt, calendarDate);
        // String schoolOid = studentAtt.getStudent().getSchoolOid();
        // String key = schoolOid + calendarDate.getSchoolCalendarOid();
        // int minutesRequired = m_minutesRequired.get(key).intValue();
        double portionAbsent = 0.0000;

        // Determines if the student has an active transportation record
        activeTransportation = getTransportationStatus(activeTransportation, date, studentOid);

        // Sets the absent indicator if present for less than 1/2 of the required minutes
        if (dismissed || tardy) {
            if (minutesPresent < (m_totalMinutesRequired)) {
                if (!absent) {
                    absent = true;
                    appendComment = true;
                    previousValue = false;
                }
            } else {
                if (absent) {
                    absent = false;
                    appendComment = true;
                    previousValue = true;
                }
            }
        }

        if (appendComment) {
            newComment = (!StringUtils.isEmpty(previousComment) ? previousComment + " " : "") +
                    "Adjusted absent indicator from " + previousValue + " to " + !previousValue + " at " +
                    getCurrentDate(student) + " by system.";
            studentAtt.setComment(newComment);
        }

        // Sets the state reporting codes. T and X codes only get used when the student is present
        // for greater than
        // 0 (zero) minutes but less than half of required minutes (see absent calculation above)
        boolean setExcusedIndicator = false;
        boolean excusedIndicator = false;
        if (m_distanceLearningRatByCodeMap.containsKey(studentAtt.getOtherCode())) {
            RefAttendanceStudent ratCode = m_distanceLearningRatByCodeMap.get(studentAtt.getOtherCode());
            stateReportingCode = ratCode.getStateCode();
            portionAbsent = ratCode.getPortionAbsent().doubleValue();
            absent = ratCode.getAbsentIndicator();
            switch (ratCode.getExcusedType()) {
                case RefAttendanceStudent.EXCUSED_TYPE_SET:
                    setExcusedIndicator = true;
                    excusedIndicator = true;
                    break;

                case RefAttendanceStudent.EXCUSED_TYPE_UNSET:
                    setExcusedIndicator = true;
                    excusedIndicator = false;
                    break;
            }
        } else if (m_distanceLearningRatByCodeMap.containsKey(studentAtt.getOtherCode02())) {
            RefAttendanceStudent ratCode = m_distanceLearningRatByCodeMap.get(studentAtt.getOtherCode02());
            stateReportingCode = ratCode.getStateCode();
            portionAbsent = ratCode.getPortionAbsent().doubleValue();
            absent = ratCode.getAbsentIndicator();
            switch (ratCode.getExcusedType()) {
                case RefAttendanceStudent.EXCUSED_TYPE_SET:
                    setExcusedIndicator = true;
                    excusedIndicator = true;
                    break;

                case RefAttendanceStudent.EXCUSED_TYPE_UNSET:
                    setExcusedIndicator = true;
                    excusedIndicator = false;
                    break;
            }
        } else if (absent) {
            portionAbsent = 1.0000;

            if (minutesPresent > 0 && activeTransportation) {
                if (excused) {
                    stateReportingCode = "T";
                } else {
                    stateReportingCode = "X";
                }
            } else {
                if (excused) {
                    stateReportingCode = "A";
                } else {
                    stateReportingCode = "U";
                }
            }
        } else {
            stateReportingCode = "P";
        }

        studentAtt.setAbsentIndicator(absent);
        if (setExcusedIndicator) {
            studentAtt.setExcusedIndicator(excusedIndicator);
        }
        studentAtt.setPortionAbsent(new BigDecimal(portionAbsent));
        studentAtt.setFieldValueByAlias(DOE_ATTENDANCE_REPORTED, stateReportingCode);
        studentAtt.setFieldValueByAlias(DOE_PRESENT_MINUTES, Integer.toString(minutesPresent));

        success = saveStudentAttendance(studentAtt);

        return success;
    }

    /**
     * Saves the Daily Attendance record.
     *
     * @param studentAtt StudentAttendance
     * @return boolean
     */
    private boolean saveStudentAttendance(StudentAttendance studentAtt) {
        boolean success = true;

        if (studentAtt != null && studentAtt.isDirty()) {
            try {
                getBroker().saveBeanForced(studentAtt);

                SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
                logMessage("Updated Student Attendance for Student [" +
                        studentAtt.getStudent().getNameView() + "] with LASID [" +
                        studentAtt.getStudent().getLocalId() + "] on " + sdf.format(studentAtt.getDate()) + " at " +
                        studentAtt.getSchool().getName());
                m_updateCount++;
            } catch (Exception ex) {
                logMessage("Error saving changes to Class Attendance record. Please contact Aspen Support.");
                success = false;
            }
        }

        return success;
    }
}
