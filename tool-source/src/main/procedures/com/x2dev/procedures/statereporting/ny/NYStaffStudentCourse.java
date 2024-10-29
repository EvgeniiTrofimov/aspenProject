/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ny;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * New York state procedure for Staff Student Course export.
 *
 * @author X2 Development Corporation
 */

public class NYStaffStudentCourse extends StateReportData {
    // static Logger m_logger = AppGlobals.getLog();

    /**
     * Entity class for Staff Student Course export.
     *
     * @author X2 Development Corporation
     */

    public static class StaffStudentCourseEntity extends StateReportEntity {
        /**
         * Local variables for reporting information.
         */
        private NYStaffStudentCourse m_data;

        private List<StaffStudentCourseRecord> m_staffStudentCourseRecords;
        private List<StudentScheduleSpan> m_studentScheduleSpans;
        private ArrayList<PlainDate> m_scheduleDropDates;

        /**
         * Structure of below map is as follows:
         *
         * *****************************************************************************************
         * ************
         * *Key | Value
         * *SisSchool | Key | Value
         * * ScheduleBell | Key | Value
         * * Period Number | Period Duration
         *
         * *****************************************************************************************
         * ************
         */
        private Map<String, HashMap<String, HashMap<String, Long>>> m_minutesByPeriodBasedOnBellSchedulePerSchool =
                new HashMap<String, HashMap<String, HashMap<String, Long>>>();
        private Map<String, HashMap<String, HashMap<String, PlainTime>>> m_periodStartTimeByBellSchedulePerSchool =
                new HashMap<String, HashMap<String, HashMap<String, PlainTime>>>();
        private Map<String, HashMap<String, HashMap<String, PlainTime>>> m_periodEndTimeByBellSchedulePerSchool =
                new HashMap<String, HashMap<String, HashMap<String, PlainTime>>>();

        private Map<String, HashMap<String, Long>> m_bellPeriodDurationMap =
                new HashMap<String, HashMap<String, Long>>();
        private Map<String, ScheduleDay> m_scheduleDaysMap = new HashMap<String, ScheduleDay>();
        private Map<String, SchedulePeriod> m_schedulePeriodsMap = new HashMap<String, SchedulePeriod>();
        private List<String[]> m_dayPeriodList = new ArrayList<String[]>();

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StaffStudentCourseEntity() {
            // no argument constructor
        }

        public final int START_DATE = 0;
        public final int END_DATE = 1;
        public final int SCHEDULE_DAY = 0;
        public final int SCHEDULE_PERIOD = 1;
        public final int TOTAL_MINUTES = 0;
        public final int TOTAL_MINUTES_WITH_ABSENT_DAYS = 1;

        public final double DEFAULT_LINK_VALUE = 1.00;
        public final String STRING_NO = "N";

        /**
         * This method returns the current staff student course record.
         *
         * @return StaffStudentCourseRecord
         */
        public StaffStudentCourseRecord getStaffStudentCourseRecord() {
            return m_staffStudentCourseRecords.get(getCurrentRow());
        }


        /**
         * Gets the staff student course records.
         *
         * @return the m_staffStudentCourseRecords
         */
        public List<StaffStudentCourseRecord> getStaffStudentCourseRecords() {
            return m_staffStudentCourseRecords;
        }


        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (NYStaffStudentCourse) data;

            SisStudent student = (SisStudent) bean;

            m_staffStudentCourseRecords = new ArrayList<StaffStudentCourseRecord>();
            m_studentScheduleSpans = m_data.m_helper.getStudentScheduleSpans(student);

            populateDropDates();

            for (StudentScheduleSpan studentScheduleSpan : m_studentScheduleSpans) {
                PlainDate spanStartDate = studentScheduleSpan.getEntryDate();
                PlainDate spanExitDate = studentScheduleSpan.getExitDate();

                // First filter if the district start date, and the span entry date are null
                if (m_data.m_schoolYearStartDate != null && spanStartDate != null
                // Then filter if the span date is before the district start date
                        && spanStartDate.before(m_data.m_schoolYearStartDate) ||
                // Or filter if the course is set to be filtered or doesn't meet criteria
                        filterCourse(studentScheduleSpan) ||
                        // Or filter if the student left the district
                        (spanStartDate != null && spanStartDate.equals(spanExitDate))) {
                    continue;
                }
                MasterSchedule masterSchedule = studentScheduleSpan.getSection();
                String sectionOid = masterSchedule.getOid();

                spanExitDate = calcStudentExitDateFromSpanOrEnrollment(studentScheduleSpan, student);

                if (masterSchedule != null) {
                    if (!m_data.m_termCodeMap.containsKey(masterSchedule.getOid())) {
                        populateStartEndDateAndTermCodeForMasterSchedule(masterSchedule);
                    }

                    PlainDate termStartDate = m_data.m_termStartDateMap.get(masterSchedule.getOid());
                    PlainDate termEndDate = m_data.m_termEndDateMap.get(masterSchedule.getOid());

                    String termCode = m_data.m_termCodeMap.get(sectionOid);

                    if (!m_data.m_masterScheduleSchoolMap.containsKey(sectionOid)) {
                        populateMasterScheduleMaps(masterSchedule);
                    }

                    // Course info
                    SchoolCourse schoolCourse = m_data.m_masterScheduleSchoolCourseMap.get(sectionOid);
                    Course course = m_data.m_masterScheduleCourseMap.get(sectionOid);

                    String courseNumber = "";
                    if (course != null) {
                        courseNumber = course.getNumber();
                    }

                    // Section School info
                    SisSchool school = m_data.m_masterScheduleSchoolMap.get(sectionOid);

                    if (schoolCourse != null && school != null) {
                        spanStartDate = calcAdjustedEnrollmentDate(school, student, spanStartDate);
                    }

                    String schoolLocationCode = "";
                    if (school != null) {
                        // FIELD 7: Course Location Code
                        schoolLocationCode = (String) school.getFieldValueByBeanPath(m_data.m_schoolLocationCodeField);

                        String schoolOid = school.getOid();

                        loadBellSchedulesMinutesInfo(schoolOid);

                        m_bellPeriodDurationMap = m_minutesByPeriodBasedOnBellSchedulePerSchool.get(schoolOid);
                        m_scheduleDaysMap = m_data.m_schoolScheduleDaysMap.get(schoolOid);
                        m_schedulePeriodsMap = m_data.m_schoolSchedulePeriodsMap.get(schoolOid);
                        m_dayPeriodList = m_data.getMasterSchedulePeriodsAndDays(masterSchedule);
                    }


                    // FIELD 10: Course Code
                    String sectionCode = calcSectionCode(masterSchedule);

                    // FIELD 17: Exclude from Evaluation Indicator
                    String indicator = calcIndicator(masterSchedule);

                    // FIELD 18: Course Duration Through Assessment Date
                    // TODO: Long Run Time in calcBeforeReportDate - responsible for more than 40%
                    // of entity initialization time
                    long totalMinutesB4ReportDate =
                            calcBeforeReportDate(school, student, masterSchedule, termStartDate, termEndDate);

                    Collection<TeacherSpan> spans = getTeacherSpans(masterSchedule);

                    if (spans != null && !spans.isEmpty()) {
                        for (TeacherSpan span : spans) {
                            PlainDate teacherStartDate = span.getSpanInterval().getStartDate();
                            PlainDate teacherExitDate = span.getSpanInterval().getEndDate();

                            if ((teacherExitDate != null && spanStartDate.after(teacherExitDate)) ||
                                    (teacherStartDate != null && spanExitDate.before(teacherStartDate))) {
                                continue;
                            }
                            PlainDate relStartDate =
                                    spanStartDate.after(teacherStartDate) ? spanStartDate : teacherStartDate;
                            PlainDate relEndDate =
                                    spanExitDate.before(teacherExitDate) ? spanExitDate : teacherExitDate;

                            // Skip the assessment record if the student started after the
                            // assessment of note.
                            if (m_data.m_assessmentAliasDate != null) {
                                if (!m_data.m_reportDate.after(relStartDate)) {
                                    continue;
                                }
                            }

                            StaffStudentCourseRecord staffStudentCrsRecord = m_data.new StaffStudentCourseRecord();
                            staffStudentCrsRecord.setSectionOid(masterSchedule.getOid());
                            // FIELD 1: Staff District Code
                            // Set in the export format.

                            // FIELD 2: Setting Staff ID
                            if (span.getStaff() != null) {
                                staffStudentCrsRecord
                                        .setStaffId((String) span.getStaff().getFieldValueByAlias(ALIAS_TEACH_ID));
                            }

                            // FIELD 3: Student District Code
                            // Set in the export format.

                            // FIELD 4: Setting Student ID
                            staffStudentCrsRecord.setStudentId(student.getLocalId());

                            // FIELD 5: School Year Date
                            // Set in the export format.

                            // FIELD 6: Course District Code
                            // Set in the export format.

                            // FIELD 7: Course Location Code
                            staffStudentCrsRecord.setCourseLocationCode(schoolLocationCode);

                            // FIELD 8: Course Code
                            staffStudentCrsRecord.setCourseCode(courseNumber);

                            // FIELD 9: Supplementary Course Differentiator
                            // Set in the export format.

                            // FIELD 10: Course Code
                            staffStudentCrsRecord.setSectionCode(sectionCode);

                            // FIELD 11: Reporting Date
                            staffStudentCrsRecord.setReportingDate(m_data.m_reportDate);

                            // FIELD 12: Setting relationship start date.
                            // If the teacher's schedule doesn't have a start date, then compare the
                            // student's start date
                            // with the term's start date. whichever is later is the relationship
                            // start date.
                            staffStudentCrsRecord.setRelationshipStartDate(relStartDate);

                            // FIELD 13: Setting relationship end date.
                            // If the teacher's schedule doesn't have an end date, then compare the
                            // student's end date
                            // with the term's end date. whichever is earlier is the relationship
                            // end date.
                            staffStudentCrsRecord.setRelationshipExitDate(relEndDate);

                            // FIELD 14: Enrollment Linkage Duration
                            // FIELD 15: Attendance Linkage Duration
                            // TODO: Long Run Time in calcRelationshipMinutes - responsible for more
                            // than 40% of entity initialization time
                            PlainDate relEndOrReportDate = relEndDate;
                            if (relEndDate == null || relEndDate.after(m_data.m_reportDate)) {
                                relEndOrReportDate = m_data.m_reportDate;
                            }
                            long[] minutes = calcRelationshipMinutes(schoolCourse, school, student, masterSchedule,
                                    relStartDate, relEndOrReportDate);
                            staffStudentCrsRecord.setTotalMinutes(minutes[TOTAL_MINUTES]);
                            staffStudentCrsRecord
                                    .setTotalMinutesWithAbsentDays(minutes[TOTAL_MINUTES_WITH_ABSENT_DAYS]);

                            // FIELD 16: Linkage Duration Adjustment
                            double linkageDurationAdjustment = calcLinkageDurationAdjustment(span.getSchTeacher());

                            staffStudentCrsRecord.setLinkageDurationAdjustment(linkageDurationAdjustment);

                            // FIELD 17: Exclude from Evaluation Indicator
                            staffStudentCrsRecord.setExcludeFromEvaluationIndicator(indicator);

                            // FIELD 18: Course Duration Through Assessment Date
                            staffStudentCrsRecord.setTotalMinutesB4ReportDate(totalMinutesB4ReportDate);

                            // FIELD 19: Term Code
                            staffStudentCrsRecord.setTermCode(termCode);


                            // Add Student Course Record
                            m_staffStudentCourseRecords.add(staffStudentCrsRecord);
                        }
                    }
                }
            }

            setRowCount(m_staffStudentCourseRecords.size());
        }

        /**
         * Aspen enters entry and withdrawals on the same day, because of counting times in course,
         * any adds within
         * the middle of a semester go to the next day active day within their calendar. Unless
         * students first day in school is that date
         *
         * @param school SisSchool
         * @param student SisStudent
         * @param spanStartDate PlainDate
         * @return PlainDate
         */
        private PlainDate calcAdjustedEnrollmentDate(SisSchool school, SisStudent student, PlainDate spanStartDate) {
            PlainDate adjustedDate = null;
            PlainDate calendarStartDate = null;
            StudentEnrollment entry =
                    m_data.m_helper.getEnrollmentForDate(student.getOid(), spanStartDate, StudentEnrollment.ENTRY);

            if ((m_scheduleDropDates.contains(spanStartDate))
                    && (entry == null || !spanStartDate.equals(entry.getEnrollmentDate()))) {
                if (school != null && spanStartDate != null) {
                    Collection<SchoolCalendar> schoolCalendars = m_data.getSchoolCalendars(school);
                    SchoolCalendar activeSchoolCalendar =
                            m_data.getActiveSchoolCalendar(schoolCalendars, student.getCalendarCode());
                    if (activeSchoolCalendar != null) {
                        for (SchoolCalendarDate date : m_data.m_schoolCalendarDatesMap
                                .get(activeSchoolCalendar.getOid())) {
                            if (date.getInSessionIndicator()) {
                                if (date.getDate().after(spanStartDate)
                                        && (adjustedDate == null || adjustedDate.after(date.getDate()))) {
                                    adjustedDate = date.getDate();
                                }

                                if (calendarStartDate == null || calendarStartDate.after(date.getDate())) {
                                    calendarStartDate = date.getDate();
                                }
                            }
                        }
                    }
                }
            }

            if (adjustedDate == null || (calendarStartDate != null && calendarStartDate.equals(spanStartDate))) {
                adjustedDate = spanStartDate;
            }

            return adjustedDate;
        }

        /**
         * Calculate Minutes before Report Date.
         *
         * @param school SisSchool
         * @param student SisStudent
         * @param section MasterSchedule
         * @param termStartDate PlainDate
         * @param termEndDate PlainDate
         * @return long
         */
        private long calcBeforeReportDate(SisSchool school,
                                          SisStudent student,
                                          MasterSchedule section,
                                          PlainDate termStartDate,
                                          PlainDate termEndDate) {
            long totalMinutesB4ReportDate = 0;

            if (termEndDate != null && school != null) {
                PlainDate calendarEndDate =
                        (m_data.m_reportDate.before(termEndDate)) ? m_data.m_reportDate : termEndDate;

                Iterator<SisSchoolCalendarDate> iterator = m_data.new InSessionSchoolCalendarDaysBetweenIterator(school,
                        student, termStartDate, calendarEndDate);

                if (iterator.hasNext()) {
                    while (iterator.hasNext()) {
                        SisSchoolCalendarDate inSessionCalendarDate = iterator.next();
                        PlainDate inSessionDate = inSessionCalendarDate.getDate();

                        if (inSessionCalendarDate != null
                                && !(inSessionDate.equals(m_data.m_reportDate) && m_data.m_excludeReportDate)) {
                            int currentScheduleDay = inSessionCalendarDate.getScheduleDayNumber();
                            ScheduleBell currentBellSchedule = inSessionCalendarDate.getBellSchedule();

                            if (currentBellSchedule != null) {
                                totalMinutesB4ReportDate = totalMinutesB4ReportDate +
                                        getSectionMinutes(currentBellSchedule, currentScheduleDay, section);
                            }
                        }
                    }
                }
            }

            return totalMinutesB4ReportDate;
        }

        /**
         * Gets the section minutes.
         *
         * @param currentBellSchedule ScheduleBell
         * @param currentScheduleDay int
         * @param section MasterSchedule
         * @return long
         */
        private long getSectionMinutes(ScheduleBell currentBellSchedule,
                                       int currentScheduleDay,
                                       MasterSchedule section) {
            long totalSectionMinutes = 0;
            String key = section.getOid() + currentBellSchedule.getOid() + currentScheduleDay;
            if (m_data.m_masterScheduleMinutes.containsKey(key)) {
                totalSectionMinutes = m_data.m_masterScheduleMinutes.get(key).longValue();
            } else {
                if (currentBellSchedule != null) {
                    // The Key for the periodMinutes map is the period Id and the value is the
                    // number of minutes for that period Id.
                    HashMap<String, Long> periodMinutes = m_bellPeriodDurationMap.get(currentBellSchedule.getOid());

                    for (Object[] dayPeriod : m_dayPeriodList) {
                        if (m_dayPeriodList != null) {
                            ScheduleDay scheduleDay = m_scheduleDaysMap.get(dayPeriod[SCHEDULE_DAY]);
                            int day = scheduleDay.getNumber();
                            SchedulePeriod schedulePeriod = m_schedulePeriodsMap.get(dayPeriod[SCHEDULE_PERIOD]);
                            String period = schedulePeriod.getId();

                            // 1. Making sure that the day of the course matches Schedule Day for
                            // the current in-session date.
                            // 2. Making sure that the period of the course exists in the current
                            // in-session date's Bell Schedule.
                            // If conditions 1 and 2 pass, then we know that the course is scheduled
                            // on the current in-session date.
                            if (day == currentScheduleDay && periodMinutes != null
                                    && periodMinutes.containsKey(period)) {
                                long currentPeriodMinutes = (periodMinutes.get(period)).longValue();
                                totalSectionMinutes = totalSectionMinutes + currentPeriodMinutes;
                            }
                        }
                    }
                }
                m_data.m_masterScheduleMinutes.put(key, Long.valueOf(totalSectionMinutes));
            }
            return totalSectionMinutes;
        }

        /**
         * Return the calc indicator.
         *
         * @param masterSchedule MasterSchedule
         * @return String
         */
        private String calcIndicator(MasterSchedule masterSchedule) {
            String indicator = (String) masterSchedule.getFieldValueByBeanPath(m_data.m_sectionExcludeFromEvalIndField);
            if (!(BooleanAsStringConverter.TRUE.equals(indicator))) {
                indicator = STRING_NO;
            }

            return indicator;
        }

        /**
         * Calculate Linkage Duration Adjustment.
         *
         * @param teacherSchedule ScheduleTeacher
         * @return double
         */
        private double calcLinkageDurationAdjustment(ScheduleTeacher teacherSchedule) {
            double linkageDurationAdjustment = DEFAULT_LINK_VALUE;

            String linkageString = (String) teacherSchedule.getFieldValueByAlias(ALIAS_LINKAGE_ADJUSTMENT);
            Double linkageValue = null;
            if (linkageString != null) {
                if (StringUtils.isNumeric(linkageString)) {
                    linkageValue = Double.valueOf(linkageString);
                }
            }

            if (linkageValue != null && linkageValue.doubleValue() == 0) {
                linkageDurationAdjustment = linkageValue.doubleValue();
            }

            return linkageDurationAdjustment;
        }

        /**
         * Calculate Minutes the Student spends with a Teacher.
         *
         * @param schoolCourse SchoolCourse
         * @param school SisSchool
         * @param student SisStudent
         * @param masterSchedule MasterSchedule
         * @param relationshipStartDate PlainDate
         * @param relationshipExitDate PlainDate
         * @return long[]
         */
        private long[] calcRelationshipMinutes(SchoolCourse schoolCourse,
                                               SisSchool school,
                                               SisStudent student,
                                               MasterSchedule masterSchedule,
                                               PlainDate relationshipStartDate,
                                               PlainDate relationshipExitDate) {
            long[] minutes = new long[2];

            long totalMinutes = 0;
            long totalMinutesWithAbsentDays = 0;

            if (schoolCourse != null && school != null) {
                String schoolOid = school.getOid();

                Iterator<SisSchoolCalendarDate> iterator = m_data.new InSessionSchoolCalendarDaysBetweenIterator(school,
                        student, relationshipStartDate, relationshipExitDate);

                if (iterator.hasNext()) {
                    // Calculating FIELD 14 and FIELD 15 for each day the school is in session
                    // between relationship start and end date
                    while (iterator.hasNext()) {
                        SisSchoolCalendarDate inSessionCalendarDate = iterator.next();
                        PlainDate inSessionDate = inSessionCalendarDate.getDate();

                        if (inSessionCalendarDate != null
                                && !(inSessionDate.equals(m_data.m_reportDate)
                                        && m_data.m_excludeReportDate)) {

                            StudentPeriodAttendance attendance = m_data.getStudentAttendanceForDateAndClass(
                                    student.getOid(), inSessionDate, masterSchedule.getOid());
                            StudentAttendance dailyAttendance =
                                    m_data.getStudentAttendanceForDate(student.getOid(), inSessionDate);
                            int currentScheduleDay = inSessionCalendarDate.getScheduleDayNumber();
                            ScheduleBell currentBellSchedule = inSessionCalendarDate.getBellSchedule();

                            // The Key for the periodMinutes map is the period Id and the value is
                            // the number of minutes for that period Id.
                            if (m_bellPeriodDurationMap != null && currentBellSchedule != null
                                    && m_bellPeriodDurationMap.containsKey(currentBellSchedule.getOid())) {
                                String bellScheduleOid = currentBellSchedule.getOid();

                                HashMap<String, Long> periodMinutes = m_bellPeriodDurationMap.get(bellScheduleOid);

                                for (Object[] dayPeriod : m_dayPeriodList) {
                                    if (m_dayPeriodList != null) {
                                        ScheduleDay scheduleDay = m_scheduleDaysMap.get(dayPeriod[SCHEDULE_DAY]);
                                        int day = scheduleDay.getNumber();
                                        SchedulePeriod schedulePeriod =
                                                m_schedulePeriodsMap.get(dayPeriod[SCHEDULE_PERIOD]);
                                        String period = schedulePeriod.getId();

                                        // 1. Making sure that the day of the course matches
                                        // Schedule Day for the current in-session date.
                                        // 2. Making sure that the period of the course exists in
                                        // the current in-session date's Bell Schedule.
                                        // If conditions 1 and 2 pass, then we know that the course
                                        // is scheduled on the current in-session date.
                                        if (day == currentScheduleDay && periodMinutes != null
                                                && periodMinutes.containsKey(period)) {
                                            long currentPeriodMinutes = (periodMinutes.get(period)).longValue();
                                            totalMinutes = totalMinutes + currentPeriodMinutes;

                                            HashMap<String, HashMap<String, PlainTime>> startTimeBellSchedule =
                                                    m_periodStartTimeByBellSchedulePerSchool.get(schoolOid);
                                            HashMap<String, PlainTime> startTimePeriodSchedule =
                                                    startTimeBellSchedule.get(bellScheduleOid);
                                            PlainTime sectionStartTime = startTimePeriodSchedule.get(period);

                                            HashMap<String, HashMap<String, PlainTime>> endTimeBellSchedule =
                                                    m_periodEndTimeByBellSchedulePerSchool.get(schoolOid);
                                            HashMap<String, PlainTime> endTimePeriodSchedule =
                                                    endTimeBellSchedule.get(bellScheduleOid);
                                            PlainTime sectionEndTime = endTimePeriodSchedule.get(period);

                                            if ((attendance == null && dailyAttendance == null) ||
                                                    (attendance == null && dailyAttendance != null
                                                            && !dailyAttendance.getAbsentIndicator())
                                                    ||
                                                    (attendance != null && !masterSchedule.getOid()
                                                            .equals(attendance.getMasterScheduleOid()))
                                                    ||
                                                    (attendance != null && BooleanAsStringConverter.TRUE
                                                            .equals(attendance.getFieldValueByBeanPath(
                                                                    m_data.m_attendanceExemptField)))) {
                                                totalMinutesWithAbsentDays =
                                                        totalMinutesWithAbsentDays + currentPeriodMinutes;
                                            } else if (attendance != null && !attendance.getAbsentIndicator()) {
                                                boolean absentDuringClass = false;
                                                PlainTime studentStartTime = sectionStartTime;
                                                PlainTime studentEndTime = sectionEndTime;

                                                if (attendance.getTardyIndicator()) {
                                                    if (attendance.getTimeIn() != null) {
                                                        // Student came to school after the start of
                                                        // class
                                                        if (attendance.getTimeIn().after(sectionEndTime)) {
                                                            absentDuringClass = true;
                                                        }
                                                        // Student came to school during the class
                                                        else if (attendance.getTimeIn().after(sectionStartTime)
                                                                && attendance.getTimeIn().before(sectionEndTime)) {
                                                            studentStartTime = attendance.getTimeIn();
                                                        }
                                                    }
                                                }

                                                if (attendance.getDismissedIndicator()) {
                                                    if (attendance.getTimeOut() != null) {
                                                        // Student was dismissed from school before
                                                        // the start of class
                                                        if (attendance.getTimeOut().before(sectionStartTime)) {
                                                            absentDuringClass = true;
                                                        }
                                                        // Student was dismissed during class
                                                        else if (attendance.getTimeOut().after(sectionStartTime)
                                                                && attendance.getTimeOut().before(sectionEndTime)) {
                                                            studentEndTime = attendance.getTimeOut();
                                                        }
                                                    }
                                                }

                                                if (!absentDuringClass) {
                                                    totalMinutesWithAbsentDays = totalMinutesWithAbsentDays
                                                            + studentEndTime.getTimeInMinutes()
                                                            - studentStartTime.getTimeInMinutes();
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                        }
                    }
                }
            }

            minutes[TOTAL_MINUTES] = totalMinutes;
            minutes[TOTAL_MINUTES_WITH_ABSENT_DAYS] = totalMinutesWithAbsentDays;

            return minutes;
        }

        /**
         * Calculate Section Code.
         *
         * @param masterSchedule MasterSchedule
         * @return String
         */
        private String calcSectionCode(MasterSchedule masterSchedule) {
            String sectionCode = masterSchedule.getSectionNumber();

            if (m_data.m_sectionCodeOverrideField != null) {
                String overrideCode =
                        (String) masterSchedule.getFieldValueByBeanPath(m_data.m_sectionCodeOverrideField);
                if (!StringUtils.isEmpty(overrideCode)) {
                    sectionCode = overrideCode;
                }
            }

            return sectionCode;
        }

        /**
         * compare the student schedule span and the enrollment to see which date is appropriate.
         * Also looks at if the student left and came back
         *
         * @param studentScheduleSpan StudentScheduleSpan
         * @param student SisStudent
         * @return PlainDate
         */
        private PlainDate calcStudentExitDateFromSpanOrEnrollment(StudentScheduleSpan studentScheduleSpan,
                                                                  SisStudent student) {
            PlainDate exitDate = studentScheduleSpan.getExitDate();
            if (exitDate != null) {
                StudentEnrollment withdrawal =
                        m_data.m_helper.getEnrollmentForDate(student.getOid(), exitDate, StudentEnrollment.WITHDRAWAL);
                if (withdrawal != null) {
                    MasterSchedule masterSchedule = studentScheduleSpan.getSection();
                    SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                    String sectionSchoolOid = schoolCourse.getSchoolOid();

                    if (withdrawal.getSchoolOid().equals(sectionSchoolOid)) {
                        StudentEnrollment entry = m_data.m_helper.getEnrollmentForDate(student.getOid(), exitDate,
                                StudentEnrollment.ENTRY);
                        if (entry == null ||
                                !(!entry.getEnrollmentDate().before(withdrawal.getEnrollmentDate())
                                        && entry.getSchoolOid().equals(sectionSchoolOid))) {
                            exitDate = withdrawal.getEnrollmentDate();
                        }
                    }
                }
            }

            return exitDate;
        }

        /**
         * Removes adjacent teacher spans with similar content.
         *
         * @param inSessionDates List<PlainDate>
         * @param tsList List<TeacherSpan>
         */
        private void coalesceTeacherSpans(List<PlainDate> inSessionDates, List<TeacherSpan> tsList) {
            Collections.sort(tsList);
            if (tsList.size() > 1) {
                Iterator<TeacherSpan> spans = tsList.iterator();
                TeacherSpan previous = spans.next();
                PlainDate previousEndDate = inSessionDates.contains(previous.getSpanInterval().getEndDate())
                        ? previous.getSpanInterval().getEndDate()
                        : previousInsessionDate(inSessionDates, previous.getSpanInterval().getEndDate());
                while (spans.hasNext()) {
                    TeacherSpan span = spans.next();
                    if (((span.getStaff() == null && previous.getStaff() == null) ||
                            (span.getStaff() != null && previous.getStaff() != null
                                    && span.getStaff().equals(previous.getStaff())))
                            &&
                            calcLinkageDurationAdjustment(span.getSchTeacher()) == calcLinkageDurationAdjustment(
                                    previous.getSchTeacher())) {
                        PlainDate previousDate =
                                previousInsessionDate(inSessionDates, span.getSpanInterval().getStartDate());
                        if (!previousDate.after(previousEndDate)) {
                            previous.getSpanInterval().setEndDate(span.getSpanInterval().getEndDate());
                            spans.remove();
                        } else {
                            previous = span;
                        }
                    } else {
                        previous = span;
                    }
                    previousEndDate = inSessionDates.contains(previous.getSpanInterval().getEndDate())
                            ? previous.getSpanInterval().getEndDate()
                            : previousInsessionDate(inSessionDates, previous.getSpanInterval().getEndDate());
                }
            }
        }


        /**
         * Checks for courses to be filtered then caches the results.
         *
         * @param studentScheduleSpan StudentScheduleSpan
         * @return boolean
         */
        private boolean filterCourse(StudentScheduleSpan studentScheduleSpan) {
            boolean filter = false;

            Course course = null;
            MasterSchedule section = studentScheduleSpan.getSection();

            if (section != null) {
                if (!m_data.m_courseFilter.containsKey(section.getOid())) {
                    SchoolCourse schoolCourse = section.getSchoolCourse();
                    if (schoolCourse != null) {
                        course = schoolCourse.getCourse();
                    }

                    if (course != null) {
                        String stateCourseCode = (String) course.getFieldValueByBeanPath(m_data.m_stateCourseCodeField);

                        if (m_data.m_filterCourse == 1 && !StringUtils.isEmpty(m_data.m_courseNumberFilter)) {
                            if (m_data.m_courseNumberFilter.contains(COMMA)) {
                                filter = true;
                                String[] stateCodes = m_data.m_courseNumberFilter.split(COMMA);

                                for (String stateCode : stateCodes) {
                                    if (!StringUtils.isEmpty(stateCode) && stateCode.trim().equals(stateCourseCode)) {
                                        filter = false;
                                    }
                                }
                            } else if (!m_data.m_courseNumberFilter.equals(stateCourseCode)) {
                                filter = true;
                            }
                        }

                        if (!BooleanAsStringConverter.TRUE
                                .equals(course.getFieldValueByBeanPath(m_data.m_courseActiveIndicatorField))) {
                            filter = true;
                        }

                        if (StringUtils.isEmpty(stateCourseCode)) {
                            filter = true;
                        }

                        if (m_data.m_assessmentAliasCriteria != null) {
                            filter = filter || !BooleanAsStringConverter.TRUE
                                    .equals(course.getFieldValueByBeanPath(m_data.m_assessmentAliasCriteria));
                        }
                    } else {
                        filter = true;
                    }

                    m_data.m_courseFilter.put(section.getOid(), Boolean.valueOf(filter));
                } else {
                    filter = m_data.m_courseFilter.get(section.getOid()).booleanValue();
                }
            }

            if (studentScheduleSpan.getEntryDate() != null &&
                    studentScheduleSpan.getEntryDate().equals(studentScheduleSpan.getExitDate())) {
                filter = true;
            }

            return filter;
        }


        /**
         * Populates in session dates for section.
         *
         * @param mst MasterSchedule
         * @return List
         */
        private List<PlainDate> getInSessionDatesForSection(MasterSchedule mst) {
            List<PlainDate> inSessionDates = null;

            if (m_data.m_inSessionDatesMap.containsKey(mst.getOid())) {
                inSessionDates = m_data.m_inSessionDatesMap.get(mst.getOid());
            } else {
                String calendarId = m_data.getMostCommonCalendar(mst);

                inSessionDates = new ArrayList<PlainDate>();

                String sklOid = m_data.m_masterScheduleSchoolMap.get(mst.getOid()).getOid();

                if (!StringUtils.isEmpty(sklOid)) {
                    Collection<SisSchoolCalendarDate> calDate = m_data.getSchoolCalendarDatesLookup(sklOid, calendarId);
                    for (SisSchoolCalendarDate sklCalDate : calDate) {
                        inSessionDates.add(sklCalDate.getDate());
                    }
                    Collections.sort(inSessionDates);
                }
                m_data.m_inSessionDatesMap.put(mst.getOid(), inSessionDates);
            }

            return inSessionDates;
        }

        /**
         * Populates Teacher Span information keyed on master schedule Oid.
         *
         * @param section MasterSchedule
         * @return Collection
         */
        private Collection<TeacherSpan> getTeacherSpans(MasterSchedule section) {
            List<TeacherSpan> tsList = null;
            String sectionOid = section.getOid();
            if (m_data.m_teacherSpanBySection.containsKey(sectionOid)) {
                tsList = m_data.m_teacherSpanBySection.get(sectionOid);
            } else {
                Collection<ScheduleTeacher> teacherSchedules = m_data.getTeacherSections(section);
                tsList = new ArrayList<NYStaffStudentCourse.TeacherSpan>();
                m_data.m_teacherSpanBySection.put(sectionOid, tsList);

                List<PlainDate> inSessionDates = getInSessionDatesForSection(section);
                ArrayList<TeacherSpan> tsListForMtc = null;
                int numOfWithoutToR = 0;
                for (ScheduleTeacher teacherSchedule : teacherSchedules) {
                    tsListForMtc = new ArrayList<TeacherSpan>();

                    if (!CODE_TEACHER_OF_RECORD.equals(teacherSchedule.getRole())) {
                        numOfWithoutToR++;
                        continue;
                    }

                    ScheduleTerm term = m_data.getScheduleTerm(teacherSchedule.getScheduleTermOid());
                    SisStaff staff = teacherSchedule.getStaff();

                    String teacherStartStr =
                            (String) teacherSchedule.getFieldValueByBeanPath(m_data.m_teacherStartDateField);
                    PlainDate teacherStartDate = DateUtils.getDate(teacherStartStr);
                    String teacherExitStr =
                            (String) teacherSchedule.getFieldValueByBeanPath(m_data.m_teacherEndDateField);
                    PlainDate teacherExitDate = DateUtils.getDate(teacherExitStr);

                    if (term != null) {
                        Collection<ScheduleTermDate> termDates = m_data.getScheduleTermDates(term);

                        for (ScheduleTermDate trmDate : termDates) {
                            TeacherSpan tsToAdd = m_data.new TeacherSpan();
                            SpanInterval interval = m_data.new SpanInterval();

                            PlainDate trmStartDate = trmDate.getStartDate();
                            PlainDate trmEndDate = trmDate.getEndDate();

                            if (teacherStartDate != null && teacherStartDate.after(trmStartDate)) {
                                interval.setStartDate(teacherStartDate);
                            } else {
                                interval.setStartDate(trmStartDate);
                            }

                            if (teacherExitDate != null && teacherExitDate.before(trmEndDate)) {
                                interval.setEndDate(teacherExitDate);
                            } else {
                                interval.setEndDate(trmEndDate);
                            }

                            tsToAdd.setStaff(staff);
                            tsToAdd.setSpanInterval(interval);
                            tsToAdd.setSchTeacher(teacherSchedule);

                            if (!tsListForMtc.contains(tsToAdd)) {
                                tsListForMtc.add(tsToAdd);
                            }
                        }
                    } else if (teacherStartDate != null && teacherExitDate != null) {
                        TeacherSpan tsToAdd = m_data.new TeacherSpan();
                        SpanInterval interval = m_data.new SpanInterval();

                        interval.setStartDate(teacherStartDate);
                        interval.setEndDate(teacherExitDate);

                        tsToAdd.setStaff(staff);
                        tsToAdd.setSpanInterval(interval);
                        tsToAdd.setSchTeacher(teacherSchedule);

                        if (!tsListForMtc.contains(tsToAdd)) {
                            tsListForMtc.add(tsToAdd);
                        }
                    }

                    Collection<StaffAttendance> sfas = m_data.getStaffAttendance(staff);
                    Collection<PlainDate> stfAttDates = new ArrayList<PlainDate>();
                    Map<PlainDate, StaffAttendance> attMap = new HashMap<PlainDate, StaffAttendance>();

                    if (sfas != null && m_data.m_absenceInterval != null
                            && sfas.size() >= m_data.m_absenceInterval.intValue()) {
                        for (StaffAttendance sfa : sfas) {
                            stfAttDates.add(sfa.getDate());
                            attMap.put(sfa.getDate(), sfa);
                        }

                        if (inSessionDates != null && !stfAttDates.isEmpty()) {
                            int absIntervalCount = 0;
                            LinkedList<PlainDate> intervalDates = new LinkedList<PlainDate>();
                            Iterator<PlainDate> daysIterator = inSessionDates.iterator();
                            while (daysIterator.hasNext()) {
                                PlainDate date = daysIterator.next();
                                if (stfAttDates.contains(date)) {
                                    absIntervalCount += 1;
                                    intervalDates.add(date);
                                }
                                if (!stfAttDates.contains(date) || (!daysIterator.hasNext())) {
                                    if (absIntervalCount >= m_data.m_absenceInterval.intValue()) {
                                        Map<SisStaff, LinkedList<PlainDate>> subsMap =
                                                getSubsForInterval(attMap, intervalDates);

                                        for (Entry<SisStaff, LinkedList<PlainDate>> subEntry : subsMap.entrySet()) {
                                            SpanInterval newInterval = m_data.new SpanInterval();
                                            newInterval.setStartDate(subEntry.getValue().getFirst());
                                            newInterval.setEndDate(subEntry.getValue().getLast());
                                            SisStaff newStaff = subEntry.getKey();

                                            List<TeacherSpan> tsLoop = new ArrayList<TeacherSpan>(tsListForMtc);
                                            for (TeacherSpan span : tsLoop) {
                                                List<SpanInterval> splits = splitIntervals(inSessionDates,
                                                        span.getSpanInterval(), newInterval);
                                                if (splits.isEmpty()) {
                                                    // change entire interval to substitute
                                                    span.setSpanInterval(newInterval);
                                                    span.setStaff(newStaff);
                                                } else {
                                                    span.setSpanInterval(splits.get(0));
                                                    TeacherSpan newSpan = m_data.new TeacherSpan(newStaff, newInterval,
                                                            teacherSchedule);
                                                    if (!tsListForMtc.contains(newSpan)) {
                                                        tsListForMtc.add(newSpan);
                                                    }
                                                    if (splits.size() > 1) {
                                                        newSpan = m_data.new TeacherSpan(span.getStaff(), splits.get(1),
                                                                span.getSchTeacher());
                                                        if (!tsListForMtc.contains(newSpan)) {
                                                            tsListForMtc.add(newSpan);
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    absIntervalCount = 0;
                                    intervalDates = new LinkedList<PlainDate>();
                                }
                            }
                        }
                    }

                    tsList.addAll(tsListForMtc);
                }
                if (numOfWithoutToR > 0 && numOfWithoutToR == teacherSchedules.size()) {
                    StateReportValidationError error = new StateReportValidationError(this.getEntityName(), "",
                            "Section " + section.getCourseView(), "Missing Teacher of Record");
                    m_data.m_validationErrors.add(error);
                }
                coalesceTeacherSpans(inSessionDates, tsList);
            }
            return tsList;
        }

        /**
         * Build map of Periods and its length for all active bell schedules of a school.
         *
         * @param schoolOid String
         */
        private void loadBellSchedulesMinutesInfo(String schoolOid) {
            if (schoolOid != null && !m_minutesByPeriodBasedOnBellSchedulePerSchool.containsKey(schoolOid)) {
                Collection<ScheduleBell> allActiveBellSchedules = m_data.m_schoolScheduleBellsMap.get(schoolOid);
                HashMap<String, HashMap<String, Long>> bellPeriodDurationMap =
                        new HashMap<String, HashMap<String, Long>>();
                HashMap<String, HashMap<String, PlainTime>> bellPeriodStartTimeMap =
                        new HashMap<String, HashMap<String, PlainTime>>();
                HashMap<String, HashMap<String, PlainTime>> bellPeriodEndTimeMap =
                        new HashMap<String, HashMap<String, PlainTime>>();

                if (allActiveBellSchedules != null) {
                    for (ScheduleBell bell : allActiveBellSchedules) {
                        // The Key for the periodMinutes map is the period Id and the value is the
                        // number of minutes for that period Id.
                        HashMap<String, Long> periodMinutes = new HashMap<String, Long>();
                        HashMap<String, PlainTime> startTime = new HashMap<String, PlainTime>();
                        HashMap<String, PlainTime> endTime = new HashMap<String, PlainTime>();

                        for (ScheduleBellPeriod bellPeriod : m_data.getScheduleBellPeriods(bell)) {
                            if (bellPeriod.getSchedulePeriod() != null) {
                                Long periodMinutesLong = Long.valueOf(bellPeriod.getEndTime().getTimeInMinutes()
                                        - bellPeriod.getStartTime().getTimeInMinutes());
                                periodMinutes.put(bellPeriod.getId(), periodMinutesLong);
                                startTime.put(bellPeriod.getId(), bellPeriod.getStartTime());
                                endTime.put(bellPeriod.getId(), bellPeriod.getEndTime());
                            }
                        }

                        bellPeriodDurationMap.put(bell.getOid(), periodMinutes);
                        bellPeriodStartTimeMap.put(bell.getOid(), startTime);
                        bellPeriodEndTimeMap.put(bell.getOid(), endTime);
                    }
                }

                m_minutesByPeriodBasedOnBellSchedulePerSchool.put(schoolOid, bellPeriodDurationMap);
                m_periodStartTimeByBellSchedulePerSchool.put(schoolOid, bellPeriodStartTimeMap);
                m_periodEndTimeByBellSchedulePerSchool.put(schoolOid, bellPeriodEndTimeMap);
            }
        }

        /**
         * Find the staff member most commonly used during a period of absence.
         *
         * @param attMap Map<PlainDate,StaffAttendance>
         * @param intervalDates LinkedList<PlainDate>
         * @return Map
         */
        private Map<SisStaff, LinkedList<PlainDate>> getSubsForInterval(Map<PlainDate, StaffAttendance> attMap,
                                                                        LinkedList<PlainDate> intervalDates) {
            Map<SisStaff, LinkedList<PlainDate>> staffDates = new HashMap();
            for (PlainDate date : intervalDates) {
                List<StaffAttendanceSub> subs = (List<StaffAttendanceSub>) attMap.get(date).getStaffAttendanceSub();
                if (subs != null) {
                    for (StaffAttendanceSub sub : subs) {
                        SisStaff subStaff = sub.getSubstitute();
                        if (subStaff != null) {
                            if (staffDates.containsKey(subStaff)) {
                                LinkedList<PlainDate> dates = staffDates.get(subStaff);
                                dates.add(date);
                                Collections.sort(dates);
                            } else {
                                LinkedList<PlainDate> dates = new LinkedList<PlainDate>();
                                dates.add(date);
                                staffDates.put(subStaff, dates);
                            }
                        }
                    }
                }
            }

            return staffDates;
        }

        /**
         * Find the next in-session date.
         *
         * @param inSessionDates Collection<PlainDate>
         * @param testDate PlainDate
         * @return PlainDate
         */
        protected PlainDate nextInsessionDate(Collection<PlainDate> inSessionDates, PlainDate testDate) {
            PlainDate result = null;
            for (PlainDate date : inSessionDates) {
                if (date.after(testDate)) {
                    result = date;
                    break;
                }
            }
            return result;
        }

        /**
         * Find the previous in-session date.
         *
         * @param inSessionDates List<PlainDate>
         * @param testDate PlainDate
         * @return PlainDate
         */
        private PlainDate previousInsessionDate(List<PlainDate> inSessionDates, PlainDate testDate) {
            PlainDate result = inSessionDates.get(0);
            for (PlainDate date : inSessionDates) {
                if (!date.before(testDate)) {
                    break;
                }
                result = date;
            }
            return result;
        }


        /**
         * Populates a map of the students drop dates. This is so that we can check if a student
         * left a class on a particular date.
         */
        private void populateDropDates() {
            m_scheduleDropDates = new ArrayList<PlainDate>();

            for (StudentScheduleSpan span : m_studentScheduleSpans) {
                if (span.getExitChange() != null) {
                    m_scheduleDropDates.add(span.getExitDate());
                }
            }
        }

        /**
         * Caching everything associated with this master schedule to reduce the number of database
         * calls.
         *
         * @param masterSchedule MasterSchedule
         */
        private void populateMasterScheduleMaps(MasterSchedule masterSchedule) {
            SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
            m_data.m_masterScheduleSchoolCourseMap.put(masterSchedule.getOid(), schoolCourse);
            m_data.m_masterScheduleSchoolMap.put(masterSchedule.getOid(), schoolCourse.getSchool());
            m_data.m_masterScheduleCourseMap.put(masterSchedule.getOid(), schoolCourse.getCourse());
        }

        /**
         * Caches the term code and beginning and end date for this master schedule.
         *
         * @param masterSchedule MasterSchedule
         */
        private void populateStartEndDateAndTermCodeForMasterSchedule(MasterSchedule masterSchedule) {
            ScheduleTerm term = m_data.getScheduleTerm(masterSchedule.getScheduleTermOid());
            PlainDate termStartDate = null;
            PlainDate termEndDate = null;

            if (term != null) {
                Collection<ScheduleTermDate> termDates = m_data.getScheduleTermDates(term);

                for (ScheduleTermDate schedTermDate : termDates) {
                    if (termStartDate == null || termStartDate.after(schedTermDate.getStartDate())) {
                        m_data.m_termStartDateMap.put(masterSchedule.getOid(), schedTermDate.getStartDate());
                    }
                    if (termEndDate == null || termEndDate.before(schedTermDate.getEndDate())) {
                        m_data.m_termEndDateMap.put(masterSchedule.getOid(), schedTermDate.getEndDate());
                    }
                }

                String termCode = term.getCode();
                String stateTermCode =
                        m_data.lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE, termCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                if (stateTermCode == null) {
                    stateTermCode = termCode;
                }

                m_data.m_termCodeMap.put(masterSchedule.getOid(), stateTermCode);
            }
        }

        /**
         * Return new span intervals for teacher .
         *
         * @param inSessionDates List<PlainDate>
         * @param sched SpanInterval
         * @param absent SpanInterval
         * @return List
         */
        private List<SpanInterval> splitIntervals(List<PlainDate> inSessionDates,
                                                  SpanInterval sched,
                                                  SpanInterval absent) {
            List<SpanInterval> splits = new LinkedList();
            PlainDate startDate;
            PlainDate endDate;

            // add split before absent interval
            if (sched.getStartDate().before(absent.getStartDate())) {
                startDate = sched.getStartDate();
                if (!absent.getStartDate().after(sched.getEndDate())) {
                    endDate = previousInsessionDate(inSessionDates, absent.getStartDate());
                } else {
                    endDate = sched.getEndDate();
                }
                splits.add(m_data.new SpanInterval(startDate, endDate));
            }
            // add split after absent interval
            if (sched.getEndDate().after(absent.getEndDate())) {
                endDate = sched.getEndDate();
                if (sched.getStartDate().after(absent.getEndDate())) {
                    startDate = sched.getStartDate();
                } else {
                    startDate = nextInsessionDate(inSessionDates, absent.getEndDate());
                }
                splits.add(m_data.new SpanInterval(startDate, endDate));
            }
            return splits;
        }
    }


    /**
     * The Class InSessionSchoolCalendarDaysBetweenIterator.
     */
    protected class InSessionSchoolCalendarDaysBetweenIterator implements Iterator<SisSchoolCalendarDate> {
        private Iterator<SisSchoolCalendarDate> m_baseIterator;
        private PlainDate m_endDate;
        private PlainDate m_startDate;
        private SisSchoolCalendarDate m_nextElement;
        private boolean m_hasNext;

        /**
         * Instantiates a new in session school calendar days between iterator.
         *
         * @param school SisSchool
         * @param student SisStudent
         * @param startDate PlainDate
         * @param endDate PlainDate
         */
        public InSessionSchoolCalendarDaysBetweenIterator(SisSchool school, SisStudent student, PlainDate startDate,
                PlainDate endDate) {
            m_baseIterator = null;
            m_nextElement = null;
            m_hasNext = false;
            m_startDate = startDate;
            m_endDate = endDate;
            if (school != null && student != null) {
                Collection<SisSchoolCalendarDate> calendarDates = NYStaffStudentCourse.this
                        .getSchoolCalendarDatesLookup(school.getOid(), student.getCalendarCode());
                if (calendarDates != null) {
                    m_baseIterator = calendarDates.iterator();
                }
            }

            nextMatch();
        }

        /**
         * @see java.util.Iterator#hasNext()
         */
        @Override
        public boolean hasNext() {
            return m_hasNext;
        }

        /**
         * @see java.util.Iterator#next()
         */
        @Override
        public SisSchoolCalendarDate next() {
            if (!m_hasNext) {
                throw new NoSuchElementException();
            }

            return nextMatch();
        }

        /**
         * @see java.util.Iterator#remove()
         */
        @Override
        public void remove() {
            m_baseIterator.remove();
        }

        /**
         * Next match.
         *
         * @return SisSchoolCalendarDate
         */
        private SisSchoolCalendarDate nextMatch() {
            SisSchoolCalendarDate oldMatch = m_nextElement;
            if (m_baseIterator != null) {
                while (m_baseIterator.hasNext()) {
                    SisSchoolCalendarDate candidate = m_baseIterator.next();
                    PlainDate date = candidate.getDate();
                    boolean matches = false;

                    if (m_startDate == null && m_endDate == null) {
                        matches = true;
                    } else if ((date.equals(m_startDate) || date.after(m_startDate)) && m_endDate == null) {
                        matches = true;
                    } else if (m_startDate == null && (date.before(m_endDate) || date.equals(m_endDate))) {
                        matches = true;
                    } else if ((date.equals(m_startDate) || date.after(m_startDate))
                            && (date.before(m_endDate) || date.equals(m_endDate))) {
                        matches = true;
                    }
                    if (matches) {
                        m_hasNext = true;
                        m_nextElement = candidate;
                        return oldMatch;
                    }
                }
            }
            m_hasNext = false;
            return oldMatch;
        }

    }
    /**
     * This class returns the Staff Student Course Records.
     */
    protected class RetrieveStaffStudentCourseDetails implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            NYStaffStudentCourse nyData = (NYStaffStudentCourse) data;
            String param = (String) field.getParameter();
            StaffStudentCourseRecord stfStdCrsRec = ((StaffStudentCourseEntity) entity).getStaffStudentCourseRecord();

            if (CALC_PARAM_STAFF_ID.equals(param)) {
                value = stfStdCrsRec.getStaffId();
            } else if (CALC_PARAM_STUDENT_ID.equals(param)) {
                value = stfStdCrsRec.getStudentId();
            } else if (CALC_PARAM_CRS_LOC_CODE.equals(param)) {
                value = stfStdCrsRec.getCourseLocationCode();
            } else if (CALC_PARAM_CRS_CODE.equals(param)) {
                value = stfStdCrsRec.getCourseCode();
            } else if (CALC_PARAM_SECTION_CODE.equals(param)) {
                value = stfStdCrsRec.getSectionCode();
            } else if (CALC_PARAM_REPORTING_DATE.equals(param)) {
                value = stfStdCrsRec.getReportingDate();
            } else if (CALC_PARAM_REL_START_DATE.equals(param)) {
                value = stfStdCrsRec.getRelationshipStartDate();
            } else if (CALC_PARAM_REL_EXIT_DATE.equals(param)) {
                Date relExitDate = stfStdCrsRec.getRelationshipExitDate();
                // Limit the relationship EndDate for assessment record to the time of the
                // assessment.
                if (relExitDate != null && nyData.m_assessmentAliasDate != null) {
                    relExitDate = nyData.m_reportDate.before(relExitDate) ? nyData.m_reportDate : relExitDate;
                }
                value = relExitDate;
            } else if (CALC_PARAM_TERM_CODE.equals(param)) {
                value = stfStdCrsRec.getTermCode();
            } else if (CALC_PARAM_TOTAL_MINS.equals(param)) {
                long totalMinutes = stfStdCrsRec.getTotalMinutes();
                value = Long.toString(totalMinutes);
            } else if (CALC_PARAM_TOT_MINS_WITH_ABSENT.equals(param)) {
                long totalMinutesWithAbsentDays = stfStdCrsRec.getTotalMinutesWithAbsentDays();
                value = Long.toString(totalMinutesWithAbsentDays);
            } else if (CALC_PARAM_TOT_MINS_B4_RPT_DATE.equals(param)) {
                long totalMinutesB4ReportDate = stfStdCrsRec.getTotalMinutesB4ReportDate();
                value = Long.toString(totalMinutesB4ReportDate);
            } else if (CALC_PARAM_LINK_DUR_ADJ.equals(param)) {
                double linkageDurationAdjustment = stfStdCrsRec.getLinkageDurationAdjustment();
                value = Double.toString(linkageDurationAdjustment);
            } else if (CALC_PARAM_EXCLUDE_FROM_EVAL_IND.equals(param)) {
                value = stfStdCrsRec.getExcludeFromEvaluationIndicator();
            }

            return value;
        }
    }

    /**
     * These validations will conditionally report records where there contain dates assigned to
     * more than
     * one teacher and create validation errors when there is no teacher assigned to in-session
     * dates.
     */
    protected class ValidateAssignments implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {

            StaffStudentCourseEntity nyEntity = (StaffStudentCourseEntity) entity;
            NYStaffStudentCourse nyData = (NYStaffStudentCourse) data;

            String stdOid = nyEntity.getBean().getOid();
            List<StaffStudentCourseRecord> records = nyEntity.getStaffStudentCourseRecords();
            StaffStudentCourseRecord currentRecord = nyEntity.getStaffStudentCourseRecord();
            String sectionOid = currentRecord.getSectionOid();
            Collection errors = new ArrayList<StateReportValidationError>();

            if (getParameter(PARAM_ASSIGN_OVERLAP) != null
                    && ((Boolean) getParameter(PARAM_ASSIGN_OVERLAP)).booleanValue()) {

                Date relStartDate = currentRecord.getRelationshipStartDate();
                Date relExitDate = currentRecord.getRelationshipExitDate();
                // Limit the relationship EndDate for assessment record to the time of the
                // assessment.
                if (relExitDate != null && nyData.m_assessmentAliasDate != null) {
                    relExitDate = nyData.m_reportDate.before(relExitDate) ? nyData.m_reportDate : relExitDate;
                }


                for (StaffStudentCourseRecord record : records) {
                    Date recordStartDate = record.getRelationshipStartDate();

                    if (!StringUtils.isEmpty(record.getStaffId()) && !StringUtils.isEmpty(currentRecord.getStaffId()) &&
                            !currentRecord.getStaffId().equals(record.getStaffId())) {
                        if (recordStartDate.equals(relStartDate) ||
                                recordStartDate.after(relStartDate) && recordStartDate.before(relExitDate)) {
                            StateReportValidationError error = new StateReportValidationError(entity,
                                    field,
                                    "Assigment Overlap",
                                    currentRecord.toString() +
                                            "\n OVERLAPS \n" +
                                            record.toString());
                            errors.add(error);

                        }
                    }
                }

            }

            if (getParameter(PARAM_ASSIGN_GAP) != null && ((Boolean) getParameter(PARAM_ASSIGN_GAP)).booleanValue()) {

                if (StringUtils.isEmpty(nyData.m_tempStdOid) || !nyData.m_tempStdOid.equals(stdOid) ||
                        !nyData.m_tempSectionOid.equals(sectionOid)) {
                    m_tempStdOid = stdOid;
                    m_tempSectionOid = sectionOid;
                    m_checkInSessionGap = true;
                }

                if (m_checkInSessionGap) {
                    List<PlainDate> inSessionDates = m_inSessionDatesMap.get(currentRecord.getSectionOid());

                    Collection<PlainDate> notUsedInSessionDates = new ArrayList<PlainDate>();

                    for (PlainDate inSessionDate : inSessionDates) {

                        if (isNoAssignmentForDate(records, inSessionDate)) {
                            notUsedInSessionDates.add(inSessionDate);
                        }

                    }

                    boolean isNewInterval = false;
                    Map<SpanInterval, StaffStudentCourseRecord> gapMap =
                            new HashMap<NYStaffStudentCourse.SpanInterval, NYStaffStudentCourse.StaffStudentCourseRecord>();

                    PlainDate spanStart = null;
                    Iterator<PlainDate> iter = notUsedInSessionDates.iterator();

                    while (iter.hasNext()) {
                        PlainDate inSessionDate = iter.next();

                        if (spanStart == null || isNewInterval) {
                            spanStart = inSessionDate;
                        }

                        PlainDate nextAfterInSessionDate = nyEntity.nextInsessionDate(inSessionDates, inSessionDate);

                        if (!notUsedInSessionDates.contains(nextAfterInSessionDate)) {
                            SpanInterval newSpanInterval = new SpanInterval(spanStart, inSessionDate);
                            gapMap.put(newSpanInterval, getAdjacentRecordByDate(records, spanStart));
                            isNewInterval = true;
                            if (iter.hasNext()) {
                                iter.next();
                            }
                        }
                    }

                    if (!gapMap.isEmpty()) {
                        for (Entry<SpanInterval, StaffStudentCourseRecord> entry : gapMap.entrySet()) {

                            String msgRecord = entry.getValue() != null
                                    ? entry.getValue().toString()
                                    : "Record was not found.";
                            StateReportValidationError error = new StateReportValidationError(entity,
                                    field,
                                    "Assigment Gap: Start Date: " +
                                            entry.getKey().getStartDate() +
                                            ", " +
                                            "End Date: " +
                                            entry.getKey().getEndDate(),
                                    "Adjacent " + msgRecord);
                            errors.add(error);

                        }

                    }

                    m_checkInSessionGap = false;
                }

                if (StringUtils.isEmpty(currentRecord.getStaffId())) {
                    Date gapDate = currentRecord.getRelationshipStartDate();
                    StaffStudentCourseRecord adjRecord = getAdjacentRecordByDate(records, gapDate);

                    StateReportValidationError error = new StateReportValidationError(entity,
                            field,
                            "Assigment Gap: Start Date: " +
                                    currentRecord.getRelationshipStartDate() +
                                    ", " +
                                    "End Date: " +
                                    currentRecord.getRelationshipExitDate(),
                            "Adjacent " + (adjRecord != null
                                    ? adjRecord.toString()
                                    : "Record was not found."));
                    errors.add(error);
                }
            }

            return errors;
        }

        /**
         * Return adjacent StaffStudentCourseRecord for the date from gap interval.
         *
         * @param records Collection<StaffStudentCourseRecord>
         * @param gapDate Date
         * @return Staff student course record
         */
        private StaffStudentCourseRecord getAdjacentRecordByDate(Collection<StaffStudentCourseRecord> records,
                                                                 Date gapDate) {
            Date candidateDate = null;
            StaffStudentCourseRecord candidateRecord = null;
            for (StaffStudentCourseRecord sscr : records) {
                Date tempDate = sscr.getRelationshipExitDate();

                if (candidateDate == null && gapDate.after(tempDate)) {
                    candidateDate = tempDate;
                    candidateRecord = sscr;
                }

                if (candidateDate != null && candidateDate.before(tempDate) && gapDate.after(tempDate)) {
                    candidateDate = tempDate;
                    candidateRecord = sscr;
                }
            }

            return candidateRecord;
        }

        /**
         * Check if there is assignment for the given in session date.
         *
         * @param records Collection<StaffStudentCourseRecord>
         * @param inSessionDate Date
         * @return true, if is no assignment for date
         */
        private boolean isNoAssignmentForDate(Collection<StaffStudentCourseRecord> records, Date inSessionDate) {
            boolean isNoAssignmentForDate = true;

            for (StaffStudentCourseRecord record : records) {
                Date relStartDate = record.getRelationshipStartDate();
                Date relExitDate = record.getRelationshipExitDate();

                if (inSessionDate.equals(relStartDate) ||
                        inSessionDate.equals(relExitDate) ||
                        inSessionDate.after(relStartDate) && inSessionDate.before(relExitDate)) {
                    isNoAssignmentForDate = false;
                    break;
                }
            }

            return isNoAssignmentForDate;
        }

    }



    /**
     *
     * Helper class to store interval of Teacher Span.
     *
     */
    private class SpanInterval implements Comparable<SpanInterval> {

        private PlainDate m_startDate;
        private PlainDate m_endDate;

        /**
         * Instantiates a new span interval.
         */
        public SpanInterval() {

        }

        /**
         * Instantiates a new span interval.
         *
         * @param startDate PlainDate
         * @param endDate PlainDate
         */
        public SpanInterval(PlainDate startDate, PlainDate endDate) {
            m_startDate = startDate;
            m_endDate = endDate;
        }

        /**
         * Implementing interface's method.
         *
         * @param o SpanInterval
         * @return int
         */
        @Override
        public int compareTo(SpanInterval o) {
            int result = m_startDate.compareTo(o.getStartDate());

            if (result == 0) {
                if (m_endDate != null) {
                    result = m_endDate.compareTo(o.getEndDate());
                }
            }

            return result;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {

            return compareTo((SpanInterval) obj) == 0 ? true : false;
        }

        /**
         * Provide a comparable hash code for HashMap compatibility.
         *
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return Objects.hash(m_startDate, m_endDate);
        }

        /**
         * Gets the end date.
         *
         * @return the m_endDate
         */
        public PlainDate getEndDate() {
            return m_endDate;
        }

        /**
         * Gets the start date.
         *
         * @return the m_startDate
         */
        public PlainDate getStartDate() {
            return m_startDate;
        }

        /**
         * Sets the end date.
         *
         * @param endDate void
         */
        public void setEndDate(PlainDate endDate) {
            this.m_endDate = endDate;
        }

        /**
         * Sets the start date.
         *
         * @param startDate void
         */
        public void setStartDate(PlainDate startDate) {
            this.m_startDate = startDate;
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "SpanInterval [m_startDate=" + m_startDate + ", m_endDate=" + m_endDate + "]";
        }
    }


    /**
     * Inner data object for saving all of the student information during the pre-processing of the
     * export.
     */
    private class StaffStudentCourseRecord {
        private String staffId;
        private String studentId;
        private String courseLocationCode;
        private String courseCode;
        private String sectionCode;
        private String sectionOid;
        private Date reportingDate;
        private Date relationshipStartDate;
        private Date relationshipExitDate;
        private String termCode;
        private long totalMinutes;
        private long totalMinutesWithAbsentDays;
        private long totalMinutesB4ReportDate;
        private double linkageDurationAdjustment;
        private Object excludeFromEvaluationIndicator;

        /**
         * Instantiates a new staff student course record.
         */
        public StaffStudentCourseRecord() {

        }

        /**
         * Gets the staff id.
         *
         * @return String
         */
        public String getStaffId() {
            return staffId;
        }

        /**
         * Sets the staff id.
         *
         * @param staffId void
         */
        public void setStaffId(String staffId) {
            this.staffId = staffId;
        }

        /**
         * Gets the student id.
         *
         * @return String
         */
        public String getStudentId() {
            return studentId;
        }

        /**
         * Sets the student id.
         *
         * @param studentId void
         */
        public void setStudentId(String studentId) {
            this.studentId = studentId;
        }

        /**
         * Gets the course location code.
         *
         * @return String
         */
        public String getCourseLocationCode() {
            return courseLocationCode;
        }

        /**
         * Sets the course location code.
         *
         * @param courseLocationCode void
         */
        public void setCourseLocationCode(String courseLocationCode) {
            this.courseLocationCode = courseLocationCode;
        }

        /**
         * Gets the course code.
         *
         * @return String
         */
        public String getCourseCode() {
            return courseCode;
        }

        /**
         * Sets the course code.
         *
         * @param courseCode void
         */
        public void setCourseCode(String courseCode) {
            this.courseCode = courseCode;
        }

        /**
         * Gets the section code.
         *
         * @return String
         */
        public String getSectionCode() {
            return sectionCode;
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getSectionOid() {
            return sectionOid;
        }

        /**
         * Sets the section code.
         *
         * @param sectionCode void
         */
        public void setSectionCode(String sectionCode) {
            this.sectionCode = sectionCode;
        }

        /**
         * Sets the section oid.
         *
         * @param sectionOid void
         */
        public void setSectionOid(String sectionOid) {
            this.sectionOid = sectionOid;
        }

        /**
         * Gets the reporting date.
         *
         * @return Date
         */
        public Date getReportingDate() {
            return reportingDate;
        }

        /**
         * Sets the reporting date.
         *
         * @param reportingDate void
         */
        public void setReportingDate(Date reportingDate) {
            this.reportingDate = reportingDate;
        }

        /**
         * Gets the relationship start date.
         *
         * @return Date
         */
        public Date getRelationshipStartDate() {
            return relationshipStartDate;
        }

        /**
         * Sets the relationship start date.
         *
         * @param relationshipStartDate void
         */
        public void setRelationshipStartDate(Date relationshipStartDate) {
            this.relationshipStartDate = relationshipStartDate;
        }

        /**
         * Gets the relationship exit date.
         *
         * @return Date
         */
        public Date getRelationshipExitDate() {
            return relationshipExitDate;
        }

        /**
         * Sets the relationship exit date.
         *
         * @param relationshipExitDate void
         */
        public void setRelationshipExitDate(Date relationshipExitDate) {
            this.relationshipExitDate = relationshipExitDate;
        }

        /**
         * Gets the term code.
         *
         * @return String
         */
        public String getTermCode() {
            return termCode;
        }

        /**
         * Sets the term code.
         *
         * @param termCode void
         */
        public void setTermCode(String termCode) {
            this.termCode = termCode;
        }

        /**
         * Gets the total minutes.
         *
         * @return long
         */
        public long getTotalMinutes() {
            return totalMinutes;
        }

        /**
         * Sets the total minutes.
         *
         * @param totalMinutes void
         */
        public void setTotalMinutes(long totalMinutes) {
            this.totalMinutes = totalMinutes;
        }

        /**
         * Gets the total minutes with absent days.
         *
         * @return long
         */
        public long getTotalMinutesWithAbsentDays() {
            return totalMinutesWithAbsentDays;
        }

        /**
         * Sets the total minutes with absent days.
         *
         * @param totalMinutesWithAbsentDays void
         */
        public void setTotalMinutesWithAbsentDays(long totalMinutesWithAbsentDays) {
            this.totalMinutesWithAbsentDays = totalMinutesWithAbsentDays;
        }

        /**
         * Gets the total minutes B 4 report date.
         *
         * @return long
         */
        public long getTotalMinutesB4ReportDate() {
            return totalMinutesB4ReportDate;
        }

        /**
         * Sets the total minutes B 4 report date.
         *
         * @param totalMinutesB4ReportDate void
         */
        public void setTotalMinutesB4ReportDate(long totalMinutesB4ReportDate) {
            this.totalMinutesB4ReportDate = totalMinutesB4ReportDate;
        }

        /**
         * Gets the linkage duration adjustment.
         *
         * @return double
         */
        public double getLinkageDurationAdjustment() {
            return linkageDurationAdjustment;
        }

        /**
         * Sets the linkage duration adjustment.
         *
         * @param linkageDurationAdjustment void
         */
        public void setLinkageDurationAdjustment(double linkageDurationAdjustment) {
            this.linkageDurationAdjustment = linkageDurationAdjustment;
        }

        /**
         * Gets the exclude from evaluation indicator.
         *
         * @return Object
         */
        public Object getExcludeFromEvaluationIndicator() {
            return excludeFromEvaluationIndicator;
        }

        /**
         * Sets the exclude from evaluation indicator.
         *
         * @param excludeFromEvaluationIndicator void
         */
        public void setExcludeFromEvaluationIndicator(Object excludeFromEvaluationIndicator) {
            this.excludeFromEvaluationIndicator = excludeFromEvaluationIndicator;
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "Record: staffId=" + staffId +
                    ", crsCode=" + courseCode +
                    ", sectionCode=" + sectionCode +
                    ", relStartDate=" + relationshipStartDate +
                    ", relEndDate=" + relationshipExitDate + ".";
        }

    }

    /**
     * Helper class to store information about teacher staff and interval.
     *
     */
    private class TeacherSpan implements Comparable<TeacherSpan> {

        private SisStaff m_staff;
        private SpanInterval m_interval;
        private ScheduleTeacher m_schTeacher;

        /**
         * Instantiates a new teacher span.
         *
         * @param staff SisStaff
         * @param interval SpanInterval
         * @param schTeacher ScheduleTeacher
         */
        public TeacherSpan(SisStaff staff, SpanInterval interval, ScheduleTeacher schTeacher) {
            m_staff = staff;
            m_interval = interval;
            m_schTeacher = schTeacher;
        }

        /**
         * Instantiates a new teacher span.
         */
        public TeacherSpan() {

        }

        /**
         * Implementing interface's method.
         *
         * @param o TeacherSpan
         * @return int
         */
        @Override
        public int compareTo(TeacherSpan o) {
            int result;
            if (m_staff == null ^ o.getStaff() == null) {
                result = (m_staff == null) ? -1 : 1;
            } else if (m_staff == null && o.getStaff() == null) {
                result = 0;
            } else {
                result = m_staff.getOid().compareTo(o.getStaff().getOid());
            }
            if (result == 0) {
                result = m_interval.compareTo(o.getSpanInterval());
            }

            return result;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            return compareTo((TeacherSpan) obj) == 0 ? true : false;
        }

        /**
         * Gets the sch teacher.
         *
         * @return the m_schTeacher
         */
        public ScheduleTeacher getSchTeacher() {
            return m_schTeacher;
        }

        /**
         * Gets the span interval.
         *
         * @return the m_startDate
         */
        public SpanInterval getSpanInterval() {
            return m_interval;
        }

        /**
         * Gets the staff.
         *
         * @return the m_staff
         */
        public SisStaff getStaff() {
            return m_staff;
        }

        /**
         * Sets the sch teacher.
         *
         * @param schTeacher void
         */
        public void setSchTeacher(ScheduleTeacher schTeacher) {
            this.m_schTeacher = schTeacher;
        }

        /**
         * Sets the span interval.
         *
         * @param interval void
         */
        public void setSpanInterval(SpanInterval interval) {
            this.m_interval = interval;
        }

        /**
         * Sets the staff.
         *
         * @param staff void
         */
        public void setStaff(SisStaff staff) {
            this.m_staff = staff;
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "TeacherSpan [staff=" + (m_staff == null ? "null" : m_staff.getNameView()) + ", m_interval="
                    + m_interval + "]";
        }
    }

    /**
     * Input Definition Parameters
     */
    protected static final String PARAM_ABSENCE_INTERVAL = "absenceInterval";
    protected static final String PARAM_ASSESSMENT_ALIAS = "assessmentAlias";
    protected static final String PARAM_ASSIGN_GAP = "assignGap";
    protected static final String PARAM_ASSIGN_OVERLAP = "assignOverlap";
    protected static final String PARAM_EXCLUDE_REPORT_DATE = "excludeReportDate";
    protected static final String PARAM_FILTER_COURSES = "filterCourses";
    protected static final String PARAM_FILTER_COURSES_CODE = "filterCoursesCode";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Retriever Parameters
     */
    protected static final String CALC_ID_STAFF_STUDENT_COURSE = "STF-STD-CRS";

    protected static final String CALC_PARAM_CRS_CODE = "CRS-CODE";
    protected static final String CALC_PARAM_CRS_LOC_CODE = "CRS-LOC-CODE";
    protected static final String CALC_PARAM_EXCLUDE_FROM_EVAL_IND = "EXCLUDE-FROM-EVAL-IND";
    protected static final String CALC_PARAM_LINK_DUR_ADJ = "LINK-DUR-ADJ";
    protected static final String CALC_PARAM_REL_EXIT_DATE = "REL-EXIT-DATE";
    protected static final String CALC_PARAM_REL_START_DATE = "REL-START-DATE";
    protected static final String CALC_PARAM_REPORTING_DATE = "REPORTING-DATE";
    protected static final String CALC_PARAM_SECTION_CODE = "SECTION-CODE";
    protected static final String CALC_PARAM_STAFF_ID = "STAFF-ID";
    protected static final String CALC_PARAM_STUDENT_ID = "STUDENT-ID";
    protected static final String CALC_PARAM_TERM_CODE = "TERM-CODE";
    protected static final String CALC_PARAM_TOT_MINS_B4_RPT_DATE = "TOT-MINS-B4-RPT-DATE";
    protected static final String CALC_PARAM_TOT_MINS_WITH_ABSENT = "TOT-MINS-WITH-ABSENT";
    protected static final String CALC_PARAM_TOTAL_MINS = "TOTAL-MINS";

    /**
     * Aliases
     */
    // ORGANIZATION
    protected static final String ALIAS_ASSESSMENT_DATE_PREFIX = "NY ASSESS DATE ";
    protected static final String ALIAS_ASSESSMENT_TYPE_PREFIX = "NY ASSESSMENT ";

    // SCHOOL
    protected static final String ALIAS_LOCATION_CODE = "LOCATION CODE";
    protected static final String ALIAS_SKL_EXLUDE = "DOE EXCLUDE SKL";

    // COURSE
    protected static final String ALIAS_ACTIVE_INDICATOR = "DOE ACTIVE";
    protected static final String ALIAS_STATE_COURSE_CODE = "DOE STATE COURSE";

    // MASTER_SCHEDULE
    protected static final String ALIAS_EXCLUDE_FROM_EVALUATION_INDICATOR = "DOE EXCLUDE EVALUATION";
    protected static final String ALIAS_PLATOON_CODE = "DOE SECTION OVERRIDE";

    // TEACHER_SCHEDULE
    protected static final String ALIAS_LINKAGE_ADJUSTMENT = "DOE LINK DUR ADJUST";
    protected static final String ALIAS_TEACHER_START_DATE = "DOE TEACHER START";
    protected static final String ALIAS_TEACHER_END_DATE = "DOE TEACHER END";

    // STUDENT_PERIOD_ATTENDANCE
    protected static final String ALIAS_IS_EXEMPT = "DOE ATTENDANCE EXEMPT";

    // STAFF
    protected static final String ALIAS_TEACH_ID = "DOE TEACH ID";

    /**
     * Codes
     */
    protected static final String CODE_TEACHER_OF_RECORD = "Teacher of Record";

    /**
     * Error Messages
     */
    protected static final String ERROR_TYPE_WARNING = "Warning";
    protected static final String ERROR_MESSAGE_NO_ACTIVE_STUDENTS =
            "No students were active in the previous School Year.";

    protected static final String VAL_ASSIGNMENTS = "VAL-ASSIGNMENTS";

    /**
     * Primitive members
     */
    protected boolean m_checkInSessionGap = false;
    protected boolean m_excludeReportDate;
    protected boolean m_removeHeaderIndicator;
    protected int m_filterCourse;

    /**
     * Reference types members
     */
    protected Integer m_absenceInterval;
    protected String m_aliasSuffix;
    protected String m_assessmentAliasCriteria;
    protected String m_assessmentAliasDate;
    protected String m_attendanceExemptField;
    protected String m_courseActiveIndicatorField;
    protected String m_courseNumberFilter;
    protected String m_currentContextOid;
    protected String m_numberOfMinutesPerSession;
    protected PlainDate m_reportDate;
    protected String m_schoolLocationCodeField;
    protected PlainDate m_schoolYearStartDate;
    protected ScheduleManager m_scheduleMgr;
    protected String m_sectionCodeOverrideField;
    protected String m_sectionExcludeFromEvalIndField;
    protected String m_sklExludeField;
    protected String m_stateCourseCodeField;
    protected String m_teacherEndDateField;
    protected String m_teacherStartDateField;
    protected String m_tempStdOid;
    protected String m_tempSectionOid;
    protected StudentHistoryHelper m_helper;

    /**
     * Collections
     */
    protected Collection<MasterTerm> m_masterTerms = new ArrayList<MasterTerm>();
    protected Map<String, Collection<MasterScheduleMatrix>> m_scheduleMasterMatrices = new HashMap();
    protected Collection<SchoolCalendar> m_schoolCalendars;

    /**
     * Maps
     */
    protected Map<String, String> m_calCodesForSectionMap = new HashMap<String, String>();
    protected Map<String, PlainDate> m_calendarStartDateMap = new HashMap<String, PlainDate>();
    protected Map<String, Boolean> m_courseFilter = new HashMap<String, Boolean>();
    protected Map<String, List<PlainDate>> m_inSessionDatesMap = new HashMap<String, List<PlainDate>>();
    protected Map<String, Course> m_masterScheduleCourseMap = new HashMap<String, Course>();
    protected Map<String, Long> m_masterScheduleMinutes = new HashMap();
    protected Map<String, SchoolCourse> m_masterScheduleSchoolCourseMap = new HashMap<String, SchoolCourse>();
    protected Map<String, SisSchool> m_masterScheduleSchoolMap = new HashMap<String, SisSchool>();
    protected Map<String, ScheduleBellDay> m_scheduleBellDay = new HashMap<String, ScheduleBellDay>();
    protected Map<String, Collection<ScheduleMatrix>> m_scheduleMatricesMap =
            new HashMap<String, Collection<ScheduleMatrix>>();
    protected Map<String, Collection<SisSchoolCalendarDate>> m_schoolCalendarDatesMap;
    protected Map<String, Collection<ScheduleBell>> m_schoolScheduleBellsMap =
            new HashMap<String, Collection<ScheduleBell>>();
    protected Map<String, Map<String, ScheduleDay>> m_schoolScheduleDaysMap =
            new HashMap<String, Map<String, ScheduleDay>>();
    protected Map<String, Map<String, SchedulePeriod>> m_schoolSchedulePeriodsMap =
            new HashMap<String, Map<String, SchedulePeriod>>();
    protected Map<String, Collection<StaffLeave>> m_staffLeavesMap = new HashMap<String, Collection<StaffLeave>>();
    protected Map<String, SisStaff> m_staffs = new HashMap<String, SisStaff>();
    protected Map<String, Map<PlainDate, Map<String, StudentPeriodAttendance>>> m_studentAttendanceMap =
            new HashMap<String, Map<PlainDate, Map<String, StudentPeriodAttendance>>>();
    protected Map<String, Map<PlainDate, StudentAttendance>> m_studentDailyAttendanceMap =
            new HashMap<String, Map<PlainDate, StudentAttendance>>();
    protected Map<String, List<TeacherSpan>> m_teacherSpanBySection = new HashMap<String, List<TeacherSpan>>();
    protected Map<String, String> m_termCodeMap = new HashMap<String, String>();
    protected Map<String, PlainDate> m_termEndDateMap = new HashMap<String, PlainDate>();
    protected Map<String, PlainDate> m_termStartDateMap = new HashMap<String, PlainDate>();
    protected Map<String, Set<String>> m_verifiedSchoolCalendars = new HashMap();

    private static final String COMMA = ",";

    private Map<String, String> m_mostCommonCalendarSection = new HashMap();
    private Map<String, ScheduleTerm> m_scheduleTerms = new HashMap();
    private Map<String, Collection<ScheduleBellPeriod>> m_scheduleBellPeriods = new HashMap();
    private Map<String, Collection<ScheduleTermDate>> m_scheduleTermDates = new HashMap();
    private Map<String, List<String[]>> m_sectionPeriods = new HashMap();
    private Map<String, Collection<StaffAttendance>> m_staffAttendances = new HashMap();
    private Map<String, Collection<SisSchoolCalendarDate>> m_schoolCalendarDates = new HashMap();
    private Map<String, Collection<SchoolCalendar>> m_schoolCalendarsMap = new HashMap();
    private Map<String, Collection<ScheduleTeacher>> m_teacherSections = new HashMap();
    private Collection<StateReportValidationError> m_validationErrors = new ArrayList<>();

    /**
     * Allows user to determine if the export needs to have a header at runtime.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        if (m_removeHeaderIndicator) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * basic initialize method .
     *
     * @throws X2BaseException exception
     */
    @Override
    public void initialize() throws X2BaseException {
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build helper object.
             */
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);
            m_scheduleMgr = new ScheduleManager(getBroker());
            X2Criteria studentCritiera = m_helper.getStudentCriteria();

            // Check Student count
            QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCritiera);
            int studentCount = getBroker().getCount(studentQuery);
            if (studentCount == 0) {
                addSetupError(ERROR_TYPE_WARNING, ERROR_MESSAGE_NO_ACTIVE_STUDENTS);
            }

            /*
             * Load lookup tables
             */
            loadSchoolCalendars();

            loadSchoolScheduleDays();

            loadSchoolSchedulePeriods();

            loadSchoolScheduleBells();

            loadScheduleMatrix();

            loadStaffs();

            loadStaffLeaves();

            // MAPS
            loadScheduleTeacherByMSTOid();
            loadSchoolCalendarsBySKLOid();
            loadScheduleTerms();
            loadStaffAttendancesBySTFOid();
            loadScheduleTermDatesByTRMOid();
            loadScheduleBellPeriodsByBELOid();
            loadMostCommonCalendarSection();
        }

        if (getSetupErrors().size() == 0) {
            buildStudentAttendance();

            buildStudentDailyAttendance();

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(true));
            setEntityClass(StaffStudentCourseEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_STAFF_STUDENT_COURSE, new RetrieveStaffStudentCourseDetails());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(VAL_ASSIGNMENTS, new ValidateAssignments());
            super.addValidators(validators);
        }
    }

    /**
     * basic initialize fields.
     *
     * @throws X2BaseException exception
     */
    public void initializeFields() throws X2BaseException {
        // System Parameters
        m_schoolYearStartDate = getOrganization().getCurrentContext().getStartDate();
        m_currentContextOid = ((SisOrganization) getOrganization()).getCurrentContextOid();

        // Load Parameters
        m_absenceInterval = (Integer) getParameter(PARAM_ABSENCE_INTERVAL);

        if (m_absenceInterval == null) {
            m_absenceInterval = Integer.valueOf(10);
        }

        m_filterCourse = 0;
        if (getParameter(PARAM_FILTER_COURSES) != null) {
            String filterCourse = (String) getParameter(PARAM_FILTER_COURSES);
            if (StringUtils.isNumeric(filterCourse)) {
                m_filterCourse = Integer.valueOf(filterCourse).intValue();
            }
        }
        m_courseNumberFilter = (String) getParameter(PARAM_FILTER_COURSES_CODE);
        m_excludeReportDate = true;
        if (getParameter(PARAM_EXCLUDE_REPORT_DATE) != null) {
            m_excludeReportDate = ((Boolean) getParameter(PARAM_EXCLUDE_REPORT_DATE)).booleanValue();
        }
        m_removeHeaderIndicator = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeaderIndicator = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }
        m_aliasSuffix = (String) getParameter(PARAM_ASSESSMENT_ALIAS);

        // Use the input def to translate the right alias, only using the end of the alias so I can
        // reuse it
        Organization organization = getOrganization();
        if (m_aliasSuffix != null) {
            m_assessmentAliasCriteria = translateAliasToJavaName(ALIAS_ASSESSMENT_TYPE_PREFIX + m_aliasSuffix, true);
            m_assessmentAliasDate = translateAliasToJavaName(ALIAS_ASSESSMENT_DATE_PREFIX + m_aliasSuffix, true);
            String assessmentDateStr = (String) organization.getFieldValueByBeanPath(m_assessmentAliasDate);
            if (assessmentDateStr != null) {
                m_reportDate = DateUtils.getDate(assessmentDateStr);
            }
        }

        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
            if (getParameter(PARAM_REPORT_DATE) != null) {
                m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
            }
        }

        // Load Alias database field Names
        m_attendanceExemptField = translateAliasToJavaName(ALIAS_IS_EXEMPT, true);
        m_courseActiveIndicatorField = translateAliasToJavaName(ALIAS_ACTIVE_INDICATOR, true);
        m_schoolLocationCodeField = translateAliasToJavaName(ALIAS_LOCATION_CODE, true);
        m_sectionCodeOverrideField = translateAliasToJavaName(ALIAS_PLATOON_CODE, false);
        m_sectionExcludeFromEvalIndField = translateAliasToJavaName(ALIAS_EXCLUDE_FROM_EVALUATION_INDICATOR, true);
        m_stateCourseCodeField = translateAliasToJavaName(ALIAS_STATE_COURSE_CODE, true);
        m_teacherEndDateField = translateAliasToJavaName(ALIAS_TEACHER_END_DATE, true);
        m_teacherStartDateField = translateAliasToJavaName(ALIAS_TEACHER_START_DATE, true);
        m_sklExludeField = translateAliasToJavaName(ALIAS_SKL_EXLUDE, true);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#postProcess()
     */
    @Override
    public Collection<StateReportValidationError> postProcess() {
        return m_validationErrors;
    }

    /**
     * Get a Section's ScheduleDat and SchedulePeriod table entries.
     *
     * @param section MasterSchedule
     * @return List<String[]>
     */
    List<String[]> getMasterSchedulePeriodsAndDays(MasterSchedule section) {
        List<String[]> dayPeriodList = null;

        if (m_sectionPeriods.containsKey(section.getOid())) {
            dayPeriodList = m_sectionPeriods.get(section.getOid());
        } else {
            dayPeriodList = new ArrayList<String[]>();
            String scheduleMasterTermOid = null;
            for (MasterTerm masterTerm : m_masterTerms) {
                if (masterTerm.getMasterScheduleOid().equals(section.getOid())
                        && masterTerm.getScheduleTermOid().equals(section.getScheduleTermOid())) {
                    scheduleMasterTermOid = masterTerm.getOid();
                }
            }

            if (scheduleMasterTermOid != null) {
                Collection<MasterScheduleMatrix> scheduleMasterMatrices =
                        m_scheduleMasterMatrices.get(scheduleMasterTermOid);
                if (scheduleMasterMatrices != null) {
                    for (MasterScheduleMatrix masterScheduleMatrix : scheduleMasterMatrices) {
                        if (m_scheduleMatricesMap.containsKey(masterScheduleMatrix.getScheduleMatrixOid())) {
                            Collection<ScheduleMatrix> scheduleMatrices =
                                    m_scheduleMatricesMap.get(masterScheduleMatrix.getScheduleMatrixOid());
                            if (scheduleMatrices != null) {
                                for (ScheduleMatrix scheduleMatrix : scheduleMatrices) {
                                    String[] oids = new String[] {scheduleMatrix.getScheduleDayOid(),
                                            scheduleMatrix.getSchedulePeriodOid()};
                                    dayPeriodList.add(oids);
                                }
                            }
                        }
                    }
                }
            }
            m_sectionPeriods.put(section.getOid(), dayPeriodList);
        }

        return dayPeriodList;
    }

    /**
     * Get the list of SisSchoolCalendarDate's for a specific schoolOid and calendarId.
     *
     * @param schoolOid String
     * @param calendarId String
     * @return Collection<SisSchoolCalendarDate>
     */
    Collection<SisSchoolCalendarDate> getSchoolCalendarDatesLookup(String schoolOid, String calendarId) {
        Collection<SisSchoolCalendarDate> dates = null;
        String key = schoolOid + calendarId;
        if (m_schoolCalendarDates.containsKey(key)) {
            dates = m_schoolCalendarDates.get(key);
        } else {
            dates = new HashSet();
            String SchoolCalendarOid = null;

            for (SchoolCalendar schoolCalendar : m_schoolCalendars) {
                if (schoolCalendar.getSchoolOid().equals(schoolOid)
                        && m_currentContextOid.equals(schoolCalendar.getDistrictContextOid())
                        && schoolCalendar.getCalendarId().equals(calendarId)) {
                    SchoolCalendarOid = schoolCalendar.getOid();
                }
            }

            if (SchoolCalendarOid != null) {
                Collection<SisSchoolCalendarDate> schoolCalendarDates = m_schoolCalendarDatesMap.get(SchoolCalendarOid);
                if (schoolCalendarDates != null) {
                    dates.addAll(schoolCalendarDates);
                }
            }
            m_schoolCalendarDates.put(key, dates);
        }


        return dates;
    }

    /**
     * Get active school calendar based on the student's calendar code and the current organization
     * context.
     *
     * @param schoolCalendars Collection<SchoolCalendar>
     * @param studentCalendarCode String
     * @return SchoolCalendar
     */
    protected SchoolCalendar getActiveSchoolCalendar(Collection<SchoolCalendar> schoolCalendars,
                                                     String studentCalendarCode) {
        SchoolCalendar currentSchoolCalendar = null;

        /*
         * Find the school calendar which is equal to the current context.
         * Once you have the school calendar, get the list of all in session dates
         */
        for (SchoolCalendar schoolCalendar : schoolCalendars) {
            if (m_currentContextOid != null && m_currentContextOid.equals(schoolCalendar.getDistrictContextOid())
                    && studentCalendarCode != null && studentCalendarCode.equals(schoolCalendar.getCalendarId())) {
                currentSchoolCalendar = schoolCalendar;
                break;
            }
        }

        return currentSchoolCalendar;
    }

    /**
     * Cache most common calendar for sections .
     *
     * @param mst MasterSchedule
     * @return String
     */
    protected String getMostCommonCalendar(MasterSchedule mst) {
        String calendarId = null;
        if (m_mostCommonCalendarSection.containsKey(mst.getOid())) {
            calendarId = m_mostCommonCalendarSection.get(mst.getOid());
        } else {
            MasterSchedule[] msts = new MasterSchedule[] {mst};

            calendarId = m_scheduleMgr.getMostCommonCalendar(mst.getSchedule(), Arrays.asList(msts));
            m_mostCommonCalendarSection.put(mst.getOid(), calendarId);
        }
        return calendarId;
    }

    /**
     * Cache schedule bell periods.
     *
     * @param bell ScheduleBell
     * @return Collection
     */
    protected Collection<ScheduleBellPeriod> getScheduleBellPeriods(ScheduleBell bell) {
        Collection<ScheduleBellPeriod> periods = null;
        if (m_scheduleBellPeriods.containsKey(bell.getOid())) {
            periods = m_scheduleBellPeriods.get(bell.getOid());
        } else {
            periods = bell.getScheduleBellPeriods();
            m_scheduleBellPeriods.put(bell.getOid(), periods);
        }
        return periods;
    }

    /**
     * Cache schedule terms for ScheduleTeacher.
     *
     * @param trmOid String
     * @return Schedule term
     */
    protected ScheduleTerm getScheduleTerm(String trmOid) {
        ScheduleTerm term = null;
        if (m_scheduleTerms.containsKey(trmOid)) {
            term = m_scheduleTerms.get(trmOid);
        } else {
            term = (ScheduleTerm) getBroker().getBeanByOid(ScheduleTerm.class, trmOid);
            m_scheduleTerms.put(trmOid, term);
        }
        return term;
    }


    /**
     * Cache schedule term dates.
     *
     * @param term ScheduleTerm
     * @return Collection
     */
    protected Collection<ScheduleTermDate> getScheduleTermDates(ScheduleTerm term) {
        Collection<ScheduleTermDate> termDates = null;
        if (m_scheduleTermDates.containsKey(term.getOid())) {
            termDates = m_scheduleTermDates.get(term.getOid());
        } else {
            termDates = term.getScheduleTermDates();
            m_scheduleTermDates.put(term.getOid(), termDates);
        }
        return termDates;
    }

    /**
     * Get school calendars for a school.
     *
     * @param school SisSchool
     * @return Collection<SchoolCalendar>
     */
    protected Collection<SchoolCalendar> getSchoolCalendars(SisSchool school) {
        Collection<SchoolCalendar> calendars = null;
        if (m_schoolCalendarsMap.containsKey(school.getOid())) {
            calendars = m_schoolCalendarsMap.get(school.getOid());
        } else {
            calendars = new ArrayList<SchoolCalendar>();

            for (SchoolCalendar SchoolCalendar : m_schoolCalendars) {
                if (SchoolCalendar.getSchoolOid().equals(school.getOid())) {
                    calendars.add(SchoolCalendar);
                }
            }
            m_schoolCalendarsMap.put(school.getOid(), calendars);
        }

        return calendars;
    }

    /**
     * Gathers the first day of a school calendar and caches it for future calls.
     *
     * @param school SisSchool
     * @param calendarId String
     * @return PlainDate
     */
    protected PlainDate getSchoolStartDateForCalendar(SisSchool school, String calendarId) {
        PlainDate startDate = m_calendarStartDateMap.get(calendarId);

        if (startDate == null) {
            Collection<SchoolCalendar> calendars = getSchoolCalendars(school);

            for (SchoolCalendar schoolCalendar : calendars) {
                if (schoolCalendar.getCalendarId().equals(calendarId)) {
                    for (SchoolCalendarDate date : m_schoolCalendarDatesMap.get(schoolCalendar.getOid())) {
                        if (date.getInSessionIndicator() && (startDate == null || date.getDate().before(startDate))) {
                            startDate = date.getDate();
                        }
                    }
                }
            }

            m_calendarStartDateMap.put(calendarId, startDate);
        }

        return startDate;
    }

    /**
     * Cache staff attendance.
     *
     * @param staff SisStaff
     * @return Collection
     */
    protected Collection<StaffAttendance> getStaffAttendance(SisStaff staff) {
        return m_staffAttendances.get(staff.getOid());
    }

    /**
     * Uses the student ID to get a map of their attendances by date, then gets the attendance for
     * the date provided, then gets the attendance
     * that matches the master schedule oid.
     *
     * @param studentOid String
     * @param inSessionDate PlainDate
     * @param masterScheduleOid String
     * @return StudentPeriodAttendance
     */
    protected StudentPeriodAttendance getStudentAttendanceForDateAndClass(String studentOid,
                                                                          PlainDate inSessionDate,
                                                                          String masterScheduleOid) {
        StudentPeriodAttendance attendance = null;
        Map<PlainDate, Map<String, StudentPeriodAttendance>> attendanceMap = m_studentAttendanceMap.get(studentOid);

        if (attendanceMap != null) {
            Map<String, StudentPeriodAttendance> studentMap = attendanceMap.get(inSessionDate);

            if (studentMap != null) {
                attendance = studentMap.get(masterScheduleOid);
            }
        }

        return attendance;
    }

    /**
     * Uses the student ID to get a map of their daily attendances by date,
     * then gets the attendance for the date provided.
     *
     * @param studentOid String
     * @param inSessionDate PlainDate
     * @return StudentAttendance
     */
    protected StudentAttendance getStudentAttendanceForDate(String studentOid, PlainDate inSessionDate) {
        StudentAttendance attendance = null;
        Map<PlainDate, StudentAttendance> attendanceMap = m_studentDailyAttendanceMap.get(studentOid);

        if (attendanceMap != null) {
            attendance = attendanceMap.get(inSessionDate);
        }

        return attendance;
    }

    /**
     * Cache teacher sections.
     *
     * @param masterSchedule MasterSchedule
     * @return Collection
     */
    protected Collection<ScheduleTeacher> getTeacherSections(MasterSchedule masterSchedule) {
        Collection<ScheduleTeacher> teacherSchedules = null;
        if (m_teacherSections.containsKey(masterSchedule.getOid())) {
            teacherSchedules = m_teacherSections.get(masterSchedule.getOid());
        } else {
            teacherSchedules = masterSchedule.getTeacherSections();
            m_teacherSections.put(masterSchedule.getOid(), teacherSchedules);
        }
        return teacherSchedules;
    }

    /**
     * Check if a staff Member is on Leave .
     *
     * @param staffOid String
     * @param inSessionDate PlainDate
     * @return boolean
     */
    protected boolean isStaffOnLeave(String staffOid, PlainDate inSessionDate) {
        boolean onLeave = false;

        Collection<StaffLeave> leaves = m_staffLeavesMap.get(staffOid);

        if (leaves != null && inSessionDate != null) {
            for (StaffLeave staffLeave : leaves) {
                if ((inSessionDate.equals(staffLeave.getStartDate()) || inSessionDate.after(staffLeave.getStartDate()))
                        && (inSessionDate.equals(staffLeave.getEndDate())
                                || inSessionDate.before(staffLeave.getEndDate()))) {
                    onLeave = true;
                    break;
                }
            }
        }

        return onLeave;
    }

    /**
     * Creates a map of students and their attendance for pre-processing. The point is so that we
     * can check a students class attendance without having to
     * access the database. Stores the data by student OID, then the date, which has all absences
     * for that date.
     */
    private void buildStudentAttendance() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentPeriodAttendance.COL_STUDENT_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria()));
        criteria.addGreaterOrEqualThan(StudentPeriodAttendance.COL_DATE, m_schoolYearStartDate);
        criteria.addLessOrEqualThan(StudentPeriodAttendance.COL_DATE, m_reportDate);

        QueryByCriteria query = new QueryByCriteria(StudentPeriodAttendance.class, criteria);
        query.addOrderBy(StudentPeriodAttendance.COL_STUDENT_OID, true);
        query.addOrderBy(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID, true);
        query.addOrderBy(StudentPeriodAttendance.COL_DATE, true);

        QueryIterator absenceIterator = getBroker().getIteratorByQuery(query);
        while (absenceIterator.hasNext()) {
            StudentPeriodAttendance attendance = (StudentPeriodAttendance) absenceIterator.next();
            Map<PlainDate, Map<String, StudentPeriodAttendance>> attendanceMap =
                    m_studentAttendanceMap.get(attendance.getStudentOid());
            Map<String, StudentPeriodAttendance> dateMap = null;

            if (attendanceMap == null) {
                attendanceMap = new HashMap<PlainDate, Map<String, StudentPeriodAttendance>>();
                dateMap = new HashMap<String, StudentPeriodAttendance>();
            } else {
                dateMap = attendanceMap.get(attendance.getDate());
                if (dateMap == null) {
                    dateMap = new HashMap<String, StudentPeriodAttendance>();
                }
            }

            dateMap.put(attendance.getMasterScheduleOid(), attendance);
            attendanceMap.put(attendance.getDate(), dateMap);

            m_studentAttendanceMap.put(attendance.getStudentOid(), attendanceMap);
        }
    }

    /**
     * Creates a map of students and their attendance for pre-processing. The point is so that we
     * can check a students daily attendance without having to
     * access the database. Stores the data by student OID, then the date, which has all absences
     * for that date.
     */
    private void buildStudentDailyAttendance() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentAttendance.COL_STUDENT_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria()));
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_schoolYearStartDate);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        query.addOrderBy(StudentAttendance.COL_STUDENT_OID, true);
        query.addOrderBy(StudentAttendance.COL_DATE, true);

        QueryIterator absenceIterator = getBroker().getIteratorByQuery(query);

        while (absenceIterator.hasNext()) {
            StudentAttendance attendance = (StudentAttendance) absenceIterator.next();
            Map<PlainDate, StudentAttendance> attendanceMap =
                    m_studentDailyAttendanceMap.get(attendance.getStudentOid());

            if (attendanceMap == null) {
                attendanceMap = new HashMap<PlainDate, StudentAttendance>();
            }

            attendanceMap.put(attendance.getDate(), attendance);

            m_studentDailyAttendanceMap.put(attendance.getStudentOid(), attendanceMap);
        }
    }

    /**
     * Load the most common calendars for the schedules selected that have students assigned.
     */

    private void loadMostCommonCalendarSection() {
        X2Criteria sscCriteria = new X2Criteria();

        sscCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_DISTRICT_CONTEXT_OID, m_currentContextOid);

        // From active Schedule
        sscCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        // check school or organization selection.
        if (isSchoolContext()) {
            sscCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            sscCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            sscCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        sscCriteria.addNotEqualTo(
                StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER + m_sklExludeField,
                Boolean.TRUE);
        String[] columns = new String[] {StudentSchedule.COL_SECTION_OID,
                StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_CALENDAR_CODE,
                StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                "count(*)"};

        processSectionCalendars(StudentSchedule.class, columns, sscCriteria);

        // Add student schedule change
        X2Criteria sccCriteria = new X2Criteria();

        sccCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_DISTRICT_CONTEXT_OID, m_currentContextOid);

        // From active Schedule
        sccCriteria.addEqualToField(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentScheduleChange.COL_SCHEDULE_OID);

        // check school or organization selection.
        if (isSchoolContext()) {
            sccCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            sccCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            sccCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        sccCriteria.addNotEqualTo(
                StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER + m_sklExludeField,
                Boolean.TRUE);

        columns = new String[] {StudentScheduleChange.COL_MASTER_SCHEDULE_OID,
                StudentScheduleChange.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_CALENDAR_CODE,
                StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                "count(*)"};

        processSectionCalendars(StudentScheduleChange.class, columns, sccCriteria);
    }

    /**
     * Load ScheduleBellPeriods map keyed on BEL Oid.
     */
    private void loadScheduleBellPeriodsByBELOid() {
        X2Criteria bpeCriteria = new X2Criteria();

        bpeCriteria.addEqualTo(ScheduleBellPeriod.REL_SCHEDULE_PERIOD + PATH_DELIMITER +
                SchedulePeriod.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_DISTRICT_CONTEXT_OID, m_currentContextOid);

        // From active Schedule
        bpeCriteria.addEqualToField(ScheduleBellPeriod.REL_SCHEDULE_PERIOD + PATH_DELIMITER +
                SchedulePeriod.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleBellPeriod.REL_SCHEDULE_PERIOD + PATH_DELIMITER +
                        SchedulePeriod.COL_SCHEDULE_OID);

        // check school or organization selection.
        if (isSchoolContext()) {
            bpeCriteria.addEqualTo(ScheduleBellPeriod.REL_SCHEDULE_PERIOD + PATH_DELIMITER +
                    SchedulePeriod.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            bpeCriteria.addNotEqualTo(ScheduleBellPeriod.REL_SCHEDULE_PERIOD + PATH_DELIMITER +
                    SchedulePeriod.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            bpeCriteria.addNotEqualTo(ScheduleBellPeriod.REL_SCHEDULE_PERIOD + PATH_DELIMITER +
                    SchedulePeriod.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        bpeCriteria.addNotEqualTo(ScheduleBellPeriod.REL_SCHEDULE_PERIOD + PATH_DELIMITER +
                SchedulePeriod.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER + m_sklExludeField,
                Boolean.TRUE);
        QueryByCriteria query = new QueryByCriteria(ScheduleBellPeriod.class, bpeCriteria);

        m_scheduleBellPeriods = getBroker().getGroupedCollectionByQuery(query, ScheduleBellPeriod.COL_BELL_SCHEDULE_OID,
                1024);
    }

    /**
     * Load Schedule Master Matrix tables.
     */
    private void loadScheduleMatrix() {
        X2Criteria scheduleMatrixTermCriteria = new X2Criteria();
        QueryByCriteria scheduleMatrixTermQuery = new QueryByCriteria(MasterTerm.class, scheduleMatrixTermCriteria);
        m_masterTerms = getBroker().getCollectionByQuery(scheduleMatrixTermQuery);

        X2Criteria scheduleMasterMatrixCriteria = new X2Criteria();
        QueryByCriteria scheduleMasterMatrixQuery =
                new QueryByCriteria(MasterScheduleMatrix.class, scheduleMasterMatrixCriteria);
        m_scheduleMasterMatrices = getBroker().getGroupedCollectionByQuery(scheduleMasterMatrixQuery,
                MasterScheduleMatrix.COL_MASTER_TERM_OID, 100);

        X2Criteria scheduleMatrixCriteria = new X2Criteria();
        QueryByCriteria scheduleMatrixQuery = new QueryByCriteria(ScheduleMatrix.class, scheduleMatrixCriteria);
        m_scheduleMatricesMap = getBroker().getGroupedCollectionByQuery(scheduleMatrixQuery, X2BaseBean.COL_OID, 100);
    }

    /**
     * Load ScheduleTeachers map keyed on MST Oid.
     */
    private void loadScheduleTeacherByMSTOid() {

        X2Criteria scheduleTeacherCriteria = new X2Criteria();

        // Master type Class
        scheduleTeacherCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

        // From active Schedule
        scheduleTeacherCriteria.addEqualToField(ScheduleTeacher.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        // check school or organization selection.
        if (isSchoolContext()) {
            scheduleTeacherCriteria.addEqualTo(ScheduleTeacher.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            scheduleTeacherCriteria.addNotEqualTo(ScheduleTeacher.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            scheduleTeacherCriteria.addNotEqualTo(ScheduleTeacher.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        scheduleTeacherCriteria.addNotEqualTo(ScheduleTeacher.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER + m_sklExludeField, Boolean.TRUE);
        QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, scheduleTeacherCriteria);

        m_teacherSections = getBroker().getGroupedCollectionByQuery(query, ScheduleTeacher.COL_SECTION_OID, 1024);

    }

    /**
     * Load ScheduleTerms map keyed on MST Oid.
     */
    private void loadScheduleTerms() {
        X2Criteria scheduleTermCriteria = new X2Criteria();

        scheduleTermCriteria.addEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                m_currentContextOid);

        // From active Schedule
        scheduleTermCriteria.addEqualToField(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleTerm.COL_SCHEDULE_OID);

        // check school or organization selection.
        if (isSchoolContext()) {
            scheduleTermCriteria.addEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            scheduleTermCriteria.addNotEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            scheduleTermCriteria.addNotEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        scheduleTermCriteria.addNotEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER + m_sklExludeField, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(ScheduleTerm.class, scheduleTermCriteria);

        m_scheduleTerms = getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 256);

    }

    /**
     * Load ScheduleTermDates map keyed on TRM Oid.
     */
    private void loadScheduleTermDatesByTRMOid() {

        X2Criteria datesCriteria = new X2Criteria();

        // From active Schedule
        datesCriteria.addEqualToField(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        datesCriteria.addLessOrEqualThan(ScheduleTermDate.COL_START_DATE, m_reportDate);

        // check school or organization selection.
        if (isSchoolContext()) {
            datesCriteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            datesCriteria.addNotEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            datesCriteria.addNotEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        datesCriteria.addNotEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER + m_sklExludeField, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, datesCriteria);

        m_scheduleTermDates =
                getBroker().getGroupedCollectionByQuery(query, ScheduleTermDate.COL_SCHEDULE_TERM_OID, 1024);
    }

    /**
     * Load SchoolCalendar and SchoolCalendarDate tables.
     */
    private void loadSchoolCalendars() {
        X2Criteria schoolCalendarCriteria = new X2Criteria();

        QueryByCriteria schoolCalendarQuery = new QueryByCriteria(SchoolCalendar.class, schoolCalendarCriteria);

        m_schoolCalendars = getBroker().getCollectionByQuery(schoolCalendarQuery);


        X2Criteria schoolCalendarDateCriteria = new X2Criteria();
        schoolCalendarDateCriteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.valueOf(true));

        QueryByCriteria schoolCalendarDateQuery =
                new QueryByCriteria(SchoolCalendarDate.class, schoolCalendarDateCriteria);

        m_schoolCalendarDatesMap = getBroker().getGroupedCollectionByQuery(schoolCalendarDateQuery,
                SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, 100);
    }

    /**
     * Load SchoolCalendars map keyed on SKL Oid.
     */
    private void loadSchoolCalendarsBySKLOid() {
        X2Criteria schoolCalendarCriteria = new X2Criteria();

        schoolCalendarCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_currentContextOid);

        // check school or organization selection.
        if (isSchoolContext()) {
            schoolCalendarCriteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            schoolCalendarCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            schoolCalendarCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        schoolCalendarCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER + m_sklExludeField,
                Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, schoolCalendarCriteria);

        m_schoolCalendarsMap = getBroker().getGroupedCollectionByQuery(query, SchoolCalendar.COL_SCHOOL_OID, 1024);

    }

    /**
     * Load SchoolScheduleBell table.
     */
    private void loadSchoolScheduleBells() {
        X2Criteria criteria = new X2Criteria();

        criteria.addEqualToField(ScheduleBell.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleBell.COL_SCHEDULE_OID);

        QueryByCriteria scheduleBellQuery = new QueryByCriteria(ScheduleBell.class, criteria);

        m_schoolScheduleBellsMap = getBroker().getGroupedCollectionByQuery(scheduleBellQuery,
                ScheduleBell.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID, 100);
    }

    /**
     * Load ScheduleDay table.
     */
    private void loadSchoolScheduleDays() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualToField(ScheduleDay.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleDay.COL_SCHEDULE_OID);

        QueryByCriteria scheduleDayQuery = new QueryByCriteria(ScheduleDay.class, criteria);

        m_schoolScheduleDaysMap = getBroker().getNestedMapByQuery(scheduleDayQuery,
                ScheduleDay.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                X2BaseBean.COL_OID, 1024, 1024);
    }

    /**
     * Load SchedulePeriod table.
     */
    private void loadSchoolSchedulePeriods() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualToField(SchedulePeriod.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                SchedulePeriod.COL_SCHEDULE_OID);

        QueryByCriteria schedulePeriodQuery = new QueryByCriteria(SchedulePeriod.class, criteria);

        m_schoolSchedulePeriodsMap = getBroker().getNestedMapByQuery(schedulePeriodQuery,
                SchedulePeriod.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID,
                X2BaseBean.COL_OID, 1024, 1024);
    }

    /**
     * Load map of SFA keyed on STF oid.
     */
    private void loadStaffAttendancesBySTFOid() {
        X2Criteria sfaCriteria = new X2Criteria();

        if (isSchoolContext()) {
            sfaCriteria.addEqualTo(StaffAttendance.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            sfaCriteria.addNotEqualTo(StaffAttendance.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            sfaCriteria.addNotEqualTo(StaffAttendance.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        }
        sfaCriteria.addNotEqualTo(StaffAttendance.REL_SCHOOL + PATH_DELIMITER + m_sklExludeField, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(StaffAttendance.class, sfaCriteria);

        m_staffAttendances = getBroker().getGroupedCollectionByQuery(query, StaffAttendance.COL_STAFF_OID, 1024);

    }

    /**
     * Load Staff table.
     */
    private void loadStaffs() {
        X2Criteria criteria = new X2Criteria();

        QueryByCriteria staffQuery = new QueryByCriteria(SisStaff.class, criteria);

        m_staffs = getBroker().getMapByQuery(staffQuery, X2BaseBean.COL_OID, 100);
    }

    /**
     * Load StaffLeave table.
     */
    private void loadStaffLeaves() {
        X2Criteria staffLeavesCriteria = new X2Criteria();

        QueryByCriteria staffLeaveQuery = new QueryByCriteria(StaffLeave.class, staffLeavesCriteria);

        m_staffLeavesMap = getBroker().getGroupedCollectionByQuery(staffLeaveQuery, StaffLeave.COL_STAFF_OID, 100);
    }

    /**
     * Process section calendars.
     *
     * @param queryClass Class
     * @param columns String[]
     * @param sscCriteria X2Criteria
     */
    void processSectionCalendars(Class queryClass, String[] columns, X2Criteria sscCriteria) {
        ReportQueryByCriteria query = new ReportQueryByCriteria(queryClass, columns, sscCriteria);
        query.addGroupBy(columns[0]);
        query.addGroupBy(columns[1]);
        query.addGroupBy(columns[2]);
        query.addOrderByDescending(columns[3]);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String sectionOid = (String) row[0];
                String calendarCode = (String) row[1];
                String schoolOid = (String) row[2];

                String key = sectionOid;

                if (StringUtils.isEmpty(calendarCode) || "null".equals(calendarCode)) {
                    MasterSchedule mst = (MasterSchedule) getBroker().getBeanByOid(MasterSchedule.class, sectionOid);
                    MasterSchedule[] msts = new MasterSchedule[] {mst};
                    calendarCode = m_scheduleMgr.getMostCommonCalendar(mst.getSchedule(), Arrays.asList(msts));

                    if (StringUtils.isEmpty(calendarCode) || "null".equals(calendarCode)) {
                        addSetupError("No calendar for section",
                                mst.getSchedule().getSchool().getName() + "-" + mst.getCourseView());
                    }
                }

                if (!m_mostCommonCalendarSection.containsKey(key)) {
                    m_mostCommonCalendarSection.put(key, calendarCode);
                    verifySchoolCalendar(calendarCode, schoolOid);
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Verify that calendar dates exist for this calendar.
     *
     * @param calendarId String
     * @param schoolOid String
     */
    private void verifySchoolCalendar(String calendarId, String schoolOid) {
        Set<String> calendarIds = m_verifiedSchoolCalendars.get(schoolOid);
        if (calendarIds == null) {
            calendarIds = new HashSet();
            m_verifiedSchoolCalendars.put(schoolOid, calendarIds);
        }
        if (!calendarIds.contains(calendarId)) {
            calendarIds.add(calendarId);
            Collection<SisSchoolCalendarDate> dates = getSchoolCalendarDatesLookup(schoolOid, calendarId);
            if (dates == null || dates.isEmpty()) {
                String errorMessage = schoolOid;
                SisSchool school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                if (school != null) {
                    errorMessage = school.getName();
                }
                addSetupError("No dates for school calendar", errorMessage + "-" + calendarId);
            }
        }
    }
}
