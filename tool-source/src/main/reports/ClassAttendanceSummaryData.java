/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2013 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.StudentPeriodAttendanceManager;
import com.x2dev.sis.model.business.attendance.AttendanceManagerFactory;
import com.x2dev.sis.model.business.gradebook.GradebookManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.model.business.schedule.SectionCalendar;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Class Attendance Summary" report.
 *
 * @author Follett Software Company
 */
public class ClassAttendanceSummaryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String INPUT_PARAM_END_DATE = "endDate";
    private static final String INPUT_PARAM_START_DATE = "startDate";

    private static final String COL_ABSENT = "absent";
    private static final String COL_DAYS_ENROLLED = "daysEnrolled";
    private static final String COL_DISMISSED = "dismissed";
    private static final String COL_SECTION = "section";
    private static final String COL_STUDENT = "student";
    private static final String COL_TARDY = "tardy";

    private static final String PARAM_TRACK_ATTENDANCE_BY_PERIOD = "trackAttendanceByPeriod";

    private PlainDate m_endDate = null;
    private MasterSchedule m_section = null;
    private SectionCalendar m_sectionCalendar = null;
    private PlainDate m_startDate = null;

    // Map<calendarId, HashMap<dayNumber, possibleAttendance>>
    private Map<String, HashMap<Integer, Integer>> m_calendarDayPossibleMap;

    // Map<calendarId, HashMap<PlainDate, possibleAttendance>>
    private Map<String, HashMap<PlainDate, Integer>> m_possibleAttendanceMap;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(50, 25);

        boolean trackAttendanceByPeriod = StudentPeriodAttendanceManager
                .getTrackAttendanceByPeriodPreferenceValue(m_section.getSchoolCourse().getSchool());

        if (m_section != null) {
            Map<String, Integer> absentTotals = StudentPeriodAttendanceManager.getAbsenceTotals(m_startDate, m_endDate,
                    m_section.getOid(), getBroker());
            Map<String, Integer> dismissedTotals = StudentPeriodAttendanceManager.getDismissedTotals(m_startDate,
                    m_endDate, m_section.getOid(), getBroker());
            Map<String, Integer> tardyTotals = StudentPeriodAttendanceManager.getTardyTotals(m_startDate, m_endDate,
                    m_section.getOid(), getBroker());

            QueryByCriteria query =
                    new QueryByCriteria(SisStudent.class, GradebookManager.getAllStudentsCriteria(m_section));
            query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

            QueryIterator students = getBroker().getIteratorByQuery(query);
            try {
                while (students.hasNext()) {
                    SisStudent student = (SisStudent) students.next();

                    Set<PlainDate> datesEnrolled = m_sectionCalendar.getDatesEnrolled(student);

                    Integer enrolled = null;

                    if (trackAttendanceByPeriod) {
                        enrolled = calculatePossibleAttendance(student, datesEnrolled);
                    } else {
                        enrolled = Integer.valueOf(datesEnrolled.size());
                    }

                    grid.append();
                    grid.set(COL_ABSENT, absentTotals.get(student.getOid()));
                    grid.set(COL_DAYS_ENROLLED, enrolled);
                    grid.set(COL_DISMISSED, dismissedTotals.get(student.getOid()));
                    grid.set(COL_SECTION, m_section);
                    grid.set(COL_STUDENT, student);
                    grid.set(COL_TARDY, tardyTotals.get(student.getOid()));
                }
            } finally {
                students.close();
            }
        }

        grid.beforeTop();

        addParameter(PARAM_TRACK_ATTENDANCE_BY_PERIOD, Boolean.valueOf(trackAttendanceByPeriod));

        return grid;
    }

    /**
     * Calculates the total possible attendance sessions for a student in a specific section. This
     * is called
     * when the Track Attendance By Period preference is enabled.
     *
     * @param student SisStudent
     * @param datesEnrolled Set<PlainDate>
     * @return Integer
     */
    private Integer calculatePossibleAttendance(SisStudent student,
                                                Set<PlainDate> datesEnrolled) {
        String calendarId = student.getCalendarCode();

        HashMap<PlainDate, Integer> possibleAttendanceForDate = m_possibleAttendanceMap.get(calendarId);
        if (possibleAttendanceForDate == null) {
            /*
             * Build up the possibleAttendanceForDate map for this specific calendarId
             */
            possibleAttendanceForDate = new HashMap<PlainDate, Integer>();
            m_possibleAttendanceMap.put(calendarId, possibleAttendanceForDate);

            ScheduleManager manager = new ScheduleManager(getBroker());
            StudentPeriodAttendanceManager spam =
                    AttendanceManagerFactory.getStudentPeriodAttendanceManager(getBroker());

            HashMap<Integer, Integer> possibleAttendanceByDayNumber = m_calendarDayPossibleMap.get(calendarId);
            if (possibleAttendanceByDayNumber == null) {
                /*
                 * This is used to store the total possible attendance for a specific
                 * SchoolCalendarDate.ScheduleDayNumber
                 * within a matching calendarId to increase efficiency.
                 */
                possibleAttendanceByDayNumber = new HashMap<Integer, Integer>();
                m_calendarDayPossibleMap.put(calendarId, possibleAttendanceByDayNumber);
            }

            /*
             * Iterate through all the meeting dates for a section and determine the possible
             * attendance for that date.
             */
            Collection<SchoolCalendarDate> meetingDates = manager.getSectionMeetingCalendarDates(m_section, calendarId);
            for (SchoolCalendarDate meetingDate : meetingDates) {
                Integer possibleAttendanceForDay = null;
                if (possibleAttendanceByDayNumber.get(Integer.valueOf(meetingDate.getScheduleDayNumber())) != null) {
                    // We have already determined the possible attendance for this
                    // SchoolCalendarDate dayNumber.
                    possibleAttendanceForDay =
                            possibleAttendanceByDayNumber.get(Integer.valueOf(meetingDate.getScheduleDayNumber()));
                } else {
                    possibleAttendanceForDay =
                            Integer.valueOf(spam.getPeriods(m_section, meetingDate.getDate(), calendarId, null).size());

                    /*
                     * Store the ScheduleDayNumber and the total possible attendance for that day in
                     * a map to be used
                     * by any other ScheduleCalendarDate objects with a matching day number.
                     */
                    possibleAttendanceByDayNumber.put(Integer.valueOf(meetingDate.getScheduleDayNumber()),
                            possibleAttendanceForDay);
                }

                possibleAttendanceForDate.put(meetingDate.getDate(), possibleAttendanceForDay);
            }
        }

        /*
         * Iterate through all the in session dates for the section and add up the possible
         * attendance for the student
         * if the student was enrolled.
         */
        int possibleAttendance = 0;
        for (PlainDate sessionDate : possibleAttendanceForDate.keySet()) {
            if (datesEnrolled.contains(sessionDate)) {
                // Student is enrolled on this date
                possibleAttendance += possibleAttendanceForDate.get(sessionDate).intValue();
            }
        }

        return Integer.valueOf(possibleAttendance);
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_startDate = (PlainDate) getParameter(INPUT_PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(INPUT_PARAM_END_DATE);

        m_calendarDayPossibleMap = new HashMap<String, HashMap<Integer, Integer>>();
        m_possibleAttendanceMap = new HashMap<String, HashMap<PlainDate, Integer>>();

        if (m_section != null) {
            m_sectionCalendar = SectionCalendar.getInstance(m_section, m_startDate, m_endDate, getBroker());
        }
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
        ScheduleTeacher parentRecord = userData.getCurrentRecord(ScheduleTeacher.class);
        if (parentRecord != null) {
            m_section = parentRecord.getSection();
        }
    }
}
