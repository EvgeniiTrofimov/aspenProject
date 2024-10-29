/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.ca;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for "CA Average Daily Attendance Single Period" Report.
 * This report should be run for particular school.
 * The data set for the report includes all students were present for exactly one
 * of their scheduled periods on the report date.
 */
public class ADASinglePeriodData extends ReportJavaSourceNet {

    private static final String DAY_CODE_MONDAY = "A";
    private static final String DAY_CODE_TUESDAY = "B";
    private static final String DAY_CODE_WEDNESDAY = "C";
    private static final String DAY_CODE_THURSDAY = "D";
    private static final String DAY_CODE_FRIDAY = "E";
    private static final String DAY_CODE_SATURDAY = "F";
    private static final String DAY_CODE_SUNDAY = "G";

    private static final String GENDER_CODE_MALE = "M";
    private static final String GENDER_CODE_FEMALE = "F";

    private static final String INPUT_PARAM_ACTIVE_ONLY = "activeOnly";
    private static final String INPUT_PARAM_HOMEROOM_ONLY = "applyFilter";
    private static final String INPUT_START_DATE = "startDate";
    private static final String INPUT_PARAM_QUERY_BY = "queryBy";

    private static final String PARAMETER_SCHOOL = "school";
    private static final String PARAMETER_REPORT_START_DATE = "startDate";
    private static final String PARAMETER_REPORT_END_DATE = "endDate";


    private static final String FIELD_DATE = "dateAtt";
    private static final String FIELD_TEACHER_NAME = "teacherName";
    private static final String FIELD_SECTION = "section";
    private static final String FIELD_SECTION_NUMBER = "sectionNamber";
    private static final String FIELD_PERIOD = "period";
    private static final String FIELD_MALE_COUNT = "maleCount";
    private static final String FIELD_FEMALE_COUNT = "femaleCount";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_STUDENT_NAME = "studentName";

    private static final char PATH_DELIMITER = '.';
    private static final char SPACE_DELIMITER = ' ';

    private PlainDate m_endDate = null;
    private List<PlainDate> m_list_of_dates = null;
    private boolean m_homeroomOnly = false;
    private ScheduleManager m_scheduleManager;
    private SisSchool m_school;
    private PlainDate m_startDate = null;
    private Map<String, List<StudentPeriodAttendance>> m_attendances;
    private Map<String, SchedulePeriod> m_periodsMap;
    private String m_dayCode;

    /**
     * Gather data.
     *
     * @return ReportDataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected ReportDataGrid gatherData() throws Exception {
        ReportDataGrid dataGrid = new ReportDataGrid();
        for (PlainDate date : m_list_of_dates) {
            initDayCode(date);
            X2Criteria sectionsCriteria =
                    (X2Criteria) m_scheduleManager.getSections(m_school.getActiveSchedule().getOid(), date, null);

            if (sectionsCriteria != null) {
                if (((Boolean) getParameter(INPUT_PARAM_ACTIVE_ONLY)).booleanValue()) {
                    String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                            SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
                    sectionsCriteria.addEqualTo(MasterSchedule.REL_PRIMARY_STAFF +
                            PATH_DELIMITER + SisStaff.COL_STATUS, activeCode);
                }
                String queryBy = (String) getParameter(INPUT_PARAM_QUERY_BY);
                if (queryBy.equals(SELECTION_SPECIAL_CASE_PREFIX + CURRENT_KEY)) {
                    Criteria staffCriteria = getCurrentCriteria();
                    if (staffCriteria != null) {
                        SubQuery staffSubQuery = new SubQuery(Staff.class, X2BaseBean.COL_OID, staffCriteria);
                        sectionsCriteria.addIn(MasterSchedule.COL_PRIMARY_STAFF_OID, staffSubQuery);
                    }
                }

                QueryByCriteria query = new QueryByCriteria(MasterSchedule.class, sectionsCriteria);
                QueryIterator iterator = getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        MasterSchedule mst = (MasterSchedule) iterator.next();
                        if (!(m_homeroomOnly
                                && StringUtils.isEmpty(mst.getSchoolCourse().getCourse().getShortDescription()))) {
                            String teacherName = null;
                            int femaleCount = 0;
                            int maleCount = 0;
                            Collection<StudentSchedule> studentSections = mst.getStudentSections(getBroker());
                            boolean isMstCountsMade = false;
                            List<SchedulePeriod> periods = getSchedulePeriods(mst);
                            for (SchedulePeriod period : periods) {
                                for (StudentSchedule stdSchedule : studentSections) {
                                    SisStudent student = stdSchedule.getStudent();
                                    boolean presentOnOthers = isPresentOnAnotherSections(stdSchedule, student, date);
                                    if (isStudentPresent(student, mst, period, date) && !presentOnOthers) {
                                        if (!isMstCountsMade) {
                                            if (!StringUtils.isEmpty(mst.getPrimaryStaffOid())) {
                                                teacherName = mst.getPrimaryStaff().getNameView();
                                            }
                                            for (StudentSchedule schedule : studentSections) {
                                                String genderCode = schedule.getStudent().getPerson().getGenderCode();
                                                if (GENDER_CODE_MALE.equals(genderCode)) {
                                                    maleCount++;
                                                } else if (GENDER_CODE_FEMALE.equals(genderCode)) {
                                                    femaleCount++;
                                                }
                                            }
                                            isMstCountsMade = true;
                                        }
                                        dataGrid.append();
                                        dataGrid.set(FIELD_TEACHER_NAME, teacherName);
                                        dataGrid.set(FIELD_SECTION, mst);
                                        dataGrid.set(FIELD_SECTION_NUMBER, mst.getSectionNumber());
                                        dataGrid.set(FIELD_FEMALE_COUNT, Integer.valueOf(femaleCount));
                                        dataGrid.set(FIELD_MALE_COUNT, Integer.valueOf(maleCount));
                                        dataGrid.set(FIELD_PERIOD, period.getName());
                                        dataGrid.set(FIELD_STUDENT, student);
                                        dataGrid.set(FIELD_STUDENT_NAME, student.getNameView());
                                        dataGrid.set(FIELD_DATE, date);
                                    }
                                }
                            }
                        }
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        dataGrid.sort(Arrays.asList(FIELD_TEACHER_NAME, FIELD_SECTION_NUMBER, FIELD_PERIOD, FIELD_STUDENT_NAME), false);
        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_scheduleManager = new ScheduleManager(getBroker());
        PlainDate startDate = (PlainDate) getParameter(INPUT_START_DATE);
        initializeDates(startDate);
        m_school = (SisSchool) getSchool();
        loadAttendances();
        loadSchedulePeriods(m_school.getActiveScheduleOid());
        addParameter(PARAMETER_REPORT_START_DATE, m_startDate);
        addParameter(PARAMETER_REPORT_END_DATE, m_endDate);
        addParameter(PARAMETER_SCHOOL, getSchool());
        Boolean applyFilter = (Boolean) getParameters().get(INPUT_PARAM_HOMEROOM_ONLY);
        if (applyFilter != null) {
            m_homeroomOnly = applyFilter.booleanValue();
        }
    }

    /**
     * Parse section.scheduleDisplay, trying to find periodId's for m_reportDate represent by
     * m_dayCode.
     * Working in assume that section can be scheduled on different two or more period within one
     * day.
     *
     * @param mst MasterSchedule
     * @return List
     */
    private List<SchedulePeriod> getSchedulePeriods(MasterSchedule mst) {
        ArrayList<SchedulePeriod> periods = new ArrayList<SchedulePeriod>();
        String scheduleDisplay = mst.getScheduleDisplay();
        Collection<String> schedules = StringUtils.convertDelimitedStringToList(scheduleDisplay, SPACE_DELIMITER);
        for (String schedule : schedules) {
            if (schedule.contains(m_dayCode)) {
                String periodId = schedule.substring(0, schedule.indexOf("("));
                periods.add(m_periodsMap.get(periodId.trim()));
                continue;
            }
            String days = schedule.substring(schedule.indexOf("(") + 1, schedule.indexOf(")"));
            if (days.length() > 1) {
                char daysRangeBeginCode = days.charAt(0);
                char daysRangeEndCode = days.charAt(2);
                if (daysRangeBeginCode < m_dayCode.charAt(0) && daysRangeEndCode > m_dayCode.charAt(0)) {
                    String periodId = schedule.substring(0, schedule.indexOf("("));
                    periods.add(m_periodsMap.get(periodId.trim()));
                }
            }
        }
        return periods;
    }

    /**
     * Inits the day code.
     *
     * @param date PlainDate
     */
    private void initDayCode(PlainDate date) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        switch (calendar.get(Calendar.DAY_OF_WEEK)) {
            case 1:
                m_dayCode = DAY_CODE_SUNDAY;
                break;
            case 2:
                m_dayCode = DAY_CODE_MONDAY;
                break;
            case 3:
                m_dayCode = DAY_CODE_TUESDAY;
                break;
            case 4:
                m_dayCode = DAY_CODE_WEDNESDAY;
                break;
            case 5:
                m_dayCode = DAY_CODE_THURSDAY;
                break;
            case 6:
                m_dayCode = DAY_CODE_FRIDAY;
                break;
            case 7:
                m_dayCode = DAY_CODE_SATURDAY;
                break;
        }
    }

    /**
     * Initialize start and end date of the period.
     *
     * @param start PlainDate
     */
    private void initializeDates(PlainDate start) {
        Calendar calStart = Calendar.getInstance();
        calStart.setTime(start);
        m_list_of_dates = new ArrayList<PlainDate>();
        calStart.set(Calendar.DAY_OF_WEEK, Calendar.MONDAY);
        m_startDate = new PlainDate(calStart.getTime());
        m_list_of_dates.add(m_startDate);
        int counter = 0;
        while (4 != counter) {
            int dayOfWeek = calStart.get(Calendar.DAY_OF_WEEK);
            calStart.add(Calendar.DAY_OF_MONTH, 1);
            if (dayOfWeek != 1 && dayOfWeek != 7) {
                m_list_of_dates.add(new PlainDate(calStart.getTime()));
                counter++;
            }
        }
        m_endDate = new PlainDate(calStart.getTime());
    }

    /**
     * Check all another student sections on reportDate.
     *
     * @param stdSchedule StudentSchedule
     * @param student SisStudent
     * @param date PlainDate
     * @return true, if is present on another sections
     */
    private boolean isPresentOnAnotherSections(StudentSchedule stdSchedule, SisStudent student, PlainDate date) {
        List<StudentSchedule> allStudentSections = (List<StudentSchedule>) m_scheduleManager
                .getSections(m_school.getActiveScheduleOid(), student.getOid(), date, null);
        allStudentSections.remove(stdSchedule);
        for (StudentSchedule anotherSchedule : allStudentSections) {
            MasterSchedule anotherMst = anotherSchedule.getSection();
            List<SchedulePeriod> anotherPeriods = getSchedulePeriods(anotherMst);
            for (SchedulePeriod anotherPeriod : anotherPeriods) {
                if (isStudentPresent(student, anotherMst, anotherPeriod, date)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Check if student have no StudentPeriodAttendance records for section and period.
     *
     * @param student SisStudent
     * @param mst MasterSchedule
     * @param period SchedulePeriod
     * @param date PlainDate
     * @return true, if is student present
     */
    private boolean isStudentPresent(SisStudent student, MasterSchedule mst, SchedulePeriod period, PlainDate date) {
        List<StudentPeriodAttendance> studentAttendances = m_attendances.get(student.getOid());
        if (studentAttendances != null) {
            for (StudentPeriodAttendance attendance : studentAttendances) {
                if (attendance.getDate().equals(date)) {
                    if (mst.getOid().equals(attendance.getMasterScheduleOid()) && attendance.getAbsentIndicator()
                            && period.getId().equals(attendance.getPeriodView())) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    /**
     * Load attendances.
     */
    private void loadAttendances() {
        X2Criteria attendancesCriteria = new X2Criteria();
        attendancesCriteria.addEqualTo(StudentPeriodAttendance.COL_SCHOOL_OID, m_school.getOid());
        attendancesCriteria.addGreaterOrEqualThan(StudentPeriodAttendance.COL_DATE, m_startDate);
        attendancesCriteria.addLessOrEqualThan(StudentPeriodAttendance.COL_DATE, m_endDate);
        QueryByCriteria query = new QueryByCriteria(StudentPeriodAttendance.class, attendancesCriteria);
        m_attendances = getBroker().getGroupedCollectionByQuery(query, StudentPeriodAttendance.COL_STUDENT_OID, 20);
    }

    /**
     * Load schedule periods.
     *
     * @param activeScheduleOid String
     */
    private void loadSchedulePeriods(String activeScheduleOid) {
        X2Criteria periodsCriteria = new X2Criteria();
        periodsCriteria.addEqualTo(SchedulePeriod.COL_SCHEDULE_OID, activeScheduleOid);
        QueryByCriteria query = new QueryByCriteria(SchedulePeriod.class, periodsCriteria);
        m_periodsMap = getBroker().getMapByQuery(query, SchedulePeriod.COL_ID, 10);
    }
}
