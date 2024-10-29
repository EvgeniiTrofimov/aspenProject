/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.uk;

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.RefAttendanceStudent;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.attendance.uk.UKAttendancePercentageSummary;
import com.x2dev.sis.tools.reports.StudentReportJavaSource;
import com.x2dev.utils.types.PlainDate;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Attendance Statistics Report for UK attendance.
 * This export should report by school for each day,
 * the percentage of enrolled students who are absent that day.
 *
 * @author X2 Development Corporation
 */
public class AttendanceStatisticsData extends StudentReportJavaSource {
    /**
     * Constants: report field and parameter names, map IDs
     */
    private static final String FIELD_ATTENDANCE = "attendance";
    private static final String FIELD_CALENDAR = "calendar";
    private static final String FIELD_CODES = "codes";
    private static final String FIELD_STATS = "stats";
    private static final String FIELD_STUDENT = "student";

    private static final String ID_ABSENT = "A";
    private static final String ID_ABSENT_STAT = "AS";
    private static final String ID_ABSENT_NONSTAT = "AN";
    private static final String ID_ABSENT_EX = "AE";
    private static final String ID_ABSENT_UN = "AU";
    private static final String ID_POSSIBLE = "P";
    private static final String ID_PRESENT = "AT";
    private static final String ID_PRESENT_STAT = "ATS";
    private static final String ID_TARDY = "T";
    private static final String ID_TARDY_EX = "TE";
    private static final String ID_TARDY_UN = "TU";

    private static final String UK_ABSENT_VARIABLE = "A";
    private static final String UK_LATE_VARIABLE = "L";
    private static final String BLANK_INDICATOR = "-";
    private static final String UK_YES_VARIABLE = "Y";
    private static final String UK_NO_VARIABLE = "N";

    /**
     * UK constant variables
     */

    private static final String PARAM_BEGIN_DATE = "startDate";
    private static final String PARAM_REPORT_DATE = "endDate";
    private static final String PARAM_WEEKS_LIST = "weeksList";
    private static final String PARAM_WEEKS_MAP = "weeksMap";

    /**
     * Report instance variables.
     */
    private Collection<SisStudent> m_stdList;
    private DecimalFormat m_numberFormat = new DecimalFormat("##0.00");
    private Map<String, Object> m_studentStats;
    private UKAttendancePercentageSummary m_UKAttHelper;

    /**
     * List of the school weeks (by monday date).
     * This is the key of the second level map of m_schoolWeeksMap
     * and the first level of the student weeks map.
     */
    private List<PlainDate> m_weeksList = null;

    /**
     * Global map of school calendar weeks and dates in parameter "weeksMap".
     * <br>
     * Map&lt;String, Map&lt;PlainDate, Map&lt;Integer, String&gt;&gt;&gt;
     * <br>
     * Key 1: String = School calendar ID.
     * <br>
     * Key 2: PlainDate = Week starting date (monday date)
     * <br>
     * Key 3: Integer = day of week (1=monday, 2=tuesday, ... 5=friday)
     * <br>
     * Value: String : null or default code for date if non-session. (EX: "#")
     * <p>
     * Student map of student attendance codes in field "attendance".
     * <br>
     * Map&lt;PlainDate, Map&lt;Integer, String&gt;&gt;
     * <br>
     * Key 1: PlainDate = Week starting date (monday date) - matches weeksMap.
     * <br>
     * Key 2: Integer = day of week (1=monday AM, ... +10 for PM so 11=monday PM.)
     * <br>
     * Value: String = student attendance code for the date and period.
     */
    private Map<String, Map<PlainDate, Map<String, String>>> m_schoolWeeksMap;

    /**
     * Gather the data for the report.
     * Report is based on student iteration.
     * Maps of student attendance data are provides to fill the attendance grid.
     *
     * @return JRDataSource
     */
    @Override
    protected JRDataSource gatherData() {
        SisSchool school = (SisSchool) getSchool();

        // Determine reporting dates and weekly date structure.
        // Build structures of weeks, week dates, dates in weeks, default codes for dates.
        PlainDate startDate = (PlainDate) getParameters().get(PARAM_BEGIN_DATE);
        PlainDate endDate = (PlainDate) getParameters().get(PARAM_REPORT_DATE);
        Schedule schedule = school.getActiveSchedule();
        if (schedule != null) {
            if (endDate.after(schedule.getEndDate())) {
                endDate = schedule.getEndDate();
            }
        } else {
            DistrictSchoolYearContext context = getCurrentContext();
            if (endDate.after(context.getEndDate())) {
                endDate = context.getEndDate();
            }
        }

        m_UKAttHelper =
                new UKAttendancePercentageSummary(m_stdList, startDate, endDate, school, getBroker(), true, true);

        // Determine reporting dates and weekly date structure.
        m_schoolWeeksMap = m_UKAttHelper.getSchoolWeekMap();
        if (m_schoolWeeksMap.size() > 0) {
            Map<PlainDate, Map<String, String>> it = null;
            if (m_schoolWeeksMap.keySet().contains("Standard")) {
                it = m_schoolWeeksMap.get("Standard");
            } else {
                it = m_schoolWeeksMap.get(m_schoolWeeksMap.keySet().iterator().next());
            }
            m_weeksList = new ArrayList<PlainDate>(it.keySet());
        }
        addParameter(PARAM_WEEKS_LIST, m_weeksList);
        addParameter(PARAM_WEEKS_MAP, m_schoolWeeksMap);

        // Prepare data grid.
        ReportDataGrid dataGrid = new ReportDataGrid();

        for (SisStudent student : m_stdList) {
            Map<PlainDate, Map<String, String>> studentDateMap = buildStudentMap(student);

            if (studentDateMap.size() > 0) {
                List<List<Object>> rowList = buildRowList(m_UKAttHelper.getAttendanceCodeMap(student.getOid()));
                dataGrid.append();
                dataGrid.set(FIELD_STUDENT, student);
                dataGrid.set(FIELD_CALENDAR, student.getCalendarCode());
                dataGrid.set(FIELD_ATTENDANCE, studentDateMap);
                dataGrid.set(FIELD_STATS, m_studentStats);
                dataGrid.set(FIELD_CODES, rowList);
            }
        }

        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Get the student from the current context.
     *
     * @param userData UserDataContainer
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        SisStudent curStudent = userData.getCurrentRecord(SisStudent.class);

        // If it's not in a single student page detail, then try to get the criteria from the
        // current list of students.
        if (curStudent == null) // && queryBy.equals("##current"))
        {

            Criteria studentCriteria = buildCriteria();
            QueryByCriteria query = createQueryByCriteria(SisStudent.class, studentCriteria);
            String sort = (String) getParameter(SORT_PARAM);
            applyUserSort(query, sort);

            m_stdList = getBroker().getCollectionByQuery(query);
        } else {
            m_stdList = new ArrayList<SisStudent>();
            m_stdList.add(curStudent);
        }
    }

    /**
     * Build a map of attendance dates for the student based on their enrollment, attendance and the
     * week map.
     * <p>
     * Map components are:
     * <br>
     * key = Integer as day of week, AM: 1=mon, 2=tue, ... 5=fri, PM: 11=mon, 12=tue, ... 15=fri
     * <br>
     * value = student attendance code for that date and time.
     *
     * @param student SisStudent
     * @return Map<String, String>
     */
    private Map<PlainDate, Map<String, String>> buildStudentMap(SisStudent student) {
        String studentOid = student.getOid();

        // Set the studentWeek to empty first, and if the total possible is not 0 (zero), we set it
        Map<PlainDate, Map<String, String>> studentWeeks = new HashMap<PlainDate, Map<String, String>>();

        // Statistics
        m_studentStats = new HashMap<String, Object>();

        int totalPossible = m_UKAttHelper.getTotalPossible(studentOid);
        int totalAbsent = m_UKAttHelper.getTotalAbsent(studentOid);

        int absentStatistical = m_UKAttHelper.getAbsentTotalStatistical(studentOid);
        int absentNonStatistical = m_UKAttHelper.getAbsentTotalNonStatistical(studentOid);

        int absentExcused = m_UKAttHelper.getAbsentTotalExcused(studentOid);
        int absentNonExcused = m_UKAttHelper.getAbsentTotalNonExcused(studentOid);

        int lateAuthorized = m_UKAttHelper.getLateTotalAuthorized(studentOid);
        int lateUnauthorized = m_UKAttHelper.getLateTotalUnauthorized(studentOid);

        m_studentStats.put(ID_POSSIBLE, Integer.toString(totalPossible));
        m_studentStats.put(ID_PRESENT, Integer.toString(totalPossible - totalAbsent));
        m_studentStats.put(ID_PRESENT_STAT, Integer.toString(totalPossible - absentStatistical));
        m_studentStats.put(ID_ABSENT, Integer.toString(totalAbsent));
        m_studentStats.put(ID_ABSENT_STAT, Integer.toString(absentStatistical));
        m_studentStats.put(ID_ABSENT_NONSTAT, Integer.toString(absentNonStatistical));
        m_studentStats.put(ID_ABSENT_EX, Integer.toString(absentExcused));
        m_studentStats.put(ID_ABSENT_UN, Integer.toString(absentNonExcused));
        m_studentStats.put(ID_TARDY, Integer.toString(lateAuthorized + lateUnauthorized));
        m_studentStats.put(ID_TARDY_EX, Integer.toString(lateAuthorized));
        m_studentStats.put(ID_TARDY_UN, Integer.toString(lateUnauthorized));

        if (totalPossible > 0) {
            studentWeeks = m_UKAttHelper.getStudentWeekMap(studentOid);

            m_studentStats.put(ID_PRESENT + "%",
                    m_numberFormat.format(((float) (totalPossible - totalAbsent)) / ((float) totalPossible) * 100.0f));
            m_studentStats.put(ID_PRESENT_STAT + "%", m_numberFormat
                    .format(((float) (totalPossible - absentStatistical)) / ((float) totalPossible) * 100.0f));
            m_studentStats.put(ID_ABSENT + "%",
                    m_numberFormat.format(((float) (totalAbsent)) / ((float) totalPossible) * 100.0f));
            m_studentStats.put(ID_ABSENT_STAT + "%",
                    m_numberFormat.format(((float) (absentStatistical)) / ((float) totalPossible) * 100.0f));
            m_studentStats.put(ID_ABSENT_NONSTAT + "%",
                    m_numberFormat.format(((float) (absentNonStatistical)) / ((float) totalPossible) * 100.0f));
            m_studentStats.put(ID_ABSENT_EX + "%",
                    m_numberFormat.format(((float) absentExcused) / ((float) totalPossible) * 100.0f));
            m_studentStats.put(ID_ABSENT_UN + "%",
                    m_numberFormat.format(((float) absentNonExcused) / ((float) totalPossible) * 100.0f));
            m_studentStats.put(ID_TARDY + "%", m_numberFormat
                    .format(((float) (lateAuthorized + lateUnauthorized)) / ((float) totalPossible) * 100.0f));
            m_studentStats.put(ID_TARDY_EX + "%",
                    m_numberFormat.format(((float) lateAuthorized) / ((float) totalPossible) * 100.0f));
            m_studentStats.put(ID_TARDY_UN + "%",
                    m_numberFormat.format(((float) lateUnauthorized) / ((float) totalPossible) * 100.0f));
        } else {
            m_studentStats.put(ID_PRESENT + "%", "");
            m_studentStats.put(ID_PRESENT_STAT + "%", "");
            m_studentStats.put(ID_ABSENT + "%", "");
            m_studentStats.put(ID_ABSENT_STAT + "%", "");
            m_studentStats.put(ID_ABSENT_NONSTAT + "%", "");
            m_studentStats.put(ID_ABSENT_EX + "%", "");
            m_studentStats.put(ID_ABSENT_UN + "%", "");
            m_studentStats.put(ID_TARDY + "%", "");
            m_studentStats.put(ID_TARDY_EX + "%", "");
            m_studentStats.put(ID_TARDY_UN + "%", "");
        }

        return studentWeeks;
    }

    /**
     * TODO javadoc, and explain what rowList might look like.
     *
     * @param refAttCodeCountMap Map<String,Map<String,Integer>>
     * @return List
     */
    private List<List<Object>> buildRowList(Map<String, Map<String, Integer>> refAttCodeCountMap) {
        List<List<Object>> result = new ArrayList<List<Object>>();

        if (refAttCodeCountMap != null) {
            for (String code : refAttCodeCountMap.keySet()) {
                Integer amCount = refAttCodeCountMap.get(code).get(UKAttendancePercentageSummary.AM_KEY);
                Integer pmCount = refAttCodeCountMap.get(code).get(UKAttendancePercentageSummary.PM_KEY);
                int totalCount =
                        (amCount != null ? amCount.intValue() : 0) +
                                (pmCount != null ? pmCount.intValue() : 0);
                RefAttendanceStudent curRefCode = m_UKAttHelper.getDailyRefAttendanceStudentMap().get(code);
                List<Object> rowList = new ArrayList<Object>();
                rowList.add(m_UKAttHelper.isAbsent(code) ? UK_ABSENT_VARIABLE
                        : m_UKAttHelper.isTardy(code) ? UK_LATE_VARIABLE : "");
                rowList.add(code);
                rowList.add(curRefCode != null ? curRefCode.getDescription() : BLANK_INDICATOR);
                rowList.add(m_UKAttHelper.isBlankAuthorized(code) ? BLANK_INDICATOR
                        : (m_UKAttHelper.isAuthorized(code) ? UK_YES_VARIABLE : UK_NO_VARIABLE));
                rowList.add(m_UKAttHelper.isBlankStatistical(code) ? BLANK_INDICATOR
                        : (m_UKAttHelper.isStatistical(code) ? UK_YES_VARIABLE : UK_NO_VARIABLE));
                rowList.add(String.valueOf(totalCount));
                rowList.add(Integer.valueOf(totalCount));
                result.add(rowList);
            }
        }

        return result;
    }
}
