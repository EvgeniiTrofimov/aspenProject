/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.fl;

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentInfo;
import com.x2dev.reports.statereporting.fl.FLStudentAttendanceInfo.AttendanceCalendar;
import com.x2dev.reports.statereporting.fl.FLStudentAttendanceReportData.AttendanceSummary;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLStudentAttendanceByPeriodReport.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStudentAttendanceByPeriodReport extends ReportJavaSourceNet {

    private static final String ALIAS_DISTRICT_NUMBER = "all-org-StateId";

    private static final String ALIAS_PERSON_FL_EDU_ID = "all-psn-StateEducationId";

    private static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";
    private static final String ALIAS_STUDENT_NUMBER = "all-std-StateId";

    private static final String BOOLEAN_NO = "N";
    private static final String BOOLEAN_YES = "Y";

    /**
     * Data grid fields.
     */
    private static final String FIELD_DST_NAME = "dstName";
    private static final String FIELD_DST_NUMBER = "dstNumber";

    private static final String FIELD_FL_EDU_ID = "flEduId";

    private static final String FIELD_PERIOD = "period";

    private static final String FIELD_SKL_NAME = "sklName";
    private static final String FIELD_SKL_NUMBER = "sklNumber";
    private static final String FIELD_SKL_YEAR = "sklYear";

    private static final String FIELD_STD_ATT_ABS = "stdAttAbs";
    private static final String FIELD_STD_ATT_ABS_EXC = "stdAttAbsExc";
    private static final String FIELD_STD_ATT_ABS_UNEXC = "stdAttAbsUnexc";
    private static final String FIELD_STD_ATT_ABS_UNEXC_ND = "stdAttAbsUnexcND";
    private static final String FIELD_STD_ATT_TARDY = "stdAttTardy";

    private static final String FIELD_STD_BIRTH_DATE = "stdBirthDate";
    private static final String FIELD_STD_ENROLLED_DAYS = "stdEnrolled";
    private static final String FIELD_STD_ETHNICITY = "stdEthnicity";
    private static final String FIELD_STD_GENDER = "stdGender";
    private static final String FIELD_STD_GRADE = "stdGrade";
    private static final String FIELD_STD_LEGAL_NAME = "stdLegalName";
    private static final String FIELD_STD_LOCAL_ID = "stdLocalId";
    private static final String FIELD_STD_NUMBER = "stdNumber";
    private static final String FIELD_STD_RACE_ASIAN = "stdRaceA";
    private static final String FIELD_STD_RACE_BLACK = "stdRaceB";
    private static final String FIELD_STD_RACE_INDIAN = "stdRaceI";
    private static final String FIELD_STD_RACE_PACIFIC = "stdRaceP";
    private static final String FIELD_STD_RACE_WHITE = "stdRaceW";
    private static final String FIELD_STD_SSN = "stdSsn";

    private static final String FIELD_TERM = "term";
    private static final String FIELD_TOTAL_DAYS = "totalDays";

    private static final String FL_DATE_FORMAT = "MMddyyyy";

    private static final String INPUT_PARAM_END_DATE = "endDate";
    private static final String INPUT_PARAM_START_DATE = "startDate";

    private static final String PARAM_FL_DATE_FORMAT = "flDateFormat";

    private static final String RACE_CODE_ASIAN = "A";
    private static final String RACE_CODE_BLACK = "B";
    private static final String RACE_CODE_INDIAN = "I";
    private static final String RACE_CODE_PACIFIC = "P";
    private static final String RACE_CODE_WHITE = "W";

    private Calendar m_calendar;

    private DataDictionaryField m_fieldDistrictNumber;
    private DataDictionaryField m_fieldFlEduId;
    private DataDictionaryField m_fieldSchoolNumber;
    private DataDictionaryField m_fieldStudentNumber;

    private ReportDataGrid m_grid;
    private FLStudentAttendanceReportData m_reportData;

    private PlainDate m_startDate;
    private FLStudentAttendanceInfo m_studentAttendanceInfo;

    private Map<String, List<PlainDate>> m_termDaysMap = new HashMap<>();
    private Map<String, Collection<SchedulePeriod>> m_termSchedulePeriodsMap = new HashMap<>();

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {

        m_grid = new ReportDataGrid();

        QueryByCriteria query =
                new QueryByCriteria(SisStudent.class, m_studentAttendanceInfo.getStudentHelper().getStudentCriteria());

        QueryIterator studentIterator = null;
        try {
            studentIterator = getBroker().getIteratorByQuery(query);
            while (studentIterator.hasNext()) {
                SisStudent std = (SisStudent) studentIterator.next();
                List<AttendanceCalendar> all = m_studentAttendanceInfo.getStudentCalendar(std);
                for (AttendanceCalendar data : all) {
                    addPage(std, data);
                }
            }
        } finally {
            if (studentIterator != null) {
                studentIterator.close();
            }
        }
        m_grid.beforeTop();
        return m_grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {

        addParameter(PARAM_FL_DATE_FORMAT, new SimpleDateFormat(FL_DATE_FORMAT));

        m_startDate = (PlainDate) getParameters().get(INPUT_PARAM_START_DATE);
        m_calendar = Calendar.getInstance();

        m_reportData = new FLStudentAttendanceReportData();
        m_reportData.setParameters(getParameters());
        m_reportData.setBroker(getBroker());
        m_reportData.setOrganization(getOrganization());
        m_reportData.setCurrentContext(getCurrentContext());
        m_reportData.setSchool(getSchool());
        m_reportData.setSchoolContext(isSchoolContext());
        m_reportData.setPrivilegeSet(getPrivilegeSet());

        m_fieldDistrictNumber = m_reportData.translateAliasToDictionaryField(ALIAS_DISTRICT_NUMBER, true);
        m_fieldSchoolNumber = m_reportData.translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER, true);
        m_fieldStudentNumber = m_reportData.translateAliasToDictionaryField(ALIAS_STUDENT_NUMBER, true);
        m_fieldFlEduId = m_reportData.translateAliasToDictionaryField(ALIAS_PERSON_FL_EDU_ID, true);

        m_studentAttendanceInfo = new FLStudentAttendanceInfo(m_reportData, m_startDate,
                (PlainDate) getParameters().get(INPUT_PARAM_END_DATE));

    }

    /**
     * Adds the page.
     *
     * @param std SisStudent
     * @param data AttendanceCalendar
     * @throws X2BaseException exception
     */
    private void addPage(SisStudent std, AttendanceCalendar data) throws X2BaseException {
        if (std == null || data == null) {
            return;
        }

        Collection<ScheduleTerm> scheduleTerms = m_reportData.getScheduleTerms(data.getSkl(), std);
        if (scheduleTerms != null && !scheduleTerms.isEmpty()) {
            for (ScheduleTerm term : scheduleTerms) {
                Integer totalDays = getTotalDays(term, std.getCalendarCode());
                List<PlainDate> dates = m_termDaysMap.get(term.getOid());
                if (dates != null && !dates.isEmpty()) {

                    PlainDate beginDate = dates.get(0);
                    PlainDate endDate = dates.get(dates.size() - 1);
                    Map<String, AttendanceSummary> attendance =
                            m_reportData.getStudentTermPeriodAttendance(beginDate, endDate, std.getOid());

                    for (SchedulePeriod period : getTermSchedulePeriods(term)) {
                        m_grid.append();
                        AttendanceSummary summary = attendance.get(period.getId());
                        setSchoolFields(data.getSkl());
                        setStudentFields(std);
                        m_grid.set(FIELD_TERM, term.getName());
                        m_grid.set(FIELD_FL_EDU_ID,
                                m_studentAttendanceInfo.getFieldValue(std.getPerson(), m_fieldFlEduId));
                        m_grid.set(FIELD_PERIOD, period.getId());
                        m_grid.set(FIELD_TOTAL_DAYS, totalDays);
                        m_grid.set(FIELD_STD_ENROLLED_DAYS,
                                m_reportData.getEnrolledDays(m_termDaysMap, std, term, period));

                        m_grid.set(FIELD_STD_ATT_ABS, Integer.valueOf(summary != null ? summary.getAbsent() : 0));
                        m_grid.set(FIELD_STD_ATT_ABS_EXC,
                                Integer.valueOf(summary != null ? summary.getAbsentExc() : 0));
                        m_grid.set(FIELD_STD_ATT_ABS_UNEXC,
                                Integer.valueOf(summary != null ? summary.getAbsentUnexc() : 0));
                        m_grid.set(FIELD_STD_ATT_ABS_UNEXC_ND,
                                Integer.valueOf(summary != null ? summary.getAbsentUnexcNotDisc() : 0));
                        m_grid.set(FIELD_STD_ATT_TARDY,
                                Integer.valueOf(summary != null ? summary.getTardy() : 0));
                    }
                }
            }
        }
    }

    /**
     * Check race.
     *
     * @param std SisStudent
     * @param raceCode String
     * @return String
     */
    private String checkRace(SisStudent std, String raceCode) {
        return m_studentAttendanceInfo.checkRace(std, raceCode) ? BOOLEAN_YES : BOOLEAN_NO;
    }

    /**
     * Gets the term schedule periods.
     *
     * @param term ScheduleTerm
     * @return Collection
     */
    private Collection<SchedulePeriod> getTermSchedulePeriods(ScheduleTerm term) {
        Collection<SchedulePeriod> res = m_termSchedulePeriodsMap.get(term.getOid());
        if (res == null) {
            res = new ArrayList<SchedulePeriod>();
            for (Object operiod : term.getSchedule().getSchedulePeriods()) {
                SchedulePeriod period = (SchedulePeriod) operiod;
                res.add(period);
            }
            m_termSchedulePeriodsMap.put(term.getOid(), res);
        }
        return res;
    }

    /**
     * Gets the total days.
     *
     * @param term ScheduleTerm
     * @param calendarCode String
     * @return Integer
     */
    private Integer getTotalDays(ScheduleTerm term, String calendarCode) {
        String key = term.getOid();
        if (!m_termDaysMap.containsKey(key)) {
            List<PlainDate> dates = new ArrayList<>();
            m_termDaysMap.put(key, dates);
            for (ScheduleTermDate std : term.getScheduleTermDates(getBroker())) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_SCHOOL_OID,
                        term.getSchedule().getSchool().getOid());
                criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_CALENDAR_ID,
                        calendarCode);
                criteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
                criteria.addEqualTo(
                        SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                        getCurrentContext().getOid());
                criteria.addGreaterOrEqualThan(SisSchoolCalendarDate.COL_DATE, std.getStartDate());
                criteria.addLessOrEqualThan(SisSchoolCalendarDate.COL_DATE, std.getEndDate());
                QueryByCriteria query = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
                QueryIterator iterator = getBroker().getIteratorByQuery(query);
                while (iterator.hasNext()) {
                    SisSchoolCalendarDate item = (SisSchoolCalendarDate) iterator.next();
                    dates.add(item.getDate());
                }
            }
            Collections.sort(dates);
        }
        return Integer.valueOf(m_termDaysMap.get(key).size());
    }

    /**
     * Sets the school fields.
     *
     * @param skl void
     * @throws X2BaseException exception
     */
    private void setSchoolFields(SisSchool skl) throws X2BaseException {
        m_grid.set(FIELD_DST_NUMBER,
                m_studentAttendanceInfo.getFieldValue(skl.getOrganization1(), m_fieldDistrictNumber));
        m_grid.set(FIELD_DST_NAME, skl.getOrganization1().getName());

        m_grid.set(FIELD_SKL_YEAR, Integer.toString(m_calendar.get(Calendar.YEAR)));
        m_grid.set(FIELD_SKL_NAME, skl.getName());
        m_grid.set(FIELD_SKL_NUMBER, m_studentAttendanceInfo.getFieldValue(skl, m_fieldSchoolNumber));
    }

    /**
     * Sets the student fields.
     *
     * @param std void
     * @throws X2BaseException exception
     */
    private void setStudentFields(SisStudent std) throws X2BaseException {
        StudentInfo info = m_studentAttendanceInfo.getStudentHelper().getStudentInfo(std);

        m_grid.set(FIELD_STD_LOCAL_ID, std.getLocalId());
        m_grid.set(FIELD_STD_NUMBER, m_studentAttendanceInfo.getFieldValue(std, m_fieldStudentNumber));
        m_grid.set(FIELD_STD_SSN, info.getSSN());

        m_grid.set(FIELD_STD_ETHNICITY, std.getPerson().getHispanicLatinoIndicator() ? BOOLEAN_YES : BOOLEAN_NO);
        m_grid.set(FIELD_STD_RACE_ASIAN, checkRace(std, RACE_CODE_ASIAN));
        m_grid.set(FIELD_STD_RACE_BLACK, checkRace(std, RACE_CODE_BLACK));
        m_grid.set(FIELD_STD_RACE_INDIAN, checkRace(std, RACE_CODE_INDIAN));
        m_grid.set(FIELD_STD_RACE_PACIFIC, checkRace(std, RACE_CODE_PACIFIC));
        m_grid.set(FIELD_STD_RACE_WHITE, checkRace(std, RACE_CODE_WHITE));

        m_grid.set(FIELD_STD_GENDER, std.getPerson().getGenderCode());
        m_grid.set(FIELD_STD_LEGAL_NAME, info.formatStudentLegalName());
        m_grid.set(FIELD_STD_BIRTH_DATE, std.getPerson().getDob());
        m_grid.set(FIELD_STD_GRADE, info.getGradeLevel(m_startDate));
    }
}
