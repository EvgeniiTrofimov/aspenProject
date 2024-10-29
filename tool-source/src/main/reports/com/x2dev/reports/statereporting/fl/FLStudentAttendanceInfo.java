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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper;
import com.x2dev.reports.statereporting.fl.FLStudentAttendanceReportData.StudentPeriodAttendanceDataset;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLStudentAttendanceInfo.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStudentAttendanceInfo {

    /**
     * The Class AttendanceCalendar.
     */
    public class AttendanceCalendar {
        private AttendanceHistory m_attendanceHistory;
        private SisSchool m_skl;

        /**
         * Gets the attendance history.
         *
         * @return Attendance history
         */
        public AttendanceHistory getAttendanceHistory() {
            return m_attendanceHistory;
        }

        /**
         * Gets the skl.
         *
         * @return Sis school
         */
        public SisSchool getSkl() {
            return m_skl;
        }

        /**
         * Sets the skl.
         *
         * @param skl void
         */
        public void setSkl(SisSchool skl) {
            m_skl = skl;
        }

        /**
         * Gets the data.
         *
         * @return List
         */
        public List<List<Daystamp>> getData() {
            return m_data;
        }

        /**
         * Sets the data.
         *
         * @param data void
         */
        public void setData(List<List<Daystamp>> data) {
            m_data = data;
        }
    }

    /**
     * The Class AttendanceHistory.
     */
    public class AttendanceHistory {
        List<PlainDate> m_attEntryDates = new ArrayList();
        List<SisSchool> m_attSchools = new ArrayList();
        List<PlainDate> m_attWithdrawalDates = new ArrayList();

        /**
         * Gets the entry dates.
         *
         * @return List
         */
        public List<PlainDate> getEntryDates() {
            return m_attEntryDates;
        }

        /**
         * Gets the schools.
         *
         * @return List
         */
        public List<SisSchool> getSchools() {
            return m_attSchools;
        }

        /**
         * Gets the withdrawal dates.
         *
         * @return List
         */
        public List<PlainDate> getWithdrawalDates() {
            return m_attWithdrawalDates;
        }
    }

    /**
     * The Class Daystamp.
     */
    public class Daystamp {
        private static final String FLAG_NON_MEMBERSHIP = "$";
        private static final String FLAG_NON_SCHOOL = "*";
        private static final String FLAG_NOT_VALID = "/";

        private Map<String, String> m_attendanceCode = new HashMap<>();
        private PlainDate m_date;
        private String m_entranceCode;
        private boolean m_isNonMembershipDay;
        private boolean m_isNonSchoolDay;
        private boolean m_isNotValidDay;
        private String m_withdrawalCode;

        /**
         * Instantiates a new daystamp.
         *
         * @param date PlainDate
         */
        public Daystamp(PlainDate date) {
            m_date = date;
        }

        /**
         * Instantiates a new daystamp.
         *
         * @param date PlainDate
         * @param attendanceCode String
         * @param entranceCode String
         * @param withdrawalCode String
         */
        public Daystamp(PlainDate date, String attendanceCode, String entranceCode, String withdrawalCode) {
            m_date = date;
            m_entranceCode = entranceCode;
            m_withdrawalCode = withdrawalCode;
            setAttendanceCode(attendanceCode);
        }

        /**
         * Clear.
         */
        public void clear() {
            m_entranceCode = null;
            m_withdrawalCode = null;
            m_attendanceCode.clear();
            m_isNonMembershipDay = false;
            setNonSchoolDay(false);
        }

        /**
         * Copy.
         *
         * @return Daystamp
         */
        public Daystamp copy() {
            Daystamp res = new Daystamp(m_date);
            res.m_attendanceCode = m_attendanceCode;
            res.m_entranceCode = m_entranceCode;
            res.m_isNonMembershipDay = m_isNonMembershipDay;
            res.m_isNonSchoolDay = m_isNonSchoolDay;
            res.m_isNotValidDay = m_isNotValidDay;
            res.m_withdrawalCode = m_withdrawalCode;
            return res;
        }

        /**
         * Gets the date.
         *
         * @return Plain date
         */
        public PlainDate getDate() {
            return m_date;
        }

        /**
         * Checks if is non school day.
         *
         * @return true, if is non school day
         */
        public boolean isNonSchoolDay() {
            return m_isNonSchoolDay;
        }

        /**
         * Checks if is not valid day.
         *
         * @return true, if is not valid day
         */
        public boolean isNotValidDay() {
            return m_isNotValidDay;
        }

        /**
         * Sets the attendance code.
         *
         * @param value void
         */
        public void setAttendanceCode(String value) {
            m_attendanceCode.put("", value);
        }

        /**
         * Sets the attendance code.
         *
         * @param period String
         * @param value String
         */
        public void setAttendanceCode(String period, String value) {
            m_attendanceCode.put(period, value);
        }

        /**
         * Sets the entrance code.
         *
         * @param value void
         */
        public void setEntranceCode(String value) {
            m_entranceCode = value;
        }

        /**
         * Sets the non membership day.
         *
         * @param nonMembershipDay void
         */
        public void setNonMembershipDay(boolean nonMembershipDay) {
            m_isNonMembershipDay = nonMembershipDay;
        }

        /**
         * Sets the non school day.
         *
         * @param nonSchool void
         */
        public void setNonSchoolDay(boolean nonSchool) {
            m_isNonSchoolDay = nonSchool;
        }

        /**
         * Sets the not valid day.
         *
         * @param nonValid void
         */
        public void setNotValidDay(boolean nonValid) {
            m_isNotValidDay = nonValid;
        }

        /**
         * Sets the wihdrawal code.
         *
         * @param value void
         */
        public void setWihdrawalCode(String value) {
            m_withdrawalCode = value;
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return toString(null);
        }

        /**
         * To string.
         *
         * @param period String
         * @return String
         */
        public String toString(String period) {
            if (m_isNotValidDay) {
                return FLAG_NOT_VALID;
            }
            if (m_isNonSchoolDay) {
                return FLAG_NON_SCHOOL;
            }
            if (m_isNonMembershipDay) {
                return FLAG_NON_MEMBERSHIP;
            }

            StringBuilder sb = new StringBuilder();
            if (!StringUtils.isEmpty(m_entranceCode)) {
                sb.append(m_entranceCode);
            }
            if (!StringUtils.isEmpty(m_withdrawalCode)) {
                sb.append(m_withdrawalCode);
            }
            if (!m_attendanceCode.isEmpty()) {
                if (period == null || period.length() == 0) {
                    for (String p : m_attendanceCode.keySet()) {
                        sb.append(p);
                        sb.append(m_attendanceCode.get(p));
                    }
                } else {
                    String code = m_attendanceCode.get(period);
                    sb.append(code != null ? code : "");
                }
            }
            return sb.toString();
        }

    }

    /**
     * The Class MultiDayAttendanceCalendar.
     */
    public class MultiDayAttendanceCalendar {
        private List<Daystamp> m_days;
        private SisSchool m_skl;

        /**
         * Adds the daystamp.
         *
         * @param ds Daystamp
         */
        public void addDaystamp(Daystamp ds) {
            if (m_days == null) {
                m_days = new ArrayList<>();
            }
            m_days.add(ds);
        }

        /**
         * Gets the begin.
         *
         * @return Plain date
         */
        public PlainDate getBegin() {
            return m_days != null && !m_days.isEmpty() ? m_days.get(0).getDate() : null;
        }

        /**
         * Gets the days.
         *
         * @return List
         */
        public List<Daystamp> getDays() {
            return m_days;
        }

        /**
         * Gets the end.
         *
         * @return Plain date
         */
        public PlainDate getEnd() {
            return m_days != null && !m_days.isEmpty() ? m_days.get(m_days.size() - 1).getDate() : null;
        }

        /**
         * Gets the skl.
         *
         * @return Sis school
         */
        public SisSchool getSkl() {
            return m_skl;
        }

        /**
         * Sets the data.
         *
         * @param days void
         */
        public void setData(List<Daystamp> days) {
            m_days = days;
        }

        /**
         * Sets the skl.
         *
         * @param skl void
         */
        public void setSkl(SisSchool skl) {
            m_skl = skl;
        }
    }

    private static final int INITIAL_MAP_SIZE = 1000;

    private Calendar m_calendar;
    private List<List<Daystamp>> m_data;
    private Map<PlainDate, Daystamp> m_dataMap;

    private Map<String, Collection<SchoolCalendarDate>> m_nonSchoolDaysMap;
    private Map<String, List<SchedulePeriod>> m_periodsBySchool;
    private FLStudentAttendanceReportData m_reportData;
    private PlainDate m_startDate;
    private FLStudentHelper m_studentHelper;
    private StudentPeriodAttendanceDataset m_studentPeriodAttendanceDataset;

    /**
     * Instantiates a new FL student attendance info.
     *
     * @param reportData FLStudentAttendanceReportData
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @throws X2BaseException exception
     */
    public FLStudentAttendanceInfo(FLStudentAttendanceReportData reportData, PlainDate startDate, PlainDate endDate)
            throws X2BaseException {
        m_reportData = reportData;
        m_calendar = Calendar.getInstance();
        m_startDate = startDate;
        m_studentHelper = new FLStudentHelper(m_reportData);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, startDate);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, endDate);

        m_studentPeriodAttendanceDataset = m_reportData.new StudentPeriodAttendanceDataset(startDate, endDate);

        initDataMap();
        initNonSchoolDays();
    }

    /**
     * Check race.
     *
     * @param std SisStudent
     * @param raceCode String
     * @return true, if successful
     */
    public boolean checkRace(SisStudent std, String raceCode) {
        boolean res = false;
        Collection<Race> races = m_studentHelper.getStudentRaceMap().get(std.getPersonOid());
        if (races != null) {
            for (Race race : races) {
                if (raceCode.equalsIgnoreCase(race.getRaceCode())) {
                    res = true;
                    break;
                }
            }
        }
        return res;
    }

    /**
     * Gets the field value.
     *
     * @param bean X2BaseBean
     * @param field DataDictionaryField
     * @return Object
     * @throws X2BaseException exception
     */
    public Object getFieldValue(X2BaseBean bean, DataDictionaryField field) throws X2BaseException {
        return m_reportData.getFieldValue(bean, field);
    }

    /**
     * Gets the periods.
     *
     * @param skl SisSchool
     * @return List
     */
    public List<SchedulePeriod> getPeriods(SisSchool skl) {
        if (m_periodsBySchool == null) {
            X2Criteria scheduleCriteria = new X2Criteria();
            scheduleCriteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                    m_reportData.getCurrentContext().getOid());
            SubQuery scheduleSubQuery = new SubQuery(SchoolScheduleContext.class,
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, scheduleCriteria);

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(SchedulePeriod.COL_SCHEDULE_OID, scheduleSubQuery);

            BeanQuery query = new BeanQuery(SchedulePeriod.class, criteria);

            query.addOrderBy(SchedulePeriod.COL_ID, true);
            m_periodsBySchool = m_reportData.getBroker().getGroupedCollectionByQuery(query,
                    SchedulePeriod.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.COL_SCHOOL_OID, 500);
        }
        return m_periodsBySchool.get(skl.getOid());
    }

    /**
     * Gets the student calendar.
     *
     * @param std SisStudent
     * @return List
     * @throws X2BaseException exception
     */
    public List<AttendanceCalendar> getStudentCalendar(SisStudent std) throws X2BaseException {
        List<AttendanceCalendar> res = new ArrayList<>();
        if (std != null) {
            List<StudentAttendance> records = new ArrayList<StudentAttendance>();
            List<StudentAttendance> all = m_studentHelper.getStudentAttendances(std.getOid());
            SisSchool skl = null;
            if (all != null && !all.isEmpty()) {
                for (StudentAttendance attendance : all) {
                    if (skl == null || !skl.getOid().equals(attendance.getSchoolOid()) ||
                            !std.getOid().equals(attendance.getStudentOid())) {
                        addPage(res, skl, std, records);
                        skl = attendance.getSchool();
                        records.clear();
                    }
                    records.add(attendance);
                }
            }
            addPage(res, skl, std, records);
        }
        return res;
    }

    /**
     * Gets the student calendar multi day.
     *
     * @param std SisStudent
     * @param daysPerRow int
     * @return List
     * @throws X2BaseException exception
     */
    public List<MultiDayAttendanceCalendar> getStudentCalendarMultiDay(SisStudent std, int daysPerRow)
            throws X2BaseException {
        List<MultiDayAttendanceCalendar> res = new ArrayList<>();

        List<AttendanceCalendar> calendar = getStudentCalendar(std);
        if (calendar != null) {
            int count = 0;
            MultiDayAttendanceCalendar current = null;
            for (AttendanceCalendar ac : calendar) {
                for (List<Daystamp> row : ac.getData()) {
                    for (Daystamp ds : row) {
                        if (ds.getDate() != null && !ds.isNotValidDay() && !ds.isNonSchoolDay()) {
                            if (count++ % daysPerRow == 0) {
                                current = new MultiDayAttendanceCalendar();
                                current.setSkl(ac.getSkl());
                                res.add(current);
                            }
                            current.addDaystamp(ds);
                        }
                    }
                }
            }
            if (current != null) {
                res.add(current);
            }
        }
        return res;
    }

    /**
     * Gets the student helper.
     *
     * @return FL student helper
     */
    public FLStudentHelper getStudentHelper() {
        return m_studentHelper;
    }

    /**
     * Gets the student periods calendar.
     *
     * @param std SisStudent
     * @return List
     * @throws X2BaseException exception
     */
    public List<AttendanceCalendar> getStudentPeriodsCalendar(SisStudent std) throws X2BaseException {
        List<AttendanceCalendar> res = new ArrayList<>();
        if (std != null) {
            List<StudentPeriodAttendance> records = new ArrayList<StudentPeriodAttendance>();
            List<StudentPeriodAttendance> all = m_studentPeriodAttendanceDataset.getAttendances(std.getOid());
            SisSchool skl = null;
            if (all != null && !all.isEmpty()) {
                for (StudentPeriodAttendance attendance : all) {
                    if (skl == null || !skl.getOid().equals(attendance.getSchoolOid()) ||
                            !std.getOid().equals(attendance.getStudentOid())) {
                        addPagePat(res, skl, std, records);
                        skl = attendance.getSchool();
                        records.clear();
                    }
                    records.add(attendance);
                }
            }
            addPagePat(res, skl, std, records);
        }
        return res;
    }

    /**
     * Gets the student periods calendar multi day.
     *
     * @param std SisStudent
     * @param daysPerRow int
     * @return List
     * @throws X2BaseException exception
     */
    public List<MultiDayAttendanceCalendar> getStudentPeriodsCalendarMultiDay(SisStudent std, int daysPerRow)
            throws X2BaseException {
        List<MultiDayAttendanceCalendar> res = new ArrayList<>();

        List<AttendanceCalendar> calendar = getStudentPeriodsCalendar(std);
        if (calendar != null) {
            int count = 0;
            MultiDayAttendanceCalendar current = null;
            for (AttendanceCalendar ac : calendar) {
                for (List<Daystamp> row : ac.getData()) {
                    for (Daystamp ds : row) {
                        if (ds.getDate() != null && !ds.isNotValidDay() && !ds.isNonSchoolDay()) {
                            if (count++ % daysPerRow == 0) {
                                current = new MultiDayAttendanceCalendar();
                                current.setSkl(ac.getSkl());
                                res.add(current);
                            }
                            current.addDaystamp(ds);
                        }
                    }
                }
            }
            if (current != null) {
                res.add(current);
            }
        }
        return res;
    }

    /**
     * Adds the page.
     *
     * @param data List<AttendanceCalendar>
     * @param skl SisSchool
     * @param std SisStudent
     * @param records List<StudentAttendance>
     * @throws X2BaseException exception
     */
    private void addPage(List<AttendanceCalendar> data,
                         SisSchool skl,
                         SisStudent std,
                         List<StudentAttendance> records)
            throws X2BaseException {
        if (skl == null || std == null) {
            return;
        }

        resetDataMap(skl);

        if (records != null) {
            for (StudentAttendance attendance : records) {
                if (m_dataMap.containsKey(attendance.getDate())) {
                    m_dataMap.get(attendance.getDate()).setAttendanceCode(attendance.getCodeView());
                }
            }
        }
        applyEnrollments(std);

        AttendanceCalendar ac = cloneAttendanceCalendar(skl);
        ac.m_attendanceHistory = createAttendanceHistory(std);
        data.add(ac);
    }

    /**
     * Adds the page pat.
     *
     * @param data List<AttendanceCalendar>
     * @param skl SisSchool
     * @param std SisStudent
     * @param records List<StudentPeriodAttendance>
     * @throws X2BaseException exception
     */
    private void addPagePat(List<AttendanceCalendar> data,
                            SisSchool skl,
                            SisStudent std,
                            List<StudentPeriodAttendance> records)
            throws X2BaseException {
        if (std == null) {
            return;
        }

        if (skl == null) {
            skl = std.getSchool();
        }

        if (skl == null) {
            return;
        }
        resetDataMap(skl);

        if (records != null) {
            for (StudentPeriodAttendance attendance : records) {
                String code = attendance.getCodeView();
                if (!StringUtils.isEmpty(code) && !StringUtils.isEmpty(attendance.getPeriodView())
                        && m_dataMap.containsKey(attendance.getDate())) {

                    for (String period : attendance.getPeriodView().split(",")) {
                        if (!StringUtils.isEmpty(period)) {
                            m_dataMap.get(attendance.getDate()).setAttendanceCode(period, attendance.getCodeView());
                        }
                    }
                }
            }
        }
        applyEnrollments(std);

        AttendanceCalendar ac = cloneAttendanceCalendar(skl);
        ac.m_attendanceHistory = createAttendanceHistory(std);
        data.add(ac);
    }

    /**
     * Apply enrollments.
     *
     * @param std SisStudent
     */
    private void applyEnrollments(SisStudent std) {
        m_calendar.setTime(m_startDate);
        for (StudentEnrollmentSpan espan : m_studentHelper.getStudentEnrollmentSpans(std, false)) {
            StudentEnrollment first = espan.getFirstActiveEnrollment();
            if (first != null) {
                while (m_calendar.before(first.getEnrollmentDate())) {
                    PlainDate key = new PlainDate(m_calendar.getTime());
                    if (m_dataMap.containsKey(key)) {
                        m_dataMap.get(key).setNonMembershipDay(true);
                    }
                    m_calendar.add(Calendar.DATE, 1);
                }
            }
            if (first != null && m_dataMap.containsKey(first.getEnrollmentDate())) {
                m_dataMap.get(first.getEnrollmentDate()).setEntranceCode(first.getEnrollmentCode());

            }
            if (espan.getLastActiveDate() != null) {
                StudentEnrollment last =
                        espan.getEnrollmentForDate(espan.getLastActiveDate(), StudentEnrollment.WITHDRAWAL);
                if (last != null && m_dataMap.containsKey(last.getEnrollmentDate())) {
                    m_dataMap.get(last.getEnrollmentDate()).setWihdrawalCode(last.getEnrollmentCode());
                }
            }
        }
    }

    /**
     * Clear data map.
     */
    private void clearDataMap() {
        for (List<Daystamp> row : m_data) {
            boolean first = true;
            for (Daystamp data : row) {
                if (first) {
                    first = false;
                } else {
                    data.clear();
                }
            }
        }
    }

    /**
     * Clone attendance calendar.
     *
     * @param skl SisSchool
     * @return AttendanceCalendar
     */
    private AttendanceCalendar cloneAttendanceCalendar(SisSchool skl) {
        AttendanceCalendar ac = new AttendanceCalendar();
        ac.setSkl(skl);
        List<List<Daystamp>> dcopy = new ArrayList<>();
        for (List<Daystamp> row : m_data) {
            List<Daystamp> rcopy = new ArrayList<>();
            for (int i = 0; i < row.size(); i++) {
                rcopy.add(row.get(i));
            }
            dcopy.add(rcopy);
        }
        ac.setData(dcopy);
        return ac;
    }

    /**
     * Creates the attendance history.
     *
     * @param std SisStudent
     * @return AttendanceHistory
     */
    private AttendanceHistory createAttendanceHistory(SisStudent std) {
        AttendanceHistory history = new AttendanceHistory();
        for (StudentEnrollmentSpan espan : m_studentHelper.getStudentEnrollmentSpans(std, false)) {
            StudentEnrollment first = espan.getFirstActiveEnrollment();
            if (first != null && m_dataMap.containsKey(first.getEnrollmentDate())) {
                history.m_attSchools.add(first.getSchool());
                history.m_attEntryDates.add(first.getEnrollmentDate());
                PlainDate wDate = null;
                if (espan.getLastActiveDate() != null) {
                    StudentEnrollment last =
                            espan.getEnrollmentForDate(espan.getLastActiveDate(), StudentEnrollment.WITHDRAWAL);
                    if (last != null && m_dataMap.containsKey(last.getEnrollmentDate())) {
                        wDate = last.getEnrollmentDate();
                    }
                }
                history.m_attWithdrawalDates.add(wDate);
            }
        }
        return history;
    }

    /**
     * Inits the data map.
     */
    private void initDataMap() {
        m_data = new ArrayList<List<Daystamp>>();
        m_dataMap = new HashMap<>();

        m_calendar.setTime(m_reportData.getCurrentContext().getStartDate());
        m_calendar.set(Calendar.DAY_OF_MONTH, 1);

        SimpleDateFormat mFormatter = new SimpleDateFormat("MMM");
        int month = m_calendar.get(Calendar.MONTH);
        do {
            List<Daystamp> row = new ArrayList<Daystamp>();
            row.add(new Daystamp(null, mFormatter.format(m_calendar.getTime()), null, null));
            for (int i = 0; i < 31; i++) {
                PlainDate pdt = new PlainDate(m_calendar.getTime());
                Daystamp data = new Daystamp(pdt);
                if (month == m_calendar.get(Calendar.MONTH)) {
                    m_dataMap.put(pdt, data);
                    m_calendar.add(Calendar.DATE, 1);
                } else {
                    data.setNotValidDay(true);
                }
                row.add(data);
            }
            m_data.add(row);
            month = m_calendar.get(Calendar.MONTH);

        } while (m_calendar.getTime().before(m_reportData.getCurrentContext().getEndDate()));
    }

    /**
     * Inits the non school days.
     */
    private void initNonSchoolDays() {

        X2Criteria criteria = new X2Criteria();
        if (m_reportData.isSchoolContext()) {
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                    m_reportData.getSchool().getOid());
        }
        criteria.addNotEmpty(SchoolCalendarDate.COL_SCHEDULE_DAY_TYPE, m_reportData.getBroker().getPersistenceKey());
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_reportData.getCurrentContext().getOid());
        QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        m_nonSchoolDaysMap = m_reportData.getBroker().getGroupedCollectionByQuery(query,
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                INITIAL_MAP_SIZE);

    }

    /**
     * Reset data map.
     *
     * @param skl SisSchool
     */
    private void resetDataMap(SisSchool skl) {
        clearDataMap();

        if (m_nonSchoolDaysMap.containsKey(skl.getOid())) {
            for (SchoolCalendarDate nonSchool : m_nonSchoolDaysMap.get(skl.getOid())) {
                if (m_dataMap.containsKey(nonSchool.getDate())) {
                    m_dataMap.get(nonSchool.getDate()).setNonSchoolDay(true);
                }
            }
        }
    }


}
