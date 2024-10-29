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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.x2dev.procedures.statereporting.fl.FLScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStateReportData;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

/**
 * The Class FLStudentAttendanceReportData.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStudentAttendanceReportData extends FLStateReportData {

    /**
     * The Class StudentPeriodAttendanceDataset.
     */
    public class StudentPeriodAttendanceDataset {
        private PlainDate m_endDate;
        private Map<String, List<StudentPeriodAttendance>> m_map = null;
        private PlainDate m_startDate;

        /**
         * Instantiates a new student period attendance dataset.
         *
         * @param startDate the start date
         * @param endDate the end date
         */
        public StudentPeriodAttendanceDataset(PlainDate startDate, PlainDate endDate) {
            m_startDate = startDate;
            m_endDate = endDate;
        }

        /**
         * Equals.
         *
         * @param obj the obj
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (obj instanceof StudentPeriodAttendanceDataset) {
                StudentPeriodAttendanceDataset data = (StudentPeriodAttendanceDataset) obj;
                boolean value = safeEquals(m_startDate, data.m_startDate) && safeEquals(m_endDate, data.m_endDate);
                if (!value) {
                    System.out.println();
                }
                return value;
            }
            return false;
        }


        /**
         * Gets the attendances.
         *
         * @param oid the oid
         * @return the attendances
         */
        public List<StudentPeriodAttendance> getAttendances(String oid) {
            if (m_map == null) {
                initializeAttendances();
            }
            return m_map.get(oid);
        }

        /**
         * Hash code.
         *
         * @return the int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_startDate.hashCode() + m_endDate.hashCode();
        }

        /**
         * Initialize attendances.
         */
        private void initializeAttendances() {

            X2Criteria studentPeriodAttendanceCriteria = new X2Criteria();
            studentPeriodAttendanceCriteria.addGreaterOrEqualThan(StudentPeriodAttendance.COL_DATE, m_startDate);
            studentPeriodAttendanceCriteria.addLessOrEqualThan(StudentPeriodAttendance.COL_DATE, m_endDate);

            // Apply school criteria.
            if (isSchoolContext()) {
                studentPeriodAttendanceCriteria.addEqualTo(StudentPeriodAttendance.REL_SCHOOL, getSchool().getOid());
            }

            // Apply user input criteria.
            applyInputCriteria(studentPeriodAttendanceCriteria, false, StudentPeriodAttendance.REL_STUDENT);

            BeanQuery studentPeriodAttendanceQuery =
                    new BeanQuery(StudentPeriodAttendance.class, studentPeriodAttendanceCriteria);

            studentPeriodAttendanceQuery.addOrderBy(StudentPeriodAttendance.COL_STUDENT_OID, true);
            studentPeriodAttendanceQuery.addOrderBy(StudentPeriodAttendance.COL_DATE, true);
            studentPeriodAttendanceQuery.addOrderBy(StudentPeriodAttendance.COL_PERIOD_VIEW, true);
            m_map = getBroker().getGroupedCollectionByQuery(studentPeriodAttendanceQuery,
                    StudentPeriodAttendance.COL_STUDENT_OID, 500);
        }

        /**
         * Safe equals.
         *
         * @param obj1 the obj 1
         * @param obj2 the obj 2
         * @return true, if successful
         */
        private boolean safeEquals(Object obj1, Object obj2) {
            return (obj1 == null && obj2 == null) || (obj1 != null && obj1.equals(obj2));
        }
    }

    /**
     * The Class AttendanceSummary.
     */
    public class AttendanceSummary {
        private int m_absentExc;
        private int m_absentUnexc;
        private int m_absentUnexcNotDisc;
        private int m_tardy;

        /**
         * Adds the absent exc.
         */
        public void addAbsentExc() {
            m_absentExc++;
        }

        /**
         * Adds the absent unexc.
         */
        public void addAbsentUnexc() {
            m_absentUnexc++;
        }

        /**
         * Adds the absent unexc not disc.
         */
        public void addAbsentUnexcNotDisc() {
            m_absentUnexcNotDisc++;
        }

        /**
         * Adds the tardy.
         */
        public void addTardy() {
            m_tardy++;
        }

        /**
         * Gets the absent.
         *
         * @return int
         */
        public int getAbsent() {
            return m_absentExc + m_absentUnexc + m_absentUnexcNotDisc;
        }

        /**
         * Gets the absent exc.
         *
         * @return the absentExc
         */
        public int getAbsentExc() {
            return m_absentExc;
        }

        /**
         * Gets the absent unexc.
         *
         * @return the absentUnexc
         */
        public int getAbsentUnexc() {
            return m_absentUnexc;
        }


        /**
         * Gets the absent unexc not disc.
         *
         * @return the absentUnexcNotDisc
         */
        public int getAbsentUnexcNotDisc() {
            return m_absentUnexcNotDisc;
        }

        /**
         * Gets the tardy.
         *
         * @return the tardy
         */
        public int getTardy() {
            return m_tardy;
        }

        /**
         * Sets the absent exc.
         *
         * @param absentExc void
         */
        public void setAbsentExc(int absentExc) {
            this.m_absentExc = absentExc;
        }

        /**
         * Sets the absent unexc.
         *
         * @param absentUnexc void
         */
        public void setAbsentUnexc(int absentUnexc) {
            this.m_absentUnexc = absentUnexc;
        }

        /**
         * Sets the absent unexc not disc.
         *
         * @param absentUnexcNotDisc void
         */
        public void setAbsentUnexcNotDisc(int absentUnexcNotDisc) {
            this.m_absentUnexcNotDisc = absentUnexcNotDisc;
        }

        /**
         * Sets the tardy.
         *
         * @param tardy void
         */
        public void setTardy(int tardy) {
            this.m_tardy = tardy;
        }
    }

    private static final String PERIOD_NUMBER_MASK = "%02d%02d";

    private Map<String, Map<String, Map<PlainDate, Map<String, Integer>>>> m_enrolled = new HashMap<>();
    private Map<String, Collection<ScheduleTermDate>> m_scheduleTermDates = new HashMap<>();
    private Map<StudentPeriodAttendanceDataset, StudentPeriodAttendanceDataset> m_studentPeriodAttendanceDatasets =
            new HashMap();

    private Map<String, PlainDate> m_termStartDates = new HashMap<>();

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getBeanClass()
     */
    @Override
    public Class getBeanClass() {
        return StudentAttendance.class;
    }

    /**
     * Gets the enrolled days.
     *
     * @param termDaysMap Map<String,List<PlainDate>>
     * @param student SisStudent
     * @param term ScheduleTerm
     * @param period SchedulePeriod
     * @return Integer
     * @throws X2BaseException exception
     */
    public Integer getEnrolledDays(Map<String, List<PlainDate>> termDaysMap,
                                   SisStudent student,
                                   ScheduleTerm term,
                                   SchedulePeriod period)
            throws X2BaseException {
        int days = 0;

        List<PlainDate> dates = termDaysMap.get(term.getOid());
        if (dates != null) {
            Map<String, Map<PlainDate, Map<String, Integer>>> termEnrolled = m_enrolled.get(student.getOid());
            if (termEnrolled == null) {
                termEnrolled = new HashMap<>();
                m_enrolled.put(student.getOid(), termEnrolled);
            }

            Map<PlainDate, Map<String, Integer>> dateEnrolled = termEnrolled.get(term.getOid());
            if (dateEnrolled == null) {
                dateEnrolled = new HashMap<>();
                termEnrolled.put(term.getOid(), dateEnrolled);

                ArrayList<StudentScheduleInfo> scheduleInfos = new ArrayList<>();
                Collection<ScheduleTermDate> termDates = m_scheduleTermDates.get(term.getOid());
                if (termDates == null) {
                    termDates = term.getScheduleTermDates(getBroker());
                    m_scheduleTermDates.put(term.getOid(), termDates);
                }
                for (ScheduleTermDate std : termDates) {
                    FLScheduleHelper scheduleHelper =
                            new FLScheduleHelper(this, std.getStartDate(), std.getEndDate());
                    StudentScheduleHelper studentScheduleHelper =
                            getStudentHelper().new StudentScheduleHelper(scheduleHelper, std.getStartDate(),
                                    std.getEndDate());
                    for (StudentScheduleInfo ssi : studentScheduleHelper.getStudentScheduleInfo(student)) {
                        String periodNumber = ssi.getMasterScheduleInfo().getPeriodNumber();
                        PlainDate date = ssi.getEntryDate();
                        while (date.before(ssi.getExitDate()) || date.compareTo(ssi.getExitDate()) == 0) {
                            if (dates.contains(date)) {
                                Map<String, Integer> periodEnrolled = dateEnrolled.get(date);
                                if (periodEnrolled == null) {
                                    periodEnrolled = new HashMap<>();
                                    dateEnrolled.put(date, periodEnrolled);
                                }
                                Integer count = periodEnrolled.get(periodNumber);
                                periodEnrolled.put(periodNumber,
                                        Integer.valueOf(count == null ? 1 : count.intValue() + 1));
                            }
                            date = DateUtils.add(date, 1);
                        }
                        scheduleInfos.add(ssi);
                    }
                }
            }

            Integer pn = Integer.valueOf(period.getNumber());
            String periodNumber = String.format(PERIOD_NUMBER_MASK, pn, pn);

            for (PlainDate date : dateEnrolled.keySet()) {
                Map<String, Integer> periodEnrolled = dateEnrolled.get(date);
                for (String periodKey : periodEnrolled.keySet()) {
                    if (periodKey.equals(periodNumber)) {
                        days += periodEnrolled.get(periodKey).intValue();
                    }
                }
            }
        }
        return Integer.valueOf(days);
    }

    /**
     * Gets the schedule terms.
     *
     * @param school SisSchool
     * @param student SisStudent
     * @return Collection
     * @throws X2BaseException exception
     */
    public Collection<ScheduleTerm> getScheduleTerms(SisSchool school, SisStudent student) throws X2BaseException {
        Collection<ScheduleTerm> res = new HashSet<ScheduleTerm>();
        for (StudentScheduleSpan sss : getStudentHelper().getStudentScheduleSpans(student)) {
            if (sss.getSection().getSchedule().getSchoolOid().equals(school.getOid())) {
                res.add(sss.getSection().getScheduleTerm());
            }
        }
        List<ScheduleTerm> scheduleTerms = new ArrayList<ScheduleTerm>(res);
        Collections.sort(scheduleTerms, new Comparator<ScheduleTerm>() {
            @Override
            public int compare(ScheduleTerm o1, ScheduleTerm o2) {
                PlainDate dt1 = getStartDate(o1);
                PlainDate dt2 = getStartDate(o2);
                return dt1 != null ? dt1.compareTo(dt2) : 0;
            }

            private PlainDate getStartDate(ScheduleTerm o) {
                PlainDate date = m_termStartDates.get(o.getOid());
                if (date == null) {
                    Collection<ScheduleTermDate> dates = o.getScheduleTermDates(getBroker());
                    if (dates != null && !dates.isEmpty()) {
                        for (ScheduleTermDate st : dates) {
                            if (st.getStartDate() != null) {
                                if (date == null || date.after(st.getStartDate())) {
                                    date = st.getStartDate();
                                }
                            }
                        }
                    }
                    m_termStartDates.put(o.getOid(), date);
                }
                return date;
            }
        });
        return scheduleTerms;
    }

    /**
     * Gets the student term period attendance.
     *
     * @param beginDate PlainDate
     * @param endDate PlainDate
     * @param stdOid String
     * @return Map
     */
    public Map<String, AttendanceSummary> getStudentTermPeriodAttendance(PlainDate beginDate,
                                                                         PlainDate endDate,
                                                                         String stdOid) {
        Map<String, AttendanceSummary> res = new HashMap<>();

        StudentPeriodAttendanceDataset in = new StudentPeriodAttendanceDataset(beginDate, endDate);
        StudentPeriodAttendanceDataset dataset = m_studentPeriodAttendanceDatasets.get(in);
        if (dataset == null) {
            dataset = in;
            m_studentPeriodAttendanceDatasets.put(dataset, dataset);
        }
        List<StudentPeriodAttendance> attendances = dataset.getAttendances(stdOid);
        if (attendances != null) {
            for (StudentPeriodAttendance pat : attendances) {
                String periods = pat.getPeriodView();
                if (!StringUtils.isEmpty(periods)) {
                    for (String period : periods.split(",")) {
                        AttendanceSummary summary = res.get(period);
                        if (summary == null) {
                            summary = new AttendanceSummary();
                            res.put(period, summary);
                        }
                        if (pat.getTardyIndicator()) {
                            summary.addTardy();
                        }
                        if (pat.getAbsentIndicator()) {
                            if (pat.getExcusedIndicator()) {
                                summary.addAbsentExc();
                            } else {
                                summary.addAbsentUnexc();
                                if (StringUtils.isEmpty(pat.getReasonCode())) {
                                    summary.addAbsentUnexcNotDisc();
                                }
                            }
                        }
                    }
                }
            }
        }
        return res;
    }

    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getSurveyPeriodCode()
     */
    @Override
    public String getSurveyPeriodCode() {
        return "";
    }
}
