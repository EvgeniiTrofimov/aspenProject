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

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.business.StudentAttendanceCodeViewBuilder;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for Fujitsu's "Class To Daily Attendance" procedure.
 *
 * @author X2 Development Corporation
 */
public class ClassToDailyAttendanceProcedure extends ProcedureJavaSource {
    // Input parameters
    private static final String PARAM_CONTEXT_OID = "contextOid";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_PREVIEW_MODE = "previewMode";
    private static final String PARAM_START_DATE = "startDate";

    // Administrative daily attendance codes
    private static final String ADMINISTRATIVE_CODES = "AUTH, ISS, OSS";

    // Use procedure field
    private static final String USE_PROCEDURE_FIELD = SisSchool.COL_FIELD_A002;

    // Private variables
    private Map<String, Schedule> m_activeSchedulesBySchoolOid;
    private DistrictSchoolYearContext m_context;
    private Map<String, Map<PlainDate, StudentAttendance>> m_dailyAttendanceByStudentOid;
    private PlainDate m_endDate;
    private Map<SisStudent, Map<PlainDate, List<StudentPeriodAttendance>>> m_periodAttendanceByStudent;
    private PlainDate m_startDate;
    private Map<String, List<StudentSchedule>> m_studentSchedulesByStudentOid;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        ScheduleManager scheduleManager = new ScheduleManager(getBroker());

        List<String> administrativeCodes = StringUtils.convertDelimitedStringToList(ADMINISTRATIVE_CODES, ',', true);

        boolean previewMode = ((Boolean) getParameter(PARAM_PREVIEW_MODE)).booleanValue();
        if (previewMode) {
            logMessage("PREVIEW MODE --- PREVIEW MODE --- PREVIEW MODE\n\n");
        } else {
            logMessage("EXECUTED! --- EXECUTED! --- EXECUTED!\n\n");
        }

        int inserts = 0;
        int updates = 0;

        for (SisStudent student : m_periodAttendanceByStudent.keySet()) {
            Schedule activeSchedule = m_activeSchedulesBySchoolOid.get(student.getSchoolOid());
            if (activeSchedule != null) {
                String studentOid = student.getOid();

                List<StudentSchedule> studentSchedules = m_studentSchedulesByStudentOid.get(studentOid);
                if (studentSchedules != null && studentSchedules.size() > 0) {
                    Map<PlainDate, List<StudentPeriodAttendance>> periodAttendanceByDate =
                            m_periodAttendanceByStudent.get(student);
                    for (PlainDate date : periodAttendanceByDate.keySet()) {
                        StudentAttendance dailyAttendance = null;
                        boolean isAdministrativeCode = false;

                        Map<PlainDate, StudentAttendance> dailyAttendanceByDate =
                                m_dailyAttendanceByStudentOid.get(studentOid);
                        if (dailyAttendanceByDate != null) {
                            dailyAttendance = dailyAttendanceByDate.get(date);
                        }

                        if (dailyAttendance == null) {
                            dailyAttendance = new StudentAttendance(getBroker().getPersistenceKey());
                            dailyAttendance.setStudentOid(studentOid);
                            dailyAttendance.setSchoolOid(student.getSchoolOid());
                            dailyAttendance.setDate(date);
                        } else if (dailyAttendance.getCodeView() != null) {
                            for (String administrativeCode : administrativeCodes) {
                                if (dailyAttendance.getCodeView().contains(administrativeCode)) {
                                    isAdministrativeCode = true;
                                    break;
                                }
                            }
                        }

                        isAdministrativeCode = isAdministrativeCode ||
                                (dailyAttendance.getOtherCode() != null
                                        && administrativeCodes.contains(dailyAttendance.getOtherCode()))
                                ||
                                (dailyAttendance.getOtherCode02() != null
                                        && administrativeCodes.contains(dailyAttendance.getOtherCode02()));

                        if (!isAdministrativeCode) {
                            List<SchedulePeriod> periods = new LinkedList<SchedulePeriod>();
                            try {
                                periods = scheduleManager.getPeriods(activeSchedule, studentOid, date,
                                        activeSchedule.isElementary());
                            } catch (Exception e) {
                                // Do nothing.
                            }

                            Set<String> scheduledPeriods = new HashSet<String>();

                            for (StudentSchedule studentSchedule : studentSchedules) {
                                for (SchedulePeriod period : periods) {
                                    if (period.getScheduleIndicator() &&
                                            scheduleManager.isScheduled(studentSchedule.getSection(), date,
                                                    period.getOid(), null)) {
                                        scheduledPeriods.add(period.getId());
                                    }
                                }
                            }

                            if (scheduledPeriods != null && scheduledPeriods.size() > 0) {
                                int absentPeriods = 0;
                                int excusedPeriods = 0;

                                List<StudentPeriodAttendance> periodAttendances = periodAttendanceByDate.get(date);
                                for (StudentPeriodAttendance periodAttendance : periodAttendances) {
                                    String periodView = periodAttendance.getPeriodView();
                                    if (!StringUtils.isEmpty(periodView)) {
                                        List<String> coveredPeriods =
                                                StringUtils.convertDelimitedStringToList(periodView, ',', true);
                                        for (String coveredPeriod : coveredPeriods) {
                                            if (scheduledPeriods.contains(coveredPeriod)) {
                                                absentPeriods++;

                                                if (periodAttendance.getExcusedIndicator()) {
                                                    excusedPeriods++;
                                                }
                                            }
                                        }
                                    }
                                }

                                if (absentPeriods > 0) {
                                    dailyAttendance.setAbsentIndicator(true);

                                    if (absentPeriods == excusedPeriods) {
                                        dailyAttendance.setExcusedIndicator(true);
                                    }

                                    dailyAttendance.setPortionAbsent(new BigDecimal(absentPeriods)
                                            .divide(new BigDecimal(scheduledPeriods.size()), 4, RoundingMode.HALF_UP));
                                    dailyAttendance.setCodeView(StudentAttendanceCodeViewBuilder
                                            .getCodeView(dailyAttendance, (SisOrganization) getOrganization(), null));

                                    boolean newBean = dailyAttendance.isNew();
                                    if (newBean || dailyAttendance.isDirty()) {
                                        if (!previewMode) {
                                            getBroker().saveBeanForced(dailyAttendance);
                                        }

                                        if (newBean) {
                                            inserts++;
                                        } else {
                                            updates++;
                                        }
                                    }
                                }
                            } else {
                                logMessage(student.getNameView().toUpperCase() +
                                        " - Daily attendance percentage cannot be calculated because the student's school schedule has NO PERIODS. (School: "
                                        +
                                        student.getSchool().getName().toUpperCase() + ")");
                            }
                        }

                        if (dailyAttendance.isNew() || dailyAttendance.isDirty()) {
                            if (dailyAttendance.isNew()) {
                                dailyAttendance = null;
                            } else if (dailyAttendanceByDate != null) {
                                dailyAttendance = dailyAttendanceByDate.get(date);
                            }
                        }
                    }
                } else {
                    logMessage(student.getNameView().toUpperCase() +
                            " - Daily attendance percentage cannot be calculated because the student is NOT SCHEDULED FOR ANY SECTIONS.");
                }
            } else {
                logMessage(student.getNameView().toUpperCase() +
                        " - Daily attendance percentage cannot be calculated because the student's school has NO ACTIVE SCHEDULE. (School: "
                        +
                        student.getSchool().getName().toUpperCase() + ")");
            }
        }

        logMessage("\n\nCreated " + inserts + " new Daily Attendance records.");
        logMessage("Updated " + updates + " existing Daily Attendance records.");
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_context = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                (String) getParameter(PARAM_CONTEXT_OID));

        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        if (m_startDate.before(m_context.getStartDate())) {
            m_startDate = m_context.getStartDate();
        }

        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);
        if (m_endDate.after(m_context.getEndDate())) {
            m_endDate = m_context.getEndDate();
        }

        loadDailyAttendanceByStudentOid();

        loadPeriodAttendanceByStudentOid();

        loadActiveSchedulesBySchoolOid();

        loadStudentSchedulesByStudentOid();
    }

    /**
     * Loads a map of active schedules keyed to the school OID.
     */
    private void loadActiveSchedulesBySchoolOid() {
        m_activeSchedulesBySchoolOid = new HashMap<String, Schedule>(1024);

        Criteria criteria = new Criteria();
        criteria.addNotNull(SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID);
        criteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, m_context.getOid());

        QueryIterator iterator =
                getBroker().getIteratorByQuery(new QueryByCriteria(SchoolScheduleContext.class, criteria));
        try {
            while (iterator.hasNext()) {
                SchoolScheduleContext scheduleContext = (SchoolScheduleContext) iterator.next();

                Schedule activeSchedule = scheduleContext.getActiveSchedule();
                if (activeSchedule != null) {
                    m_activeSchedulesBySchoolOid.put(activeSchedule.getSchoolOid(), activeSchedule);
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a map of daily attendance records keyed to dates maps, keyed to the
     * student OID.
     */
    private void loadDailyAttendanceByStudentOid() {
        m_dailyAttendanceByStudentOid = new HashMap<String, Map<PlainDate, StudentAttendance>>(4096);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentAttendance.REL_SCHOOL + PATH_DELIMITER + USE_PROCEDURE_FIELD, "1");
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_startDate);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_endDate);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());
        }

        QueryIterator iterator = getBroker().getIteratorByQuery(new QueryByCriteria(StudentAttendance.class, criteria));
        try {
            while (iterator.hasNext()) {
                StudentAttendance attendance = (StudentAttendance) iterator.next();
                String studentOid = attendance.getStudentOid();

                Map<PlainDate, StudentAttendance> dailyAttendanceByDate = m_dailyAttendanceByStudentOid.get(studentOid);
                if (dailyAttendanceByDate == null) {
                    dailyAttendanceByDate = new HashMap<PlainDate, StudentAttendance>();
                }

                dailyAttendanceByDate.put(attendance.getDate(), attendance);
                m_dailyAttendanceByStudentOid.put(studentOid, dailyAttendanceByDate);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a map of period attendance lists keyed to dates maps, keyed to the
     * student.
     */
    private void loadPeriodAttendanceByStudentOid() {
        m_periodAttendanceByStudent =
                new LinkedHashMap<SisStudent, Map<PlainDate, List<StudentPeriodAttendance>>>(4096);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentPeriodAttendance.REL_SCHOOL + PATH_DELIMITER + USE_PROCEDURE_FIELD, "1");
        criteria.addGreaterOrEqualThan(StudentPeriodAttendance.COL_DATE, m_startDate);
        criteria.addLessOrEqualThan(StudentPeriodAttendance.COL_DATE, m_endDate);
        criteria.addEqualTo(StudentPeriodAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentPeriodAttendance.COL_SCHOOL_OID, getSchool().getOid());
        }

        QueryByCriteria query = new QueryByCriteria(StudentPeriodAttendance.class, criteria);
        query.addOrderByAscending(StudentPeriodAttendance.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
        query.addOrderByAscending(StudentPeriodAttendance.COL_DATE);
        query.addOrderByAscending(StudentPeriodAttendance.COL_PERIOD_VIEW);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentPeriodAttendance periodAttendance = (StudentPeriodAttendance) iterator.next();
                SisStudent student = periodAttendance.getStudent();
                PlainDate date = periodAttendance.getDate();

                Map<PlainDate, List<StudentPeriodAttendance>> periodAttendanceByDate =
                        m_periodAttendanceByStudent.get(student);
                if (periodAttendanceByDate == null) {
                    periodAttendanceByDate = new LinkedHashMap<PlainDate, List<StudentPeriodAttendance>>();
                }

                List<StudentPeriodAttendance> periodAttendances = periodAttendanceByDate.get(date);
                if (periodAttendances == null) {
                    periodAttendances = new ArrayList<StudentPeriodAttendance>(4);
                }

                periodAttendances.add(periodAttendance);
                periodAttendanceByDate.put(date, periodAttendances);
                m_periodAttendanceByStudent.put(student, periodAttendanceByDate);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a map of Student Schedule lists keyed to the Student OID.
     */
    private void loadStudentSchedulesByStudentOid() {
        m_studentSchedulesByStudentOid = new HashMap<String, List<StudentSchedule>>(1024);

        Criteria criteria = new Criteria();

        if (isSchoolContext() && m_activeSchedulesBySchoolOid.get(getSchool().getOid()) != null) {
            criteria.addEqualTo(StudentSchedule.COL_SCHEDULE_OID,
                    m_activeSchedulesBySchoolOid.get(getSchool().getOid()).getOid());
        } else {
            List<String> scheduleOids = new LinkedList<String>();
            for (String schoolOid : m_activeSchedulesBySchoolOid.keySet()) {
                scheduleOids.add(m_activeSchedulesBySchoolOid.get(schoolOid).getOid());
            }

            if (scheduleOids.size() > 0) {
                criteria.addIn(StudentSchedule.COL_SCHEDULE_OID, scheduleOids);
            }
        }

        if (!criteria.isEmpty()) {
            m_studentSchedulesByStudentOid =
                    getBroker().getGroupedCollectionByQuery(new QueryByCriteria(StudentSchedule.class, criteria),
                            StudentSchedule.COL_STUDENT_OID,
                            1024);
        }
    }
}
