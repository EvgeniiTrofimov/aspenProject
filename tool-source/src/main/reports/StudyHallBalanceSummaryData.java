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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sched.utils.ScheduleMap;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleStructureManager;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.sql.Time;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Study Hall Balance Summary" report. This report shows student totals
 * that
 * have a hole in their schedule for each time slot broken down by grade.
 *
 * @author X2 Development Corporation
 */
public class StudyHallBalanceSummaryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "break by day" report parameter. This value is a Boolean.
     */
    public static final String DAY_BREAK_PARAM = "dayBreak";

    /**
     * Name for the "break by term" report parameter. This value is a Boolean.
     */
    public static final String TERM_BREAK_PARAM = "termBreak";

    private static final String FIELD_SCHEDULE = "schedule";
    private static final String FIELD_STUDENT_COUNT_YOG1 = "count_yog1";
    private static final String FIELD_STUDENT_COUNT_YOG2 = "count_yog2";
    private static final String FIELD_STUDENT_COUNT_YOG3 = "count_yog3";
    private static final String FIELD_STUDENT_COUNT_YOG4 = "count_yog4";
    private static final String FIELD_STUDENT_COUNT_YOG_OTHER = "count_yog_other";
    private static final String FIELS_STUDENT_COUNT_TOTAL = "count_total";
    private static final String FIELD_TIME_SLOT = "time_slot";
    private static final String FIELD_YOG1 = "yog1";
    private static final String FIELD_YOG2 = "yog2";
    private static final String FIELD_YOG3 = "yog3";
    private static final String FIELD_YOG4 = "yog4";
    private static final String FIELD_YOG_OTHER = "yog_other";

    private final static String SCHEDULE_DISPLAY_CONNECT_SYMBOL = "-";
    private final static String SCHEDULE_DISPLAY_DAY_PERIOD_END_DELIMITER = ")";
    private final static String SCHEDULE_DISPLAY_DAY_PERIOD_START_DELIMITER = "(";
    private final static String SCHEDULE_DISPLAY_TERM_END_DELIMITER = "] ";
    private final static String SCHEDULE_DISPLAY_TERM_START_DELIMITER = " [";
    private final static String SCHEDULE_TERM_DAY_PERIOD_DELIMITER = "|";

    private boolean m_breakDay;
    private boolean m_breakTerm;
    private Map m_sectionScheduledSlots;
    private ScheduleReportHelper m_reportHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Retrieive the list of different yogs.
         */
        Criteria studentCriteria = new Criteria();
        studentCriteria.addEqualTo(SisStudent.COL_NEXT_SCHOOL_OID, m_reportHelper.getSchedule().getSchoolOid());

        SubQuery yogQuery = new SubQuery(SisStudent.class, SisStudent.COL_YOG, studentCriteria);
        yogQuery.addOrderByAscending(SisStudent.COL_YOG);
        yogQuery.setDistinct(true);
        Collection yogs = getBroker().getSubQueryCollectionByQuery(yogQuery);

        Map scheduleHolesSummary = getScheduleHolesStudentSummary();

        ReportDataGrid data = new ReportDataGrid(scheduleHolesSummary.size(), 10);
        Iterator scheduleHoleIterator = scheduleHolesSummary.keySet().iterator();
        while (scheduleHoleIterator.hasNext()) {
            String timeslot = (String) scheduleHoleIterator.next();
            HashMap studentCountByGrade = (HashMap) scheduleHolesSummary.get(timeslot);

            data.append();
            data.set(FIELD_TIME_SLOT, timeslot);
            data.set(FIELD_SCHEDULE, m_reportHelper.getSchedule());

            int totalCount = 0;
            int otherCount = 0;
            int yogIndex = 0;
            Iterator yogIterator = yogs.iterator();
            while (yogIterator.hasNext()) {
                String yog = String.valueOf(((BigDecimal) yogIterator.next()).intValue());
                Integer countForYog = (Integer) studentCountByGrade.get(Integer.valueOf(yog));

                if (countForYog != null) {
                    totalCount += countForYog.intValue();
                }

                yogIndex++;
                switch (yogIndex) {
                    case 1:
                        data.set(FIELD_STUDENT_COUNT_YOG1, countForYog);
                        data.set(FIELD_YOG1, yog);
                        break;

                    case 2:
                        data.set(FIELD_STUDENT_COUNT_YOG2, countForYog);
                        data.set(FIELD_YOG2, yog);
                        break;

                    case 3:
                        data.set(FIELD_STUDENT_COUNT_YOG3, countForYog);
                        data.set(FIELD_YOG3, yog);
                        break;

                    case 4:
                        data.set(FIELD_STUDENT_COUNT_YOG4, countForYog);
                        data.set(FIELD_YOG4, yog);
                        break;

                    default:
                        data.set(FIELD_YOG_OTHER, "");
                        if (countForYog != null) {
                            otherCount += countForYog.intValue();
                        }
                        break;
                }
            }

            data.set(FIELD_STUDENT_COUNT_YOG_OTHER, Integer.valueOf(otherCount));
            data.set(FIELS_STUDENT_COUNT_TOTAL, Integer.valueOf(totalCount));
        }

        data.beforeTop();
        return data;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_reportHelper = new ScheduleReportHelper(userData);

        m_breakDay = ((Boolean) getParameter(DAY_BREAK_PARAM)).booleanValue();
        m_breakTerm = ((Boolean) getParameter(TERM_BREAK_PARAM)).booleanValue();
        m_sectionScheduledSlots = new HashMap(5000);
    }

    /**
     * Builds the schedule display that covers all the days for the passed schedule and period.
     *
     * @param period SchedulePeriod
     * @return String
     */
    private String buildAllDayScheduleDisplayForPeriod(SchedulePeriod period) {
        /*
         * Retrieve the first day and last day of the schedule.
         */
        ScheduleDay firstDay = null;
        ScheduleDay lastDay = null;
        Iterator dayIterator = m_reportHelper.getSchedule().getScheduleDays().iterator();
        while (dayIterator.hasNext()) {
            ScheduleDay day = (ScheduleDay) dayIterator.next();

            if (day.getNumber() == 1) {
                firstDay = day;
            }

            if (day.getNumber() == m_reportHelper.getSchedule().getDays()) {
                lastDay = day;
            }

            if (firstDay != null && lastDay != null) {
                break;
            }
        }

        return m_reportHelper.getSchedule().scheduleExpressionPeriodFirst() ? period.getId() +
                SCHEDULE_DISPLAY_DAY_PERIOD_START_DELIMITER +
                firstDay.getId() +
                SCHEDULE_DISPLAY_CONNECT_SYMBOL +
                lastDay.getId() +
                SCHEDULE_DISPLAY_DAY_PERIOD_END_DELIMITER
                : firstDay.getId() +
                        SCHEDULE_DISPLAY_CONNECT_SYMBOL +
                        lastDay.getId() +
                        SCHEDULE_DISPLAY_DAY_PERIOD_START_DELIMITER +
                        period.getId() +
                        SCHEDULE_DISPLAY_DAY_PERIOD_END_DELIMITER;

    }

    /**
     * Builds the schedule expression based on the given parameters.
     *
     * @param period SchedulePeriod
     * @param day ScheduleDay
     * @param term ScheduleTerm
     * @return String
     */
    private String buildSchedule(SchedulePeriod period, ScheduleDay day, ScheduleTerm term) {
        String display = "";

        if (term != null) {
            display += SCHEDULE_DISPLAY_TERM_START_DELIMITER + term.getCode() +
                    SCHEDULE_DISPLAY_TERM_END_DELIMITER;
        }

        if (day != null) {
            if (m_reportHelper.getSchedule().scheduleExpressionPeriodFirst()) {
                display += period.getId() + SCHEDULE_DISPLAY_DAY_PERIOD_START_DELIMITER;
                display += day.getId() + SCHEDULE_DISPLAY_DAY_PERIOD_END_DELIMITER;
            } else {
                display += day.getId() + SCHEDULE_DISPLAY_DAY_PERIOD_START_DELIMITER;
                display += period.getId() + SCHEDULE_DISPLAY_DAY_PERIOD_END_DELIMITER;
            }
        } else {
            display += buildAllDayScheduleDisplayForPeriod(period);
        }

        return display;
    }

    /**
     * Converts a ScheduleMap object to a list of scheduled time slots.
     *
     * @param scheduleMap ScheduleMap
     * @param includeOverlappingTimeSlots boolean
     * @param bellScheduleOid String
     * @return A list of String objects in the format of termNumber|dayNumber|periodNumber
     */
    private Collection convertScheduleMapToScheduledSlots(ScheduleMap scheduleMap,
                                                          boolean includeOverlappingTimeSlots,
                                                          String bellScheduleOid) {
        HashSet scheduledSlots = new HashSet();

        if (m_reportHelper.getSchedule() != null && scheduleMap != null) {
            int multipleFactor = m_reportHelper.getSchedule().getTerms() / scheduleMap.getBaseTerms();

            HashMap periodNumberOidMap = new HashMap();
            HashMap periodOidNumberMap = new HashMap();
            HashMap overlappingPeriodOidMap = new HashMap();
            if (includeOverlappingTimeSlots) {
                preparePeriodNumberOidMap(periodNumberOidMap, periodOidNumberMap);
                overlappingPeriodOidMap = getOverlappingPeriodOidsMap(bellScheduleOid);
            }

            for (int term = 1; term <= m_reportHelper.getSchedule().getTerms(); term++) {
                for (int day = 1; day <= m_reportHelper.getSchedule().getDays(); day++) {
                    for (int period = 1; period <= m_reportHelper.getSchedule().getPeriods(); period++) {
                        if (scheduleMap.isScheduled((term - 1) / multipleFactor + 1, day, period)) {
                            String schedSlot = String.valueOf(term) + SCHEDULE_TERM_DAY_PERIOD_DELIMITER +
                                    String.valueOf(day) + SCHEDULE_TERM_DAY_PERIOD_DELIMITER +
                                    String.valueOf(period);
                            scheduledSlots.add(schedSlot);

                            if (includeOverlappingTimeSlots && !StringUtils.isEmpty(bellScheduleOid)) {
                                /*
                                 * If there are overlapping periods with the current period number,
                                 * adds
                                 * them to the time slots list.
                                 */
                                String periodOid = (String) periodNumberOidMap.get(Integer.valueOf(period));
                                Collection overlappingPeriodOids = (Collection) overlappingPeriodOidMap.get(periodOid);

                                if (overlappingPeriodOids != null) {
                                    Iterator periodOidIterator = overlappingPeriodOids.iterator();
                                    while (periodOidIterator.hasNext()) {
                                        String oid = (String) periodOidIterator.next();
                                        Integer periodNumber = (Integer) periodOidNumberMap.get(oid);

                                        if (periodNumber != null) {
                                            String timeSlot =
                                                    String.valueOf(term) + SCHEDULE_TERM_DAY_PERIOD_DELIMITER +
                                                            String.valueOf(day) + SCHEDULE_TERM_DAY_PERIOD_DELIMITER +
                                                            periodNumber.toString();

                                            scheduledSlots.add(timeSlot);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return scheduledSlots;
    }

    /**
     * Returns the list of time slots to cover for study purpose.
     *
     * @param periodToApply If not null, the time slots only apply for the period.
     * @param dayToApply If not null, the time slots only apply for the day.
     * @param termToApply If not null, the time slots only apply for the term.
     *
     * @return Colleciton of String objects
     */
    private Collection getAllTimeSlotsToCoverForStudy(SchedulePeriod periodToApply,
                                                      ScheduleDay dayToApply,
                                                      ScheduleTerm termToApply) {
        HashSet allTimeSlots = new HashSet();

        if (m_reportHelper.getSchedule() != null) {
            for (int term = 1; term <= m_reportHelper.getSchedule().getTerms(); term++) {
                if (termToApply == null || termToApply.getUniversalTermMap().charAt(term - 1) == '1') {
                    Iterator dayIterator = m_reportHelper.getSchedule().getScheduleDays(getBroker()).iterator();
                    while (dayIterator.hasNext()) {
                        ScheduleDay day = (ScheduleDay) dayIterator.next();
                        if (dayToApply == null || dayToApply.getNumber() == day.getNumber()) {
                            Iterator periodIterator =
                                    m_reportHelper.getSchedule().getSchedulePeriods(getBroker()).iterator();
                            while (periodIterator.hasNext()) {
                                SchedulePeriod period = (SchedulePeriod) periodIterator.next();
                                if (period.getStudyIndicator() &&
                                        (periodToApply == null || periodToApply.getNumber() == period.getNumber())) {
                                    String timeSlot = String.valueOf(term) + "|" +
                                            String.valueOf(day.getNumber()) + "|" +
                                            String.valueOf(period.getNumber());

                                    if (!allTimeSlots.contains(timeSlot)) {
                                        allTimeSlots.add(timeSlot);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return allTimeSlots;
    }

    /**
     * Retrieve the bell schedule oid for the course of the passed section.
     *
     * @param section X2BaseBean
     * @return String
     */
    private String getCourseBellScheduleOid(X2BaseBean section) {
        String bellScheduleOid = null;

        if (section != null) {
            String courseOid = null;
            Schedule schedule = null;
            if (section.getClass().equals(MasterSchedule.class)) {
                courseOid = ((MasterSchedule) section).getSchoolCourseOid();
                schedule = ((MasterSchedule) section).getSchedule();
            } else if (section.getClass().equals(BuildMasterSchedule.class)) {
                courseOid = ((BuildMasterSchedule) section).getSchoolCourseOid();
                schedule = ((BuildMasterSchedule) section).getSchedule();
            }

            if (!StringUtils.isEmpty(courseOid) &&
                    schedule != null &&
                    schedule.useBellSchedule()) {
                ScheduleStructureManager structureManager = new ScheduleStructureManager(getBroker());
                ScheduleBell bell = structureManager.getCourseBellSchedule(courseOid, schedule.getOid());
                bellScheduleOid = bell != null ? bell.getOid() : null;
                /*
                 * If the schedule uses bell schedule and there is only one bell schedule,
                 * default it to the course's bell schedule.
                 */
                if (StringUtils.isEmpty(bellScheduleOid) && schedule.getScheduleBells().size() == 1) {
                    Iterator bellScheduleIterator = schedule.getScheduleBells().iterator();
                    bellScheduleOid = ((ScheduleBell) bellScheduleIterator.next()).getOid();
                }
            }
        }

        return bellScheduleOid;
    }

    /**
     * Returns the list of overlapping period oids for the passed period oid and bell schedule oid.
     *
     * @param periodOid String
     * @param bellScheduleOid String
     * @return Collection It includes the passed periodOid.
     */
    private Collection getOverlappingPeriodOids(String periodOid, String bellScheduleOid) {
        Collection overlappingPeriodOids = new HashSet();

        if (!StringUtils.isEmpty(bellScheduleOid) && !StringUtils.isEmpty(periodOid)) {
            X2Criteria bellPeriodCriteria = new X2Criteria();
            bellPeriodCriteria.addEqualTo(ScheduleBellPeriod.COL_BELL_SCHEDULE_OID, bellScheduleOid);
            bellPeriodCriteria.addNotEmpty(ScheduleBellPeriod.COL_SCHEDULE_PERIOD_OID, getBroker().getPersistenceKey());

            QueryByCriteria bellPeriodQuery = new QueryByCriteria(ScheduleBellPeriod.class, bellPeriodCriteria);
            Collection bellPeriods = getBroker().getCollectionByQuery(bellPeriodQuery);

            if (!bellPeriods.isEmpty()) {
                /*
                 * First identify the start time and end time to be compared to.
                 */
                Time startTime = null;
                Time endTime = null;
                Iterator bellPeriodIterator = bellPeriods.iterator();
                while (bellPeriodIterator.hasNext()) {
                    ScheduleBellPeriod bellPeriod = (ScheduleBellPeriod) bellPeriodIterator.next();
                    if (bellPeriod.getSchedulePeriodOid().equals(periodOid)) {
                        startTime = bellPeriod.getStartTime();
                        endTime = bellPeriod.getEndTime();
                        break;
                    }
                }

                /*
                 * Identify the overlapping period oids.
                 */
                if (startTime != null && endTime != null) {
                    bellPeriodIterator = bellPeriods.iterator();
                    while (bellPeriodIterator.hasNext()) {
                        ScheduleBellPeriod bellPeriod = (ScheduleBellPeriod) bellPeriodIterator.next();
                        if (!bellPeriod.getSchedulePeriodOid().equals(periodOid)) {
                            Time bellPeriodStartTime = bellPeriod.getStartTime();
                            Time bellPeriodEndTime = bellPeriod.getEndTime();

                            boolean isOverlapping = bellPeriodStartTime.equals(startTime) ||
                                    bellPeriodEndTime.equals(endTime) ||
                                    (bellPeriodStartTime.before(endTime) && bellPeriodStartTime.after(startTime)) ||
                                    (startTime.before(bellPeriodEndTime) && startTime.after(bellPeriodStartTime));

                            if (isOverlapping) {
                                overlappingPeriodOids.add(periodOid);
                                overlappingPeriodOids.add(bellPeriod.getSchedulePeriodOid());
                            }
                        }
                    }
                }
            }
        }

        return overlappingPeriodOids;
    }

    /**
     * Returns the overlapping period oids map.
     *
     * @param bellScheduleOid String
     * @return HashMap Key is a SchedulePeriod oid string
     *         value is the list overlapping periods' oids
     */
    private HashMap getOverlappingPeriodOidsMap(String bellScheduleOid) {
        HashMap overlappingPeriodOidMap = new HashMap();

        Iterator periodIterator = m_reportHelper.getSchedule().getSchedulePeriods().iterator();
        while (periodIterator.hasNext()) {
            SchedulePeriod period = (SchedulePeriod) periodIterator.next();
            overlappingPeriodOidMap.put(period.getOid(), getOverlappingPeriodOids(period.getOid(), bellScheduleOid));
        }

        return overlappingPeriodOidMap;
    }

    /**
     * Returns the student schedule holes summary.
     *
     * @return Map Key is a period time slot, value is the number of students
     *         that have holes in the time slot.
     */
    private Map getScheduleHolesStudentSummary() {
        HashMap holesSummaryMap = new LinkedHashMap();

        if (m_reportHelper.getSchedule() != null) {
            /*
             * Retrieve the schedule for each student and find out the holes in their schedule.
             */
            Criteria studentScheduleCriteria = new Criteria();
            studentScheduleCriteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
            QueryByCriteria studentScheduleQuery =
                    new QueryByCriteria(m_reportHelper.getStudentSectionClass(), studentScheduleCriteria);

            HashMap studentSchedTimeSlotMap = new HashMap();
            QueryIterator studentScheduleIterator = getBroker().getIteratorByQuery(studentScheduleQuery);
            try {
                while (studentScheduleIterator.hasNext()) {
                    SisStudent student = null;
                    Collection schedTimeSlotsForStudent = new ArrayList();

                    StudentSection studentSchedule = (StudentSection) studentScheduleIterator.next();
                    student = studentSchedule.getStudent();

                    if (m_sectionScheduledSlots.get(studentSchedule.getSectionOid()) == null) {
                        schedTimeSlotsForStudent =
                                getScheduledSlotsForSection((X2BaseBean) studentSchedule.getSection(), true);
                    } else {
                        schedTimeSlotsForStudent =
                                (Collection) m_sectionScheduledSlots.get(studentSchedule.getSectionOid());
                    }

                    Collection allSlots = (Collection) studentSchedTimeSlotMap.get(student);
                    if (allSlots == null) {
                        allSlots = new ArrayList();
                    }
                    allSlots.addAll(schedTimeSlotsForStudent);
                    studentSchedTimeSlotMap.put(student, allSlots);
                }
            } finally {
                studentScheduleIterator.close();
            }

            /*
             * Go through each period that joins study hall, find out the number of students who
             * have holes in that period.
             */
            Iterator periodIterator = m_reportHelper.getSchedule().getSchedulePeriods().iterator();
            while (periodIterator.hasNext()) {
                SchedulePeriod period = (SchedulePeriod) periodIterator.next();
                if (period.getStudyIndicator()) {
                    String timeSlotDisplay = "";
                    if (m_breakDay) {
                        Iterator dayIterator = m_reportHelper.getSchedule().getScheduleDays().iterator();
                        while (dayIterator.hasNext()) {
                            ScheduleDay day = (ScheduleDay) dayIterator.next();

                            if (m_breakTerm) {
                                Iterator termIterator = m_reportHelper.getSchedule().getScheduleTerms().iterator();
                                while (termIterator.hasNext()) {
                                    ScheduleTerm term = (ScheduleTerm) termIterator.next();

                                    timeSlotDisplay = buildSchedule(period, day, term);
                                    Collection timeSlotsForPeriod =
                                            getAllTimeSlotsToCoverForStudy(period, day, term);

                                    holesSummaryMap.put(timeSlotDisplay,
                                            getStudentHoles(studentSchedTimeSlotMap, timeSlotsForPeriod));
                                }
                            } else {
                                timeSlotDisplay = buildSchedule(period, day, null);
                                Collection timeSlotsForPeriod =
                                        getAllTimeSlotsToCoverForStudy(period, day, null);

                                holesSummaryMap.put(timeSlotDisplay,
                                        getStudentHoles(studentSchedTimeSlotMap, timeSlotsForPeriod));
                            }
                        }
                    } else {
                        if (m_breakTerm) {
                            Iterator termIterator = m_reportHelper.getSchedule().getScheduleTerms().iterator();
                            while (termIterator.hasNext()) {
                                ScheduleTerm term = (ScheduleTerm) termIterator.next();

                                timeSlotDisplay = buildSchedule(period, null, term);
                                Collection timeSlotsForPeriod =
                                        getAllTimeSlotsToCoverForStudy(period, null, term);

                                holesSummaryMap.put(timeSlotDisplay,
                                        getStudentHoles(studentSchedTimeSlotMap, timeSlotsForPeriod));
                            }
                        } else {
                            timeSlotDisplay = buildSchedule(period, null, null);
                            Collection timeSlotsForPeriod =
                                    getAllTimeSlotsToCoverForStudy(period, null, null);

                            holesSummaryMap.put(timeSlotDisplay,
                                    getStudentHoles(studentSchedTimeSlotMap, timeSlotsForPeriod));
                        }
                    }
                }
            }
        }

        return holesSummaryMap;
    }

    /**
     * Returns the list of scheduled time slots covered by the passed section.
     * This method works on BuildMasterSchedule objects.
     *
     * @param buildMaster BuildMasterSchedule
     * @param includeOverlappingTimeSlots true means count the overlapping time slots the section
     *        cover
     * @return Collection
     */
    private Collection getScheduledSlotsForBuildMaster(BuildMasterSchedule buildMaster,
                                                       boolean includeOverlappingTimeSlots) {
        Collection scheduledSlots = new HashSet();

        if (buildMaster != null && !StringUtils.isEmpty(buildMaster.getTermMap())
                && !StringUtils.isEmpty(buildMaster.getScheduleMatrix())) {
            String courseBellScheduleOid = getCourseBellScheduleOid(buildMaster);

            ScheduleMap scheduleMap = new ScheduleMap(buildMaster.getTermMap(), buildMaster.getScheduleMatrix());

            scheduledSlots = convertScheduleMapToScheduledSlots(scheduleMap,
                    includeOverlappingTimeSlots,
                    courseBellScheduleOid);
        }

        return scheduledSlots;
    }

    /**
     * Returns the list of scheduled time slots covered by the passed section.
     * This method works on MasterSchedule objects.
     *
     * @param master MasterSchedule
     * @param includeOverlappingSlots true means count the overlapping time slots the section cover.
     * @return Collection
     */
    private Collection getScheduledSlotsForMaster(MasterSchedule master, boolean includeOverlappingSlots) {
        HashSet scheduledSlots = new HashSet();

        if (master != null) {
            String courseBellScheduleOid = getCourseBellScheduleOid(master);
            HashMap overlappingPeriodOidMap = getOverlappingPeriodOidsMap(courseBellScheduleOid);

            Iterator itTerm = master.getMasterTerms(getBroker()).iterator();
            while (itTerm.hasNext()) {
                MasterTerm masterTerm = (MasterTerm) itTerm.next();
                ScheduleTerm term = masterTerm.getScheduleTerm();

                Iterator termIt = term.countUsedTermNumbers().iterator();
                while (termIt.hasNext()) {
                    int termNumber = ((Integer) termIt.next()).intValue();

                    Iterator matrixIt = masterTerm.getMasterMatrices(getBroker()).iterator();
                    while (matrixIt.hasNext()) {
                        MasterScheduleMatrix mstMatrix = (MasterScheduleMatrix) matrixIt.next();
                        String periodOid = mstMatrix.getScheduleMatrix().getSchedulePeriodOid();
                        String scheduledSlot = String.valueOf(termNumber) + SCHEDULE_TERM_DAY_PERIOD_DELIMITER +
                                mstMatrix.getScheduleMatrix().getScheduleDay().getNumber()
                                + SCHEDULE_TERM_DAY_PERIOD_DELIMITER +
                                mstMatrix.getScheduleMatrix().getSchedulePeriod().getNumber();
                        scheduledSlots.add(scheduledSlot);

                        if (includeOverlappingSlots && !StringUtils.isEmpty(courseBellScheduleOid)) {
                            /*
                             * If there are overlapping periods with the current scheduled period,
                             * add all
                             * overlapping periods as scheduled time slots because no study hall is
                             * allowed.
                             */
                            Collection overLappingPeriods = (Collection) overlappingPeriodOidMap.get(periodOid);
                            if (overLappingPeriods != null && !overLappingPeriods.isEmpty()) {
                                Iterator periodIterator = overLappingPeriods.iterator();
                                while (periodIterator.hasNext()) {
                                    String oid = (String) periodIterator.next();
                                    if (!oid.equals(periodOid)) {
                                        SchedulePeriod period =
                                                (SchedulePeriod) getBroker().getBeanByOid(SchedulePeriod.class, oid);
                                        if (period != null) {
                                            String timeSlot =
                                                    String.valueOf(termNumber) + SCHEDULE_TERM_DAY_PERIOD_DELIMITER +
                                                            mstMatrix.getScheduleMatrix().getScheduleDay().getNumber()
                                                            + SCHEDULE_TERM_DAY_PERIOD_DELIMITER +
                                                            period.getNumber();

                                            scheduledSlots.add(timeSlot);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return scheduledSlots;
    }

    /**
     * Returns the list of scheduled time slots covered by the passed section.
     * This method works for either MasterSchedule or BuildMasterSchedule objects.
     *
     * @param section X2BaseBean
     * @param includeOverlappingSlots boolean
     * @return Collection
     */
    private Collection getScheduledSlotsForSection(X2BaseBean section, boolean includeOverlappingSlots) {
        Collection scheduledSlots;

        if (section.getClass().equals(MasterSchedule.class)) {
            scheduledSlots = getScheduledSlotsForMaster((MasterSchedule) section, includeOverlappingSlots);
        } else {
            scheduledSlots = getScheduledSlotsForBuildMaster((BuildMasterSchedule) section, includeOverlappingSlots);
        }

        return scheduledSlots;
    }

    /**
     * Returns a HashMap of the number of students in each grade, keyed to the Grade Level, that
     * have the passed time slots open or partially open.
     *
     * @param studentSchedTimeSlotMap HashMap
     * @param timeSlotsForPeriod Collection
     * @return HashMap
     */
    private HashMap getStudentHoles(HashMap studentSchedTimeSlotMap, Collection timeSlotsForPeriod) {
        HashMap studentsHasHolesByGrade = new HashMap();

        Iterator studentIterator = studentSchedTimeSlotMap.keySet().iterator();
        while (studentIterator.hasNext()) {
            SisStudent student = (SisStudent) studentIterator.next();

            if (student != null &&
                    !student.getStudyExcludeIndicator(m_reportHelper.getSchedule().getOid())) {
                Collection schedTimeSlots =
                        (Collection) studentSchedTimeSlotMap.get(student);
                if (!schedTimeSlots.containsAll(timeSlotsForPeriod)) {
                    Integer studentCountsForYog =
                            (Integer) studentsHasHolesByGrade.get(Integer.valueOf(student.getYog()));
                    int newCount =
                            studentCountsForYog == null ? 1 : studentCountsForYog.intValue() + 1;

                    studentsHasHolesByGrade.put(Integer.valueOf(student.getYog()), Integer.valueOf(newCount));
                }
            }
        }

        return studentsHasHolesByGrade;
    }

    /**
     * Prepares two HashMap object for periods.
     *
     * @param periodNumberOidMap Key on period number, value is the related perioid's oid
     * @param periodOidNumberMap key on period oid, value is the related perid's number
     */
    private void preparePeriodNumberOidMap(HashMap periodNumberOidMap, HashMap periodOidNumberMap) {
        if (periodNumberOidMap == null) {
            periodNumberOidMap = new HashMap();
        }

        if (periodOidNumberMap == null) {
            periodOidNumberMap = new HashMap();
        }

        Iterator periodIterator = m_reportHelper.getSchedule().getSchedulePeriods().iterator();
        while (periodIterator.hasNext()) {
            SchedulePeriod period = (SchedulePeriod) periodIterator.next();
            periodNumberOidMap.put(Integer.valueOf(period.getNumber()), period.getOid());
            periodOidNumberMap.put(period.getOid(), Integer.valueOf(period.getNumber()));
        }
    }
}
