/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sched.utils.ScheduleMap;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.TeacherSection;
import com.x2dev.sis.model.business.schedule.ScheduleStructureManager;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Unscheduled Teacher" report. This report shows, by period, a list of
 * teachers who are free and when they are free for that period.
 *
 * @author X2 Development Corporation
 */
public class UnscheduledTeacherData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "selection" report parameter. The value is an String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "sort" report parameter. The value is an String.
     */
    public static final String SORT_PARAM = "sort";

    // Grid fields
    private static final String FIELD_DISPLAY = "display";
    private static final String FIELD_MASTER_SET = "masterSet";
    private static final String FIELD_PERIOD = "period";
    private static final String FIELD_STAFF = "staff";

    private ScheduleReportHelper m_reportHelper;
    private Map<String, Collection<String>> m_scheduledTimesForSection;
    private Map<String, ScheduleMap> m_sectionToScheduleMap;
    private ScheduleStructureManager m_stuctureManager;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.reporting.ReportDataSource#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_scheduledTimesForSection = new HashMap<String, Collection<String>>(5000);
        m_stuctureManager = new ScheduleStructureManager(getBroker());
        m_sectionToScheduleMap = new HashMap<String, ScheduleMap>();

        ReportDataGrid staffGrid = getStaffGrid();
        ReportDataGrid resultsGrid = new ReportDataGrid(4);

        if (m_reportHelper.getSchedule() != null) {
            // Iterate over each period looking for staff that do not have a class that period.
            Iterator periods = m_reportHelper.getSchedule().getSchedulePeriods().iterator();
            while (periods.hasNext()) {
                SchedulePeriod period = (SchedulePeriod) periods.next();
                Collection timeSlots =
                        m_stuctureManager.getTimeSlots(m_reportHelper.getSchedule(), period.getNumber(), false);

                while (staffGrid.next()) {
                    List<X2BaseBean> masterTeacherSet = (List<X2BaseBean>) staffGrid.get(FIELD_MASTER_SET);
                    SisStaff staff = (SisStaff) staffGrid.get(FIELD_STAFF);

                    if (masterTeacherSet != null) {
                        String unscheduledTimes = getUnscheduledForPeriod(period, masterTeacherSet, timeSlots);
                        if (!StringUtils.isEmpty(unscheduledTimes)) {
                            resultsGrid.append();
                            resultsGrid.set(FIELD_PERIOD, String.valueOf(period.getNumber()));
                            resultsGrid.set(FIELD_STAFF, staff);
                            resultsGrid.set(FIELD_DISPLAY, unscheduledTimes);
                        }
                    }
                }

                staffGrid.beforeTop();
            }

            resultsGrid.beforeTop();
        }

        return resultsGrid;
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
    }

    /**
     * Builds a grid of Staff records and sets of Master sections as well as builds a Map of a
     * section's scheduled times keyed to the section OID.
     *
     * @return ReportDataGrid
     */
    private ReportDataGrid getStaffGrid() {
        X2Criteria masterCriteria = new X2Criteria();
        masterCriteria.addEqualTo(TeacherSection.REL_SECTION + "." +
                Section.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        addUserCriteria(masterCriteria, queryBy, queryString, null, null);

        QueryByCriteria query = new QueryByCriteria(m_reportHelper.getTeacherSectionClass(), masterCriteria);

        String sortBy = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sortBy);

        query.addOrderByAscending(TeacherSection.COL_STAFF_OID);

        QueryIterator teacherSchedules = getBroker().getIteratorByQuery(query);
        ReportDataGrid grid = new ReportDataGrid(4);

        try {
            SisStaff lastStaff = null;

            while (teacherSchedules.hasNext()) {
                TeacherSection teacherSchedule = (TeacherSection) teacherSchedules.next();
                Section section = teacherSchedule.getSection();
                String sectionOid = teacherSchedule.getSectionOid();
                SisStaff staff = teacherSchedule.getStaff();

                if (section != null) {
                    if (lastStaff == null || !lastStaff.equals(staff)) {
                        grid.append();
                        grid.set(FIELD_STAFF, staff);
                        grid.set(FIELD_MASTER_SET, new LinkedList());
                    }
                    lastStaff = staff;

                    LinkedList masterSet = (LinkedList) grid.get(FIELD_MASTER_SET);
                    masterSet.add(section);

                    if (!m_scheduledTimesForSection.containsKey(sectionOid)) {
                        /*
                         * Get the schedule map for the teacher and if not empty use that to
                         * determine scheduled time slots.
                         */
                        ScheduleMap map = teacherSchedule.getScheduleMap(getBroker());
                        if (map != null && !map.isEmpty()) {
                            Collection scheduledSlots =
                                    m_stuctureManager.convertScheduleMapToScheduledSlots(m_reportHelper.getSchedule(),
                                            map,
                                            false,
                                            null);
                            m_scheduledTimesForSection.put(sectionOid, scheduledSlots);
                        } else {
                            m_scheduledTimesForSection.put(sectionOid, new ArrayList());
                        }
                        m_sectionToScheduleMap.put(sectionOid, map);
                    }
                }
            }
        } finally {
            teacherSchedules.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Checks if list of sections is available for given period for any day or term. If so, returns
     * the schedule display of the unscheduled time.
     *
     * @param period SchedulePeriod
     * @param sections List<X2BaseBean>
     * @param timeSlots Collection
     * @return String
     */
    private String getUnscheduledForPeriod(SchedulePeriod period, List<X2BaseBean> sections, Collection timeSlots) {
        String unscheduledDisplay = "";
        Collection<String> scheduledTimes = new LinkedList<String>();

        for (X2BaseBean section : sections) {
            ScheduleMap scheduleMap;
            if (section.getClass().equals(ScheduleTeacher.class)) {
                scheduleMap = m_sectionToScheduleMap.get(section.getOid());
            } else {
                scheduleMap = m_sectionToScheduleMap.get(section.getOid());
            }

            if (scheduleMap != null && scheduleMap.isScheduledForPeriod(period.getNumber())) {
                scheduledTimes.addAll(m_scheduledTimesForSection.get(section.getOid()));
            }
        }

        /*
         * Look through scheduledTimes & extract unscheduled times for period
         */
        Collection unscheduledTimes = new ArrayList(timeSlots);
        if (unscheduledTimes.removeAll(scheduledTimes)) {
            Map<String, Collection<String>> termTimeSlots = new HashMap<String, Collection<String>>();

            /*
             * Split the unscheduled times up by term.
             */
            for (String unscheduledTime : (Collection<String>) unscheduledTimes) {
                String term = unscheduledTime.trim().split("\\|")[1];
                Collection<String> times = termTimeSlots.get(term);
                if (times == null) {
                    times = new ArrayList<String>();
                }
                times.add(unscheduledTime);
                termTimeSlots.put(term, times);
            }

            Map<String, List<String>> expressionToTerm = new HashMap<String, List<String>>();

            /*
             * Convert the timeslots for each term and put them into the expression
             * to terms map.
             */
            for (String term : termTimeSlots.keySet()) {
                /*
                 * Example value: " [4] 2(1-3)"
                 * Note: There is a prefix space when showTermInDisplay is set to true
                 */
                String tempDisplay = m_stuctureManager.convertTimeSlotsToScheduleDisplay(m_reportHelper.getSchedule(),
                        termTimeSlots.get(term), true, false);

                if (!StringUtils.isEmpty(tempDisplay)) {
                    // Split into [empty string, term, day/period]
                    String[] data = tempDisplay.split(" ");
                    if (data.length == 3) {
                        List<String> terms = expressionToTerm.get(data[2]);
                        if (terms == null) {
                            terms = new ArrayList<String>();
                        }

                        terms.add(data[1]);
                        expressionToTerm.put(data[2], terms);
                    }
                }
            }

            /*
             * Iterate through the unique expressions concatenating the terms together.
             */
            for (String expression : expressionToTerm.keySet()) {
                List<String> terms = expressionToTerm.get(expression);
                if (terms.size() != m_reportHelper.getSchedule().getTerms()) {
                    String termString = "[";

                    Collections.sort(terms);

                    for (String term : terms) {
                        term = term.replace('[', '\0');
                        term = term.replace(']', '\0');
                        termString += term.trim() + " ";
                    }
                    termString = termString.trim();
                    termString += "]";
                    unscheduledDisplay = unscheduledDisplay + termString + " " + expression + ", ";
                } else {
                    unscheduledDisplay = expression;
                }
            }

            /*
             * Remove a trailing comma.
             */
            if (!StringUtils.isEmpty(unscheduledDisplay) &&
                    unscheduledDisplay.lastIndexOf(',') != -1 &&
                    unscheduledDisplay.lastIndexOf(',') == (unscheduledDisplay.length() - 2)) {
                unscheduledDisplay = unscheduledDisplay.substring(0, unscheduledDisplay.lastIndexOf(','));
            }
        } else {
            unscheduledDisplay =
                    m_stuctureManager.buildAllDayScheduleDisplayForPeriod(m_reportHelper.getSchedule(), period);
        }

        return unscheduledDisplay;
    }
}
