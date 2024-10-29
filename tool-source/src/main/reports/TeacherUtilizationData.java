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

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sched.utils.ScheduleMap;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.TeacherSection;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the "Teacher Utilization" report.
 *
 * @author X2 Development Corporation
 */
public class TeacherUtilizationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the overbooked only parameter. The value is a boolean.
     */
    public static final String PARAM_OVER_BOOKED_ONLY = "overBookedOnly";

    /**
     * Name for the "selection" report parameter. The value is an String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "QueryBy input" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    // Grid fields
    private static final String FIELD_STAFF = "staff";

    // Report parameter name constants
    private static final String PARAMETER_PERIOD_ID_LOOKUP = "periodIdLookup";
    private static final String PARAMETER_SCHOOL_YEAR_CONTEXT = "schoolYear";

    private ScheduleReportHelper m_reportHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(15);

        if (m_reportHelper.getSchedule() != null) {
            Boolean overbookedOnly = (Boolean) getParameter(PARAM_OVER_BOOKED_ONLY);
            Criteria criteria = new Criteria();
            criteria.addEqualTo(TeacherSection.REL_SECTION + "." + Section.COL_SCHEDULE_OID,
                    m_reportHelper.getScheduleOid());

            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);
            addUserCriteria(criteria, queryBy, queryString, null, null);

            QueryByCriteria query = new QueryByCriteria(m_reportHelper.getTeacherSectionClass(), criteria);

            query.addOrderByAscending(TeacherSection.REL_STAFF + "." + SisStaff.COL_DEPARTMENT_CODE);
            query.addOrderByAscending(TeacherSection.REL_STAFF + "." + SisStaff.COL_NAME_VIEW);
            query.addOrderByAscending(TeacherSection.REL_STAFF + "." + X2BaseBean.COL_OID);
            query.addOrderByAscending(TeacherSection.REL_SECTION + "." + Section.COL_COURSE_VIEW);
            query.addOrderByAscending(TeacherSection.REL_SECTION + "." + Section.COL_SCHEDULE_DISPLAY);

            QueryIterator sectionIterator = getBroker().getIteratorByQuery(query);

            try {
                StringBuilder roomDisplay = null;
                SisStaff lastStaff = null;

                LinkedList<ScheduleMap> scheduleMaps = new LinkedList<ScheduleMap>();

                while (sectionIterator.hasNext()) {
                    TeacherSection sectionTeacher = (TeacherSection) sectionIterator.next();

                    SisStaff staff = sectionTeacher.getStaff();
                    ScheduleMap scheduleMap;
                    String courseView = sectionTeacher.getSection().getCourseView();
                    String description = sectionTeacher.getSection().getDescription();
                    String roomView = sectionTeacher.getSection().getRoomView();
                    String termView = sectionTeacher.getSection().getTermView();
                    int enrollmentTotal = sectionTeacher.getSection().getEnrollmentTotal();

                    if (m_reportHelper.getSectionClass().equals(MasterSchedule.class)) {
                        scheduleMap = sectionTeacher.getScheduleMap(getBroker());
                    } else {
                        scheduleMap = sectionTeacher.getScheduleMap(getBroker());
                    }

                    if (lastStaff == null || !staff.equals(lastStaff)) {
                        if (lastStaff != null && overbookedOnly.booleanValue() && !isOverBooked(scheduleMaps)) {
                            grid.deleteRow();
                        }

                        grid.append();
                        grid.set(FIELD_STAFF, staff);

                        scheduleMaps.clear();
                    }

                    if (scheduleMap != null) {
                        String daySched = sectionTeacher.getScheduleDisplay();

                        for (int i = 1; i <= m_reportHelper.getSchedule().getPeriods(); i++) {
                            String periodNumber = String.valueOf(i);
                            if (scheduleMap.isScheduledForPeriod(i)) {
                                roomDisplay = new StringBuilder(64);
                                roomDisplay.append(courseView + "\n");
                                if (description != null && description.trim().length() > 0) {
                                    roomDisplay.append(description + "\n");
                                }
                                if (roomView != null) {
                                    roomDisplay.append("Rm: " + roomView + " ");
                                }
                                roomDisplay.append("Ttl: " + enrollmentTotal + " ");

                                if (scheduleMap.getBaseTerms() != scheduleMap.getCoveredTerms()) {
                                    roomDisplay.append("[" + termView + "]");
                                }

                                // TODO: The range of days only works if the schedule expression is
                                // period-first.
                                roomDisplay.append(daySched + "\n\n");

                                StringBuilder preExists = (StringBuilder) grid.get(periodNumber);
                                if (preExists == null) {
                                    grid.set(periodNumber, roomDisplay);
                                } else {
                                    grid.set(periodNumber, preExists.append(roomDisplay));
                                }
                            }
                        }

                        scheduleMaps.add(scheduleMap);
                    }

                    lastStaff = staff;
                }
            } finally {
                if (sectionIterator != null) {
                    sectionIterator.close();
                }
            }

            addParameter(PARAMETER_PERIOD_ID_LOOKUP, getPeriodIdMap());
            addParameter(PARAMETER_SCHOOL_YEAR_CONTEXT,
                    Integer.valueOf(m_reportHelper.getSchedule().getDistrictContext().getSchoolYear()));
        }

        grid.beforeTop();
        return grid;
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
     * Returns the period ID's into a map by the period number.
     *
     * @return A Map of String objects keyed on Integer objects
     */
    private Map<Integer, String> getPeriodIdMap() {
        HashMap<Integer, String> periodIdLookup = new HashMap<Integer, String>(15);

        Iterator<SchedulePeriod> periods = m_reportHelper.getSchedule().getSchedulePeriods(getBroker()).iterator();
        while (periods.hasNext()) {
            SchedulePeriod period = periods.next();
            periodIdLookup.put(Integer.valueOf(period.getNumber()), period.getId());
        }

        return periodIdLookup;
    }

    /**
     * Returns true if at least two among the passed list of schedule maps are in conflict.
     *
     * @param scheduleMaps LinkedList<ScheduleMap>
     * @return true, if is over booked
     */
    private boolean isOverBooked(LinkedList<ScheduleMap> scheduleMaps) {
        boolean conflictFound = false;

        int index = 0;
        Iterator<ScheduleMap> scheduleMapIterator = scheduleMaps.iterator();
        while (scheduleMapIterator.hasNext() && !conflictFound) {
            ScheduleMap scheduleMap = scheduleMapIterator.next();

            int compareIndex = 0;
            Iterator compareIterator = scheduleMaps.iterator();
            while (compareIterator.hasNext() && !conflictFound) {
                ScheduleMap mapToCompare = (ScheduleMap) compareIterator.next();

                if (compareIndex != index) {
                    conflictFound = mapToCompare.conflicts(scheduleMap, null);
                }
                compareIndex++;
            }

            index++;
        }

        return conflictFound;
    }
}
