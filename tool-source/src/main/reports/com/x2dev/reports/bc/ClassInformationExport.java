/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleDay;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class to export master schedule information for BC as part of their GDE.
 *
 * @author Follett Software Company
 */
public class ClassInformationExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;
    // Input parameters
    private static final String PARAM_INCL_IDS = "includeIds";

    // Grid fields
    private static final String FIELD_SKL_ID = "School Number";
    private static final String FIELD_COURSE_NUMBER = "Course Code";
    private static final String FIELD_STAFF_ID = "Teacher Id";
    private static final String FIELD_SECTION_NUMBER = "Section Letter";
    private static final String FIELD_SEMESTER = "Semester";
    private static final String FIELD_TERM_CODE = "Term";
    private static final String FIELD_DAY = "Day";
    private static final String FIELD_PEROID = "Period";

    // Optional field
    private static final String FIELD_ID = "Master Timetable ID";
    private static final int FIELD_COUNT = 8;
    private static final int FIELD_COUNT_WITH_IDS = 9;

    // Aliases
    private static final String ALIAS_SEMESTER = "trm-semester";

    private List<String> m_columns;
    private Map<String, List<String>> m_dayIdMap;
    private Map<String, Boolean> m_periodFirstMap;
    private Map<String, List<String>> m_periodIdMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid() {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }

        };

        boolean includeIds = ((Boolean) getParameter(PARAM_INCL_IDS)).booleanValue();

        QueryByCriteria query = new QueryByCriteria(MasterSchedule.class, buildCriteria());
        query.addOrderByAscending(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID);
        query.addOrderByAscending(MasterSchedule.COL_COURSE_VIEW);

        QueryIterator sections = getBroker().getIteratorByQuery(query);
        try {
            while (sections.hasNext()) {
                MasterSchedule section = (MasterSchedule) sections.next();
                try {
                    grid.append();

                    // Fill grid data list with export information
                    grid.set(FIELD_SKL_ID, section.getSchedule().getSchool().getSchoolId());
                    grid.set(FIELD_COURSE_NUMBER, section.getSchoolCourse().getNumber());
                    grid.set(FIELD_SECTION_NUMBER, section.getSectionNumber());

                    setDayPeriodFields(grid, section);

                    ScheduleTerm term = section.getScheduleTerm();
                    if (term != null) {
                        grid.set(FIELD_TERM_CODE, term.getCode());
                        grid.set(FIELD_SEMESTER, term.getFieldValueByAlias(ALIAS_SEMESTER));
                    }

                    SisStaff staff = section.getPrimaryStaff();
                    if (staff != null) {
                        grid.set(FIELD_STAFF_ID, staff.getLocalId());
                    }

                    if (includeIds) {
                        grid.set(FIELD_ID, section.getOid());
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(section.getOid());
                    strBldr.append("].");

                    strBldr.append("Null encountered when setting Columns.");
                    grid.deleteRow(); // Delete the incomplete row that was appended to the grid.

                    strBldr.append("\n\n\nNullPointerException: \n");
                    strBldr.append(ExceptionUtils.getStackTrace(npe));
                    logToolMessage(Level.WARNING, strBldr.toString(), false);
                }
            }
        } finally {
            sections.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
        // Set field count depending on "include Ids" export parameter
        int fieldCount = FIELD_COUNT;

        boolean includeIds = ((Boolean) getParameter(PARAM_INCL_IDS)).booleanValue();
        if (includeIds) {
            fieldCount = FIELD_COUNT_WITH_IDS;
        }

        // Set columns
        m_columns = new ArrayList<String>(fieldCount);
        m_columns.add(FIELD_SKL_ID);
        m_columns.add(FIELD_COURSE_NUMBER);
        m_columns.add(FIELD_STAFF_ID);
        m_columns.add(FIELD_SECTION_NUMBER);
        m_columns.add(FIELD_SEMESTER);
        m_columns.add(FIELD_TERM_CODE);
        m_columns.add(FIELD_DAY);
        m_columns.add(FIELD_PEROID);

        if (includeIds) {
            m_columns.add(FIELD_ID);
        }

        /*
         * Initialize maps
         */
        m_dayIdMap = new HashMap<String, List<String>>(2048);
        m_periodFirstMap = new HashMap<String, Boolean>(2048);
        m_periodIdMap = new HashMap<String, List<String>>(2048);
    }

    /**
     * Builds export criteria.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();

        criteria.addEqualTo(Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);
        criteria.addEqualTo(Section.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        criteria.addAndCriteria(
                getSchoolCriteria().copyWithAdjustedPath(Section.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL,
                        Section.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID));

        return criteria;
    }

    /**
     * Returns the day IDs of the passed schedule in a list ordered by day number. The information
     * is cached for
     * repeated lookups.
     *
     * @param schedule Schedule
     * @return List<String>
     */
    private List<String> getDayIds(Schedule schedule) {
        List<String> ids = m_dayIdMap.get(schedule.getOid());

        if (ids == null) {
            /*
             * Load days
             */
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ScheduleDay.COL_SCHEDULE_OID, schedule.getTimeScheduleOid());

            QueryByCriteria query = new QueryByCriteria(ScheduleDay.class, criteria);
            query.addOrderByAscending(ScheduleDay.COL_NUMBER);

            ids = new ArrayList<String>(getBroker().getCount(query));

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    ScheduleDay day = (ScheduleDay) iterator.next();
                    ids.add(day.getId());
                }
            } finally {
                iterator.close();
            }

            m_dayIdMap.put(schedule.getOid(), ids);
        }

        return ids;
    }

    /**
     * Returns if the passed schedule is period first. The information is cached for repeated
     * lookups.
     *
     * @param schedule Schedule
     * @return boolean
     */
    private boolean getPeriodFirst(Schedule schedule) {
        Boolean periodFirst = m_periodFirstMap.get(schedule.getOid());

        if (periodFirst == null) {
            periodFirst = Boolean.valueOf(schedule.scheduleExpressionPeriodFirst());
            m_periodFirstMap.put(schedule.getOid(), periodFirst);
        }

        return periodFirst.booleanValue();
    }

    /**
     * Returns the period IDs of the passed schedule in a list ordered by period number. The
     * information is cached for
     * repeated lookups.
     *
     * @param schedule Schedule
     * @return List<String>
     */
    private List<String> getPeriodIds(Schedule schedule) {
        List<String> ids = m_periodIdMap.get(schedule.getOid());

        if (ids == null) {
            /*
             * Load periods
             */
            Criteria criteria = new Criteria();
            criteria.addEqualTo(SchedulePeriod.COL_SCHEDULE_OID, schedule.getTimeScheduleOid());

            QueryByCriteria query = new QueryByCriteria(SchedulePeriod.class, criteria);
            query.addOrderByAscending(SchedulePeriod.COL_NUMBER);

            ids = new ArrayList<String>(getBroker().getCount(query));

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    SchedulePeriod period = (SchedulePeriod) iterator.next();
                    ids.add(period.getId());
                }
            } finally {
                iterator.close();
            }

            m_periodIdMap.put(schedule.getOid(), ids);
        }

        return ids;
    }

    /**
     * Parses the section's schedule display to get the day/period display (the first ID of each).
     *
     * @param grid DataGrid
     * @param section MasterSchedule
     */
    private void setDayPeriodFields(DataGrid grid, MasterSchedule section) {
        String scheduleDisplay = section.getScheduleDisplay();
        if (!StringUtils.isEmpty(scheduleDisplay)) {
            int index = scheduleDisplay.indexOf('(');

            if (index > 0) {
                String day = scheduleDisplay.substring(0, index);
                String period = scheduleDisplay.substring(index + 1, scheduleDisplay.length() - 1);

                if (getPeriodFirst(section.getSchedule())) {
                    day = scheduleDisplay.substring(index + 1, scheduleDisplay.length() - 1);
                    period = scheduleDisplay.substring(0, index);
                }

                // Loop over IDs til we find one that is in the display. The IDs are ordered by
                // number
                List<String> dayIds = getDayIds(section.getSchedule());
                for (String dayId : dayIds) {
                    if (day.contains(dayId)) {
                        day = dayId;
                        break;
                    }
                }

                List<String> periodIds = getPeriodIds(section.getSchedule());
                for (String periodId : periodIds) {
                    if (period.contains(periodId)) {
                        period = periodId;
                        break;
                    }
                }

                grid.set(FIELD_DAY, day);
                grid.set(FIELD_PEROID, period);
            }
        }
    }
}
