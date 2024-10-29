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


import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.nav.FilterException;
import com.x2dev.sis.model.beans.BuildMasterSchedule;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.StaffScheduleAttributes;
import com.x2dev.sis.web.schedule.ScheduleUtils;
import com.x2dev.utils.X2BaseException;
import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.JDOMException;

/**
 * Prepares the data for the "Teacher Assignment Verification" report.
 *
 * @author X2 Development Corporation
 */
public class TeacherAssignmentVerificationData extends ReportJavaSourceNet {

    /**
     * Name for the enumerated "filter" report parameter. The value is an Integer.
     */
    public static final String FILTER_BY_PARAM = "filterBy";

    /**
     * Name for the "minimum assignments" report parameter. The value is an Integer.
     */
    public static final String MINIMUM_ASSIGNMENTS_PARAM = "minimumAssignments";

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the "requirement" report parameter. The value is an ProgramStudies object.
     */
    public static final String SCHEDULE_PARAM = "schedule";

    /**
     * String constants for the grid fields
     */
    private static final String FIELD_STAFF_NAME = "nameView";
    private static final String FIELD_STAFF_MINIMUM = "minimumAssignment";
    private static final String FIELD_STAFF_ASSIGNED = "assigned";

    /**
     * Constants for the filter option dropdown in the report input
     */
    private static final int ALL_TEACHERS = 0;
    private static final int TEACHERS_UNDER_MINIMUM = 1;
    private static final int TEACHERS_AT_OR_ABOVE_MINIMUM = 2;

    /**
     * Private member for the current schedule in use for the report.
     */
    private Schedule m_schedule;


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws FilterException exception
     * @throws JDOMException exception
     * @throws IOException Signals that an I/O exception has occurred.
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws FilterException, JDOMException, IOException {

        if (m_schedule == null) {
            return null;
        }

        X2Criteria criteria = new X2Criteria();

        // Pull out report inputs
        Integer minimumAssignments = (Integer) getParameter(MINIMUM_ASSIGNMENTS_PARAM);
        int assignmentFilter = ((Integer) getParameter(FILTER_BY_PARAM)).intValue();
        String queryBy = (String) getParameter(QUERY_BY_PARAM);

        if (queryBy.equals(SELECTION_SPECIAL_CASE_PREFIX + CURRENT_KEY)) {
            criteria = getCurrentCriteria();
        } else {
            addUserCriteria(criteria, queryBy, (String) getParameter(QUERY_STRING_PARAM), null, null);
        }

        // Make sure the staff are part of this schedule and marked as include in scheduling
        criteria.addEqualTo(StaffScheduleAttributes.COL_SCHEDULE_OID, m_schedule.getStaffScheduleOid());
        criteria.addEqualTo(StaffScheduleAttributes.COL_SCHEDULE_PARTICIPATE_INDICATOR, Boolean.TRUE);

        if (minimumAssignments != null) {
            // If minimum assignments filter is defined then make sure it matches
            criteria.addEqualTo(StaffScheduleAttributes.COL_MINIMUM_ASSIGNMENTS, minimumAssignments);
        }

        // Find all the sections in the current scenario or schedule
        Criteria teacherAssignmentCriteria = new Criteria();
        teacherAssignmentCriteria.addEqualTo(Section.COL_SCHEDULE_OID, m_schedule.getOid());
        QueryByCriteria teacherAssignmentQuery = new QueryByCriteria(
                m_schedule.getBuildScenarioIndicator() ? BuildMasterSchedule.class : MasterSchedule.class,
                teacherAssignmentCriteria);
        // Get a map of staff sections to determine number of assignments
        Map<String, Collection<Section>> staffSectionMap =
                getBroker().getGroupedCollectionByQuery(teacherAssignmentQuery, Section.COL_PRIMARY_STAFF_OID, 100);


        QueryByCriteria staffQuery = new QueryByCriteria(StaffScheduleAttributes.class, criteria);
        applyUserSort(staffQuery, (String) getParameter(SORT_PARAM));

        ReportDataGrid grid = new ReportDataGrid(3);
        QueryIterator iterator = getBroker().getIteratorByQuery(staffQuery);

        try {
            while (iterator.hasNext()) {
                StaffScheduleAttributes staffAttributes = (StaffScheduleAttributes) iterator.next();

                boolean includeTeacher = false;
                int minimumAssigned = staffAttributes.getMinimumAssignments();
                int actualAssigned = staffSectionMap.containsKey(staffAttributes.getStaffOid())
                        ? staffSectionMap.get(staffAttributes.getStaffOid()).size()
                        : 0;

                switch (assignmentFilter) {
                    case ALL_TEACHERS:
                        includeTeacher = true;
                        break;

                    case TEACHERS_AT_OR_ABOVE_MINIMUM:
                        if (minimumAssigned <= actualAssigned) {
                            includeTeacher = true;
                        }
                        break;

                    case TEACHERS_UNDER_MINIMUM:
                        if (minimumAssigned > actualAssigned) {
                            includeTeacher = true;
                        }
                        break;
                }
                if (includeTeacher) {
                    grid.append();
                    grid.set(FIELD_STAFF_NAME, staffAttributes.getStaff().getNameView());
                    grid.set(FIELD_STAFF_MINIMUM, String.valueOf(minimumAssigned));
                    grid.set(FIELD_STAFF_ASSIGNED, String.valueOf(actualAssigned));
                }
            }
        } finally {
            if (iterator != null) {
                iterator.close();
            }
        }

        addParameter(SCHEDULE_PARAM, m_schedule);

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
        m_schedule = ScheduleUtils.getSchedule(userData);
    }
}
