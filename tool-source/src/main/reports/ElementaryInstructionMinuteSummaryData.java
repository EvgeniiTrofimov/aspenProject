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

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleDay;
import com.x2dev.sis.model.beans.ScheduleLunchConfig;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolCourseSchedule;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.StudentScheduleGroup;
import com.x2dev.sis.model.beans.StudentSection;
import com.x2dev.sis.model.business.ElementaryScheduleManager;
import com.x2dev.sis.model.business.schedule.CourseProgramManager;
import com.x2dev.sis.model.business.schedule.elementary.ElementaryMasterMatrixDataObject;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Elementary Instruction Minutes Summary" report.
 *
 * @author X2 Development Corporation
 */
public class ElementaryInstructionMinuteSummaryData extends ReportJavaSourceNet {

    private static final long serialVersionUID = 1L;

    /**
     * Name for the "group id" report parameter. The value is a String.
     */
    public static final String STUDENT_SCHEDULE_GROUP_ID = "groupIds";

    private static final String PARAMETER_CLASS_MINUTES_TOTAL = "totalClassMinutesGroup";
    private static final String PARAMETER_WEEK_MINUTES_TOTAL = "totalWeekMinutes";

    private static final String GRID_FIELD_SCHOOL_COURSE_DESCRIPTION = "courseDescription";
    private static final String GRID_FIELD_SCHOOL_COURSE_MINUTES_PER_SESSION = "minutesPerSession";
    private static final String GRID_FIELD_SCHOOL_COURSE_DISTRICT_NUMBER = "districtCourseNumber";
    private static final String GRID_FIELD_SCHOOL_COURSE_NUMBER = "courseNumber";
    private static final String GRID_FIELD_SCHOOL_COURSE_TOTAL_MINUTES = "totalMinutes";
    private static final String GRID_FIELD_SCHOOL_COURSE_TOTAL_MINUTES_SCHED = "totalMinutesSched";
    private static final String GRID_FIELD_STUDENT_SCHEDULE_GROUP = "studentScheduleGroup";

    private ScheduleReportHelper m_reportHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {

        ReportDataGrid grid = new ReportDataGrid(10, 15);
        ModelBroker broker = new ModelBroker(getPrivilegeSet());

        /*
         * Load course by OID map first
         */
        X2Criteria schoolCourseCriteria = new X2Criteria();
        schoolCourseCriteria.addEqualTo(SchoolCourse.REL_COURSE + "." + Course.COL_DISTRICT_CONTEXT_OID,
                m_reportHelper.getSchedule().getDistrictContextOid());
        schoolCourseCriteria.addEqualTo(SchoolCourse.COL_SCHOOL_OID, m_reportHelper.getSchedule().getSchoolOid());

        QueryByCriteria schoolCourseQuery = new QueryByCriteria(SchoolCourse.class, schoolCourseCriteria);
        Map<String, SchoolCourse> courseByOidMap = broker.getMapByQuery(schoolCourseQuery, X2BaseBean.COL_OID, 200);

        /*
         * Loop for student groups selected
         */
        Map<String, Map<String, Integer>> totalMinutesSchedByGroup = new HashMap<String, Map<String, Integer>>();
        Map<String, Collection<String>> groupMembers = new HashMap<String, Collection<String>>();
        Map<String, Collection<String>> coursesByGroup = new HashMap<String, Collection<String>>();
        Map<String, Integer> totalClassMinutesSchedByGroup = new HashMap<String, Integer>();
        Map<String, Boolean> subGroupIndicatorByCourseOidMap =
                ElementaryScheduleManager.getSubGroupIndicatorByCourseOid(m_reportHelper.getSchedule(), broker);
        Map<String, ScheduleLunchConfig> lunchConfigs =
                ElementaryScheduleManager.getLunchConfig(m_reportHelper.getSchedule(), broker);

        X2Criteria studentGroupCriteria = new X2Criteria();
        studentGroupCriteria.addEqualTo(StudentScheduleGroup.COL_DISTRICT_CONTEXT_OID,
                m_reportHelper.getSchedule().getDistrictContextOid());
        studentGroupCriteria.addEqualTo(StudentScheduleGroup.COL_SCHOOL_OID,
                m_reportHelper.getSchedule().getSchoolOid());

        String selectedGroupOidString = (String) getParameter(STUDENT_SCHEDULE_GROUP_ID);
        Collection<String> selectedGroupOids = StringUtils.convertDelimitedStringToList(selectedGroupOidString, ",");
        if (!StringUtils.isEmpty(selectedGroupOidString)) {

            Collection<String> adjustedSelectedGroupOids = new HashSet<String>();

            X2Criteria selectedGroupsCriteria = new X2Criteria();
            selectedGroupsCriteria.addIn(X2BaseBean.COL_OID, selectedGroupOids);

            QueryByCriteria selectedGroupsQuery =
                    new QueryByCriteria(StudentScheduleGroup.class, selectedGroupsCriteria);
            selectedGroupsQuery.addOrderByAscending(StudentScheduleGroup.COL_DESCRIPTION);
            Collection<StudentScheduleGroup> selectedGroups = getBroker().getCollectionByQuery(selectedGroupsQuery);

            for (StudentScheduleGroup selectedGroup : selectedGroups) {
                if (selectedGroup.getParentStudentScheduleGroup() != null) {
                    adjustedSelectedGroupOids.add(selectedGroup.getParentStudentScheduleGroupOid());
                }
                adjustedSelectedGroupOids.add(selectedGroup.getOid());
            }

            studentGroupCriteria.addIn(X2BaseBean.COL_OID, adjustedSelectedGroupOids);
        }

        ElementaryMasterMatrixDataObject matrixDataObject =
                new ElementaryMasterMatrixDataObject(m_reportHelper.getSchedule(), m_reportHelper.getSectionClass(),
                        broker);

        QueryByCriteria studentGroupQuery = new QueryByCriteria(StudentScheduleGroup.class, studentGroupCriteria);
        studentGroupQuery.addOrderByAscending(StudentScheduleGroup.COL_DESCRIPTION);
        QueryIterator studentGroupIterator = getBroker().getIteratorByQuery(studentGroupQuery);
        while (studentGroupIterator.hasNext()) {
            StudentScheduleGroup group = (StudentScheduleGroup) studentGroupIterator.next();

            groupMembers.put(group.getOid(),
                    ElementaryScheduleManager.getGroupMembersForGroup(m_reportHelper.getSchedule(), group, false,
                            broker));

            ElementaryScheduleManager.calculateTotalMinutesScheduledByGroup(m_reportHelper.getSchedule(),
                    group,
                    null,
                    m_reportHelper.getSectionClass(),
                    coursesByGroup,
                    groupMembers,
                    isLunchScheduledForGroup(m_reportHelper.getSchedule(), lunchConfigs, group),
                    totalMinutesSchedByGroup,
                    subGroupIndicatorByCourseOidMap,
                    matrixDataObject,
                    broker);

            Collection<String> coursesForGroup = coursesByGroup.get(group.getOid());

            if (coursesForGroup != null
                    && (selectedGroupOids.isEmpty() || selectedGroupOids.contains(group.getOid()))) {

                int totalInstructionMinutes = 0;
                for (String courseOid : coursesForGroup) {
                    SchoolCourse schoolcourse = courseByOidMap.get(courseOid);
                    if (schoolcourse != null) {
                        grid.append();
                        grid.set(GRID_FIELD_STUDENT_SCHEDULE_GROUP, group);
                        grid.set(GRID_FIELD_SCHOOL_COURSE_NUMBER, schoolcourse.getNumber());
                        grid.set(GRID_FIELD_SCHOOL_COURSE_DISTRICT_NUMBER, schoolcourse.getCourse().getNumber());
                        grid.set(GRID_FIELD_SCHOOL_COURSE_DESCRIPTION, schoolcourse.getDescription());
                        if (schoolcourse.getMasterType().equals(SchoolCourse.MASTER_TYPE_LUNCH)) {
                            grid.set(GRID_FIELD_SCHOOL_COURSE_TOTAL_MINUTES,
                                    Integer.valueOf(30 * 5));
                            grid.set(GRID_FIELD_SCHOOL_COURSE_MINUTES_PER_SESSION,
                                    Integer.valueOf(30));
                        } else {
                            grid.set(GRID_FIELD_SCHOOL_COURSE_TOTAL_MINUTES,
                                    Integer.valueOf(CourseProgramManager.getAdjustedMinutesPerCycle(group.getProgramOid(),
                                            schoolcourse, getBroker())));
                            grid.set(GRID_FIELD_SCHOOL_COURSE_MINUTES_PER_SESSION,
                                    Integer.valueOf(schoolcourse.getMinimumMinutesPerSession()));
                        }
                        grid.set(GRID_FIELD_SCHOOL_COURSE_TOTAL_MINUTES_SCHED,
                                totalMinutesSchedByGroup.get(group.getOid()).get(courseOid));

                        totalInstructionMinutes +=
                                totalMinutesSchedByGroup.get(group.getOid()).get(courseOid) == null
                                        ? 0
                                        : totalMinutesSchedByGroup.get(group.getOid()).get(courseOid).intValue();
                    }
                }

                totalClassMinutesSchedByGroup.put(group.getOid(), Integer.valueOf(totalInstructionMinutes));
            }
        }

        grid.beforeTop();

        addParameter(PARAMETER_CLASS_MINUTES_TOTAL, totalClassMinutesSchedByGroup);
        /*
         * Get the total minutes of the week.
         */
        Collection<ScheduleDay> days = m_reportHelper.getSchedule().getScheduleDays(true);
        int totalMinutesWeek = 0;
        for (ScheduleDay day : days) {
            totalMinutesWeek += day.getEndTime().getTimeInMinutes() - day.getStartTime().getTimeInMinutes();
        }
        addParameter(PARAMETER_WEEK_MINUTES_TOTAL, Integer.valueOf(totalMinutesWeek));

        return grid;
    }

    /**
     * Returns whether lunch is schedule for the group
     *
     * @param schedule Schedule
     * @param lunchConfigs Map<String, ScheduleLunchConfig>
     * @param group StudentScheduleGroup
     *
     * @return KeyValuePair<ScheduleLunchConfig, Boolean>
     */
    private KeyValuePair<ScheduleLunchConfig, Boolean> isLunchScheduledForGroup(Schedule schedule,
                                                                                Map<String, ScheduleLunchConfig> lunchConfigs,
                                                                                StudentScheduleGroup group) {
        KeyValuePair<ScheduleLunchConfig, Boolean> isLunchScheduled = null;

        if (!schedule.isLunchSectionByHomeroom()) {
            X2Criteria lunchCriteria = new X2Criteria();
            lunchCriteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, schedule.getOid());
            lunchCriteria.addEqualTo(
                    StudentSection.REL_SECTION + "." + Section.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_LUNCH);

            String studentHomeRoomField = ElementaryScheduleManager.getStudentHomeroomField(schedule);
            String homeroom = group.getParentStudentScheduleGroup() == null ? group.getId()
                    : group.getParentStudentScheduleGroup().getId();

            lunchCriteria.addEqualTo(StudentSection.REL_STUDENT + "." + studentHomeRoomField, homeroom);

            String[] columns = new String[] {StudentSection.COL_SECTION_OID};

            ColumnQuery lunchQuery = new ColumnQuery(m_reportHelper.getStudentSectionClass(), columns, lunchCriteria);
            lunchQuery.addGroupBy(StudentSection.COL_SECTION_OID);
            lunchQuery.addOrderByDescending("count(" + StudentSection.COL_SECTION_OID + ")");

            String lunchMasterOid = null;
            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(lunchQuery);
            try {
                if (iterator.hasNext()) {
                    lunchMasterOid = (String) ((Object[]) iterator.next())[0];
                }
            } finally {
                iterator.close();
            }

            Section lunchSection = (Section) getBroker().getBeanByOid(m_reportHelper.getSectionClass(), lunchMasterOid);

            if (lunchSection != null) {
                isLunchScheduled =
                        new KeyValuePair<ScheduleLunchConfig, Boolean>(lunchConfigs.get(lunchSection.getHouseCode()),
                                !StringUtils.isEmpty(lunchSection.getScheduleDisplay()));
            }
        }

        return isLunchScheduled;
    }

    /**
     * Returns the sub-query for the recess course oid.
     *
     * @param schedule Schedule
     * @param masterType String
     *
     * @return SubQuery
     */
    private SubQuery getCourseSubQuery(Schedule schedule, String masterType) {
        X2Criteria courseAttributeCriteria = new X2Criteria();
        courseAttributeCriteria.addEqualTo(SchoolCourseSchedule.COL_SCHEDULE_OID, schedule.getCourseScheduleOid());
        courseAttributeCriteria.addEqualTo(SchoolCourseSchedule.COL_SCHEDULE_INCLUDE_INDICATOR, Boolean.TRUE);
        courseAttributeCriteria.addEqualTo(
                SchoolCourseSchedule.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                masterType);

        SubQuery courseOidSubQuery = new SubQuery(SchoolCourseSchedule.class,
                SchoolCourseSchedule.COL_SCHOOL_COURSE_OID, courseAttributeCriteria);
        return courseOidSubQuery;
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
}
