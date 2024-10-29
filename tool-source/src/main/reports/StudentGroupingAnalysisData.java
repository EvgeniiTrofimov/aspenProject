/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.DatabaseOptimizerFactory;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.CourseRequest;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolCourseSchedule;
import com.x2dev.sis.model.beans.StudentScheduleAttributes;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.CrossTab;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source for the Student Grouping Analysis report. This report lists all possible combinations
 * of student requests for courses broken down by department.
 *
 * @author X2 Development Corporation
 */
public class StudentGroupingAnalysisData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "minimum requests" report parameter. The value is a String.
     */
    private static final String MIN_REQUESTS_PARAM = "minRequests";

    // Grid fields
    private static final String FIELD_MESSAGE = "message";
    private static final String FIELD_TOTAL = "total";

    // Subreport constants
    private static final String SUBREPORT_DATA = "subreportData";
    private static final String SUBREPORT_FORMAT = "subreportFormat";
    private static final String SUBREPORT_FORMAT_ID = "SYS-BLD-029-SUB";

    // Other constants
    private static final int MAX_DEPARTMENTS = 7;

    // Report parameter
    private static final String DEPARTMENT_NAMES_MAP_PARAM = "departmentNamesMap";

    private TreeMap<Integer, List<SchoolCourseSchedule>> m_coursesMap;
    private CrossTab m_crossTab;
    private Map<Integer, String> m_departmentNames;
    private int m_minRequests;
    private ScheduleReportHelper m_reportHelper;
    private Map<String, List<String>> m_studentRequests;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(MAX_DEPARTMENTS);

        /*
         * Build the criteria based on user input
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addNotNull(SchoolCourseSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_DEPARTMENT_CODE);

        SubQuery subQuery = new SubQuery(SchoolCourseSchedule.class, X2BaseBean.COL_OID, getCurrentCriteria());
        criteria.addIn(X2BaseBean.COL_OID, subQuery);

        String column = "COUNT(DISTINCT " + SchoolCourseSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER
                + SchoolCourse.COL_DEPARTMENT_CODE + ")";
        SubQuery countQuery = new SubQuery(SchoolCourseSchedule.class, column, criteria);
        Object count = getBroker().getSubQueryValueByQuery(countQuery);

        int departmentCount =
                DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey()).getCount(count);
        if (departmentCount > MAX_DEPARTMENTS) {
            grid.append();
            grid.set(FIELD_MESSAGE, "You must select between two and seven departments.");
        } else {
            indexCourses(criteria);
            buildStudentRequests(criteria);

            /*
             * Iterates over each possible combination of courses.
             */
            for (long i = 0; i < m_crossTab.getSize(); i++) {
                int[] courseIndices = new int[m_coursesMap.size()];

                if (m_crossTab.getValueAtIndex(i, courseIndices)) {
                    List<String> studentList = null;
                    grid.append();

                    /*
                     * Iterates over the indices of the courses in the combination.
                     */
                    for (int j = 0; j < courseIndices.length; j++) {
                        List<SchoolCourseSchedule> coursesDept = m_coursesMap.get(Integer.valueOf(j));

                        if (coursesDept != null) {
                            SchoolCourseSchedule course = coursesDept.get(courseIndices[j] - 1);

                            if (course != null) {
                                grid.set(String.valueOf(j), course);
                                studentList = getTotalRequests(course, studentList);

                                if (studentList != null && studentList.size() >= m_minRequests) {
                                    grid.set(FIELD_TOTAL, Integer.valueOf(studentList == null ? 0 : studentList.size()));
                                } else {
                                    grid.deleteRow();
                                    break;
                                }
                            }
                        }
                    }
                }
            }

            addParameter(DEPARTMENT_NAMES_MAP_PARAM, m_departmentNames);
            addParameter(SUBREPORT_DATA, buildHeaderGrid());
            addParameter(SUBREPORT_FORMAT, new ByteArrayInputStream(
                    ReportUtils.getReport(SUBREPORT_FORMAT_ID, getBroker()).getCompiledFormat()));
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_coursesMap = new TreeMap<Integer, List<SchoolCourseSchedule>>();
        m_departmentNames = new HashMap<Integer, String>();
        m_minRequests = Integer.parseInt((String) getParameter(MIN_REQUESTS_PARAM));
        m_studentRequests = new HashMap<String, List<String>>();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_reportHelper = new ScheduleReportHelper(userData);
    }

    /**
     * Builds the grid used in the heading of the report to display courses.
     *
     * @return ReportDataGrid
     */
    private ReportDataGrid buildHeaderGrid() {
        ReportDataGrid grid = new ReportDataGrid(MAX_DEPARTMENTS);

        if (m_coursesMap != null) {
            Iterator departmentIterator = m_coursesMap.keySet().iterator();
            while (departmentIterator.hasNext()) {
                Integer column = (Integer) departmentIterator.next();

                Collection<SchoolCourseSchedule> courses = m_coursesMap.get(column);
                if (courses != null) {
                    int courseCount = 0;

                    Iterator coursesIterator = courses.iterator();
                    while (coursesIterator.hasNext()) {
                        SchoolCourseSchedule course = (SchoolCourseSchedule) coursesIterator.next();

                        if (grid.gotoRow(courseCount)) {
                            grid.set(column.toString(), course);
                        } else {
                            grid.append();
                            grid.set(column.toString(), course);
                        }

                        courseCount++;
                    }
                }
            }
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Builds a map of SchoolCourse OIDs keyed to lists of Student OIDs that requested the course.
     *
     * @param courseCriteria X2Criteria
     */
    private void buildStudentRequests(X2Criteria courseCriteria) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID,
                m_reportHelper.getSchedule().getDistrictContextOid());
        criteria.addEqualTo(CourseRequest.COL_SCHOOL_OID, m_reportHelper.getSchedule().getSchoolOid());
        criteria.addEqualTo(CourseRequest.COL_ALTERNATE_INDICATOR, Boolean.FALSE);

        SubQuery courseSubQuery =
                new SubQuery(SchoolCourseSchedule.class, SchoolCourseSchedule.COL_SCHOOL_COURSE_OID, courseCriteria);
        criteria.addIn(CourseRequest.COL_SCHOOL_COURSE_OID, courseSubQuery);

        QueryByCriteria query = new QueryByCriteria(CourseRequest.class, criteria);
        query.addOrderByAscending(CourseRequest.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER);

        Criteria studentCriteria = new Criteria();
        studentCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_INCLUDE_INDICATOR, Boolean.TRUE);
        studentCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_OID,
                m_reportHelper.getSchedule().getStudentScheduleOid());

        SubQuery studentSubQuery = new SubQuery(StudentScheduleAttributes.class,
                StudentScheduleAttributes.COL_STUDENT_OID, studentCriteria);
        criteria.addIn(CourseRequest.COL_STUDENT_OID, studentSubQuery);

        String[] columns = new String[] {CourseRequest.COL_SCHOOL_COURSE_OID, CourseRequest.COL_STUDENT_OID};
        ReportQueryByCriteria scheduleQuery = new ReportQueryByCriteria(CourseRequest.class, columns, criteria);

        m_studentRequests = getBroker().getGroupedColumnCollectionByQuery(scheduleQuery, 10000);
    }

    /**
     * Returns the intersection of the list of requests for the passed course and the current list
     * of requests.
     *
     * @param course SchoolCourseSchedule
     * @param studentList List<String>
     * @return List<String>
     */
    private List<String> getTotalRequests(SchoolCourseSchedule course, List<String> studentList) {
        List<String> studentOids = null;

        if (studentList == null) {
            List<String> courseRequestList = m_studentRequests.get(course.getSchoolCourseOid());
            if (courseRequestList == null) {
                courseRequestList = new ArrayList<String>();
            }
            studentOids = new ArrayList(courseRequestList);
        } else {
            List<String> courseRequestList = new ArrayList(m_studentRequests.get(course.getSchoolCourseOid()));
            studentList.retainAll(courseRequestList);

            studentOids = studentList;
        }

        return studentOids;
    }

    /**
     * Builds the CrossTab object and indexes the courses in each department.
     *
     * @param criteria X2Criteria
     */
    private void indexCourses(X2Criteria criteria) {
        QueryByCriteria query = new QueryByCriteria(SchoolCourseSchedule.class, criteria);
        query.addOrderByAscending(
                SchoolCourseSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_DEPARTMENT_CODE);
        query.addOrderByAscending(SchoolCourseSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER);

        Map<String, Collection<SchoolCourseSchedule>> coursesByDepartment =
                new TreeMap(getBroker().getGroupedCollectionByQuery(query,
                        SchoolCourseSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                                SchoolCourse.COL_DEPARTMENT_CODE,
                        500));

        int[] elements = new int[coursesByDepartment.size()];
        int departmentCount = 0;

        for (String department : coursesByDepartment.keySet()) {
            Collection<SchoolCourseSchedule> courses = coursesByDepartment.get(department);

            elements[departmentCount] = courses.size();
            m_coursesMap.put(Integer.valueOf(departmentCount), new ArrayList(courses));
            m_departmentNames.put(Integer.valueOf(departmentCount), department);

            departmentCount++;
        }

        m_crossTab = new CrossTab(elements);
    }
}
