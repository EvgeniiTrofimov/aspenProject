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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.CourseRequest;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolCourseSchedule;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.StudentScheduleAttributes;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Course Schedule Summary" report.
 *
 * @author X2 Development Corporation
 */
public class CourseScheduleSummaryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

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

    // Grid fields
    private static final String FIELD_AVERAGE_CLASS_SIZE = "averageClassSize";
    private static final String FIELD_COURSE_NUMBER = "courseNumber";
    private static final String FIELD_COURSE_DESCRIPTION = "courseDescription";
    private static final String FIELD_COURSE_SECTIONS = "sections";
    private static final String FIELD_COURSE_REQUESTS = "requests";
    private static final String FIELD_COURSE_SEATS = "seats";
    private static final String FIELD_SCHEDULED = "scheduled";
    private static final String FIELD_SCHEDULED_PERCENT = "scheduledPercent";
    private static final String FIELD_UNSCHEDULED = "unscheduled";

    private ScheduleReportHelper m_reportHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        X2Criteria courseOidCriteria = new X2Criteria();
        courseOidCriteria.addEqualTo(SchoolCourseSchedule.COL_SCHEDULE_OID, m_reportHelper.getCourseScheduleOid());
        courseOidCriteria.addEqualTo(SchoolCourseSchedule.COL_SCHEDULE_INCLUDE_INDICATOR, Boolean.TRUE);
        SubQuery courseOidSubQuery =
                new SubQuery(SchoolCourseSchedule.class, SchoolCourseSchedule.COL_SCHOOL_COURSE_OID, courseOidCriteria);

        /*
         * Retrieve courses
         */
        X2Criteria courseCriteria = new X2Criteria();
        courseCriteria.addIn(X2BaseBean.COL_OID, courseOidSubQuery);
        addUserCriteria(courseCriteria, (String) getParameter(QUERY_BY_PARAM),
                (String) getParameter(QUERY_STRING_PARAM), null, null);

        QueryByCriteria courseQuery = new QueryByCriteria(SchoolCourse.class, courseCriteria);
        applyUserSort(courseQuery, (String) getParameter(SORT_PARAM));

        Collection courses = getBroker().getCollectionByQuery(courseQuery);

        /*
         * Retrieve all students that are flagged to be scheduled.
         */
        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_OID, m_reportHelper.getStudentScheduleOid());
        studentCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_INCLUDE_INDICATOR, Boolean.TRUE);

        SubQuery studentOidSubQuery = new SubQuery(StudentScheduleAttributes.class,
                StudentScheduleAttributes.COL_STUDENT_OID, studentCriteria);

        /*
         * Retrieve the request by course map.
         */
        X2Criteria requestCriteria = new X2Criteria();
        requestCriteria.addEqualTo(CourseRequest.COL_SCHOOL_OID, m_reportHelper.getSchedule().getSchoolOid());
        requestCriteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID,
                m_reportHelper.getSchedule().getDistrictContextOid());
        requestCriteria.addEqualTo(CourseRequest.COL_ALTERNATE_INDICATOR, Boolean.FALSE);
        requestCriteria.addIn(CourseRequest.COL_SCHOOL_COURSE_OID, courseOidSubQuery);
        requestCriteria.addIn(CourseRequest.COL_STUDENT_OID, studentOidSubQuery);

        if (!m_reportHelper.getSchedule().useOptionalRequest()) {
            /*
             * Based on the preference to decide if including optional requests to the count.
             */
            requestCriteria.addEqualTo(CourseRequest.COL_OPTIONAL_INDICATOR, Boolean.FALSE);
        }

        QueryByCriteria requestQuery = new QueryByCriteria(CourseRequest.class, requestCriteria);
        Map requestsByCourseMap =
                getBroker().getGroupedCollectionByQuery(requestQuery, CourseRequest.COL_SCHOOL_COURSE_OID, 10000);

        /*
         * Retrieve the master by course map.
         */
        X2Criteria masterCriteria = new X2Criteria();
        masterCriteria.addEqualTo(Section.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
        masterCriteria.addIn(Section.COL_SCHOOL_COURSE_OID, courseOidSubQuery);

        QueryByCriteria masterQuery = new QueryByCriteria(m_reportHelper.getSectionClass(), masterCriteria);
        Map masterByCourseMap =
                getBroker().getGroupedCollectionByQuery(masterQuery, Section.COL_SCHOOL_COURSE_OID, 1000);

        ReportDataGrid grid = new ReportDataGrid(2000, 10);

        Iterator courseIterator = courses.iterator();
        while (courseIterator.hasNext()) {
            SchoolCourse course = (SchoolCourse) courseIterator.next();
            String courseOid = course.getOid();
            Collection sections = (Collection) masterByCourseMap.get(courseOid);

            grid.append();
            grid.set(FIELD_COURSE_NUMBER, course.getNumber());
            grid.set(FIELD_COURSE_DESCRIPTION, course.getDescription());

            int sectionCount = sections == null ? 0 : sections.size();
            grid.set(FIELD_COURSE_SECTIONS, Integer.valueOf(sectionCount));

            Collection courseRequests = (Collection) requestsByCourseMap.get(courseOid);
            int requestCount = courseRequests == null ? 0 : courseRequests.size();
            grid.set(FIELD_COURSE_REQUESTS, Integer.valueOf(requestCount));

            int courseSeats = 0;
            int scheduledCount = 0;
            if (sections != null) {
                Iterator sectionIterator = sections.iterator();
                while (sectionIterator.hasNext()) {
                    Section section = (Section) sectionIterator.next();
                    courseSeats += section.getEnrollmentMax();
                    scheduledCount += section.getEnrollmentTotal();
                }
            }

            grid.set(FIELD_COURSE_SEATS, Integer.valueOf(courseSeats));
            grid.set(FIELD_SCHEDULED, Integer.valueOf(scheduledCount));

            double schedulePercent = requestCount == 0 ? 0.0 : (double) scheduledCount * 100 / requestCount;
            grid.set(FIELD_SCHEDULED_PERCENT, Double.valueOf(schedulePercent));

            grid.set(FIELD_UNSCHEDULED, Integer.valueOf(requestCount - scheduledCount));

            int averageSize = sectionCount == 0 ? 0 : scheduledCount / sectionCount;
            grid.set(FIELD_AVERAGE_CLASS_SIZE, Integer.valueOf(averageSize));
        }

        grid.beforeTop();
        return grid;
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
}
