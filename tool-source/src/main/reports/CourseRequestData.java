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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.CourseRequest;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolCourseSchedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Student Request" report.
 *
 * @author X2 Development Corporation
 */
public class CourseRequestData extends ReportJavaSourceNet {
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
     * Name for the enumerated "course sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "courseSort";

    /**
     * Name for the enumerated "student sort" report parameter. The value is a String.
     */
    public static final String STUDENT_SORT_PARAM = "studentSort";

    /**
     * Name for "students with next school only" report parameter. This value is a Boolean.
     */
    public static final String STUDENTS_WITH_NEXT_SCHOOL_ONLY = "studentsWithNextSchoolOnly";

    private SchoolCourse m_currentCourse;
    private SisSchool m_currentSchool;
    private ScheduleReportHelper m_reportHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = new Criteria();

        if (m_currentCourse != null) {
            criteria.addEqualTo(CourseRequest.COL_SCHOOL_COURSE_OID, m_currentCourse.getOid());
            criteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID, m_currentSchool.getBuildContextOid());
        } else {
            criteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID, m_currentSchool.getBuildContextOid());
            criteria.addEqualTo(CourseRequest.COL_SCHOOL_OID, m_currentSchool.getOid());

            addUserCriteria(criteria,
                    (String) getParameter(QUERY_BY_PARAM),
                    (String) getParameter(QUERY_STRING_PARAM),
                    CourseRequest.class,
                    SchoolCourseSchedule.class,
                    SchoolCourseSchedule.COL_SCHOOL_COURSE_OID,
                    CourseRequest.COL_SCHOOL_COURSE_OID);
        }

        if (!m_reportHelper.getSchedule().useOptionalRequest()) {
            criteria.addEqualTo(CourseRequest.COL_OPTIONAL_INDICATOR, Boolean.FALSE);
        }

        boolean studentsWithNextSchoolOnly = ((Boolean) getParameter(STUDENTS_WITH_NEXT_SCHOOL_ONLY)).booleanValue();
        if (studentsWithNextSchoolOnly) {
            criteria.addNotNull(CourseRequest.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NEXT_SCHOOL_OID);
        }

        QueryByCriteria query = new QueryByCriteria(CourseRequest.class, criteria);

        /*
         * Sort the query.
         */
        applyUserSort(query, (String) getParameter(SORT_PARAM));
        query.addOrderByAscending(CourseRequest.COL_SCHOOL_COURSE_OID);

        // Add student sort
        applyUserSort(query, (String) getParameter(STUDENT_SORT_PARAM));
        query.addOrderByAscending(CourseRequest.COL_STUDENT_OID);

        /*
         * Execute the query
         */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see
     *      com.x2dev.sis.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_currentSchool = (SisSchool) getSchool();

        if (userData.getSessionNavConfig().getApplicationContext() == ApplicationContext.BUILD) {
            SchoolCourseSchedule currentAttributes;
            currentAttributes = userData.getCurrentRecord(SchoolCourseSchedule.class);
            if (currentAttributes != null) {
                m_currentCourse = currentAttributes.getSchoolCourse();
            }
        } else {
            m_currentCourse = userData.getCurrentRecord(SchoolCourse.class);
        }
        m_reportHelper = new ScheduleReportHelper(userData);
    }
}
