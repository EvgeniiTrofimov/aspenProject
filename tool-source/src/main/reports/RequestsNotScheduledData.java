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
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.web.SisUserDataContainer;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the "Requests Not Scheduled By Student" report.
 *
 * @author X2 Development Corporation
 */
public class RequestsNotScheduledData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "course sort" report parameter. The value is an Integer.
     */
    public static final String COURSE_SORT_PARAM = "courseSort";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String COURSE_QUERY_BY_PARAM = "courseQuery";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String COURSE_QUERY_STRING_PARAM = "courseString";

    /**
     * Name for the int value relating to SchoolCourse grouping. The value is an int.
     */
    public static final int GROUP_BY_COURSE = 1;

    /**
     * Name for the "group by" report parameter. The value is an Integer.
     */
    public static final String GROUP_BY_PARAM = "groupBy";

    /**
     * Name for the int value relating to Student grouping. The value is an int.
     */
    public static final int GROUP_BY_STUDENT = 0;


    /**
     * Name for the "student sort" report parameter. The value is an Integer.
     */
    public static final String STUDENT_SORT_PARAM = "studentSort";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String STUDENT_QUERY_BY_PARAM = "studentQuery";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String STUDENT_QUERY_STRING_PARAM = "studentString";

    // Grid fields
    private static final String FIELD_STUDENT_NAME = "name";
    private static final String FIELD_STUDENT_YOG = "yog";
    private static final String FIELD_STUDENT_HOMEROOM = "homeroom";
    private static final String FIELD_COURSE_NUMBER = "courseNumber";
    private static final String FIELD_COURSE_DESCRIPTION = "courseDescription";

    // ID prefix for report card formats
    private static final String REPORT_ID_PREFIX = "SYS-BLD-";

    // Member variables
    private String m_currentObjectOid;
    private ApplicationContext m_context;
    private int m_groupBy;
    private String m_districtContextOid;
    private String m_scheduleOid;
    private String m_studentScheduleOid;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(1000, 20);

        m_groupBy = ((Integer) getParameter(GROUP_BY_PARAM)).intValue();

        String objectCriteria = "";
        if (!StringUtils.isEmpty(m_currentObjectOid)) {
            if (m_groupBy == GROUP_BY_STUDENT) {
                objectCriteria += " AND STD_OID = '" + m_currentObjectOid + "' ";
            } else {
                objectCriteria += " AND CSK_OID = '" + m_currentObjectOid + "' ";
            }
        }

        else {
            int studentQuery = ((Integer) getParameter(STUDENT_QUERY_BY_PARAM)).intValue();
            switch (studentQuery) {
                case 1: // Yog
                    objectCriteria += " AND STD_YOG = " + getParameter(STUDENT_QUERY_STRING_PARAM) + " ";
                    break;

                default: // All
                    objectCriteria += "";
                    break;
            }

            int courseQuery = ((Integer) getParameter(COURSE_QUERY_BY_PARAM)).intValue();
            switch (courseQuery) {
                case 1: // Department
                    objectCriteria += " AND CSK_DEPARTMENT_CODE = '" +
                            getParameter(COURSE_QUERY_STRING_PARAM) + "' ";
                    break;

                default: // All
                    objectCriteria += "";
                    break;
            }
        }

        String studentSort;
        int studentSortPick = ((Integer) getParameter(STUDENT_SORT_PARAM)).intValue();
        switch (studentSortPick) {
            case 0: // Name
                studentSort = "STD_NAME_VIEW, STD_OID";
                break;

            case 1: // YOG
                studentSort = "STD_YOG, STD_NAME_VIEW, STD_OID";
                break;

            default:
                studentSort = "STD_OID";
                break;
        }

        String courseSort;
        int courseSortPick = ((Integer) getParameter(COURSE_SORT_PARAM)).intValue();
        switch (courseSortPick) {
            case 0: // Number
                courseSort = "CSK_COURSE_NUMBER, CSK_OID";
                break;

            case 1: // Department
                courseSort = "CSK_DEPARTMENT_CODE, CSK_COURSE_NUMBER, CSK_OID";
                break;

            default:
                courseSort = "CSK_OID";
                break;
        }

        String sort;
        if (m_groupBy == GROUP_BY_STUDENT) {
            sort = "ORDER BY " + studentSort + ", " + courseSort + " ";
        } else {
            sort = "ORDER BY " + courseSort + ", " + studentSort + " ";
        }

        try {
            populateGrid(grid, objectCriteria, sort);
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        }

        setReportFormat();

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
        m_context = userData.getSessionNavConfig().getApplicationContext();

        Schedule schedule;
        if (m_context == ApplicationContext.BUILD) {
            schedule = ((SisUserDataContainer) userData).getBuildSchedule();

            StudentScheduleAttributes studentAttributes =
                    userData.getCurrentRecord(StudentScheduleAttributes.class);
            if (studentAttributes != null) {
                m_currentObjectOid = studentAttributes.getStudentOid();
            } else {
                SchoolCourseSchedule courseAttributes =
                        userData.getCurrentRecord(SchoolCourseSchedule.class);
                if (courseAttributes != null) {
                    m_currentObjectOid = courseAttributes.getSchoolCourseOid();
                }
            }
        } else {
            schedule = ((SisSchool) userData.getSchool()).getActiveSchedule();

            SisStudent student = userData.getCurrentRecord(SisStudent.class);
            if (student != null) {
                m_currentObjectOid = student.getOid();
            } else {
                SchoolCourse course = userData.getCurrentRecord(SchoolCourse.class);
                if (course != null) {
                    m_currentObjectOid = course.getOid();
                }
            }
        }

        m_scheduleOid = schedule.getOid();
        m_studentScheduleOid = schedule.getStudentScheduleOid();
        m_districtContextOid = schedule.getDistrictContextOid();
    }

    /**
     * Populates the grid with the results from the STUDENT_COURSE_REQUEST query.
     *
     *
     * @param grid ReportDataGrid
     * @param criteria String
     * @param sort String
     * @throws SQLException exception
     */
    private void populateGrid(ReportDataGrid grid, String criteria, String sort) throws SQLException {
        String masterScheduleSubquery;
        if (m_context == ApplicationContext.BUILD) {
            masterScheduleSubquery = "SELECT BLM_CSK_OID FROM SCHEDULE_BUILD_MASTER " +
                    " WHERE BLM_SCH_OID = ?";
        } else {
            masterScheduleSubquery = "SELECT MST_CSK_OID FROM SCHEDULE_MASTER " +
                    " WHERE MST_SCH_OID = ?";
        }

        /*
         * Grab a map of student > student sections so that they query does not need to do a not in.
         * We can do that processing
         * in the java code.
         */
        X2Criteria studentScheduleCriteria = new X2Criteria();
        studentScheduleCriteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_scheduleOid);
        QueryByCriteria studentScheduleQuery = new QueryByCriteria(
                m_context == ApplicationContext.BUILD ? BuildStudentSchedule.class : StudentSchedule.class,
                studentScheduleCriteria);
        Map<String, Collection<StudentSection>> studentScheduleMap =
                getBroker().getGroupedCollectionByQuery(studentScheduleQuery, StudentSection.COL_STUDENT_OID, 1000);

        String sql = "SELECT STD_NAME_VIEW, STD_OID, STD_YOG, STD_HOMEROOM, CSK_OID, " +
                "CSK_COURSE_NUMBER, CSK_COURSE_DESCRIPTION " +
                "FROM STUDENT_COURSE_REQUEST " +
                "INNER JOIN STUDENT ON REQ_STD_OID = STD_OID " +
                "INNER JOIN COURSE_SCHOOL ON CSK_OID = REQ_CSK_OID " +
                "INNER JOIN STUDENT_SCHEDULE_ATTRIBUTES ON REQ_STD_OID = SSA_STD_OID " +
                "WHERE REQ_CTX_OID = ? " +
                "  AND REQ_SKL_OID = ? " +
                "  AND REQ_ALTERNATE_IND <> '1' " +
                "  AND SSA_SCH_OID = ? " +
                "  AND SSA_SCHD_INCLUDE_IND = '1' " +
                "  AND REQ_CSK_OID in (" + masterScheduleSubquery + ") " +
                criteria + " " + sort;

        Connection connection = getBroker().borrowConnection();
        try {
            PreparedStatement statement = connection.prepareStatement(sql);
            try {
                statement.setString(1, m_districtContextOid);
                statement.setString(2, getSchool().getOid());
                statement.setString(3, m_studentScheduleOid);
                statement.setString(4, m_scheduleOid);

                ResultSet results = statement.executeQuery();
                try {
                    while (results.next()) {
                        Collection<String> courseOids = new ArrayList<String>();
                        String studentOid = results.getString("STD_OID");
                        Collection<StudentSection> sections = studentScheduleMap.get(studentOid);

                        if (!CollectionUtils.isEmpty(sections)) {
                            /*
                             * If the student is scheduled in sections then grab all the CSK_OIDs
                             * into a collection.
                             */
                            CollectionUtils.loadPropertyCollection(sections, StudentSection.REL_SECTION
                                    + ModelProperty.PATH_DELIMITER + Section.COL_SCHOOL_COURSE_OID, courseOids);
                        }

                        if (!courseOids.contains(results.getString("CSK_OID"))) {
                            /*
                             * If the collection does not have this course then it is a request that
                             * is
                             * not scheduled.
                             */
                            grid.append();
                            grid.set(FIELD_STUDENT_NAME, results.getString("STD_NAME_VIEW"));
                            grid.set(FIELD_STUDENT_YOG, results.getString("STD_YOG"));
                            grid.set(FIELD_STUDENT_HOMEROOM, results.getString("STD_HOMEROOM"));
                            grid.set(FIELD_COURSE_NUMBER, results.getString("CSK_COURSE_NUMBER"));
                            grid.set(FIELD_COURSE_DESCRIPTION, results.getString("CSK_COURSE_DESCRIPTION"));
                        }
                    }
                } finally {
                    results.close();
                }
            } finally {
                statement.close();
            }
        } finally {
            getBroker().returnConnection();
        }
    }

    /**
     * Sets the report format based on the selected grade term.
     */
    private void setReportFormat() {
        String reportSuffix;
        if (m_groupBy == GROUP_BY_STUDENT) {
            reportSuffix = "017";
        } else {
            reportSuffix = "018";
        }

        String reportId = REPORT_ID_PREFIX + reportSuffix;
        setFormatId(reportId);
    }
}
