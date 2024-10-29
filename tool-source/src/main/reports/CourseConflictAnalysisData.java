/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolCourseSchedule;
import com.x2dev.sis.web.schedule.ScheduleUtils;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the request conflicts report. For each course in the selection, this report
 * lists the other courses requested along with that course and a count thereof. This information
 * can help determine the number of potential student schedule conflicts if two courses are
 * scheduled at the same time.
 *
 * @author X2 Development Corporation
 */
public class CourseConflictAnalysisData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    private static final String GRID_COL_CONFLICT_COURSE = "conflictCourse";
    private static final String GRID_COL_CONFLICTS = "conflicts";
    private static final String GRID_COL_PRIMARY_COURSE = "primaryCourse";
    private static final String GRID_COL_REQUESTS = "requests";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    private PreparedStatement m_conflictStatement;
    private Connection m_conflictsConnection;
    private ApplicationContext m_context;
    private Map m_courseMap;
    private ReportDataGrid m_grid;
    private Map m_requestCountMap;
    private Schedule m_schedule;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        m_grid = new ReportDataGrid(10000, 10);
        ModelBroker conflictsBroker = new ModelBroker(getPrivilegeSet());

        try {

            m_conflictsConnection = conflictsBroker.borrowConnection();
            prepare();

            QueryByCriteria query = buildCourseQuery();
            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    SchoolCourse primaryCourse = (SchoolCourse) iterator.next();

                    ResultSet conflicts = getConflicts(primaryCourse.getOid());
                    while (conflicts.next()) {
                        String conflictCourseOid = conflicts.getString("CONFLICT_COURSE");
                        int conflictCount = conflicts.getInt("CONFLICTS");
                        SchoolCourse conflictCourse = (SchoolCourse) m_courseMap.get(conflictCourseOid);
                        Integer requests = (Integer) m_requestCountMap.get(conflictCourseOid);

                        if (conflictCourse != null) {
                            m_grid.append();

                            m_grid.set(GRID_COL_PRIMARY_COURSE, primaryCourse);
                            m_grid.set(GRID_COL_CONFLICT_COURSE, conflictCourse);
                            m_grid.set(GRID_COL_CONFLICTS, Integer.valueOf(conflictCount));
                            m_grid.set(GRID_COL_REQUESTS, requests);
                        }
                    }
                }
            } finally {
                iterator.close();
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.SEVERE, sqle.getMessage(), sqle);
        } finally {
            conflictsBroker.returnConnection();
        }

        m_grid.beforeTop();

        return m_grid;
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#releaseResources()
     */
    @Override
    protected void releaseResources() {
        try {
            if (m_conflictStatement != null) {
                m_conflictStatement.close();
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.SEVERE, sqle.getMessage(), sqle);
        }

        getBroker().returnConnection();

        m_grid.clear();
        m_grid = null;

        super.releaseResources();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see
     *      com.follett.fsc.core.k12.tools.reports.ReportJavaSource#saveState(com.follett.fsc.core.k12.
     *      web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_context = userData.getSessionNavConfig().getApplicationContext();
        m_schedule = ScheduleUtils.getSchedule(userData);
    }

    /**
     * Builds the query for the courses to include on the report.
     *
     * @return QueryByCriteria
     */
    private QueryByCriteria buildCourseQuery() {
        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();

        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 0: // Current selection
                if (m_context == ApplicationContext.BUILD) {
                    Criteria buildCriteria = getCurrentCriteria();
                    SubQuery buildSubQuery = new SubQuery(SchoolCourseSchedule.class,
                            SchoolCourseSchedule.COL_SCHOOL_COURSE_OID, buildCriteria);
                    criteria.addIn(X2BaseBean.COL_OID, buildSubQuery);
                } else {
                    criteria = getCurrentCriteria();
                }
                break;

            case 1: // Department
                criteria.addEqualTo(SchoolCourse.COL_DEPARTMENT_CODE, getParameter(QUERY_STRING_PARAM));
                break;

            default:
                // No additional criteria (this is the case for "All")
                break;
        }

        criteria.addEqualTo(SchoolCourse.REL_COURSE + "." + Course.COL_DISTRICT_CONTEXT_OID,
                m_schedule.getDistrictContextOid());
        criteria.addEqualTo(SchoolCourse.COL_SCHOOL_OID, m_schedule.getSchoolOid());

        QueryByCriteria query = new QueryByCriteria(SchoolCourse.class, criteria);
        query.addOrderByAscending(SchoolCourse.COL_NUMBER);

        return query;
    }

    /**
     * Returns the potential conflicts for the passed school course OID.
     *
     * @param schoolCourseOid String
     * @return ResultSet
     * @throws SQLException exception
     */
    private ResultSet getConflicts(String schoolCourseOid) throws SQLException {
        m_conflictStatement.clearParameters();
        m_conflictStatement.setString(1, schoolCourseOid);

        return m_conflictStatement.executeQuery();
    }

    /**
     * Performs the following preparation steps:
     * <ul>
     * <li>Prepares the statement for the request conflict query.
     * <li>Loads the course lookup map.
     * <li>Loads the request counts map.
     * </ul>
     *
     * @throws SQLException exception
     */
    private void prepare() throws SQLException {
        // Conflict query statement
        String conflictSql = "SELECT a.REQ_CSK_OID as PRIMARY_COURSE, " +
                "       b.REQ_CSK_OID as CONFLICT_COURSE, " +
                "       max(CSK_COURSE_NUMBER) as CSK_COURSE_NUMBER, " + "       count(*) as CONFLICTS " +
                "  FROM STUDENT_COURSE_REQUEST a " +
                " INNER JOIN STUDENT_COURSE_REQUEST b " +
                "    ON a.REQ_STD_OID = b.REQ_STD_OID " +
                " INNER JOIN COURSE_SCHOOL " +
                "    ON b.REQ_CSK_OID = CSK_OID " +
                " WHERE a.REQ_CSK_OID = ? " +
                " AND a.REQ_CTX_OID = '" + m_schedule.getDistrictContextOid() + "' " +
                " AND a.REQ_SKL_OID = '" + m_schedule.getSchoolOid() + "' " +
                " AND a.REQ_ALTERNATE_IND = '0' " +
                " AND b.REQ_ALTERNATE_IND = '0' " +
                " GROUP BY a.REQ_CSK_OID, b.REQ_CSK_OID " +
                " ORDER BY CSK_COURSE_NUMBER ";

        m_conflictStatement = m_conflictsConnection.prepareStatement(conflictSql,
                ResultSet.TYPE_FORWARD_ONLY,
                ResultSet.CONCUR_READ_ONLY);

        // Course map
        Criteria courseCriteria = new Criteria();
        courseCriteria.addEqualTo(SchoolCourse.REL_COURSE + "." + Course.COL_DISTRICT_CONTEXT_OID,
                m_schedule.getDistrictContextOid());
        courseCriteria.addEqualTo(SchoolCourse.COL_SCHOOL_OID, m_schedule.getSchoolOid());

        QueryByCriteria query = new QueryByCriteria(SchoolCourse.class, courseCriteria);

        m_courseMap = getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 1000);

        // Request counts map
        m_requestCountMap = new HashMap(1000);

        String requestSql = "SELECT REQ_CSK_OID, count(*) as REQUESTS " +
                "  FROM STUDENT_COURSE_REQUEST " +
                " WHERE REQ_CTX_OID = '" + m_schedule.getDistrictContextOid() + "' " +
                " AND REQ_SKL_OID = '" + m_schedule.getSchoolOid() + "' " +
                " AND REQ_ALTERNATE_IND = '0' " +
                " GROUP BY REQ_CSK_OID";

        Statement statement = m_conflictsConnection.createStatement(ResultSet.TYPE_FORWARD_ONLY,
                ResultSet.CONCUR_READ_ONLY);
        ResultSet counts = null;
        try {
            counts = statement.executeQuery(requestSql);
            while (counts.next()) {
                m_requestCountMap.put(counts.getString("REQ_CSK_OID"),
                        Integer.valueOf(counts.getInt("REQUESTS")));
            }
        } finally {
            if (counts != null) {
                counts.close();
            }
            statement.close();
        }

    }
}
