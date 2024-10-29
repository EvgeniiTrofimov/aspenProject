/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolCourseSchedule;
import com.x2dev.sis.web.schedule.ScheduleUtils;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Course Tally report.
 *
 * @author X2 Development Corporation
 */
public class CourseTallyData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "calculate section counts" report parameter. The value is an Boolean.
     */
    public static final String CALCULATE_SECTION_COUNTS_PARAM = "calculateSectionCounts";

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "scheduled only" report parameter. The value is an Boolean.
     */
    public static final String SCHEDULED_ONLY_PARAM = "scheduledStudentsOnly";

    /**
     * Name for the "show inclusion counts" report parameter. The value is an Boolean.
     */
    public static final String SHOW_INCLUSION_COUNTS_PARAM = "showInclusionCounts";

    /**
     * Name for the enumerated "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    // Data grid columns
    private static final String COL_ALTERNATE_DISPLAY = "alternateDisplay";
    private static final String COL_ENROLLMENT_MAX = "enrollmentMax";
    private static final String COL_PRIMARY_DISPLAY = "primaryDisplay";
    private static final String COL_SCHOOL_COURSE = "schoolCourse";
    private static final String COL_SECION_COUNT = "sectionCount";
    private static final String COL_TOTAL_DISPLAY = "totalDisplay";

    // Tally array indexes.
    private static final int ALTERNATE_TOTAL_INDEX = 0;
    private static final int ALTERNATE_INCLUSION_INDEX = 1;
    private static final int PRIMARY_TOTAL_INDEX = 2;
    private static final int PRIMARY_INCLUSION_INDEX = 3;
    private static final int TALLY_ARRAY_SIZE = 4;

    private String m_courseScheduleOid;
    private String m_schoolYearContextOid;
    private String m_studentScheduleOid;
    private Boolean m_useOptionalRequests;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(1000, 5);

        Map courseTallies = calculateTallies();

        boolean showInclusionCounts =
                ((Boolean) getParameter(SHOW_INCLUSION_COUNTS_PARAM)).booleanValue();

        boolean calculateSectionCount =
                ((Boolean) getParameter(CALCULATE_SECTION_COUNTS_PARAM)).booleanValue();

        Criteria criteria = buildCriteria();
        QueryByCriteria query = new QueryByCriteria(SchoolCourse.class, criteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        Map<String, X2BaseBean> courseAttributeMap = loadCourseAttributeMap();

        QueryIterator courses = getBroker().getIteratorByQuery(query);
        try {
            while (courses.hasNext()) {
                SchoolCourse course = (SchoolCourse) courses.next();
                int[] tallies = (int[]) courseTallies.get(course.getOid());

                if (tallies != null) {
                    String alternateDisplay = String.valueOf(tallies[ALTERNATE_TOTAL_INDEX]);
                    String primaryDisplay = String.valueOf(tallies[PRIMARY_TOTAL_INDEX]);
                    String totalDisplay = String.valueOf(tallies[ALTERNATE_TOTAL_INDEX] + tallies[PRIMARY_TOTAL_INDEX]);

                    SchoolCourseSchedule attribute = (SchoolCourseSchedule) courseAttributeMap.get(course.getOid());
                    int enrollmentMax = 0;
                    double sectionCount = 0.00;
                    if (attribute != null) {
                        enrollmentMax = attribute.getEnrollmentMax();
                        sectionCount = attribute.getSections();
                        if (calculateSectionCount && enrollmentMax > 0) {
                            sectionCount = Double.valueOf(primaryDisplay).intValue() / (double) enrollmentMax;
                            BigDecimal count =
                                    new BigDecimal(String.valueOf(sectionCount)).setScale(2, RoundingMode.HALF_UP);
                            sectionCount = count.doubleValue();
                        }
                    }

                    if (showInclusionCounts) {
                        alternateDisplay = alternateDisplay + " (" + tallies[ALTERNATE_INCLUSION_INDEX] + ")";
                        primaryDisplay = primaryDisplay + " (" + tallies[PRIMARY_INCLUSION_INDEX] + ")";
                        totalDisplay = totalDisplay + " ("
                                + (tallies[ALTERNATE_INCLUSION_INDEX] + tallies[PRIMARY_INCLUSION_INDEX]) + ")";
                    }

                    grid.append();
                    grid.set(COL_SCHOOL_COURSE, course);
                    grid.set(COL_ALTERNATE_DISPLAY, alternateDisplay);
                    grid.set(COL_PRIMARY_DISPLAY, primaryDisplay);
                    grid.set(COL_TOTAL_DISPLAY, totalDisplay);
                    grid.set(COL_ENROLLMENT_MAX, String.valueOf(enrollmentMax));
                    grid.set(COL_SECION_COUNT, String.valueOf(sectionCount));
                }
            }
        } finally {
            courses.close();
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
        Schedule schedule = ScheduleUtils.getSchedule(userData);

        m_schoolYearContextOid = schedule.getDistrictContextOid();
        m_studentScheduleOid = schedule.getStudentScheduleOid();
        m_courseScheduleOid = schedule.getCourseScheduleOid();
        m_useOptionalRequests = schedule.useOptionalRequest();
    }

    /**
     * Returns the criteria for this report based on the input parameters. The criteria always
     * filters by courses for the selected school and school year context.
     *
     * @return Criteria a criteria rooted at SchoolCourse
     */
    private X2Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();

        addUserCriteria(criteria,
                (String) getParameter(QUERY_BY_PARAM),
                (String) getParameter(QUERY_STRING_PARAM),
                SchoolCourse.class,
                SchoolCourseSchedule.class,
                SchoolCourseSchedule.COL_SCHOOL_COURSE_OID,
                X2BaseBean.COL_OID);

        criteria.addEqualTo(SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
        criteria.addEqualTo(SchoolCourse.REL_COURSE + PATH_DELIMITER + Course.COL_DISTRICT_CONTEXT_OID,
                m_schoolYearContextOid);

        return criteria;
    }

    /**
     * Populates the passed HashMap with primary request tallies for each course.
     *
     * @return A Map of course OIDs (String objects) to tally counts (int[] objects)
     */
    private Map calculateTallies() {
        Map courseTallies = new HashMap(1024);

        boolean scheduledInclude = ((Boolean) getParameter(SCHEDULED_ONLY_PARAM)).booleanValue();
        String sqlScheduledClause = "";
        if (scheduledInclude) {
            sqlScheduledClause = " AND SSA_SCHD_INCLUDE_IND = '1' ";
        }
        String sqlOptionalRequests = "";
        if (!m_useOptionalRequests.booleanValue()) {
            sqlOptionalRequests = "   AND REQ_OPTIONAL_IND = '0'";
        }

        String sql = "SELECT REQ_CSK_OID, REQ_ALTERNATE_IND, REQ_INCLUSION_IND, COUNT(*) as TALLY" +
                " FROM STUDENT_COURSE_REQUEST LEFT OUTER JOIN STUDENT_SCHEDULE_ATTRIBUTES " +
                "                             ON REQ_STD_OID = SSA_STD_OID AND SSA_SCH_OID = ?" +
                " WHERE REQ_CTX_OID = ?" +
                "   AND REQ_SKL_OID = ?" + sqlScheduledClause + sqlOptionalRequests +
                " GROUP BY REQ_CSK_OID, REQ_ALTERNATE_IND, REQ_INCLUSION_IND " +
                " ORDER BY REQ_CSK_OID ";


        Connection connection = getBroker().borrowConnection();
        PreparedStatement statement = null;
        ResultSet results = null;

        try {
            statement = connection.prepareStatement(sql);
            statement.setString(1, m_studentScheduleOid);
            statement.setString(2, m_schoolYearContextOid);
            statement.setString(3, getSchool().getOid());

            results = statement.executeQuery();
            while (results.next()) {
                String courseOid = results.getString("REQ_CSK_OID");
                boolean isAlternate = results.getBoolean("REQ_ALTERNATE_IND");
                boolean isInclusion = results.getBoolean("REQ_INCLUSION_IND");
                int tally = results.getInt("TALLY");

                int[] tallies = (int[]) courseTallies.get(courseOid);
                if (tallies == null) {
                    tallies = new int[TALLY_ARRAY_SIZE];
                    tallies[ALTERNATE_INCLUSION_INDEX] = 0;
                    tallies[ALTERNATE_TOTAL_INDEX] = 0;
                    tallies[PRIMARY_INCLUSION_INDEX] = 0;
                    tallies[PRIMARY_TOTAL_INDEX] = 0;

                    courseTallies.put(courseOid, tallies);
                }

                /*
                 * A request is either a primary or an alternate, it cannot be both. However, both
                 * primary and alternate requests can be flagged for inclusion or not.
                 */
                if (isAlternate) {
                    tallies[ALTERNATE_TOTAL_INDEX] += tally;

                    if (isInclusion) {
                        tallies[ALTERNATE_INCLUSION_INDEX] += tally;
                    }
                } else {
                    tallies[PRIMARY_TOTAL_INDEX] += tally;

                    if (isInclusion) {
                        tallies[PRIMARY_INCLUSION_INDEX] += tally;
                    }
                }
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.SEVERE, sqle.getMessage(), sqle);
        } finally {
            try {
                if (results != null) {
                    results.close();
                }

                if (statement != null) {
                    statement.close();
                }
            } catch (SQLException sqle) {
                AppGlobals.getLog().log(Level.SEVERE, sqle.getMessage(), sqle);
            }

            getBroker().returnConnection();
        }

        return courseTallies;
    }

    /**
     * Loads the school course attributes map.
     *
     * @return Map key is SchoolCourse oid, value is the SchoolCourseAttribute bean
     */
    private Map<String, X2BaseBean> loadCourseAttributeMap() {
        X2Criteria courseCriteria = buildCriteria();

        X2Criteria attributeCriteria = new X2Criteria();
        attributeCriteria.addEqualTo(SchoolCourseSchedule.COL_SCHEDULE_OID, m_courseScheduleOid);
        attributeCriteria.addAndCriteria(courseCriteria.copyWithAdjustedPath(SchoolCourseSchedule.REL_SCHOOL_COURSE,
                SchoolCourseSchedule.COL_SCHOOL_COURSE_OID));

        QueryByCriteria attributeQuery = new QueryByCriteria(SchoolCourseSchedule.class, attributeCriteria);

        return getBroker().getMapByQuery(attributeQuery, SchoolCourseSchedule.COL_SCHOOL_COURSE_OID, 1000);
    }
}
