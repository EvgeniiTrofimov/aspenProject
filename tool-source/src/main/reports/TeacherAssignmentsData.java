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

import com.follett.fsc.core.framework.persistence.DatabaseOptimizer;
import com.follett.fsc.core.framework.persistence.DatabaseOptimizerFactory;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourseSchedule;
import com.x2dev.sis.web.SisUserDataContainer;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the "Teacher Assignments" report. This report displays each teacher (sorted by
 * department) and the sections to which they are assigned. This report is based on the Build Master
 * Schedule (BLM) table.
 *
 * @author X2 Development Corporation
 */
public class TeacherAssignmentsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "schedule percentages" report parameter. The value is a Map of Staff OIDs
     * (String objects) to schedule percentages (Double objects).
     */
    public static final String PERCENTAGE_MAP_PARAM = "percentages";

    /**
     * Name for the "prescheduled terms only" report parameter. The value is a Boolean.
     */
    public static final String PRESCHEDULED_TERMS_ONLY_PARAM = "prescheduledTermsOnly";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    private Schedule m_schedule;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(1000, 2);

        Map courseAttributes = getCourseAttributeMap();

        Connection connection = getBroker().borrowConnection();
        Statement statement = null;
        ResultSet results = null;

        Map percentages = new HashMap();

        try {
            statement = connection.createStatement();
            results = statement.executeQuery(buildSql());

            int columns = results.getMetaData().getColumnCount();

            int scheduleMatrixSize = m_schedule.getDays() * m_schedule.getPeriods();
            while (results.next()) {
                /*
                 * Populate the grid
                 */
                grid.append();

                for (int i = 1; i <= columns; i++) {
                    grid.set(results.getMetaData().getColumnName(i), results.getObject(i));
                }

                /*
                 * Calculate the schedule percentage for the staff member. This value gets populated
                 * incrementally for each course the staff member is teaching.
                 */
                String schoolCourseOid = results.getString("CSK_OID");
                String staffOid = results.getString("STF_OID");
                int sectionCount = results.getInt("TALLY");

                SchoolCourseSchedule attributes =
                        (SchoolCourseSchedule) courseAttributes.get(schoolCourseOid);

                if (attributes != null) {
                    if (scheduleMatrixSize > 0 &&
                            attributes.getPeriodsPerCycle() != null &&
                            attributes.getBaseTermsPerYear() > 0) {
                        double coursePercent = (((attributes.getPeriodsPerCycle().doubleValue() / scheduleMatrixSize) *
                                ((double) attributes.getCoveredTermsPerYear()
                                        / (double) attributes.getBaseTermsPerYear()))
                                * sectionCount) * 100;

                        Double teacherPercent = (Double) percentages.get(staffOid);
                        if (teacherPercent == null) {
                            teacherPercent = Double.valueOf(0);
                        }

                        teacherPercent = Double.valueOf(teacherPercent.doubleValue() + coursePercent);
                        percentages.put(staffOid, teacherPercent);
                    }
                }
            }

            statement.close();
            results.close();
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        } finally {
            getBroker().returnConnection();
        }

        addParameter(PERCENTAGE_MAP_PARAM, percentages);

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
        m_schedule = ((SisUserDataContainer) userData).getBuildSchedule();
    }

    /**
     * Returns the SELECT string used to query the SCHEDULE_BUILD_MASTER table.
     *
     * @return String
     */
    private String buildSql() {
        StringBuilder sql = new StringBuilder(256);

        /*
         * Determine if only pre-schedule terms should be considered.
         */
        String selectFields = "STF_DEPARTMENT_CODE, STF_NAME_VIEW, CSK_COURSE_NUMBER, STF_OID, " +
                "CSK_OID, BLM_SECTION_TYPE_CODE, CSK_COURSE_DESCRIPTION, BLM_TEAM_CODE";
        String groupFields = selectFields + ", BLM_TERM_VIEW";

        DatabaseOptimizer optimizer = DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey());
        boolean prescheduledTermsOnly =
                ((Boolean) getParameter(PRESCHEDULED_TERMS_ONLY_PARAM)).booleanValue();
        if (prescheduledTermsOnly) {
            selectFields = selectFields + ", " +
                    optimizer.sqlIf("BLM_PRESCHEDULED_TERM_IND = '1'", "BLM_TERM_VIEW", "''") + " AS BLM_TERM_VIEW ";
            groupFields += ", BLM_PRESCHEDULED_TERM_IND";
        } else {
            selectFields = groupFields;
        }

        /*
         * Build the SELECT string
         */
        sql.append("SELECT COUNT(*) AS TALLY, " + selectFields);
        sql.append("  FROM SCHEDULE_BUILD_MASTER INNER JOIN STAFF ON BLM_STF_OID_PRIMARY = STF_OID ");
        sql.append("                             INNER JOIN COURSE_SCHOOL ON CSK_OID = BLM_CSK_OID ");
        sql.append(" WHERE BLM_SCH_OID = '" + m_schedule.getOid() + "'");

        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // Team
                sql.append("AND BLM_TEAM_CODE = '" + getParameter(QUERY_STRING_PARAM) + "' ");
                break;

            case 2: // House
                sql.append("AND BLM_HOUSE_CODE = '" + getParameter(QUERY_STRING_PARAM) + "' ");
                break;

            case 3: // Platoon
                sql.append("AND BLM_PLATOON_CODE = '" + getParameter(QUERY_STRING_PARAM) + "' ");
                break;
        }

        sql.append("  GROUP BY " + groupFields);
        sql.append("  ORDER BY STF_DEPARTMENT_CODE, STF_NAME_VIEW, CSK_COURSE_NUMBER");

        return sql.toString();
    }

    /**
     * Builds and returns the course attributes map.
     *
     * @return A Map of SchoolCourse OIDs (String objects) to SchoolCourseSchedule beans
     */
    private Map getCourseAttributeMap() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SchoolCourseSchedule.COL_SCHEDULE_OID, m_schedule.getCourseScheduleOid());

        QueryByCriteria query = new QueryByCriteria(SchoolCourseSchedule.class, criteria);

        return getBroker().getMapByQuery(query, SchoolCourseSchedule.COL_SCHOOL_COURSE_OID, 256);
    }
}
