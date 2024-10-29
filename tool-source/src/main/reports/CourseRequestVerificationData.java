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

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleBell;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.web.SisUserDataContainer;
import com.x2dev.utils.StringUtils;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Prepares the data for the following course reports:
 * <ul>
 * <li>Course Catalog
 * <li>Course Tally
 * </ul>
 * These reports simply select courses from the current school (with an optional criteria for
 * department) and order the results by course number or department.
 *
 * @author X2 Development Corporation
 */
public class CourseRequestVerificationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "scheduled only" report parameter. The value is an Boolean.
     */
    public static final String SCHEDULED_ONLY_PARAM = "scheduledStudentsOnly";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the enumerated "filter" report parameter. The value is an Integer.
     */
    public static final String FILTER_BY_PARAM = "filterBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    // student fields used in query and thus available in report
    public static final String STUDENT_FIELDS = "STD_NAME_VIEW, STD_YOG, STD_ID_LOCAL, ";

    // report param
    public static final String SCHOOL_YEAR_CONTEXT = "schoolYearContext";

    // Data grid columns
    private static final String COL_STUDENT_NAME = "studentName";
    private static final String COL_STUDENT_YOG = "studentYog";
    private static final String COL_STUDENT_ID = "studentId";
    private static final String COL_COURSE_REQUEST = "courseRequest"; // request column prefix
    private static final String COL_TOTAL_CREDITS = "totalCredits";
    private static final String COL_PERCENT_SCHEDULED = "percentScheduled";

    // Verification array indexes.
    public static final int STUDENT_IDX = 1;
    public static final int TOTAL_CREDITS_IDX = 19;
    public static final int PERCENT_SCHEDULED_IDX = 20;

    private String m_courseScheduleOid;
    private String m_currentSchoolOid;
    private String m_scheduleOid;
    private String m_studentScheduleOid;
    private int m_scheduleMatrixSize;
    private DistrictSchoolYearContext m_schoolYearContext;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Schedule schedule = (Schedule) getBroker().getBeanByOid(Schedule.class, m_scheduleOid);

        // Get the selection criteria from the user input
        String studentScheduledClause = "";
        boolean scheduledStudentsOnly = ((Boolean) getParameter(SCHEDULED_ONLY_PARAM)).booleanValue();
        if (scheduledStudentsOnly) {
            studentScheduledClause = "AND SSA_SCHD_INCLUDE_IND = '1' ";
        }

        // Get the selection criteria from the user input
        String studentWhereClause = "";
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 0: // All
                // No additional criteria (this is the case for "All")
                break;

            case 1: // YOG
                studentWhereClause = "AND STD_YOG = " + getParameter(QUERY_STRING_PARAM);
                break;

            case 2: // Guidance
                DataDictionaryField counselorField = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey())
                        .findDataDictionaryFieldByAlias(Student.ALIAS_COUNSELOR);
                if (counselorField != null) {
                    studentWhereClause =
                            "AND " + counselorField.getDatabaseName() + " = " + getParameter(QUERY_STRING_PARAM);
                }
                break;

            default:
                // No additional criteria (this is the case for "All")
                break;
        }

        // Get the sort order from the user input
        String orderByFields = "";
        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        switch (sort) {
            case 0: // YOG
                orderByFields = "STD_YOG, STD_NAME_VIEW, STD_OID, CSK_COURSE_NUMBER";
                break;

            case 1: // Name
                orderByFields = "STD_NAME_VIEW, STD_OID, CSK_COURSE_NUMBER";
                break;

            default:
                orderByFields = "STD_NAME_VIEW, STD_OID, CSK_COURSE_NUMBER";
                break;
        }

        StringBuilder sql = new StringBuilder(1200);
        sql.append("SELECT STD_OID, " + STUDENT_FIELDS);
        sql.append("       REQ_CSK_OID, ");
        sql.append("       CSK_COURSE_NUMBER, ");
        sql.append("       CSK_CREDIT, ");
        sql.append("       CSS_PERIODS_PER_CYCLE, CSS_BASE_TERMS_PER_YEAR, CSS_COVERED_TERM_PER_YEAR ");
        sql.append("  FROM STUDENT ");

        if (schedule != null && !schedule.useOptionalRequest()) {
            sql.append(
                    "  LEFT OUTER JOIN STUDENT_COURSE_REQUEST      ON REQ_STD_OID = STD_OID AND REQ_CTX_OID = ? AND REQ_ALTERNATE_IND = '0' AND REQ_OPTIONAL_IND = '0' ");
        } else {
            sql.append(
                    "  LEFT OUTER JOIN STUDENT_COURSE_REQUEST      ON REQ_STD_OID = STD_OID AND REQ_CTX_OID = ? AND REQ_ALTERNATE_IND = '0' ");
        }

        sql.append("  LEFT OUTER JOIN STUDENT_SCHEDULE_ATTRIBUTES ON SSA_STD_OID = STD_OID AND SSA_SCH_OID = ? ");
        sql.append("  LEFT OUTER JOIN COURSE_SCHOOL               ON REQ_CSK_OID = CSK_OID");
        sql.append(
                "  LEFT OUTER JOIN COURSE_SCHEDULE             ON CSS_CSK_OID = CSK_OID AND CSS_SCH_OID = ? AND CSS_SCHD_INCLUDE_IND = '1' ");
        sql.append(" WHERE STD_SKL_OID_NEXT = ? " + studentScheduledClause + studentWhereClause);
        sql.append(" ORDER BY " + orderByFields);

        Connection connection = getBroker().borrowConnection();
        PreparedStatement studentRequestStatement = null;
        ResultSet studentRequestResults = null;

        ReportDataGrid grid = new ReportDataGrid(1000, 20);
        try {
            studentRequestStatement = connection.prepareStatement(sql.toString());

            if (m_schoolYearContext == null) {
                // can happen if there is no active schedule
                studentRequestStatement.setString(1, null);
            } else {
                studentRequestStatement.setString(1, m_schoolYearContext.getOid());
            }
            studentRequestStatement.setString(2, m_studentScheduleOid);
            studentRequestStatement.setString(3, m_courseScheduleOid);
            studentRequestStatement.setString(4, m_currentSchoolOid);

            studentRequestResults = studentRequestStatement.executeQuery();

            String lastStudentOid = null;
            double totalCredit = 0;
            double percentScheduled = 0;
            int crsIndex = 0;

            int requestCount = 0;

            while (studentRequestResults.next()) {
                String studentOid = studentRequestResults.getString("STD_OID");
                if (lastStudentOid == null || !lastStudentOid.equals(studentOid)) {
                    if (lastStudentOid != null) {
                        saveGridRow(grid, totalCredit, percentScheduled, requestCount);
                    }
                    // initialize new student
                    String studentName = studentRequestResults.getString("STD_NAME_VIEW");
                    int studentYog = studentRequestResults.getInt("STD_YOG");
                    String studentId = studentRequestResults.getString("STD_ID_LOCAL");
                    grid.append();
                    crsIndex = 1;
                    grid.set(COL_STUDENT_NAME, studentName);
                    grid.set(COL_STUDENT_YOG, Integer.valueOf(studentYog));
                    grid.set(COL_STUDENT_ID, studentId);
                    lastStudentOid = studentOid;
                    totalCredit = 0;
                    percentScheduled = 0;
                    requestCount = 0;
                }

                String courseNumber = studentRequestResults.getString("CSK_COURSE_NUMBER");
                double courseCredit = studentRequestResults.getDouble("CSK_CREDIT");
                double ppc = studentRequestResults.getInt("CSS_PERIODS_PER_CYCLE");
                double btm = studentRequestResults.getInt("CSS_BASE_TERMS_PER_YEAR");
                double ctm = studentRequestResults.getInt("CSS_COVERED_TERM_PER_YEAR");

                // add the course to the grid
                String requestColumnName = COL_COURSE_REQUEST + StringUtils.padLeft(Integer.toString(crsIndex), 2, '0');
                grid.set(requestColumnName, courseNumber);
                crsIndex = crsIndex + 1;

                if (courseNumber != null) {
                    requestCount++;
                }

                // accummulate the credits and percent scheduled
                double coursePercent = 0.0;
                if (m_scheduleMatrixSize != 0 && btm != 0) {
                    coursePercent = (ppc / m_scheduleMatrixSize) * (ctm / btm);
                }
                percentScheduled = percentScheduled + coursePercent;
                totalCredit = totalCredit + courseCredit;
            }

            if (lastStudentOid != null) {
                saveGridRow(grid, totalCredit, percentScheduled, requestCount);
            }
            studentRequestStatement.close();
            studentRequestResults.close();
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        } finally {
            getBroker().returnConnection();
        }

        grid.beforeTop();

        addParameter(SCHOOL_YEAR_CONTEXT, m_schoolYearContext);

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     *
     *      TODO: query the associated period table to determine the periods actually scheduled
     *      in order to calculate the schedule matrix size
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_currentSchoolOid = getSchool().getOid();
        m_schoolYearContext = getCurrentContext();
        int daysPerCycle = 0;
        int periodsPerDay = 0;

        if (userData.getSessionNavConfig().getApplicationContext() == ApplicationContext.BUILD) {
            Schedule buildSchedule = ((SisUserDataContainer) userData).getBuildSchedule();
            if (buildSchedule != null) {
                m_scheduleOid = buildSchedule.getOid();
                m_studentScheduleOid = buildSchedule.getStudentScheduleOid();
                m_courseScheduleOid = buildSchedule.getCourseScheduleOid();
                m_schoolYearContext = buildSchedule.getDistrictContext();
                daysPerCycle = buildSchedule.getDays();
                periodsPerDay = buildSchedule.getPeriods();
            }
        } else {
            m_scheduleOid = ((SisSchool) getSchool()).getActiveScheduleOid();
            if (m_scheduleOid == null) {
                m_schoolYearContext = null;
            } else {
                m_schoolYearContext = ((SisSchool) getSchool()).getActiveSchedule().getDistrictContext();
                daysPerCycle = ((SisSchool) getSchool()).getActiveSchedule().getDays();
                periodsPerDay = ((SisSchool) getSchool()).getActiveSchedule().getPeriods();
            }
        }

        /*
         * If bell schedule is used, retrieve the correct number of periods per day.
         */
        Schedule schedule = (Schedule) getBroker().getBeanByOid(Schedule.class, m_scheduleOid);
        if (schedule != null) {
            ScheduleBell bellSchedule = null;
            if (schedule.useBellSchedule() && !schedule.getScheduleBells().isEmpty()) {
                bellSchedule = schedule.getScheduleBells().iterator().next();
            }
            ScheduleManager manager = new ScheduleManager(getBroker());
            periodsPerDay = manager.getMaxNumberOfPeriodsCanBeUsed(schedule, bellSchedule);
        }

        m_scheduleMatrixSize = daysPerCycle * periodsPerDay;
    }

    /**
     * Saves the grid row and updates the total credit and percent scheduled counts if the current
     * record meets the criteria. Otherwise the student is removed from the grid.
     *
     * @param grid ReportDataGrid
     * @param totalCredit double
     * @param percentScheduled double
     * @param requestCount int
     */
    private void saveGridRow(ReportDataGrid grid, double totalCredit, double percentScheduled, int requestCount) {
        BigDecimal roundedPercentScheduled =
                new BigDecimal(String.valueOf(percentScheduled * 100)).setScale(2, RoundingMode.HALF_UP);

        boolean drop = false;

        switch (((Integer) getParameter(FILTER_BY_PARAM)).intValue()) {
            case 0: // All
                // No additional criteria (this is the case for "All")
                break;

            case 1: // None
                if (requestCount != 0) {
                    drop = true;
                }
                break;

            case 2: // Over
                if (roundedPercentScheduled.doubleValue() <= 100.00) {
                    drop = true;
                }
                break;

            case 3: // Under
                if (roundedPercentScheduled.doubleValue() >= 100.00) {
                    drop = true;
                }
                break;

            default:
                // No additional criteria (this is the case for "All")
                break;
        }

        if (drop) {
            grid.deleteRow();
        } else {
            // Load the totals columns for the student
            grid.set(COL_TOTAL_CREDITS, Double.valueOf(totalCredit));
            grid.set(COL_PERCENT_SCHEDULED, Double.valueOf(roundedPercentScheduled.doubleValue()));
        }
    }
}
