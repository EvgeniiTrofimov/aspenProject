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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ScheduleBell;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Student Schedule Summary" report.
 *
 * @author X2 Development Corporation
 */
public class StudentScheduleSummaryData extends ReportJavaSourceNet {
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

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    // Grid fields
    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String FIELD_STUDENT_OID = "studentOid";
    private static final String FIELD_STUDENT_YOG = "studentYOG";
    private static final String FIELD_PRIMARY_TOTAL = "totalPrimary";
    private static final String FIELD_ALTERNATE_TOTAL = "totalAlternate";
    private static final String FIELD_PRIMARY_DROPPED = "primaryDropped";
    private static final String FIELD_ALTERNATE_IN = "alternateIncluded";
    private static final String FIELD_TOTAL_CREDITS = "totalCredits";
    private static final String FIELD_PERCENT_SCHEDULED = "percentScheduled";

    private Connection m_connection;
    private ReportDataGrid m_grid;
    private ScheduleReportHelper m_reportHelper;
    private ScheduleManager m_scheduleManager;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Map masterMap = null;
        Map studentMap = null;

        try {
            m_connection = getBroker().borrowConnection();
            loadRequests();
            masterMap = getMasterMap();
        } finally {
            getBroker().returnConnection();
        }

        QueryIterator requestIterator = null;

        try {
            String currentStudent;

            Integer primaryTotal;
            Integer primaryScheduled;
            Integer alternateScheduled;

            while (m_grid.next() && masterMap != null) {
                currentStudent = (String) m_grid.get(FIELD_STUDENT_OID);
                primaryTotal = (Integer) m_grid.get(FIELD_PRIMARY_TOTAL);

                studentMap = (Map) masterMap.get(currentStudent);
                if (studentMap == null) {
                    primaryScheduled = Integer.valueOf(0);
                    alternateScheduled = Integer.valueOf(0);
                } else {
                    primaryScheduled = (Integer) studentMap.get(FIELD_PRIMARY_TOTAL);
                    alternateScheduled = (Integer) studentMap.get(FIELD_ALTERNATE_TOTAL);
                }

                int primaryDropped = primaryTotal.intValue() - primaryScheduled.intValue();
                m_grid.set(FIELD_PRIMARY_DROPPED, Integer.valueOf(primaryDropped));
                m_grid.set(FIELD_ALTERNATE_IN, alternateScheduled);
                Double[] calcValues = calculateValues(currentStudent);

                m_grid.set(FIELD_TOTAL_CREDITS, new BigDecimal(calcValues[0].doubleValue()));
                m_grid.set(FIELD_PERCENT_SCHEDULED, calcValues[1]);
            }
        } finally {
            if (requestIterator != null) {
                requestIterator.close();
            }
        }

        m_grid.beforeTop();
        return m_grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_scheduleManager = new ScheduleManager(getBroker());
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

    /**
     * Calculates the scheduled bits percentage.
     *
     * @param studentOid String
     * @return percent scheduled
     */
    private Double[] calculateValues(String studentOid) {
        Collection studentSchedules;
        Double[] calculatedValues = new Double[2];
        SisStudent student = (SisStudent) getBroker().getBeanByOid(SisStudent.class, studentOid);
        studentSchedules = m_scheduleManager.getStudentSchedule(m_reportHelper.getSectionClass(), student.getOid(),
                m_reportHelper.getScheduleOid(), null);

        ScheduleBell bellSchedule = m_scheduleManager.getStudentBellSchedule(studentOid, m_reportHelper.getSchedule());
        int periods = m_scheduleManager.getMaxNumberOfPeriodsCanBeUsed(m_reportHelper.getSchedule(), bellSchedule);

        int bitsScheduledPercentage = 0;
        if (m_reportHelper.getSchedule() != null) {
            /*
             * Term by code map.
             */
            X2Criteria termCriteria = new X2Criteria();
            termCriteria.addEqualTo(ScheduleTerm.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

            QueryByCriteria termQuery = new QueryByCriteria(ScheduleTerm.class, termCriteria);
            Map<String, ScheduleTerm> termByCode = getBroker().getMapByQuery(termQuery, ScheduleTerm.COL_CODE, 10);

            calculatedValues[0] =
                    Double.valueOf(m_scheduleManager.calculateCreditsForSections(studentSchedules, termByCode));

            int totalBitsInSchedule =
                    m_reportHelper.getSchedule().getDays() * periods * m_reportHelper.getSchedule().getTerms();
            if (totalBitsInSchedule != 0) {
                int[] bitsScheduled =
                        m_scheduleManager.getScheduledBits(studentSchedules, m_reportHelper.getSectionClass());
                bitsScheduledPercentage =
                        (bitsScheduled[0] + bitsScheduled[1] + bitsScheduled[2]) * 100 / totalBitsInSchedule;
            }
            calculatedValues[1] = Double.valueOf(bitsScheduledPercentage);
        }

        return calculatedValues;
    }

    /**
     * Gets all the requests that are in the workspace schedule. Maps a student OID to a map that
     * tracks total credits, percent scheduled, # of primary requests that have been scheduled, and
     * # of alternate requests that have been scheduled.
     *
     * @return Map
     */
    private Map getMasterMap() {
        String studentCriteria = "";
        HashMap requestsFilled = null;
        HashMap students = null;

        PreparedStatement statement = null;
        ResultSet results = null;

        if (m_reportHelper.getStudentOid() != null) {
            studentCriteria = " and STD_OID = '" + m_reportHelper.getStudentOid() + "' ";
        } else {
            studentCriteria = "";
        }

        try {
            String sectionCourseOidColumn =
                    m_reportHelper.getSectionClass().equals(MasterSchedule.class) ? "MST_CSK_OID" : "BLM_CSK_OID";

            String sectionOidColumn =
                    m_reportHelper.getSectionClass().equals(MasterSchedule.class) ? "MST_OID" : "BLM_OID";

            String studentScheduleSectionOidColumn =
                    m_reportHelper.getSectionClass().equals(MasterSchedule.class) ? "SSC_MST_OID" : "BLS_BLM_OID";

            String studentScheduleScheduleOidColumn =
                    m_reportHelper.getSectionClass().equals(MasterSchedule.class) ? "SSC_SCH_OID" : "BLS_SCH_OID";

            String studentScheduleStudentOidColumn =
                    m_reportHelper.getSectionClass().equals(MasterSchedule.class) ? "SSC_STD_OID" : "BLS_STD_OID";

            String sectionTable = m_reportHelper.getSectionClass().equals(MasterSchedule.class) ? "SCHEDULE_MASTER"
                    : "SCHEDULE_BUILD_MASTER";

            String studentScheduleTable = m_reportHelper.getSectionClass().equals(MasterSchedule.class)
                    ? "STUDENT_SCHEDULE" : "SCHEDULE_BUILD_STUDENT";

            String sql;
            sql = "SELECT STD_OID, REQ_ALTERNATE_IND, CSK_CREDIT, CSS_BASE_TERMS_PER_YEAR, " +
                    "CSS_COVERED_TERM_PER_YEAR, CSS_PERIODS_PER_CYCLE " +
                    "FROM STUDENT_COURSE_REQUEST " +
                    "INNER JOIN STUDENT ON REQ_STD_OID = STD_OID " +
                    "INNER JOIN COURSE_SCHOOL ON CSK_OID = REQ_CSK_OID " +
                    "INNER JOIN COURSE_SCHEDULE ON CSK_OID = CSS_CSK_OID " +
                    "WHERE REQ_CTX_OID = ? " +
                    "  AND REQ_SKL_OID = ? " +
                    "  AND CSS_SCH_OID = ? " +
                    "  AND REQ_CSK_OID IN " +
                    "               (SELECT " + sectionCourseOidColumn +
                    "                FROM " + sectionTable +
                    "                  INNER JOIN " + studentScheduleTable + " ON " + studentScheduleSectionOidColumn + " = " + sectionOidColumn +
                    "                WHERE " + studentScheduleScheduleOidColumn + " = ? " +
                    "                  AND REQ_STD_OID = " + studentScheduleStudentOidColumn + " ) " +
                    studentCriteria + " " +
                    "ORDER BY STD_NAME_VIEW ";

            m_connection = getBroker().borrowConnection();
            statement = m_connection.prepareStatement(sql);

            statement.setString(1, m_reportHelper.getSchedule().getDistrictContextOid());
            statement.setString(2, getSchool().getOid());
            statement.setString(3, m_reportHelper.getCourseScheduleOid());
            statement.setString(4, m_reportHelper.getScheduleOid());

            results = statement.executeQuery();

            students = new HashMap(50);

            requestsFilled = null;
            String lastOid = null;
            String currentOid = null;
            String alternateInd;

            int primaryCount = 0;
            int alternateCount = 0;

            // Iterate over results counting alternates & primaries, summing credits and %scheduled.
            while (results.next()) {
                currentOid = results.getString("STD_OID");
                if (lastOid == null || !currentOid.equals(lastOid)) {
                    if (lastOid != null) {
                        students.put(lastOid, requestsFilled);
                    }

                    requestsFilled = new HashMap(50);
                    requestsFilled.put(FIELD_PRIMARY_TOTAL, Integer.valueOf(0));
                    requestsFilled.put(FIELD_ALTERNATE_TOTAL, Integer.valueOf(0));
                }

                alternateInd = results.getString("REQ_ALTERNATE_IND");
                if (alternateInd.equals("1")) {
                    alternateCount = ((Integer) requestsFilled.get(FIELD_ALTERNATE_TOTAL)).intValue();
                    alternateCount++;
                    requestsFilled.put(FIELD_ALTERNATE_TOTAL, Integer.valueOf(alternateCount));
                } else {
                    primaryCount = ((Integer) requestsFilled.get(FIELD_PRIMARY_TOTAL)).intValue();
                    primaryCount++;
                    requestsFilled.put(FIELD_PRIMARY_TOTAL, Integer.valueOf(primaryCount));
                }

                lastOid = currentOid;
            }

            students.put(lastOid, requestsFilled);
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        } finally {
            if (results != null) {
                try {
                    results.close();
                } catch (SQLException sqle) {
                    // Does nothing
                }
            }

            if (statement != null) {
                try {
                    statement.close();
                } catch (SQLException sqle) {
                    // Does nothing
                }
            }

            getBroker().returnConnection();
        }

        return students;
    }

    /**
     * Builds the criteria and query to get the Course Requests.
     *
     * @return QueryByCriteria
     */
    private void loadRequests() {
        m_grid = new ReportDataGrid(2000, 10);

        PreparedStatement statement = null;
        ResultSet results = null;

        String requestCriteria = " ";
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                requestCriteria = " AND STD_YOG = " + getParameter(QUERY_STRING_PARAM) + " ";
                break;

            case 2: // Homeroom
                requestCriteria = " AND STD_HOMEROOM = '" + getParameter(QUERY_STRING_PARAM) + "' ";
                break;

            case 3: // Counselor
                DataDictionaryField counselorField = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey())
                        .findDataDictionaryFieldByAlias(Student.ALIAS_COUNSELOR);
                if (counselorField != null) {
                    requestCriteria = " AND " + counselorField.getDatabaseName() + " = '"
                            + getParameter(QUERY_STRING_PARAM) + "' ";
                }
                break;
            default:
                requestCriteria = " ";
                break;
        }

        String order;
        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        switch (sort) {
            case 0: // Name
                order = " ORDER BY STD_NAME_VIEW ";
                break;

            case 1: // YOG
                order = " ORDER BY STD_YOG, STD_NAME_VIEW ";
                break;

            case 2: // Homeroom
                order = " ORDER BY STD_HOMEROOM, STD_NAME_VIEW ";
                break;

            default:
                order = " ";
                break;
        }

        String sql = "SELECT STD_OID, STD_NAME_VIEW, STD_YOG, REQ_ALTERNATE_IND, COUNT(*) AS TALLY " +
                "FROM STUDENT LEFT OUTER JOIN STUDENT_COURSE_REQUEST ON STD_OID = REQ_STD_OID " +
                "WHERE REQ_CTX_OID = ? " +
                "  AND STD_OID IN (SELECT SSA_STD_OID FROM STUDENT_SCHEDULE_ATTRIBUTES " +
                "                WHERE SSA_SCHD_INCLUDE_IND = '1' " +
                "                  AND SSA_SCH_OID = ? ) " +
                "  AND REQ_CSK_OID IN (SELECT CSS_CSK_OID FROM COURSE_SCHEDULE, COURSE_SCHOOL " +
                "                     WHERE CSS_CSK_OID = CSK_OID " +
                "                     AND CSS_SCHD_INCLUDE_IND = '1' " +
                "                     AND CSS_SCH_OID = ? " +
                "                     AND CSK_SKL_OID = ?) " +
                requestCriteria +
                "GROUP BY STD_OID, STD_NAME_VIEW, STD_YOG, REQ_ALTERNATE_IND " +
                order;

        try {
            m_connection = getBroker().borrowConnection();
            statement = m_connection.prepareStatement(sql);

            statement.setString(1, m_reportHelper.getSchedule().getDistrictContextOid());
            statement.setString(2, m_reportHelper.getStudentScheduleOid());
            statement.setString(3, m_reportHelper.getCourseScheduleOid());
            statement.setString(4, getSchool().getOid());

            results = statement.executeQuery();

            String lastOid = null;
            String currentOid;

            // Iterate through results, setting up student's information
            while (results.next()) {
                currentOid = results.getString("STD_OID");
                if (lastOid == null || !currentOid.equals(lastOid)) {
                    m_grid.append();
                    m_grid.set(FIELD_STUDENT_OID, results.getString("STD_OID"));
                    m_grid.set(FIELD_STUDENT_NAME, results.getString("STD_NAME_VIEW"));
                    m_grid.set(FIELD_STUDENT_YOG, Integer.valueOf(results.getInt("STD_YOG")));
                    m_grid.set(FIELD_ALTERNATE_TOTAL, Integer.valueOf(0));
                    m_grid.set(FIELD_PRIMARY_TOTAL, Integer.valueOf(0));
                }

                if (results.getString("REQ_ALTERNATE_IND").equals("1")) {
                    m_grid.set(FIELD_ALTERNATE_TOTAL, Integer.valueOf(results.getInt("tally")));
                } else if (results.getString("REQ_ALTERNATE_IND").equals("0")) {
                    m_grid.set(FIELD_PRIMARY_TOTAL, Integer.valueOf(results.getInt("tally")));
                }

                lastOid = currentOid;
            }
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        } finally {
            if (results != null) {
                try {
                    results.close();
                } catch (SQLException sqle) {
                    // Does nothing
                }
            }

            if (statement != null) {
                try {
                    statement.close();
                } catch (SQLException sqle) {
                    // Does nothing
                }
            }

            getBroker().returnConnection();
        }

        m_grid.beforeTop();
    }
}
