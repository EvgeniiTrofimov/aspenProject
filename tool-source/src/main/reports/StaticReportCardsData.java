/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.GradeTerm;
import com.x2dev.sis.model.beans.GradeTermDate;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.qlist.QualificationListManager;
import com.x2dev.sis.tools.reports.StaticTranscriptReportGrid;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for a "static" report card. All transcript data for a static report card is contained
 * in a single row in the report data grid. Transcript data is identified by course numbers on the
 * format.
 *
 * @author X2 Development Corporation
 */
public class StaticReportCardsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    // Input parameters
    public static final String CONTEXT_OID_PARAM = "contextOid";
    public static final String CONVERT_NUMERIC_PARAM = "convertNumeric";
    public static final String DATE_PARAM = "date";
    public static final String GRADE_TERM_OID_PARAM = "gradeTermOid";
    public static final String GRADE_TRANSFORM_PARAM = "gradeTransform";
    public static final String GRID_PARAM = "grid";
    public static final String HONOR_ROLL_COLUMN_PARAM = "honorRollColumn";
    public static final String QUERY_BY_PARAM = "queryBy";
    public static final String QUERY_STRING_PARAM = "queryString";
    public static final String STUDENT_SORT_PARAM = "studentSort";

    // Subreport constants
    private static final String ATTENDANCE_SUBREPORT_ID = "SYS-GRD-002-SUB1";
    private static final String SUBREPORT_ABSENT_FIELD = "absent";
    private static final String SUBREPORT_DISMISSED_FIELD = "dismissed";
    private static final String SUBREPORT_TARDY_FIELD = "tardy";
    private static final String SUBREPORT_TERM_FIELD = "term";

    private static final String SUBREPORT_BEAN = "subreport";
    private static final String SUBREPORT_DATASOURCES = "subreportDataSources";

    // Credit map constants
    private static final String ATTEMPTED_CREDITS_PARAM = "attemptedCredits";
    private static final String FAILED_CREDITS_PARAM = "failedCredits";
    private static final String PREVIOUS_CREDITS_PARAM = "previousCredits";

    // Report card message
    private static final String MESSAGE_PARAM = "message";

    // Honor roll map
    private static final String HONOR_ROLL_MAP_PARAM = "honorRollMap";

    // ID prefix for report card formats
    private static final String REPORT_ID_PREFIX = "SYS-GRD-002-T";

    private Report m_attendanceSubreport;
    private DistrictSchoolYearContext m_context;
    private Connection m_connection;
    private SisStudent m_currentStudent;
    private GradesManager m_gradesManager;
    private PreparedStatement m_previousCreditsQuery;
    private PlainDate m_reportDate;
    private PreparedStatement m_subreportAbsenceStatement;
    private HashMap m_subreportDataSources;
    private PreparedStatement m_subreportDismissalStatement;
    private PreparedStatement m_subreportTardyStatement;

    // Credit summary maps - all keyed on student OID
    private HashMap m_attemptedCredits;
    private HashMap m_previousCredits;
    private HashMap m_failedCredits;

    /**
     * Release resources.
     *
     * @see com.x2dev.sis.reporting.ReportDataSource#releaseResources()
     */
    @Override
    public void releaseResources() {
        super.releaseResources();

        // Release subreport resources
        Iterator studentOids = m_subreportDataSources.keySet().iterator();
        while (studentOids.hasNext()) {
            String studentOid = (String) studentOids.next();

            ReportDataGrid grid = (ReportDataGrid) m_subreportDataSources.get(studentOid);
            grid.clear();
            grid = null;
        }

        m_subreportDataSources.clear();
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.reporting.ReportDataSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        StaticTranscriptReportGrid grid = null;

        try {
            m_connection = getBroker().borrowConnection();
            prepareStatements();

            m_subreportDataSources = new HashMap();
            m_attendanceSubreport = getAttendanceSubreport();

            String contextOid = (String) getParameter(CONTEXT_OID_PARAM);
            m_context = (DistrictSchoolYearContext) getBroker().getBeanByOid(
                    DistrictSchoolYearContext.class, contextOid);

            m_reportDate = (PlainDate) getParameter(DATE_PARAM);

            m_gradesManager = new GradesManager(getBroker());

            m_previousCredits = new HashMap();
            m_failedCredits = new HashMap();
            m_attemptedCredits = new HashMap();

            /*
             * Create a criteria that will specify the transcript records to include in the
             * TranscriptReportGrid.
             */
            Criteria transcriptCriteria = new Criteria();

            if (m_currentStudent != null) {
                /*
                 * Running for one student
                 */
                transcriptCriteria.addEqualTo(Transcript.COL_STUDENT_OID, m_currentStudent.getOid());
            } else {
                /*
                 * Running for multiple students
                 */
                if (isSchoolContext()) {
                    transcriptCriteria.addEqualTo(Transcript.REL_STUDENT + "." +
                            SisStudent.COL_SCHOOL_OID, getSchool().getOid());
                } else {
                    transcriptCriteria.addAndCriteria(getOrganizationCriteria(Transcript.class));
                }

                /*
                 * Active students only
                 */
                transcriptCriteria.addAndCriteria(
                        StudentManager.getActiveStudentStatusCriteria(getOrganization(), Transcript.REL_STUDENT + "." +
                                SisStudent.COL_ENROLLMENT_STATUS));

                int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
                switch (queryBy) {
                    case 0:
                        String yogString = (String) getParameter(QUERY_STRING_PARAM);
                        if (StringUtils.isNumeric(yogString)) {
                            // YOG
                            Integer yog = Integer.valueOf(yogString);
                            transcriptCriteria.addEqualTo(Transcript.REL_STUDENT + "." +
                                    SisStudent.COL_YOG, yog);

                        } else { // force a "No Data" message
                            addNoMatchCriteria(transcriptCriteria);
                        }

                        break;

                    case 1: // Record Set
                        addRecordSetCriteria(transcriptCriteria);
                        break;

                    default:
                        // No additional criteria (this is the case for "All")
                        break;
                }
            }

            transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, m_context.getOid());

            String[] transcriptSortOrder = new String[] {Transcript.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_NUMBER};

            grid = new StaticTranscriptReportGrid(transcriptCriteria, getSortOrder(), transcriptSortOrder,
                    false, true, false, getOrganization(), getBroker());

            // Populate the attendance and credit structures
            SisStudent lastStudent = null;
            while (grid.next()) {
                SisStudent student = grid.getStudent();
                if (!student.equals(lastStudent)) {
                    addAttendanceData(student);
                    addCreditData(student, 1);
                }

                lastStudent = student;
            }

            // Calculate the honor roll
            addParameter(HONOR_ROLL_MAP_PARAM, calculateHonorRoll(transcriptCriteria));
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            try {
                m_subreportAbsenceStatement.close();
            } catch (SQLException sqle) {
                AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
            }

            try {
                m_subreportTardyStatement.close();
            } catch (SQLException sqle) {
                AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
            }

            try {
                m_subreportDismissalStatement.close();
            } catch (SQLException sqle) {
                AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
            }

            getBroker().returnConnection();

            if (grid != null) {
                grid.beforeTop();
            }
        }
        addParameter(SUBREPORT_DATASOURCES, m_subreportDataSources);
        addParameter(SUBREPORT_BEAN, m_attendanceSubreport);

        addParameter(ATTEMPTED_CREDITS_PARAM, m_attemptedCredits);
        addParameter(FAILED_CREDITS_PARAM, m_failedCredits);
        addParameter(PREVIOUS_CREDITS_PARAM, m_previousCredits);
        addParameter(GRID_PARAM, grid);

        String message = PreferenceManager.getPreferenceValue(getSchool(),
                SisPreferenceConstants.GRADES_REPORT_CARD_MESSAGE);

        addParameter(MESSAGE_PARAM, message);

        setReportFormat();

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see @see
     *      com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Adds an entry for the passed student to the attendance subreport data source map to provide
     * support for the attendance summary.
     *
     * <pre>
     * m_subReportDataSources
     * </pre>
     *
     * is populated with a
     * DataGrid for the student.
     *
     * @param student SisStudent
     * @throws SQLException exception
     */
    private void addAttendanceData(SisStudent student) throws SQLException {
        ReportDataGrid attendanceSummary = getAttendanceSummaryGrid(student);
        m_subreportDataSources.put(student.getOid(), attendanceSummary);
    }

    /**
     * Adds map entries to
     *
     * <pre>
     * m_previousCredits
     * </pre>
     *
     * ,
     *
     * <pre>
     * m_currentCredits
     * </pre>
     *
     * , and
     *
     * <pre>
     * m_failedCredits
     * </pre>
     *
     * for the passed student.
     *
     * @param student SisStudent
     * @param startYear the first year to consider when totalling previous year credits
     * @throws SQLException exception
     */
    private void addCreditData(SisStudent student, int startYear) throws SQLException {
        Double previous = Double.valueOf(0);

        m_previousCreditsQuery.setString(1, student.getOid());
        m_previousCreditsQuery.setString(2, m_context.getOid());
        m_previousCreditsQuery.setInt(3, startYear);
        ResultSet previousCredits = m_previousCreditsQuery.executeQuery();
        try {
            if (previousCredits.next()) {
                previous = Double.valueOf(previousCredits.getDouble(1));
            }
        } finally {
            previousCredits.close();
            m_previousCreditsQuery.clearParameters();
        }

        m_previousCredits.put(student.getOid(), previous);

        KeyValuePair attemptedFailed =
                m_gradesManager.getCreditsAttemptedAndFailed(student.getOid(), m_context.getOid());

        m_attemptedCredits.put(student.getOid(), attemptedFailed.getKey());
        m_failedCredits.put(student.getOid(), attemptedFailed.getValue());
    }

    /**
     * Adds criteria to filter the report selection by record set.
     *
     * @param transcriptCriteria Criteria
     */
    private void addRecordSetCriteria(Criteria transcriptCriteria) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                getParameter(QUERY_STRING_PARAM));

        transcriptCriteria.addIn(Transcript.COL_STUDENT_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Calculates the honor roll. A Map is returned containing an entry for each student that made
     * the honor roll; the key is student OID, and the value is the name of the honor roll for
     * which the student qualified (i.e. High Honors, Honors).
     *
     * @param transcriptCriteria Criteria
     * @return Map
     */
    private Map calculateHonorRoll(Criteria transcriptCriteria) {
        Map listMembership = new HashMap(0);

        String qlistCategory = PreferenceManager.getPreferenceValue(getSchool(),
                SisPreferenceConstants.GRADES_HONOR_ROLL_QUALIFICATION_LIST);
        String columnOid = (String) getParameter(HONOR_ROLL_COLUMN_PARAM);

        if (qlistCategory != null && columnOid != null) {
            // Query for the grade column to use
            TranscriptColumnDefinition transcriptColumn =
                    (TranscriptColumnDefinition) getBroker().getBeanByOid(TranscriptColumnDefinition.class, columnOid);

            if (transcriptColumn != null) {
                // Calculate honor roll using the qualification list manager
                listMembership = QualificationListManager.getListMembership(transcriptCriteria,
                        qlistCategory, transcriptColumn, getBroker());
            }
        }

        return listMembership;
    }

    /**
     * Returns a data grid containing the attendance summary for the passed student. The grid is in
     * the following format:
     *
     * term | abs | tdy | dsm
     * ----------+--------+---------+----------
     * GradeTerm | Double | Integer | Integer
     * ----------+--------+---------+----------
     * GradeTerm | Double | Integer | Integer
     * ----------+--------+---------+----------
     *
     * @param student SisStudent
     * @return Report data grid
     * @throws SQLException exception
     */
    private ReportDataGrid getAttendanceSummaryGrid(SisStudent student) throws SQLException {
        ReportDataGrid grid = new ReportDataGrid(4, 4);

        Criteria criteria = new Criteria();
        criteria.addGreaterOrEqualThan(GradeTermDate.COL_END_DATE, m_context.getStartDate());
        criteria.addLessOrEqualThan(GradeTermDate.COL_START_DATE, m_reportDate);
        criteria.addEqualTo(GradeTermDate.COL_DISTRICT_CONTEXT_OID, m_context.getOid());
        criteria.addEqualTo(GradeTermDate.COL_SCHOOL_OID, student.getSchoolOid());

        QueryByCriteria query = new QueryByCriteria(GradeTermDate.class, criteria);
        query.addOrderByAscending(GradeTermDate.COL_START_DATE);

        QueryIterator datesIterator = getBroker().getIteratorByQuery(query);
        try {
            while (datesIterator.hasNext()) {
                GradeTermDate termDate = (GradeTermDate) datesIterator.next();

                double absences = 0;
                int tardies = 0;
                int dismissals = 0;

                // Absences
                m_subreportAbsenceStatement.setString(1, student.getOid());
                m_subreportAbsenceStatement.setDate(2, termDate.getStartDate());
                m_subreportAbsenceStatement.setDate(3, termDate.getEndDate());

                ResultSet absentResults = m_subreportAbsenceStatement.executeQuery();
                try {
                    if (absentResults.next()) {
                        absences = absentResults.getDouble(1);
                    }
                } finally {
                    absentResults.close();
                    m_subreportAbsenceStatement.clearParameters();
                }

                // Tardies
                m_subreportTardyStatement.setString(1, student.getOid());
                m_subreportTardyStatement.setDate(2, termDate.getStartDate());
                m_subreportTardyStatement.setDate(3, termDate.getEndDate());

                ResultSet tardyResults = m_subreportTardyStatement.executeQuery();
                try {
                    if (tardyResults.next()) {
                        tardies = tardyResults.getInt(1);
                    }
                } finally {
                    tardyResults.close();
                    m_subreportTardyStatement.clearParameters();
                }

                // Dismissals
                m_subreportDismissalStatement.setString(1, student.getOid());
                m_subreportDismissalStatement.setDate(2, termDate.getStartDate());
                m_subreportDismissalStatement.setDate(3, termDate.getEndDate());

                ResultSet dismissalResults = m_subreportDismissalStatement.executeQuery();
                try {
                    if (dismissalResults.next()) {
                        dismissals = dismissalResults.getInt(1);
                    }
                } finally {
                    dismissalResults.close();
                    m_subreportDismissalStatement.clearParameters();
                }

                grid.append();
                grid.set(SUBREPORT_TERM_FIELD, termDate.getGradeTerm());
                grid.set(SUBREPORT_ABSENT_FIELD, Double.valueOf(absences));
                grid.set(SUBREPORT_TARDY_FIELD, Integer.valueOf(tardies));
                grid.set(SUBREPORT_DISMISSED_FIELD, Integer.valueOf(dismissals));
            }
        } finally {
            datesIterator.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Returns the array of student sort attributes.
     *
     * @return String[]
     */
    private String[] getSortOrder() {
        /*
         * Build the sort based on user input.
         */
        int sort = ((Integer) getParameter(STUDENT_SORT_PARAM)).intValue();
        String[] sortArray = null;
        switch (sort) {
            case 0: // Name view
                sortArray = new String[1];
                sortArray[0] = SisStudent.COL_NAME_VIEW;
                break;

            case 1: // YOG
                sortArray = new String[2];
                sortArray[0] = SisStudent.COL_YOG;
                sortArray[1] = SisStudent.COL_NAME_VIEW;
                break;

            case 2: // Homeroom
                sortArray = new String[2];
                sortArray[0] = SisStudent.COL_HOMEROOM;
                sortArray[1] = SisStudent.COL_NAME_VIEW;
                break;

            default: // No sort specified
                sortArray = new String[1];
                sortArray[0] = X2BaseBean.COL_OID;
                break;
        }

        return sortArray;
    }

    /**
     * Returns the attendance subreport bean.
     *
     * @return Report
     */
    private Report getAttendanceSubreport() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ID, ATTENDANCE_SUBREPORT_ID);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        return (Report) getBroker().getBeanByQuery(query);
    }

    /**
     * Initializes prepared statements used to query attendance & credit summary data.
     *
     * @throws SQLException exception
     */
    private void prepareStatements() throws SQLException {
        // Attendance query
        StringBuilder queryString = new StringBuilder(200);

        queryString.append("SELECT sum(att_portion_absent) as total ");
        queryString.append("FROM STUDENT_ATTENDANCE ");
        queryString.append("WHERE att_std_oid = ? ");
        queryString.append("AND att_date >= ? ");
        queryString.append("AND att_date <= ? ");
        queryString.append("AND att_absent_ind = '1' ");
        queryString.append("GROUP BY att_std_oid ");

        m_subreportAbsenceStatement = m_connection.prepareStatement(queryString.toString());

        // Tardy query
        queryString = new StringBuilder(200);

        queryString.append("SELECT count(*) as total ");
        queryString.append("FROM STUDENT_ATTENDANCE ");
        queryString.append("WHERE att_std_oid = ? ");
        queryString.append("AND att_date >= ? ");
        queryString.append("AND att_date <= ? ");
        queryString.append("AND att_tardy_ind = '1' ");
        queryString.append("GROUP BY att_std_oid ");

        m_subreportTardyStatement = m_connection.prepareStatement(queryString.toString());

        // Dismissal query
        queryString = new StringBuilder(200);

        queryString.append("SELECT count(*) as total ");
        queryString.append("FROM STUDENT_ATTENDANCE ");
        queryString.append("WHERE att_std_oid = ? ");
        queryString.append("AND att_date >= ? ");
        queryString.append("AND att_date <= ? ");
        queryString.append("AND att_dismissed_ind = '1' ");
        queryString.append("GROUP BY att_std_oid ");

        m_subreportDismissalStatement = m_connection.prepareStatement(queryString.toString());

        // Previous credits query
        queryString = new StringBuilder(300);

        queryString.append("SELECT sum(trn_total_credit) as total ");
        queryString.append("FROM STUDENT_TRANSCRIPT ");
        queryString.append("INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT ON trn_ctx_oid = ctx_oid ");
        queryString.append("WHERE trn_std_oid = ? ");
        queryString.append("AND ctx_school_year < ");
        queryString.append("(SELECT ctx_school_year ");
        queryString.append("FROM DISTRICT_SCHOOL_YEAR_CONTEXT ");
        queryString.append("WHERE ctx_oid = ?) ");
        queryString.append("AND ctx_school_year >= ? ");
        queryString.append("GROUP BY trn_std_oid ");

        m_previousCreditsQuery = m_connection.prepareStatement(queryString.toString());
    }

    /**
     * Sets the report format based on the selected grade term.
     */
    private void setReportFormat() {
        int termNumber = 1;

        String gradeTermOid = (String) getParameter(GRADE_TERM_OID_PARAM);
        if (!StringUtils.isEmpty(gradeTermOid)) {
            GradeTerm gradeTerm =
                    (GradeTerm) getBroker().getBeanByOid(GradeTerm.class, gradeTermOid);

            termNumber = gradeTerm.getGradeTermNum();
        }

        String reportId = REPORT_ID_PREFIX + termNumber;
        setFormatId(reportId);
    }
}
