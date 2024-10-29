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

package com.x2dev.reports.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SecondaryStudentDataSource;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.tn.TNClassSectionHelper;
import com.x2dev.procedures.statereporting.tn.TNClassSectionScheduleData;
import com.x2dev.procedures.statereporting.tn.TNClassSectionScheduleData.TNClassSectionScheduleEntity;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentScheduleSpan;
import com.x2dev.procedures.statereporting.tn.TNStateReportData;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.springframework.util.CollectionUtils;

/**
 * Data source for the TN Student Detail report.
 *
 * @author X2 Development Corporation
 */
public class StudentDetailReportData extends SecondaryStudentDataSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * The Class EnrollmentStatistics.
     */
    class EnrollmentStatistics extends TNStateReportData {
        protected Map<String, ReferenceCode> m_referenceGradeCodeMap;

        protected TNEnrollmentHelper m_tnEnrHelper;

        private TNStudentHistoryHelper m_tnStudentHelper;
        private Map<String, Collection<SisStudent>> m_schoolStudents = new HashMap<String, Collection<SisStudent>>();
        private Map<String, ReferenceCode> m_studentGradeLvlMap = new HashMap<String, ReferenceCode>();

        /**
         * Close the student history helper.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#close()
         */
        @Override
        public void close() {
            super.close();
            if (m_tnStudentHelper != null) {
                m_tnStudentHelper.close();
            }
        }

        /**
         * Return calendar days.
         *
         * @param school SisSchool
         * @param calendar String
         * @return Set<PlainDate>
         */
        public Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
            return m_tnStudentHelper.getCalendarDays(school, calendar);
        }

        /**
         * Return student attendances.
         *
         * @param studentOid String
         * @return List<StudentAttendance>
         */
        public List<StudentAttendance> getStudentAttendances(String studentOid) {
            return m_tnStudentHelper.getStudentAttendances(studentOid);
        }

        /**
         * Return the current student criteria.
         *
         * @return X2Criteria
         */
        public X2Criteria getStudentCriteria() {
            return m_tnStudentHelper.getStudentCriteria();
        }

        /**
         * Returns a list of student enrollments.
         *
         * @param student SisStudent
         * @return List
         */
        public List<StudentEnrollment> getStudentEnrollment(SisStudent student) {
            return m_tnStudentHelper.getStudentEnrollments(student);
        }

        /**
         * Returns a list of student enrollment spans that represent all of the students enrollment
         * activity and segments.
         *
         * @param student Student
         * @param limit boolean
         * @return List<StudentEnrollmentSpan>
         */
        public List<TNStudentEnrollmentSpan> getStudentEnrollmentSpans(Student student, boolean limit) {
            return m_tnStudentHelper.getTNStudentEnrollmentSpans(student, limit);
        }

        /**
         * Return student's grade level.
         *
         * @param student SisStudent
         * @return ReferenceCode
         */
        public ReferenceCode getStudentGradeLvl(SisStudent student) {
            ReferenceCode gradeLvl = m_studentGradeLvlMap.get(student.getOid());
            if (gradeLvl == null) {
                gradeLvl =
                        m_tnStudentHelper.getGradeLevelByDates(student, m_schoolPeriodStartDate, m_schoolPeriodEndDate);
                m_studentGradeLvlMap.put(student.getOid(), gradeLvl);
            }

            return gradeLvl;
        }

        /**
         * Return student collection by school.
         *
         * @param schoolOid String
         * @return Collection
         */
        public Collection<SisStudent> getStudents(String schoolOid) {

            if (m_schoolStudents.get(schoolOid) == null || m_schoolStudents.get(schoolOid).isEmpty()) {
                X2Criteria studentCriteria = getStudentCriteria();
                QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
                applyInputSort(studentQuery, null);
                m_schoolStudents.put(schoolOid, getBroker().getCollectionByQuery(studentQuery));
            }

            Collection<SisStudent> students = m_schoolStudents.get(schoolOid);
            // exclude students:
            // 1) with excluded grade
            // 2) without spans
            // 3) If "Include all Students" is unchecked, leave students which enrolled on or after
            // period start date and withdrawn before or on period end date
            Iterator iterator = students.iterator();
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                Collection<TNStudentEnrollmentSpan> spans = m_data.getStudentEnrollmentSpans(student, true);
                if (excludeStudent(student, spans)) {
                    iterator.remove();
                } else {
                    if (!m_allStudents.booleanValue()) {
                        Iterator spanIterator = spans.iterator();

                        while (spanIterator.hasNext()) {
                            TNStudentEnrollmentSpan span = (TNStudentEnrollmentSpan) spanIterator.next();

                            if (span.getSchool() == null || !span.getSchool().getOid().equals(schoolOid)) {
                                spanIterator.remove();
                            } else if ((span.getLastActiveDate() != null
                                    && span.getLastActiveDate().before(m_schoolPeriodStartDate)) ||
                                    (span.getFirstActiveDate() != null
                                            && span.getFirstActiveDate().after(m_schoolPeriodEndDate))) {
                                spanIterator.remove();
                            } else if (span.getFirstActiveEnrollment() != null && StudentEnrollment.ENTRY
                                    .equals(span.getFirstActiveEnrollment().getEnrollmentType()) &&
                                    span.getFirstInactiveEnrollment() != null && StudentEnrollment.WITHDRAWAL
                                            .equals(span.getFirstInactiveEnrollment().getEnrollmentType())
                                    &&
                                    span.getFirstActiveEnrollment().getEnrollmentDate()
                                            .equals(span.getFirstInactiveEnrollment().getEnrollmentDate())) {
                                // Remove if first and last day are the same
                                spanIterator.remove();
                            }

                        }
                        if (spans.size() == 0) {
                            iterator.remove();
                        }
                    }
                }
            }

            return m_schoolStudents.get(schoolOid);
        }

        /**
         * Return student's sped status.
         *
         * @param student SisStudent
         * @return String
         */
        public String getStudentSpedStatus(SisStudent student) {
            return (String) m_tnEnrHelper.getStudentValueByBeanPath(student, SisStudent.COL_SPED_STATUS_CODE);
        }

        /**
         * Return the list of student schedule spans.
         *
         * @param student SisStudent
         * @return List<StudentScheduleSpan>
         */
        public List<TNStudentScheduleSpan> getTNStudentScheduleSpans(SisStudent student) {
            return m_tnStudentHelper.getTNStudentScheduleSpans(student);
        }

        /**
         * Initialize the export.
         * Set up the student history helper.
         *
         * @throws X2BaseException exception
         */
        @Override
        public void initialize() throws X2BaseException {
            super.initialize();
            // Set to include the student schedule history when getting the student schedule span
            m_tnEnrHelper = new TNEnrollmentHelper(this);
            m_tnStudentHelper = m_tnEnrHelper.getStudentHistoryHelper();
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);
            m_tnStudentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_schoolPeriodStartDate);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_schoolPeriodEndDate);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY,
                    Boolean.valueOf(includeSecondaryStudents()));

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.TRUE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_MATERIALIZED_STUDENT_SELECTION,
                    Boolean.TRUE);
        }

        /**
         * Materialize the contained history helper.
         *
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#materializeStudentCriteria()
         */
        public void materializeStudentCriteria() {
            m_tnStudentHelper.materializeStudentCriteria();
        }

        /**
         * Return true if the student should be excluded, otherwise false.
         *
         * @param student SisStudent
         * @param spans Collection<TNStudentEnrollmentSpan>
         * @return true, if successful
         */
        private boolean excludeStudent(SisStudent student, Collection<TNStudentEnrollmentSpan> spans) {
            boolean excludeStudent = true;

            ReferenceCode stdGradeLvl = getStudentGradeLvl(student);

            if (spans.size() != 0 && !m_excludedGrades.contains(stdGradeLvl.getCode())) {
                for (TNStudentEnrollmentSpan span : spans) {
                    if (span.getSchool().equals(getSchool())) {
                        excludeStudent = false;
                    }
                }
            }

            return excludeStudent;
        }
    }

    /**
     * The Class StudentSpanCount.
     */
    class StudentSpanCount {

        private int m_absenceMinutes = 0;
        private Map<PlainDate, Integer> m_dateScheduleMinutesMap = new HashMap<PlainDate, Integer>();
        private int m_membershipMinutes = 0;

        /**
         * Instantiates a new student span count.
         *
         * @param membershipMinutes int
         * @param absenceMinutes int
         * @param helper TNClassSectionScheduleEntity
         * @param school SisSchool
         */
        protected StudentSpanCount(int membershipMinutes, int absenceMinutes, TNClassSectionScheduleEntity helper,
                SisSchool school) {
            m_membershipMinutes = membershipMinutes;
            m_absenceMinutes = absenceMinutes;
            Collection<PlainDate> days = m_periodHelper.getDaysInSession(school.getOid());
            for (PlainDate day : days) {
                m_dateScheduleMinutesMap.put(day, Integer.valueOf(helper.getScheduledMinutes(day, 0, false)));
            }

        }

        /**
         * Adds the.
         *
         * @param count StudentSpanCount
         */
        protected void add(StudentSpanCount count) {
            m_membershipMinutes += count.getMembershipMinutes();
            m_absenceMinutes += count.getAbsenceMinutes();
            // Summarize minutes from the count by date.
            for (Entry dateMinutesEntry : m_dateScheduleMinutesMap.entrySet()) {
                Integer minutes = null;
                minutes = count.getScheduledMinutesMap().get(dateMinutesEntry.getKey());
                if (minutes != null) {
                    dateMinutesEntry.setValue(Integer.valueOf(
                            ((Integer) dateMinutesEntry.getValue()).intValue() + minutes.intValue()));
                }
            }
        }

        /**
         * Gets the absence minutes.
         *
         * @return int
         */
        protected int getAbsenceMinutes() {
            return m_absenceMinutes;
        }

        /**
         * Gets the membership minutes.
         *
         * @return int
         */
        protected int getMembershipMinutes() {
            return m_membershipMinutes;
        }

        /**
         * Gets the scheduled minutes.
         *
         * @param date PlainDate
         * @return int
         */
        protected int getScheduledMinutes(PlainDate date) {
            return m_dateScheduleMinutesMap.get(date).intValue();
        }

        /**
         * Gets the scheduled minutes map.
         *
         * @return Map
         */
        protected Map<PlainDate, Integer> getScheduledMinutesMap() {
            return m_dateScheduleMinutesMap;
        }
    }

    public static final String ALIAS_STUDENT_EIS_STATE_ID = "DOE EIS STATE ID";
    public static final String ALIAS_STUDENT_HISTORY_STANDARD_DAY = "all-rcd-StdStandardDayHistory";
    public static final String ALIAS_STUDENT_STANDARD_DAY = "DOE STUDENT STANDARD DAY";
    /**
     * School year context
     */
    public static final String CONTEXT_OID_PARAM = "contextOid";


    /**
     * First YOG in the YOG range
     */
    public static final String YOG_FIRST_PARAM = "firstYog";

    /**
     * Boolean for whether to display only the totals of the report.
     */
    public static final String TOTALS_ONLY_PARAM = "totalsOnly";

    List<String> m_excludedGrades;
    EnrollmentStatistics m_data = null;
    TNMultiYearHelper m_multiYearHelper;
    TNReportingPeriodHelper m_periodHelper = null;
    PlainDate m_schoolPeriodEndDate;
    PlainDate m_schoolPeriodStartDate;

    private static final String BEANPATH_FIELD_A033 = "fieldA033";

    // Attendance codes
    private static final String CODE_ATT_NOT_MEMB_DAY = "*";
    private static final String CODE_ABSENT = "A";
    private static final String CODE_TARDY = "T";
    private static final String CODE_DISMISSED = "D";

    private static final String COLUMN_ENROLLMENT_TYPE = "enrollment_";
    private static final String COLUMN_ERROR_MESSAGE = "error";
    private static final String COLUMN_REPORT_PERIOD = "reportPeriod";
    private static final String COLUMN_SCHOOL = "school";
    private static final String COLUMN_STUDENT = "student";
    private static final String COLUMN_STUDENT_CODE = "studentCode";
    private static final String COLUMN_STUDENT_GRADE = "gradeLevel";
    private static final String COLUMN_STUDENT_HOMEROOM = "homeroom";
    private static final String COLUMN_STUDENT_NAME = "studentName";
    private static final String COLUMN_STUDENT_OID = "studentOid";
    private static final String COLUMN_STUDENT_SPED_STATUS = "spedStatusCode";

    // Columns for period totals
    private static final String COLUMN_PERIOD_ABSENT = "pa";
    private static final String COLUMN_PERIOD_ADA = "ada";
    private static final String COLUMN_PERIOD_ADM = "adm";
    private static final String COLUMN_PERIOD_DAYS_IN_SESSION = "pds";
    private static final String COLUMN_PERIOD_DISMISSED = "pd";
    private static final String COLUMN_PERIOD_NONMEMBER = "pn";
    private static final String COLUMN_PERIOD_PRESENT = "pp";
    private static final String COLUMN_PERIOD_TARDY = "pt";

    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    // Columns for parameters used in the design
    private static final String PARAM_CURRENT_CONTEXT = "currentContext";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_EXCLUDED_GRADES = "excludedGrades";
    private static final String PARAM_INCLUDE_ALL_STUDENTS = "includeAllStudents";
    private static final String PARAM_INCLUDE_SCHEDULED_WITHDRAWN = "includeScheduledWithdrawn";
    private static final String PARAM_OVER_SCH_ONLY = "overUnderScheduledOnly";
    private static final String PARAM_PERIOD_DAY_LOOKUP = "days";
    private static final String PARAM_REPORT_PERIOD = "reportPeriod";
    private static final String PARAM_START_DATE = "startDate";
    private static final String PARAM_TOTALS_ONLY = "totalsOnly";
    private static final String PROCEDURE_ID_CLASS_SECTION_SCHEDULE = "EXPDATA-TN-MSTS";
    private static final int DATE_COLUMNS = 20;
    private static final int INITIAL_COLUMN_SIZE = 35;
    private static final int INITIAL_ROW_SIZE = 1500;
    private static final long serialVersionUID = 1L;

    private static final String DATE_FORMAT = "yyyy-MM-dd";
    private static final String FULLY_SCHEDULED = "_Scheduled";

    private static final String NOT_APPLICALBE = "NA";
    private static final String STRING_NO = "No";
    private static final String STRING_OVER = "Over";
    private static final String STRING_YES = "Yes";
    private static final String STRING_ZERO = "Zero";

    // Symbols
    private static final String SYMBOL_NONMEMBER = "--";
    private static final String SYMBOL_NONSESSION = "X";

    Boolean m_allStudents;
    private Map<String, String> m_attendance;
    private Map<String, Set<PlainDate>> m_calendars;
    private Map<String, String> m_cellToDay;
    private Map<String, String> m_cellToHeader;
    private DistrictSchoolYearContext m_context = null;
    private DataDictionary m_dictionary;
    private Map<String, String> m_dailyScheduled;
    private Map<String, String> m_dayToCell;
    private String m_fieldStudentEisStateCode;
    private ReportDataGrid m_grid;
    private Boolean m_includeScheduledWithdrawn;
    private SimpleDateFormat m_lookupFormat;
    private Boolean m_overUnderSchOnly;

    private ReferenceCode m_reportPeriod = null;
    private TNClassSectionScheduleData m_scheduleData;
    private PlainDate m_schoolEndDate;
    private PlainDate m_schoolStartDate;

    private int m_studentAbsent;
    private Map<String, StudentSpanCount> m_studentCountsMap;
    private int m_studentDaysInSession;
    private int m_studentDismissed;
    private int m_studentNonmember;
    private Map<String, Boolean> m_studentMembership;
    private int m_studentTardy;

    /**
     * Cleanup.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#cleanup()
     */
    @Override
    protected void cleanup() {
        super.cleanup();
        if (m_data != null) {
            m_data.close();
        }
    }

    /**
     * Constructs a ReportDataGrid containing reporting period register data. The grid contains a
     * row for
     * each student for the selected reporting period, and the following columns:
     * <ul>
     * <li>The reporting period stored as a String in the following format: 01
     * <li>Student object
     * <li>Student OID
     * <li>Columns 1-31, corresponding to the day of the month, containing the register detail
     * <li>Enrollment columns enrollment_1 - enrollment_31, containing null, "E", or "W"
     * <li>Period totals columns, corresponding to constants prefixed with COLUMN_PERIOD_
     * <li>Scheduled columns prefixed by the corresponding day of the month show if the student is
     * fully scheduled
     * </ul>
     *
     * @return JRDataSource
     * @throws ParseException exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws ParseException {

        // This report is designed to only work for a single school at a time due to a sorting issue
        // for secondary
        // students where students who were secondarily enrolled at the school would appear at the
        // bottom of the report
        // for every school. This was caused by ordering the query by the student's primary school
        // in order to
        // accommodate running for all schools. By dropping the all schools option, the sort was
        // eliminated and
        // the students sort correctly within the given school regardless of primary or secondary
        // status.

        if (m_schoolPeriodStartDate == null || m_schoolPeriodEndDate == null) {
            String message = "Report for school " + getSchool().getName() +
                    " cannot be generated. Report period with code: " +
                    m_reportPeriod.getCode() + " is not defined. Please define dates for the selected period "
                    + "and rerun the report.";
            m_grid.append();
            m_grid.set(COLUMN_ERROR_MESSAGE, message);
            m_grid.beforeTop();
            return m_grid;
        }

        m_cellToHeader = loadCycleDates();
        loadDailyScheduled();

        populateStudents();
        populateGrid();


        m_grid.beforeTop();

        // Add report parameters
        Boolean totalsOnly = (Boolean) getParameter(TOTALS_ONLY_PARAM);
        addParameter(PARAM_START_DATE, m_schoolPeriodStartDate);
        addParameter(PARAM_END_DATE, m_schoolPeriodEndDate);
        addParameter(PARAM_PERIOD_DAY_LOOKUP, m_cellToHeader);
        addParameter(PARAM_TOTALS_ONLY, totalsOnly);
        addParameter(PARAM_CURRENT_CONTEXT, m_context);

        return m_grid;
    }

    /**
     * Returns standard day in minutes for student.
     *
     * @param student SisStudent
     * @return int
     */
    protected int getStandardDay(SisStudent student) {
        // The standard day minutes. We can get it from the 'DOE STUDENT STANDARD DAY' field in
        // Grade Level reference table.
        int stdStandardDay = 0;
        try {
            ReferenceCode refCode = m_data.getStudentGradeLvl(student);
            String standardDayString = m_multiYearHelper.getHistoryValueByAlias(refCode,
                    ALIAS_STUDENT_HISTORY_STANDARD_DAY, ALIAS_STUDENT_STANDARD_DAY);
            stdStandardDay = Integer.valueOf(standardDayString).intValue();
        } catch (NumberFormatException nfe) {
            stdStandardDay = 0; // Set the value back to 0
        } catch (NullPointerException npe) {
            String message = "Student '" + student.getNameView() + "' has grade level '"
                    + m_data.getStudentGradeLvl(student).getCode()
                    + "' that doesn't match any of grade levels reference code'";
            AppGlobals.getLog().log(Level.SEVERE, message, npe);
        }

        // If the standard day is null or 0, then throw an error (since we later divided a number by
        // standard day)
        if (stdStandardDay == 0) {
            AppGlobals.getLog().log(Level.SEVERE,
                    "Standard Day for grade " + m_data.getStudentGradeLvl(student).getCode() + "cannot be 0 or null.");
        }
        return stdStandardDay;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_multiYearHelper =
                new TNEnrollmentHelper.TNStudentMultiYearHelper(getOrganization(), getCurrentContext(), getBroker());

        PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_attendance = new HashMap<String, String>();
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        initializeFields();
        m_calendars = new HashMap<String, Set<PlainDate>>();
        m_cellToDay = new HashMap(200);
        m_cellToHeader = new HashMap(200);
        m_dailyScheduled = new HashMap<String, String>();
        m_dayToCell = new HashMap(200);
        m_studentMembership = new HashMap<String, Boolean>();
        m_grid = new ReportDataGrid(INITIAL_ROW_SIZE, INITIAL_COLUMN_SIZE);
        m_lookupFormat = new SimpleDateFormat("dd");

        m_context = getCurrentContext();

        String reportPeriodOid = (String) getParameter(PARAM_REPORT_PERIOD);
        m_allStudents = (Boolean) getParameter(PARAM_INCLUDE_ALL_STUDENTS);
        m_includeScheduledWithdrawn = (Boolean) getParameter(PARAM_INCLUDE_SCHEDULED_WITHDRAWN);
        m_overUnderSchOnly = (Boolean) getParameter(PARAM_OVER_SCH_ONLY);
        m_reportPeriod = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, reportPeriodOid);
        m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, m_reportPeriod, getBroker());

        m_schoolStartDate = m_periodHelper.getYearBeginDate();
        m_schoolEndDate = m_periodHelper.getYearEndDate();
        m_schoolPeriodStartDate = m_periodHelper.getDateBegin(getSchool().getOid());
        m_schoolPeriodEndDate = m_periodHelper.getDateEnd(getSchool().getOid());

        if (m_schoolPeriodStartDate == null || m_schoolPeriodEndDate == null) {
            return;
        }

        m_studentAbsent = 0;
        m_studentTardy = 0;
        m_studentDismissed = 0;
        m_studentNonmember = 0;
        m_studentDaysInSession = 0;

        // Grades to be excluded
        m_excludedGrades =
                com.x2dev.utils.StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_EXCLUDED_GRADES),
                        ",",
                        true);

        initHelpers();

        loadStudentMembershipMap();
        loadAttendance();

        loadStudentCounts();
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();

        if (m_cellToDay != null) {
            m_cellToDay.clear();
            m_cellToDay = null;
        }

        if (m_cellToHeader != null) {
            m_cellToHeader.clear();
            m_cellToHeader = null;
        }

        if (m_dayToCell != null) {
            m_dayToCell.clear();
            m_dayToCell = null;
        }

        if (m_grid != null) {
            m_grid.clear();
            m_grid = null;
        }
    }

    /**
     * Calculates totals for multiple fields.
     *
     * @param entryIsMember boolean
     * @param withdrawalIsMember boolean
     * @param periodId String
     * @param i int
     */
    private void calculateTotals(boolean entryIsMember, boolean withdrawalIsMember, String periodId, int i) {

        String detail = (String) m_grid.get(Integer.toString(i));
        boolean inSession = (getCellDay(periodId, i) != null && !SYMBOL_NONSESSION.equals(detail));

        StudentEnrollment enrollment = (StudentEnrollment) m_grid.get(COLUMN_ENROLLMENT_TYPE + i);
        String enrollmentType = null;
        if (enrollment != null) {
            enrollmentType = enrollment.getEnrollmentType();
        }

        if (StudentEnrollment.WITHDRAWAL.equals(enrollmentType) && inSession) {
            // add one to non-member days based on preference
            if (!withdrawalIsMember) {
                m_studentNonmember++;
            }
        } else if (StudentEnrollment.ENTRY.equals(enrollmentType) && inSession) {
            // add one to non-member days based on preference
            if (!entryIsMember) {
                m_studentNonmember++;
            }
        } else {
            if (detail != null && inSession) {
                m_studentAbsent += (detail.indexOf(CODE_ABSENT) != -1 ? 1 : 0);
                m_studentDismissed += (detail.indexOf(CODE_DISMISSED) != -1 ? 1 : 0);
                m_studentTardy += (detail.indexOf(CODE_TARDY) != -1 ? 1 : 0);
            }

            if (detail != null) {
                m_studentNonmember += (detail.equals(SYMBOL_NONMEMBER) ? 1 : 0);
            }
        }

        if (inSession) {
            m_studentDaysInSession++;
        }
    }

    /**
     * Returns the day corresponding to the passed reporting period and cell number, null if one
     * does not exist.
     *
     * @param periodId String
     * @param cell int
     * @return String
     */
    private String getCellDay(String periodId, int cell) {
        return m_cellToDay.get(periodId + "_" + cell);
    }

    /**
     * Returns in-session days for the reporting period.
     *
     * @param calendarDays Collection<PlainDate>
     * @return Set<PlainDate> days in session
     */
    private Set<PlainDate> getDaysInSession(Collection<PlainDate> calendarDays) {
        Set<PlainDate> daysInSession = new HashSet<PlainDate>();
        for (PlainDate calendarDay : calendarDays) {
            if (!calendarDay.before(m_schoolPeriodStartDate) && !calendarDay.after(m_schoolPeriodEndDate)) {
                daysInSession.add(calendarDay);
            }
        }
        return daysInSession;
    }

    /**
     * Returns true if the school day is in session.
     *
     * @param schoolOid String
     * @param calendarId String
     * @param i int
     * @return boolean
     * @throws ParseException exception
     */
    private boolean getInSession(String schoolOid, String calendarId, int i) throws ParseException {
        // Default needs to be true b/c ADM only evaluates all in session days. Defaulting to false
        // would cause days not marked as being in session to be flagged as not fully scheduled.
        boolean inSession = true;

        if (calendarId != null) {
            String key = schoolOid + calendarId;
            Set nonSessionDays = m_calendars.get(key);
            if (nonSessionDays != null) {
                SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
                Date cellDate;

                String date = m_cellToDay.get(m_reportPeriod.getCode() + "_" + i);

                if (date != null) {
                    try {
                        cellDate = sdf.parse(date);
                    } catch (ParseException e) {
                        e.printStackTrace();
                        throw e;
                    }

                    if (cellDate != null && (nonSessionDays.contains(cellDate) ||
                            cellDate.before(m_schoolStartDate) ||
                            cellDate.after(m_schoolEndDate))) {
                        inSession = false;
                    }
                } else {
                    inSession = false;
                }
            }
        }

        return inSession;
    }

    /**
     * Returns the student's membership status after evaluating enrollment records for the given
     * day.
     *
     * @param student SisStudent
     * @param periodId String
     * @param i int
     * @return boolean
     */
    private boolean getMembershipStatus(SisStudent student, String periodId, int i) {
        String dateVal = getCellDay(periodId, i);
        String studentOid = student.getOid();

        boolean isMember = false;

        // Since m_enrollment only contains changes to enrollment within the report's selected date
        // range, isMember will
        // only be modified when those enrollment records match the date of the cell being
        // processed.
        if (dateVal != null) {
            String key = studentOid + dateVal;

            if (m_studentMembership.get(key) != null) {
                isMember = m_studentMembership.get(key).booleanValue();
            }
        }

        return isMember;
    }

    /**
     * Returns data structure with student`s absence and membership data.
     *
     * @param span TNStudentEnrollmentSpan
     * @param student SisStudent
     * @param daysInSession Collection<PlainDate>
     * @param daysInSessionForStudent Collection<PlainDate>
     * @return StudentSpanCount
     */
    private StudentSpanCount getStudentCountsForSpan(TNStudentEnrollmentSpan span,
                                                     SisStudent student,
                                                     Collection<PlainDate> daysInSession,
                                                     Collection<PlainDate> daysInSessionForStudent) {
        int membershipMinutes = 0;
        int absenceMinutes = 0;
        TNClassSectionScheduleEntity sectionHelper = null;

        PlainDate spanFirstActiveDate = span.getFirstActiveDate();
        PlainDate spanLastActiveDate = span.getFirstInactiveEnrollment() != null ? span.getLastActiveDate() : null;

        if (spanFirstActiveDate == null || (spanFirstActiveDate != null && m_schoolPeriodStartDate != null
                && spanFirstActiveDate.before(m_schoolPeriodStartDate))) {
            spanFirstActiveDate = m_schoolPeriodStartDate;
        }
        if (spanLastActiveDate == null || (spanLastActiveDate != null && m_schoolPeriodEndDate != null
                && spanLastActiveDate.after(m_schoolPeriodEndDate))) {
            spanLastActiveDate = m_schoolPeriodEndDate;
        }

        try {
            List<TNStudentScheduleSpan> scheduleSpans = m_data.getTNStudentScheduleSpans(student);

            if (m_includeScheduledWithdrawn.booleanValue()) {
                sectionHelper = new TNClassSectionScheduleEntity(m_scheduleData, scheduleSpans, daysInSession,
                        m_schoolPeriodStartDate, m_schoolPeriodEndDate);
            } else {
                sectionHelper = new TNClassSectionScheduleEntity(m_scheduleData, scheduleSpans, daysInSession,
                        spanFirstActiveDate, spanLastActiveDate);
            }

            if (daysInSessionForStudent != null) {
                List<StudentAttendance> absences = m_data.getStudentAttendances(student.getOid());
                List<PlainDate> absenceDates = new ArrayList<PlainDate>();
                if (!CollectionUtils.isEmpty(absences)) {
                    for (StudentAttendance absence : absences) {
                        if (absence.getAbsentIndicator()) {
                            absenceDates.add(absence.getDate());
                        }
                    }
                }
                // Get the total membership minutes
                membershipMinutes += sectionHelper.getTotalMinutes(0, false);

                // Get the absence minutes
                if (!spanFirstActiveDate.after(spanLastActiveDate) && !CollectionUtils.isEmpty(absenceDates)) {
                    for (PlainDate absenceDate : absenceDates) {
                        if (DateUtils.isBetween(absenceDate, spanFirstActiveDate, spanLastActiveDate)) {
                            absenceMinutes += sectionHelper.getScheduledMinutes(absenceDate, getStandardDay(student));
                        }
                    }
                }
            }
        } catch (X2BaseException x2Exception) {
            membershipMinutes = 0;
            absenceMinutes = 0;
            AppGlobals.getLog().log(Level.SEVERE, x2Exception.getMessage(), x2Exception);
        }
        return new StudentSpanCount(membershipMinutes, absenceMinutes, sectionHelper, (SisSchool) getSchool());
    }

    /**
     * Initialize helper objects. Should be called when
     * m_schoolPeriodStartDate/m_schoolPeriodEndDate initialized
     */
    private void initHelpers() {
        Collection<StateReportValidationError> initErrors = new ArrayList<StateReportValidationError>();
        try {
            m_data = new EnrollmentStatistics();
            m_data.setBroker(getBroker());
            m_data.setOrganization(getOrganization());
            m_data.setPrivilegeSet(getPrivilegeSet());
            m_data.setSchoolContext(true);
            m_data.setSchool(getSchool());
            m_data.setParameters(getParameters());
            m_data.setUser(getUser());
            m_data.initializeExport();
            initErrors.addAll(m_data.getSetupErrors());
        } catch (X2BaseException x2be) {
            String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                    .getMessage(INITIALIZE_KEY);
            initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));
        }

        /*
         * Adjust student criteria here so that criteria is already set when initializing
         * TNClassSectionScheduleData
         */
        if (m_data != null) {
            if (m_allStudents != null && !m_allStudents.booleanValue()) {
                m_data.materializeStudentCriteria();
            }

            addParameter(TNClassSectionHelper.INPUT_PARAM_ENROLLMENT_HELPER, m_data.m_tnEnrHelper);
            addParameter(TNClassSectionHelper.INPUT_PARAM_BYPASS_DUP_SECT_TEST, Boolean.TRUE);
            addParameter(TNClassSectionHelper.INPUT_PARAM_LOAD_STUDENT_SECTIONS_ONLY, Boolean.TRUE);
        }

        try {
            // Initialize the report data object.
            m_scheduleData = new TNClassSectionScheduleData();
            m_scheduleData.setBroker(getBroker());
            m_scheduleData.setOrganization(getOrganization());
            m_scheduleData.setPrivilegeSet(getPrivilegeSet());
            m_scheduleData.setSchoolContext(true);
            m_scheduleData.setSchool(getSchool());
            m_scheduleData.setParameters(getParameters());
            m_scheduleData.setUser(getUser());
            initErrors.addAll(m_scheduleData.loadDefinitions(PROCEDURE_ID_CLASS_SECTION_SCHEDULE, getBroker()));
            m_scheduleData.initializeExport();
        } catch (X2BaseException x2be) {
            String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                    .getMessage(INITIALIZE_KEY);
            initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));
        }
    }

    /**
     * initializes fields that are used in the class.
     */
    private void initializeFields() {
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_STUDENT_EIS_STATE_ID);
        if (field != null) {
            m_fieldStudentEisStateCode = field.getJavaName();
        } else {
            m_fieldStudentEisStateCode = BEANPATH_FIELD_A033;
        }

    }

    /**
     * Loads the max and min calendar dates to m_reportStartDate and m_reportEndDate for the given
     * reporting period.
     *
     * @return Map
     */
    private Map<String, String> loadCycleDates() {
        int cell = 1;
        Map<String, String> cellToHeader = new HashMap<String, String>();

        TreeSet<PlainDate> days = new TreeSet(m_periodHelper.getDaysInSession(getSchool().getOid()));

        for (PlainDate day : days) {
            cellToHeader.put(m_reportPeriod.getCode() + "_" + cell, m_lookupFormat.format(day));
            m_cellToDay.put(m_reportPeriod.getCode() + "_" + cell, day.toString());
            m_dayToCell.put(m_reportPeriod.getCode() + "_" + day.toString(), Integer.toString(cell));
            cell++;
        }

        return cellToHeader;
    }

    /**
     * Loads whether a student is fully scheduled for each day in the funding period.
     */
    private void loadDailyScheduled() {
        Collection<SisStudent> students = m_data.getStudents(getSchool().getOid());
        Collection<PlainDate> inSessionDays = m_periodHelper.getDaysInSession(getSchool().getOid());

        for (SisStudent student : students) {

            int stdStandardDay = getStandardDay(student);

            for (PlainDate day : inSessionDays) {
                StudentSpanCount count = m_studentCountsMap.get(student.getOid());

                String cell = m_dayToCell.get(m_reportPeriod.getCode() + "_" + day);
                String key = student.getOid() + cell;

                String fullyScheduled = "";
                int schMinutes = count == null ? 0 : count.getScheduledMinutes(day);
                if (schMinutes == 0) {
                    fullyScheduled = STRING_ZERO;
                } else if (schMinutes < stdStandardDay) {
                    fullyScheduled = STRING_NO;
                } else if (schMinutes > stdStandardDay) {
                    fullyScheduled = STRING_OVER;
                } else {
                    fullyScheduled = STRING_YES;
                }

                m_dailyScheduled.put(key, fullyScheduled);
            }
        }
    }

    /**
     * Loads map for students counts.
     */
    private void loadStudentCounts() {
        m_studentCountsMap = new HashMap<String, StudentSpanCount>();
        Collection<SisStudent> students = m_data.getStudents(getSchool().getOid());
        Collection<PlainDate> inSessionDays = m_periodHelper.getDaysInSession(getSchool().getOid());

        Collection<PlainDate> daysInSessionForStudent = null;
        for (SisStudent student : students) {
            // Get the in-session calendar days for student
            Collection<PlainDate> calendarDays = m_data.getCalendarDays((SisSchool) getSchool(),
                    (String) m_data.m_tnEnrHelper.getStudentValueByBeanPath(student, SisStudent.COL_CALENDAR_CODE));
            if (calendarDays != null) {
                daysInSessionForStudent = getDaysInSession(calendarDays);
            } else {
                daysInSessionForStudent = inSessionDays;
            }

            List<TNStudentEnrollmentSpan> enrollmentSpans = m_data.getStudentEnrollmentSpans(student, true);
            for (TNStudentEnrollmentSpan span : enrollmentSpans) {
                if (span.getSchool() != null && span.getSchool().getOid().equals(getSchool().getOid())) {
                    StudentSpanCount studentSpanCount = m_studentCountsMap.get(student.getOid());
                    if (studentSpanCount == null) {
                        studentSpanCount =
                                getStudentCountsForSpan(span, student, inSessionDays, daysInSessionForStudent);
                        m_studentCountsMap.put(student.getOid(), studentSpanCount);
                    } else {
                        studentSpanCount
                                .add(getStudentCountsForSpan(span, student, inSessionDays, daysInSessionForStudent));
                    }
                }
            }
        }
    }

    /**
     * Loads map for students with period dates and membership status for each date.
     */
    private void loadStudentMembershipMap() {
        Collection<SisStudent> students = m_data.getStudents(getSchool().getOid());
        Collection<PlainDate> inSessionDays = m_periodHelper.getDaysInSession(getSchool().getOid());

        for (SisStudent student : students) {
            List<TNStudentEnrollmentSpan> enrollmentSpans = m_data.getStudentEnrollmentSpans(student, true);

            for (PlainDate date : inSessionDays) {

                String key = student.getOid() + date;

                m_studentMembership.put(key, Boolean.valueOf(false));

                for (TNStudentEnrollmentSpan enrollmentSpan : enrollmentSpans) {
                    if (enrollmentSpan.getSchool() != null &&
                            enrollmentSpan.getSchool().getOid().equals(getSchool().getOid()) &&
                            !date.before(enrollmentSpan.getFirstActiveDate()) &&
                            (enrollmentSpan.getLastActiveDate() == null ||
                                    !date.after(enrollmentSpan.getLastActiveDate()))) {
                        m_studentMembership.put(key, Boolean.valueOf(true));
                        /*
                         * Check if the student is enrolled and withdrawn on the same day.
                         * If yes, then we don't want to count this student.
                         */
                        if ((enrollmentSpan.getFirstActiveEnrollment() != null && StudentEnrollment.ENTRY
                                .equals(enrollmentSpan.getFirstActiveEnrollment().getEnrollmentType()) &&
                                enrollmentSpan.getFirstInactiveEnrollment() != null && StudentEnrollment.WITHDRAWAL
                                        .equals(enrollmentSpan.getFirstInactiveEnrollment().getEnrollmentType())
                                &&
                                enrollmentSpan.getFirstActiveEnrollment().getEnrollmentDate()
                                        .equals(enrollmentSpan.getFirstInactiveEnrollment().getEnrollmentDate()))) {
                            m_studentMembership.put(key, Boolean.valueOf(false));
                        } else {
                            // Ok, the date belongs to this span, go to next date.
                            break;
                        }

                    }
                }
            }
        }
    }

    /**
     * Populates student attendance info to a map keyed on student oid + date.
     */
    private void loadAttendance() {
        Collection<SisStudent> students = m_data.getStudents(getSchool().getOid());
        for (SisStudent student : students) {
            List<StudentAttendance> studentAttendances = m_data.getStudentAttendances(student.getOid());
            if (studentAttendances != null) {
                for (StudentAttendance attendance : studentAttendances) {
                    if (attendance.getDate() != null) {
                        StringBuilder attendanceString = new StringBuilder(4);
                        attendanceString.append(attendance.getAbsentIndicator() ? CODE_ABSENT : "");
                        attendanceString.append(attendance.getTardyIndicator() ? CODE_TARDY : "");
                        attendanceString.append(attendance.getDismissedIndicator() ? CODE_DISMISSED : "");

                        // Add '*' to string being displayed as the attendance display for days that
                        // are not membership days
                        Boolean isMember = m_studentMembership.get(student.getOid() + attendance.getDate());
                        // Skip attendances on not session days.
                        if (isMember == null) {
                            continue;
                        } else if (attendanceString.length() > 0 && !isMember.booleanValue()) {
                            attendanceString.append(CODE_ATT_NOT_MEMB_DAY);
                        }

                        String key = attendance.getStudentOid() + attendance.getDate();

                        m_attendance.put(key, attendanceString.toString());
                    }
                }
            }
        }
    }

    /**
     * Populates the grid.
     *
     * @throws ParseException exception
     */
    private void populateGrid() throws ParseException {
        boolean entryIsMember = Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();

        boolean withdrawalIsMember = Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

        /*
         * Set the non-session days on the grid. A date cell is labeled as non-session if
         * at least one of the following are true:
         *
         * 1) The date's corresponding SchoolCalendarDate bean has an in-session flag = false
         * 2) The date is before the first day of school
         * 3) The date is after the last day of school
         */
        m_grid.beforeTop();
        while (m_grid.next()) {
            double averageDailyMembership = 0.00;

            SisStudent student = (SisStudent) m_grid.get(COLUMN_STUDENT);
            String calendarId =
                    (String) m_data.m_tnEnrHelper.getStudentValueByBeanPath(student, SisStudent.COL_CALENDAR_CODE);
            String periodId = (String) m_grid.get(COLUMN_REPORT_PERIOD);
            String studentOid = (String) m_grid.get(COLUMN_STUDENT_OID);
            StudentSpanCount count = m_studentCountsMap.get(studentOid);

            m_studentAbsent = 0;
            m_studentTardy = 0;
            m_studentDismissed = 0;
            m_studentNonmember = 0;
            m_studentDaysInSession = 0;

            boolean isMember = false;
            boolean deleteIfOverOrUnder = true;
            for (int i = 1; i <= DATE_COLUMNS; i++) {
                String fullyScheduled = "";

                String fullyScheduledKey = studentOid + i;
                String scheduled = m_dailyScheduled.get(fullyScheduledKey);

                // Sets membership status
                isMember = getMembershipStatus(student, periodId, i);
                setMembershipStatus(isMember, periodId, i);

                // Sets attendance
                setAttendance(studentOid, periodId, i);

                // Updates non-session days
                boolean inSession = getInSession(
                        (String) m_data.m_tnEnrHelper.getStudentValueByBeanPath(student, SisStudent.COL_SCHOOL_OID),
                        calendarId, i);

                setNonSessionDays(inSession, i);

                // Calculates the totals for absences, tardies, days member, etc
                calculateTotals(entryIsMember, withdrawalIsMember, periodId, i);

                if (isMember && inSession) {
                    if (scheduled == null || STRING_ZERO.equals(scheduled)) {
                        fullyScheduled = STRING_NO;
                        deleteIfOverOrUnder = false;
                    } else {
                        fullyScheduled = scheduled;
                        if (STRING_NO.equals(fullyScheduled) || STRING_OVER.equals(fullyScheduled)) {
                            deleteIfOverOrUnder = false;
                        }
                    }
                } else if (!isMember && inSession && m_includeScheduledWithdrawn.booleanValue()) {
                    if (scheduled != null && !STRING_ZERO.equals(scheduled)) {
                        // Highlight cells where student is not member and is scheduled for classes
                        fullyScheduled = STRING_OVER;
                        deleteIfOverOrUnder = false;
                    } else {
                        fullyScheduled = NOT_APPLICALBE;
                    }
                } else {
                    fullyScheduled = NOT_APPLICALBE;
                }

                m_grid.set(i + FULLY_SCHEDULED, fullyScheduled);
            }

            if (m_overUnderSchOnly.booleanValue() && deleteIfOverOrUnder) {
                m_grid.deleteRow();
            } else {
                int daysPresent = m_studentDaysInSession - m_studentAbsent - m_studentNonmember;
                double averageDaysAttendance = 0.00;

                if (daysPresent > 0) {
                    averageDaysAttendance = (double) (count.getMembershipMinutes() - count.getAbsenceMinutes()) /
                            (m_studentDaysInSession * getStandardDay(student));

                    // Rounds to 2 decimal places
                    averageDaysAttendance = roundToTwoDecimals(averageDaysAttendance);
                }

                // Rounds to 2 decimal places
                if (count != null) {
                    averageDailyMembership = roundToTwoDecimals((double) (count.getMembershipMinutes()) /
                            (m_studentDaysInSession * getStandardDay(student)));
                }

                // Set the total values on the grid
                m_grid.set(COLUMN_PERIOD_ABSENT, Integer.valueOf(m_studentAbsent));
                m_grid.set(COLUMN_PERIOD_ADA, Double.valueOf(averageDaysAttendance));
                m_grid.set(COLUMN_PERIOD_ADM, Double.valueOf(averageDailyMembership));
                m_grid.set(COLUMN_PERIOD_DAYS_IN_SESSION, Integer.valueOf(m_studentDaysInSession));
                m_grid.set(COLUMN_PERIOD_DISMISSED, Integer.valueOf(m_studentDismissed));
                m_grid.set(COLUMN_PERIOD_NONMEMBER, Integer.valueOf(m_studentNonmember));
                m_grid.set(COLUMN_PERIOD_PRESENT, Integer.valueOf(daysPresent));
                m_grid.set(COLUMN_PERIOD_TARDY, Integer.valueOf(m_studentTardy));
            }
        }
    }

    /**
     * Populates the grid with student data.
     */
    private void populateStudents() {
        m_grid.beforeTop();

        School school = getSchool();

        Collection<SisStudent> students = m_data.getStudents(school.getOid());

        for (SisStudent student : students) {
            String studentOid = student.getOid();
            if (!m_grid.locate(COLUMN_STUDENT_OID, studentOid)) {
                m_grid.append();

                m_grid.set(COLUMN_SCHOOL, school);
                m_grid.set(COLUMN_STUDENT, student);
                m_grid.set(COLUMN_STUDENT_GRADE, m_data.getStudentGradeLvl(student).getCode());
                m_grid.set(COLUMN_STUDENT_HOMEROOM,
                        m_data.m_tnEnrHelper.getStudentValueByBeanPath(student, SisStudent.COL_HOMEROOM));
                m_grid.set(COLUMN_STUDENT_NAME, student.getNameView());

                m_grid.set(COLUMN_STUDENT_OID, studentOid);
                m_grid.set(COLUMN_STUDENT_CODE, student.getFieldValueByBeanPath(m_fieldStudentEisStateCode));
                m_grid.set(COLUMN_REPORT_PERIOD, m_reportPeriod.getCode());
                m_grid.set(COLUMN_STUDENT_SPED_STATUS, m_data.getStudentSpedStatus(student));
            }
        }
    }

    /**
     * Round value to nearest two decimal places.
     *
     * @param val double
     * @return double
     */
    private double roundToTwoDecimals(double val) {
        return (double) Math.round(val * 100) / 100;
    }

    /**
     * Sets non-session days on the grid.
     *
     * @param studentOid String
     * @param periodId String
     * @param i int
     */
    private void setAttendance(String studentOid, String periodId, int i) {
        String calDate = getCellDay(periodId, i);
        String key = studentOid + calDate;
        String attendance = m_attendance.get(key);

        if (attendance != null) {
            m_grid.set(Integer.toString(i), attendance);
        }
    }

    /**
     * Sets the membership status on the grid for the student for the passed day. Returns the
     * membership status
     *
     * @param isMember boolean
     * @param periodId String
     * @param i int
     */
    private void setMembershipStatus(boolean isMember, String periodId, int i) {
        // Only update membership in a cell if the cell corresponds to a valid day
        if (getCellDay(periodId, i) != null) {
            if (!isMember) {
                m_grid.set(Integer.toString(i), SYMBOL_NONMEMBER);
            }
        }
    }

    /**
     * Sets non-session days on the grid.
     *
     * @param inSession boolean
     * @param i int
     */
    private void setNonSessionDays(boolean inSession, int i) {
        if (!inSession) {
            m_grid.set(Integer.toString(i), SYMBOL_NONSESSION);
        }
    }
}
