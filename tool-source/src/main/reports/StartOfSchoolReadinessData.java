/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.beans.SystemPreferenceDefinition.STAFF_ACTIVE_CODE;
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolBroker;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.ReportValueConverter;
import com.follett.fsc.core.utils.database.DatabaseSyntaxFactory;
import com.follett.fsc.core.utils.database.sqlquerybuilder.SqlQueryBuilder;
import com.follett.fsc.core.utils.string.AdvancedStringBuilder;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.sf.jasperreports3.engine.JRDataSource;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source for the "Start of School Readiness" report.
 *
 * @author X2 Development Corporation
 */
public class StartOfSchoolReadinessData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "details" report parameter. This value is an Integer.
     */
    public static final String DETAILS = "details";

    /**
     * Name for the "pre-reg status" code parameter. This value is a String.
     */
    public static final String PREREG_STATUS_PARAM = "preregStatus";

    /**
     * Name for the "school year" parameter. This value is an Integer.
     */
    public static final String SCHOOL_YEAR_PARAM = "schoolYear";

    /**
     * New line for given environment
     */
    public static final String NL = System.lineSeparator();

    // Grid fields
    private static final String FIELD_ICON = "icon";
    private static final String FIELD_MESSAGE = "message";
    private static final String FIELD_SCHOOL = "school";

    // Report parameters
    private static final String CONTEXT_PARAM = "context";
    private static final String DISTRICT_RESULT_MAP_PARAM = "districtResults";

    // Subreport constants
    private static final String SUBREPORT_ID = "SYS-ADM-014-SUB1";
    private static final String SUBREPORT_DATA_PARAM = "subreportData";
    private static final String SUBREPORT_FORMAT_PARAM = "subreportFormat";

    // List delimiters
    private static final String DELIMITER_SEMI_COLON = "; ";
    private static final String DELIMITER_NEW_LINE = "\n";

    // District ID
    private static final String DISTRICT_ID = "District";

    // Color codes
    private static final String COLOR_CODE_COMPLETE = "#339900";
    private static final String COLOR_CODE_ERROR = "#FF0000";
    private static final String COLOR_CODE_WARNING = "#FFFF00";

    // Pattern constants
    private static final String SINGULAR_PATTERN = "\\w+/(\\w+|\\s)";
    private static final String NUMBER_PATTERN = "#n";

    // Pattern columns constants
    private static final int COL_SINGULAR = 0;
    private static final int COL_PLURAL = 1;

    // Step status
    private static final int STATUS_COMPLETE = 1;
    private static final int STATUS_ERROR = 3;
    private static final int STATUS_WARNING = 2;

    // Calendar date difference
    private static final int CALENDAR_DATE_DIFFERENCE = 10;

    // Detail limit
    private static final int DETAIL_LIMIT = 25;

    // Inactive student threshold per school
    private static final int INACTIVE_STUDENT_THRESHOLD = 100;

    // Initialize map size
    private static final int INITIAL_MAP_SIZE = 500;

    // Variables used by the inner classes
    protected DataDictionary m_dictionary;
    protected Locale m_locale;
    protected Pattern m_pattern;

    private Map<String, Collection<String>> m_bellScheduleOids;
    private DistrictSchoolYearContext m_context;
    private int m_count;
    private int m_detailsToDisplay;
    private Map<String, Object> m_districtMap;
    private TreeMap<Integer, List<String>> m_gradeLevelMap;
    private Map<String, Collection> m_gradeTermCoverMaps;
    private Map<String, Collection> m_gradeTermDates;
    private ReportDataGrid m_grid;
    private ReportDataGrid m_messages;
    private Map<String, String> m_inactiveStudentCounts;
    private Map<String, String> m_inactiveStudentScheduleCounts;
    private Map<String, Collection> m_inactiveStudents;
    private Map<String, Collection> m_inactiveStudentSchedules;
    private Map<String, Collection> m_nonMatchingNextSchools;
    private String m_preregStatus;
    private Map<String, Collection> m_preregStudents;
    private Map<String, Collection<Result>> m_results;
    private Map<String, Collection> m_scheduleDays;
    private Collection<String> m_schoolOids;
    private QueryIterator m_schools;
    private String m_staffActiveCode;
    private Map<String, Collection> m_studentGradeLevels;
    private DbColumns m_cols;

    /**
     * Class to hold db column names used in the report.
     */
    private class DbColumns {

        public StudentCols StudentCols = new StudentCols();
        public SchoolCols SchoolCols = new SchoolCols();
        public StudentScheduleCols StudentScheduleCols = new StudentScheduleCols();
        public SchoolScheduleContextCols SchoolScheduleContextCols = new SchoolScheduleContextCols();

        /**
         * Class to hold student db column names used in the report.
         */
        private class StudentCols {

            public final String STD_OID;
            public final String STD_SKL_OID;
            public final String STD_ENROLLMENT_STATUS;
            public final String STD_ID_LOCAL;
            public final String STD_NAME_VIEW;

            public StudentCols() {

                String className = SisStudent.class.getName();

                STD_OID = findColDbName(className, X2BaseBean.COL_OID);
                STD_SKL_OID = findColDbName(className, SisStudent.COL_SCHOOL_OID);
                STD_ENROLLMENT_STATUS = findColDbName(className, SisStudent.COL_ENROLLMENT_STATUS);
                STD_ID_LOCAL = findColDbName(className, SisStudent.COL_LOCAL_ID);
                STD_NAME_VIEW = findColDbName(className, SisStudent.COL_NAME_VIEW);
            }
        }

        /**
         * Class to hold school db column names used in the report.
         */
        private class SchoolCols {

            public final String SKL_OID;
            public final String SKL_SKX_OID_ACTIV;

            public SchoolCols() {

                String className = SisSchool.class.getName();

                SKL_OID = findColDbName(className, X2BaseBean.COL_OID);
                SKL_SKX_OID_ACTIV = findColDbName(className, SisSchool.COL_ACTIVE_SCHOOL_SCHED_OID);
            }
        }

        /**
         * Class to hold student schedule db column names used in the report.
         */
        private class StudentScheduleCols {

            public final String SSC_STD_OID;
            public final String SSC_SCH_OID;

            public StudentScheduleCols() {

                String className = StudentSchedule.class.getName();

                SSC_STD_OID = findColDbName(className, StudentSchedule.COL_STUDENT_OID);
                SSC_SCH_OID = findColDbName(className, StudentSchedule.COL_SCHEDULE_OID);
            }
        }

        /**
         * Class to hold school schedule db column names used in the report.
         */
        private class SchoolScheduleContextCols {

            public final String SKX_OID;
            public final String SKX_SCH_OID_ACTIVE;
            public final String SKX_CTX_OID;

            public SchoolScheduleContextCols() {

                String className = SchoolScheduleContext.class.getName();

                SKX_OID = findColDbName(className, X2BaseBean.COL_OID);
                SKX_SCH_OID_ACTIVE = findColDbName(className, SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID);
                SKX_CTX_OID = findColDbName(className, SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID);
            }
        }
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws SQLException
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws SQLException {
        checkDistrictContext();

        if (m_context != null) {

            loadSchools();
            loadBellSchedules();
            loadGradeTermCoverMaps();
            loadGradeTermDates();
            loadInactiveStudentCounts();
            loadInactiveStudentScheduleCounts();
            loadInactiveStudentSchedules();
            loadInactiveStudents();
            loadNonMatchingNextSchools();
            loadPreregStudents();
            loadScheduleDays();
            loadStudentGradeLevels();

            checkDistrictCalendarDates();
            checkDistrictCalendarDateRanges();
            checkDistrictCalendarDateNumbers();
            checkUserRoles();

            appendResults(null);

            m_count = 1;
            try {
                while (m_schools.hasNext()) {
                    SisSchool school = (SisSchool) m_schools.next();

                    /*
                     * Check schedules
                     */
                    checkSchedule(school);
                    checkScheduleNoDates(school);
                    checkScheduleDateConflicts(school);
                    checkGradeTermCoverMaps(school);

                    /*
                     * Check calendars
                     */
                    checkSchoolCalendarDates(school);
                    checkSchoolCalendarDateRanges(school);
                    checkSchoolCalendarDateNumbers(school);
                    checkSchoolCalendarScheduleDays(school);
                    checkSchoolCalendarBellSchedules(school);

                    /*
                     * Check students
                     */
                    checkStudentHomerooms(school);
                    checkInactiveStudentSchedules(school);
                    checkSecondaryStudentSchedules(school);
                    checkStudentCalendarCodes(school);
                    checkPreregStudents(school);
                    checkNextSchool(school);
                    checkInactiveStudents(school);
                    checkGradeLevels(school);

                    /*
                     * Check staff
                     */
                    checkStaffCalendarCodes(school);
                    checkStaffHomerooms(school);

                    /*
                     * Check grade terms
                     */
                    checkGradeTermDates(school);
                    checkGradeTermDateRanges(school);

                    /*
                     * Add the results to the grid.
                     */
                    appendResults(school);
                }
            } finally {
                m_schools.close();
            }
        } else {
            appendResults(null);
        }

        addParameters();

        m_grid.beforeTop();
        return m_grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_detailsToDisplay = ((Integer) getParameter(DETAILS)).intValue();
        m_preregStatus = (String) getParameter(PREREG_STATUS_PARAM);

        m_count = 1;

        m_grid = new ReportDataGrid();
        m_messages = new ReportDataGrid();

        m_results = new HashMap<String, Collection<Result>>();

        m_staffActiveCode = PreferenceManager.getPreferenceValue(getOrganization(), STAFF_ACTIVE_CODE);
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        m_cols = new DbColumns(); // This has to come after m_dictionary

        m_locale = getLocale();

        m_pattern = Pattern.compile(SINGULAR_PATTERN);

        m_gradeLevelMap = StudentManager.buildGradeLevelMap(getBroker());
    }

    /**
     * Adds the parameters to be used in the report format.
     */
    private void addParameters() {
        addParameter(CONTEXT_PARAM, m_context);
        addParameter(DISTRICT_RESULT_MAP_PARAM, m_districtMap);

        m_messages.beforeTop();
        addParameter(SUBREPORT_DATA_PARAM, m_messages);

        Report subreportFormat = ReportUtils.getReport(SUBREPORT_ID, getBroker());
        addParameter(SUBREPORT_FORMAT_PARAM, new ByteArrayInputStream(subreportFormat.getCompiledFormat()));
    }

    /**
     * Adds a school filter to a given where clause.
     *
     * @param whereClause String - The where clause to add the school oid filter to
     * @param sklOidCol String - The SKL_OID column to filter on
     * @param indentLevel int
     * @return String
     */
    private String addSchoolFilter(String whereClause, String sklOidCol, int indentLevel) {

        AdvancedStringBuilder newWhereClause = new AdvancedStringBuilder(whereClause);

        if (isSchoolContext()) {
            newWhereClause
                    .setIndentLevel(indentLevel)
                    .appendLine("AND   " + sklOidCol + " = '" + getSchool().getOid() + "'");
        }

        return newWhereClause.toString();
    }

    /**
     * Adds the school criteria to a given criteria if in a school context.
     *
     * @param baseCriteria Criteria
     */
    private void addSchoolCriteria(Criteria baseCriteria) {

        if (isSchoolContext()) {
            Criteria schoolCriteria = new Criteria();
            schoolCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            baseCriteria.addAndCriteria(schoolCriteria);
        }
    }

    /**
     * Appends the results for the passed school to the grid.
     *
     * @param school SisSchool
     */
    private void appendResults(SisSchool school) {
        Map<String, Object> row = null;
        String schoolName = null;

        if (school != null) {
            m_grid.append();
            m_grid.set(FIELD_SCHOOL, school);

            row = m_grid.getCurrentRow();
            schoolName = school.getName();
        } else {
            m_districtMap = new HashMap<String, Object>();

            row = m_districtMap;
            schoolName = DISTRICT_ID;
        }

        appendResults(row, schoolName);
    }

    /**
     * Appends the results to the passed row.
     *
     * @param row Map<String,Object>
     * @param schoolName String
     */
    private void appendResults(Map<String, Object> row, String schoolName) {
        Collection<Result> results = m_results.get(schoolName);
        for (Result result : results) {
            row.put(result.getId(), result.getFieldValue());

            if (displayDetails(result.getStatus())) {
                m_messages.append();
                m_messages.set(FIELD_SCHOOL, schoolName);
                m_messages.set(FIELD_MESSAGE, result.getMessage());
                m_messages.set(FIELD_ICON, result.getFieldValue());
            }
        }

        m_count = 1;
    }

    /**
     * Checks to see if the number of district calendar dates are about the same as last year.
     */
    private void checkDistrictCalendarDateNumbers() {
        Criteria currentCriteria = new Criteria();
        currentCriteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, m_context.getOid());
        currentCriteria.addEqualTo(DistrictCalendar.COL_IN_SESSION_INDICATOR, Boolean.valueOf(true));

        QueryByCriteria currentQuery = new QueryByCriteria(DistrictCalendar.class, currentCriteria);
        int currentCount = getBroker().getCount(currentQuery);

        Criteria previousCriteria = new Criteria();
        previousCriteria.addEqualTo(
                DistrictCalendar.REL_DISTRICT_CONTEXT + PATH_DELIMITER + DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                Integer.valueOf(m_context.getSchoolYear() - 1));
        previousCriteria.addEqualTo(DistrictCalendar.COL_IN_SESSION_INDICATOR, Boolean.valueOf(true));

        QueryByCriteria previousQuery = new QueryByCriteria(DistrictCalendar.class, previousCriteria);
        int previousCount = getBroker().getCount(previousQuery);

        int difference = Math.abs(previousCount - currentCount);

        String messageString = "There are " + difference + (previousCount > currentCount ? " less " : " more ") +
                "district calendar dates this year than last year";

        ValidationMessage message = new ValidationMessage(messageString, null, null);
        validateStep(false, difference > CALENDAR_DATE_DIFFERENCE, null, message);
    }

    /**
     * Checks for district calendar dates that are outside the school year start and end dates.
     */
    private void checkDistrictCalendarDateRanges() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, m_context.getOid());
        criteria.addNotBetween(DistrictCalendar.COL_DATE, m_context.getStartDate(), m_context.getEndDate());

        QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);
        query.addOrderByAscending(DistrictCalendar.COL_DATE);

        Collection dates = getBroker().getCollectionByQuery(query);

        String messageString =
                "There is/are #n district calendar date/dates that falls/fall outside the school year start and end dates: ";
        String[] properties = {DistrictCalendar.COL_DATE};

        ValidationMessage message = new ValidationMessage(messageString, dates, properties);
        validateStep(!CollectionUtils.isEmpty(dates), false, null, message);
    }

    /**
     * Checks for a district calendar with no dates initialized.
     */
    private void checkDistrictCalendarDates() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, m_context.getOid());

        QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);
        int count = getBroker().getCount(query);

        String messageString = "No district calendar dates initialized.";

        ValidationMessage message = new ValidationMessage(messageString, null, null);
        validateStep(count == 0, false, null, message);
    }

    /**
     * Checks for a district school year context for the entered school year.
     */
    private void checkDistrictContext() {
        Integer schoolYear = (Integer) getParameter(SCHOOL_YEAR_PARAM);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, schoolYear);

        QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
        int count = getBroker().getCount(query);

        m_context = (DistrictSchoolYearContext) getBroker().getBeanByQuery(query);

        String messageString = "School year not initialized.";

        ValidationMessage message = new ValidationMessage(messageString, null, null);
        validateStep(m_context == null || count > 1, false, null, message);
    }

    /**
     * Checks for any students with a grade level which does not match the YOG.
     *
     * @param school SisSchool
     */
    private void checkGradeLevels(SisSchool school) {
        Collection students = m_studentGradeLevels.get(school.getOid());

        String messageString = "#n student/students has/have a/ grade level/levels that does/do not match their YOG: ";
        String[] properties = {SisStudent.COL_NAME_VIEW, SisStudent.COL_GRADE_LEVEL, SisStudent.COL_YOG};

        ValidationMessage message = new ValidationMessage(messageString, students, properties);
        validateStep(!CollectionUtils.isEmpty(students), false, school, message);
    }

    /**
     * Checks to make sure the grade term cover maps exist for all schedule terms in the active
     * schedule.
     *
     * @param school SisSchool
     */
    private void checkGradeTermCoverMaps(SisSchool school) {
        Collection scheduleTerms = m_gradeTermCoverMaps.get(school.getActiveScheduleOid());

        String messageString = "Grade term map/maps is/are not set for #n schedule term/terms: ";
        String[] properties = {ScheduleTerm.COL_CODE};

        ValidationMessage message = new ValidationMessage(messageString, scheduleTerms, properties);
        validateStep(!CollectionUtils.isEmpty(scheduleTerms), false, school, message);
    }

    /**
     * Checks for grade term dates that fall outside the schedule start and end dates.
     *
     * @param school SisSchool
     */
    private void checkGradeTermDateRanges(SisSchool school) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(GradeTermDate.COL_SCHOOL_OID, school.getOid());
        criteria.addEqualTo(GradeTermDate.COL_DISTRICT_CONTEXT_OID, m_context.getOid());

        Criteria dateCriteria = new Criteria();
        dateCriteria.addAndCriteria(criteria);

        if (school.getActiveSchedule() != null) {
            /*
             * Start date criteria
             */
            Criteria startDateCriteria = new Criteria();
            startDateCriteria.addNotBetween(GradeTermDate.COL_START_DATE, school.getActiveSchedule().getStartDate(),
                    school.getActiveSchedule().getEndDate());

            /*
             * End date criteria
             */
            Criteria endDateCriteria = new Criteria();
            endDateCriteria.addNotBetween(GradeTermDate.COL_END_DATE, school.getActiveSchedule().getStartDate(),
                    school.getActiveSchedule().getEndDate());

            Criteria andCriteria = new Criteria();
            andCriteria.addOrCriteria(startDateCriteria);
            andCriteria.addOrCriteria(endDateCriteria);

            dateCriteria.addAndCriteria(andCriteria);
        }

        QueryByCriteria query = new QueryByCriteria(GradeTermDate.class, dateCriteria);
        query.addOrderByAscending(GradeTermDate.REL_GRADE_TERM + PATH_DELIMITER + GradeTerm.COL_GRADE_TERM_ID);

        Collection termDates = getBroker().getCollectionByQuery(query);

        String messageString = "#n grade term date/dates falls/fall outside the schedule start and end dates: ";
        String[] properties = {GradeTermDate.REL_GRADE_TERM + PATH_DELIMITER + GradeTerm.COL_GRADE_TERM_ID};

        ValidationMessage message = new ValidationMessage(messageString, termDates, properties);
        validateStep(false, !CollectionUtils.isEmpty(termDates), school, message);
    }

    /**
     * Checks for the correct number of grade term dates.
     *
     * @param school SisSchool
     */
    private void checkGradeTermDates(SisSchool school) {
        Collection gradeTermDates = m_gradeTermDates.get(school.getOid());

        boolean isTermCountMatched = true;
        if (gradeTermDates != null && !gradeTermDates.isEmpty()) {
            int termCount = 0;
            GradeTermDefinition lastDefinition = null;
            for (GradeTermDate termDate : (Collection<GradeTermDate>) gradeTermDates) {
                GradeTermDefinition definition = termDate.getGradeTerm().getGradeTermDefinition();
                if (!ObjectUtils.match(lastDefinition, definition) && lastDefinition != null) {
                    if (termCount != lastDefinition.getGradeTermsPerYear()) {
                        isTermCountMatched = false;
                        break;
                    }
                }

                termCount++;
                lastDefinition = definition;
            }

            /*
             * Checks the last grade term definition in the query.
             */
            if (lastDefinition != null && isTermCountMatched) {
                if (termCount != lastDefinition.getGradeTermsPerYear()) {
                    isTermCountMatched = false;
                }
            }
        }

        String messageString = null;
        if (CollectionUtils.isEmpty(gradeTermDates)) {
            messageString = "No grade term dates have been set.";
        } else if (!isTermCountMatched && !CollectionUtils.isEmpty(gradeTermDates)) {
            messageString = "Not all grade term dates are set.";
        }

        ValidationMessage message = new ValidationMessage(messageString, null, null);
        validateStep(CollectionUtils.isEmpty(gradeTermDates), !isTermCountMatched, school, message);
    }

    /**
     * Checks for inactive students.
     *
     * @param school SisSchool
     */
    private void checkInactiveStudents(SisSchool school) {

        Collection students = m_inactiveStudents.get(school.getOid());

        String messageString = "There is/are #n student/students with an inactive status, but have not been archived: ";
        String[] properties = {SisStudent.COL_NAME_VIEW, SisStudent.COL_LOCAL_ID};

        boolean hasStudents = !CollectionUtils.isEmpty(students);
        boolean hasStudentCount = m_inactiveStudentCounts.containsKey(school.getOid());
        int studentCount = hasStudentCount ? Integer.valueOf(m_inactiveStudentCounts.get(school.getOid())) : 0;

        ValidationMessage message = new ValidationMessage(messageString, students, properties, studentCount);
        validateStep(hasStudents && hasStudentCount, false, school, message);
    }

    /**
     * Check to see if there are any inactive students with schedule records.
     *
     * @param school SisSchool
     */
    private void checkInactiveStudentSchedules(SisSchool school) {

        Collection students = m_inactiveStudentSchedules.get(school.getOid());

        String messageString = "#n student/students is/are inactive, but has/have schedule records: ";
        String[] properties = new String[] {SisStudent.COL_NAME_VIEW, SisStudent.COL_LOCAL_ID};

        boolean hasStudents = !CollectionUtils.isEmpty(students);
        boolean hasStudentCount = m_inactiveStudentScheduleCounts.containsKey(school.getOid());
        int studentCount = hasStudentCount ? Integer.valueOf(m_inactiveStudentScheduleCounts.get(school.getOid())) : 0;

        ValidationMessage message = new ValidationMessage(messageString, students, properties, studentCount);
        validateStep(hasStudents && hasStudentCount, false, school, message);
    }

    /**
     * Checks for any students with a next school that does not match their current school.
     *
     * @param school SisSchool
     */
    private void checkNextSchool(SisSchool school) {
        Collection students = m_nonMatchingNextSchools.get(school.getOid());

        String messageString = "#n student/students has/have a next school that is different from the current school: ";
        String[] properties = {SisStudent.COL_NAME_VIEW, SisStudent.COL_LOCAL_ID};

        ValidationMessage message = new ValidationMessage(messageString, students, properties);
        validateStep(!CollectionUtils.isEmpty(students), false, school, message);
    }

    /**
     * Checks for any students with a status of <code>m_preregStatus</code>.
     *
     * @param school SisSchool
     */
    private void checkPreregStudents(SisSchool school) {
        Collection students = m_preregStudents.get(school.getOid());

        String messageString = "#n pre-registered student/students has/have not been activated: ";
        String[] properties = {SisStudent.COL_NAME_VIEW, SisStudent.COL_LOCAL_ID};

        ValidationMessage message = new ValidationMessage(messageString, students, properties);
        validateStep(!CollectionUtils.isEmpty(students), false, school, message);
    }

    /**
     * Check for no active schedule or schedule not for the current context.
     *
     * @param school SisSchool
     */
    private void checkSchedule(SisSchool school) {
        Schedule schedule = school.getActiveSchedule();
        boolean invalid = false;

        String messageString = null;
        if (schedule == null) {
            messageString = "There is no active schedule set.";
            invalid = true;
        } else if (!schedule.getDistrictContextOid().equals(m_context.getOid())) {
            messageString = "Active schedule is not for the current school year.";
            invalid = true;
        }

        ValidationMessage message = new ValidationMessage(messageString, null, null);
        validateStep(invalid, false, school, message);
    }

    /**
     * Checks for schedules with no schedule term dates or empty start or end dates.
     *
     * @param school SisSchool
     */
    private void checkScheduleNoDates(SisSchool school) {
        /*
         * Get schedule term dates for the active schedule with empty start or end dates.
         */
        X2Criteria emptyStartDateCriteria = new X2Criteria();
        emptyStartDateCriteria.addEmpty(ScheduleTermDate.COL_START_DATE, getBroker().getPersistenceKey());
        emptyStartDateCriteria.addEqualTo(
                ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.COL_SCHEDULE_OID,
                school.getActiveScheduleOid());

        X2Criteria emptyEndDateCriteria = new X2Criteria();
        emptyEndDateCriteria.addEmpty(ScheduleTermDate.COL_END_DATE, getBroker().getPersistenceKey());
        emptyEndDateCriteria.addEqualTo(
                ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.COL_SCHEDULE_OID,
                school.getActiveScheduleOid());

        Criteria dateCriteria = new Criteria();
        dateCriteria.addOrCriteria(emptyStartDateCriteria);
        dateCriteria.addOrCriteria(emptyEndDateCriteria);

        SubQuery dateSubQuery =
                new SubQuery(ScheduleTermDate.class, ScheduleTermDate.COL_SCHEDULE_TERM_OID, dateCriteria, true);

        Criteria scheduleTermCriteria1 = new Criteria();
        scheduleTermCriteria1.addIn(X2BaseBean.COL_OID, dateSubQuery);

        /*
         * Get all schedule term dates for the active schedule.
         */
        Criteria allTermDateCriteria = new Criteria();
        allTermDateCriteria.addEqualTo(
                ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.COL_SCHEDULE_OID,
                school.getActiveScheduleOid());

        SubQuery allTermDateSubQuery =
                new SubQuery(ScheduleTermDate.class, ScheduleTermDate.COL_SCHEDULE_TERM_OID, allTermDateCriteria, true);

        Criteria scheduleTermCriteria2 = new Criteria();
        scheduleTermCriteria2.addNotIn(X2BaseBean.COL_OID, allTermDateSubQuery);
        scheduleTermCriteria2.addEqualTo(ScheduleTerm.COL_SCHEDULE_OID, school.getActiveScheduleOid());

        /*
         * Query for schedule term dates with empty start or end dates or schedule terms with no
         * dates
         */
        Criteria criteria = new Criteria();
        criteria.addOrCriteria(scheduleTermCriteria1);
        criteria.addOrCriteria(scheduleTermCriteria2);

        QueryByCriteria query = new QueryByCriteria(ScheduleTerm.class, criteria);
        query.addOrderByAscending(ScheduleTerm.COL_CODE);

        Collection scheduleTerms = getBroker().getCollectionByQuery(query);

        String messageString = "#n schedule term/terms has/have no dates: ";
        String[] properties = {ScheduleTerm.COL_CODE};

        ValidationMessage message = new ValidationMessage(messageString, scheduleTerms, properties);
        validateStep(false, !CollectionUtils.isEmpty(scheduleTerms), school, message);
    }

    /**
     * Checks to see if any schedule term dates do not fall within the schedule start and end
     * dates.
     *
     * @param school SisSchool
     */
    private void checkScheduleDateConflicts(SisSchool school) {
        Schedule schedule = school.getActiveSchedule();
        Collection scheduleTerms = new ArrayList();
        boolean isWarning = false;

        if (schedule != null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.COL_SCHEDULE_OID,
                    schedule.getOid());

            /*
             * Start date criteria
             */
            Criteria startDateCriteria = new Criteria();
            startDateCriteria.addNotBetween(ScheduleTermDate.COL_START_DATE, schedule.getStartDate(),
                    schedule.getEndDate());

            /*
             * End date criteria
             */
            Criteria endDateCriteria = new Criteria();
            endDateCriteria.addNotBetween(ScheduleTermDate.COL_END_DATE, schedule.getStartDate(),
                    schedule.getEndDate());

            Criteria andCriteria = new Criteria();
            andCriteria.addOrCriteria(startDateCriteria);
            andCriteria.addOrCriteria(endDateCriteria);

            criteria.addAndCriteria(andCriteria);

            QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
            scheduleTerms = getBroker().getCollectionByQuery(query);

            isWarning = !scheduleTerms.isEmpty();
        }

        String messageString = "#n schedule term date/dates falls/fall outside the schedule start and end dates: ";
        String[] properties = {ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.COL_CODE};

        ValidationMessage message = new ValidationMessage(messageString, scheduleTerms, properties);
        validateStep(false, isWarning, school, message);
    }

    /**
     * Checks to see every school calendar date has a bell schedule set.
     *
     * @param school SisSchool
     */
    private void checkSchoolCalendarBellSchedules(SisSchool school) {
        Criteria criteria = new Criteria();

        Collection<String> bellScheduleOids = m_bellScheduleOids.get(school.getActiveScheduleOid());
        if (bellScheduleOids != null && !bellScheduleOids.isEmpty()) {
            criteria.addEqualTo(
                    SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                    school.getOid());
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER
                    + SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_context.getOid());
            criteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.valueOf(true));

            X2Criteria bellCriteria = new X2Criteria();
            bellCriteria.addEmpty(SisSchoolCalendarDate.COL_BELL_SCHEDULE_OID, getBroker().getPersistenceKey());

            Criteria orCriteria = new Criteria();
            orCriteria.addNotIn(SisSchoolCalendarDate.COL_BELL_SCHEDULE_OID, bellScheduleOids);

            bellCriteria.addOrCriteria(orCriteria);
            criteria.addAndCriteria(bellCriteria);
        } else {
            addNoMatchCriteria(criteria);
        }

        QueryByCriteria query = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
        query.addOrderByAscending(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID);
        query.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);

        Collection dates = getBroker().getCollectionByQuery(query);

        String messageString = "#n school calendar date/dates has/have no bell schedule/schedules: ";
        String[] properties = {SisSchoolCalendarDate.COL_DATE,
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID};

        ValidationMessage message = new ValidationMessage(messageString, dates, properties);
        validateStep(!CollectionUtils.isEmpty(dates), false, school, message);
    }

    /**
     * Checks to see if this years calendar has about the same amount of days as last years
     * calendar.
     *
     * @param school SisSchool
     */
    private void checkSchoolCalendarDateNumbers(SisSchool school) {
        Criteria currentCriteria = new Criteria();
        currentCriteria.addEqualTo(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                school.getOid());
        currentCriteria.addEqualTo(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                m_context.getOid());
        currentCriteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.valueOf(true));

        QueryByCriteria currentQuery = new QueryByCriteria(SisSchoolCalendarDate.class, currentCriteria);
        int currentCount = getBroker().getCount(currentQuery);

        Criteria previousCriteria = new Criteria();
        previousCriteria.addEqualTo(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                school.getOid());
        previousCriteria.addEqualTo(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.REL_DISTRICT_CONTEXT
                        + PATH_DELIMITER + DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                Integer.valueOf(m_context.getSchoolYear() - 1));
        previousCriteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.valueOf(true));

        QueryByCriteria previousQuery = new QueryByCriteria(SisSchoolCalendarDate.class, previousCriteria);
        int previousCount = getBroker().getCount(previousQuery);

        int difference = Math.abs(currentCount - previousCount);

        String messageString = "There are " + difference + (previousCount > currentCount ? " less " : " more ") +
                "school calendar dates this year than last year";

        ValidationMessage message = new ValidationMessage(messageString, null, null);
        validateStep(false, Math.abs(currentCount - previousCount) > CALENDAR_DATE_DIFFERENCE, school, message);
    }

    /**
     * Checks to see if the school calendar dates fall within the schedule start and end dates.
     *
     * @param school SisSchool
     */
    private void checkSchoolCalendarDateRanges(SisSchool school) {
        Collection dates = new ArrayList();
        Criteria criteria = new Criteria();

        if (school.getActiveSchedule() != null) {
            criteria.addEqualTo(
                    SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                    school.getOid());
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER
                    + SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_context.getOid());
            criteria.addNotBetween(SisSchoolCalendarDate.COL_DATE, school.getActiveSchedule().getStartDate(),
                    school.getActiveSchedule().getEndDate());

            QueryByCriteria query = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
            query.addOrderByAscending(
                    SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID);
            query.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);

            dates = getBroker().getCollectionByQuery(query);
        }

        String messageString = "#n school calendar date/dates falls/fall outside the schedule start and end dates: ";
        String[] properties = {SisSchoolCalendarDate.COL_DATE,
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID};

        ValidationMessage message = new ValidationMessage(messageString, dates, properties);
        validateStep(!CollectionUtils.isEmpty(dates), false, school, message);
    }

    /**
     * Checks to see if the school calendar dates have been initialized.
     *
     * @param school SisSchool
     */
    private void checkSchoolCalendarDates(SisSchool school) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                school.getOid());
        criteria.addEqualTo(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                m_context.getOid());

        QueryByCriteria query = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
        int count = getBroker().getCount(query);

        String messageString = "No calendar dates initialized for the current school year.";

        ValidationMessage message = new ValidationMessage(messageString, null, null);
        validateStep(count == 0, false, school, message);
    }

    /**
     * Checks to see if the schedule day numbers are set for in session dates.
     *
     * @param school SisSchool
     */
    private void checkSchoolCalendarScheduleDays(SisSchool school) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                school.getOid());
        criteria.addEqualTo(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                m_context.getOid());
        criteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.valueOf(true));

        Collection<Integer> scheduleDays = m_scheduleDays.get(school.getActiveScheduleOid());
        if (scheduleDays != null && !scheduleDays.isEmpty()) {
            criteria.addNotIn(SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER, scheduleDays);
        } else {
            addNoMatchCriteria(criteria);
        }

        QueryByCriteria query = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
        query.addOrderByAscending(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID);
        query.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);

        Collection dates = getBroker().getCollectionByQuery(query);

        String messageString = "#n school calendar date/dates has/have an/ invalid schedule day number/numbers: ";
        String[] properties = {SisSchoolCalendarDate.COL_DATE,
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID};

        ValidationMessage message = new ValidationMessage(messageString, dates, properties);
        validateStep(!CollectionUtils.isEmpty(dates), false, school, message);
    }

    /**
     * Checks to see if there are any students with schedules in the current school's active
     * schedule who do not have a secondary association.
     *
     * @param school SisSchool
     */
    private void checkSecondaryStudentSchedules(SisSchool school) {
        Criteria criteria = new Criteria();

        /*
         * Criteria for student schedules
         */
        Criteria scheduleCriteria = new Criteria();
        scheduleCriteria.addEqualTo(StudentSchedule.COL_SCHEDULE_OID, school.getActiveScheduleOid());
        scheduleCriteria.addNotEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                school.getOid());

        SubQuery scheduleSubQuery =
                new SubQuery(StudentSchedule.class, StudentSchedule.COL_STUDENT_OID, scheduleCriteria, true);
        criteria.addIn(X2BaseBean.COL_OID, scheduleSubQuery);

        /*
         * Criteria for secondary school associations
         */
        Criteria secondarySchoolCriteria = new Criteria();
        secondarySchoolCriteria.addEqualTo(StudentSchool.COL_SCHOOL_OID, school.getOid());
        secondarySchoolCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, m_context.getOid());
        secondarySchoolCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));

        SubQuery secondarySchoolSubQuery =
                new SubQuery(StudentSchool.class, StudentSchool.COL_STUDENT_OID, secondarySchoolCriteria, true);
        criteria.addNotIn(X2BaseBean.COL_OID, secondarySchoolSubQuery);

        QueryByCriteria otherSchedulesQuery = new QueryByCriteria(SisStudent.class, criteria);
        otherSchedulesQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        Collection students = getBroker().getCollectionByQuery(otherSchedulesQuery);

        String messageString =
                "#n student/students has/have a/  schedule record/records in the school, but no secondary association: ";
        String[] properties = new String[] {SisStudent.COL_NAME_VIEW, SisStudent.COL_LOCAL_ID};

        ValidationMessage message = new ValidationMessage(messageString, students, properties);
        validateStep(!CollectionUtils.isEmpty(students), false, school, message);
    }

    /**
     * Checks to see if all staff have been assigned a valid calendar code.
     *
     * @param school SisSchool
     */
    private void checkStaffCalendarCodes(SisSchool school) {
        Collection staff = null;

        /*
         * Criteria for active staff in the school
         */
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisStaff.COL_SCHOOL_OID, school.getOid());
        criteria.addEqualTo(SisStaff.COL_STATUS, m_staffActiveCode);

        /*
         * Throw away criteria just to join the school table
         */
        criteria.addEqualTo(SisStaff.REL_SCHOOL + "." + School.COL_SCHOOL_ID, school.getSchoolId());

        /*
         * Criteria for staff with an empty calendar code
         */
        X2Criteria emptyCalendarCriteria = new X2Criteria();
        emptyCalendarCriteria.addEmpty(SisStaff.COL_CALENDAR_ID, getBroker().getPersistenceKey());

        /*
         * Criteria for staff with a calendar code that is not for the school or the current
         * school year
         */
        X2Criteria calendarCriteria = new X2Criteria();
        calendarCriteria.addEqualToField(StaffCalendar.COL_CALENDAR_ID,
                Criteria.PARENT_QUERY_PREFIX + SisStaff.COL_CALENDAR_ID);
        calendarCriteria.addAndCriteria(OrganizationManager.getOrganizationCriteriaEqualsParent(SisStaff.REL_SCHOOL));

        SubQuery calendarSubQuery = new SubQuery(StaffCalendar.class, StaffCalendar.COL_CALENDAR_ID, calendarCriteria);
        X2Criteria orgCalendarCriteria = new X2Criteria();
        orgCalendarCriteria.addNotIn(Staff.COL_CALENDAR_ID, calendarSubQuery);

        emptyCalendarCriteria.addOrCriteria(orgCalendarCriteria);

        criteria.addAndCriteria(emptyCalendarCriteria);

        QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);
        query.addOrderByAscending(SisStaff.COL_NAME_VIEW);

        staff = getBroker().getCollectionByQuery(query);

        String messageString = "#n staff does/do not have a/ valid calendar code/codes: ";
        String[] properties = {SisStaff.COL_NAME_VIEW};

        ValidationMessage message = new ValidationMessage(messageString, staff, properties);
        validateStep(!CollectionUtils.isEmpty(staff), false, school, message);
    }

    /**
     * Checks for homerooms that are assigned to students, but not staff.
     *
     * @param school SisSchool
     */
    private void checkStaffHomerooms(SisSchool school) {
        /*
         * Criteria for staff with homerooms
         */
        X2Criteria staffCriteria = new X2Criteria();
        staffCriteria.addEqualTo(SisStaff.COL_SCHOOL_OID, school.getOid());
        staffCriteria.addNotEmpty(SisStaff.COL_HOMEROOM, getBroker().getPersistenceKey());
        staffCriteria.addEqualTo(SisStaff.COL_STATUS, m_staffActiveCode);

        SubQuery staffSubQuery = new SubQuery(SisStaff.class, SisStaff.COL_HOMEROOM, staffCriteria, true);

        /*
         * Criteria for students with homerooms that are not assigned to any staff
         */
        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, school.getOid());
        studentCriteria.addNotIn(SisStudent.COL_HOMEROOM, staffSubQuery);

        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
        studentQuery.addOrderByAscending(SisStudent.COL_HOMEROOM);

        Collection studentHomerooms = getBroker().getCollectionByQuery(studentQuery);
        removeDuplicateHomerooms(studentHomerooms);

        String messageString = "There is/are no staff assigned to #n homeroom/homerooms: ";
        String[] properties = new String[] {SisStudent.COL_HOMEROOM};

        ValidationMessage message = new ValidationMessage(messageString, studentHomerooms, properties);
        validateStep(!CollectionUtils.isEmpty(studentHomerooms), false, school, message);
    }

    /**
     * Checks to see if all students have been assigned a valid calendar code.
     *
     * @param school SisSchool
     */
    private void checkStudentCalendarCodes(SisSchool school) {
        /*
         * Criteria for active students in the school
         */
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, school.getOid());
        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        /*
         * Criteria for students with an empty calendar code
         */
        X2Criteria emptyCalendarCriteria = new X2Criteria();
        emptyCalendarCriteria.addEmpty(SisStudent.COL_CALENDAR_CODE, getBroker().getPersistenceKey());

        Criteria calendarCriteria = new Criteria();
        calendarCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_context.getOid());
        calendarCriteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, school.getOid());

        SubQuery calendarSubQuery =
                new SubQuery(SchoolCalendar.class, SchoolCalendar.COL_CALENDAR_ID, calendarCriteria, true);

        /*
         * Criteria for students with a calendar code that is not for the school or the current
         * school year
         */
        Criteria orCriteria = new Criteria();
        orCriteria.addNotIn(SisStudent.COL_CALENDAR_CODE, calendarSubQuery);

        emptyCalendarCriteria.addOrCriteria(orCriteria);
        criteria.addAndCriteria(emptyCalendarCriteria);

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        Collection students = getBroker().getCollectionByQuery(query);

        String messageString = "#n student/students does/do not have a/ valid calendar code/codes: ";
        String[] properties = {SisStudent.COL_NAME_VIEW};

        ValidationMessage message = new ValidationMessage(messageString, students, properties);
        validateStep(!CollectionUtils.isEmpty(students), false, school, message);
    }

    /**
     * Checks to see if all or no students have been assigned homerooms.
     *
     * @param school SisSchool
     */
    private void checkStudentHomerooms(SisSchool school) {
        Criteria studentCriteria = new Criteria();
        studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, school.getOid());
        studentCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        X2Criteria nonEmptyHomeroomCriteria = new X2Criteria();
        nonEmptyHomeroomCriteria.addNotEmpty(SisStudent.COL_HOMEROOM, getBroker().getPersistenceKey());
        nonEmptyHomeroomCriteria.addAndCriteria(studentCriteria);

        SubQuery studentSubQuery = new SubQuery(SisStudent.class, SisStudent.COL_HOMEROOM, nonEmptyHomeroomCriteria);
        Collection studentHomerooms = getBroker().getSubQueryCollectionByQuery(studentSubQuery);

        X2Criteria studentsNoHomeroomCriteria = new X2Criteria();
        studentsNoHomeroomCriteria.addAndCriteria(studentCriteria);

        if (!studentHomerooms.isEmpty()) {
            studentsNoHomeroomCriteria.addEmpty(SisStudent.COL_HOMEROOM, getBroker().getPersistenceKey());
        } else {
            addNoMatchCriteria(studentsNoHomeroomCriteria);
        }

        QueryByCriteria studentsNoHomeroomQuery = new QueryByCriteria(SisStudent.class, studentsNoHomeroomCriteria);
        studentsNoHomeroomQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        Collection studentsNoHomeroom = getBroker().getCollectionByQuery(studentsNoHomeroomQuery);

        String messageString = "#n student/students has/have no homeroom: ";
        String[] properties = new String[] {SisStudent.COL_NAME_VIEW};

        ValidationMessage message = new ValidationMessage(messageString, studentsNoHomeroom, properties);
        validateStep(!CollectionUtils.isEmpty(studentsNoHomeroom), false, school, message);
    }

    /**
     * Checks to see if there are any users with role that have staff view access that are not
     * flagged as staff.
     */
    private void checkUserRoles() {
        /*
         * Criteria for roles that have access to the Staff view
         */
        Criteria roleCriteria = new Criteria();
        roleCriteria.addEqualTo(Role.COL_STAFF_APPLICATION_INDICATOR, Boolean.valueOf(true));

        SubQuery roleSubQuery = new SubQuery(Role.class, X2BaseBean.COL_OID, roleCriteria);

        /*
         * Criteria for users that have roles that have access to the Staff view
         */
        Criteria userRoleCriteria = new Criteria();
        userRoleCriteria.addIn(UserRole.COL_ROLE_OID, roleSubQuery);
        userRoleCriteria.addEqualTo(UserRole.REL_USER + PATH_DELIMITER + SisUser.REL_PERSON + PATH_DELIMITER
                + SisPerson.COL_STAFF_INDICATOR, Boolean.valueOf(false));

        SubQuery userRoleSubQuery = new SubQuery(UserRole.class, UserRole.COL_USER_OID, userRoleCriteria, true);

        Criteria criteria = new Criteria();
        criteria.addIn(X2BaseBean.COL_OID, userRoleSubQuery);

        QueryByCriteria query = new QueryByCriteria(SisUser.class, criteria, true);
        query.addOrderByAscending(SisUser.COL_NAME_VIEW);

        Collection users = getBroker().getCollectionByQuery(query);

        String messageString =
                "There is/are #n user/users that have access to the staff view, but are not flagged as staff: ";
        String[] properties = {SisUser.COL_NAME_VIEW};

        ValidationMessage message = new ValidationMessage(messageString, users, properties);
        validateStep(false, !CollectionUtils.isEmpty(users), null, message);
    }

    /**
     * Returns true if the results of the passed status should be displayed.
     *
     * @param resultStatus int
     * @return boolean
     */
    private boolean displayDetails(int resultStatus) {
        return resultStatus != STATUS_COMPLETE && (m_detailsToDisplay == 0 || m_detailsToDisplay == resultStatus);
    }

    /**
     * Gets the active code list.
     *
     * @return a comma-delimited list of student "active" code strings
     */
    private String getActiveCodeList() {
        Collection<String> activeCodes = StudentManager.getActiveStudentCodeList(getOrganization());
        return "'" + StringUtils.convertCollectionToDelimitedString(activeCodes, "', '") + "'";
    }

    /**
     * Loads a Map of all schools in the district keyed on school OID.
     *
     * @return Query iterator
     */
    private void loadSchools() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.valueOf(false));
        criteria.addEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.valueOf(false));

        if (isSchoolContext()) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
        }

        QueryByCriteria query = new QueryByCriteria(SisSchool.class, criteria);
        query.addOrderByAscending(SisSchool.COL_NAME);

        m_schools = getBroker().getIteratorByQuery(query);
    }

    /**
     * Loads a map of schedule OIDs keyed to collections of schedule bell OIDs.
     */
    private void loadBellSchedules() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ScheduleBell.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_BUILD_SCENARIO_INDICATOR,
                Boolean.valueOf(false));

        String[] columns = {ScheduleBell.COL_SCHEDULE_OID, X2BaseBean.COL_OID};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ScheduleBell.class, columns, criteria);

        m_bellScheduleOids = getBroker().getGroupedColumnCollectionByQuery(query, INITIAL_MAP_SIZE);
    }

    /**
     * Loads a map of schedule OIDs keyed to collections of schedule terms that have an empty
     * grade term cover map, or a grade term cover map of all zeros.
     */
    private void loadGradeTermCoverMaps() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEmpty(ScheduleTerm.COL_GRADE_TERM_MAP, getBroker().getPersistenceKey());

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addNotContains(ScheduleTerm.COL_GRADE_TERM_MAP, "1");

        criteria.addOrCriteria(orCriteria);

        QueryByCriteria query = new QueryByCriteria(ScheduleTerm.class, criteria);
        query.addOrderByAscending(ScheduleTerm.COL_CODE);

        m_gradeTermCoverMaps =
                getBroker().getGroupedCollectionByQuery(query, ScheduleTerm.COL_SCHEDULE_OID, INITIAL_MAP_SIZE);
    }

    /**
     * Loads a map of school OIDs keyed to collections of grade term dates.
     */
    private void loadGradeTermDates() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(GradeTermDate.COL_DISTRICT_CONTEXT_OID, m_context.getOid());

        QueryByCriteria query = new QueryByCriteria(GradeTermDate.class, criteria);
        query.addOrderByAscending(
                GradeTermDate.REL_GRADE_TERM + PATH_DELIMITER + GradeTerm.COL_GRADE_TERM_DEFINITION_OID);

        m_gradeTermDates =
                getBroker().getGroupedCollectionByQuery(query, GradeTermDate.COL_SCHOOL_OID, INITIAL_MAP_SIZE);
    }

    /**
     * Loads a map of school OIDs keyed to integer counts of inactive students enrolled at that
     * school
     */
    private void loadInactiveStudentCounts() {

        // Enrollment status
        Criteria criteria =
                StudentManager.getStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS,
                        false);

        // School if in school context
        addSchoolCriteria(criteria);

        // Have to use VARCHAR for Sql Server and CHAR for MySQL
        String castType =
                DatabaseSyntaxFactory.getSyntax(getBroker().getPersistenceKey().getDBType()).getVarcharCastDataType();
        String countSql = "CAST(COUNT(*) AS " + castType + "(20)) AS studentCount";

        ColumnQuery query =
                new ColumnQuery(SisStudent.class,
                        new String[] {SisStudent.COL_SCHOOL_OID, countSql},
                        criteria);
        query.addGroupBy(SisStudent.COL_SCHOOL_OID);

        int keyIndex = 0;
        Map<Object, List> data = ((ToolBroker) getBroker()).getMapByReportQuery(query, keyIndex, INITIAL_MAP_SIZE);

        m_inactiveStudentCounts = new HashMap();

        for (Map.Entry<Object, List> entry : data.entrySet()) {
            m_inactiveStudentCounts.put((String) entry.getKey(), (String) entry.getValue().get(0));
        }
    }

    /**
     * Loads a map of school OIDs keyed to integer counts of inactive students with active schedules
     * enrolled at that school
     */
    private void loadInactiveStudentScheduleCounts() {

        Criteria criteria = new Criteria();

        // Enrollment status
        criteria.addAndCriteria(
                StudentManager.getStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS, false));

        // Active schedule
        criteria.addEqualToField(SisStudent.REL_STUDENT_SCHEDULES + PATH_DELIMITER + StudentSchedule.COL_SCHEDULE_OID,
                SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID);

        // School year
        criteria.addEqualTo(
                SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                        + SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                m_context.getOid());

        // School if in school context
        addSchoolCriteria(criteria);

        // Have to use VARCHAR for Sql Server and CHAR for MySQL
        String castType =
                DatabaseSyntaxFactory.getSyntax(getBroker().getPersistenceKey().getDBType()).getVarcharCastDataType();
        String countSql = "CAST(COUNT(DISTINCT A0.STD_ID_LOCAL) AS " + castType + "(20)) AS studentCount";

        ColumnQuery query = new ColumnQuery(SisStudent.class,
                new String[] {SisStudent.COL_SCHOOL_OID,
                        countSql},
                criteria);
        query.addGroupBy(SisStudent.COL_SCHOOL_OID);

        int countIndex = 0;
        Map<Object, List> data = ((ToolBroker) getBroker()).getMapByReportQuery(query, countIndex, INITIAL_MAP_SIZE);

        m_inactiveStudentScheduleCounts = new HashMap();

        for (Map.Entry<Object, List> entry : data.entrySet()) {
            m_inactiveStudentScheduleCounts.put((String) entry.getKey(), (String) entry.getValue().get(0));
        }
    }

    /**
     * Loads a map of school OIDs keyed to collections of inactive students.
     *
     * @throws SQLException
     *
     * @throws Exception
     */
    private void loadInactiveStudents() throws SQLException {
        m_inactiveStudents = new HashMap();

        try (SqlQueryBuilder query = SqlQueryBuilder.getBuilder(getBroker().getPersistenceKey().getDBType())) {

            List<String> returnedColumns =
                    Arrays.asList(
                            m_cols.StudentCols.STD_SKL_OID,
                            m_cols.StudentCols.STD_NAME_VIEW,
                            m_cols.StudentCols.STD_ID_LOCAL);
            List<String> groupByColumns = Arrays.asList(m_cols.StudentCols.STD_SKL_OID);

            String whereClause = "<enrollmentStatusColumn> NOT IN (<activeCodeList>)" + NL;
            whereClause = addSchoolFilter(whereClause, m_cols.StudentCols.STD_SKL_OID, 2);

            query.appendTopNGroupQuery(DETAIL_LIMIT, SisStudent.DATABASE_NAME, returnedColumns, groupByColumns,
                    returnedColumns, null, whereClause, false);

            query
                    .addReplacement("<enrollmentStatusColumn>", m_cols.StudentCols.STD_ENROLLMENT_STATUS)
                    .addReplacement("<activeCodeList>", getActiveCodeList());

            Connection connection = getBroker().borrowConnection();
            ResultSet results = query.executeQuery(connection);

            while (results.next()) {

                String key = results.getString(m_cols.StudentCols.STD_SKL_OID);

                Collection<SisStudent> studentList = m_inactiveStudents.get(key);
                if (studentList == null) {
                    studentList = new ArrayList<>();
                    m_inactiveStudents.put(key, studentList);
                }

                SisStudent inactiveStudent = new SisStudent(getBroker().getPersistenceKey());
                inactiveStudent.setNameView(results.getString(m_cols.StudentCols.STD_NAME_VIEW));
                inactiveStudent.setLocalId(results.getString(m_cols.StudentCols.STD_ID_LOCAL));
                studentList.add(inactiveStudent);
            }
        } catch (SQLException e) {
            logToolMessage(Level.SEVERE, e.getMessage(), false);
            throw e; // Fail the report
        } finally {
            getBroker().returnConnection();
        }
    }

    /**
     * Loads a map of school OIDs keyed to collections of students with inactive schedules.
     *
     * @throws SQLException
     */
    private void loadInactiveStudentSchedules() throws SQLException {
        m_inactiveStudentSchedules = new HashMap();

        SqlQueryBuilder query = SqlQueryBuilder.getBuilder(getBroker().getPersistenceKey().getDBType());
        try {
            List<String> returnedColumns =
                    Arrays.asList(
                            m_cols.StudentCols.STD_SKL_OID,
                            m_cols.StudentCols.STD_NAME_VIEW,
                            m_cols.StudentCols.STD_ID_LOCAL);
            List<String> groupByColumns = Arrays.asList(m_cols.StudentCols.STD_SKL_OID);

            List<String> joins = new ArrayList<>();
            joins.add("JOIN STUDENT_SCHEDULE ON <SSC_STD_OID> = <STD_OID>");
            joins.add("JOIN SCHOOL ON <STD_SKL_OID> = <SKL_OID>");
            joins.add("JOIN SCHOOL_SCHEDULE_CONTEXT ON <SKL_SKX_OID_ACTIV> = <SKX_OID>");
            joins.add(" AND <SSC_SCH_OID> = <SKX_SCH_OID_ACTIVE>");
            joins.add(" AND <SKX_CTX_OID> = '<yearOid>'");

            String whereClause = "WHERE <enrollmentStatus> NOT IN (<activeCodeList>)" + NL;

            whereClause = addSchoolFilter(whereClause, m_cols.StudentCols.STD_SKL_OID, 4);

            query.appendTopNGroupQuery(DETAIL_LIMIT, SisStudent.DATABASE_NAME, returnedColumns, groupByColumns,
                    returnedColumns, joins, whereClause, true);
            query
                    .addReplacement("<SSC_STD_OID>", m_cols.StudentScheduleCols.SSC_STD_OID)
                    .addReplacement("<STD_OID>", m_cols.StudentCols.STD_OID)
                    .addReplacement("<STD_SKL_OID>", m_cols.StudentCols.STD_SKL_OID)
                    .addReplacement("<SKL_OID>", m_cols.SchoolCols.SKL_OID)
                    .addReplacement("<SKL_SKX_OID_ACTIV>", m_cols.SchoolCols.SKL_SKX_OID_ACTIV)
                    .addReplacement("<SKX_OID>", m_cols.SchoolScheduleContextCols.SKX_OID)
                    .addReplacement("<SKX_CTX_OID>", m_cols.SchoolScheduleContextCols.SKX_CTX_OID)
                    .addReplacement("<SSC_SCH_OID>", m_cols.StudentScheduleCols.SSC_SCH_OID)
                    .addReplacement("<SKX_SCH_OID_ACTIVE>", m_cols.SchoolScheduleContextCols.SKX_SCH_OID_ACTIVE)
                    .addReplacement("<enrollmentStatus>", m_cols.StudentCols.STD_ENROLLMENT_STATUS)
                    .addReplacement("<activeCodeList>", getActiveCodeList())
                    .addReplacement("<yearOid>", m_context.getOid());

            Connection connection = getBroker().borrowConnection();
            ResultSet results = query.executeQuery(connection);

            while (results.next()) {
                String key = results.getString(m_cols.StudentCols.STD_SKL_OID);

                Collection<SisStudent> studentList = m_inactiveStudentSchedules.get(key);
                if (studentList == null) {
                    studentList = new ArrayList<>();
                    m_inactiveStudentSchedules.put(key, studentList);
                }

                SisStudent inactiveStudent = new SisStudent(getBroker().getPersistenceKey());
                inactiveStudent.setNameView(results.getString(m_cols.StudentCols.STD_NAME_VIEW));
                inactiveStudent.setLocalId(results.getString(m_cols.StudentCols.STD_ID_LOCAL));
                studentList.add(inactiveStudent);
            }
        } catch (SQLException e) {
            logToolMessage(Level.SEVERE, e.getMessage(), false);
            throw e; // Fail the report
        } finally {
            getBroker().returnConnection();
        }
    }

    /**
     * Loads a map of school OIDs keyed to collections of students with a next school that doesn't
     * match the current school.
     */
    private void loadNonMatchingNextSchools() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        criteria.addNotEmpty(SisStudent.COL_NEXT_SCHOOL_OID, getBroker().getPersistenceKey());
        criteria.addNotEqualToField(SisStudent.COL_NEXT_SCHOOL_OID, SisStudent.COL_SCHOOL_OID);

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        m_nonMatchingNextSchools =
                getBroker().getGroupedCollectionByQuery(query, SisStudent.COL_SCHOOL_OID, INITIAL_MAP_SIZE);
    }

    /**
     * Loads a map of school OIDs keyed to collections of students with <code>m_preregStatus</code>
     * as an enrollment status.
     */
    private void loadPreregStudents() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, m_preregStatus);

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        m_preregStudents = getBroker().getGroupedCollectionByQuery(query, SisStudent.COL_SCHOOL_OID, INITIAL_MAP_SIZE);
    }

    /**
     * Loads a map of schedule OIDs keyed to collections of schedule day numbers.
     */
    private void loadScheduleDays() {
        m_scheduleDays = new HashMap<String, Collection>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(ScheduleDay.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_BUILD_SCENARIO_INDICATOR,
                Boolean.valueOf(false));

        QueryByCriteria query = new QueryByCriteria(ScheduleDay.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ScheduleDay scheduleDay = (ScheduleDay) iterator.next();
                String scheduleOid = scheduleDay.getScheduleOid();

                Collection scheduleDayNumbers = m_scheduleDays.get(scheduleOid);
                if (scheduleDayNumbers == null) {
                    scheduleDayNumbers = new ArrayList();
                }

                scheduleDayNumbers.add(Integer.valueOf(scheduleDay.getNumber()));
                m_scheduleDays.put(scheduleOid, scheduleDayNumbers);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a map of school OIDs keyed to collections of students with invalid grade levels for
     * their YOG.
     */
    private void loadStudentGradeLevels() {
        m_studentGradeLevels = new HashMap<String, Collection>();

        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        SubQuery studentYogSubQuery = new SubQuery(SisStudent.class, SisStudent.COL_YOG, studentCriteria, true);
        Collection<BigDecimal> yogs = getBroker().getSubQueryCollectionByQuery(studentYogSubQuery);
        if (!yogs.isEmpty()) {
            Criteria gradeLevelCriteria = new Criteria();
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

            for (BigDecimal yog : yogs) {
                if (yog == null) {
                    yog = new BigDecimal("0");
                }

                List<String> gradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog.intValue(),
                        m_context.getSchoolYear(), m_gradeLevelMap);

                Criteria yogCriteria = new Criteria();
                yogCriteria.addEqualTo(SisStudent.COL_YOG, String.valueOf(yog));
                yogCriteria.addNotIn(SisStudent.COL_GRADE_LEVEL, gradeLevels);

                gradeLevelCriteria.addOrCriteria(yogCriteria);
            }

            SubQuery studentOidSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

            Criteria gradeCriteria = new Criteria();
            gradeCriteria.addAndCriteria(gradeLevelCriteria);
            gradeCriteria.addIn(X2BaseBean.COL_OID, studentOidSubQuery);

            QueryByCriteria query = new QueryByCriteria(SisStudent.class, gradeCriteria);
            query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

            m_studentGradeLevels =
                    getBroker().getGroupedCollectionByQuery(query, SisStudent.COL_SCHOOL_OID, INITIAL_MAP_SIZE);
        }
    }

    /**
     * Creates a collection of distinct student homerooms.
     *
     * @param students Collection<SisStudent>
     */
    private void removeDuplicateHomerooms(Collection<SisStudent> students) {
        Set<String> homerooms = new HashSet<String>();
        Collection<SisStudent> studentsToRemove = new ArrayList<SisStudent>();

        for (SisStudent student : students) {
            if (homerooms.contains(student.getHomeroom())) {
                studentsToRemove.add(student);
            }

            homerooms.add(student.getHomeroom());
        }

        if (!studentsToRemove.isEmpty()) {
            students.removeAll(studentsToRemove);
        }
    }

    /**
     * Validates the step.
     *
     * @param isError boolean
     * @param isWarning boolean
     * @param school SisSchool
     * @param message ValidationMessage
     */
    private void validateStep(boolean isError, boolean isWarning, SisSchool school, ValidationMessage message) {

        int status = STATUS_COMPLETE;
        if (isError) {
            status = STATUS_ERROR;
        } else if (isWarning) {
            status = STATUS_WARNING;
        }

        String stepId = String.valueOf(m_count);
        Result result = new Result(stepId, status, message);

        String schoolName = DISTRICT_ID;
        if (school != null) {
            schoolName = school.getName();
        }

        Collection<Result> results = m_results.get(schoolName);
        if (results == null) {
            results = new ArrayList<Result>();
        }

        results.add(result);
        m_results.put(schoolName, results);

        m_count++;
    }

    /**
     * Inner class designed to track the results of a single check.
     *
     * @author X2 Development Corporation
     */
    public class Result {
        private String m_colorCode;
        private String m_fieldValue;
        private String m_id;
        private String m_message;
        private int m_status;

        /**
         * Initialized a new Result object.
         *
         * @param id String
         * @param status int
         * @param message ValidationMessage
         */
        public Result(String id, int status, ValidationMessage message) {
            m_id = id;
            m_status = status;
            m_message = message.getMessage();

            switch (m_status) {
                case STATUS_ERROR:
                    m_colorCode = COLOR_CODE_ERROR;
                    break;

                case STATUS_WARNING:
                    m_colorCode = COLOR_CODE_WARNING;
                    break;

                default:
                    m_colorCode = COLOR_CODE_COMPLETE;
                    break;
            }

            initializeFieldValue();
        }

        /**
         * Returns the field value initialized in <code>initializeFieldValue</code>.
         *
         * @return String
         */
        public String getFieldValue() {
            return m_fieldValue;
        }

        /**
         * Returns the ID for the result.
         *
         * @return String
         */
        public String getId() {
            return m_id;
        }

        /**
         * Returns the message for the result.
         *
         * @return String
         */
        public String getMessage() {
            return m_message;
        }

        /**
         * Returns the status for the result.
         *
         * @return int
         */
        public int getStatus() {
            return m_status;
        }

        /**
         * Initializes the style field value to display in iReport.
         */
        private void initializeFieldValue() {
            StringBuilder fieldValue = new StringBuilder();
            fieldValue.append("<style backcolor=\"");
            fieldValue.append(m_colorCode);
            fieldValue.append("\" forecolor=\"");
            fieldValue.append(m_colorCode);
            fieldValue.append("\"> A </style>");

            m_fieldValue = fieldValue.toString();
        }
    }

    /**
     * Inner class to build a validation message based on the collection of beans.
     *
     * @author X2 Development Corporation
     */
    public class ValidationMessage {
        private StringBuilder m_message;

        /**
         * Instantiates a new validation message.
         *
         * @param message the message
         * @param beans the beans
         * @param properties the properties
         */
        public ValidationMessage(String message, Collection<X2BaseBean> beans, String[] properties) {
            this(message, beans, properties, beans == null ? -1 : beans.size());
        }

        /**
         * Constructs a new validation message.
         *
         * @param message String
         * @param beans Collection<X2BaseBean>
         * @param properties String[]
         * @param totalSize
         */
        public ValidationMessage(String message, Collection<X2BaseBean> beans, String[] properties, int totalSize) {
            m_message = new StringBuilder();

            /*
             * Append the message prefix.
             */
            if (message != null && beans != null) {
                message = formatMessage(message, totalSize);
            }

            m_message.append(message);

            /*
             * Append the details of the message.
             */
            if (beans != null && beans.size() > 0) {
                /*
                 * Determine the detail delimiter.
                 */
                String delimiter = DELIMITER_SEMI_COLON;
                if (properties.length > 1) {
                    delimiter = DELIMITER_NEW_LINE;
                    m_message.append(delimiter);
                }

                int count = 0;

                /*
                 * Append the bean details to the message.
                 */
                Iterator beanIterator = beans.iterator();
                while (beanIterator.hasNext() && count < DETAIL_LIMIT) {
                    X2BaseBean bean = (X2BaseBean) beanIterator.next();
                    m_message.append(getBeanDetails(bean, properties));

                    count++;
                    if (beanIterator.hasNext() && count < DETAIL_LIMIT) {
                        m_message.append(delimiter);
                    }
                }

                /*
                 * Display a count of records that were not displayed above
                 *
                 * This handles two cases:
                 * 1) Beans collection only contains the first DETAIL_LIMIT records. Remainder is
                 * calculated from totalSize passed in.
                 * 2) Beans collection contains all records. Remainder is calculated from how many
                 * were displayed above
                 */
                int remainder = totalSize == beans.size() ? beans.size() - count : totalSize - beans.size();
                if (remainder > 0) {
                    if (DELIMITER_SEMI_COLON.equals(delimiter)) {
                        m_message.append(" ");
                    } else {
                        m_message.append(delimiter);
                    }

                    m_message.append("(");
                    m_message.append(remainder);
                    m_message.append(" more)");
                }
            }
        }

        /**
         * Returns the validation message.
         *
         * @return String
         */
        public String getMessage() {
            return m_message.toString();
        }

        /**
         * Formats the passed message.
         *
         * @param message String
         * @param size int
         * @return String
         */
        private String formatMessage(String message, int size) {
            String formattedMessage = message;

            Matcher matcher = m_pattern.matcher(formattedMessage);
            while (matcher.find()) {
                String match = matcher.group();

                int column = COL_PLURAL;
                if (size == 1) {
                    column = COL_SINGULAR;
                }

                List<String> values = StringUtils.convertDelimitedStringToList(match, '/');
                formattedMessage = formattedMessage.replace(match, values.get(column));
            }

            formattedMessage = formattedMessage.replace(NUMBER_PATTERN, String.valueOf(size));
            return formattedMessage;
        }

        /**
         * Returns a string containing the details for the passed bean.
         *
         * @param bean X2BaseBean
         * @param properties String[]
         * @return String
         */
        private String getBeanDetails(X2BaseBean bean, String[] properties) {
            StringBuilder beanDetails = new StringBuilder();

            String primaryProperty = properties[0];
            beanDetails.append(getProperty(bean, primaryProperty, false));

            if (properties.length > 1) {
                beanDetails.append(" (");
                for (int i = 1; i < properties.length; i++) {
                    beanDetails.append(getProperty(bean, properties[i], true));

                    if (properties.length > i + 1) {
                        beanDetails.append(" ");
                    }
                }

                beanDetails.append(")");
            }

            return beanDetails.toString();
        }

        /**
         * Returns a string containing the value of the bean for the passed property. This method
         * has the option to contain a description of the property.
         *
         * @param bean X2BaseBean
         * @param property String
         * @param includeDescriptor boolean
         * @return String
         */
        private String getProperty(X2BaseBean bean, String property, boolean includeDescriptor) {
            StringBuilder propertyDetail = new StringBuilder();
            try {
                Class propertyClass = PropertyUtils.getPropertyType(bean, property);
                if (includeDescriptor) {
                    DataDictionaryField dictionaryField =
                            m_dictionary.findDataDictionaryField(bean.getClass().getName(), property);
                    if (dictionaryField != null) {
                        String fieldName = dictionaryField.getDataFieldConfig().getUserShortName();

                        propertyDetail.append(fieldName);
                        propertyDetail.append(" ");
                    }
                }

                String propertyValue = "";
                if (String.class.equals(propertyClass)) {
                    propertyValue = (String) PropertyUtils.getProperty(bean, property);
                } else {
                    ReportValueConverter converter = new ReportValueConverter(bean, m_dictionary, m_locale);
                    propertyValue = converter.getString(property);
                }

                propertyDetail.append(propertyValue);
            } catch (IllegalAccessException e) {
                // Do nothing
            } catch (InvocationTargetException e) {
                // Do nothing
            } catch (NoSuchMethodException e) {
                // Do nothing
            }

            return propertyDetail.toString();
        }
    }

    /**
     * Shortcut for finding the db name of a column
     *
     * @param className String
     * @param propertyName String
     * @return String
     */
    private String findColDbName(String className, String propertyName) {
        return m_dictionary.findDataDictionaryFieldDbName(className, propertyName);
    }
}
