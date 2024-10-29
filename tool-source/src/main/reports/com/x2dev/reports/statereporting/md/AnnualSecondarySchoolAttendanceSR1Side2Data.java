/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.md;

/* ++++ Upgraded by PhoenixUpgradeProcedure ++++ on Mon Feb 06 17:52:44 EST 2012 *//*
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ============
                                                                                    *
                                                                                    * X2 Development
                                                                                    * Corporation
                                                                                    *
                                                                                    * Copyright (c)
                                                                                    * 2002-2011 X2
                                                                                    * Development
                                                                                    * Corporation.
                                                                                    * All rights
                                                                                    * reserved.
                                                                                    *
                                                                                    * Redistribution
                                                                                    * and use in
                                                                                    * source and
                                                                                    * binary forms,
                                                                                    * with or
                                                                                    * without
                                                                                    * modification,
                                                                                    * is not
                                                                                    * permitted
                                                                                    * without
                                                                                    * express
                                                                                    * written
                                                                                    * agreement
                                                                                    * from X2
                                                                                    * Development
                                                                                    * Corporation.
                                                                                    *
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ============
                                                                                    */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.FieldAliases;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source for the "Annual Secondary School Attendance" report for Allegany.
 * <p>
 * This report has been "highly" customized for Allegany and cannot be used for other districts
 * without significant
 * changes to the java source
 *
 * @author X2 Development Corporation
 */
public class AnnualSecondarySchoolAttendanceSR1Side2Data extends ReportJavaSourceNet {

    // INPUT PARAMETERS
    private static final String ACTIVE_ONLY_PARAM = "activeOnly";
    private static final String EXCLUDE_EXCUSED_PARAM = "excludeExcused";
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String STUDENT_SORT_PARAM = "studentSort";

    // GRID FIELDS
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_STUDENT_NAME = "nameView";
    private static final String FIELD_STUDENT_OID = "studentOid";
    private static final String FIELD_SCHOOL_YEAR = "schoolYear";
    private static final String FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_ABSENT_DAYS = "absent";
    private static final String FIELD_PRESENT_DAYS = "present";
    private static final String FIELD_ENTRY_CODE = "entryCode";
    private static final String FIELD_ENTRY_DATE = "entryDate";
    private static final String FIELD_ENTRY_FROM = "entryFrom";
    private static final String FIELD_WITHDRAWAL_CODE = "withdrawalCode";
    private static final String FIELD_WITHDRAWAL_DATE = "withdrawalDate";
    private static final String FIELD_WITHDRAWAL_TO = "withdrawalTo";

    private static final String STUDENT_SORT_BY_HOMEROOM = "homeroom";
    private static final String STUDENT_SORT_BY_YOG = "yog";

    private static final String REF_TABLE_GRADE_LEVEL = "rtbGradeLevel";

    private static final String CONTINUE_ENROLLMENT_CODE = "R02";
    private static final String CALENDAR_ID_STANDARD = "Standard";
    private static final int YEAR_OF_CUTOVER = 2011;

    private SisStudent m_currentStudent;
    private boolean m_excludeExcused;

    private PlainDate m_currentDate;
    private SisDistrictSchoolYearContext m_currentSchoolYearContext;
    int m_currentSchoolYear;

    private Collection<String> m_studentOids;

    // Maps a year to the appropriate context
    private Map<Integer, SisDistrictSchoolYearContext> m_schoolYears;

    // Maps the grade to the appropriate code
    private Map<Integer, String> m_gradeLevelMap;

    // Maps student oid to the collection of all enrollment records for the student
    // private Map<String, Collection<StudentEnrollment>> m_allEnrollments = new HashMap<>();

    // Maps a string that is the school_oid + year to an academic calendar
    private Map<String, AcademicCalendar> m_allCalendars = new HashMap<>();

    // Maps a student oid to a map of absences data
    // Key for internal map is a string that is the context_oid + school_oid and the value is the
    // days absent
    private Map<String, Map<String, Double>> m_allAbsences = new HashMap<>();


    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_currentDate = new PlainDate();
        m_currentSchoolYearContext = ((SisOrganization) getOrganization()).getCurrentContext();
        m_currentSchoolYear = m_currentSchoolYearContext.getSchoolYear();
        m_excludeExcused = ((Boolean) getParameter(EXCLUDE_EXCUSED_PARAM)).booleanValue();
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        // -- load data needed to generate report --
        loadSchoolYears();
        loadGradeLevelMap();
        loadStudentOids();

        // Load and process enrollment data and build a report for each student
        // Maps a student oid to the appropriate attendance report
        Map<String, AttendanceReport> allReports = buildAttendanceReports();

        loadAllStudentAbsences();

        ReportDataGrid grid = new ReportDataGrid();
        Set<String> students = allReports.keySet();
        for (String studentOid : students) {
            AttendanceReport report = allReports.get(studentOid);
            report.processAttendanceRecords();
            report.addAttendanceRecordsToGrid(grid);
        }

        // -- Sort the grid results --
        grid.beforeTop();

        List<String> sortColumns = new ArrayList<String>();
        String sortValue = (String) getParameter(STUDENT_SORT_PARAM);
        if (sortValue != null
                && (sortValue.equals(STUDENT_SORT_BY_HOMEROOM) || sortValue.equals(STUDENT_SORT_BY_YOG))) {
            sortColumns.add(sortValue);
        }
        sortColumns.add(FIELD_STUDENT_NAME);
        sortColumns.add(FIELD_STUDENT_OID);
        sortColumns.add(FIELD_SCHOOL_YEAR);
        grid.sort(sortColumns, false);

        grid.beforeTop();

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.x2dev.sis.tools.ToolJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = (SisStudent) userData.getCurrentRecord(Student.class);
    }

    /**
     * Loads all enrollment records for each student and builds an attendance
     * report for each student. The map that is returned maps the student oid to
     * that students attendance report
     * 
     * @return the map of student attendance reports
     */
    private Map<String, AttendanceReport> buildAttendanceReports() {
        Map<String, AttendanceReport> allReports = new HashMap<>();
        Map<String, Collection<StudentEnrollment>> allEnrollments = loadAllStudentEnrollments();
        Set<String> allStudentOids = allEnrollments.keySet();

        for (String studentOid : allStudentOids) {
            Collection<StudentEnrollment> enrollments = allEnrollments.get(studentOid);
            Iterator<StudentEnrollment> itr = enrollments.iterator();

            /*
             * Collection<StudentEnrollment> referenceCodes = broker.getCollectionByQuery(query);
             * 
             * for (StudentEnrollment code : referenceCodes)
             * {
             * }
             */
            StudentEnrollment currentEnrollment = itr.hasNext() ? itr.next() : null;
            SisStudent student = currentEnrollment.getStudent();
            AttendanceReport studentReport = new AttendanceReport(student);

            // TODO replace iterator
            try {
                while (currentEnrollment != null) {
                    StudentEnrollment nextEnrollment = itr.hasNext() ? itr.next() : null;

                    if (isEntryRecord(currentEnrollment)) {
                        studentReport.addEntry(currentEnrollment, nextEnrollment);
                    }

                    currentEnrollment = nextEnrollment;
                }
            } catch (Exception e) {
                String warningFormat = "An error occurred processing enrollments for student: %s (%s)";
                String warningMessage = String.format(warningFormat, student.getNameView(), student.getOid());
                AppGlobals.getLog().warning(warningMessage);
                e.printStackTrace();
            }

            allReports.put(studentOid, studentReport);
        }

        return allReports;
    }

    /**
     * Determines if a given date is between a start date and end date (inclusive).
     *
     * @param date the date that is being checked
     * @param startDate the start date
     * @param endDate the end date
     * @return true if date is between startDate and endDate, otherwise return false
     */
    private boolean isBetween(PlainDate date, PlainDate startDate, PlainDate endDate) {
        return startDate.compareTo(date) <= 0 && endDate.compareTo(date) >= 0;
    }

    /**
     * Loads all enrollment records for all students included in this report
     * into a map that is keyed by the student oid. The value for each key in
     * the map is a collection of enrollment records that are associated with that
     * student.
     *
     * @return map of student enrollment records
     */
    private Map<String, Collection<StudentEnrollment>> loadAllStudentEnrollments() {
        Criteria criteria = new Criteria();
        criteria.addIn(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + X2BaseBean.COL_OID, m_studentOids);
        criteria.addNotEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.YOG_CHANGE);
        criteria.addNotEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.STATUS_CHANGE);

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
        query.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);
        query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_TYPE);

        return getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, m_studentOids.size());
    }

    /**
     * Loads all absences for all of the students into a map that is keyed by
     * the student oid. The value for each key in the map is another map that
     * associates a year and a school with the number of absences for the
     * student at that school during that year.
     */
    private void loadAllStudentAbsences() {
        final String SUM_OF_ABSENCES = "SUM(ATT_PORTION_ABSENT)";
        String[] columns = {StudentAttendance.COL_STUDENT_OID, StudentAttendance.COL_SCHOOL_OID, SUM_OF_ABSENCES};

        Set<Integer> allYears = m_schoolYears.keySet();
        for (Integer year : allYears) {
            SisDistrictSchoolYearContext context = m_schoolYears.get(year);
            if (!isPreAspen(context.getSchoolYear())) {
                boolean isCurrentYear = context.getOid().equals(m_currentSchoolYearContext.getOid());
                PlainDate endDate = isCurrentYear ? m_currentDate : context.getEndDate();
                Criteria criteria = new Criteria();
                criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, context.getStartDate());
                criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, endDate);
                criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

                if (m_excludeExcused) {
                    criteria.addEqualTo(StudentAttendance.COL_EXCUSED_INDICATOR, Boolean.FALSE);
                }

                criteria.addIn(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + X2BaseBean.COL_OID, m_studentOids);

                ReportQueryByCriteria query = new ReportQueryByCriteria(StudentAttendance.class, columns, criteria);
                query.addGroupBy(StudentAttendance.COL_STUDENT_OID);
                query.addGroupBy(StudentAttendance.COL_SCHOOL_OID);

                // TODO replace iterator
                ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();

                        String studentOid = (String) row[0];
                        String schoolOid = (String) row[1];
                        double absentCount = (row[2] != null) ? Double.parseDouble(row[2].toString()) : 0;

                        Map<String, Double> absences = m_allAbsences.get(studentOid);
                        if (absences == null) {
                            absences = new HashMap<String, Double>();
                        }

                        absences.put(context.getOid() + schoolOid, Double.valueOf(absentCount));
                        m_allAbsences.put(studentOid, absences);
                    }
                } finally {
                    iterator.close();
                }
            }
        }
    }

    /**
     * Loads the grade level map from the database.
     */
    private void loadGradeLevelMap() {
        m_gradeLevelMap = new HashMap<Integer, String>();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_TABLE_GRADE_LEVEL);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

        Collection<ReferenceCode> referenceCodes = getBroker().getCollectionByQuery(query);

        for (ReferenceCode code : referenceCodes) {
            if (code != null) {
                String numericGradeLevel = (String) code.getFieldValueByAlias(FieldAliases.GRADE_LEVEL_NUMERIC_VALUE);
                if (!StringUtils.isEmpty(numericGradeLevel) && StringUtils.isNumeric(numericGradeLevel)) {
                    m_gradeLevelMap.put(Integer.valueOf(numericGradeLevel), code.getCode());
                }
            }
        }

        /*
         * QueryIterator iterator = getBroker().getIteratorByQuery(query);
         * 
         * try
         * {
         * while (iterator.hasNext())
         * {
         * ReferenceCode code = (ReferenceCode) iterator.next();
         * 
         * if (code != null)
         * {
         * String numericGradeLevel = (String)
         * code.getFieldValueByAlias(FieldAliases.GRADE_LEVEL_NUMERIC_VALUE);
         * if (!StringUtils.isEmpty(numericGradeLevel) && StringUtils.isNumeric(numericGradeLevel))
         * {
         * m_gradeLevelMap.put(Integer.valueOf(numericGradeLevel), code.getCode());
         * }
         * }
         * }
         * }
         * finally
         * {
         * iterator.close();
         * }
         */ }

    /**
     * Loads all district school year contexts from 1996 to the current year into a
     * map that associates the year (as an integer) to appropriate context.
     */
    private void loadSchoolYears() {
        Criteria criteria = new Criteria();

        // school years from 1996 - current year (inclusive)
        criteria.addGreaterOrEqualThan(SisDistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(1996));
        criteria.addLessOrEqualThan(SisDistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(m_currentSchoolYear));

        QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
        query.addOrderByDescending(SisDistrictSchoolYearContext.COL_SCHOOL_YEAR);

        m_schoolYears = new HashMap<Integer, SisDistrictSchoolYearContext>();

        Collection<SisDistrictSchoolYearContext> schoolYears = getBroker().getCollectionByQuery(query);

        for (SisDistrictSchoolYearContext schoolYearContext : schoolYears) {
            m_schoolYears.put(Integer.valueOf(schoolYearContext.getSchoolYear()), schoolYearContext);
        }
    }

    /**
     * Loads the set of in session dates for a particular school for a
     * particular school year context. This method assumes that the only
     * calendar that is of interest is the Standard calendar.
     *
     * @param schoolOid String
     * @param context SisDistrictSchoolYearContext
     * @return the set of in session dates
     */
    private Set<PlainDate> loadStandardCalendar(String schoolOid, SisDistrictSchoolYearContext context) {
        Set<PlainDate> inSessionDays = new HashSet<>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_SCHOOL_OID, schoolOid);
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID, context.getOid());
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_CALENDAR_ID, CALENDAR_ID_STANDARD);

        criteria.addLessOrEqualThan(SisSchoolCalendarDate.COL_DATE, context.getEndDate());
        criteria.addGreaterOrEqualThan(SisSchoolCalendarDate.COL_DATE, context.getStartDate());

        QueryByCriteria calendarQuery = new QueryByCriteria(SchoolCalendarDate.class, criteria);

        Collection<SisSchoolCalendarDate> calendarDates = getBroker().getCollectionByQuery(calendarQuery);

        for (SisSchoolCalendarDate calendarDate : calendarDates) {
            if (calendarDate.getInSessionIndicator()) {
                inSessionDays.add(calendarDate.getDate());
            }
        }

        // TODO replace iterator
        /*
         * QueryIterator queryItr = null;
         * try
         * {
         * queryItr = getBroker().getIteratorByQuery(calendarQuery);
         * while (queryItr.hasNext())
         * {
         * SisSchoolCalendarDate calendarDate = (SisSchoolCalendarDate) queryItr.next();
         * 
         * if (calendarDate.getInSessionIndicator())
         * {
         * inSessionDays.add(calendarDate.getDate());
         * }
         * }
         * }
         * finally
         * {
         * if (queryItr != null)
         * {
         * queryItr.close();
         * }
         * }
         */
        return inSessionDays;
    }

    /**
     * Loads all student oids needed for this report into collection.
     */
    private void loadStudentOids() {
        Criteria criteria = new Criteria();

        if (m_currentStudent != null) {
            // running for one student
            criteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            // running for multiple students
            if (isSchoolContext()) {
                criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
            }

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly) {
                String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
                criteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);
            }

            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(criteria, queryBy, queryString, Student.class, X2BaseBean.COL_OID);
        }

        SubQuery subQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, criteria);
        String sortBy = (String) getParameter(STUDENT_SORT_PARAM);
        applyUserSort(subQuery, sortBy);

        m_studentOids = getBroker().getSubQueryCollectionByQuery(subQuery);
    }

    /**
     * Queries the database for days absent for a given student at a given
     * school in the date range that is passed in. This method should be used if
     * the start of the date range is after the Aspen cutover date and when the
     * student has entered the school more than one time in the school year.
     * 
     * @param studentOid the student identifier
     * @param schoolOid the school identifier
     * @param start the start date
     * @param end the end date
     * @return number of absences
     */
    double getAbsencesFromQuery(String studentOid, String schoolOid, PlainDate start, PlainDate end) {
        double result = 0.0;
        final String SUM_OF_ABSENCES = "SUM(ATT_PORTION_ABSENT)";
        String[] columns = {SUM_OF_ABSENCES};

        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, studentOid);
        criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, schoolOid);
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, start);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, end);
        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

        if (m_excludeExcused) {
            criteria.addEqualTo(StudentAttendance.COL_EXCUSED_INDICATOR, Boolean.FALSE);
        }

        criteria.addIn(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + X2BaseBean.COL_OID, m_studentOids);

        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentAttendance.class, columns, criteria);
        // TODO replace iterator
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            if (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                result = (row[0] != null) ? Double.parseDouble(row[0].toString()) : 0;
            }
        } finally {
            iterator.close();
        }

        return result;
    }

    /**
     * Looks up the number of days absent for a given student at a given school
     * in a given year. This method should be used if the year is after the
     * Aspen cutover date and only if the student has not entered the school
     * more than once in the same year.
     * 
     * @param studentOid the student identifier
     * @param schoolOid the school identifier
     * @param year the school year
     * @return the number of days absent
     */
    double getAbsencesFromAttendanceTable(String studentOid, String schoolOid, int year) {
        double absences = 0.0;
        String contextOid = m_schoolYears.get(Integer.valueOf(year)).getOid();

        Map<String, Double> studentYearlyAbsences = m_allAbsences.get(studentOid);
        String absenceLookUpKey = contextOid + schoolOid;
        if (studentYearlyAbsences != null && studentYearlyAbsences.get(absenceLookUpKey) != null) {
            absences = studentYearlyAbsences.get(absenceLookUpKey).doubleValue();
        }

        return absences;
    }

    /**
     * returns the number of days absent based on an enrollment record that is
     * before the Aspen cutover date.
     * 
     * @param enrollment
     *        enrollment record
     * @return the number of days absent
     */
    Double getAbsencesFromEnrollment(StudentEnrollment enrollment) {
        double absences = 0.0;

        // TODO reference by alias
        if (StringUtils.isNumeric(enrollment.getFieldB001())) {
            absences += Double.parseDouble(enrollment.getFieldB001());
        }

        if (StringUtils.isNumeric(enrollment.getFieldB002())) {
            absences += Double.parseDouble(enrollment.getFieldB002());
        }

        if (StringUtils.isNumeric(enrollment.getFieldB003())) {
            absences += Double.parseDouble(enrollment.getFieldB003());
        }

        if (StringUtils.isNumeric(enrollment.getFieldB004())) {
            absences += Double.parseDouble(enrollment.getFieldB004());
        }

        if (StringUtils.isNumeric(enrollment.getFieldB005())) {
            absences += Double.parseDouble(enrollment.getFieldB005());
        }

        if (StringUtils.isNumeric(enrollment.getFieldB006())) {
            absences += Double.parseDouble(enrollment.getFieldB006());
        }

        if (StringUtils.isNumeric(enrollment.getFieldB007())) {
            absences += Double.parseDouble(enrollment.getFieldB007());
        }

        if (StringUtils.isNumeric(enrollment.getFieldB008())) {
            absences += Double.parseDouble(enrollment.getFieldB008());
        }

        if (StringUtils.isNumeric(enrollment.getFieldB009())) {
            absences += Double.parseDouble(enrollment.getFieldB009());
        }

        if (StringUtils.isNumeric(enrollment.getFieldB010())) {
            absences += Double.parseDouble(enrollment.getFieldB010());
        }

        return (Double.valueOf(absences));
    }

    /**
     * Finds the academic calendar for a school in a given year.
     * 
     * @param schoolOid school identifier
     * @param year year for the academic calendar
     * @return the academic calendar with all in session days, null if calendar is not found for
     *         school and year
     */
    AcademicCalendar getAcademicCalendar(String schoolOid, int year) {
        SisDistrictSchoolYearContext context = m_schoolYears.get(Integer.valueOf(year));
        AcademicCalendar acedemicCalendar = m_allCalendars.get(schoolOid + "-" + context.getSchoolYear());

        if (acedemicCalendar == null) {
            Set<PlainDate> inSessionDays = loadStandardCalendar(schoolOid, context);
            acedemicCalendar = new AcademicCalendar(schoolOid, context.getSchoolYear(), inSessionDays);
            m_allCalendars.put(acedemicCalendar.m_oid, acedemicCalendar);
        }

        return acedemicCalendar;
    }

    /**
     * Returns a string that represents the grade level given the year of graduation and the school
     * year.
     * 
     * @param yog year of graduation
     * @param schoolYear the school year for this grade
     * @return the string representation of the school year
     */
    String getGradeLevel(int yog, int schoolYear) {
        String gradeLevel = "";

        if (yog > 0) {
            int numericGradeLevel = 12 - (yog - schoolYear);
            if (numericGradeLevel > 0) {
                gradeLevel = m_gradeLevelMap.get(Integer.valueOf(numericGradeLevel));
            } else if (numericGradeLevel == 0) {
                gradeLevel = "K";
            } else if (numericGradeLevel <= -1) {
                gradeLevel = "PK";
            }
        }

        return gradeLevel;
    }

    /**
     * Looks up the district school year context for a given date.
     *
     * @param date the date
     * @return the district school year context
     */
    SisDistrictSchoolYearContext getSchoolYearContext(PlainDate date) {
        SisDistrictSchoolYearContext schoolYear = null;

        for (SisDistrictSchoolYearContext context : m_schoolYears.values()) {
            if (isBetween(date, context.getStartDate(), context.getEndDate())) {
                schoolYear = context;
                break;
            }
        }

        // At the cross over point, there is a gap where there are days in the summer that between
        // the start and stop date of any school year context. This can be a problem in the rare
        // case a student enrollment record falls in this date.
        if (schoolYear == null) {
            boolean afterEndOf2010Year = date.compareTo(m_schoolYears.get(Integer.valueOf(2010)).getEndDate()) > 0;
            boolean beforeStartOf2011Year = date.compareTo(m_schoolYears.get(Integer.valueOf(2011)).getStartDate()) < 0;

            if (afterEndOf2010Year && beforeStartOf2011Year) {
                schoolYear = m_schoolYears.get(Integer.valueOf(2011));
            }
        }

        return schoolYear;
    }

    /**
     * Determines whether the first character of the enrollment code begins with an R, N, or E.
     *
     * @param record StudentEnrollment
     * @return returns true first character indicates an entry enrollment otherwise returns false
     */
    boolean isEntryRecord(StudentEnrollment record) {
        char status = Character.toUpperCase(record.getEnrollmentCode().charAt(0));
        return status == 'R' || status == 'N' || status == 'E';
    }

    /**
     * Determines whether a particular year comes before the cutover to Aspen.
     *
     * @param year int
     * @return true if year is before YEAR_OF_CUTOVER otherwise return false
     */
    boolean isPreAspen(int year) {
        return year < YEAR_OF_CUTOVER;
    }

    /**
     * Determines whether the first character of the enrollment code begins with an C, W, or T.
     *
     * @param record StudentEnrollment
     * @return true first character indicates a withdraw enrollment otherwise returns false
     */
    boolean isWithdrawRecord(StudentEnrollment record) {
        char status = Character.toUpperCase(record.getEnrollmentCode().charAt(0));
        return status == 'C' || status == 'W' || status == 'T';
    }

    /**
     * 
     * The AcademicCalendar is used to manage the in session dates for a
     * particular school and year. This class is used to the days present for a
     * student given an entry and withdraw date in that year.
     * 
     */
    private class AcademicCalendar {
        String m_oid;
        PlainDate m_firstDay;
        PlainDate m_lastDay;
        Map<PlainDate, Integer> m_dayNumberMap;
        Set<PlainDate> m_inSessionDays;

        /**
         * Instantiates a new academic calendar.
         *
         * @param schoolOid String
         * @param year int
         * @param inSessionDays Set<PlainDate>
         */
        public AcademicCalendar(String schoolOid, int year, Set<PlainDate> inSessionDays) {
            m_oid = schoolOid + "-" + year;
            this.m_inSessionDays = new TreeSet<>(inSessionDays);
            m_dayNumberMap = new HashMap<>();

            int dayNumber = 1;
            for (PlainDate theDate : this.m_inSessionDays) {
                if (dayNumber == 1) {
                    m_firstDay = theDate;
                } else if (dayNumber == this.m_inSessionDays.size()) {
                    m_lastDay = theDate;
                }
                m_dayNumberMap.put(theDate, Integer.valueOf(dayNumber));
                dayNumber++;
            }
        }

        /**
         * Contains.
         *
         * @param date PlainDate
         * @return true, if successful
         */
        public boolean contains(PlainDate date) {
            return m_dayNumberMap.get(date) != null;
        }

        /**
         * Gets the number of days.
         *
         * @param d1 PlainDate
         * @param d2 PlainDate
         * @return int
         */
        public int getNumberOfDays(PlainDate d1, PlainDate d2) {
            if (m_firstDay == null && m_lastDay == null) {
                return 0;
            }

            d1 = (d1 == null) ? m_firstDay : findClosestDate(d1);
            d2 = (d2 == null) ? m_lastDay : findClosestDate(d2);

            // if d1 is the same as d2 or later then return 0 (no days between the date)
            if (d1.compareTo(d2) >= 0) {
                return 0;
            }

            return m_dayNumberMap.get(d2).intValue() - m_dayNumberMap.get(d1).intValue() + 1;
        }

        /**
         * Find closest date.
         *
         * @param date PlainDate
         * @return PlainDate
         */
        public PlainDate findClosestDate(PlainDate date) {
            if (contains(date)) {
                return date;
            } else if (date.before(m_firstDay)) {
                return m_firstDay;
            } else if (date.after(m_lastDay)) {
                return m_lastDay;
            }

            PlainDate previous = m_firstDay;
            PlainDate next = null;

            // TODO replace iterator
            Iterator<PlainDate> itr = m_inSessionDays.iterator();
            while (itr.hasNext() && date != next) {
                next = itr.next();
                if (date.after(previous) && date.before(next)) {
                    date = next;
                }
            }

            return date;
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "(" + m_oid + ")" + " firstDay=" + m_firstDay + " lastDay=" + m_lastDay + " in session days="
                    + m_inSessionDays.size();
        }
    }


    /**
     * 
     * The AttendanceReport class manages all of the attendance records for a
     * given student. Each student will have one attendance report.
     * 
     */
    private class AttendanceReport {
        private SisStudent m_student;
        private List<AttendanceRecord> m_attendanceRecords;


        /**
         * Instantiates a new attendance report.
         *
         * @param student SisStudent
         */
        public AttendanceReport(SisStudent student) {
            this.m_student = student;
            m_attendanceRecords = new ArrayList<AttendanceRecord>();
        }

        /**
         * Adds the attendance records to grid.
         *
         * @param grid ReportDataGrid
         */
        public void addAttendanceRecordsToGrid(ReportDataGrid grid) {
            for (AttendanceRecord attendanceRecord : m_attendanceRecords) {
                grid.append();
                grid.set(FIELD_STUDENT, m_student);
                grid.set(FIELD_STUDENT_NAME, m_student.getNameView());
                grid.set(FIELD_SCHOOL_YEAR, Integer.valueOf(attendanceRecord.m_year));
                grid.set(FIELD_GRADE_LEVEL, attendanceRecord.m_grade);
                grid.set(FIELD_SCHOOL_NAME, attendanceRecord.m_schoolName);
                grid.set(FIELD_ENTRY_CODE, attendanceRecord.m_entryCode);
                grid.set(FIELD_ENTRY_DATE, attendanceRecord.m_entryDate);
                grid.set(FIELD_WITHDRAWAL_CODE, attendanceRecord.m_withdrawCode);
                grid.set(FIELD_WITHDRAWAL_DATE, attendanceRecord.m_withdrawDate);
                grid.set(FIELD_ENTRY_FROM, attendanceRecord.m_entryFrom);
                grid.set(FIELD_WITHDRAWAL_TO, attendanceRecord.m_withdrawTo);

                grid.set(FIELD_ABSENT_DAYS, Double.valueOf(attendanceRecord.m_daysAbsent));
                grid.set(FIELD_PRESENT_DAYS, Double.valueOf(attendanceRecord.m_daysPresent));
            }
        }

        /**
         * Adds the entry.
         *
         * @param entry AttendanceRecord
         */
        public void addEntry(AttendanceRecord entry) {
            if (m_attendanceRecords.isEmpty()) {
                m_attendanceRecords.add(entry);
            } else {
                AttendanceRecord last = getLastEntry();
                boolean duplicate =
                        last != null && last.isContinuation() && entry.isContinuation() && entry.m_year == last.m_year;
                if (!duplicate) {
                    m_attendanceRecords.add(entry);
                }
            }
        }

        /**
         * Adds the entry.
         *
         * @param currentEnrollment StudentEnrollment
         * @param nextRecord StudentEnrollment
         */
        public void addEntry(StudentEnrollment currentEnrollment, StudentEnrollment nextRecord) {
            SisDistrictSchoolYearContext context = getSchoolYearContext(currentEnrollment.getEnrollmentDate());

            AttendanceRecord attendanceRecord = new AttendanceRecord(currentEnrollment, context.getSchoolYear());
            addEntry(attendanceRecord);

            int yearOfCurrentRecord = context.getSchoolYear();
            int yearOfNextRecord = (nextRecord == null) ? m_currentSchoolYear
                    : getSchoolYearContext(nextRecord.getEnrollmentDate()).getSchoolYear();

            int gap = yearOfNextRecord - yearOfCurrentRecord; // the gap represents the number of
                                                              // years that are missing R02 records

            if (nextRecord == null) {
                if (gap > 0) {
                    // CASE: Current record is the last, but R02 entries are missing at the end
                    // until current year
                    // ACTION: Fill in attendance records until current year
                    fillInRecords(currentEnrollment, yearOfCurrentRecord, gap);
                }
            } else {
                if (isWithdrawRecord(nextRecord) && gap == 0) {
                    // CASE: Next record is a withdraw and needs to be attached to the last
                    // attendance record in list
                    // ACTION: Update the last attendance record in the list with withdraw
                    // information
                    getLastEntry().setWithdrawInformation(nextRecord);
                } else if (isWithdrawRecord(nextRecord) && gap > 0) {
                    // CASE: Next record is a withdraw, but a gap in enrollment information has been
                    // detected
                    // ACTION: Fill in R02 attendance entries up to withdraw, and add this withdraw
                    // information to the last entry
                    fillInRecords(currentEnrollment, yearOfCurrentRecord, gap);
                    getLastEntry().setWithdrawInformation(nextRecord);
                } else if (isEntryRecord(nextRecord) && gap > 1) {
                    // CASE: Next record is an entry, but there is a gap that needs to be filled in
                    // ACTION: Fill in R02 attendance entries, and the next attendance record will
                    // be add in next loop through
                    fillInRecords(currentEnrollment, yearOfCurrentRecord, gap - 1);
                }
            }
        }

        /**
         * Fill in records.
         *
         * @param currentRecord StudentEnrollment
         * @param currentYear int
         * @param gap int
         */
        public void fillInRecords(StudentEnrollment currentRecord, int currentYear, int gap) {
            AttendanceRecord lastRecord = getLastEntry();

            if (lastRecord.m_withdrawCode != null) {
                return;
            }

            for (int i = 1; i <= gap; i++) {
                int year = currentYear + i;
                SisSchool school = currentRecord.getSchool();
                AcademicCalendar acedemicCalendar = getAcademicCalendar(school.getOid(), year);
                int yog = currentRecord.getYog();
                m_attendanceRecords.add(new AttendanceRecord(year, acedemicCalendar.m_firstDay, school, yog));
            }
        }

        /**
         * Gets the enrollment count.
         *
         * @return Map
         */
        public Map<String, Integer> getEnrollmentCount() {
            Map<String, Integer> enrollmentCountMap = new HashMap<>();
            for (AttendanceRecord record : m_attendanceRecords) {
                String enrollmentCountKey = record.m_schoolOid + record.m_year;
                Integer count = enrollmentCountMap.get(enrollmentCountKey);
                if (count == null) {
                    count = Integer.valueOf(0);
                }
                enrollmentCountMap.put(enrollmentCountKey, Integer.valueOf(count.intValue() + 1));
            }

            return enrollmentCountMap;
        }

        /**
         * Gets the last entry.
         *
         * @return Attendance record
         */
        public AttendanceRecord getLastEntry() {
            return (m_attendanceRecords.size() > 0) ? m_attendanceRecords.get(m_attendanceRecords.size() - 1) : null;
        }

        /**
         * Process attendance records.
         */
        public void processAttendanceRecords() {
            Map<String, Integer> enrollmentCountMap = getEnrollmentCount();
            int numberOfRecords = m_attendanceRecords.size();

            for (int i = 0; i < numberOfRecords; i++) {
                AttendanceRecord record = m_attendanceRecords.get(i);
                String enrollmentCountKey = record.m_schoolOid + record.m_year;
                AcademicCalendar acedemicCalendar = getAcademicCalendar(record.m_schoolOid, record.m_year);

                boolean requiresEntryFrom = i > 0 && m_attendanceRecords.get(i - 1).m_withdrawCode != null;
                boolean requiresWithdrawTo = i < m_attendanceRecords.size() - 1 && record.m_withdrawCode != null;

                record.m_entryFrom = requiresEntryFrom ? m_attendanceRecords.get(i - 1).m_schoolName : "";
                record.m_withdrawTo = requiresWithdrawTo ? m_attendanceRecords.get(i + 1).m_schoolName : "";

                // Set the attendance for this record if the school year is not pre-Aspen
                // NOTE: Attendance for years pre-Aspen is set when the attendance record was
                // created
                if (!isPreAspen(record.m_year)) {
                    if (enrollmentCountMap.get(enrollmentCountKey).intValue() > 1) {
                        // CASE: Student has multiple entries to this school in the same year, need
                        // to lookup attendance
                        // from database directly given the entry and withdraw dates for this record
                        PlainDate start = record.m_entryDate;
                        PlainDate end =
                                (record.m_withdrawDate == null) ? acedemicCalendar.m_lastDay : record.m_withdrawDate;
                        record.m_daysAbsent = getAbsencesFromQuery(m_student.getOid(), record.m_schoolOid, start, end);
                    } else {
                        // CASE: Student does not have multiple entries to this school in the same
                        // year, lookup attendance
                        // from data loaded from the database already
                        record.m_daysAbsent =
                                getAbsencesFromAttendanceTable(m_student.getOid(), record.m_schoolOid, record.m_year);
                    }
                }

                // Calculate number of days present from the academic calendar based on the entry
                // and withdraw dates
                double totalDays = acedemicCalendar.getNumberOfDays(record.m_entryDate, record.m_withdrawDate);
                record.m_daysPresent = (totalDays - record.m_daysAbsent < 0) ? 0 : totalDays - record.m_daysAbsent;
            }
        }
    }

    /**
     * The AttendanceRecord class holds the data needed to fill out one row in
     * the attendance report for a given student.
     */
    private class AttendanceRecord {
        int m_year;
        String m_schoolName;
        String m_schoolOid;
        PlainDate m_entryDate;
        String m_entryCode;
        String m_entryFrom = "";
        String m_withdrawCode;
        PlainDate m_withdrawDate;
        String m_withdrawTo = "";
        double m_daysAbsent = 0.0;
        double m_daysPresent = 0.0;
        String m_grade;

        /**
         * Instantiates a new attendance record.
         *
         * @param year int
         * @param entryDate PlainDate
         * @param school SisSchool
         * @param yog int
         */
        public AttendanceRecord(int year, PlainDate entryDate, SisSchool school, int yog) {
            this.m_year = year;
            this.m_entryDate = entryDate;
            m_entryCode = CONTINUE_ENROLLMENT_CODE;
            m_schoolName = school.getName();
            m_schoolOid = school.getOid();
            m_grade = getGradeLevel(yog, year);
        }

        /**
         * Instantiates a new attendance record.
         *
         * @param entry StudentEnrollment
         * @param year int
         */
        public AttendanceRecord(StudentEnrollment entry, int year) {
            this.m_year = year;
            m_entryCode = entry.getEnrollmentCode();
            m_entryDate = entry.getEnrollmentDate();
            m_schoolName = entry.getSchool().getName();
            m_schoolOid = entry.getSchool().getOid();
            m_grade = getGradeLevel(entry.getYog(), this.m_year);
            m_daysAbsent = getAbsencesFromEnrollment(entry).doubleValue();
        }

        /**
         * Checks if is continuation.
         *
         * @return true, if is continuation
         */
        public boolean isContinuation() {
            return m_entryCode != null && m_entryCode.equals(CONTINUE_ENROLLMENT_CODE);
        }

        /**
         * Sets the withdraw information.
         *
         * @param withdraw void
         */
        public void setWithdrawInformation(StudentEnrollment withdraw) {
            m_withdrawCode = withdraw.getEnrollmentCode();
            m_withdrawDate = withdraw.getEnrollmentDate();
        }
    }

    /*
     * private void logEnrollments(String studentOid, Collection<StudentEnrollment> enrollments)
     * {
     * StringBuilder enrollmentData = new StringBuilder("\nSTUDENT: ");
     * enrollmentData.append(studentOid);
     * if (!CollectionUtils.isEmpty(enrollments))
     * {
     * for (StudentEnrollment enrollment : enrollments)
     * {
     * enrollmentData.append('\n');
     * enrollmentData.append(enrollmentToString(enrollment));
     * }
     * }
     * AppGlobals.getLog().log(Level.INFO, enrollmentData.toString());
     * }
     */


    /*
     * private String enrollmentToString(StudentEnrollment enrollment)
     * {
     * return String.format("[%s] date=%s grade=%s school=%s code=%s", enrollment.getOid(),
     * enrollment.getEnrollmentDate(),
     * Integer.valueOf(enrollment.getYog()),
     * enrollment.getSchool().getName(),
     * enrollment.getEnrollmentCode());
     * }
     */
}
