/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.md;

import static com.follett.fsc.core.k12.beans.SystemPreferenceDefinition.STUDENT_ACTIVE_CODE;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the "Attendance for MSDE Quarterly Metrics" report for Allegany.
 *
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class MSDEQuarterlyMetricsAttendanceData extends ReportJavaSourceNet {
    /**
     * Name for the "date" report parameter. This value is a PlainDate.
     */
    private static final String DATE_END_PARAM = "endDate";
    private static final String DATE_START_PARAM = "startDate";

    // Grid fields
    private static final String COL_ASIAN = "asian";
    private static final String COL_ATTENDANCE = "attendance";
    private static final String COL_BLACK = "black";
    private static final String COL_FARMS = "farms";
    private static final String COL_FEMALE = "female";
    private static final String COL_GRADE_LEVEL = "gradeLevel";
    private static final String COL_HISPANIC = "hispanic";
    private static final String COL_IEP = "iep";
    private static final String COL_INDIAN = "indian";
    private static final String COL_LEP = "lep";
    private static final String COL_MALE = "male";
    private static final String COL_MULTIPLE = "multiple";
    private static final String COL_PACIFIC = "pacificIsland";
    private static final String COL_WHITE = "white";

    // Report parameters
    private static final String END_DATE_PARAM = "endDate";
    private static final String START_DATE_PARAM = "startDate";

    /*
     * List of grades to include by state code
     */
    private static final List<String> GRADES_INCLUDED =
            Arrays.asList("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12");

    // Column suffixes
    private static final String MEMBERSHIP_SUFFIX = "_membership";
    private static final String PERCENT_SUFFIX = "_percent";
    private static final String PRESENT_SUFFIX = "_present";
    private static final String STUDENTS_SUFFIX = "_students";

    // Race code constants
    private static final String RACE_ASIAN = "Asian";
    private static final String RACE_BLACK = "Black";
    private static final String RACE_INDIAN = "Am Indian or Alaska Nat";
    private static final String RACE_PACIFIC_ISLANDER = "NatHawaiian Pacific Is";
    private static final String RACE_WHITE = "White";

    // Program codes
    private static final String PROGRAM_CODE_ELL = "ELL";
    private static final String PROGRAM_CODE_IEP = "IEP";

    // Aliases
    private static final String ALIAS_STD_FARMS = "FARMS";
    private static final String ALIAS_STD_SPED_PGM = "DOE SPED 504";

    /**
     * Class members.
     */
    private Map<String, Map<String, Set<PlainDate>>> m_calendarLookups;
    private PlainDate m_endDate;
    private EnrollmentManager m_enrollmentManager;
    private Set<String> m_firstDayMembers;
    private Map<String, Collection<String>> m_raceCodes;
    private PlainDate m_startDate;
    private Map<String, Integer> m_studentAttendance;
    private Criteria m_studentCriteria;
    private Collection<String> m_studentPrograms;
    private List<String> m_totalColumns;

    /**
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        buildCriteria();
        loadAttendance();
        loadFirstDayMembers();
        loadCalendarLookups();
        loadStudentPrograms();
        loadRaceCodes();

        ReportDataGrid grid = new ReportDataGrid();
        Map<String, Integer> rowMap = initializeGrid(grid);

        QueryByCriteria query = new QueryByCriteria(Student.class, m_studentCriteria);
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                Map<String, Set<PlainDate>> calendarLookup = m_calendarLookups.get(student.getSchoolOid());
                Set<PlainDate> sessionDates = calendarLookup.get(student.getCalendarCode());

                boolean isInitiallyMember = m_firstDayMembers.contains(student.getOid());

                double absences = getAbsences(student.getOid());
                double membershipDays = m_enrollmentManager.getMembershipTotal(student,
                        sessionDates,
                        isInitiallyMember,
                        m_startDate,
                        m_endDate,
                        student.getSchool());
                double presentDays = 0.0;
                if (membershipDays >= absences) {
                    presentDays = membershipDays - absences;
                }

                grid.gotoRow(rowMap.get(student.getGradeLevel()));

                List<String> studentColumns = getStudentColumns(student);
                for (String column : studentColumns) {
                    incrementCount(grid, column + MEMBERSHIP_SUFFIX, membershipDays);
                    incrementCount(grid, column + PRESENT_SUFFIX, presentDays);
                    incrementCount(grid, column + STUDENTS_SUFFIX, 1.0);
                }
            }
        } finally {
            students.close();
        }

        calculatePercents(grid);

        addParameter(START_DATE_PARAM, m_startDate);
        addParameter(END_DATE_PARAM, m_endDate);

        grid.beforeTop();
        return grid;
    }

    /**
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_startDate = (PlainDate) getParameter(DATE_START_PARAM);
        m_endDate = (PlainDate) getParameter(DATE_END_PARAM);

        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_calendarLookups = new HashMap<String, Map<String, Set<PlainDate>>>();
        m_firstDayMembers = new HashSet<String>();

        m_totalColumns = Arrays.asList(COL_ATTENDANCE, COL_MALE, COL_FEMALE, COL_INDIAN, COL_ASIAN, COL_BLACK,
                COL_WHITE, COL_PACIFIC, COL_MULTIPLE, COL_LEP, COL_FARMS, COL_IEP, COL_HISPANIC);
    }

    /**
     * Builds the <code>m_studentCriteria</code> object.
     */
    private void buildCriteria() {
        /*
         * Criteria for students who are currently active in the school.
         */
        m_studentCriteria = new Criteria();
        m_studentCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS,
                PreferenceManager.getPreferenceValue(getOrganization(), STUDENT_ACTIVE_CODE));

        if (isSchoolContext()) {
            m_studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
        }

        /*
         * Criteria for students who withdrew within the date range.
         */
        Criteria withdrawalCriteria = new Criteria();
        withdrawalCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        withdrawalCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_endDate);

        if (isSchoolContext()) {
            withdrawalCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
        }

        SubQuery withdrawalSubQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, withdrawalCriteria);

        /*
         * Criteria student students who entered the school after the end date. These students
         * will not be included for this school.
         */
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_endDate);

        if (isSchoolContext()) {
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
        }

        SubQuery enrollmentSubQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);

        Criteria orCriteria = new Criteria();
        orCriteria.addIn(X2BaseBean.COL_OID, withdrawalSubQuery);
        orCriteria.addNotIn(X2BaseBean.COL_OID, enrollmentSubQuery);

        m_studentCriteria.addOrCriteria(orCriteria);

        m_studentCriteria.addIn(Student.COL_GRADE_LEVEL, loadGradeLevels());
    }

    /**
     * Calculates the percent for <code>m_totalColumns</code>.
     *
     * @param grid
     */
    private void calculatePercents(ReportDataGrid grid) {
        grid.beforeTop();
        while (grid.next()) {
            for (String column : m_totalColumns) {
                double membershipDays = (Double) grid.get(column + MEMBERSHIP_SUFFIX);
                double presentDays = (Double) grid.get(column + PRESENT_SUFFIX);

                if (membershipDays > 0) {
                    double percentPresent = (presentDays / membershipDays) * 100.0;
                    grid.set(column + PERCENT_SUFFIX, percentPresent);
                }
            }
        }
    }

    /**
     * Returns the number of absences for the passed student OID.
     *
     * @param studentOid
     *
     * @return double
     */
    private double getAbsences(String studentOid) {
        double absences = 0.0;

        if (m_studentAttendance.containsKey(studentOid)) {
            absences = m_studentAttendance.get(studentOid);
        }

        return absences;
    }

    /**
     * Returns a list of columns which apply to the passed student.
     *
     * @param student
     *
     * @return List<String>
     */
    private List<String> getStudentColumns(SisStudent student) {
        List<String> studentColumns = new ArrayList<String>();
        studentColumns.add(COL_ATTENDANCE);

        /*
         * Add gender columns
         */
        if (Person.GENDER_MALE.equals(student.getPerson().getGenderCode())) {
            studentColumns.add(COL_MALE);
        } else if (Person.GENDER_FEMALE.equals(student.getPerson().getGenderCode())) {
            studentColumns.add(COL_FEMALE);
        }

        /*
         * Add race columns
         */
        Collection<String> raceCodes = m_raceCodes.get(student.getPersonOid());
        if (!CollectionUtils.isEmpty(raceCodes)) {
            if (raceCodes.size() > 1) {
                studentColumns.add(COL_MULTIPLE);
            } else {
                if (raceCodes.contains(RACE_ASIAN)) {
                    studentColumns.add(COL_ASIAN);
                }

                if (raceCodes.contains(RACE_BLACK)) {
                    studentColumns.add(COL_BLACK);
                }

                if (raceCodes.contains(RACE_INDIAN)) {
                    studentColumns.add(COL_INDIAN);
                }

                if (raceCodes.contains(RACE_PACIFIC_ISLANDER)) {
                    studentColumns.add(COL_PACIFIC);
                }

                if (raceCodes.contains(RACE_WHITE)) {
                    studentColumns.add(COL_WHITE);
                }
            }
        }

        /*
         * Add special education columns
         */
        if (student.getFieldValueByAlias(ALIAS_STD_SPED_PGM) != null
                && PROGRAM_CODE_IEP.equals(((String) student.getFieldValueByAlias(ALIAS_STD_SPED_PGM)).trim())) {
            studentColumns.add(COL_IEP);
        }

        if (BooleanAsStringConverter.TRUE.equals(student.getFieldValueByAlias(ALIAS_STD_FARMS))) {
            studentColumns.add(COL_FARMS);
        }

        if (m_studentPrograms.contains(student.getOid())) {
            studentColumns.add(COL_LEP);
        }

        if (student.getPerson().getHispanicLatinoIndicator()) {
            studentColumns.add(COL_HISPANIC);
        }

        return studentColumns;
    }

    /**
     * Increments the value at the passed column for the passed grid.
     *
     * @param grid
     * @param column
     * @param value
     */
    private void incrementCount(ReportDataGrid grid, String column, double value) {
        double currentValue = (Double) grid.get(column);
        grid.set(column, new Double(currentValue + value));
    }

    /**
     * Initializes the passed grid and returns a row map. The key of the map is a grade level code
     * and the value is the row number on the grid.
     *
     * @param grid
     *
     * @return Map<String, Integer>
     */
    private Map<String, Integer> initializeGrid(ReportDataGrid grid) {
        Map<String, Integer> rowMap = new HashMap<String, Integer>();

        SubQuery subQuery = new SubQuery(Student.class, SisStudent.COL_GRADE_LEVEL, m_studentCriteria, true);
        subQuery.addOrderByAscending(SisStudent.COL_GRADE_LEVEL);

        Collection<String> gradeLevels = getBroker().getSubQueryCollectionByQuery(subQuery);
        for (String gradeLevel : gradeLevels) {
            grid.append();
            grid.set(COL_GRADE_LEVEL, gradeLevel);

            for (String column : m_totalColumns) {
                grid.set(column + MEMBERSHIP_SUFFIX, 0.0);
                grid.set(column + PERCENT_SUFFIX, 0.0);
                grid.set(column + PRESENT_SUFFIX, 0.0);
                grid.set(column + STUDENTS_SUFFIX, 0.0);
            }

            rowMap.put(gradeLevel, grid.currentRowNumber());
        }

        grid.beforeTop();
        return rowMap;
    }

    /**
     * Loads the attendance summary, first day members list, and calendar lookup maps.
     */
    private void loadAttendance() {
        m_studentAttendance = new HashMap<String, Integer>();

        Criteria criteria = new Criteria();
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_startDate);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_endDate);
        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, true);

        SubQuery subQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, m_studentCriteria);
        criteria.addIn(StudentAttendance.COL_STUDENT_OID, subQuery);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
        }

        String[] columns = {StudentAttendance.COL_STUDENT_OID, "COUNT(*)"};
        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentAttendance.class, columns, criteria);
        query.addGroupBy(StudentAttendance.COL_STUDENT_OID);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String studentOid = (String) row[0];

                Integer absentCount = (Integer) row[1];

                m_studentAttendance.put(studentOid, absentCount);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads the calendar lookup for each school in <code>m_studentCriteria</code>.
     */
    private void loadCalendarLookups() {
        SubQuery schoolSubQuery = new SubQuery(Student.class, SisStudent.COL_SCHOOL_OID, m_studentCriteria);

        Criteria schoolCriteria = new Criteria();
        schoolCriteria.addIn(X2BaseBean.COL_OID, schoolSubQuery);

        QueryByCriteria schoolQuery = new QueryByCriteria(School.class, schoolCriteria);
        QueryIterator schools = getBroker().getIteratorByQuery(schoolQuery);
        try {
            while (schools.hasNext()) {
                SisSchool school = (SisSchool) schools.next();

                Map<String, Set<PlainDate>> calendarLookup =
                        m_enrollmentManager.getCalendarLookup(school, m_startDate, m_endDate);
                m_calendarLookups.put(school.getOid(), calendarLookup);
            }
        } finally {
            schools.close();
        }
    }

    /**
     * Loads a list of student OIDs for students who were members on <code>m_startDate</code>.
     */
    private void loadFirstDayMembers() {
        if (isSchoolContext()) {
            m_firstDayMembers = m_enrollmentManager.getMembershipAsOf(m_startDate, (SisSchool) getSchool());
        } else {
            m_firstDayMembers = m_enrollmentManager.getMembershipAsOf(m_startDate, getOrganization());
        }
    }

    private List<String> loadGradeLevels() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, ReferenceTable.REF_TABLE_OID_GRADE_LEVEL);

        BeanQuery query = new BeanQuery(ReferenceCode.class, criteria);

        Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);
        return codes.stream()
                .map(ReferenceCode::getCode)
                .filter(code -> GRADES_INCLUDED.contains(code))
                .collect(Collectors.toList());
    }

    /**
     * Loads a map of period OIDs keyed to collections of race codes.
     */
    private void loadRaceCodes() {
        SubQuery subQuery = new SubQuery(Student.class, SisStudent.COL_PERSON_OID, m_studentCriteria);

        Criteria criteria = new Criteria();
        criteria.addIn(Race.COL_PERSON_OID, subQuery);

        String[] columns = {Race.COL_PERSON_OID, Race.COL_RACE_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(Race.class, columns, criteria);

        m_raceCodes = getBroker().getGroupedColumnCollectionByQuery(query, 500);
    }

    /**
     * Loads a collection of students with an active ELL student program for the selected month.
     */
    private void loadStudentPrograms() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, PROGRAM_CODE_ELL);
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_endDate);

        Criteria endDateCriteria = new Criteria();
        endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_startDate);

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());

        endDateCriteria.addOrCriteria(orCriteria);
        criteria.addAndCriteria(endDateCriteria);

        SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, m_studentCriteria);
        criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);

        SubQuery subQuery =
                new SubQuery(StudentProgramParticipation.class, StudentProgramParticipation.COL_STUDENT_OID, criteria);
        m_studentPrograms = getBroker().getSubQueryCollectionByQuery(subQuery);
    }
}
