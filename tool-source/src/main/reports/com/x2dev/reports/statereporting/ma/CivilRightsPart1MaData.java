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
package com.x2dev.reports.statereporting.ma;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.Selection;
import com.follett.fsc.core.k12.beans.SelectionObject;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisStudent.Section504StatusCode;
import com.x2dev.sis.model.beans.SisStudent.SpedStatusCode;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the civil rights data collection report, part 1. This class creates a
 * report data grid containing enrolled student counts. The grid contains the following line
 * items:
 * <ol>
 * <li>Table 6 - Overall enrollment
 * <li>Table 7 - Early Childhood / PK
 * <li>Table 8 - Students in a gifted program
 * <li>Table 9.1 - Students considered LEP
 * <li>Table 9.2 - Students in an LEP program
 * <li>Table 10.1 - Students with IDEA disabilities
 * <li>Table 10.2 - Students with 504 onl
 * <li>Table 11.1 - Count of Algebra I sections
 * <li>Table 11.2 - Count of Geometry sections
 * <li>Table 11.3 - Count of Algebra II sections
 * <li>Table 11.4 - Count of Advanced Math sections
 * <li>Table 11.5 - Count of Calculus sections
 * <li>Table 11.6 - Count of Biology sections
 * <li>Table 11.7 - Count of Chemistry sections
 * <li>Table 11.8 - Count of Physics sections
 * <li>Table 12 - Students Gr 7 or 8, in Algebra I
 * <li>Table 13.1 - Students Gr 9 or 10, in Algebra I
 * <li>Table 13.2 - Students Gr 11 or 12, in Algebra I
 * <li>Table 13.3 - Students in Geometry
 * <li>Table 13.4 - Students in Algebra II
 * <li>Table 13.5 - Students in Advanced Math
 * <li>Table 13.6 - Students in Calculus
 * <li>Table 14.1 - Students in Biology
 * <li>Table 14.2 - Students in Chemistry
 * <li>Table 14.3 - Students in Physics
 * <li>Table 14.4 - Students in an IB program
 * <li>Table 17 - Students in at least 1 AP class
 * <li>Table 18.1 - Students in AP math
 * <li>Table 18.2 - Students in AP science
 * <li>Table 18.3 - Students in AP foreign language
 * </ol>
 * Each row contains the following count columns:
 * <ul>
 * <li>Count of sections, only applies to rows 14 through 21 of the report
 * <li>Hispanic/Latino (HL)
 * <li>American Indian (IND)
 * <li>Asian (ASN)
 * <li>Native Hawaiian/Pacific Islander (HPI)
 * <li>Black or African American (BAA)
 * <li>White (WHI)
 * <li>Two or more races (MULTI)
 * <li>Active sped (IDEA) (Active special education students)
 * <li>Active 504 (504) (Active 504 students)
 * <li>Active LEP (LEP) (Based on DOE 26)
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class CivilRightsPart1MaData extends ReportJavaSourceNet {
    /**
     * Name for the "queryBy" input parameter. This value is a string.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "end date" input parameter. This value is a PlainDate.
     */
    public static final String DATE_PARAM = "date";

    /**
     * Name for the "snapshot" input parameter. This value is a String. If this value is not null,
     * then the student selection should be just the members of the snapshot
     */
    public static final String SNAPSHOT_PARAM = "snapshot";

    /**
     * Name for the "start grade" input parameter. This value is a String. It is included only for
     * districts with schools where grades must be reported separately.
     */
    public static final String START_GRADE_PARAM = "startGrade";

    /**
     * Name for the "end grade" input parameter. This value is a String. It is included only for
     * districts with schools where grades must be reported separately.
     */
    public static final String END_GRADE_PARAM = "endGrade";

    /**
     * Alias of the student field containing the gifted indicator.
     */
    public static final String STUDENT_GIFTED_ALIAS = "gifted";

    /**
     * Alias of the student field containing the
     */
    public static final String STUDENT_CONSIDERED_LEP_ALIAS = "DOE 25";
    public static final String STUDENT_IS_LEP = "01";

    /**
     * Alias of the student field containing the
     */
    public static final String STUDENT_LEP_PROGRAM_ALIAS = "DOE 26";
    public static final String[] STUDENT_LEP_PROGRAMS =
            new String[] {"01", "02", "03"};

    /**
     * Alias of the student field containing the
     */
    public static final String COURSE_SUBJECT_AREA_ALIAS = "WA10-MTC";

    public static final String GRID_COLUMN_SECTION_COUNT = "sections";

    /**
     * NCES subject area codes to identify particular courses (values in field with alias identified
     * above in string COURSE_SUBJECT_AREA_ALIAS)
     */
    public static final String[] ALGEBRA_1_COURSES =
            new String[] {"02052", "02053", "02054", "02069M"};
    public static final String[] GEOMETRY_COURSES =
            new String[] {"02072"};
    public static final String[] ALGEBRA_2_COURSES =
            new String[] {"02056", "02057"};
    public static final String[] ADVANCED_MATH_COURSES =
            new String[] {"02056", "02073", "02075", "02101", "02102", "02103", "02105", "02106", "02107", "02108",
                    "02109", "02110", "02111", "02112", "02113", "02121", "02131", "02132", "02133", "02134", "02141",
                    "02201", "02202", "02203", "02204", "02209"};
    public static final String[] BIOLOGY_COURSES =
            new String[] {"03051", "03052", "03053", "03054", "03055", "03056", "03057", "03058", "03059", "03060",
                    "03061", "03062", "03063"};
    public static final String[] CHEMISTRY_COURSES =
            new String[] {"03101", "03102", "03103", "03104", "03105", "03106", "03107", "03108"};
    public static final String[] PHYSICS_COURSES =
            new String[] {"03151", "03152", "03153", "03154", "03155", "03156", "03157", "03158", "03159", "03160",
                    "03161"};
    public static final String[] CALCULUS_COURSES =
            new String[] {"02121", "02122", "02123", "02124", "02125", "02126"};
    public static final String[] AP_ALL =
            new String[] {"01005", "01006", "02124", "02125", "02203", "03056", "03106", "03155", "03207", "03A01",
                    "03A02", "04004", "04056", "04057", "04104", "04157", "04158", "04203", "04204", "04205", "04256",
                    "04A01", "04A02", "05115", "05153", "05172", "05A01", "05A02", "06112", "06113", "06132", "06133",
                    "06212", "06313", "06A01", "06A02", "06A03", "06A04", "10158", "10159"};
    public static final String[] AP_MATH =
            new String[] {"02124", "02125", "02203"};
    public static final String[] AP_SCIENCE =
            new String[] {"03056", "03106", "03155", "03207", "03A01", "03A02", "04004"};
    public static final String[] AP_FOREIGN_LANGUAGE =
            new String[] {"06112", "06113", "06132", "06133", "06212", "06313", "06A01", "06A02", "06A03", "06A04"};

    /*
     * Local constants
     */
    private static final Integer INTEGER_ONE = Integer.valueOf(1);
    private static final int NUMBER_OF_ROWS = 50;
    private static final int NUMBER_OF_COLUMNS = 10;

    /*
     * Local instance variables
     */
    private SystemStringConverter m_booleanConverter = null;
    private DataDictionary m_dictionary = null;
    private PlainDate m_date = null;
    private Map<String, Collection<Race>> m_raceLookup = null;
    private Map<String, Collection<StudentSchedule>> m_studentScheduleLookup = null;
    private HashMap<String, Integer> m_gradeLevelLookup = null;

    private DataDictionaryField m_lepField = null;
    private DataDictionaryField m_lepProgramField = null;
    private DataDictionaryField m_raceCodeField = null;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid(10, 10);
        initializeGrid(grid);

        /*
         * Initialize member variables
         */
        m_date = (PlainDate) getParameter(DATE_PARAM);
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_gradeLevelLookup = StudentManager.buildNumericGradeLevelMap(getBroker());
        m_booleanConverter = (SystemStringConverter) ConverterFactory.getConverterForClass(Converter.BOOLEAN_CONVERTER,
                getLocale(), true);

        m_lepField = m_dictionary.findDataDictionaryFieldByAlias(STUDENT_CONSIDERED_LEP_ALIAS);
        m_lepProgramField = m_dictionary.findDataDictionaryFieldByAlias(STUDENT_LEP_PROGRAM_ALIAS);
        m_raceCodeField = m_dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);

        Selection studentSelection = getStudentSelection();
        m_raceLookup = getRaceLookup(studentSelection);
        m_studentScheduleLookup = getStudentScheduleLookup(studentSelection);

        addStudentCounts(grid, studentSelection);
        addSectionCounts(grid);

        /*
         * Delete the selection object - it's only needed to run the student query above.
         */
        getBroker().deleteBean(studentSelection);

        grid.beforeTop();

        return grid;
    }

    /**
     * Adds the section counts.
     *
     * @param grid ReportDataGrid
     */
    private void addSectionCounts(ReportDataGrid grid) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, ((SisSchool) getSchool()).getActiveScheduleOid());
        criteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);

        QueryByCriteria query = new QueryByCriteria(MasterSchedule.class, criteria);

        QueryIterator sections = getBroker().getIteratorByQuery(query);
        try {
            while (sections.hasNext()) {
                MasterSchedule section = (MasterSchedule) sections.next();

                Course course = null;
                try {
                    course = section.getSchoolCourse().getCourse();
                } catch (NullPointerException npe) {
                    // Skip this section - it's not related to a course
                }

                if (course != null) {
                    for (int i = 14; i <= 21; i++) // Rows 14-20 contain course counts
                    {
                        grid.gotoRow(i);

                        if (includeSectionInRow(course, i)) {
                            Integer currentCount = (Integer) grid.get(GRID_COLUMN_SECTION_COUNT);
                            if (currentCount == null) {
                                currentCount = INTEGER_ONE;
                            } else {
                                currentCount = Integer.valueOf(currentCount.intValue() + 1);
                            }

                            grid.set(GRID_COLUMN_SECTION_COUNT, currentCount);
                        }
                    }
                }
            }
        } finally {
            sections.close();
        }
    }

    /**
     * Adds the student counts.
     *
     * @param grid ReportDataGrid
     * @param studentSelection Selection
     */
    private void addStudentCounts(ReportDataGrid grid, Selection studentSelection) {
        /*
         * Build student criteria.
         */
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX
                + X2BaseBean.COL_OID);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));

        /*
         * Generate query
         */
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, studentCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();

                for (int i = 0; i < NUMBER_OF_ROWS; i++) {
                    if (includeStudentInRow(student, i)) {
                        grid.gotoRow(i);

                        for (int j = 1; j <= NUMBER_OF_COLUMNS; j++) // Column 0 is reserved for the
                                                                     // row label
                        {
                            if (includeStudentInColumn(student, j)) {
                                String columnId = Integer.toString(j);

                                Integer currentCount = (Integer) grid.get(columnId);
                                if (currentCount == null) {
                                    currentCount = INTEGER_ONE;
                                } else {
                                    currentCount = Integer.valueOf(currentCount.intValue() + 1);
                                }

                                grid.set(columnId, currentCount);
                            }
                        }
                    }
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Gets the race lookup.
     *
     * @param studentSelection Selection
     * @return Map
     */
    private Map<String, Collection<Race>> getRaceLookup(Selection studentSelection) {
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX
                + X2BaseBean.COL_OID);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));

        SubQuery studentQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);

        Criteria raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, studentQuery);

        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);

        return getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 2048);
    }

    /**
     * Gets the student schedule lookup.
     *
     * @param studentSelection Selection
     * @return Map
     */
    private Map<String, Collection<StudentSchedule>> getStudentScheduleLookup(Selection studentSelection) {
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX
                + StudentSchedule.COL_STUDENT_OID);

        Criteria scheduleCriteria = new X2Criteria();
        scheduleCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        scheduleCriteria.addEqualTo(StudentSchedule.COL_SCHEDULE_OID, ((SisSchool) getSchool()).getActiveScheduleOid());

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, scheduleCriteria);
        query.addPrefetchedRelationship(StudentSchedule.REL_SECTION);

        return getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 2048);
    }

    /**
     * Returns a Selection of the students that were members of the school on the report start date.
     *
     * @return Selection
     */
    private Selection getStudentSelection() {
        Selection selection = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());

        String startGrade = (String) getParameter(START_GRADE_PARAM);
        String endGrade = (String) getParameter(END_GRADE_PARAM);

        Collection<String> currentMembers = new ArrayList<String>(0);

        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 0:
                /*
                 * If the report is run for today we look at active students only. Otherwise we use
                 * enrollment records to determine the active students on the reporting date.
                 */
                if (m_date.equals(new PlainDate(getTimeZone()))) {
                    Criteria criteria = new Criteria();
                    criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                            SisStudent.COL_ENROLLMENT_STATUS));
                    criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());

                    if (startGrade != null && startGrade.length() > 0 && endGrade != null && endGrade.length() > 0) {
                        criteria.addGreaterOrEqualThan(SisStudent.COL_GRADE_LEVEL, startGrade);
                        criteria.addLessOrEqualThan(SisStudent.COL_GRADE_LEVEL, endGrade);
                    }

                    SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, criteria);
                    currentMembers = getBroker().getSubQueryCollectionByQuery(subQuery);
                } else {
                    EnrollmentManager emanager =
                            new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
                    currentMembers = emanager.getMembershipAsOf(m_date, ((SisSchool) getSchool()));
                }
                break;

            case 1:
                /*
                 * If the report is run for a snapshot, get the students in the snapshot
                 */
                Criteria criteria = new Criteria();
                criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
                criteria.addIn(X2BaseBean.COL_OID, ReportUtils
                        .getRecordSetSubQuery((String) getParameter(SNAPSHOT_PARAM), getUser(), getSchool()));

                if (startGrade != null && startGrade.length() > 0 && endGrade != null && endGrade.length() > 0) {
                    criteria.addGreaterOrEqualThan(SisStudent.COL_GRADE_LEVEL, startGrade);
                    criteria.addLessOrEqualThan(SisStudent.COL_GRADE_LEVEL, endGrade);
                }

                SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, criteria);
                currentMembers = getBroker().getSubQueryCollectionByQuery(subQuery);

                break;

            default:
                break;

        }

        for (String oid : currentMembers) {
            SelectionObject selectedObject =
                    X2BaseBean.newInstance(SelectionObject.class, getBroker().getPersistenceKey());
            selectedObject.setObjectOid(oid);
            selection.addToSelectionObjects(selectedObject);
        }

        selection.setTimestamp(System.currentTimeMillis());
        getBroker().saveBeanForced(selection);

        return selection;
    }

    /**
     * Checks for race code.
     *
     * @param student SisStudent
     * @param stateRaceCode String
     * @return true, if successful
     */
    private boolean hasRaceCode(SisStudent student, String stateRaceCode) {
        boolean hasRaceCode = false;

        Collection<Race> raceCodes = m_raceLookup.get(student.getPersonOid());

        if (!student.getPerson().getHispanicLatinoIndicator()) {
            if ("MULTI".equals(stateRaceCode)) {
                hasRaceCode = raceCodes != null && raceCodes.size() > 1;
            } else if (raceCodes != null && raceCodes.size() == 1) {
                Race race = raceCodes.iterator().next();

                String thisStateCode =
                        Integer.valueOf(m_dictionary.findStateReferenceCode(m_raceCodeField.getReferenceTableOid(),
                                race.getRaceCode())).toString();

                hasRaceCode = thisStateCode != null && thisStateCode.equals(stateRaceCode);
            }
        }

        return hasRaceCode;
    }

    /**
     * Include section in row.
     *
     * @param course Course
     * @param rowNumber int
     * @return true, if successful
     */
    private boolean includeSectionInRow(Course course, int rowNumber) {
        boolean include = false;
        String subjectAreaCode = (String) course.getFieldValueByAlias(COURSE_SUBJECT_AREA_ALIAS);

        if (subjectAreaCode != null) {
            switch (rowNumber) {
                case 14: // Algebra 1 sections
                    include = arrayContains(ALGEBRA_1_COURSES, subjectAreaCode);
                    break;

                case 15: // Geometry sections
                    include = arrayContains(GEOMETRY_COURSES, subjectAreaCode);
                    break;

                case 16: // Algebra 2 sections
                    include = arrayContains(ALGEBRA_2_COURSES, subjectAreaCode);
                    break;

                case 17: // Advanced math sections
                    include = arrayContains(ADVANCED_MATH_COURSES, subjectAreaCode);
                    break;

                case 18: // Calculus sections
                    include = arrayContains(CALCULUS_COURSES, subjectAreaCode);
                    break;

                case 19: // Biology sections
                    include = arrayContains(BIOLOGY_COURSES, subjectAreaCode);
                    break;

                case 20: // Chemistry sections
                    include = arrayContains(CHEMISTRY_COURSES, subjectAreaCode);
                    break;

                case 21: // Physics sections
                    include = arrayContains(PHYSICS_COURSES, subjectAreaCode);
                    break;
            }
        }

        return include;
    }

    /**
     * Include student in column.
     *
     * @param student SisStudent
     * @param columnNumber int
     * @return true, if successful
     */
    private boolean includeStudentInColumn(SisStudent student, int columnNumber) {
        boolean include = false;

        switch (columnNumber) {
            case 1: // Hispanic/latino
                include = student.getPerson().getHispanicLatinoIndicator();
                break;

            case 2: // Indian
                include = hasRaceCode(student, "8");
                break;

            case 3: // Asian
                include = hasRaceCode(student, "4");
                break;

            case 4: // Pac Islander/Hawaiian
                include = hasRaceCode(student, "16");
                break;

            case 5: // Black
                include = hasRaceCode(student, "2");
                break;

            case 6: // White
                include = hasRaceCode(student, "1");
                break;

            case 7: // Multiple races
                include = hasRaceCode(student, "MULTI");
                break;

            case 8: // Special education
                include = student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE;
                break;

            case 9: // 504
                include = student.getSection504StatusCodeEnum() == Section504StatusCode.ACTIVE;
                break;

            case 10: // LEP
                include = isLep(student);
                break;
        }

        return include;
    }

    /**
     * Returns true if the passed student should be counted in the passed row number.
     *
     * @param student SisStudent
     * @param rowNumber int
     * @return true, if successful
     */
    private boolean includeStudentInRow(SisStudent student, int rowNumber) {
        boolean include = false;

        Person person = student.getPerson();
        String genderCode = person != null ? person.getGenderCode() : null;
        boolean male = "M".equalsIgnoreCase(genderCode);

        switch (rowNumber) {
            case 0: // Overall enrollment, males
                include = male;
                break;

            case 1: // Overall enrollment, females
                include = !male;
                break;

            case 2: // PK enrollment, males
                include = male && isInGradeRange(student, -1000, -1);
                break;

            case 3: // PK enrollment, females
                include = !male && isInGradeRange(student, -1000, -1);
                break;

            case 4: // Gifted, males
                include = male && isGifted(student);
                break;

            case 5: // Gifted, females
                include = !male && isGifted(student);
                break;

            case 6: // students considered LEP, males
                include = male && isLep(student);
                break;

            case 7: // students considered LEP, females
                include = !male && isLep(student);
                break;

            case 8: // students in an LEP program, males
                include = male && isInLepProgram(student);
                break;

            case 9: // students in an LEP program, females
                include = !male && isInLepProgram(student);
                break;

            case 10: // sped, males
                include = male && student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE;
                break;

            case 11: // sped, females
                include = !male && student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE;
                break;

            case 12: // 504, males
                include = male && student.getSection504StatusCodeEnum() == Section504StatusCode.ACTIVE;
                break;

            case 13: // 504, females
                include = !male && student.getSection504StatusCodeEnum() == Section504StatusCode.ACTIVE;
                break;

            case 14: // Rows 14-21 contain section counts which are not tied to students
            case 15:
            case 16:
            case 17:
            case 18:
            case 19:
            case 20:
            case 21:
                include = false;
                break;

            case 22: // Students Gr 7 or 8, in Algebra I, males
                include = male && isInGradeRange(student, 7, 8) && isInCourse(student, ALGEBRA_1_COURSES);
                break;

            case 23: // Students Gr 7 or 8, in Algebra I, females
                include = !male && isInGradeRange(student, 7, 8) && isInCourse(student, ALGEBRA_1_COURSES);
                break;

            case 24: // Students Gr 9 or 10, in Algebra I, males
                include = male && isInGradeRange(student, 9, 10) && isInCourse(student, ALGEBRA_1_COURSES);
                break;

            case 25: // Students Gr 9 or 10, in Algebra I, females
                include = !male && isInGradeRange(student, 9, 10) && isInCourse(student, ALGEBRA_1_COURSES);
                break;

            case 26: // Students Gr 11 or 12, in Algebra I, males
                include = male && isInGradeRange(student, 11, 12) && isInCourse(student, ALGEBRA_1_COURSES);
                break;

            case 27: // Students Gr 11 or 12, in Algebra I, females
                include = !male && isInGradeRange(student, 11, 12) && isInCourse(student, ALGEBRA_1_COURSES);
                break;

            case 28: // Students in geometry, males
                include = male && isInCourse(student, GEOMETRY_COURSES);
                break;

            case 29: // Students in geometry, females
                include = !male && isInCourse(student, GEOMETRY_COURSES);
                break;

            case 30: // Students in algebra 2, males
                include = male && isInCourse(student, ALGEBRA_2_COURSES);
                break;

            case 31: // Students in algebra 2, females
                include = !male && isInCourse(student, ALGEBRA_2_COURSES);
                break;

            case 32: // Students in advanced math, males
                include = male && isInCourse(student, ADVANCED_MATH_COURSES);
                break;

            case 33: // Students in advanced math, females
                include = !male && isInCourse(student, ADVANCED_MATH_COURSES);
                break;

            case 34: // Students in calculus, males
                include = male && isInCourse(student, CALCULUS_COURSES);
                break;

            case 35: // Students in calculus, females
                include = !male && isInCourse(student, CALCULUS_COURSES);
                break;

            case 36: // Students in biology, males
                include = male && isInCourse(student, BIOLOGY_COURSES);
                break;

            case 37: // Students in biology, females
                include = !male && isInCourse(student, BIOLOGY_COURSES);
                break;

            case 38: // Students in chemistry, males
                include = male && isInCourse(student, CHEMISTRY_COURSES);
                break;

            case 39: // Students in chemistry, females
                include = !male && isInCourse(student, CHEMISTRY_COURSES);
                break;

            case 40: // Students in physics, males
                include = male && isInCourse(student, PHYSICS_COURSES);
                break;

            case 41: // Students in physics, females
                include = !male && isInCourse(student, PHYSICS_COURSES);
                break;

            case 42: // Students in any AP class, males
                include = male && isInCourse(student, AP_ALL);
                break;

            case 43: // Students in any AP class, females
                include = !male && isInCourse(student, AP_ALL);
                break;

            case 44: // Students in AP math, males
                include = male && isInCourse(student, AP_MATH);
                break;

            case 45: // Students in AP math, females
                include = !male && isInCourse(student, AP_MATH);
                break;

            case 46: // Students in AP science, males
                include = male && isInCourse(student, AP_SCIENCE);
                break;

            case 47: // Students in AP science, females
                include = !male && isInCourse(student, AP_SCIENCE);
                break;

            case 48: // Students in AP foriegn language, males
                include = male && isInCourse(student, AP_FOREIGN_LANGUAGE);
                break;

            case 49: // Students in AP foriegn language, females
                include = !male && isInCourse(student, AP_FOREIGN_LANGUAGE);
                break;
        }

        return include;
    }

    /**
     * Checks if is in course.
     *
     * @param student SisStudent
     * @param courseNumbers String[]
     * @return true, if is in course
     */
    private boolean isInCourse(SisStudent student, String[] courseNumbers) {
        boolean include = false;

        Collection<StudentSchedule> schedules = m_studentScheduleLookup.get(student.getOid());
        if (schedules != null) {
            for (StudentSchedule schedule : schedules) {
                Course course = null;
                try {
                    course = schedule.getSection().getSchoolCourse().getCourse();
                } catch (NullPointerException npe) {
                    // Skip this section - it's not related to a course
                }

                if (course != null) {
                    String subjectAreaCode = (String) course.getFieldValueByAlias(COURSE_SUBJECT_AREA_ALIAS);

                    if (subjectAreaCode != null) {
                        include = arrayContains(courseNumbers, subjectAreaCode);
                        if (include) {
                            break;
                        }
                    }
                }
            }
        }

        return include;
    }

    /**
     * Initialize grid.
     *
     * @param grid ReportDataGrid
     */
    private void initializeGrid(ReportDataGrid grid) {
        // Note - number of rows appended here must match NUMBER_OF_ROWS

        grid.append(); // Row 0
        grid.set("0", "Table 6.M - Overall enrollment");

        grid.append(); // Row 1
        grid.set("0", "Table 6.F - Overall enrollment");

        grid.append(); // Row 2
        grid.set("0", "Table 7.M - Early Childhood / PK");

        grid.append(); // Row 3
        grid.set("0", "Table 7.F - Early Childhood / PK");

        grid.append(); // Row 4
        grid.set("0", "Table 8.M - Students in a GT program");

        grid.append(); // Row 5
        grid.set("0", "Table 8.F - Students in a GT program");

        grid.append(); // Row 6
        grid.set("0", "Table 9.1.M - Students considered LEP");

        grid.append(); // Row 7
        grid.set("0", "Table 9.1.F - Students considered LEP");

        grid.append(); // Row 8
        grid.set("0", "Table 9.2.M - Students in LEP program");

        grid.append(); // Row 9
        grid.set("0", "Table 9.2.F - Students in LEP program");

        grid.append(); // Row 10
        grid.set("0", "Table 10.1.M - Students with IDEA disabilities");

        grid.append(); // Row 11
        grid.set("0", "Table 10.1.F - Students with IDEA disabilities");

        grid.append(); // Row 12
        grid.set("0", "Table 10.2.M - Students with 504 only");

        grid.append(); // Row 13
        grid.set("0", "Table 10.2.F - Students with 504 only");

        grid.append(); // Row 14
        grid.set("0", "Table 11.1 - Count of Algebra I sections");

        grid.append(); // Row 15
        grid.set("0", "Table 11.2 - Count of Geometry sections");

        grid.append(); // Row 16
        grid.set("0", "Table 11.3 - Count of Algebra II sections");

        grid.append(); // Row 17
        grid.set("0", "Table 11.4 - Count of Advanced Math sections");

        grid.append(); // Row 18
        grid.set("0", "Table 11.5 - Count of Calculus sections");

        grid.append(); // Row 19
        grid.set("0", "Table 11.6 - Count of Biology sections");

        grid.append(); // Row 20
        grid.set("0", "Table 11.7 - Count of Chemistry sections");

        grid.append(); // Row 21
        grid.set("0", "Table 11.8 - Count of Physics sections");

        grid.append(); // Row 22
        grid.set("0", "Table 12.M - Students Gr 7 or 8, in  Algebra I");

        grid.append(); // Row 23
        grid.set("0", "Table 12.F - Students Gr 7 or 8, in  Algebra I");

        grid.append(); // Row 24
        grid.set("0", "Table 13.1.M - Students Gr 9 or 10, in Algebra I");

        grid.append(); // Row 25
        grid.set("0", "Table 13.1.F - Students Gr 9 or 10, in Algebra I");

        grid.append(); // Row 26
        grid.set("0", "Table 13.2.M - Students Gr 11 or 12, in Algebra I");

        grid.append(); // Row 27
        grid.set("0", "Table 13.2.F - Students Gr 11 or 12, in Algebra I");

        grid.append(); // Row 28
        grid.set("0", "Table 13.3.M - Students in Geometry");

        grid.append(); // Row 29
        grid.set("0", "Table 13.3.F - Students in Geometry");

        grid.append(); // Row 30
        grid.set("0", "Table 13.4.M - Students in Algebra II");

        grid.append(); // Row 31
        grid.set("0", "Table 13.4.F - Students in Algebra II");

        grid.append(); // Row 32
        grid.set("0", "Table 13.6.M - Students in Advanced Math");

        grid.append(); // Row 33
        grid.set("0", "Table 13.6.F - Students in Advanced Math");

        grid.append(); // Row 34
        grid.set("0", "Table 13.7.M - Students in Calculus");

        grid.append(); // Row 35
        grid.set("0", "Table 13.7.F - Students in Calculus");

        grid.append(); // Row 36
        grid.set("0", "Table 14.1.M - Students in Biology");

        grid.append(); // Row 37
        grid.set("0", "Table 14.1.F - Students in Biology");

        grid.append(); // Row 38
        grid.set("0", "Table 14.2.M - Students in Chemistry");

        grid.append(); // Row 39
        grid.set("0", "Table 14.2.F - Students in Chemistry");

        grid.append(); // Row 40
        grid.set("0", "Table 14.3.M - Students in Physics");

        grid.append(); // Row 41
        grid.set("0", "Table 14.3.F - Students in Physics");

        grid.append(); // Row 42
        grid.set("0", "Table 17.M - Students in at least 1 AP class");

        grid.append(); // Row 43
        grid.set("0", "Table 17.F - Students in at least 1 AP class");

        grid.append(); // Row 44
        grid.set("0", "Table 18.1.M - Students in AP Math");

        grid.append(); // Row 45
        grid.set("0", "Table 18.1.F - Students in AP Math");

        grid.append(); // Row 46
        grid.set("0", "Table 18.2.M - Students in AP Science");

        grid.append(); // Row 47
        grid.set("0", "Table 18.2.F - Students in AP Science");

        grid.append(); // Row 48
        grid.set("0", "Table 18.3.M - Students in AP Foreign Language");

        grid.append(); // Row 49
        grid.set("0", "Table 18.3.F - Students in AP Foreign Language");
    }

    /**
     * Checks if is gifted.
     *
     * @param student SisStudent
     * @return true, if is gifted
     */
    private boolean isGifted(SisStudent student) {
        boolean include = false;

        String giftedValue = (String) student.getFieldValueByAlias(STUDENT_GIFTED_ALIAS);
        if (giftedValue != null) {
            include = ((Boolean) m_booleanConverter.parseSystemString(giftedValue)).booleanValue();
        }
        return include;
    }

    /**
     * Checks if is in lep program.
     *
     * @param student SisStudent
     * @return true, if is in lep program
     */
    private boolean isInLepProgram(SisStudent student) {
        boolean include = false;

        String lepProgramValue = (String) student.getFieldValueByAlias(STUDENT_LEP_PROGRAM_ALIAS);
        if (lepProgramValue != null) {
            String stateCode =
                    m_dictionary.findStateReferenceCode(m_lepProgramField.getReferenceTableOid(), lepProgramValue);
            include = arrayContains(STUDENT_LEP_PROGRAMS, stateCode);
        }

        return include;
    }

    /**
     * Checks if is lep.
     *
     * @param student SisStudent
     * @return true, if is lep
     */
    private boolean isLep(SisStudent student) {
        boolean include = false;

        String lepValue = (String) student.getFieldValueByAlias(STUDENT_CONSIDERED_LEP_ALIAS);
        if (lepValue != null) {
            String stateCode =
                    m_dictionary.findStateReferenceCode(m_lepField.getReferenceTableOid(), lepValue);
            include = STUDENT_IS_LEP.equalsIgnoreCase(stateCode);
        }

        return include;
    }

    /**
     * Checks if is in grade range.
     *
     * @param student SisStudent
     * @param startGrade int
     * @param endGrade int
     * @return true, if is in grade range
     */
    private boolean isInGradeRange(SisStudent student, int startGrade, int endGrade) {
        boolean include;
        Integer gradeLevel = m_gradeLevelLookup.get(student.getGradeLevel());
        include = gradeLevel != null &&
                gradeLevel.intValue() >= startGrade && gradeLevel.intValue() <= endGrade;
        return include;
    }

    /**
     * Array contains.
     *
     * @param array String[]
     * @param value String
     * @return true, if successful
     */
    private boolean arrayContains(String[] array, String value) {
        boolean contains = false;

        if (value != null) {
            value = value.trim();
        }

        for (int i = 0; i < array.length; i++) {
            if (array[i].equals(value)) {
                contains = true;
                break;
            }
        }
        return contains;
    }
}
