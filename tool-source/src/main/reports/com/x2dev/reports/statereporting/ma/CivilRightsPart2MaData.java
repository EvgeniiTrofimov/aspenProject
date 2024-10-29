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
package com.x2dev.reports.statereporting.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
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
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.SisStudent.Section504StatusCode;
import com.x2dev.sis.model.beans.SisStudent.SpedStatusCode;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the student summary by civil rights categories report. This class creates a
 * report data grid containing student counts. The grid contains the following line items to be
 * reported for the PREVIOUS SCHOOL YEAR:
 * <ol>
 * <li>Table 24 - Students in grades 7 or 8 who passed Algebra I
 * <li>Table 25.1 - Students in grades 9 or 10 who passed Algebra I
 * <li>Table 25.2 - Students in grades 11 or 12 who passed Algebra I
 * <li>Table 26 - Students in grades 9-12 who took the SAT or ACT
 * <li>Table 27.1 - Students who took AP tests for some AP courses taken
 * <li>Table 27.2 - Students who took AP tests for all AP courses taken
 * <li>Table 27.3 - Students who took AP courses, but did not take any AP tests
 * <li>Table 28.1 - Students who passed all AP tests taken
 * <li>Table 28.2 - Students who passed some AP tests taken
 * <li>Table 28.3 - Students who passed no AP tests taken
 * <li>Table 29.1 - Students retained in Kindergarten
 * <li>Table 29.2 - Students retained in Grade 1
 * <li>Table 29.3 - Students retained in Grade 2
 * <li>Table 29.4 - Students retained in Grade 3
 * <li>Table 29.5 - Students retained in Grade 4
 * <li>Table 29.6 - Students retained in Grade 5
 * <li>Table 30.1 - Students retained in Grade 6
 * <li>Table 30.2 - Students retained in Grade 7
 * <li>Table 30.3 - Students retained in Grade 8
 * <li>Table 31.1 - Students retained in Grade 9
 * <li>Table 31.2 - Students retained in Grade 10
 * <li>Table 31.3 - Students retained in Grade 11
 * <li>Table 31.4 - Students retained in Grade 12
 * <li>Table 34.A - Students without disabilities who received corporal punishment
 * <li>Table 34.B - Students without disabilities who received one or more in-school suspension
 * <li>Table 34.C - Students without disabilities who received only one out-of-school suspension
 * <li>Table 34.D - Students without disabilities who received more than one out-of-school
 * suspension
 * <li>Table 34.E - Students without disabilities expelled with educational services
 * <li>Table 34.F - Students without disabilities expelled without educational services
 * <li>Table 34.G - Students without disabilities expelled under zero-tolerance policies
 * <li>Table 34.H - Students without disabilities referred to law enforcement
 * <li>Table 34.I - Students without disabilities subjected to school-related arrest
 * <li>Table 35.A - Students with disabilities who received corporal punishment
 * <li>Table 35.B - Students with disabilities who received one or more in-school suspension
 * <li>Table 35.C - Students with disabilities who received only one out-of-school suspension
 * <li>Table 35.D - Students with disabilities who received more than one out-of-school suspension
 * <li>Table 35.E - Students with disabilities expelled with educational services
 * <li>Table 35.F - Students with disabilities expelled without educational services
 * <li>Table 35.G - Students with disabilities expelled under zero-tolerance policies
 * <li>Table 35.H - Students with disabilities referred to law enforcement
 * <li>Table 35.I - Students with disabilities subjected to school-related arrest
 * <li>Table 37.1 - Students reported to have been harassed or bullied on the basis of sex
 * <li>Table 37.2 - Students reported to have been harassed or bullied on the basis of race, color
 * or national origin
 * <li>Table 37.3 - Students reported to have been harassed or bullied on the basis of disability
 * <li>Table 38.1 - Students disciplined for bulling on the basis of sex
 * <li>Table 38.2 - Students disciplined for bulling on the basis of race, color or national origin
 * <li>Table 38.3 - Students disciplined for bulling on the basis of disability
 * <li>Table 39.1 - Non-IDEA students subjected to mechanical restraint
 * <li>Table 39.2 - Non-IDEA students subjected to physical restraint
 * <li>Table 39.3 - Non-IDEA students subjected to seclusion
 * <li>Table 40.1 - Students with disabilities (IDEA) subjected to mechanical restraint
 * <li>Table 40.2 - Students with disabilities (IDEA) subjected to physical restraint
 * <li>Table 40.3 - Students with disabilities (IDEA) subjected to seclusion
 * <li>Table 41.1 - Incidents of mechanical restraint
 * <li>Table 41.2 - Incidents of physical restraint
 * <li>Table 41.3 - Incidents of seclusion
 * <li>Table 42 - FTE of teachers who were absent more than 10 days of the school year
 * </ol>
 * Each row contains the following count columns:
 * <ul>
 * <li>Hispanic/Latino (HL)
 * <li>American Indian (IND)
 * <li>Asian (ASN)
 * <li>Native Hawaiian/Pacific Islander (HPI)
 * <li>Black or African American (BAA)
 * <li>White (WHI)
 * <li>Two or more races (MULTI)
 * <li>Active sped (IDEA) (Active special education students)
 * <li>Active 504 (504)
 * <li>Active LEP (LEP)
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class CivilRightsPart2MaData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    /*
     * DEBUGGING
     */
    // private int studentCounter = 0;
    // private String reportMessage = "";
    /*
     * ================================= STATE SPECIFIC PARAMETERS =================================
     */
    /*
     * Alias of the student field containing the LEP status
     */
    public static final String STUDENT_CONSIDERED_LEP_ALIAS = "DOE 25";
    public static final String STUDENT_IS_LEP = "01";
    /*
     * Alias of the district course catalog field containing the state subject area codes for
     * Algebra 1 and AP courses
     */
    public static final String STATE_COURSE_CODE_ALIAS = "WA10-MTC";
    /*
     * Table 24-25 strings -- the list of state course codes for Algebra 1. These values are
     * expected to be found in a user-defined field (UDF) on the district Course table. This UDF
     * must be set with the alias "STATE_COURSE_CODE_ALIAS", defined above
     */
    public static final String[] ALGEBRA_1_COURSES = new String[] {"02052", "02053", "02054", "02069M"};
    /*
     * Table 27-28 strings -- the list of state course codes for AP courses. These values are
     * expected to be found in a user-defined field (UDF) on the district Course table. This UDF
     * must be set with the alias "STATE_COURSE_CODE_ALIAS", defined above
     */
    public static final String[] AP_ALL =
            new String[] {"01005", "01006", "02124", "02125", "02203", "03056", "03106", "03155", "03207", "03A01",
                    "03A02", "04004", "04053", "04056", "04057", "04104", "04157", "04158", "04203", "04204",
                    "04205", "04255", "04256", "04A01", "04A02", "05115", "05153", "05172", "05A01", "05A02",
                    "06112", "06113", "06132", "06133", "06212", "06313", "06A01", "06A02", "06A03",
                    "06A04", "10158", "10159"};
    /*
     * Ethnicity codes
     */
    public static final String STATE_CODE_INDIAN_NATIVE_AMERICAN = "8";
    public static final String STATE_CODE_ASIAN = "4";
    public static final String STATE_CODE_HAWAIIAN_PACIFIC_ISLANDER = "16";
    public static final String STATE_CODE_BLACK_AFRICAN_AMERICAN = "2";
    public static final String STATE_CODE_CAUCASIAN = "1";
    public static final String STATE_CODE_MULTIPLE_ETHNICITIES = "MULTI";
    /*
     * ============================ STANDARD PARAMETERS FOR ALL STATES ============================
     */
    public static final String[] KINDERGARTEN_GRADES = new String[] {"K", "KG", "KP", "KF", "KT"};
    public static final String ALGEBRA_1_MIN_GRADE_LEVEL = "07";
    public static final String ALGEBRA_1_MAX_GRADE_LEVEL = "12";
    public static final String AP_TEST_RESULT_ALIAS = "AP test result";
    public static final String AP_TEST_RESULT_PASS = "Pass";
    public static final String AP_TEST_RESULT_FAIL = "Fail";
    public static final String AP_TEST_RESULT_NOT_TAKEN = "Not taken";
    // Table 26 strings
    public static final String ACT_ASSESSMENT_NAME = "ACT";
    public static final String SAT_ASSESSMENT_NAME = "SAT";
    public static final String SAT_ACT_MIN_GRADE_LEVEL = "09";
    public static final String SAT_ACT_MAX_GRADE_LEVEL = "12";
    // Table 29-31 strings
    public static final String REPEAT_ENROLLMENT_TYPE = "Y";
    /*
     * ======================== PARAMETERS FROM INPUT SCREEN - DO NOT EDIT =========================
     */
    public static final String CONDUCT_ACTION_CORP_PARAM = "actionCorporal";
    public static final String CONDUCT_ACTION_ISS_PARAM = "actionIss";
    public static final String CONDUCT_ACTION_OSS_PARAM = "actionOss";
    public static final String CONDUCT_ACTION_EXP_W_SRVCS_PARAM = "actionExpWithSrvcs";
    public static final String CONDUCT_ACTION_EXP_W_O_SRVCS_PARAM = "actionExpWoSrvcs";
    public static final String CONDUCT_ACTION_EXP_ZERO_TOL_PARAM = "actionExpZeroTol";
    public static final String CONDUCT_ACTION_REF_LAW_PARAM = "actionRefLawEnf";
    public static final String CONDUCT_ACTION_SCHL_ARREST_PARAM = "actionSchoolArrest";
    public static final String CONDUCT_INCIDENT_BULLY_SEX_PARAM = "incidentBullyingSex";
    public static final String CONDUCT_INCIDENT_BULLY_RACE_PARAM = "incidentBullyingRace";
    public static final String CONDUCT_INCIDENT_BULLY_DISABILITY_PARAM = "incidentBullyingDisability";

    public static final String CONTEXT_OID_PARAM = "contextOid";
    public static final String LOWEST_PASSING_LETTER_PARAM = "lowestPassingLetter";
    public static final String LOWEST_PASSING_NUMERIC_PARAM = "lowestPassingNumeric";
    public static final String REPEAT_REASON_CODE_PARAM = "repeatCode";
    public static final String SUMMER_START_DATE_PARAM = "summerStartDate";
    public static final String SUMMER_END_DATE_PARAM = "summerEndDate";

    private static final Integer INTEGER_ZERO = Integer.valueOf(0);
    private static final int NUMBER_OF_ROWS = 46;
    private static final int NUMBER_OF_COLUMNS = 20;
    private static final int NUMBER_OF_TOTAL_COLUMNS = 30;

    private DataDictionary m_dictionary = null;
    private Map<String, Collection<Race>> m_raceLookup = null;
    private Map<String, Collection<Transcript>> m_algebraTranscriptLookup = null;
    private Map<String, Collection<StudentAssessment>> m_assessmentLookup = null;
    private Map<String, Collection<Transcript>> m_apTranscriptLookup = null;
    private Map<String, Collection<StudentEnrollment>> m_retainedLookup = null;
    private Map<String, Collection<ConductAction>> m_conductActionLookup = null;
    private Map<String, Collection<ConductAction>> m_conductActionIssLookup = null;
    private Map<String, Collection<ConductAction>> m_conductActionOssLookup = null;
    private Map<String, Collection<ConductIncident>> m_conductIncidentAggressorLookup = null;
    private Map<String, Collection<ConductIncident>> m_conductIncidentTargetLookup = null;
    private HashMap<String, Integer> m_gradeLevelLookup = null;
    private String m_lowestPassingLetter = null;
    private String m_lowestPassingNumeric = null;
    private PlainDate m_schoolYearStartDate = null;
    private PlainDate m_schoolYearEndDate = null;
    private PlainDate m_summerStartDate = null;
    private PlainDate m_summerEndDate = null;
    private String m_contextOid = null;
    private String m_conductActionCorporalPunishment = null;
    private String m_conductActionIss = null;
    private String m_conductActionOss = null;
    private String m_conductActionExpWithSrvcs = null;
    private String m_conductActionExpWoSrvcs = null;
    private String m_conductActionExpZeroTol = null;
    private String m_conductActionRefLawEnf = null;
    private String m_conductActionSchoolArrest = null;
    private String m_conductIncidentBullyingSex = null;
    private String m_conductIncidentBullyingRace = null;
    private String m_conductIncidentBullyingDisability = null;
    private int apTestsPassed;
    private int apTestsFailed;
    private int apCoursesTaken;
    private DataDictionaryField m_lepField = null;
    private DataDictionaryField m_raceCodeField = null;
    private DataDictionaryField m_stateCourseCode = null;

    /**
     * TODO: Comment for gatherData method.
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid(10, 10);
        initializeGrid(grid);
        /*
         * Initialize member variables
         */
        m_contextOid = (String) getParameter(CONTEXT_OID_PARAM);
        m_summerStartDate = (PlainDate) getParameter(SUMMER_START_DATE_PARAM);
        m_summerEndDate = (PlainDate) getParameter(SUMMER_END_DATE_PARAM);
        m_lowestPassingLetter = (String) getParameter(LOWEST_PASSING_LETTER_PARAM);
        m_lowestPassingNumeric = (String) getParameter(LOWEST_PASSING_NUMERIC_PARAM);
        m_conductActionCorporalPunishment = (String) getParameter(CONDUCT_ACTION_CORP_PARAM);
        m_conductActionIss = (String) getParameter(CONDUCT_ACTION_ISS_PARAM);
        m_conductActionOss = (String) getParameter(CONDUCT_ACTION_OSS_PARAM);
        m_conductActionExpWithSrvcs = (String) getParameter(CONDUCT_ACTION_EXP_W_SRVCS_PARAM);
        m_conductActionExpWoSrvcs = (String) getParameter(CONDUCT_ACTION_EXP_W_O_SRVCS_PARAM);
        m_conductActionExpZeroTol = (String) getParameter(CONDUCT_ACTION_EXP_ZERO_TOL_PARAM);
        m_conductActionRefLawEnf = (String) getParameter(CONDUCT_ACTION_REF_LAW_PARAM);
        m_conductActionSchoolArrest = (String) getParameter(CONDUCT_ACTION_SCHL_ARREST_PARAM);
        m_conductIncidentBullyingSex = (String) getParameter(CONDUCT_INCIDENT_BULLY_SEX_PARAM);
        m_conductIncidentBullyingRace = (String) getParameter(CONDUCT_INCIDENT_BULLY_RACE_PARAM);
        m_conductIncidentBullyingDisability = (String) getParameter(CONDUCT_INCIDENT_BULLY_DISABILITY_PARAM);
        /*
         * Get first and last day of the school year selected on the input screen
         */
        Criteria criteria = new Criteria();
        criteria.addEqualTo(X2BaseBean.COL_OID, m_contextOid);
        QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
        DistrictSchoolYearContext schoolYearContext = (DistrictSchoolYearContext) getBroker().getBeanByQuery(query);

        m_schoolYearStartDate = schoolYearContext.getStartDate();
        m_schoolYearEndDate = schoolYearContext.getEndDate();
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_gradeLevelLookup = StudentManager.buildNumericGradeLevelMap(getBroker());
        m_lepField = m_dictionary.findDataDictionaryFieldByAlias(STUDENT_CONSIDERED_LEP_ALIAS);
        m_raceCodeField = m_dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);
        m_stateCourseCode = m_dictionary.findDataDictionaryFieldByAlias(STATE_COURSE_CODE_ALIAS);
        /*
         * Get selection of students to include in the report
         */
        Selection studentSelection = getStudentSelection();
        /*
         * Populate collections with records that belong to the students in the student selection
         */
        m_raceLookup = getRaceLookup(studentSelection);
        m_algebraTranscriptLookup = getAlgebraTranscriptLookup(studentSelection);
        m_assessmentLookup = getAssessmentLookup(studentSelection);
        m_apTranscriptLookup = getApTranscriptLookup(studentSelection);
        m_retainedLookup = getRetainedStudentsLookup(studentSelection);
        m_conductActionLookup = getConductActionLookup(studentSelection);
        m_conductActionIssLookup = getConductActionIssLookup(studentSelection);
        m_conductActionOssLookup = getConductActionOssLookup(studentSelection);
        m_conductIncidentAggressorLookup = getConductIncidentAggressorLookup(studentSelection);
        m_conductIncidentTargetLookup = getConductIncidentTargetLookup(studentSelection);
        /*
         * Iterate over students in selection and evaluate whether each student should be added
         * to the grid for each row and column in the report
         */
        addStudentCounts(grid, studentSelection);
        /*
         * Delete the selection object - it's only needed to run the student query above.
         */
        getBroker().deleteBean(studentSelection);
        grid.beforeTop();
        /* debugging */
        // addParameter("reportMessage", reportMessage);
        return grid;
    }

    /**
     * Returns a Selection of the students according to the user's selection on the input screen.
     *
     * @return Selection
     */
    private Selection getStudentSelection() {
        Selection selection = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());
        Collection<String> currentMembers = null;
        Criteria criteria = new Criteria();
        Criteria gradeLevelSubCriteria = new Criteria();
        Criteria kindergartenSubCriteria = new Criteria();
        /*
         * String lasid = (String) getParameter("lasid");
         * if (lasid != null)
         * {
         * criteria.addEqualTo(Student.COL_LOCAL_ID, lasid);
         * }
         */
        /*
         * If running for a single school, only query students who were in the grades of that school
         * last year This would include students from the start grade (may have been retained, and
         * so therefore would still be in the school's start grade) up to the end grade plus 1, as
         * students in the highest grade of the school last year would now be in 1 grade higher.
         */
        if (isSchoolContext()) {
            // Get start and number of grades for school. End grade = start grade + number of
            // grades.
            int startGradeValue = getSchool().getStartGrade();
            int endGradeValue = startGradeValue + getSchool().getNumberOfGrades() + 1;
            String startGrade = null;
            String endGrade = null;
            boolean includeKindergarten = false;
            // If start grade is PK or K, reset to grade 1 and set boolean.
            if (startGradeValue < 1) {
                includeKindergarten = true;
                startGradeValue = 1;
            }
            // Set start grade to string, with leading zero if single digit (example "01")
            startGrade = String.valueOf(startGradeValue);
            if (startGrade.length() == 1) {
                startGrade = "0" + startGrade;
            }
            // Set end grade to string, with leading zero if single digit (example "08")
            endGrade = String.valueOf(endGradeValue);
            if (endGrade.length() == 1) {
                endGrade = "0" + endGrade;
            }
            // Add criteria for student's grade level between the start and end grade
            gradeLevelSubCriteria.addGreaterOrEqualThan(SisStudent.COL_GRADE_LEVEL, startGrade);
            gradeLevelSubCriteria.addLessOrEqualThan(SisStudent.COL_GRADE_LEVEL, endGrade);
            // If school includes kindergarten, add OR subcriteria for the string of possible K
            // grades
            if (includeKindergarten) {
                Collection kindergartenGrades = Arrays.asList(KINDERGARTEN_GRADES);
                kindergartenSubCriteria.addIn(SisStudent.COL_GRADE_LEVEL, kindergartenGrades);
                gradeLevelSubCriteria.addOrCriteria(kindergartenSubCriteria);
            }
            criteria.addAndCriteria(gradeLevelSubCriteria);
        }
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, criteria);
        currentMembers = getBroker().getSubQueryCollectionByQuery(subQuery);
        for (String oid : currentMembers) {
            SelectionObject selectedObject =
                    X2BaseBean.newInstance(SelectionObject.class, getBroker().getPersistenceKey());
            selectedObject.setObjectOid(oid);
            selection.addToSelectionObjects(selectedObject);
            /* debugging */
            // studentCounter++;
        }
        /* debugging */
        // reportMessage += "\n" + subQuery.toString() + " " + String.valueOf(studentCounter);
        selection.setTimestamp(System.currentTimeMillis());
        getBroker().saveBeanForced(selection);
        return selection;
    }

    /**
     * Adds the student counts.
     *
     * @param grid ReportDataGrid
     * @param studentSelection Iterate over students in selection and evaluate whether each student
     *        should be added
     *        to the grid for each row and column in the report
     */
    private void addStudentCounts(ReportDataGrid grid, Selection studentSelection) {
        /*
         * Build student criteria.
         */
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX + X2BaseBean.COL_OID);
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
                apTestsPassed = 0;
                apTestsFailed = 0;
                apCoursesTaken = 0;
                for (int i = 0; i < NUMBER_OF_ROWS; i++) {
                    if (includeStudentInRow(student, i)) {
                        grid.gotoRow(i);
                        for (int j = 1; j <= NUMBER_OF_COLUMNS; j++) // Column 0 is reserved for the
                                                                     // row label
                        {
                            String columnId = Integer.toString(j);
                            Integer currentCount = (Integer) grid.get(columnId);
                            if (currentCount == null) {
                                currentCount = INTEGER_ZERO;
                            }
                            if (includeStudentInColumn(student, j)) {
                                currentCount = Integer.valueOf(currentCount.intValue() + 1);
                            }
                            grid.set(columnId, currentCount);
                        }
                    }
                }
            }
            for (int i = 0; i < NUMBER_OF_ROWS; i++) {
                grid.gotoRow(i);
                for (int j = NUMBER_OF_COLUMNS + 1; j <= NUMBER_OF_TOTAL_COLUMNS; j++) // Last 10
                                                                                       // columns
                                                                                       // are for
                                                                                       // totals
                {
                    String columnId = Integer.toString(j);
                    String maleColumnId = Integer.toString(j - NUMBER_OF_COLUMNS);
                    String femaleColumnId = Integer.toString(j - NUMBER_OF_COLUMNS / 2);
                    Integer maleCount = (Integer) grid.get(maleColumnId);
                    if (maleCount == null) {
                        maleCount = INTEGER_ZERO;
                    }
                    Integer femaleCount = (Integer) grid.get(femaleColumnId);
                    if (femaleCount == null) {
                        femaleCount = INTEGER_ZERO;
                    }
                    Integer currentCount = Integer.valueOf(maleCount.intValue()
                            + femaleCount.intValue());
                    grid.set(columnId, currentCount);
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
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX + X2BaseBean.COL_OID);
        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        SubQuery studentQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);
        Criteria raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, studentQuery);
        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        return getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 2048);
    }

    /**
     * Gets the assessment lookup.
     *
     * @param studentSelection Selection
     * @return Map
     */
    private Map<String, Collection<StudentAssessment>> getAssessmentLookup(Selection studentSelection) {
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + Transcript.COL_STUDENT_OID);
        /*
         * Get assessments for the school year
         */
        X2Criteria assessmentCriteria = new X2Criteria();

        assessmentCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        assessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, m_schoolYearStartDate);
        assessmentCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE, m_schoolYearEndDate);
        assessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_GRADE_LEVEL_CODE, SAT_ACT_MIN_GRADE_LEVEL);
        assessmentCriteria.addLessOrEqualThan(StudentAssessment.COL_GRADE_LEVEL_CODE, SAT_ACT_MAX_GRADE_LEVEL);
        X2Criteria assessmentTestSubCriteria = new X2Criteria();
        assessmentTestSubCriteria.addBeginsWith(StudentAssessment.REL_ASSESSMENT_DEFINITION
                + PATH_DELIMITER + AssessmentDefinition.COL_NAME, SAT_ASSESSMENT_NAME);
        X2Criteria assessmentActSubCriteria = new X2Criteria();
        assessmentActSubCriteria.addBeginsWith(StudentAssessment.REL_ASSESSMENT_DEFINITION
                + PATH_DELIMITER + AssessmentDefinition.COL_NAME, ACT_ASSESSMENT_NAME);
        assessmentTestSubCriteria.addOrCriteria(assessmentActSubCriteria);
        assessmentCriteria.addAndCriteria(assessmentTestSubCriteria);
        QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, assessmentCriteria);
        return getBroker().getGroupedCollectionByQuery(query, StudentAssessment.COL_STUDENT_OID, 2048);
    }

    /**
     * Gets the algebra transcript lookup.
     *
     * @param studentSelection Selection
     * @return Map
     */
    private Map<String, Collection<Transcript>> getAlgebraTranscriptLookup(Selection studentSelection) {
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + Transcript.COL_STUDENT_OID);
        Criteria transcriptCriteria = new X2Criteria();
        if (isSchoolContext()) {
            transcriptCriteria.addEqualTo(Transcript.COL_SCHOOL_OID, getSchool().getOid());
        }
        Collection algebra1Courses = Arrays.asList(ALGEBRA_1_COURSES);
        transcriptCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, m_contextOid);
        transcriptCriteria.addGreaterOrEqualThan(Transcript.COL_GRADE_LEVEL, ALGEBRA_1_MIN_GRADE_LEVEL);
        transcriptCriteria.addLessOrEqualThan(Transcript.COL_GRADE_LEVEL, ALGEBRA_1_MAX_GRADE_LEVEL);
        transcriptCriteria.addIn(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                + PATH_DELIMITER + m_stateCourseCode.getJavaName(), algebra1Courses);
        QueryByCriteria query = new QueryByCriteria(Transcript.class, transcriptCriteria);
        query.addPrefetchedRelationship(Transcript.REL_MASTER_SCHEDULE);
        return getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 2048);
    }

    /**
     * Gets the ap transcript lookup.
     *
     * @param studentSelection Selection
     * @return Map
     */
    private Map<String, Collection<Transcript>> getApTranscriptLookup(Selection studentSelection) {
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + Transcript.COL_STUDENT_OID);
        Criteria transcriptCriteria = new X2Criteria();
        if (isSchoolContext()) {
            transcriptCriteria.addEqualTo(Transcript.COL_SCHOOL_OID, getSchool().getOid());
        }
        Collection apCourses = Arrays.asList(AP_ALL);
        transcriptCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, m_contextOid);
        transcriptCriteria.addIn(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER
                + SchoolCourse.REL_COURSE + PATH_DELIMITER + m_stateCourseCode.getJavaName(), apCourses);
        QueryByCriteria query = new QueryByCriteria(Transcript.class, transcriptCriteria);
        return getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 2048);
    }

    /**
     * Gets the retained students lookup.
     *
     * @param studentSelection Selection
     * @return Map
     */
    private Map<String, Collection<StudentEnrollment>> getRetainedStudentsLookup(
                                                                                 Selection studentSelection) {
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + StudentEnrollment.COL_STUDENT_OID);
        Criteria enrollmentCriteria = new X2Criteria();
        if (isSchoolContext()) {
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, REPEAT_ENROLLMENT_TYPE);
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_REASON_CODE, getParameter(REPEAT_REASON_CODE_PARAM));
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_summerStartDate);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_summerEndDate);
        enrollmentCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, enrollmentCriteria);
        query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
        return getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, 2048);
    }

    /**
     * Gets the conduct action lookup.
     *
     * @param studentSelection Selection
     * @return Map
     */
    private Map<String, Collection<ConductAction>> getConductActionLookup(Selection studentSelection) {
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + ConductAction.COL_STUDENT_OID);
        Criteria criteria = new X2Criteria();
        if (isSchoolContext()) {
            criteria.addEqualTo(ConductAction.COL_SCHOOL_OID, getSchool().getOid());
        }
        List conductActionCodes = new ArrayList();
        if (!StringUtils.isEmpty(m_conductActionCorporalPunishment)) {
            conductActionCodes.add(m_conductActionCorporalPunishment);
        }
        if (!StringUtils.isEmpty(m_conductActionExpWithSrvcs)) {
            conductActionCodes.add(m_conductActionExpWithSrvcs);
        }
        if (!StringUtils.isEmpty(m_conductActionExpWoSrvcs)) {
            conductActionCodes.add(m_conductActionExpWoSrvcs);
        }
        if (!StringUtils.isEmpty(m_conductActionExpZeroTol)) {
            conductActionCodes.add(m_conductActionExpZeroTol);
        }
        if (!StringUtils.isEmpty(m_conductActionRefLawEnf)) {
            conductActionCodes.add(m_conductActionRefLawEnf);
        }
        if (!StringUtils.isEmpty(m_conductActionSchoolArrest)) {
            conductActionCodes.add(m_conductActionSchoolArrest);
        }

        criteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        criteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_schoolYearStartDate);
        criteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_schoolYearEndDate);
        criteria.addIn(ConductAction.COL_ACTION_CODE, conductActionCodes);
        QueryByCriteria query = new QueryByCriteria(ConductAction.class, criteria);

        return getBroker().getGroupedCollectionByQuery(query, ConductAction.COL_STUDENT_OID, 2048);
    }

    /**
     * Gets the conduct action oss lookup.
     *
     * @param studentSelection Selection
     * @return Map
     */
    private Map<String, Collection<ConductAction>> getConductActionOssLookup(Selection studentSelection) {
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + ConductAction.COL_STUDENT_OID);
        X2Criteria criteria = new X2Criteria();
        if (isSchoolContext()) {
            criteria.addEqualTo(ConductAction.COL_SCHOOL_OID, getSchool().getOid());
        }
        criteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        criteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_schoolYearStartDate);
        criteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_schoolYearEndDate);
        criteria.addBeginsWith(ConductAction.COL_ACTION_CODE, m_conductActionOss);
        QueryByCriteria query = new QueryByCriteria(ConductAction.class, criteria);

        return getBroker().getGroupedCollectionByQuery(query, ConductAction.COL_STUDENT_OID, 2048);
    }

    /**
     * Gets the conduct action iss lookup.
     *
     * @param studentSelection Selection
     * @return Map
     */
    private Map<String, Collection<ConductAction>> getConductActionIssLookup(Selection studentSelection) {
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + ConductAction.COL_STUDENT_OID);
        X2Criteria criteria = new X2Criteria();
        if (isSchoolContext()) {
            criteria.addEqualTo(ConductAction.COL_SCHOOL_OID, getSchool().getOid());
        }
        criteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        criteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_schoolYearStartDate);
        criteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_schoolYearEndDate);
        criteria.addBeginsWith(ConductAction.COL_ACTION_CODE, m_conductActionIss);
        QueryByCriteria query = new QueryByCriteria(ConductAction.class, criteria);

        return getBroker().getGroupedCollectionByQuery(query, ConductAction.COL_STUDENT_OID, 2048);
    }

    /**
     * Gets the conduct incident aggressor lookup.
     *
     * @param studentSelection Selection
     * @return Map
     */
    private Map<String, Collection<ConductIncident>> getConductIncidentAggressorLookup(Selection studentSelection) {
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + ConductIncident.COL_STUDENT_OID);
        Criteria criteria = new X2Criteria();
        if (isSchoolContext()) {
            criteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        }
        List conductIncidentCodes = new ArrayList();
        if (!StringUtils.isEmpty(m_conductIncidentBullyingSex)) {
            conductIncidentCodes.add(m_conductIncidentBullyingSex);
        }
        if (!StringUtils.isEmpty(m_conductIncidentBullyingRace)) {
            conductIncidentCodes.add(m_conductIncidentBullyingRace);
        }
        if (!StringUtils.isEmpty(m_conductIncidentBullyingDisability)) {
            conductIncidentCodes.add(m_conductIncidentBullyingDisability);
        }

        if (conductIncidentCodes.size() > 0) {
            criteria.addIn(ConductIncident.COL_INCIDENT_CODE, conductIncidentCodes);
        } else {
            return null;
        }
        criteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_schoolYearStartDate);
        criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_schoolYearEndDate);
        criteria.addIn(ConductIncident.COL_INCIDENT_CODE, conductIncidentCodes);
        QueryByCriteria query = new QueryByCriteria(ConductIncident.class, criteria);

        return getBroker().getGroupedCollectionByQuery(query, ConductIncident.COL_STUDENT_OID, 2048);
    }

    /**
     * Gets the conduct incident target lookup.
     *
     * @param studentSelection Selection
     * @return Map
     */
    private Map<String, Collection<ConductIncident>> getConductIncidentTargetLookup(Selection studentSelection) {
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + ConductIncident.COL_VICTIM_OID);
        Criteria criteria = new X2Criteria();
        if (isSchoolContext()) {
            criteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        }
        List conductIncidentCodes = new ArrayList();
        if (!StringUtils.isEmpty(m_conductIncidentBullyingSex)) {
            conductIncidentCodes.add(m_conductIncidentBullyingSex);
        }
        if (!StringUtils.isEmpty(m_conductIncidentBullyingRace)) {
            conductIncidentCodes.add(m_conductIncidentBullyingRace);
        }
        if (!StringUtils.isEmpty(m_conductIncidentBullyingDisability)) {
            conductIncidentCodes.add(m_conductIncidentBullyingDisability);
        }

        if (conductIncidentCodes.size() > 0) {
            criteria.addIn(ConductIncident.COL_INCIDENT_CODE, conductIncidentCodes);
        } else {
            return null;
        }
        criteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_schoolYearStartDate);
        criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_schoolYearEndDate);

        QueryByCriteria query = new QueryByCriteria(ConductIncident.class, criteria);

        return getBroker().getGroupedCollectionByQuery(query, ConductIncident.COL_STUDENT_OID, 2048);
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
            if (STATE_CODE_MULTIPLE_ETHNICITIES.equals(stateRaceCode)) {
                hasRaceCode = raceCodes != null && raceCodes.size() > 1;
            } else if (raceCodes != null && raceCodes.size() == 1) {
                Race race = raceCodes.iterator().next();
                String thisStateCode = Integer.valueOf(
                        m_dictionary.findStateReferenceCode(m_raceCodeField.getReferenceTableOid(),
                                race.getRaceCode()))
                        .toString();
                hasRaceCode = thisStateCode != null && thisStateCode.equals(stateRaceCode);
            }
        }
        return hasRaceCode;
    }


    /**
     * Checks for assessment record.
     *
     * @param student SisStudent
     * @return true, if successful
     */
    private boolean hasAssessmentRecord(SisStudent student) {
        boolean include = false;
        Collection<StudentAssessment> assessments = m_assessmentLookup.get(student.getOid());
        if (assessments != null && assessments.size() > 0) {
            include = true;
        }
        return include;
    }

    /**
     * Checks for passed course.
     *
     * @param student SisStudent
     * @param startGrade int
     * @param endGrade int
     * @return true, if successful
     */
    private boolean hasPassedCourse(SisStudent student, int startGrade, int endGrade) {
        boolean include = false;
        boolean passed = false;
        Collection<Transcript> transcripts = m_algebraTranscriptLookup.get(student.getOid());
        if (transcripts != null) {
            for (Transcript transcript : transcripts) {
                if (transcript.getFinalGrade() != null) {
                    String grade = transcript.getFinalGrade().trim();
                    if (StringUtils.isNumeric(grade)) {
                        passed = Double.valueOf(grade).doubleValue() >= Double.valueOf(m_lowestPassingNumeric)
                                .doubleValue();
                    } else {
                        passed = grade.compareTo(m_lowestPassingLetter) < 0;
                    }
                    Integer transcriptGradeLevel = m_gradeLevelLookup.get(transcript
                            .getGradeLevel());
                    include = passed && transcriptGradeLevel != null
                            && transcriptGradeLevel.intValue() >= startGrade
                            && transcriptGradeLevel.intValue() <= endGrade;
                } else {
                    include = false;
                }
                if (include) {
                    break;
                }
            }
        }
        return include;
    }

    /**
     * Checks for taken some ap tests.
     *
     * @param student SisStudent
     * @return true, if successful
     */
    private boolean hasTakenSomeApTests(SisStudent student) {
        boolean include = false;
        Collection<Transcript> transcripts = m_apTranscriptLookup.get(student.getOid());
        if (transcripts != null) {
            for (Transcript transcript : transcripts) {
                apCoursesTaken++;
                String apTestResult = (String) transcript.getFieldValueByAlias(AP_TEST_RESULT_ALIAS);
                if (AP_TEST_RESULT_PASS.equals(apTestResult)) {
                    apTestsPassed++;
                } else if (AP_TEST_RESULT_FAIL.equals(apTestResult)) {
                    apTestsFailed++;
                }
            }
            if (apCoursesTaken > (apTestsPassed + apTestsFailed) && (apTestsPassed + apTestsFailed) > 0) {
                include = true;
            }
        }
        return include;
    }

    /**
     * No parameters...calculations done in the tookSomeApTests method
     *
     * @return true, if successful
     */
    private boolean hasTakenAllApTests() {
        boolean include = false;
        if (apCoursesTaken > 0 && apCoursesTaken == (apTestsPassed + apTestsFailed)) {
            include = true;
        }
        return include;
    }

    /**
     * No parameters...calculations done in the tookSomeApTests method
     *
     * @return true, if successful
     */
    private boolean hasTakenNoApTests() {
        boolean include = false;
        if ((apCoursesTaken > 0) && ((apTestsPassed + apTestsFailed) == 0)) {
            include = true;
        }
        return include;
    }

    /**
     * No parameters...calculations done in the tookSomeApTests method
     *
     * @return true, if successful
     */
    private boolean hasPassedAllApTests() {
        boolean include = false;
        if (apTestsPassed > 0 && apTestsFailed == 0) {
            include = true;
        }
        return include;
    }

    /**
     * No parameters...calculations done in the tookSomeApTests method
     *
     * @return true, if successful
     */
    private boolean hasPassedSomeApTests() {
        boolean include = false;
        if (apTestsPassed > 0 && apTestsFailed > 0) {
            include = true;
        }
        return include;
    }

    /**
     * No parameters...calculations done in the tookSomeApTests method
     *
     * @return true, if successful
     */
    private boolean hasPassedNoApTests() {
        boolean include = false;
        if (apTestsPassed == 0 && apTestsFailed > 0) {
            include = true;
        }
        return include;
    }

    /**
     * Checks for conduct action record.
     *
     * @param conductActions Collection<ConductAction>
     * @param conductActionCode String
     * @return true, if successful
     */
    private boolean hasConductActionRecord(Collection<ConductAction> conductActions, String conductActionCode) {
        boolean include = false;
        if (conductActions != null && conductActions.size() > 0) {
            for (ConductAction conductAction : conductActions) {
                if (conductActionCode.startsWith(conductAction.getActionCode())) {
                    include = true;
                    break;
                }
            }
        }
        return include;
    }

    /**
     * Checks for one conduct record.
     *
     * @param conductAction Collection<ConductAction>
     * @return true, if successful
     */
    private boolean hasOneConductRecord(Collection<ConductAction> conductAction) {
        boolean include = false;
        if (conductAction != null && conductAction.size() == 1) {
            include = true;
        }
        return include;
    }

    /**
     * Checks for multiple conduct records.
     *
     * @param conductAction Collection<ConductAction>
     * @return true, if successful
     */
    private boolean hasMultipleConductRecords(Collection<ConductAction> conductAction) {
        boolean include = false;
        if (conductAction != null && conductAction.size() > 1) {
            include = true;
        }
        return include;
    }

    /**
     * Checks for conduct incident record.
     *
     * @param conductIncidents Collection<ConductIncident>
     * @param conductIncidentCode String
     * @return true, if successful
     */
    private boolean hasConductIncidentRecord(Collection<ConductIncident> conductIncidents, String conductIncidentCode) {
        boolean include = false;
        if (conductIncidents != null && conductIncidents.size() > 0) {
            for (ConductIncident conductIncident : conductIncidents) {
                if (conductIncidentCode.equals(conductIncident.getIncidentCode())) {
                    include = true;
                    break;
                }
            }
        }
        return include;
    }

    /**
     * Checks if is retained.
     *
     * @param student SisStudent
     * @return true, if is retained
     */
    private boolean isRetained(SisStudent student) {
        boolean include = false;
        Collection<StudentEnrollment> enrollments = m_retainedLookup.get(student.getOid());
        if (enrollments != null) {
            for (StudentEnrollment enrollment : enrollments) {
                if (enrollment.getYog() == student.getYog()) {
                    include = true;
                } else {
                    include = false;
                }
            }
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
            String stateCode = m_dictionary.findStateReferenceCode(m_lepField.getReferenceTableOid(), lepValue);
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
        include = gradeLevel != null && gradeLevel.intValue() >= startGrade && gradeLevel.intValue() <= endGrade;
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
        SisPerson person = student.getPerson();
        String genderCode = person != null ? person.getGenderCode() : null;
        boolean male = "M".equalsIgnoreCase(genderCode);
        switch (columnNumber) {
            case 1: // Hispanic/latino, male
                include = male && student.getPerson().getHispanicLatinoIndicator();
                break;
            case 2: // Indian, male
                include = male && hasRaceCode(student, STATE_CODE_INDIAN_NATIVE_AMERICAN);
                break;
            case 3: // Asian, male
                include = male && hasRaceCode(student, STATE_CODE_ASIAN);
                break;
            case 4: // Pac Islander/Hawaiian, male
                include = male && hasRaceCode(student, STATE_CODE_HAWAIIAN_PACIFIC_ISLANDER);
                break;
            case 5: // Black, male
                include = male && hasRaceCode(student, STATE_CODE_BLACK_AFRICAN_AMERICAN);
                break;
            case 6: // White, male
                include = male && hasRaceCode(student, STATE_CODE_CAUCASIAN);
                break;
            case 7: // Multiple races, male
                include = male && hasRaceCode(student, STATE_CODE_MULTIPLE_ETHNICITIES);
                break;
            case 8: // Special education, male
                include = male && student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE;
                break;
            case 9: // 504, male
                include = male && student.getSection504StatusCodeEnum() == Section504StatusCode.ACTIVE;
                break;
            case 10: // LEP, male
                include = male && isLep(student);
                break;
            case 11: // Hispanic/latino, female
                include = !male && student.getPerson().getHispanicLatinoIndicator();
                break;
            case 12: // Indian, female
                include = !male && hasRaceCode(student, STATE_CODE_INDIAN_NATIVE_AMERICAN);
                break;
            case 13: // Asian, female
                include = !male && hasRaceCode(student, STATE_CODE_ASIAN);
                break;
            case 14: // Pac Islander/Hawaiian, female
                include = !male && hasRaceCode(student, STATE_CODE_HAWAIIAN_PACIFIC_ISLANDER);
                break;
            case 15: // Black, female
                include = !male && hasRaceCode(student, STATE_CODE_BLACK_AFRICAN_AMERICAN);
                break;
            case 16: // White, female
                include = !male && hasRaceCode(student, STATE_CODE_CAUCASIAN);
                break;
            case 17: // Multiple races, female
                include = !male && hasRaceCode(student, STATE_CODE_MULTIPLE_ETHNICITIES);
                break;
            case 18: // Special education, female
                include = !male && student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE;
                break;
            case 19: // 504, female
                include = !male && student.getSection504StatusCodeEnum() == Section504StatusCode.ACTIVE;
                break;
            case 20: // LEP, female
                include = !male && isLep(student);
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
        switch (rowNumber) {
            case 0: // Students who passed Algebra I last year in grade 7 or 8
                include = hasPassedCourse(student, 7, 8);
                break;
            case 1: // Students who passed Algebra I last year in grade 9 or 10
                include = hasPassedCourse(student, 9, 10);
                break;
            case 2: // Students who passed Algebra I last year in grade 11 or 12
                include = hasPassedCourse(student, 11, 12);
                break;
            case 3: // Students in grades 9-12 who took the SAT or ACT
                include = hasAssessmentRecord(student);
                break;
            case 4: // Students who took AP tests for some AP courses taken
                include = hasTakenSomeApTests(student);
                break;
            case 5: // Students who took AP tests for all AP courses taken
                include = hasTakenAllApTests();
                break;
            case 6: // Students who took AP tests for none of the AP courses taken
                include = hasTakenNoApTests();
                break;
            case 7: // Students who passed all AP tests taken
                include = hasPassedAllApTests();
                break;
            case 8: // Students who passed some AP tests taken
                include = hasPassedSomeApTests();
                break;
            case 9: // Students who passed no AP tests taken
                include = hasPassedNoApTests();
                break;
            case 10: // Students retained in Kindergarten
                include = isInGradeRange(student, 0, 0) && isRetained(student);
                break;
            case 11: // Students retained in Grade 1
                include = "01".equals(student.getGradeLevel()) && isRetained(student);
                break;
            case 12: // Students retained in Grade 2
                include = "02".equals(student.getGradeLevel()) && isRetained(student);
                break;
            case 13: // Students retained in Grade 3
                include = "03".equals(student.getGradeLevel()) && isRetained(student);
                break;
            case 14: // Students retained in Grade 4
                include = "04".equals(student.getGradeLevel()) && isRetained(student);
                break;
            case 15: // Students retained in Grade 5
                include = "05".equals(student.getGradeLevel()) && isRetained(student);
                break;
            case 16: // Students retained in Grade 6
                include = "06".equals(student.getGradeLevel()) && isRetained(student);
                break;
            case 17: // Students retained in Grade 7
                include = "07".equals(student.getGradeLevel()) && isRetained(student);
                break;
            case 18: // Students retained in Grade 8
                include = "08".equals(student.getGradeLevel()) && isRetained(student);
                break;
            case 19: // Students retained in Grade 9
                include = "09".equals(student.getGradeLevel()) && isRetained(student);
                break;
            case 20: // Students retained in Grade 10
                include = "10".equals(student.getGradeLevel()) && isRetained(student);
                break;
            case 21: // Students retained in Grade 11
                include = "11".equals(student.getGradeLevel()) && isRetained(student);
                break;
            case 22: // Students retained in Grade 12
                include = "12".equals(student.getGradeLevel()) && isRetained(student);
                break;
            case 23: // Students without disabilities who received corporal punishment
                if (!StringUtils.isEmpty(m_conductActionCorporalPunishment)) {
                    include = student.getSpedStatusCodeEnum() != SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionLookup.get(student.getOid()),
                                    m_conductActionCorporalPunishment);
                }
                break;
            case 24: // Students without disabilities who received one or more in-school suspension
                if (!StringUtils.isEmpty(m_conductActionIss)) {
                    include = student.getSpedStatusCodeEnum() != SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionIssLookup.get(student.getOid()),
                                    m_conductActionIss);
                }
                break;
            case 25: // Students without disabilities who received only one out-of-school suspension
                if (!StringUtils.isEmpty(m_conductActionOss)) {
                    include = student.getSpedStatusCodeEnum() != SpedStatusCode.ACTIVE
                            && hasOneConductRecord(m_conductActionOssLookup.get(student.getOid()));
                }
                break;
            case 26: // Students without disabilities who received more than one out-of-school
                     // suspension
                if (!StringUtils.isEmpty(m_conductActionOss)) {
                    include = student.getSpedStatusCodeEnum() != SpedStatusCode.ACTIVE
                            && hasMultipleConductRecords(m_conductActionOssLookup.get(student.getOid()));
                }
                break;
            case 27: // Students without disabilities expelled with educational services
                if (!StringUtils.isEmpty(m_conductActionExpWithSrvcs)) {
                    include = student.getSpedStatusCodeEnum() != SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionLookup.get(student.getOid()),
                                    m_conductActionExpWithSrvcs);
                }
                break;
            case 28: // Students without disabilities expelled without educational services
                if (!StringUtils.isEmpty(m_conductActionExpWoSrvcs)) {
                    include = student.getSpedStatusCodeEnum() != SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionLookup.get(student.getOid()),
                                    m_conductActionExpWoSrvcs);
                }
                break;
            case 29: // Students without disabilities expelled under a Zero Tolerance policy
                if (!StringUtils.isEmpty(m_conductActionExpZeroTol)) {
                    include = student.getSpedStatusCodeEnum() != SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionLookup.get(student.getOid()),
                                    m_conductActionExpZeroTol);
                }
                break;
            case 30: // Students without disabilities referred to law enforcement
                if (!StringUtils.isEmpty(m_conductActionRefLawEnf)) {
                    include = student.getSpedStatusCodeEnum() != SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionLookup.get(student.getOid()),
                                    m_conductActionRefLawEnf);
                }
                break;
            case 31: // Students without disabilities subjected to school-related arrest
                if (!StringUtils.isEmpty(m_conductActionSchoolArrest)) {
                    include = student.getSpedStatusCodeEnum() != SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionLookup.get(student.getOid()),
                                    m_conductActionSchoolArrest);
                }
                break;
            case 32: // Students with disabilities who received corporal punishment
                if (!StringUtils.isEmpty(m_conductActionCorporalPunishment)) {
                    include = student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionLookup.get(student.getOid()),
                                    m_conductActionCorporalPunishment);
                }
                break;
            case 33: // Students with disabilities who received one or more in-school suspension
                if (!StringUtils.isEmpty(m_conductActionIss)) {
                    include = student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionIssLookup.get(student.getOid()),
                                    m_conductActionIss);
                }
                break;
            case 34: // Students with disabilities who received only one out-of-school suspension
                if (!StringUtils.isEmpty(m_conductActionOss)) {
                    include = student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE
                            && hasOneConductRecord(m_conductActionOssLookup.get(student.getOid()));
                }
                break;
            case 35: // Students with disabilities who received more than one out-of-school
                     // suspension
                if (!StringUtils.isEmpty(m_conductActionOss)) {
                    include = student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE
                            && hasMultipleConductRecords(m_conductActionOssLookup.get(student.getOid()));
                }
                break;
            case 36: // Students with disabilities expelled with educational services
                if (!StringUtils.isEmpty(m_conductActionExpWithSrvcs)) {
                    include = student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionLookup.get(student.getOid()),
                                    m_conductActionExpWithSrvcs);
                }
                break;
            case 37: // Students with disabilities expelled without educational services
                if (!StringUtils.isEmpty(m_conductActionExpWoSrvcs)) {
                    include = student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionLookup.get(student.getOid()),
                                    m_conductActionExpWoSrvcs);
                }
                break;
            case 38: // Students with disabilities expelled under a Zero Tolerance policy
                if (!StringUtils.isEmpty(m_conductActionExpZeroTol)) {
                    include = student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionLookup.get(student.getOid()),
                                    m_conductActionExpZeroTol);
                }
                break;
            case 39: // Students with disabilities referred to law enforcement
                if (!StringUtils.isEmpty(m_conductActionRefLawEnf)) {
                    include = student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionLookup.get(student.getOid()),
                                    m_conductActionRefLawEnf);
                }
                break;
            case 40: // Students with disabilities subjected to school-related arrest
                if (!StringUtils.isEmpty(m_conductActionSchoolArrest)) {
                    include = student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE
                            && hasConductActionRecord(m_conductActionLookup.get(student.getOid()),
                                    m_conductActionSchoolArrest);
                }
                break;
            case 41: // Students reported to have been bullied or harassed on the basis of sex
                if (!StringUtils.isEmpty(m_conductIncidentBullyingSex) && m_conductIncidentTargetLookup != null) {
                    include = hasConductIncidentRecord(m_conductIncidentTargetLookup.get(student.getOid()),
                            m_conductIncidentBullyingSex);
                }
                break;
            case 42: // Students reported to have been bullied or harassed on the basis of race
                if (!StringUtils.isEmpty(m_conductIncidentBullyingRace) && m_conductIncidentTargetLookup != null) {
                    include = hasConductIncidentRecord(m_conductIncidentTargetLookup.get(student.getOid()),
                            m_conductIncidentBullyingRace);
                }
                break;
            case 43: // Students reported to have been bullied or harassed on the basis of
                     // disability
                if (!StringUtils.isEmpty(m_conductIncidentBullyingDisability)
                        && m_conductIncidentTargetLookup != null) {
                    include = hasConductIncidentRecord(m_conductIncidentTargetLookup.get(student.getOid()),
                            m_conductIncidentBullyingDisability);
                }
                break;
            case 44: // Students disciplined for bullying on the basis of sex
                if (!StringUtils.isEmpty(m_conductIncidentBullyingSex) && m_conductIncidentAggressorLookup != null) {
                    include = hasConductIncidentRecord(m_conductIncidentAggressorLookup.get(student.getOid()),
                            m_conductIncidentBullyingSex);
                }
                break;
            case 45: // Students disciplined for bullying on the basis of race
                if (!StringUtils.isEmpty(m_conductIncidentBullyingRace) && m_conductIncidentAggressorLookup != null) {
                    include = hasConductIncidentRecord(m_conductIncidentAggressorLookup.get(student.getOid()),
                            m_conductIncidentBullyingRace);
                }
                break;
            case 46: // Students disciplined for bullying on the basis of disability
                if (!StringUtils.isEmpty(m_conductIncidentBullyingDisability)
                        && m_conductIncidentAggressorLookup != null) {
                    include = hasConductIncidentRecord(m_conductIncidentAggressorLookup.get(student.getOid()),
                            m_conductIncidentBullyingDisability);
                }
                break;
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
        grid.set("0", "Table 24 - Students in grades 7 or 8 who passed Algebra I");
        grid.append(); // Row 1
        grid.set("0", "Table 25.1 - Students in grades 9 or 10 who passed Algebra I");
        grid.append(); // Row 2
        grid.set("0", "Table 25.2 - Students in grades 11 or 12 who passed Algebra I");
        grid.append(); // Row 3
        grid.set("0", "Table 26 - Students in grades 9-12 who took the SAT or ACT");
        grid.append(); // Row 4
        grid.set("0", "Table 27.1 - Students who took AP tests for some AP courses taken");
        grid.append(); // Row 5
        grid.set("0", "Table 27.2 - Students who took AP tests for all AP courses taken");
        grid.append(); // Row 6
        grid.set("0", "Table 27.3 - Students who took AP courses, but did not take any AP tests");
        grid.append(); // Row 7
        grid.set("0", "Table 28.1 - Students who passed all AP tests taken");
        grid.append(); // Row 8
        grid.set("0", "Table 28.2 - Students who passed some AP tests taken");
        grid.append(); // Row 9
        grid.set("0", "Table 28.3 - Students who passed no AP tests taken");
        grid.append(); // Row 10
        grid.set("0", "Table 29.1 - Students retained in Kindergarten");
        grid.append(); // Row 11
        grid.set("0", "Table 29.2 - Students retained in Grade 1");
        grid.append(); // Row 12
        grid.set("0", "Table 29.3 - Students retained in Grade 2");
        grid.append(); // Row 13
        grid.set("0", "Table 29.4 - Students retained in Grade 3");
        grid.append(); // Row 14
        grid.set("0", "Table 29.5 - Students retained in Grade 4");
        grid.append(); // Row 15
        grid.set("0", "Table 29.6 - Students retained in Grade 5");
        grid.append(); // Row 16
        grid.set("0", "Table 30.1 - Students retained in Grade 6");
        grid.append(); // Row 17
        grid.set("0", "Table 30.2 - Students retained in Grade 7");
        grid.append(); // Row 18
        grid.set("0", "Table 30.3 - Students retained in Grade 8");
        grid.append(); // Row 19
        grid.set("0", "Table 31.1 - Students retained in Grade 9");
        grid.append(); // Row 20
        grid.set("0", "Table 31.2 - Students retained in Grade 10");
        grid.append(); // Row 21
        grid.set("0", "Table 31.3 - Students retained in Grade 11");
        grid.append(); // Row 22
        grid.set("0", "Table 31.4 - Students retained in Grade 12");
        grid.append(); // Row 23
        grid.set("0", "Table 34.A - Students without disabilities who received corporal punishment");
        grid.append(); // Row 24
        grid.set("0", "Table 34.B - Students without disabilities who received one or more in-school suspension");
        grid.append(); // Row 25
        grid.set("0", "Table 34.C - Students without disabilities who received only one out-of-school suspension");
        grid.append(); // Row 26
        grid.set("0", "Table 34.D - Students without disabilities who received more than one out-of-school suspension");
        grid.append(); // Row 27
        grid.set("0", "Table 34.E - Students without disabilities expelled with educational services");
        grid.append(); // Row 28
        grid.set("0", "Table 34.F - Students without disabilities expelled without educational services");
        grid.append(); // Row 29
        grid.set("0", "Table 34.G - Students without disabilities expelled under zero-tolerance policies");
        grid.append(); // Row 30
        grid.set("0", "Table 34.H - Students without disabilities referred to law enforcement");
        grid.append(); // Row 31
        grid.set("0", "Table 34.I - Students without disabilities subjected to school-related arrest");
        grid.append(); // Row 32
        grid.set("0", "Table 35.A - Students with disabilities who received corporal punishment");
        grid.append(); // Row 33
        grid.set("0", "Table 35.B - Students with disabilities who received one or more in-school suspension");
        grid.append(); // Row 34
        grid.set("0", "Table 35.C - Students with disabilities who received only one out-of-school suspension");
        grid.append(); // Row 35
        grid.set("0", "Table 35.D - Students with disabilities who received more than one out-of-school suspension");
        grid.append(); // Row 36
        grid.set("0", "Table 35.E - Students with disabilities expelled with educational services");
        grid.append(); // Row 37
        grid.set("0", "Table 35.F - Students with disabilities expelled without educational services");
        grid.append(); // Row 38
        grid.set("0", "Table 35.G - Students with disabilities expelled under zero-tolerance policies");
        grid.append(); // Row 39
        grid.set("0", "Table 35.H - Students with disabilities referred to law enforcement");
        grid.append(); // Row 40
        grid.set("0", "Table 35.I - Students with disabilities subjected to school-related arrest ");
        grid.append(); // Row 41
        grid.set("0", "Table 37.1 - Students reported to have been harassed or bullied on the basis of sex");
        grid.append(); // Row 42
        grid.set("0",
                "Table 37.2 - Students reported to have been harassed or bullied on the basis of race, color or national origin");
        grid.append(); // Row 43
        grid.set("0", "Table 37.3 - Students reported to have been harassed or bullied on the basis of disability ");
        grid.append(); // Row 44
        grid.set("0", "Table 38.1 - Students disciplined for bullying on the basis of sex");
        grid.append(); // Row 45
        grid.set("0", "Table 38.2 - Students disciplined for bullying on the basis of race, color or national origin");
        grid.append(); // Row 46
        grid.set("0", "Table 38.3 - Students disciplined for bullying on the basis of disability");
        /*
         * grid.append(); // Row 47
         * grid.set("0", "Table 39.1 - Non-IDEA students subjected to mechanical restraint");
         * grid.append(); // Row 48
         * grid.set("0", "Table 39.2 - Non-IDEA students subjected to physical restraint");
         * grid.append(); // Row 49
         * grid.set("0", "Table 39.3 - Non-IDEA students subjected to seclusion");
         * grid.append(); // Row 50
         * grid.set("0",
         * "Table 40.1 - Students with disabilities (IDEA) subjected to mechanical restraint");
         * grid.append(); // Row 51
         * grid.set("0",
         * "Table 40.2 - Students with disabilities (IDEA) subjected to physical restraint");
         * grid.append(); // Row 52
         * grid.set("0", "Table 40.3 - Students with disabilities (IDEA) subjected to seclusion");
         * grid.append(); // Row 53
         * grid.set("0", "Table 41.1 - Incidents of mechanical restraint");
         * grid.append(); // Row 54
         * grid.set("0", "Table 41.2 - Incidents of physical restraint");
         * grid.append(); // Row 55
         * grid.set("0", "Table 41.3 - Incidents of seclusion");
         * grid.append(); // Row 56
         * grid.set("0",
         * "Table 42 - FTE of teachers who were absent more than 10 days of the school year");
         */
    }
}
