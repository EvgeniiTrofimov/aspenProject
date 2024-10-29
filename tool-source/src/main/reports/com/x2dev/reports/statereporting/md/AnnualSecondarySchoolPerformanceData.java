/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.md;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GpaClassSizeLookup;
import com.x2dev.sis.tools.reports.TranscriptReportGrid;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Annual Secondary School Performance Summary" report for Allegany.
 *
 * @author X2 Development Corporation
 */
public class AnnualSecondarySchoolPerformanceData extends ReportJavaSourceNet {
    // Input parameters
    private static final String ACTIVE_ONLY_PARAM = "activeOnly";
    private static final String CONVERT_NUMERIC_PARAM = "convertNumeric";
    private static final String END_GRADE_INPUT_PARAM = "endGrade";
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String START_GRADE_INPUT_PARAM = "startGrade";
    private static final String STUDENT_SORT_PARAM = "studentSort";

    // Additional grid fields
    private static final String FIELD_DATA = "data";
    private static final String FIELD_FORMAT = "format";
    private static final String FIELD_STUDENT = "student";
    private static final String SCHOOL_FIELD = "school";

    // Subreport constants
    private static final String PAGE_1_FORMAT_PARAM = "MD-ASD-PRF-001-PG1";
    private static final String PAGE_2_FORMAT_PARAM = "MD-ASD-PRF-001-PG2";

    // Report parameters
    private static final String CLASS_SIZE_LOOKUP_PARAM = "classSizeLookup";
    private static final String GRAD_ENROLLMENT_MAP = "graduationEnrollments";
    private static final String GPA_CONTEXT = "gpaContext";
    private static final String GRAD_SUMMARY_PARAM = "gradSummary";
    private static final String GRAD_SUMMARY_TOTALS = "gradSummaryTotals";
    private static final String GRAD_SUMMARY_TOT_CREDITS = "gradSummaryTotalCredits";
    private static final String GRAD_TOTAL_BY_GRD_LEVEL = "gradSummaryTotalsByGradeLevel";
    private static final String HSA_ALGEBRA_SCORES = "hsaAlgebra";
    private static final String HSA_BIOLOGY_SCORES = "hsaBiology";
    private static final String HSA_COMPOSITE_SCORES = "hsaComposite";
    private static final String HSA_ENGLISH_SCORES = "hsaEnglish";
    private static final String HSA_GOVERNMENT_SCORES = "hsaGovernment";
    private static final String HSA_MOST_RECENT_TEST = "hsaMostRecentTest";

    // Configuration variables
    private static final int PAD_ROWS = 10;
    private static final int PAD_YEARS = 5;

    // Graduation program OID
    private static final String ALLEGANY_GRADUATION_PROGRAM_OID = "GPR0000002nP9t";

    // Graduation enrollment status
    private static final String GRADUATE_ENROLLMENT_STATUS = "Graduate";

    /*
     * Assessment definition OID's
     */
    private static final String HSA_ALGEBRA_ASSESS_DEF_OID = "ASD0000005gNjz";
    private static final String HSA_BIOLOGY_ASSESS_DEF_OID = "ASD0000005gNjj";
    private static final String HSA_ENGLISH_ASSESS_DEF_OID = "ASD0000005gNjU";
    private static final String HSA_GOV_ASSESS_DEF_OID = "ASD0000005gNk9";

    private Criteria m_assessmentCriteria;
    private SisStudent m_currentStudent;
    private Map<Integer, SisDistrictSchoolYearContext> m_districtContexts;
    private Criteria m_enrollmentCriteria;
    private int m_governmentPassingScore;
    private Map<String, StudentEnrollment> m_graduationEnrollment;
    private Map<String, Collection<String>> m_graduationRequirementCourses;
    private Map<String, String> m_graduationRequirementsReportCodes;
    private ReportDataGrid m_grid;
    private Map<String, Collection<StudentAssessment>> m_hsaAssessmentsByStudent;
    private Map<String, Integer> m_hsaAlgebraScore;
    private Map<String, Integer> m_hsaBiologyScore;
    private Map<String, Integer> m_hsaCompositeScore;
    private Map<String, Integer> m_hsaEnglishScore;
    private Map<String, Integer> m_hsaGovernmentScore;
    private Map<String, PlainDate> m_hsaMostRecentTest;
    private Report m_page1Report;
    private Report m_page2Report;
    private int m_rowCount;
    private Map<String, Collection<String>> m_studentGraduationSummaryCourses;
    private Map<String, Map<String, Map<String, BigDecimal>>> m_studentGraduationSummaryMap;
    private Map<String, BigDecimal> m_studentGraduationSummaryTotalCredits;
    private Map<String, Map<String, BigDecimal>> m_studentGraduationSummaryTotals;
    private Map<String, Map<String, BigDecimal>> m_studentGraduationSummaryTotalsByGradeLevel;

    /**
     * Enumeration of the transcript grid iteration stages.
     */
    private enum GridAnalysis {
        END_GRID,

        NO_CHANGE,

        START_GRID,

        STUDENT_CHANGE,

        YEAR_CHANGE
    }

    /**
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        loadGraduationCourseRequirements();
        loadGraduationRequirements();

        Criteria transcriptCriteria = buildCriteria();

        loadHsaAssessments();

        String enteredEndGrade = (String) getParameter(END_GRADE_INPUT_PARAM);
        String enteredStartGrade = (String) getParameter(START_GRADE_INPUT_PARAM);
        boolean convertNumeric = ((Boolean) getParameter(CONVERT_NUMERIC_PARAM)).booleanValue();

        m_grid = new ReportDataGrid();
        TranscriptReportGrid grid;

        String studentSort = (String) getParameter(STUDENT_SORT_PARAM);
        String[] studentSortOrder = getUserSortOrderAsStringArray(studentSort);

        String[] transcriptSortOrder =
                new String[] {Transcript.REL_DISTRICT_CONTEXT + "." + SisDistrictSchoolYearContext.COL_SCHOOL_YEAR,
                        Transcript.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME,
                        Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_DESCRIPTION};

        if (StringUtils.isInteger(enteredStartGrade) && StringUtils.isInteger(enteredEndGrade)) {
            grid = new TranscriptReportGrid(transcriptCriteria,
                    studentSortOrder,
                    transcriptSortOrder,
                    convertNumeric,
                    false,
                    Integer.parseInt(enteredStartGrade),
                    Integer.parseInt(enteredEndGrade),
                    getOrganization(),
                    getBroker());
        } else {
            grid = new TranscriptReportGrid(transcriptCriteria,
                    studentSortOrder,
                    convertNumeric,
                    false,
                    true,
                    getOrganization(),
                    getBroker());
        }

        grid.beforeTop();
        iterateGrid(grid);

        populateGraduationSummaryForGrades78Courses(transcriptCriteria);

        populateGraduationEnrollmentMap();

        addParameters();

        m_grid.beforeTop();
        return m_grid;
    }

    /**
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class);
        m_districtContexts = getBroker().getMapByQuery(query, SisDistrictSchoolYearContext.COL_SCHOOL_YEAR, 32);

        m_page1Report = ReportUtils.getReport(PAGE_1_FORMAT_PARAM, getBroker());
        m_page2Report = ReportUtils.getReport(PAGE_2_FORMAT_PARAM, getBroker());

        m_enrollmentCriteria = new Criteria();
        m_assessmentCriteria = new Criteria();

        m_hsaAlgebraScore = new HashMap<String, Integer>();
        m_hsaBiologyScore = new HashMap<String, Integer>();
        m_hsaCompositeScore = new HashMap<String, Integer>();
        m_hsaEnglishScore = new HashMap<String, Integer>();
        m_hsaGovernmentScore = new HashMap<String, Integer>();
        m_hsaMostRecentTest = new HashMap<String, PlainDate>();

        m_governmentPassingScore = ((Integer) getParameter("governmentPassing")).intValue();

        m_studentGraduationSummaryCourses = new HashMap<String, Collection<String>>();
        m_studentGraduationSummaryMap = new HashMap<String, Map<String, Map<String, BigDecimal>>>();
        m_studentGraduationSummaryTotals = new HashMap<String, Map<String, BigDecimal>>();
        m_studentGraduationSummaryTotalsByGradeLevel = new HashMap<String, Map<String, BigDecimal>>();
        m_studentGraduationSummaryTotalCredits = new HashMap<String, BigDecimal>();
    }

    /**
     * @see com.x2dev.sis.tools.reports.ReportJavaSource#initialize(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = (SisStudent) userData.getCurrentRecord(Student.class);
    }

    /**
     * Adds all parameters onto the parameter map which is passed onto the report format.
     */
    private void addParameters() {
        addParameter(GRAD_SUMMARY_PARAM, m_studentGraduationSummaryMap);
        addParameter(GRAD_SUMMARY_TOTALS, m_studentGraduationSummaryTotals);
        addParameter(GRAD_TOTAL_BY_GRD_LEVEL, m_studentGraduationSummaryTotalsByGradeLevel);
        addParameter(GRAD_SUMMARY_TOT_CREDITS, m_studentGraduationSummaryTotalCredits);

        GpaClassSizeLookup classSizeLookup = new GpaClassSizeLookup(getOrganization(), getBroker());
        addParameter(CLASS_SIZE_LOOKUP_PARAM, classSizeLookup);

        addParameter(GPA_CONTEXT, getGpaContextOid());

        addParameter(GRAD_ENROLLMENT_MAP, m_graduationEnrollment);

        addParameter(HSA_ALGEBRA_SCORES, m_hsaAlgebraScore);
        addParameter(HSA_BIOLOGY_SCORES, m_hsaBiologyScore);
        addParameter(HSA_COMPOSITE_SCORES, m_hsaCompositeScore);
        addParameter(HSA_ENGLISH_SCORES, m_hsaEnglishScore);
        addParameter(HSA_GOVERNMENT_SCORES, m_hsaGovernmentScore);
        addParameter(HSA_MOST_RECENT_TEST, m_hsaMostRecentTest);
    }

    /**
     * Returns the criteria used to retrieve the Transcript records for the report.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        Criteria criteria = new Criteria();

        if (m_currentStudent != null) {
            /*
             * Running for one student
             */
            criteria.addEqualTo(Transcript.COL_STUDENT_OID, m_currentStudent.getOid());
            m_enrollmentCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, m_currentStudent.getOid());
            m_assessmentCriteria.addEqualTo(StudentAssessment.COL_STUDENT_OID, m_currentStudent.getOid());
        } else {
            if (isSchoolContext()) {
                String schoolOid = ((SisSchool) getSchool()).getOid();
                criteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID, schoolOid);
                m_assessmentCriteria.addEqualTo(
                        StudentAssessment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID, schoolOid);
                m_enrollmentCriteria.addEqualTo(
                        StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID, schoolOid);
            }

            /*
             * Running for multiple students
             */
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(criteria, queryBy, queryString, Student.class, Transcript.COL_STUDENT_OID);
            addUserCriteria(m_assessmentCriteria, queryBy, queryString, SisStudent.class,
                    StudentAssessment.COL_STUDENT_OID);
            addUserCriteria(m_enrollmentCriteria, queryBy, queryString, Student.class,
                    StudentEnrollment.COL_STUDENT_OID);

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly) {
                String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
                criteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS,
                        activeCode);
                m_assessmentCriteria.addEqualTo(
                        StudentAssessment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS, activeCode);
                m_enrollmentCriteria.addEqualTo(
                        StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS, activeCode);
            }
        }

        criteria.addNotEqualTo(Transcript.COL_TRANSCRIPT_HIDE_IND, Boolean.TRUE);
        criteria.addNotEqualTo(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_HIDE_TRANSCRIPT_INDICATOR, Boolean.TRUE);

        return criteria;
    }

    /**
     * @param studentGrid
     */
    private void formatPage1(ReportDataGrid studentGrid) {
        studentGrid.beforeTop();
        m_grid.append();
        m_grid.set(FIELD_DATA, studentGrid);
        m_grid.set(FIELD_FORMAT, new ByteArrayInputStream(m_page1Report.getCompiledFormat()));
    }

    /**
     * @param student
     */
    private void formatPage2(SisStudent student) {
        ReportDataGrid page2Grid = new ReportDataGrid();
        page2Grid.append();
        page2Grid.set(FIELD_STUDENT, student);

        page2Grid.beforeTop();
        m_grid.append();
        m_grid.set(FIELD_DATA, page2Grid);
        m_grid.set(FIELD_FORMAT, new ByteArrayInputStream(m_page2Report.getCompiledFormat()));
    }

    /**
     * Gets the context of the most recently run GPA
     *
     * @return String
     */
    private String getGpaContextOid() {
        String oid = null;

        QueryByCriteria gpaHistoryQuery = new QueryByCriteria(GpaHistory.class);
        gpaHistoryQuery.addOrderByDescending(
                GpaHistory.REL_DISTRICT_CONTEXT + PATH_DELIMITER + SisDistrictSchoolYearContext.COL_SCHOOL_YEAR);

        GpaHistory mostRecentGpa = (GpaHistory) getBroker().getBeanByQuery(gpaHistoryQuery);

        if (mostRecentGpa != null) {
            oid = mostRecentGpa.getDistrictContextOid();
        }

        return oid;
    }

    /**
     * Determines whether is the passed government score qualifies as a passing score.
     *
     * @param governmentScore
     *
     * @return boolean
     */
    private boolean isPassingGovernmentScore(int governmentScore) {
        boolean isPassingGovernment = false;
        if (governmentScore >= m_governmentPassingScore) {
            isPassingGovernment = true;
        }

        return isPassingGovernment;
    }

    /**
     * Iterates over the grid to perform operations that require grid iteration.
     */
    private void iterateGrid(TranscriptReportGrid grid) {
        ReportDataGrid studentGrid = new ReportDataGrid();

        boolean insert = true;
        boolean iterate = !grid.isEmpty();

        SisSchool school = null;

        // Grid spacing variables
        m_rowCount = 0;
        int schoolYear = 0;
        int yearCount = 0;

        grid.next();

        // Start iteration
        while (iterate) {
            SisStudent student = grid.getStudent();
            schoolYear = grid.getTranscript().getDistrictContext().getSchoolYear();
            studentGrid.append(grid.getCurrentRow());

            populateGraduationRequirements(grid.getTranscript());

            m_rowCount++;

            switch (moveAndAnalyze(grid)) {
                case START_GRID:
                    m_rowCount = 0;

                    //$FALL-THROUGH$
                case NO_CHANGE:
                    student = grid.getStudent();
                    schoolYear = grid.getTranscript().getDistrictContext().getSchoolYear();

                    school = grid.getTranscript().getSchool();
                    grid.set(SCHOOL_FIELD, school);
                    break;

                case YEAR_CHANGE:
                    if (PAD_ROWS > 0) {
                        padRows(studentGrid, student, school, schoolYear, m_rowCount, insert);
                        yearCount++;
                    }

                    m_rowCount = 0;
                    break;

                case END_GRID:
                    if (m_rowCount > 1) {
                        m_rowCount++;
                    }

                    if (!studentGrid.getCurrentRow().equals(grid.getCurrentRow())) {
                        studentGrid.append(grid.getCurrentRow());
                    }

                    insert = false;
                    iterate = false;

                    // Populate graduation requirements summary for the last row on the grid
                    populateGraduationRequirements(grid.getTranscript());

                    // no break

                    //$FALL-THROUGH$
                case STUDENT_CHANGE:
                    if (PAD_ROWS > 0) {
                        padRows(studentGrid, student, school, schoolYear, m_rowCount, insert);
                        m_rowCount = 0;
                        yearCount++;
                    }

                    for (int i = yearCount; i < PAD_YEARS; i++) {
                        schoolYear = 1990 + i;
                        padRows(studentGrid, student, school, schoolYear, m_rowCount, insert);
                        m_rowCount = 0;
                    }

                    formatPage1(studentGrid);
                    formatPage2(student);
                    populateHsaScoresMap(student);
                    m_rowCount = 0;
                    studentGrid = new ReportDataGrid();

                    yearCount = 0;
                    break;
            }
        }

        grid.beforeTop();
    }

    /**
     * Loads a map of collection of course Oids keyed on requirement Oid.
     */
    private void loadGraduationCourseRequirements() {
        m_graduationRequirementCourses = new TreeMap<String, Collection<String>>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(GraduationCourseRequirement.REL_REQUIREMENT + PATH_DELIMITER +
                GraduationRequirement.COL_PROGRAM_STUDIES_OID, ALLEGANY_GRADUATION_PROGRAM_OID);

        QueryByCriteria query = new QueryByCriteria(GraduationCourseRequirement.class, criteria);
        query.addOrderByAscending(X2BaseBean.COL_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                GraduationCourseRequirement courseRequirement = (GraduationCourseRequirement) iterator.next();

                String requirementOid = courseRequirement.getRequirementOid();

                Collection<String> courseOids = m_graduationRequirementCourses.get(requirementOid);
                if (courseOids == null) {
                    courseOids = new ArrayList<String>();
                }

                courseOids.add(courseRequirement.getCourseOid());
                m_graduationRequirementCourses.put(requirementOid, courseOids);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a map of report codes (custom-defined field set up for the purpose of this report)
     * keyed on requirement Oid.
     */
    private void loadGraduationRequirements() {
        m_graduationRequirementsReportCodes = new HashMap<String, String>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(GraduationRequirement.COL_PROGRAM_STUDIES_OID, ALLEGANY_GRADUATION_PROGRAM_OID);

        QueryByCriteria query = new QueryByCriteria(GraduationRequirement.class, criteria);
        query.addOrderByAscending(GraduationRequirement.COL_CODE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                GraduationRequirement requirement = (GraduationRequirement) iterator.next();

                String reportCode = requirement.getFieldB001();

                if (!StringUtils.isEmpty(reportCode)) {
                    m_graduationRequirementsReportCodes.put(requirement.getOid(), reportCode);
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a map of collection of HSA assessment beans keyed on student OID.
     */
    private void loadHsaAssessments() {
        m_assessmentCriteria.addIn(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID,
                Arrays.asList(HSA_ALGEBRA_ASSESS_DEF_OID,
                        HSA_BIOLOGY_ASSESS_DEF_OID,
                        HSA_ENGLISH_ASSESS_DEF_OID,
                        HSA_GOV_ASSESS_DEF_OID));

        QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, m_assessmentCriteria);

        m_hsaAssessmentsByStudent = getBroker().getGroupedCollectionByQuery(query,
                StudentAssessment.COL_STUDENT_OID,
                1000);
    }

    /**
     * Moves to the next row on the grid if possible. Compares the previous record with the current
     * record and returns the change.
     *
     * @return gridChange
     */
    private GridAnalysis moveAndAnalyze(TranscriptReportGrid grid) {
        GridAnalysis gridAnalysis = GridAnalysis.NO_CHANGE;

        SisStudent lastStudent = null;
        int lastYear = -1;

        if (grid.currentRowNumber() != -1) {
            lastStudent = grid.getStudent();
            lastYear = grid.getTranscript().getDistrictContext().getSchoolYear();
        }

        grid.next();

        SisStudent nextStudent = grid.getStudent();
        int nextYear = grid.getTranscript().getDistrictContext().getSchoolYear();

        if (grid.currentRowNumber() == 0) {
            gridAnalysis = GridAnalysis.START_GRID;
        } else if (!nextStudent.equals(lastStudent)) {
            gridAnalysis = GridAnalysis.STUDENT_CHANGE;
        } else if (lastYear != -1 && lastYear != nextYear) {
            gridAnalysis = GridAnalysis.YEAR_CHANGE;
        } else if (grid.currentRowNumber() == grid.bottomRowNumber()) {
            gridAnalysis = GridAnalysis.END_GRID;
        }

        return gridAnalysis;
    }

    /**
     * Pads the rows of each transcript out to PAD_ROWS.
     *
     * @param grid ReportDataGrid
     * @param student SisStudent
     * @param school SisSchool
     * @param year int
     * @param count int
     * @param insert boolean
     */
    private void padRows(ReportDataGrid grid,
                         SisStudent student,
                         SisSchool school,
                         int year,
                         int count,
                         boolean insert) {
        Transcript temp = X2BaseBean.newInstance(Transcript.class, getBroker().getPersistenceKey());
        temp.setStudentOid(student.getOid());
        temp.setDistrictContextOid(m_districtContexts.get(Integer.valueOf(year)).getOid());
        temp.setCourseDescription("");
        temp.setTermCode("");
        temp.setTotalCredit(null);
        temp.setFailedTermCode("");
        temp.setSchoolCourseOid("PADDING");

        for (int i = count; i < PAD_ROWS; i++) {
            if (insert) {
                grid.insertRow(grid.currentRowNumber() + 1);
            } else {
                grid.append();
            }

            grid.set(TranscriptReportGrid.COL_STUDENT, temp.getStudent());
            grid.set(TranscriptReportGrid.COL_TRANSCRIPT_HEADER, temp);
            grid.set(SCHOOL_FIELD, school);
            grid.gotoRow(grid.currentRowNumber() + 1);
        }
    }

    /**
     * Populates a map of graduate enrollment records (with status code of "Graduate") keyed on
     * student Oid.
     */
    private void populateGraduationEnrollmentMap() {
        m_graduationEnrollment = new HashMap<String, StudentEnrollment>();

        m_enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        m_enrollmentCriteria.addEqualTo(StudentEnrollment.COL_STATUS_CODE, GRADUATE_ENROLLMENT_STATUS);

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, m_enrollmentCriteria);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) iterator.next();

                m_graduationEnrollment.put(enrollment.getStudentOid(), enrollment);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Builds two maps: one for graduation summary totals broken down by requirement and grade level
     * and another map for
     * graduation summary totals broken down by grade level.
     * <p>
     * <ol>
     * <li>Map<studentOid, Map<reportCode, Map<gradeLevel, credits>>>
     * <li>Map<studentOid, Map<reportCode, creditTotals>>
     * </ol>
     *
     * @param grid
     */
    private void populateGraduationRequirements(Transcript transcript) {
        String courseOid = "";
        if (transcript.getSchoolCourse() != null) {
            courseOid = transcript.getSchoolCourse().getCourseOid();
        }

        if (!StringUtils.isEmpty(courseOid) && transcript.getTotalCredit() != null) {
            SisStudent student = transcript.getStudent();

            Map<String, Map<String, BigDecimal>> creditsByReportCodes =
                    m_studentGraduationSummaryMap.get(student.getOid());
            if (creditsByReportCodes == null) {
                creditsByReportCodes = new HashMap<String, Map<String, BigDecimal>>();
            }

            Map<String, BigDecimal> creditTotalsByReportCodes = m_studentGraduationSummaryTotals.get(student.getOid());
            if (creditTotalsByReportCodes == null) {
                creditTotalsByReportCodes = new HashMap<String, BigDecimal>();
            }

            Map<String, BigDecimal> creditTotalsByGradeLevel =
                    m_studentGraduationSummaryTotalsByGradeLevel.get(student.getOid());
            if (creditTotalsByGradeLevel == null) {
                creditTotalsByGradeLevel = new HashMap<String, BigDecimal>();
            }

            for (String requirementOid : m_graduationRequirementCourses.keySet()) {
                Collection<String> coursesByRequirementOid = m_graduationRequirementCourses.get(requirementOid);
                if (!CollectionUtils.isEmpty(coursesByRequirementOid)) {
                    Collection<String> coursesByStudent = m_studentGraduationSummaryCourses.get(student.getOid());
                    if (coursesByStudent == null) {
                        coursesByStudent = new ArrayList<String>();
                    }

                    if (coursesByRequirementOid.contains(courseOid) && !coursesByStudent.contains(courseOid)) {
                        if (transcript.getTotalCredit().compareTo(new BigDecimal(0)) > 0) {
                            coursesByStudent.add(courseOid);
                            m_studentGraduationSummaryCourses.put(student.getOid(), coursesByStudent);
                        }

                        String key = m_graduationRequirementsReportCodes.get(requirementOid);

                        if (!StringUtils.isEmpty(key)) {
                            Map<String, BigDecimal> creditsByGradeLevel = creditsByReportCodes.get(key);
                            if (creditsByGradeLevel == null) {
                                creditsByGradeLevel = new HashMap<String, BigDecimal>();
                            }

                            BigDecimal creditTotals = creditTotalsByReportCodes.get(key);
                            if (creditTotals == null) {
                                creditTotals = new BigDecimal(0);
                            }

                            BigDecimal credits = creditsByGradeLevel.get(transcript.getGradeLevel());
                            if (credits == null) {
                                credits = new BigDecimal(0);
                            }

                            BigDecimal creditTotalsForGradeLevel =
                                    creditTotalsByGradeLevel.get(transcript.getGradeLevel());
                            if (creditTotalsForGradeLevel == null) {
                                creditTotalsForGradeLevel = new BigDecimal(0);
                            }

                            BigDecimal totalCreditsForStudent =
                                    m_studentGraduationSummaryTotalCredits.get(student.getOid());
                            if (totalCreditsForStudent == null) {
                                totalCreditsForStudent = new BigDecimal(0);
                            }

                            credits = credits.add(transcript.getTotalCredit());
                            creditsByGradeLevel.put(transcript.getGradeLevel(), credits);
                            creditsByReportCodes.put(key, creditsByGradeLevel);
                            m_studentGraduationSummaryMap.put(student.getOid(), creditsByReportCodes);

                            creditTotals = creditTotals.add(transcript.getTotalCredit());
                            creditTotalsByReportCodes.put(key, creditTotals);
                            m_studentGraduationSummaryTotals.put(student.getOid(), creditTotalsByReportCodes);

                            creditTotalsForGradeLevel = creditTotalsForGradeLevel.add(transcript.getTotalCredit());
                            creditTotalsByGradeLevel.put(transcript.getGradeLevel(), creditTotalsForGradeLevel);
                            m_studentGraduationSummaryTotalsByGradeLevel.put(student.getOid(),
                                    creditTotalsByGradeLevel);

                            totalCreditsForStudent = totalCreditsForStudent.add(transcript.getTotalCredit());
                            m_studentGraduationSummaryTotalCredits.put(student.getOid(), totalCreditsForStudent);
                        }
                    }
                }
            }
        }
    }

    /**
     * Additional method to populate graduation summary for courses taken during grades 7 and 8.
     *
     * @param transcriptCriteria
     */
    private void populateGraduationSummaryForGrades78Courses(Criteria transcriptCriteria) {
        Criteria grades78Criteria = transcriptCriteria.copy(true, true, true);

        Criteria grade7Criteria = new Criteria();
        grade7Criteria.addEqualTo(Transcript.COL_GRADE_LEVEL, "07");

        Criteria orCriteria = new Criteria();
        orCriteria.addEqualTo(Transcript.COL_GRADE_LEVEL, "08");
        orCriteria.addOrCriteria(grade7Criteria);

        grades78Criteria.addAndCriteria(orCriteria);

        QueryByCriteria query = new QueryByCriteria(Transcript.class, grades78Criteria);
        query.addOrderByAscending(Transcript.COL_STUDENT_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Transcript transcript = (Transcript) iterator.next();

                populateGraduationRequirements(transcript);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Populates a map of all HSA scores keyed on student OID.
     *
     * @param student
     */
    private void populateHsaScoresMap(SisStudent student) {
        String studentOid = student.getOid();

        int algebraScore = 0;
        int biologyScore = 0;
        int englishScore = 0;
        int governmentScore = 0;

        PlainDate mostRecentTestDate = null;

        Collection<StudentAssessment> assessments = m_hsaAssessmentsByStudent.get(studentOid);
        if (!CollectionUtils.isEmpty(assessments)) {
            for (StudentAssessment assessment : assessments) {
                PlainDate testDate = assessment.getDate();
                if (mostRecentTestDate == null || mostRecentTestDate.compareTo(testDate) < 0) {
                    mostRecentTestDate = testDate;
                }

                int scaleScore = assessment.getScaleScore().intValue();

                String assessDefOid = assessment.getAssessmentDefinitionOid();
                if (HSA_ALGEBRA_ASSESS_DEF_OID.equals(assessDefOid) &&
                        scaleScore > algebraScore) {
                    algebraScore = scaleScore;
                } else if (HSA_BIOLOGY_ASSESS_DEF_OID.equals(assessDefOid) &&
                        scaleScore > biologyScore) {
                    biologyScore = scaleScore;
                } else if (HSA_ENGLISH_ASSESS_DEF_OID.equals(assessDefOid) &&
                        scaleScore > englishScore) {
                    englishScore = scaleScore;
                } else if (HSA_GOV_ASSESS_DEF_OID.equals(assessDefOid) &&
                        scaleScore > governmentScore) {
                    governmentScore = scaleScore;
                }
            }
        }

        int compositeScore = algebraScore + biologyScore + englishScore;

        /*
         * Use government score only if student passed it.
         */
        if (isPassingGovernmentScore(governmentScore)) {
            compositeScore += governmentScore;
            m_hsaGovernmentScore.put(studentOid, Integer.valueOf(governmentScore));
        }

        m_hsaAlgebraScore.put(studentOid, Integer.valueOf(algebraScore));
        m_hsaBiologyScore.put(studentOid, Integer.valueOf(biologyScore));
        m_hsaCompositeScore.put(studentOid, Integer.valueOf(compositeScore));
        m_hsaEnglishScore.put(studentOid, Integer.valueOf(englishScore));
        m_hsaMostRecentTest.put(studentOid, mostRecentTestDate);
    }
}
