/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryRelationship;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.nav.Filter;
import com.follett.fsc.core.k12.web.nav.FilterException;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.GraduationManager;
import com.x2dev.sis.model.business.GraduationSummaryProcedure;
import com.x2dev.sis.model.business.graduation.GraduationHelper;
import com.x2dev.sis.tools.reports.GradeReportJavaSource;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.MapUtils;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.Query;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * Builds the data for BC's School Transcript report.
 * <p>
 * This contains an inner class overwriting the GraduationManager in order to apply BC-specific
 * business rules. It is
 * setup so it can be called from other tools as necessary.
 *
 * @author Follett Software Company
 */
public class SchoolTranscriptData extends GradeReportJavaSource { /* DEBUG */
    // Fields
    private static final String FIELD_COURSE_CODE = "courseCode";
    private static final String FIELD_EQUIVALENCY = "equivalency";
    private static final String FIELD_EXAM_MARK = "examMark";
    private static final String FIELD_FINAL_GRADE = "Final Grade";
    private static final String FIELD_FINAL_GRADE_LETTER = "Final Grade Letter";
    private static final String FIELD_FINAL_GRADE_12 = "Final Grade 12";
    private static final String FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String FIELD_IN_PROGRESS = "inProgress";
    private static final String FIELD_PROGRAM = "gradProgram";
    private static final String FIELD_REQUIREMENT_CODE = "requirementCode";
    private static final String FIELD_SCHOOL_COURSE = "sklCourse";
    private static final String FIELD_SESSION_DATE = "sessionDate";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_TRANSCRIPT = "transcript";

    // Report constants
    private static final String FOOTER_SUBREPORT_ID = "BC-OST-001-2";
    private static final String GRADES_SUBREPORT_ID = "BC-OST-001-1";
    private static final String REPORT_DATA = "reportData";
    private static final String REPORT_FORMAT = "reportFormat";

    // Params
    private static final String GRADE_LEVEL_08_PARAM = "gradeLevel08";
    private static final String GRADE_LEVEL_09_PARAM = "gradeLevel09";
    private static final String SECONDARY_STUDENT_PARAM = "secondaryStudent";
    private static final String SORT_PARAM = "sort";
    private static final String PRINT_09_PARAM = "printExp_09";
    private static final String PRINT_08_PARAM = "printExp_08";
    private static final String REQUIREMENT_MAP_PARAM = "requirementMap";

    // Other constants
    private static final String INCLUDE_BYPASS_CODE = "grade0809";
    private static final byte REVERSE_ORDER = 0;
    private static final byte STRAIGHT_ORDER = 1;

    // allowed grade levels for this report
    private static final String GRADE_LEVEL_08 = "08";
    private static final String GRADE_LEVEL_09 = "09";
    private static final String GRADE_LEVEL_10 = "10";
    private static final String GRADE_LEVEL_11 = "11";
    private static final String GRADE_LEVEL_12 = "12";

    // Aliases
    private static final String BLENDED_MARK_ALIAS = "trn-blended-mark";
    private static final String COURSE_CODE_ALIAS = "crs-external-code";
    private static final String EXAM_MARK_ALIAS = "trn-prov-exam-mark";
    private static final String RCD_TRAX_CODE_ALIAS = "rcd-exam-trax";
    private static final String TRN_COMPLETION_ALIAS = "trn-completion-date";
    private static final String TRN_END_DATE_ALIAS = "trn-end-date";
    private static final String TRN_TRAX_ALIAS = "trn-trax-override";

    // Report formats
    private static final SimpleDateFormat SYSTEM_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");

    // Graduation program categories
    private static final String TYPE_ADULT = "ADULT";
    private static final String TYPE_SCCP = "SCCP";

    // Graduation alternate requirements
    private static final Collection<String> ADULT_LEVELS = Arrays.asList(GRADE_LEVEL_11, GRADE_LEVEL_12);
    private static final Collection<String> SCCP_LEVELS = Arrays.asList(GRADE_LEVEL_10, GRADE_LEVEL_11, GRADE_LEVEL_12);

    // Enable to provide additional Aspen System log messages for debugging.
    private static final boolean DEBUG_MODE = false;
    private static final String DEBUG_LOG_PREFIX = "School Transcript Log Message";

    // Other constants
    private static final String ELECTIVE_REQUIREMENT_CATEGORY = "00";
    private static final int INITIAL_MAP_SIZE = 2048;
    private static final String TRAX_COMPLETE_SUFFIX = "(Q)";

    public static final String STATUS_COMPLETE = "complete";
    public static final String STATUS_INVALID = "invalid";
    public static final String STATUS_PROGRESS = "inProgress";

    public static final String GRADE_TO_IGNORE_1 = "W";
    public static final String GRADE_TO_IGNORE_2 = "WR";
    public static final String GRADE_TO_IGNORE_3 = "I";

    private Collection<String> m_gradeLevels;
    private boolean m_isGrade08Checked;
    private boolean m_isGrade09Checked;

    private Map<String, List<GraduationCourseRequirement>> m_courseRequirementsMap;
    private DecimalFormat m_decimalFormat;
    private Map<String, TranscriptColumnDefinition> m_finalColumnMap;
    private GradesManager m_gradesManager;
    private BcGraduationManager m_graduationManager;
    private Map<String, GraduationStudentProgram> m_programMap;
    private Map<String, Boolean> m_requirementMetMap;
    private Map<String, GraduationRequirement> m_requirementsByOid;
    private Map<String, List<GraduationRequirement>> m_requirementsMap;
    private Map<String, Collection<StudentSchedule>> m_studentScheduleMap;
    private Map<String, PlainDate> m_termEndDateMap;
    private Map<String, Collection<Transcript>> m_transcriptMap;
    private Collection<Transcript> m_transcriptsRemovedWithLowerGrade;
    private UserDataContainer m_userData;

    /*
     * Reference lookups
     */
    private Map<String, DataDictionary> m_extendedReferenceDictionaries;
    private Map<String, ReferenceCode> m_traxCodes;

    /**
     * Compares transcript grades to find the higher grade.
     *
     * @param transcript1 Transcript
     * @param transcript2 Transcript
     * @return int
     */
    int compareTranscriptGrades(Transcript transcript1, Transcript transcript2) {
        int comparison = 0;

        try {
            BigDecimal grade1 = getNumericGrade(transcript1, false);
            BigDecimal grade2 = getNumericGrade(transcript2, false);

            comparison = grade1.compareTo(grade2);

            if (comparison == 0) {
                grade1 = getNumericGrade(transcript1, true);
                grade2 = getNumericGrade(transcript2, true);

                comparison = grade1.compareTo(grade2);
            }
        } catch (Exception e) {
            // Do nothing, leave value as 0
        }

        return comparison;
    }

    /**
     * Returns the course code used in the report.
     *
     * @param course SchoolCourse
     * @return String
     */
    String getCourseCode(SchoolCourse course) {
        return (String) course.getCourse().getRootCourse().getFieldValueByAlias(COURSE_CODE_ALIAS);
    }

    /**
     * Returns the value for use in the "Final %" column of the report. If the course is
     * non-examinable, will display
     * the transcript's final grade. If the course is examinable, will display the Blended Mark
     * (alias trn-blended-mark). If the course is examinable and there is no blended mark, but the
     * TRAX override
     * is a Q code, then use the final grade.
     *
     * @param transcript Transcript
     * @param examinable boolean
     * @param trax String
     * @return String
     */
    String getFinalGrade(Transcript transcript, boolean examinable, String trax) {
        String finalGrade = transcript.getFinalGrade();
        String blendGrade = formatNumericGrade((String) transcript.getFieldValueByAlias(BLENDED_MARK_ALIAS));

        if (examinable && StringUtils.isEmpty(blendGrade) && trax != null && trax.endsWith(TRAX_COMPLETE_SUFFIX)) {
            blendGrade = finalGrade;
        }

        return examinable ? blendGrade : finalGrade;
    }

    /**
     * Checks the related Course(s) to see if an exam is required.
     *
     * @param schoolCourse SchoolCourse
     * @return boolean
     */
    boolean lookupExamStatus(SchoolCourse schoolCourse) {
        Course course = schoolCourse.getCourse();

        return course.getExamRequiredIndicator() || course.getRootCourse().getExamRequiredIndicator();
    }

    /**
     * Check the transcript record to see if it should be included on the report as completed or
     * in-progress.
     *
     * @param transcript Transcript
     * @param examinable boolean
     * @param trax String
     * @return String
     */
    protected String checkStatus(Transcript transcript, boolean examinable, String trax) {
        String status = STATUS_INVALID;

        SchoolCourse schoolCourse = transcript.getSchoolCourse();
        String blendGrade = getFinalGrade(transcript, examinable, trax);
        BigDecimal credit = transcript.getTotalCredit();
        String completionDate = (String) transcript.getFieldValueByAlias(TRN_COMPLETION_ALIAS);

        if (isGrade0809(schoolCourse)) {
            // Grade level 08 and 09 are allowed 0 earned credits so set a fake value for later
            // checks
            credit = BigDecimal.ONE;
        }

        /*
         * For a complete status, the course must meet one of the following requirements:
         * - Must be examinable with a blend grade and have a credit value > 0
         * - Must be non-examinable with a completion date and have a credit value > 0
         */
        if (((examinable && !StringUtils.isEmpty(blendGrade)) ||
                (!examinable && !StringUtils.isEmpty(completionDate))) &&
                credit != null && credit.doubleValue() > 0) // Final requirement of a credit value >
                                                            // 0 always exists
        {
            status = STATUS_COMPLETE;
        }

        // If a course is not complete, check if it is in progress
        if (!STATUS_COMPLETE.equals(status)) {
            if (((examinable && StringUtils.isEmpty(blendGrade)) ||
                    (!examinable && StringUtils.isEmpty(completionDate))) &&
                    !GRADE_TO_IGNORE_1.equals(transcript.getFinalGrade()) &&
                    !GRADE_TO_IGNORE_2.equals(transcript.getFinalGrade()) &&
                    !GRADE_TO_IGNORE_3.equals(transcript.getFinalGrade())) {
                status = STATUS_PROGRESS;
            }
        }

        return status;
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid mainGrid = new ReportDataGrid();

        Report gradeReport = ReportUtils.getReport(GRADES_SUBREPORT_ID, getBroker());
        Report footerReport = ReportUtils.getReport(FOOTER_SUBREPORT_ID, getBroker());

        /*
         * Build the query and sort based on user input
         */
        QueryByCriteria query = createQueryByCriteria(SisStudent.class, m_studentCriteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                Map<PlainDate, List<Transcript>> transcriptMap = getTranscriptMap(student);

                SortedMap<PlainDate, List<Transcript>> sortedMap = sortMap(transcriptMap, STRAIGHT_ORDER);
                ReportDataGrid studentGrid = buildStudentGrid(sortedMap, student);

                if (studentGrid.rowCount() > 0) {
                    GraduationProgram programOfStudy = null;
                    GraduationStudentProgram program = m_programMap.get(student.getOid());
                    if (program != null) {
                        programOfStudy = program.getProgramStudies();
                    }

                    studentGrid.beforeTop();

                    mainGrid.append();
                    mainGrid.set(FIELD_PROGRAM, programOfStudy);
                    mainGrid.set(REPORT_DATA, studentGrid);
                    mainGrid.set(REPORT_FORMAT, new ByteArrayInputStream(gradeReport.getCompiledFormat()));

                    mainGrid.append();
                    mainGrid.set(FIELD_PROGRAM, programOfStudy);
                    mainGrid.set(REPORT_DATA, buildEmptyGrid());
                    mainGrid.set(REPORT_FORMAT, new ByteArrayInputStream(footerReport.getCompiledFormat()));
                }
            }
        } finally {
            students.close();
        }

        addParameter(REQUIREMENT_MAP_PARAM, m_requirementMetMap);

        mainGrid.beforeTop();

        if (DEBUG_MODE) {
            updateSystemLog("************************************** End Log ***************************************");
        }

        return mainGrid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_finalColumnMap = new HashMap<String, TranscriptColumnDefinition>(16);
        m_graduationManager = new BcGraduationManager(getBroker());
        m_gradesManager = new GradesManager(getBroker());
        m_requirementMetMap = new HashMap<String, Boolean>(INITIAL_MAP_SIZE);
        m_termEndDateMap = new HashMap<>(128);
        m_transcriptsRemovedWithLowerGrade = new ArrayList<Transcript>();

        Boolean isGrade08Checked = (Boolean) getParameter(GRADE_LEVEL_08_PARAM);
        Boolean isGrade09Checked = (Boolean) getParameter(GRADE_LEVEL_09_PARAM);

        m_isGrade08Checked = isGrade08Checked.booleanValue();
        m_isGrade09Checked = isGrade09Checked.booleanValue();

        addParameter(PRINT_08_PARAM, isGrade08Checked);
        addParameter(PRINT_09_PARAM, isGrade09Checked);

        m_gradeLevels = new ArrayList<String>();
        m_gradeLevels.add(GRADE_LEVEL_10);
        m_gradeLevels.add(GRADE_LEVEL_11);
        m_gradeLevels.add(GRADE_LEVEL_12);

        if (m_isGrade08Checked) {
            m_gradeLevels.add(GRADE_LEVEL_08);
        }

        if (m_isGrade09Checked) {
            m_gradeLevels.add(GRADE_LEVEL_09);
        }

        buildStudentCriteria();
        m_studentScheduleMap = buildStudentScheduleMap();
        m_programMap = buildProgramStudiesMap();
        m_transcriptMap = buildTranscriptsMap();

        if (DEBUG_MODE) {
            if (!MapUtils.isEmpty(m_transcriptMap)) {
                for (String studentOid : m_transcriptMap.keySet()) {
                    Collection<Transcript> transcripts = m_transcriptMap.get(studentOid);
                    if (!CollectionUtils.isEmpty(transcripts)) {
                        for (Transcript transcript : transcripts) {
                            updateSystemLog("Student OID: " + studentOid + " - Transcript : "
                                    + transcript.getSchoolCourse().getNumber() + " year: "
                                    + transcript.getDistrictContext().getContextId());
                        }
                    }
                }
            }
        }

        buildGraduationReqMap();

        if (!m_programMap.isEmpty()) {
            m_courseRequirementsMap = buildCourseRequirementsMap();
        } else {
            m_courseRequirementsMap = new HashMap<String, List<GraduationCourseRequirement>>();
        }

        /*
         * Load reference lookups
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_extendedReferenceDictionaries = new HashMap<String, DataDictionary>(64);
        m_traxCodes = loadCodeMap(dictionary.findDataDictionaryFieldByAlias(TRN_TRAX_ALIAS));
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        if (DEBUG_MODE) {
            runOnApplicationServer();
        }

        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Adds a severe message to the system log using the prefix specified in this class.
     *
     * @param message String
     */
    protected void updateSystemLog(String message) {
        AppGlobals.getLog().severe(DEBUG_LOG_PREFIX + " - " + message);
    }

    /**
     * Build the course requirements for the graduation programs.
     *
     * @return Map<String, List<GraduationCourseRequirement>>
     */
    private Map<String, List<GraduationCourseRequirement>> buildCourseRequirementsMap() {
        X2Criteria partialCourseReqCriteria = new X2Criteria();
        partialCourseReqCriteria.addIn(GraduationCourseRequirement.REL_REQUIREMENT + PATH_DELIMITER +
                GraduationRequirement.COL_PROGRAM_STUDIES_OID, m_requirementsMap.keySet());

        QueryByCriteria partialQuery = new QueryByCriteria(GraduationCourseRequirement.class, partialCourseReqCriteria);
        Map<String, List<GraduationCourseRequirement>> partialCourseRequirments =
                getBroker().getGroupedCollectionByQuery(partialQuery,
                        GraduationCourseRequirement.COL_COURSE_OID,
                        1024);

        return partialCourseRequirments;
    }

    /**
     * Build the second part of report.
     *
     * @return data grid for current sub-report
     */
    private ReportDataGrid buildEmptyGrid() {
        ReportDataGrid emptyGrid = new ReportDataGrid();

        emptyGrid.append();
        emptyGrid.beforeTop();

        return emptyGrid;
    }

    /**
     * Build the map of graduation requirements keyed to the program OID.
     */
    private void buildGraduationReqMap() {
        m_requirementsMap = new HashMap<String, List<GraduationRequirement>>(64);
        m_requirementsByOid = new HashMap<String, GraduationRequirement>(2048);

        for (GraduationStudentProgram program : m_programMap.values()) {
            if (program != null) {
                String programOid = program.getProgramStudies().getOid();

                if (!m_requirementsMap.containsKey(programOid)) {
                    List<GraduationRequirement> requirements = m_graduationManager.getAllRequirements(programOid);
                    m_requirementsMap.put(programOid, requirements);

                    for (GraduationRequirement requirement : requirements) {
                        m_requirementsByOid.put(requirement.getOid(), requirement);
                    }
                }
            }
        }
    }

    /**
     * Build students programs studies map.
     *
     * @return Map of schedules
     */
    private Map<String, GraduationStudentProgram> buildProgramStudiesMap() {
        X2Criteria criteria = new X2Criteria();
        X2Criteria x2StudentCriteria = new X2Criteria();
        x2StudentCriteria.addAndCriteria(m_studentCriteria);
        criteria.addAndCriteria(x2StudentCriteria.copyWithAdjustedPath(GraduationStudentProgram.REL_STUDENT,
                GraduationStudentProgram.COL_STUDENT_OID));

        QueryByCriteria query = createQueryByCriteria(GraduationStudentProgram.class, criteria);
        query.addOrderByAscending(GraduationStudentProgram.COL_STUDENT_OID);
        query.addOrderByDescending(GraduationStudentProgram.COL_PRIMARY_INDICATOR);

        return getBroker().getMapByQuery(query, GraduationStudentProgram.COL_STUDENT_OID, INITIAL_MAP_SIZE);
    }

    /**
     * Build first sub report - student grades.
     *
     * @param transcriptMap SortedMap<PlainDate,List<Transcript>>
     * @param student SisStudent
     * @return ReportDataGrid
     */
    private ReportDataGrid buildStudentGrid(SortedMap<PlainDate, List<Transcript>> transcriptMap, SisStudent student) {
        ReportDataGrid studentGrid = new ReportDataGrid();

        /*
         * Load current schedule courses
         */
        Collection<StudentSchedule> currentSchedules = m_studentScheduleMap.get(student.getOid());

        /*
         * Initialize graduation requirement information
         */
        GraduationStudentProgram study = m_programMap.get(student.getOid());
        HashMap<String, List<SchoolCourse>> coursesGainedCredit = new HashMap<String, List<SchoolCourse>>();
        HashMap<String, List<SchoolCourse>> coursesTaken = new HashMap<String, List<SchoolCourse>>();
        HashMap<String, List<SchoolCourse>> coursesTaking = new HashMap<String, List<SchoolCourse>>();
        HashMap<String, Double> creditsGained = new HashMap<String, Double>();
        HashMap<String, Double> rawCreditsGained = new HashMap<String, Double>();
        HashMap<String, Double> creditsWaived = new HashMap<String, Double>();
        HashMap<String, Double> creditsRequired = new HashMap<String, Double>();
        HashMap<String, Double> creditsByCourse = new HashMap<String, Double>();
        HashMap<String, Double> creditsInProgress = new HashMap<String, Double>();
        HashMap<String, String> gradeLevelByCourse = new HashMap<String, String>();
        Map<String, Map<String, Object>> otherRequirementValues = new HashMap<String, Map<String, Object>>();
        List<String> satisfiedOtherRequirementOids = new ArrayList<String>();

        /*
         * Initialize sort order
         */
        List<String> sortOrder = Arrays.asList(FIELD_IN_PROGRESS,
                FIELD_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_GRADE_LEVEL,
                FIELD_COURSE_CODE,
                FIELD_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER);

        if (!transcriptMap.isEmpty()) {
            /*
             * If the student has a program of study defined, pull the graduation information
             */
            if (study != null) {
                try {
                    m_graduationManager.determineGraduationStatus(student,
                            m_userData,
                            study.getProgramStudiesOid(),
                            coursesGainedCredit,
                            coursesTaken,
                            coursesTaking,
                            new HashMap<String, List<SchoolCourse>>(),
                            new HashMap<String, List<String>>(),
                            creditsGained,
                            rawCreditsGained,
                            creditsWaived,
                            creditsRequired,
                            creditsByCourse,
                            creditsInProgress,
                            new HashMap<String, Double>(),
                            gradeLevelByCourse,
                            false,
                            m_courseRequirementsMap,
                            new HashMap<String, Map<String, String>>(),
                            otherRequirementValues,
                            satisfiedOtherRequirementOids,
                            null,
                            new LinkedList<String>());

                    /*
                     * Iterate over each requirement to determine if the student is meeting the
                     * requirements
                     */
                    boolean meeting = reviewRequirements(study,
                            coursesTaking,
                            rawCreditsGained,
                            creditsWaived,
                            creditsRequired,
                            creditsInProgress,
                            satisfiedOtherRequirementOids);

                    m_requirementMetMap.put(student.getOid(), Boolean.valueOf(meeting));
                } catch (FilterException fe) {
                    // Do nothing
                } catch (IOException ioe) {
                    // Do nothing
                } catch (JDOMException jdome) {
                    // Do nothing
                }
            }

            /*
             * Iterate over the transcript records and include on the report if necessary
             */
            for (PlainDate sessionDate : transcriptMap.keySet()) {
                Collection<Transcript> transcripts = transcriptMap.get(sessionDate);
                for (Transcript transcript : transcripts) {
                    /*
                     * Pull information for transcript status
                     */
                    SchoolCourse schoolCourse = transcript.getSchoolCourse();
                    boolean examinable = lookupExamStatus(schoolCourse);
                    String trax = (String) transcript.getFieldValueByAlias(TRN_TRAX_ALIAS);

                    String status = checkStatus(transcript, examinable, trax);

                    if (DEBUG_MODE) {
                        updateSystemLog("    Transcript - " + transcript.getSchoolCourse().getNumber() + " - year "
                                + transcript.getDistrictContext().getContextId() + " status: " + status);
                    }

                    if (!STATUS_INVALID.equals(status) && study != null) {
                        String requirementCode = toInclude(study.getProgramStudies(), transcript, examinable,
                                coursesGainedCredit, coursesTaking);

                        if (DEBUG_MODE) {
                            updateSystemLog("**** Transcript - " + transcript.getSchoolCourse().getNumber() + " - year "
                                    + transcript.getDistrictContext().getContextId() + " requirement code: "
                                    + requirementCode);
                        }

                        if (!transcript.getTranscriptHideInd() &&
                                (STATUS_PROGRESS.equals(status) || !StringUtils.isEmpty(requirementCode)
                                        || STATUS_COMPLETE.equals(status))) {
                            studentGrid.append();
                            studentGrid.set(FIELD_STUDENT, student);
                            studentGrid.set(FIELD_TRANSCRIPT, transcript);
                            studentGrid.set(FIELD_SCHOOL_COURSE, schoolCourse);
                            studentGrid.set(FIELD_COURSE_CODE, getCourseCode(schoolCourse));
                            studentGrid.set(FIELD_IN_PROGRESS, Boolean.valueOf(STATUS_PROGRESS.equals(status)));
                            studentGrid.set(FIELD_GRADE_LEVEL, getGradeLevel(schoolCourse));
                            studentGrid.set(FIELD_REQUIREMENT_CODE,
                                    INCLUDE_BYPASS_CODE.equals(requirementCode) ? "" : requirementCode);
                            studentGrid.set(FIELD_FINAL_GRADE_12, transcript.getFinalGrade());
                            studentGrid.set(FIELD_EQUIVALENCY, lookupTraxValue(trax));

                            if (examinable) {
                                String examMark =
                                        formatNumericGrade((String) transcript.getFieldValueByAlias(EXAM_MARK_ALIAS));
                                studentGrid.set(FIELD_EXAM_MARK, examMark);
                            }

                            /*
                             * Get completion date and format for report
                             */
                            String completionDate = getSessionDateString(transcript);
                            if (!StringUtils.isEmpty(completionDate)) {
                                try {
                                    studentGrid.set(FIELD_SESSION_DATE, SYSTEM_DATE_FORMAT.parse(completionDate));
                                } catch (ParseException e) {
                                    // Do nothing
                                }
                            }

                            /*
                             * Pull final grade info
                             */
                            String finalMark = getFinalGrade(transcript, examinable, trax);
                            if (!StringUtils.isEmpty(finalMark)) {
                                TranscriptColumnDefinition column =
                                        getFinalColumn(transcript.getTranscriptDefinition());
                                GradeScale gradeScale = column == null ? null : column.getGradeScale();

                                BigDecimal numericValue = null;
                                String letterValue = null;

                                if (StringUtils.isNumeric(finalMark)) {
                                    numericValue = new BigDecimal(finalMark);
                                    if (gradeScale != null) {
                                        letterValue = m_gradesManager.getLetterValue(numericValue, gradeScale,
                                                transcript.getSchool(),
                                                transcript.getSchoolCourseOid());
                                    }
                                } else {
                                    letterValue = finalMark;
                                    if (gradeScale != null) {
                                        numericValue = m_gradesManager.getNumericValue(letterValue, gradeScale,
                                                transcript.getSchool(),
                                                transcript.getSchoolCourseOid());
                                    }
                                }

                                String numericDisplay = finalMark;
                                if (numericValue != null) {
                                    numericDisplay = getDecimalFormat().format(numericValue);
                                }

                                studentGrid.set(FIELD_FINAL_GRADE, numericDisplay);
                                studentGrid.set(FIELD_FINAL_GRADE_LETTER, letterValue);
                            }
                        }
                    }
                }

                studentGrid.sort(sortOrder, true);
            }
        }

        /*
         * Append courses included in the student's current year schedule
         */
        Set<Transcript> allTranscripts = getMapValues(transcriptMap);

        /*
         * Transcripts removed with a lower grade could be a transcript for a current
         * StudentSchedule. If the old
         * transcript has the higher grade, the lower grade transcript tied to the current
         * StudentSchedule will not
         * be listed in the transcript map. Because of this, the current student schedule record
         * will appear as in
         * progress unless we add the transcripts removed with lower grades to the set of
         * transcripts to remove
         * from student schedules.
         */
        allTranscripts.addAll(m_transcriptsRemovedWithLowerGrade);

        // Note - BC may determine that in progress transcripts should not removed at this point.
        // This should be done here if they make that decision.

        /*
         * If the student has taken a course more than once:
         * - the original completed transcript should display
         * - the "repeat" in-progress transcript should display as in progress (with an asterik next
         * to it)
         * - the student schedule for this should not display because the in progress transcript is
         * displayed
         */
        Set<StudentSchedule> inProgressCourses = removeTranscriptCourses(currentSchedules, allTranscripts);

        ReportDataGrid currentScheduleGrid = new ReportDataGrid(5);
        if (inProgressCourses != null) {
            for (StudentSchedule studentSchedule : inProgressCourses) {
                SchoolCourse schoolCourse = studentSchedule.getSection().getSchoolCourse();
                String requirementCode =
                        study == null ? "" : toInclude(study.getProgramStudies(), schoolCourse, coursesTaking);

                currentScheduleGrid.append();
                currentScheduleGrid.set(FIELD_STUDENT, student);
                currentScheduleGrid.set(FIELD_SCHOOL_COURSE, schoolCourse);
                currentScheduleGrid.set(FIELD_COURSE_CODE, getCourseCode(schoolCourse));
                currentScheduleGrid.set(FIELD_REQUIREMENT_CODE, requirementCode);
                currentScheduleGrid.set(FIELD_IN_PROGRESS, Boolean.TRUE);
                currentScheduleGrid.set(FIELD_GRADE_LEVEL, getGradeLevel(schoolCourse));
                currentScheduleGrid.set(FIELD_SESSION_DATE, lookupSessionDate(studentSchedule));
            }
        }

        if (!currentScheduleGrid.isEmpty()) {
            currentScheduleGrid.sort(sortOrder, true);
            currentScheduleGrid.beforeTop();

            studentGrid.append(currentScheduleGrid);
        }

        return studentGrid;
    }

    /**
     * Build query for students according to input definitions.
     */
    @Override
    protected void buildStudentCriteria() {
        m_studentCriteria = new X2Criteria();

        if (m_currentStudent != null) {
            m_studentCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            addUserCriteria(m_studentCriteria, queryBy, (String) getParameter(QUERY_STRING_PARAM), null, null);

            if (isSchoolContext() && !queryBy.contains(CURRENT_KEY)) {
                m_studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());

                /*
                 * Include secondary students of the school if needed.
                 */
                if (((Boolean) getParameter(SECONDARY_STUDENT_PARAM)).booleanValue()) {
                    m_studentCriteria.addOrCriteria(StudentManager.buildSecondaryStudentCriteria(getSchool()));
                }
            }

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly) {
                m_studentCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                        SisStudent.COL_ENROLLMENT_STATUS));
            }
        }
    }

    /**
     * Build student schedules map which contain courses student continues or scheduled to be taken.
     *
     * @return Map of schedules
     */
    private Map<String, Collection<StudentSchedule>> buildStudentScheduleMap() {
        X2Criteria criteria = new X2Criteria();
        X2Criteria x2StudentCriteria = new X2Criteria();
        x2StudentCriteria.addAndCriteria(m_studentCriteria);

        criteria.addAndCriteria(
                x2StudentCriteria.copyWithAdjustedPath(StudentSchedule.REL_STUDENT, StudentSchedule.COL_STUDENT_OID));
        criteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);
        criteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_HIDE_TRANSCRIPT_INDICATOR, Boolean.TRUE);
        criteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        // Only include records associated with a term that covers the current day
        // criteria.addLessOrEqualThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
        // Section.REL_SCHEDULE_TERM + PATH_DELIMITER +
        // ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
        // ScheduleTermDate.COL_START_DATE, getPlainDate());
        // criteria.addGreaterOrEqualThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
        // Section.REL_SCHEDULE_TERM + PATH_DELIMITER +
        // ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
        // ScheduleTermDate.COL_END_DATE, getPlainDate());

        // Restrict record inclusion based on the enterprise-level course grade level
        X2Criteria courseCriteria = new X2Criteria();
        courseCriteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.COL_GRADE_LEVEL, m_gradeLevels);
        courseCriteria.addEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.COL_PARENT_COURSE_OID, getBroker().getPersistenceKey());

        Criteria rootCriteria = new Criteria();
        rootCriteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.REL_PARENT_COURSE + PATH_DELIMITER +
                Course.COL_GRADE_LEVEL, m_gradeLevels);

        courseCriteria.addOrCriteria(rootCriteria);
        criteria.addAndCriteria(courseCriteria);

        QueryByCriteria query = createQueryByCriteria(StudentSchedule.class, criteria);

        return getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, INITIAL_MAP_SIZE);
    }

    /**
     * Build transcript map according to input definitions.
     *
     * @return QueryByCriteria
     */
    private Map<String, Collection<Transcript>> buildTranscriptsMap() {
        X2Criteria criteria = new X2Criteria();
        X2Criteria x2StudentCriteria = new X2Criteria();
        x2StudentCriteria.addAndCriteria(m_studentCriteria);

        criteria.addAndCriteria(
                x2StudentCriteria.copyWithAdjustedPath(Transcript.REL_STUDENT, Transcript.COL_STUDENT_OID));

        // criteria.addEqualToIgnoreCase(Transcript.COL_COURSE_DESCRIPTION, "ENGLISH 10");

        // Restrict record inclusion based on the enterprise-level course grade level
        X2Criteria courseCriteria = new X2Criteria();
        courseCriteria.addIn(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.COL_GRADE_LEVEL, m_gradeLevels);
        courseCriteria.addEmpty(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.COL_PARENT_COURSE_OID, getBroker().getPersistenceKey());

        Criteria rootCriteria = new Criteria();
        rootCriteria.addIn(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.REL_PARENT_COURSE + PATH_DELIMITER +
                Course.COL_GRADE_LEVEL, m_gradeLevels);

        courseCriteria.addOrCriteria(rootCriteria);
        criteria.addAndCriteria(courseCriteria);

        QueryByCriteria query = createQueryByCriteria(Transcript.class, criteria);
        query.addOrderByAscending(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_GRADE_LEVEL);
        query.addOrderByAscending(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                + PATH_DELIMITER + Course.COL_NUMBER);
        query.addOrderByAscending(Transcript.COL_COURSE_DESCRIPTION);

        return getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, INITIAL_MAP_SIZE);
    }

    /**
     * Determines the graduation requirement code by requirement priority.
     * This should only be called in the event that a transcript is not being applied
     * to a requirement because all the requirements it could apply to have already been met by
     * other transcripts. If this course could apply to multiple requirements, the one with the
     * lowest priority
     * numeric value is selected. In the event of a tie for lowest numeric priority, if one is an
     * elective requirement,
     * the non-elective requirement is preferred.
     *
     * @param schoolCourse SchoolCourse
     * @param requirements Collection<GraduationRequirement>
     * @return String
     */
    public String determineCodeByRequirementPriority(SchoolCourse schoolCourse,
                                                     Collection<GraduationRequirement> requirements) {
        GraduationRequirement bestMatch = null;

        if (!CollectionUtils.isEmpty(requirements)) {
            for (GraduationRequirement requirement : requirements) {
                Collection<GraduationCourseRequirement> courseRequirements = requirement.getCourseRequirements();

                if (!CollectionUtils.isEmpty(courseRequirements)) {
                    Set<Object> courseOids = CollectionUtils.getPropertySet(courseRequirements,
                            GraduationCourseRequirement.COL_COURSE_OID);
                    if (courseOids.contains(schoolCourse.getCourse().getRootCourse().getOid())) {
                        /*
                         * Any of the following conditions must be true in order to update the code
                         * to a new value.
                         *
                         * The priority must currently be null (so the code is also null)
                         * OR
                         * The priority value for the saved code must be greater numerically than
                         * the current requirement's priority
                         * OR
                         * The priority value must be equal to the current requirement's priority,
                         * the code must currently be saved as an elective and the current priority
                         * is not an elective category.
                         * (The last condition is to ensure the code used is for a standard
                         * requirement in the event that a standard has the same priority as an
                         * elective requirement.)
                         */
                        if (bestMatch == null ||
                                bestMatch.getPriority() > requirement.getPriority() ||
                                (bestMatch.getPriority() == requirement.getPriority()
                                        && ELECTIVE_REQUIREMENT_CATEGORY.equals(bestMatch.getFieldA001())
                                        && !ELECTIVE_REQUIREMENT_CATEGORY.equals(requirement.getFieldA001()))) {
                            bestMatch = requirement;
                        }
                    }
                }
            }
        }

        return bestMatch != null ? bestMatch.getCode() : null;
    }

    /**
     * Format the passed value as a number (e.g. strip leading 0's).
     *
     * @param gradeValue String
     * @return String
     */
    private String formatNumericGrade(String gradeValue) {
        String formattedGrade = gradeValue;

        if (StringUtils.isNumeric(gradeValue)) {
            BigDecimal decimal = new BigDecimal(gradeValue);
            formattedGrade = getDecimalFormat().format(decimal);
        }

        return formattedGrade;
    }

    /**
     * Returns the decimal formatter. If one does not exist it is created.
     *
     * @return DecimalFormat
     */
    private DecimalFormat getDecimalFormat() {
        if (m_decimalFormat == null) {
            m_decimalFormat = new DecimalFormat("0.####");
        }

        return m_decimalFormat;
    }

    /**
     * Returns the final column for the passed transcript definition. The column is cached for
     * frequent lookup.
     *
     * @param definition TranscriptDefinition
     * @return TranscriptColumnDefinition
     */
    private TranscriptColumnDefinition getFinalColumn(TranscriptDefinition definition) {
        if (m_finalColumnMap == null) {
            m_finalColumnMap = new HashMap<String, TranscriptColumnDefinition>(16);
        }

        TranscriptColumnDefinition column = m_finalColumnMap.get(definition.getOid());

        if (column == null) {
            column = definition.getFinalColumnDefinition();
            m_finalColumnMap.put(definition.getOid(), column);
        }

        return column;
    }

    /**
     * Returns the grade level of the school course. The value is pulled from the related
     * enterprise-level course.
     *
     * @param schoolCourse SchoolCourse
     * @return String
     */
    private String getGradeLevel(SchoolCourse schoolCourse) {
        Course course = schoolCourse.getCourse().getRootCourse();

        return course.getGradeLevel();
    }

    /**
     * Get all transcripts from transcript map to compare with schedules courses.
     *
     * @param transcriptMap SortedMap<PlainDate,List<Transcript>>
     * @return Set of transcripts
     */
    private Set<Transcript> getMapValues(SortedMap<PlainDate, List<Transcript>> transcriptMap) {
        Set<Transcript> transcripts = new HashSet<Transcript>();

        if (!transcriptMap.isEmpty()) {
            Collection<List<Transcript>> lists = transcriptMap.values();
            for (List<Transcript> trn : lists) {
                transcripts.addAll(trn);
            }
        }

        return transcripts;
    }

    /**
     * Returns the numeric value of the final grade of the transcript record. If the numeric value
     * cannot be determined,
     * -1 is used to ensure it is the least value.
     *
     * @param transcript Transcript
     * @param overrideFinal boolean
     * @return BigDecimal
     */
    private BigDecimal getNumericGrade(Transcript transcript, boolean overrideFinal) {
        BigDecimal numericValue = null;
        String grade = overrideFinal ? transcript.getFinalGrade()
                : getFinalGrade(transcript, lookupExamStatus(transcript.getSchoolCourse()),
                        (String) transcript.getFieldValueByAlias(TRN_TRAX_ALIAS));

        if (!StringUtils.isEmpty(grade)) {
            TranscriptColumnDefinition column = getFinalColumn(transcript.getTranscriptDefinition());
            GradeScale gradeScale = column == null ? null : column.getGradeScale();

            if (StringUtils.isNumeric(grade)) {
                numericValue = new BigDecimal(grade);
            } else if (gradeScale != null) {
                try {
                    numericValue = m_gradesManager.getNumericValue(grade, gradeScale,
                            transcript.getSchool(),
                            transcript.getSchoolCourseOid());
                } catch (NullPointerException e) {
                    // Do nothing, let the grade be equivalent to -1
                }
            }
        }

        if (numericValue == null) {
            numericValue = new BigDecimal(-1);
        }

        return numericValue;
    }

    /**
     * Returns the in progress percentage for the passed program requirement
     *
     * See GraduationManager.getProgramInprogressStatus()
     *
     * @param requirementOid String
     * @param creditsInProgress HashMap<String,Double>
     * @param coursesTaking HashMap<String,List<SchoolCourse>>
     * @param creditsRequiredMap HashMap<String,Double>
     * @param rawCreditsGained HashMap<String,Double>
     * @return int
     */
    private int getRequirementInprogressStatus(String requirementOid,
                                               HashMap<String, Double> creditsInProgress,
                                               HashMap<String, List<SchoolCourse>> coursesTaking,
                                               HashMap<String, Double> creditsRequiredMap,
                                               HashMap<String, Double> rawCreditsGained) {
        int percent = 0;

        double creditsRequired = m_graduationManager.getTotalRequired(null, requirementOid, creditsRequiredMap);
        double creditsProgress = 0;

        List<SchoolCourse> currentCourses = coursesTaking.get(requirementOid);
        if (!CollectionUtils.isEmpty(currentCourses)) {
            for (SchoolCourse course : currentCourses) {
                if (course.getCredit() != null) {
                    creditsProgress += course.getCredit().doubleValue();
                }
            }
        }

        if (creditsRequired == 0 || (creditsRequired <= creditsProgress)) {
            percent = 100;
        } else {
            percent = (int) Math.round((creditsProgress) * 100 / creditsRequired);
        }

        return percent;
    }

    /**
     * Returns the completion date (alias "trn-completion-date") or course end date (alias
     * "trn-end-date").
     *
     * @param transcript Transcript
     * @return String
     */
    private String getSessionDateString(Transcript transcript) {
        return StringUtils.coalesce((String) transcript.getFieldValueByAlias(TRN_COMPLETION_ALIAS),
                (String) transcript.getFieldValueByAlias(TRN_END_DATE_ALIAS));
    }

    /**
     * Build transcript map with date key for student.
     *
     * @param student SisStudent
     * @return Map<PlainDate, Collection<Transcript>
     */
    private Map<PlainDate, List<Transcript>> getTranscriptMap(SisStudent student) {
        Map<PlainDate, List<Transcript>> transcriptMap = new HashMap<PlainDate, List<Transcript>>();
        Map<String, Transcript> courseMap = new HashMap<String, Transcript>();

        Collection<Transcript> unsortedTranscripts = m_transcriptMap.get(student.getOid());
        if (!CollectionUtils.isEmpty(unsortedTranscripts)) {
            // Order transcript records for tracking duplicates
            List<Transcript> transcripts = new ArrayList<Transcript>(unsortedTranscripts);
            Collections.sort(transcripts, new BcRepeatTranscriptComparator());

            for (Transcript transcript : transcripts) {
                boolean process = true;
                SchoolCourse schoolCourse = transcript.getSchoolCourse();

                // Track course numbers for grade comparison
                String courseCode = getCourseCode(schoolCourse);
                String courseKey = StringUtils.coalesce(courseCode, schoolCourse.getNumber());

                Transcript previousTranscript = courseMap.get(courseKey);
                if (previousTranscript != null) {
                    boolean examinable = lookupExamStatus(schoolCourse);

                    /*
                     * T30354124
                     *
                     * Do not remove the transcript with the lower grade in the event one transcript
                     * has been completed and the other is still in progress.
                     * In the event one is still in progress, BC expects both the completed and in
                     * progress versions to display.
                     */
                    boolean isOneCompleted = false;
                    boolean isOneInProgress = false;

                    String transcriptStatus = checkStatus(transcript, examinable,
                            (String) transcript.getFieldValueByAlias(TRN_TRAX_ALIAS));
                    String previousTranscriptStatus = checkStatus(previousTranscript, examinable,
                            (String) previousTranscript.getFieldValueByAlias(TRN_TRAX_ALIAS));

                    if (STATUS_PROGRESS.equals(transcriptStatus) || STATUS_PROGRESS.equals(previousTranscriptStatus)) {
                        isOneInProgress = true;
                    }

                    if (STATUS_COMPLETE.equals(transcriptStatus) || STATUS_COMPLETE.equals(previousTranscriptStatus)) {
                        isOneCompleted = true;
                    }

                    if (isOneInProgress && isOneCompleted) {
                        // Do nothing. Do not attempt to remove the lowest grade. See comment above.
                    } else {
                        int comparison = compareTranscriptGrades(transcript, previousTranscript);
                        if (comparison > 0) {
                            // New transcript has a higher grade - need to remove the previous one
                            for (List<Transcript> list : transcriptMap.values()) {
                                for (int i = 0; i < list.size(); ++i) {
                                    Transcript trn = list.get(i);
                                    if (trn.getOid().equals(previousTranscript.getOid())) {
                                        m_transcriptsRemovedWithLowerGrade.add(trn);
                                        list.remove(i);
                                        break;
                                    }
                                }
                            }
                        } else {
                            process = false;
                        }
                    }
                }

                if (process) {
                    if (DEBUG_MODE) {
                        updateSystemLog("Processing transcript: " + transcript.getSchoolCourse().getNumber()
                                + " - year " + transcript.getDistrictContext().getContextId());
                    }

                    PlainDate sessionDate = null;
                    String sessionDateAsString = getSessionDateString(transcript);

                    if (!StringUtils.isEmpty(sessionDateAsString)) {
                        try {
                            sessionDate = new PlainDate(SYSTEM_DATE_FORMAT.parse(sessionDateAsString));
                        } catch (ParseException pe) {
                            // Do nothing - take NULL value
                        }
                    }

                    if (sessionDate == null) {
                        if (transcript.getDistrictContext() != null) {
                            sessionDate = transcript.getDistrictContext().getEndDate();
                        } else {
                            sessionDate = getOrganization().getCurrentContext().getEndDate();
                        }
                    }

                    if (sessionDate != null) {
                        if (DEBUG_MODE) {
                            updateSystemLog("Updating transcript map. Session Date: " + sessionDate.toString()
                                    + " transcript:" + transcript.getSchoolCourse().getNumber() + " - year "
                                    + transcript.getDistrictContext().getContextId());
                        }

                        if (transcriptMap.containsKey(sessionDate)) {
                            transcriptMap.get(sessionDate).add(transcript);
                        } else {
                            List<Transcript> trnsTemp = new ArrayList<Transcript>();
                            trnsTemp.add(transcript);
                            transcriptMap.put(sessionDate, trnsTemp);
                        }
                    }

                    courseMap.put(courseKey, transcript);
                }
            }
        }

        return transcriptMap;
    }

    /**
     * Returns if the transcript is for a grade 08 or grade 09 course.
     *
     * @param course SchoolCourse
     * @return boolean
     */
    private boolean isGrade0809(SchoolCourse course) {
        String gradeLevel = getGradeLevel(course);

        return GRADE_LEVEL_08.equals(gradeLevel) || GRADE_LEVEL_09.equals(gradeLevel);
    }

    /**
     * Returns a map of the reference codes associated with the passed field.
     *
     * @param field DataDictionaryField
     * @return Map<String, ReferenceCode>
     */
    private Map<String, ReferenceCode> loadCodeMap(DataDictionaryField field) {
        Map<String, ReferenceCode> codes = new HashMap<String, ReferenceCode>(64);

        if (field != null) {
            ReferenceTable table = field.getReferenceTable();
            if (table != null) {
                /*
                 * If table is found, build code map and track the extended dictionary
                 */
                codes = table.getCodeMap(getBroker());

                DataDictionary dictionary = DataDictionary.getDistrictDictionary(table.getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());
                m_extendedReferenceDictionaries.put(table.getOid(), dictionary);
            }
        }

        return codes;
    }

    /**
     * Load the map of end dates for each schedule term associated with the provided schedule OID.
     *
     * @param scheduleOid String
     */
    private void loadTermEndDates(String scheduleOid) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.COL_SCHEDULE_OID,
                scheduleOid);

        QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
        query.addOrderByAscending(ScheduleTermDate.COL_SCHEDULE_TERM_OID);
        query.addOrderByDescending(ScheduleTermDate.COL_END_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            ScheduleTerm lastTerm = null;
            while (iterator.hasNext()) {
                ScheduleTermDate termDate = (ScheduleTermDate) iterator.next();
                ScheduleTerm term = termDate.getScheduleTerm();

                if (!ObjectUtils.match(term, lastTerm)) {
                    m_termEndDateMap.put(termDate.getScheduleTermOid(), termDate.getEndDate());
                }

                lastTerm = term;
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Returns the session date for the passed students chedule record. The session date of a
     * student schedule record
     * is the associated term's end date. The term-end date values are cached for faster lookup.
     *
     * @param studentSchedule StudentSchedule
     * @return PlainDate
     */
    private PlainDate lookupSessionDate(StudentSchedule studentSchedule) {
        PlainDate date = null;

        ScheduleTerm term = studentSchedule.getSection().getScheduleTerm();
        if (term != null) {
            date = m_termEndDateMap.get(term.getOid());
            if (date == null) {
                // Load all term end dates for the schedule
                loadTermEndDates(term.getScheduleOid());
                date = m_termEndDateMap.get(term.getOid());
            }
        }

        return date;
    }

    /**
     * Returns the Trax Code (UDF on reference code) based on the value set on the transcript
     * record.
     *
     * @param traxCode String
     * @return String
     */
    private String lookupTraxValue(String traxCode) {
        String value = "";

        ReferenceCode code = m_traxCodes.get(traxCode);
        if (code != null) {
            DataDictionary dictionary = m_extendedReferenceDictionaries.get(code.getReferenceTableOid());
            value = (String) code.getFieldValueByAlias(RCD_TRAX_CODE_ALIAS, dictionary);
        }

        return value;
    }

    /**
     * Checks if the requirement is being met based on the completed and in-progress courses.
     *
     * @param requirementOid String
     * @param coursesTaking HashMap<String,List<SchoolCourse>>
     * @param rawCreditsGained HashMap<String,Double>
     * @param creditsWaived HashMap<String,Double>
     * @param creditsRequired HashMap<String,Double>
     * @param creditsInProgress HashMap<String,Double>
     * @param satisfiedOtherRequirementOids List<String>
     * @return boolean
     */
    private boolean meetingRequirement(String requirementOid,
                                       HashMap<String, List<SchoolCourse>> coursesTaking,
                                       HashMap<String, Double> rawCreditsGained,
                                       HashMap<String, Double> creditsWaived,
                                       HashMap<String, Double> creditsRequired,
                                       HashMap<String, Double> creditsInProgress,
                                       List<String> satisfiedOtherRequirementOids) {
        int completePercent = 0;
        int progressPercent = 0;

        completePercent = m_graduationManager.getRequirementSatisfiedStatus(requirementOid,
                rawCreditsGained,
                creditsWaived,
                creditsRequired,
                satisfiedOtherRequirementOids);

        if (completePercent < 100) {
            progressPercent = getRequirementInprogressStatus(requirementOid,
                    creditsInProgress,
                    coursesTaking,
                    creditsRequired,
                    rawCreditsGained);
        }

        return completePercent + progressPercent >= 100;
    }

    /**
     * Get scheduled courses which are not in collection of transcripts.
     *
     * @param currentSchedules Collection<StudentSchedule>
     * @param transcripts Collection<Transcript>
     * @return Collection<SchoolCourse>
     */
    private Set<StudentSchedule> removeTranscriptCourses(Collection<StudentSchedule> currentSchedules,
                                                         Collection<Transcript> transcripts) {
        Set<StudentSchedule> schedules = new HashSet<StudentSchedule>();
        Set<String> transcriptCourses = new HashSet<String>();

        /*
         * Load the course OIDs represented by transcripts
         */
        for (Transcript transcript : transcripts) {
            transcriptCourses.add(transcript.getSchoolCourseOid());
        }

        /*
         * Add courses from the student schedule that are not in the list of transcripts
         */
        if (!CollectionUtils.isEmpty(currentSchedules)) {
            for (StudentSchedule schedule : currentSchedules) {
                if (!transcriptCourses.contains(schedule.getSection().getSchoolCourseOid())) {
                    schedules.add(schedule);
                }
            }
        }

        return schedules;
    }

    /**
     * Reviews the current requirements and checks to see if any completed courses can be
     * re-purposed from electives.
     *
     * @param study GraduationStudentProgram
     * @param coursesTaking HashMap<String,List<SchoolCourse>>
     * @param rawCreditsGained HashMap<String,Double>
     * @param creditsWaived HashMap<String,Double>
     * @param creditsRequired HashMap<String,Double>
     * @param creditsInProgress HashMap<String,Double>
     * @param satisfiedOtherRequirementOids List<String>
     * @return String
     */
    private boolean reviewRequirements(GraduationStudentProgram study,
                                       HashMap<String, List<SchoolCourse>> coursesTaking,
                                       HashMap<String, Double> rawCreditsGained,
                                       HashMap<String, Double> creditsWaived,
                                       HashMap<String, Double> creditsRequired,
                                       HashMap<String, Double> creditsInProgress,
                                       List<String> satisfiedOtherRequirementOids) {
        boolean meeting = true;

        List<GraduationRequirement> requirements =
                m_graduationManager.getDirectRequirements(study.getProgramStudiesOid(), null);

        // Find the elective requirement
        String electiveOid = null;
        for (GraduationRequirement requirement : requirements) {
            if (ELECTIVE_REQUIREMENT_CATEGORY.equals(requirement.getFieldA001())) {
                electiveOid = requirement.getOid();
                break;
            }
        }

        // Check if completed based on new distribution
        for (GraduationRequirement requirement : requirements) {
            if (!requirement.getOid().equals(electiveOid)) {
                meeting = meeting && meetingRequirement(requirement.getOid(),
                        coursesTaking,
                        rawCreditsGained,
                        creditsWaived,
                        creditsRequired,
                        creditsInProgress,
                        satisfiedOtherRequirementOids);
            }

            if (!meeting) {
                break;
            }
        }

        // If necessary, check the elective requirement
        if (meeting) {
            meeting = meeting && meetingRequirement(electiveOid,
                    coursesTaking,
                    rawCreditsGained,
                    creditsWaived,
                    creditsRequired,
                    creditsInProgress,
                    satisfiedOtherRequirementOids);
        }

        return meeting;
    }

    /**
     * Sort transcript map by date (key).
     *
     * @param source Map<PlainDate,List<Transcript>>
     * @param order byte
     * @return Sorted Map
     */
    private SortedMap<PlainDate, List<Transcript>> sortMap(Map<PlainDate, List<Transcript>> source, byte order) {
        SortedMap<PlainDate, List<Transcript>> destination;

        if (order == REVERSE_ORDER) {
            Comparator<PlainDate> reverse = Collections.reverseOrder();
            destination = new TreeMap<PlainDate, List<Transcript>>(reverse);
        } else {
            destination = new TreeMap<PlainDate, List<Transcript>>();
        }

        destination.putAll(source);

        return destination;
    }

    /**
     * Checks to see that the passed transcript is being included in the student's graduation
     * requirements. If so,
     * returns the requirement code that the course is counted toward
     *
     * @param program GraduationProgram
     * @param transcript Transcript
     * @param examinable boolean
     * @param coursesGainedCredit HashMap<String,List<SchoolCourse>>
     * @param coursesTaking HashMap<String,List<SchoolCourse>>
     * @return String
     */
    private String toInclude(GraduationProgram program,
                             Transcript transcript,
                             boolean examinable,
                             HashMap<String, List<SchoolCourse>> coursesGainedCredit,
                             HashMap<String, List<SchoolCourse>> coursesTaking) {
        String code = null;
        boolean isFail = false;

        // Check to see if the final grade is a failing grade
        SchoolCourse schoolCourse = transcript.getSchoolCourse();
        String finalGrade =
                getFinalGrade(transcript, examinable, (String) transcript.getFieldValueByAlias(TRN_TRAX_ALIAS));

        if (!StringUtils.isEmpty(finalGrade) && transcript.getTranscriptDefinition() != null) {
            TranscriptColumnDefinition column = getFinalColumn(transcript.getTranscriptDefinition());
            GradeScale gradeScale = column == null ? null : column.getGradeScale();

            String letterValue = null;

            if (StringUtils.isNumeric(finalGrade)) {
                if (gradeScale != null) {
                    BigDecimal numericValue = new BigDecimal(finalGrade);
                    letterValue = m_gradesManager.getLetterValue(numericValue, gradeScale,
                            transcript.getSchool(),
                            transcript.getSchoolCourseOid());
                }
            } else {
                letterValue = finalGrade;
            }

            if (gradeScale != null) {
                GradeScaleGradeDefinition gradeDefinition = m_gradesManager.getGradeDefinition(letterValue, gradeScale,
                        transcript.getSchoolOid(), transcript.getSchoolCourseOid());
                if (gradeDefinition != null && !gradeDefinition.getCreditIndicator()) {
                    isFail = true;
                }
            }
        }

        // Only include the record if it is not a failing grade
        if (!isFail) {
            if (TYPE_ADULT.equals(program.getDiplomaType()) && ADULT_LEVELS.contains(schoolCourse.getGradeLevel())) {
                code = INCLUDE_BYPASS_CODE;
            } else if (TYPE_SCCP.equals(program.getDiplomaType())
                    && SCCP_LEVELS.contains(schoolCourse.getGradeLevel())) {
                code = INCLUDE_BYPASS_CODE;
            } else {
                SchoolCourse equivalentCourse = transcript.getEquivalentSchoolCourse();

                for (String requirementOid : coursesGainedCredit.keySet()) {
                    List<SchoolCourse> courses = coursesGainedCredit.get(requirementOid);

                    if (!CollectionUtils.isEmpty(courses)) {
                        if (courses.contains(schoolCourse)
                                || (equivalentCourse != null && courses.contains(equivalentCourse))) {
                            GraduationRequirement requirement = m_requirementsByOid.get(requirementOid);
                            code = requirement.getFieldA001();
                        }
                    }
                }

                if (StringUtils.isEmpty(code)) {
                    for (String requirementOid : coursesTaking.keySet()) {
                        List<SchoolCourse> courses = coursesTaking.get(requirementOid);

                        if (!CollectionUtils.isEmpty(courses)) {
                            if (courses.contains(schoolCourse)
                                    || (equivalentCourse != null && courses.contains(equivalentCourse))) {
                                GraduationRequirement requirement = m_requirementsByOid.get(requirementOid);
                                code = requirement.getFieldA001();
                            }
                        }
                    }
                }

                if (StringUtils.isEmpty(code)) {
                    code = determineCodeByRequirementPriority(schoolCourse, m_requirementsMap.get(program.getOid()));
                }
            }

            // Always include grade08/09 courses if included
            if (StringUtils.isEmpty(code) && isGrade0809(schoolCourse)) {
                code = INCLUDE_BYPASS_CODE;
            }
        }

        return code;
    }

    /**
     * Checks to see that the passed school course is being included in the student's graduation
     * requirements. If so,
     * returns the requirement code that the course is counted toward.
     *
     * @param program GraduationProgram
     * @param course SchoolCourse
     * @param coursesTaking HashMap<String,List<SchoolCourse>>
     * @return String
     */
    private String toInclude(GraduationProgram program,
                             SchoolCourse course,
                             HashMap<String, List<SchoolCourse>> coursesTaking) {
        String code = null;

        if (TYPE_ADULT.equals(program.getDiplomaType()) && ADULT_LEVELS.contains(course.getGradeLevel())) {
            code = INCLUDE_BYPASS_CODE;
        } else if (TYPE_SCCP.equals(program.getDiplomaType()) && SCCP_LEVELS.contains(course.getGradeLevel())) {
            code = INCLUDE_BYPASS_CODE;
        } else {
            for (String requirementOid : coursesTaking.keySet()) {
                List<SchoolCourse> courses = coursesTaking.get(requirementOid);

                if (!CollectionUtils.isEmpty(courses)) {
                    if (courses.contains(course)) {
                        GraduationRequirement requirement = m_requirementsByOid.get(requirementOid);
                        code = requirement.getFieldA001();
                    }
                }

                if (!StringUtils.isEmpty(code)) {
                    break;
                }
            }
        }

        return code;
    }

    /**
     * Comparator based on the sorting of transcript records for BC. BC sorts transcripts
     * conditionally based on
     * 2 possible UDF date fields - TRN_FIELDA_025 or TRN_FIELDA_026. The check never compares
     * values as equal to
     * ensure that all values in the source map are present in the sorted map.
     *
     * @author Follett Software Company
     */
    class BcRepeatTranscriptComparator implements Comparator<Transcript> {

        /**
         * Instantiates a new bc repeat transcript comparator.
         */
        public BcRepeatTranscriptComparator() {
            // Does nothing
        }

        /**
         * Sort:
         * Course number | Ascending
         * Completion date | Descending
         * School year | Descending
         * Total credit | Descending.
         *
         * @param transcript1 Transcript
         * @param transcript2 Transcript
         * @return int
         * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
         */
        @Override
        public int compare(Transcript transcript1, Transcript transcript2) {
            int comparison = -1;

            // Check course number (ascending)
            String number1 = transcript1.getSchoolCourse().getNumber();
            String number2 = transcript2.getSchoolCourse().getNumber();

            comparison = number1.compareTo(number2);

            if (comparison == 0) {
                // Check completion date
                String date1 = getEndDate(transcript1);
                String date2 = getEndDate(transcript2);

                comparison = compareSafe(date2, date1);

                if (comparison == 0) {
                    // Check year
                    int year1 = transcript1.getDistrictContext().getSchoolYear();
                    int year2 = transcript2.getDistrictContext().getSchoolYear();

                    comparison = year1 == year2 ? 0 : year2 > year1 ? 1 : -1;

                    if (comparison == 0) {
                        // Check total credit
                        BigDecimal credit1 = transcript1.getTotalCredit();
                        BigDecimal credit2 = transcript2.getTotalCredit();

                        comparison = compareSafe(credit2, credit1);
                    }
                }
            }

            // Do not return 0 to ensure all records are included in the set
            return comparison == 0 ? -1 : comparison;
        }

        /**
         * Checks for null values in the comparison.
         *
         * @param object1 Comparable
         * @param object2 Comparable
         * @return int -1,0,1
         */
        private int compareSafe(Comparable object1, Comparable object2) {
            int comparison = -1;

            if (object1 == null && object2 == null) {
                comparison = 0;
            } else {
                if (object1 != null && object2 == null) {
                    comparison = 1;
                } else if (object1 == null && object2 != null) {
                    comparison = -1;
                } else {
                    comparison = object1.compareTo(object2);
                }
            }

            return comparison;
        }

        /**
         * Returns the end date of the transcript record - either the completion date or end date.
         *
         * @param transcript Transcript
         * @return String
         */
        private String getEndDate(Transcript transcript) {
            String completionDate = transcript.getFieldA025();
            String endDate = transcript.getFieldA026();

            return StringUtils.coalesce(completionDate, endDate);
        }
    }

    /**
     * Overrides the standard graduation manager with BC-specific logic.
     *
     * @author Follett Software Company
     */
    class BcGraduationManager {
        private X2Broker m_broker;
        private GraduationSummaryProcedure m_graduationSummaryProcedure;

        /**
         * Map of root Course OID with a value of the Collection of School Course objects taken, but
         * in progress.
         */
        private HashMap<String, Collection<SchoolCourse>> m_coursesAlreadyTakenButConsideredInProgress;

        /**
         * Constructs a new GraduationManager.
         *
         * @param broker X2Broker
         */
        public BcGraduationManager(X2Broker broker) {
            m_broker = broker;
            m_coursesAlreadyTakenButConsideredInProgress = new HashMap<String, Collection<SchoolCourse>>();
        }

        /**
         * Creates a list of course requirements records for the passed courses and requirement.
         *
         * @param courseOids Collection<String>
         * @param requirementOid String
         */
        public void createCourseRequirements(Collection<String> courseOids, String requirementOid) {
            for (String courseOid : courseOids) {
                GraduationCourseRequirement courseRequirement =
                        new GraduationCourseRequirement(m_broker.getPersistenceKey());
                courseRequirement.setCourseOid(courseOid);
                courseRequirement.setRequirementOid(requirementOid);

                m_broker.saveBeanForced(courseRequirement);
            }
        }

        /**
         * Determines the graduation status for the passed student.
         *
         * @param student SisStudent
         * @param userData UserDataContainer
         * @param programStudiesOid String
         * @param coursesGainedCredits Key is a Requirement oid,
         *        value is the list courses gained the credits for the requirement
         * @param coursesTaken Key is a Requirement oid,
         *        value is the list courses the student has taken towards the requirement
         * @param coursesTaking Key is a Requirement oid,
         *        value is the list courses the student is current taking towards the requirement
         * @param coursesRequesting Key is a Requirement oid,
         *        value is the list courses the student is current requesting towards the
         *        requirement
         * @param coursesToTranscripts Key is the school course oid
         *        value is the list of transcript oids already used for credits
         * @param creditsGained Key is a Requirement oid, value is the amount credits gained for the
         *        requirement
         * @param rawCreditsGained Key is a Requirement oid, value is the actual raw amount credits
         *        gained for the requirement without scaling.
         * @param creditsWaived Key is a Requirement oid, value is the amount credits waived for the
         *        requirement
         * @param creditsRequired Key is a Requirement oid, value is the amount credits required for
         *        the requirement
         * @param creditsByCourse Key is a SchoolCourse oid, value is the amount credits gained by
         *        the student for the course
         * @param creditsInProgress Key is a SchoolCourse oid, value is the amount credits the
         *        student can get from the course
         * @param creditsFuture Key is a SchoolCourse oid, value is the amount credits the student
         *        requests for future
         * @param gradeLevelByCourse Key is a SchoolCourse oid, value is the grade level the student
         *        is in when taking the course
         * @param projectFuture if true retrieve the information for the future request
         * @param partialCourseRequirments Key is course oid, value is list of course requirements
         *        (only partial credit requirements)
         * @param equivalentCourseDesc Key is the requirement and value is a a map of course
         *        descriptions where
         *        key is course oid, value is the course on the transcript.
         * @param otherRequirementValues Key is requirement, value is the map of field value pairs
         *        for other type requirement criteria fields.
         * @param satisfiedOtherRequirementOids Oids of other requirement types which have been
         *        satisfied.
         * @throws JDOMException exception
         * @throws IOException Signals that an I/O exception has occurred.
         * @throws FilterException exception
         */
        public void determineGraduationStatus(SisStudent student,
                                              UserDataContainer userData,
                                              String programStudiesOid,
                                              HashMap<String, List<SchoolCourse>> coursesGainedCredits,
                                              HashMap<String, List<SchoolCourse>> coursesTaken,
                                              HashMap<String, List<SchoolCourse>> coursesTaking,
                                              HashMap<String, List<SchoolCourse>> coursesRequesting,
                                              HashMap<String, List<String>> coursesToTranscripts,
                                              HashMap<String, Double> creditsGained,
                                              HashMap<String, Double> rawCreditsGained,
                                              HashMap<String, Double> creditsWaived,
                                              HashMap<String, Double> creditsRequired,
                                              HashMap<String, Double> creditsByCourse,
                                              HashMap<String, Double> creditsInProgress,
                                              HashMap<String, Double> creditsFuture,
                                              HashMap<String, String> gradeLevelByCourse,
                                              boolean projectFuture,
                                              Map<String, List<GraduationCourseRequirement>> partialCourseRequirments,
                                              Map<String, Map<String, String>> equivalentCourseDesc,
                                              Map<String, Map<String, Object>> otherRequirementValues,
                                              List<String> satisfiedOtherRequirementOids)
                throws JDOMException, IOException, FilterException {
            determineGraduationStatus(student, userData, programStudiesOid, coursesGainedCredits, coursesTaken,
                    coursesTaking,
                    coursesRequesting, coursesToTranscripts, creditsGained, rawCreditsGained, creditsWaived,
                    creditsRequired,
                    creditsByCourse, creditsInProgress, creditsFuture, gradeLevelByCourse, projectFuture,
                    partialCourseRequirments,
                    equivalentCourseDesc, otherRequirementValues, satisfiedOtherRequirementOids, null, null);
        }

        /**
         * Determines the graduation status for the passed student.
         *
         * @param student SisStudent
         * @param userData UserDataContainer
         * @param programStudiesOid String
         * @param coursesGainedCredits Key is a Requirement oid,
         *        value is the list courses gained the credits for the requirement
         * @param coursesTaken Key is a Requirement oid,
         *        value is the list courses the student has taken towards the requirement
         * @param coursesTaking Key is a Requirement oid,
         *        value is the list courses the student is current taking towards the requirement
         * @param coursesRequesting Key is a Requirement oid,
         *        value is the list courses the student is current requesting towards the
         *        requirement
         * @param coursesToTranscripts Key is the school course oid
         *        value is the list of transcript oids already used for credits
         * @param creditsGained Key is a Requirement oid, value is the amount credits gained for the
         *        requirement
         * @param rawCreditsGained Key is a Requirement oid, value is the actual raw amount credits
         *        gained for the requirement without scaling.
         * @param creditsWaived Key is a Requirement oid, value is the amount credits waived for the
         *        requirement
         * @param creditsRequired Key is a Requirement oid, value is the amount credits required for
         *        the requirement
         * @param creditsByCourse Key is a SchoolCourse oid, value is the amount credits gained by
         *        the student for the course
         * @param creditsInProgress Key is a SchoolCourse oid, value is the amount credits the
         *        student can get from the course
         * @param creditsFuture Key is a SchoolCourse oid, value is the amount credits the student
         *        requests for future
         * @param gradeLevelByCourse Key is a SchoolCourse oid, value is the grade level the student
         *        is in when taking the course
         * @param projectFuture if true retrieve the information for the future request
         * @param partialCourseRequirments Key is course oid, value is list of course requirements
         *        (only partial credit requirements)
         * @param equivalentCourseDesc Key is the requirement and value is a a map of course
         *        descriptions where
         *        key is course oid, value is the course on the transcript.
         * @param otherRequirementValues Key is requirement, value is the map of field value pairs
         *        for other type requirement criteria fields.
         * @param satisfiedOtherRequirementOids Oids of other requirement types which have been
         *        satisfied.
         * @param customColumnValues Key is a SchoolCourse oid, value is the grade column header to
         *        its value.
         * @param customColumnHeaders List of custom ColumnHeaders to display.
         * @throws JDOMException exception
         * @throws IOException Signals that an I/O exception has occurred.
         * @throws FilterException exception
         */
        public void determineGraduationStatus(SisStudent student,
                                              UserDataContainer userData,
                                              String programStudiesOid,
                                              HashMap<String, List<SchoolCourse>> coursesGainedCredits,
                                              HashMap<String, List<SchoolCourse>> coursesTaken,
                                              HashMap<String, List<SchoolCourse>> coursesTaking,
                                              HashMap<String, List<SchoolCourse>> coursesRequesting,
                                              HashMap<String, List<String>> coursesToTranscripts,
                                              HashMap<String, Double> creditsGained,
                                              HashMap<String, Double> rawCreditsGained,
                                              HashMap<String, Double> creditsWaived,
                                              HashMap<String, Double> creditsRequired,
                                              HashMap<String, Double> creditsByCourse,
                                              HashMap<String, Double> creditsInProgress,
                                              HashMap<String, Double> creditsFuture,
                                              HashMap<String, String> gradeLevelByCourse,
                                              boolean projectFuture,
                                              Map<String, List<GraduationCourseRequirement>> partialCourseRequirments,
                                              Map<String, Map<String, String>> equivalentCourseDesc,
                                              Map<String, Map<String, Object>> otherRequirementValues,
                                              List<String> satisfiedOtherRequirementOids,
                                              Map<String, Map<String, String>> customColumnValues,
                                              List<String> customColumnHeaders)
                throws JDOMException, IOException, FilterException {
            /*
             * Load graduation summary procedure and add any custom column headers. Save off the
             * aliases in case the headers get customized.
             * We need the aliases for later processing in determining grade values.
             */
            m_graduationSummaryProcedure = GraduationSummaryProcedure.initializeProcedure(null, m_broker);
            List<String> customColumnHeadersAliases = new ArrayList();
            if (customColumnHeaders != null) {
                customColumnHeadersAliases.addAll(customColumnHeaders);
            }
            m_graduationSummaryProcedure.customizeColumnHeaders(customColumnHeaders);

            // Determine the credits waived for each requirement
            getCreditWaived(student.getOid(), programStudiesOid, creditsWaived);

            /*
             * Determine the credits gained for each requirement
             */
            getUnitsGainedForProgram(student.getOid(), userData, programStudiesOid, coursesGainedCredits,
                    coursesTaken, coursesToTranscripts, creditsGained, rawCreditsGained,
                    creditsWaived, creditsRequired, creditsByCourse, gradeLevelByCourse,
                    partialCourseRequirments, equivalentCourseDesc, otherRequirementValues,
                    satisfiedOtherRequirementOids, customColumnHeadersAliases, customColumnValues);

            /*
             * Determine the course the student is currently taking towards each requirement.
             */
            String districtContextYearOid = OrganizationManager.getRootOrganization(m_broker).getCurrentContextOid();
            getCoursesTaking(student.getOid(), districtContextYearOid, programStudiesOid, coursesTaking,
                    creditsInProgress, false);

            /*
             * Determine the course the student is requesting for the build year towards each
             * requirement.
             */
            if (projectFuture) {
                String buildYearContextOid = student.getNextSchool().getBuildContextOid();
                getCoursesTaking(student.getOid(), buildYearContextOid, programStudiesOid, coursesRequesting,
                        creditsFuture, true);
            }

            processBcRules(programStudiesOid,
                    coursesGainedCredits,
                    coursesTaken,
                    coursesTaking,
                    rawCreditsGained,
                    creditsWaived,
                    creditsRequired,
                    creditsByCourse,
                    creditsInProgress,
                    satisfiedOtherRequirementOids);

            // Clear the list of courses already taken but considered in progress.
            m_coursesAlreadyTakenButConsideredInProgress.clear();
        }

        /**
         * Returns the all the requirements for the passed program studies including all nested
         * requirements.
         *
         * @param programStudiesOid String
         * @return List<GraduationRequirement>
         */
        public List<GraduationRequirement> getAllRequirements(String programStudiesOid) {
            X2Criteria subProgramCriteria = new X2Criteria();
            subProgramCriteria.addEqualTo(GraduationRequirement.COL_PROGRAM_STUDIES_OID, programStudiesOid);
            subProgramCriteria.addEqualTo(GraduationRequirement.COL_TYPE,
                    Integer.valueOf(GraduationRequirement.TYPE_PROGRAM));
            subProgramCriteria.addNotEmpty(GraduationRequirement.COL_SUB_PROGRAM_STUDIES_OID,
                    m_broker.getPersistenceKey());

            SubQuery subProgramQuery = new SubQuery(GraduationRequirement.class,
                    GraduationRequirement.COL_SUB_PROGRAM_STUDIES_OID, subProgramCriteria);
            Collection<String> programOids = m_broker.getSubQueryCollectionByQuery(subProgramQuery);
            programOids.add(programStudiesOid);

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(GraduationRequirement.COL_PROGRAM_STUDIES_OID, programOids);
            criteria.addNotEqualTo(GraduationRequirement.COL_TYPE,
                    Integer.valueOf(GraduationRequirement.TYPE_REQUIRMENT_GROUP));
            criteria.addNotEqualTo(GraduationRequirement.COL_TYPE, Integer.valueOf(GraduationRequirement.TYPE_PROGRAM));

            QueryByCriteria query = new QueryByCriteria(GraduationRequirement.class, criteria);
            query.addOrderByAscending(GraduationRequirement.COL_CODE);

            return new LinkedList<GraduationRequirement>(m_broker.getCollectionByQuery(query));
        }

        /**
         * Returns the list of direct under requirements for the passed program study and
         * requirement.
         *
         * @param programStudiesOid String
         * @param requirementOid if not null, return just the requirement
         * @return LinkedList<GraduationRequirement>
         */
        public LinkedList<GraduationRequirement> getDirectRequirements(String programStudiesOid,
                                                                       String requirementOid) {
            LinkedList<GraduationRequirement> requirements = new LinkedList<GraduationRequirement>();

            if (StringUtils.isEmpty(requirementOid)) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(GraduationRequirement.COL_PROGRAM_STUDIES_OID, programStudiesOid);
                criteria.addEmpty(GraduationRequirement.COL_PARENT_REQUIREMENT_OID, m_broker.getPersistenceKey());

                QueryByCriteria query = new QueryByCriteria(GraduationRequirement.class, criteria);
                query.addOrderByAscending(GraduationRequirement.COL_CODE);

                requirements = new LinkedList<GraduationRequirement>(m_broker.getCollectionByQuery(query));
            } else {
                GraduationRequirement requirement =
                        (GraduationRequirement) m_broker.getBeanByOid(GraduationRequirement.class, requirementOid);
                if (requirement != null) {
                    requirements.add(requirement);
                }
            }

            return requirements;
        }

        /**
         * Retrieves the list of other requirements for the passed requirement.
         *
         * @param requirement GraduationRequirement
         * @param subRequirementOids Key on the sub requirement type
         *        value is list of sub requirement oids.
         * @return filter
         */
        public Collection<Filter> getOtherRequirement(GraduationRequirement requirement,
                                                      HashMap<String, Collection<String>> subRequirementOids) {
            Collection<Filter> filters = new LinkedList<Filter>();

            String evalXml = requirement.getEvaluationDefinition();
            if (!StringUtils.isEmpty(evalXml)) {
                SAXBuilder builder = new SAXBuilder();
                org.jdom.Document document = null;
                try {
                    document = builder.build(new ByteArrayInputStream(evalXml.getBytes()));
                } catch (JDOMException e) {
                    e.printStackTrace();
                } catch (IOException e) {
                    e.printStackTrace();
                }

                if (document != null) {
                    Element root = document.getRootElement();
                    String evalType = root.getAttributeValue(GraduationManager.EVALUATION_TYPE_ATTRIB);

                    if (evalType.equals(String.valueOf(GraduationRequirement.TYPE_REQUIRMENT_GROUP))) {
                        Collection<String> requirementOids = new HashSet<String>();
                        Iterator requirementIterator =
                                root.getChildren(GraduationManager.REQUIREMENT_SUB_ELEMENT).iterator();
                        while (requirementIterator.hasNext()) {
                            Element subRequirement = (Element) requirementIterator.next();

                            String requirementOid =
                                    subRequirement.getAttributeValue(GraduationManager.REQUIREMENT_ATTRIB);

                            requirementOids.add(requirementOid);
                        }

                        String subRequirementType =
                                root.getAttributeValue(GraduationManager.REQUIREMENT_SUB_RELATION_ATTRIB);
                        subRequirementOids.put(subRequirementType, requirementOids);
                    } else if (evalType.equals(String.valueOf(GraduationRequirement.TYPE_OTHER))) {
                        DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
                        List filterElements = root.getChildren(Filter.FILTER_ELEMENT);
                        if (filterElements != null) {
                            /*
                             * Adds each filter to the filter definition.
                             */
                            Iterator filterIterator = filterElements.iterator();
                            while (filterIterator.hasNext()) {
                                Element filterElement = (Element) filterIterator.next();
                                Filter filter = new Filter(filterElement, dictionary);

                                filters.add(filter);
                            }
                        }
                    }
                }
            }

            return filters;
        }

        /**
         * Returns the overall satisfied percentage for the passed program study.
         *
         * The formula for the overall satisfied percentage is as follow:
         * 1. The total number of requirements are the overall credit requirement for the program of
         * study
         * plus the number of direct requirements for the program of study
         * 2. Any non-credit requirement or requirement with zero credit is considerd as 1 / total
         * number
         * of requirements
         * 3. All credit requirements are weighted based on percentage of their required credits
         * compared
         * with total number of credits required
         *
         * @param programStudiesOid String
         * @param creditsGainedMap Key is a Requirement oid, value is the amount credits gained for
         *        the requirement
         * @param creditsWaivedMap Key is a Requirement oid, value is the amount credits waived for
         *        the requirement
         * @param creditsRequiredMap Key is a Requirement oid, value is the amount credits required
         *        for the requirement
         * @param satisfiedOtherRequirementOids List of satisfied other requirement oids.
         * @return int
         */
        public int getProgramSatisfiedStatus(String programStudiesOid,
                                             HashMap<String, Double> creditsGainedMap,
                                             HashMap<String, Double> creditsWaivedMap,
                                             HashMap<String, Double> creditsRequiredMap,
                                             List<String> satisfiedOtherRequirementOids) {
            double percent = 0;

            GraduationProgram program =
                    (GraduationProgram) m_broker.getBeanByOid(GraduationProgram.class, programStudiesOid);
            double creditsRequired = getTotalRequired(programStudiesOid, null, creditsRequiredMap);

            if (program != null && creditsRequired != 0) {
                Collection<GraduationRequirement> directRequirements = getDirectRequirements(programStudiesOid, null);

                int totalRequirements = directRequirements.size();

                /*
                 * Count total non-credit requirements.
                 */
                int totalNoCreditRequirement = 0;
                double totalCredits = 0;
                for (GraduationRequirement directRequirement : directRequirements) {
                    /*
                     * If this requirement is not included then don't count it.
                     */
                    if (directRequirement.getExcludeIndicator()) {
                        totalRequirements--;
                    } else {
                        if (directRequirement.getType() == GraduationRequirement.TYPE_OTHER ||
                                directRequirement.getRequiredUnit() == null ||
                                directRequirement.getRequiredUnit().doubleValue() == 0.0) {
                            totalNoCreditRequirement += 1;
                        } else {
                            totalCredits += directRequirement.getRequiredUnit().doubleValue();
                        }
                    }
                }

                double totalCreditPecent = (totalRequirements - totalNoCreditRequirement) * 100 / totalRequirements;

                /*
                 * Weigh each requirement based on their required credits.
                 */
                HashMap<String, Double> requirementWeight = new HashMap<String, Double>();
                for (GraduationRequirement directRequirement : directRequirements) {
                    if (!directRequirement.getExcludeIndicator() &&
                            directRequirement.getType() != GraduationRequirement.TYPE_OTHER &&
                            directRequirement.getRequiredUnit() != null &&
                            directRequirement.getRequiredUnit().doubleValue() > 0.0) {
                        double weight = directRequirement.getRequiredUnit() == null || totalCredits == 0 ? 0
                                : directRequirement.getRequiredUnit().doubleValue() * totalCreditPecent / totalCredits;

                        requirementWeight.put(directRequirement.getOid(), Double.valueOf(weight));
                    }
                }

                /*
                 * Total credits satisfy status
                 */
                double creditsGained = getTotalCreditsGained(programStudiesOid, null, creditsGainedMap);
                double creditsWaived = getTotalWaiver(programStudiesOid, null, creditsWaivedMap);

                if (creditsRequired == 0 || (creditsRequired <= creditsGained + creditsWaived)) {
                    percent += totalNoCreditRequirement == 0 ? 0
                            : (1 * totalNoCreditRequirement * 100 / totalRequirements) / totalNoCreditRequirement;
                }

                /*
                 * Go through each of the requirement and collect the satisfy status
                 */
                for (GraduationRequirement directRequirement : directRequirements) {
                    if (!directRequirement.getExcludeIndicator() &&
                            getRequirementSatisfiedStatus(directRequirement.getOid(), creditsGainedMap,
                                    creditsWaivedMap, creditsRequiredMap, satisfiedOtherRequirementOids) == 100) {
                        percent += requirementWeight.containsKey(directRequirement.getOid())
                                ? requirementWeight.get(directRequirement.getOid()).doubleValue()
                                : (totalNoCreditRequirement == 0 ? 0
                                        : (1 * totalNoCreditRequirement * 100 / totalRequirements)
                                                / totalNoCreditRequirement);
                    }
                }
            }

            return percent > 100 ? 100
                    : new BigDecimal(String.valueOf(percent)).setScale(0, RoundingMode.HALF_UP).intValue();
        }

        /**
         * Returns the overall satisfied percentage for the passed program study.
         *
         * @param programStudiesOid String
         * @param creditsInProgress double
         * @param creditsRequiredMap Key is a Requirement oid, value is the amount credits required
         *        for the requirement
         * @return int
         */
        public int getProgramInprogressStatus(String programStudiesOid,
                                              double creditsInProgress,
                                              HashMap<String, Double> creditsRequiredMap) {

            double creditsRequired = getTotalRequired(programStudiesOid, null, creditsRequiredMap);

            int percent = 0;
            if (creditsRequired == 0 || (creditsRequired <= creditsInProgress)) {
                percent = 100;
            } else {
                percent = (int) Math.round((creditsInProgress) * 100 / creditsRequired);
            }

            return percent;
        }

        /**
         * Returns the list of program studies the passed student has.
         *
         * @param student SisStudent
         * @return List<ProgramStudies>
         */
        public List<GraduationProgram> getProgramStudies(SisStudent student) {
            List<GraduationProgram> programStudies = new LinkedList<GraduationProgram>();

            if (student != null) {
                /*
                 * Collects the list of programs of studies for the student.
                 */
                X2Criteria studentProgramStudy = new X2Criteria();
                studentProgramStudy.addEqualTo(GraduationStudentProgram.COL_STUDENT_OID, student.getOid());

                QueryByCriteria studentProgramQuery =
                        new QueryByCriteria(GraduationStudentProgram.class, studentProgramStudy);
                studentProgramQuery.addOrderByDescending(GraduationStudentProgram.COL_PRIMARY_INDICATOR);
                studentProgramQuery.addOrderByAscending(
                        GraduationStudentProgram.REL_PROGRAM_STUDIES + "." + GraduationProgram.COL_NAME);

                QueryIterator programIterator = m_broker.getIteratorByQuery(studentProgramQuery);
                try {
                    while (programIterator.hasNext()) {
                        GraduationStudentProgram studentProgram = (GraduationStudentProgram) programIterator.next();
                        programStudies.add(studentProgram.getProgramStudies());
                    }
                } finally {
                    programIterator.close();
                }

                if (programStudies.isEmpty()) {
                    /*
                     * If the student does not have program studies of his/her own, retrive all the
                     * default
                     * program studies in the school.
                     */
                    X2Criteria programCriteria = new X2Criteria();
                    programCriteria.addEqualTo(GraduationProgram.COL_ORGANIZATION1_OID, student.getOrganization1Oid());
                    programCriteria.addEqualTo(GraduationProgram.COL_OBSOLETE_INDICATOR, Boolean.FALSE);
                    programCriteria.addEqualTo(GraduationProgram.COL_HIDE_INDICATOR, Boolean.FALSE);

                    QueryByCriteria programQuery = new QueryByCriteria(GraduationProgram.class, programCriteria);
                    programQuery.addOrderByAscending(GraduationProgram.COL_NAME);

                    programStudies.addAll(m_broker.getCollectionByQuery(programQuery));
                }
            }

            return programStudies;
        }

        /**
         * Returns the total number of units/credits in progress.
         *
         * @param courses List<SchoolCourse>
         * @param creditsInProgress Key is a SchoolCourse oid, value is the amount credits in
         *        progress for the requirement
         * @return double
         */
        public double getRequirementCreditsInProgress(List<SchoolCourse> courses,
                                                      HashMap<String, Double> creditsInProgress) {
            double credits = 0.0;

            /*
             * Accumulate the credits in progress.
             */
            if (!CollectionUtils.isEmpty(courses)) {
                for (SchoolCourse schoolCourse : courses) {
                    Double creditValue = creditsInProgress.get(schoolCourse.getOid());
                    if (creditValue != null) {
                        credits += creditValue.doubleValue();
                    }
                }
            }

            DataDictionaryField creditField =
                    DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey())
                            .findDataDictionaryField(Transcript.class.getName(), Transcript.COL_TOTAL_CREDIT);
            int decimals = creditField.getDatabaseDecimal();

            return new BigDecimal(String.valueOf(credits)).setScale(decimals, RoundingMode.HALF_UP).doubleValue();
        }

        /**
         * Returns the overall satisfied percentage for the passed requirement.
         *
         * @param requirementOid String
         * @param creditsInProgress double
         * @param creditsRequiredMap Key is a Requirement oid, value is the amount credits required
         *        for the requirement
         * @return int
         */
        public int getRequirementInprogressStatus(String requirementOid,
                                                  double creditsInProgress,
                                                  HashMap<String, Double> creditsRequiredMap) {

            double creditsRequired = getTotalRequired(null, requirementOid, creditsRequiredMap);

            int percent = 0;
            if (creditsRequired == 0 || (creditsRequired <= creditsInProgress)) {
                percent = 100;
            } else {
                percent = (int) Math.round((creditsInProgress) * 100 / creditsRequired);
            }

            return percent;
        }

        /**
         * Returns the overall satisfied percentage for the passed requirement.
         *
         * @param requirementOid String
         * @param creditsGainedMap Key is a Requirement oid, value is the amount credits gained for
         *        the requirement
         * @param creditsWaivedMap Key is a Requirement oid, value is the amount credits waived for
         *        the requirement
         * @param creditsRequiredMap Key is a Requirement oid, value is the amount credits required
         *        for the requirement
         * @param satisfiedOtherRequirementOids List of satisfied other requirements oids.
         * @return int
         */
        public int getRequirementSatisfiedStatus(String requirementOid,
                                                 HashMap<String, Double> creditsGainedMap,
                                                 HashMap<String, Double> creditsWaivedMap,
                                                 HashMap<String, Double> creditsRequiredMap,
                                                 List<String> satisfiedOtherRequirementOids) {

            double creditsRequired = getTotalRequired(null, requirementOid, creditsRequiredMap);
            double creditsGained = getTotalCreditsGained(null, requirementOid, creditsGainedMap);
            double creditsWaived = getTotalWaiver(null, requirementOid, creditsWaivedMap);
            boolean otherTypeSatisfied = true;

            if (requirementOid != null) {
                GraduationRequirement requirement =
                        (GraduationRequirement) m_broker.getBeanByOid(GraduationRequirement.class, requirementOid);
                if (requirement != null && requirement.getType() == GraduationRequirement.TYPE_OTHER) {
                    /*
                     * In case the requirement is an other requirement we need to check the list to
                     * see
                     * if it is satisfied or not.
                     */
                    otherTypeSatisfied = satisfiedOtherRequirementOids.contains(requirementOid);
                }
            }

            int percent = 0;
            if (creditsRequired == 0 || (creditsRequired <= creditsGained + creditsWaived)) {
                percent = otherTypeSatisfied ? 100 : 0;
            } else {
                percent = (int) Math.round((creditsGained + creditsWaived) * 100 / creditsRequired);
            }

            return percent;
        }

        /**
         * Returns the list of requirements that are part of the requirement group of the passed
         * requirement
         * and its program study.
         *
         * @param combinedOid String
         * @return LinkedList<Requirement>
         */
        public LinkedList<GraduationRequirement> getSubRequirements(String combinedOid) {
            int indexOfDelimiter = combinedOid.indexOf("|");
            String programStudiesOid =
                    indexOfDelimiter == -1 ? combinedOid : combinedOid.substring(0, indexOfDelimiter);
            String parentRequirmentOid = indexOfDelimiter == -1 ? "" : combinedOid.substring(indexOfDelimiter + 1);

            return getSubRequirements(programStudiesOid, parentRequirmentOid);
        }

        /**
         * Returns the relationship among the sub requirements of the passed requirement.
         *
         * @param combinedOid String
         * @return String
         */
        public String getSubRequirementsRelation(String combinedOid) {
            int indexOfDelimiter = combinedOid.indexOf("|");
            String parentRequirmentOid = indexOfDelimiter == -1 ? "" : combinedOid.substring(indexOfDelimiter + 1);

            GraduationRequirement parentRequirement =
                    (GraduationRequirement) m_broker.getBeanByOid(GraduationRequirement.class, parentRequirmentOid);

            String relation = "";
            if (parentRequirement != null) {
                HashMap<String, Collection<String>> subRequirementMap = new HashMap<String, Collection<String>>();
                getOtherRequirement(parentRequirement, subRequirementMap);

                if (!subRequirementMap.isEmpty()) {
                    relation = subRequirementMap.keySet().iterator().next();
                }
            }

            return relation;
        }

        /**
         * Returns the total number of units/credits gained for the passed program
         * study/requirement.
         *
         * @param programStudiesOid String
         * @param requirementOid String
         * @param creditsGained Key is a Requirement oid, value is the amount credits gained for the
         *        requirement
         * @return double
         */
        public double getTotalCreditsGained(String programStudiesOid,
                                            String requirementOid,
                                            HashMap<String, Double> creditsGained) {
            double credits = 0.0;

            if (!StringUtils.isEmpty(programStudiesOid)) {
                /*
                 * Accumulate the credits gained for all requirements that are part of the program
                 * study.
                 */
                for (String includeRequirementOid : creditsGained.keySet()) {
                    GraduationRequirement requirement = (GraduationRequirement) m_broker
                            .getBeanByOid(GraduationRequirement.class, includeRequirementOid);

                    /*
                     * Only includes direct requirement for the program studies
                     */
                    if (requirement != null &&
                            !requirement.getExcludeIndicator() &&
                            programStudiesOid.equals(requirement.getProgramStudiesOid()) &&
                            requirement.getParentRequirementOid() == null) {
                        credits += creditsGained.get(includeRequirementOid) == null ? 0.0
                                : creditsGained.get(includeRequirementOid).doubleValue();
                    }
                }
            } else {
                credits += creditsGained.get(requirementOid) == null ? 0.0
                        : creditsGained.get(requirementOid).doubleValue();
            }

            DataDictionaryField creditField =
                    DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey())
                            .findDataDictionaryField(Transcript.class.getName(), Transcript.COL_TOTAL_CREDIT);
            int decimals = creditField.getDatabaseDecimal();

            return new BigDecimal(String.valueOf(credits)).setScale(decimals, RoundingMode.HALF_UP).doubleValue();
        }

        /**
         * Returns the total number of units/credits in progress.
         *
         * @param creditsInProgress Key is a SchoolCourse oid, value is the amount credits in
         *        progress for the requirement
         *
         * @return double
         */
        public double getTotalCreditsInProgress(HashMap<String, Double> creditsInProgress) {
            double credits = 0.0;

            /*
             * Accumulate the credits in progress.
             */
            for (String courseOid : creditsInProgress.keySet()) {
                credits += creditsInProgress.get(courseOid).doubleValue();
            }

            DataDictionaryField creditField =
                    DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey())
                            .findDataDictionaryField(Transcript.class.getName(), Transcript.COL_TOTAL_CREDIT);
            int decimals = creditField.getDatabaseDecimal();

            return new BigDecimal(String.valueOf(credits)).setScale(decimals, RoundingMode.HALF_UP).doubleValue();
        }

        /**
         * Returns the total number of units/credits requested.
         *
         * @param creditsRequested Key is a SchoolCourse oid, value is the amount credits in
         *        progress for the requirement
         *
         * @return double
         */
        public double getTotalCreditsRequesting(HashMap<String, Double> creditsRequested) {
            double credits = 0.0;

            /*
             * Accumulate the credits in progress.
             */
            for (String courseOid : creditsRequested.keySet()) {
                credits += creditsRequested.get(courseOid).doubleValue();
            }

            DataDictionaryField creditField =
                    DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey())
                            .findDataDictionaryField(Transcript.class.getName(), Transcript.COL_TOTAL_CREDIT);
            int decimals = creditField.getDatabaseDecimal();

            return new BigDecimal(String.valueOf(credits)).setScale(decimals, RoundingMode.HALF_UP).doubleValue();
        }

        /**
         * Returns the total number of units/credits required for the passed program
         * study/requirement.
         *
         * @param programStudiesOid String
         * @param requirementOid String
         * @param creditsRequiredMap Key is a Requirement oid, value is the amount credits required
         *        for the requirement
         * @return double
         */
        public double getTotalRequired(String programStudiesOid,
                                       String requirementOid,
                                       HashMap<String, Double> creditsRequiredMap) {
            double required = 0.0;

            if (!StringUtils.isEmpty(programStudiesOid)) {
                GraduationProgram programStudy =
                        (GraduationProgram) m_broker.getBeanByOid(GraduationProgram.class, programStudiesOid);
                if (programStudy != null) {
                    required = programStudy.getTotalCredit().doubleValue();
                }
            } else {
                GraduationRequirement requirement =
                        (GraduationRequirement) m_broker.getBeanByOid(GraduationRequirement.class, requirementOid);
                if (requirement != null) {
                    required =
                            creditsRequiredMap.get(requirementOid) == null ? requirement.getRequiredUnit().doubleValue()
                                    : creditsRequiredMap.get(requirementOid).doubleValue();
                }
            }

            return new BigDecimal(String.valueOf(required)).setScale(4, RoundingMode.HALF_UP).doubleValue();
        }

        /**
         * Returns the total number of units/credits waived for the passed program
         * study/requirement.
         *
         * @param programStudiesOid String
         * @param requirementOid String
         * @param creditsWaived Key is a Requirement oid, value is the amount credits waived for the
         *        requirement
         * @return double
         */
        public double getTotalWaiver(String programStudiesOid,
                                     String requirementOid,
                                     HashMap<String, Double> creditsWaived) {
            double waiver = 0.0;

            if (!StringUtils.isEmpty(programStudiesOid)) {
                /*
                 * Accumulate the waivers for all requirements that are part of the program study.
                 */
                for (String includeRequirementOid : creditsWaived.keySet()) {
                    GraduationRequirement requirement = (GraduationRequirement) m_broker
                            .getBeanByOid(GraduationRequirement.class, includeRequirementOid);
                    if (requirement != null && programStudiesOid.equals(requirement.getProgramStudiesOid())) {
                        if (requirement.getParentRequirementOid() != null) {
                            GraduationRequirement parentRequirement = (GraduationRequirement) m_broker
                                    .getBeanByOid(GraduationRequirement.class, requirement.getParentRequirementOid());

                            if (parentRequirement != null
                                    && parentRequirement.getType() == GraduationRequirement.TYPE_REQUIRMENT_GROUP &&
                                    creditsWaived.get(parentRequirement.getOid()) != null
                                    && creditsWaived.get(parentRequirement.getOid()).doubleValue() > 0) {
                                // if this requirement has a parent who is a group and that group
                                // has waived credits then we can skip this requirement's waivers.
                                continue;
                            }
                        }

                        waiver += creditsWaived.get(includeRequirementOid) == null ? 0.0
                                : creditsWaived.get(includeRequirementOid).doubleValue();
                    }
                }
            } else {
                GraduationRequirement requirement =
                        (GraduationRequirement) m_broker.getBeanByOid(GraduationRequirement.class, requirementOid);
                if (requirement != null) {
                    waiver += creditsWaived.get(requirementOid) == null ? 0.0
                            : creditsWaived.get(requirementOid).doubleValue();
                }
            }

            return new BigDecimal(String.valueOf(waiver)).setScale(4, RoundingMode.HALF_UP).doubleValue();
        }

        /**
         * Returns true if the passed requirement is expandable.
         * A requirement is expandable if the requirement contains a sub program or the type of the
         * requirement
         * is a group requirement.
         *
         * @param requirementOid String
         * @return boolean
         */
        public boolean isExpandable(String requirementOid) {
            boolean isExpandable = false;

            GraduationRequirement requirement =
                    (GraduationRequirement) m_broker.getBeanByOid(GraduationRequirement.class, requirementOid);

            if (requirement != null) {
                isExpandable = requirement.getSubProgramStudies() != null ||
                        requirement.getType() == GraduationRequirement.TYPE_REQUIRMENT_GROUP;
            }

            return isExpandable;
        }

        /**
         * Comparator based on the sorting of transcript records for BC. BC sorts transcripts
         * conditionally based on
         * 2 possible UDF date fields - TRN_FIELDA_025 or TRN_FIELDA_026. The check never compares
         * values as equal to
         * ensure that all values in the source map are present in the sorted map.
         *
         * @author Follett Software Company
         */
        private class BcTranscriptListComparator implements Comparator<Transcript> {

            /**
             * Instantiates a new bc transcript list comparator.
             */
            public BcTranscriptListComparator() {
                // Does nothing
            }

            /**
             * Sort:
             * School year | Ascending
             * Completion date | Ascending
             * Total credit | Descending
             * Course number | Ascending.
             *
             * @param transcript1 Transcript
             * @param transcript2 Transcript
             * @return int
             * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
             */
            @Override
            public int compare(Transcript transcript1, Transcript transcript2) {
                int comparison = -1;

                // Check year
                int year1 = transcript1.getDistrictContext().getSchoolYear();
                int year2 = transcript2.getDistrictContext().getSchoolYear();

                comparison = year1 == year2 ? 0 : year1 > year2 ? 1 : -1;

                if (comparison == 0) {
                    // Check completion date
                    String date1 = getEndDate(transcript1);
                    String date2 = getEndDate(transcript2);

                    comparison = compareSafe(date1, date2);

                    if (comparison == 0) {
                        // Check total credit
                        BigDecimal credit1 = transcript1.getTotalCredit();
                        BigDecimal credit2 = transcript2.getTotalCredit();

                        comparison = compareSafe(credit2, credit1);

                        if (comparison == 0) {
                            // Check course number
                            String number1 = transcript1.getSchoolCourse().getNumber();
                            String number2 = transcript2.getSchoolCourse().getNumber();

                            comparison = number1.compareTo(number2);
                        }
                    }
                }

                // Do not return 0 to ensure all records are included in the set
                return comparison == 0 ? -1 : comparison;
            }

            /**
             * Checks for null values in the comparison.
             *
             * @param object1 Comparable
             * @param object2 Comparable
             * @return int -1,0,1
             */
            private int compareSafe(Comparable object1, Comparable object2) {
                int comparison = -1;

                if (object1 == null && object2 == null) {
                    comparison = 0;
                } else {
                    if (object1 != null && object2 == null) {
                        comparison = 1;
                    } else if (object1 == null && object2 != null) {
                        comparison = -1;
                    } else {
                        comparison = object1.compareTo(object2);
                    }
                }

                return comparison;
            }

            /**
             * Returns the end date of the transcript record - either the completion date or end
             * date.
             *
             * @param transcript Transcript
             * @return String
             */
            private String getEndDate(Transcript transcript) {
                String completionDate = transcript.getFieldA025();
                String endDate = transcript.getFieldA026();

                return StringUtils.coalesce(completionDate, endDate);
            }
        }

        /**
         * Simple comparator based on the value of a map of credits. The check never compares values
         * as equal to
         * ensure that all values in the source map are present in the sorted map.
         *
         * @author Follett Software Company
         * @param <E> the element type
         */
        private class CreditMapValueComparator<E> implements Comparator<E> {
            Map<E, E> m_map = null;

            /**
             * Instantiates a new credit map value comparator.
             *
             * @param map Map
             */
            CreditMapValueComparator(Map map) {
                m_map = map;
            }

            /**
             * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
             */
            @Override
            public int compare(E arg0, E arg1) {
                Comparable value0 = (Comparable) m_map.get(arg0);
                Comparable value1 = (Comparable) m_map.get(arg1);

                return value1.compareTo(value0) > 0 ? 1 : -1;
            }
        }

        /**
         * Adds the course to the list of taken courses for the graduation requirement.
         *
         * @param coursesTaken HashMap<String,List<SchoolCourse>>
         * @param schoolCourse SchoolCourse
         * @param requirementOid String
         */
        private void addTakenCourse(HashMap<String, List<SchoolCourse>> coursesTaken,
                                    SchoolCourse schoolCourse,
                                    String requirementOid) {
            List<SchoolCourse> courseList = coursesTaken.get(requirementOid);
            if (courseList == null) {
                courseList = new ArrayList<SchoolCourse>();
            }

            if (!courseList.contains(schoolCourse)) {
                courseList.add(schoolCourse);
            }

            coursesTaken.put(requirementOid, courseList);
        }

        /**
         * Assigns the credit information for the course towards the requirement.
         *
         * @param creditsGained HashMap<String,Double>
         * @param rawCreditsGained HashMap<String,Double>
         * @param creditsByCourse HashMap<String,Double>
         * @param partialCredits Map<String,Map<String,Double>>
         * @param decimals int
         * @param transcriptCredit Map<String,Double>
         * @param transcript Transcript
         * @param schoolCourse SchoolCourse
         * @param requirement GraduationRequirement
         * @param requiredUnits BigDecimal
         * @param currentCredits Double
         */
        private void assignCredits(HashMap<String, Double> creditsGained,
                                   HashMap<String, Double> rawCreditsGained,
                                   HashMap<String, Double> creditsByCourse,
                                   Map<String, Map<String, Double>> partialCredits,
                                   int decimals,
                                   Map<String, Double> transcriptCredit,
                                   Transcript transcript,
                                   SchoolCourse schoolCourse,
                                   GraduationRequirement requirement,
                                   BigDecimal requiredUnits,
                                   Double currentCredits) {
            double possibleCredits = getPossibleCredits(requirement.getOid(), schoolCourse.getOid(),
                    transcriptCredit.get(transcript.getOid()), partialCredits);
            double creditTotal = currentCredits.doubleValue() + possibleCredits;

            rawCreditsGained.put(requirement.getOid(), Double.valueOf(new BigDecimal(String.valueOf(creditTotal))
                    .setScale(decimals, RoundingMode.HALF_UP).doubleValue()));
            if (creditTotal > requiredUnits.doubleValue()) {
                creditTotal = requiredUnits.doubleValue();
            }
            creditsGained.put(requirement.getOid(), Double.valueOf(creditTotal));

            /*
             * Store the course | requirement value of credits as well as
             * the course | requirement | transcript value of credits assigned.
             */
            String combinedOid = schoolCourse.getOid() + "|" + requirement.getOid();
            String fullOid = combinedOid + "|" + transcript.getOid();
            double existingRequirementCredits = 0.0;

            if (creditsByCourse.containsKey(combinedOid)) {
                existingRequirementCredits = creditsByCourse.get(combinedOid).doubleValue();
            }

            creditsByCourse.put(fullOid, Double.valueOf(possibleCredits));
            creditsByCourse.put(combinedOid, Double.valueOf(possibleCredits + existingRequirementCredits));
        }

        /**
         * Determine the best allocation of transcripts across the credit graduation
         * requirements for a particular program of study.
         * <p>
         * Changes for BC:
         * <ul>
         * <li>Only uses direct course on transcript - equivalent course is ignored
         * <li>For repeated courses (based on district course code) only use the record with the
         * higher grade
         * </ul>
         *
         * @param programStudiesOid String
         * @param studentOid String
         * @param coursesGainedCredits HashMap<String,List<SchoolCourse>>
         * @param coursesTaken HashMap<String,List<SchoolCourse>>
         * @param coursesToTranscripts HashMap<String,List<String>>
         * @param creditsGained HashMap<String,Double>
         * @param rawCreditsGained HashMap<String,Double>
         * @param creditsWaived HashMap<String,Double>
         * @param creditsByCourse HashMap<String,Double>
         * @param gradeLevelByCourse HashMap<String,String>
         * @param requirementsProcessed Set<GraduationRequirement>
         * @param partialCourseRequirments Map<String,List<GraduationCourseRequirement>>
         * @param equivalentCourseDesc Map<String,Map<String,String>>
         * @param customColumnHeaders List<String>
         * @param customColumnValues Map<String,Map<String,String>>
         */
        private void determineBestAssignments(String programStudiesOid,
                                              String studentOid,
                                              HashMap<String, List<SchoolCourse>> coursesGainedCredits,
                                              HashMap<String, List<SchoolCourse>> coursesTaken,
                                              HashMap<String, List<String>> coursesToTranscripts,
                                              HashMap<String, Double> creditsGained,
                                              HashMap<String, Double> rawCreditsGained,
                                              HashMap<String, Double> creditsWaived,
                                              HashMap<String, Double> creditsByCourse,
                                              HashMap<String, String> gradeLevelByCourse,
                                              Set<GraduationRequirement> requirementsProcessed,
                                              Map<String, List<GraduationCourseRequirement>> partialCourseRequirments,
                                              Map<String, Map<String, String>> equivalentCourseDesc,
                                              List<String> customColumnHeaders,
                                              Map<String, Map<String, String>> customColumnValues) {
            Map<String, Map<String, Double>> partialCredits = new HashMap<String, Map<String, Double>>();

            // Lookup elective requirement
            Criteria electiveRequirementCriteria = new Criteria();
            electiveRequirementCriteria.addEqualTo(GraduationRequirement.COL_FIELD_A001, ELECTIVE_REQUIREMENT_CATEGORY);
            electiveRequirementCriteria.addEqualTo(GraduationRequirement.COL_PROGRAM_STUDIES_OID, programStudiesOid);

            QueryByCriteria electiveRequirementQuery =
                    new QueryByCriteria(GraduationRequirement.class, electiveRequirementCriteria);
            GraduationRequirement electiveRequirement =
                    (GraduationRequirement) m_broker.getBeanByQuery(electiveRequirementQuery);

            // Load transcripts
            Collection<Transcript> transcriptsToInclude = filterTranscripts(studentOid, customColumnValues);

            // Set decimals
            DataDictionaryField creditField =
                    DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey())
                            .findDataDictionaryField(Transcript.class.getName(), Transcript.COL_TOTAL_CREDIT);
            int decimals = creditField.getDatabaseDecimal();

            // Map of transcript oid to total credit.
            Map<String, Double> transcriptCredit = new HashMap<String, Double>();
            // Map of transcript oid to school course object.
            Map<String, SchoolCourse> transcriptSchoolCourse = new LinkedHashMap<String, SchoolCourse>();
            // Set of course oids this student has taken.
            Set<String> courseOids = new HashSet<String>();
            // Map of transcripts with equivalent course descriptions
            Map<String, String> transcriptEquivalent = new HashMap<String, String>();

            Collection<Transcript> toRemove = new LinkedList<Transcript>();

            /*
             * Iterate through the transcripts building up the maps.
             */
            for (Transcript transcript : transcriptsToInclude) {
                String schoolCourseOid = transcript.getSchoolCourseOid();

                String status = checkStatus(transcript, lookupExamStatus(transcript.getSchoolCourse()),
                        (String) transcript.getFieldValueByAlias(TRN_TRAX_ALIAS));
                BigDecimal credit = (STATUS_PROGRESS.equals(status) && transcript.getSchoolCourse().getCredit() != null)
                        ? transcript.getSchoolCourse().getCredit()
                        : transcript.getTotalCredit();
                if (!transcript.getExcludeCreditIndicator() && credit != null) {
                    String transcriptOid = transcript.getOid();
                    String gradeLevel = transcript.getGradeLevel();
                    SchoolCourse schoolCourse = transcript.getSchoolCourse();
                    boolean userDescriptionInd = transcript.getUserDescriptionIndicator();
                    String userDescription = transcript.getCourseDescription();
                    double existingCredits = 0;

                    List<String> transcriptsForCourse = coursesToTranscripts.get(schoolCourseOid);
                    if (transcriptsForCourse == null) {
                        transcriptsForCourse = new ArrayList<String>();
                    }

                    if (creditsByCourse.containsKey(schoolCourseOid)) {
                        existingCredits = creditsByCourse.get(schoolCourseOid).doubleValue();
                    }

                    if (customColumnValues != null) {
                        Map<String, String> gradeValues = new HashMap<>();
                        m_graduationSummaryProcedure.addColumnHeadersFromTranscript(transcript, customColumnHeaders,
                                gradeValues);

                        if (!gradeValues.isEmpty()) {
                            if (customColumnValues.get(schoolCourseOid) != null) {
                                // If a schoolCourse transcript record has already been put in and
                                // it has counted for more than 0 credits -
                                // use that one instead.
                                if (existingCredits == 0 && credit.doubleValue() > 0) {
                                    customColumnValues.put(schoolCourseOid, gradeValues);
                                }
                            } else {
                                customColumnValues.put(schoolCourseOid, gradeValues);
                            }
                        }
                    }

                    if (!transcriptsForCourse.contains(transcriptOid)) {
                        creditsByCourse.put(schoolCourseOid, Double.valueOf(credit.doubleValue() + existingCredits));
                        transcriptsForCourse.add(transcriptOid);
                    }

                    gradeLevelByCourse.put(schoolCourseOid, gradeLevel);
                    transcriptCredit.put(transcriptOid, Double.valueOf(credit.doubleValue()));

                    if (schoolCourse != null) {
                        /*
                         * If the transcript has a user defined description, copy the bean and
                         * update the schoolCourse description with the user defined description
                         */
                        if (userDescriptionInd && !StringUtils.isEmpty(userDescription)) {
                            /*
                             * get a clone of the bean so we can change the description without
                             * modifying
                             * the cached version of the bean.
                             */
                            schoolCourse = schoolCourse.clone();
                            transcriptEquivalent.put(transcriptOid, schoolCourse.getDescription());
                            schoolCourse.setDescription(userDescription);
                        }

                        transcriptSchoolCourse.put(transcriptOid, schoolCourse);
                        // Needs to be root course oids as requirements only tired with root course
                        courseOids.add(schoolCourse.getCourse().getRootCourse().getOid());
                    }
                    coursesToTranscripts.put(schoolCourseOid, transcriptsForCourse);
                } else {
                    if (customColumnValues != null) {
                        Map<String, String> gradeValues = new HashMap<>();
                        m_graduationSummaryProcedure.addColumnHeadersFromTranscript(transcript, customColumnHeaders,
                                gradeValues);
                        customColumnValues.put(schoolCourseOid, gradeValues);
                    }

                    toRemove.add(transcript);
                }
            }

            /*
             * Get the collection of graduation course requirements grouped by course OID.
             */
            Map<String, List<GraduationCourseRequirement>> courseToRequirements =
                    new HashMap<String, List<GraduationCourseRequirement>>();
            Collection<String> electiveCourseOids = new LinkedList<String>();

            if (!courseOids.isEmpty()) {
                // Handle invalid programs of study without an electives requirement
                String electiveOid = electiveRequirement != null ? electiveRequirement.getOid() : "noMatchValue";

                X2Criteria courseCriteria = new X2Criteria();
                courseCriteria.addIn(GraduationCourseRequirement.COL_COURSE_OID, courseOids);
                courseCriteria.addEqualTo(GraduationCourseRequirement.REL_REQUIREMENT + PATH_DELIMITER
                        + GraduationRequirement.COL_PROGRAM_STUDIES_OID, programStudiesOid);

                // Load non-elective lookup
                X2Criteria standardCriteria = courseCriteria.copy();
                standardCriteria.addNotEqualTo(GraduationCourseRequirement.COL_REQUIREMENT_OID, electiveOid);

                Query standardQuery = new QueryByCriteria(GraduationCourseRequirement.class, standardCriteria);
                courseToRequirements = m_broker.getGroupedCollectionByQuery(standardQuery,
                        GraduationCourseRequirement.COL_COURSE_OID, 100);

                // Load elective courses
                X2Criteria electiveCriteria = courseCriteria.copy();
                electiveCriteria.addEqualTo(GraduationCourseRequirement.COL_REQUIREMENT_OID, electiveOid);

                SubQuery electiveQuery = new SubQuery(GraduationCourseRequirement.class,
                        GraduationCourseRequirement.COL_COURSE_OID, electiveCriteria);
                electiveCourseOids = m_broker.getSubQueryCollectionByQuery(electiveQuery);
            }


            if (DEBUG_MODE) {
                if (!CollectionUtils.isEmpty(toRemove)) {
                    String transcriptsToRemove = "";
                    for (Transcript transcript : toRemove) {
                        transcriptsToRemove += "Transcript: " + transcript.getSchoolCourse().getNumber() + " context: "
                                + transcript.getDistrictContext().getContextId() + "/r/n";
                    }
                    updateSystemLog("Transcripts to remove: " + transcriptsToRemove);
                } else {
                    updateSystemLog("No transcripts to remove");
                }
            }
            /*
             * Remove invalid transcript records
             */
            transcriptsToInclude.removeAll(toRemove);

            /*
             * Iterate over transcripts and add to standard requirement if not already met,
             * otherwise add to elective
             */
            for (Transcript transcript : transcriptsToInclude) {
                SchoolCourse schoolCourse = transcript.getSchoolCourse();
                String courseOid = schoolCourse.getCourse().getRootCourse().getOid();

                boolean appliedToStandard = false;

                List<GraduationCourseRequirement> courseRequirements = courseToRequirements.get(courseOid);
                if (courseRequirements != null && !courseRequirements.isEmpty()) {
                    /*
                     * Iterate over standard requirements
                     */
                    for (GraduationCourseRequirement courseRequirement : courseRequirements) {
                        GraduationRequirement requirement = courseRequirement.getRequirement();
                        BigDecimal requiredUnits = requirement.getRequiredUnit();
                        Double currentCredits = getMapCredits(rawCreditsGained, requirement.getOid());

                        // Include if the requirement is not yet met
                        if (currentCredits.doubleValue() < requiredUnits.doubleValue()) {
                            addTakenCourse(coursesTaken, schoolCourse, courseRequirement.getRequirementOid());

                            assignCredits(creditsGained,
                                    rawCreditsGained,
                                    creditsByCourse,
                                    partialCredits,
                                    decimals,
                                    transcriptCredit,
                                    transcript,
                                    schoolCourse,
                                    requirement,
                                    requiredUnits,
                                    currentCredits);

                            trackIncludedCourses(coursesGainedCredits, schoolCourse, requirement);

                            appliedToStandard = true;
                            requirementsProcessed.add(requirement);

                            break;
                        }
                    }
                }

                /*
                 * Apply to elective
                 */
                if (electiveCourseOids.contains(courseOid)) {
                    addTakenCourse(coursesTaken, schoolCourse, electiveRequirement.getOid());

                    if (!appliedToStandard) {
                        Double currentCredits = getMapCredits(rawCreditsGained, electiveRequirement.getOid());

                        assignCredits(creditsGained, rawCreditsGained, creditsByCourse, partialCredits, decimals,
                                transcriptCredit, transcript, schoolCourse, electiveRequirement,
                                electiveRequirement.getRequiredUnit(), currentCredits);

                        trackIncludedCourses(coursesGainedCredits, schoolCourse, electiveRequirement);

                        requirementsProcessed.add(electiveRequirement);
                    }
                }
            }
        }

        /**
         * Applies BC's repeat rules to remove duplicate transcript records.
         *
         * @param studentOid String
         * @param customColumnValues Map<String,Map<String,String>>
         * @return Collection
         */
        private Collection<Transcript> filterTranscripts(String studentOid,
                                                         Map<String, Map<String, String>> customColumnValues) {
            /*
             * Build transcript criteria/query
             */
            X2Criteria transcriptCriteria = new X2Criteria();
            transcriptCriteria.addEqualTo(Transcript.COL_STUDENT_OID, studentOid);
            transcriptCriteria.addNotEmpty(Transcript.COL_SCHOOL_COURSE_OID, m_broker.getPersistenceKey());

            if (customColumnValues == null) {
                transcriptCriteria.addEqualTo(Transcript.COL_EXCLUDE_CREDIT_INDICATOR, Boolean.FALSE);
                // transcriptCriteria.addGreaterThan(Transcript.COL_TOTAL_CREDIT, new
                // BigDecimal(0.0));
            }

            BeanQuery transcriptQuery = new BeanQuery(Transcript.class, transcriptCriteria);
            transcriptQuery.addOrderByAscending(
                    Transcript.REL_DISTRICT_CONTEXT + PATH_DELIMITER + DistrictSchoolYearContext.COL_SCHOOL_YEAR);
            transcriptQuery.addOrderByDescending(Transcript.COL_TOTAL_CREDIT);
            transcriptQuery
                    .addOrderByAscending(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER);

            List<Transcript> filteredTranscripts = new ArrayList(m_broker.getCount(transcriptQuery));
            Map<String, Transcript> transcriptsByCourseCode = new HashMap<String, Transcript>(128);

            /*
             * Iterate over transcripts and remove duplicates by course code
             */
            List<Transcript> transcripts = new ArrayList<Transcript>(m_broker.getCollectionByQuery(transcriptQuery));
            Collections.sort(transcripts, new BcRepeatTranscriptComparator());

            for (Transcript transcript : transcripts) {
                SchoolCourse schoolCourse = transcript.getSchoolCourse();

                // Track course numbers for grade comparison
                String courseCode = getCourseCode(schoolCourse);
                String courseKey = StringUtils.coalesce(courseCode, schoolCourse.getNumber());

                boolean valid = validateMarks(transcript);

                if (valid) {
                    // Compare current against existing record
                    Transcript previousTranscript = transcriptsByCourseCode.get(courseKey);
                    if (previousTranscript != null) {
                        int comparison = compareTranscriptGrades(transcript, previousTranscript);
                        if (comparison > 0) {
                            // New transcript has a higher grade - need to remove the previous one
                            filteredTranscripts.remove(previousTranscript);
                        } else {
                            valid = false;
                        }
                    }
                }

                if (valid) {
                    filteredTranscripts.add(transcript);
                    transcriptsByCourseCode.put(courseKey, transcript);
                }

                // Determine if this transcript is considered in progress. If so, store it to add to
                // coursesTaking later.
                if (!valid) {
                    boolean examinable = lookupExamStatus(schoolCourse);
                    String trax = (String) transcript.getFieldValueByAlias(TRN_TRAX_ALIAS);
                    String status = checkStatus(transcript, examinable, trax);

                    if (STATUS_PROGRESS.equals(status)) {
                        String rootCourseOid = transcript.getSchoolCourse().getCourse().getRootCourse().getOid();

                        Collection<SchoolCourse> schoolCourses =
                                m_coursesAlreadyTakenButConsideredInProgress.get(rootCourseOid);
                        if (schoolCourses == null) {
                            schoolCourses = new ArrayList<SchoolCourse>();
                            m_coursesAlreadyTakenButConsideredInProgress.put(rootCourseOid, schoolCourses);
                        }
                        schoolCourses.add(transcript.getSchoolCourse());
                    }
                }

                if (DEBUG_MODE) {
                    boolean examinable = lookupExamStatus(schoolCourse);
                    String trax = (String) transcript.getFieldValueByAlias(TRN_TRAX_ALIAS);
                    String status = checkStatus(transcript, examinable, trax);
                    updateSystemLog("filterTranscripts: transcript: " + transcript.getSchoolCourse().getNumber()
                            + " status: " + status + " context: " + transcript.getDistrictContext().getContextId()
                            + " valid: " + valid);
                }
            }

            Collections.sort(filteredTranscripts, new BcTranscriptListComparator());

            return filteredTranscripts;
        }

        /**
         * Returns the list of courses the student is current taking or requesting towards to the
         * graduation requirement.
         *
         * @param studentOid String
         * @param districtContextYearOid String
         * @param programStudiesOid String
         * @param coursesTaking key is a Requirement oid,
         *        value is the list of courses the student is taking towards to
         * @param creditsInProgress key is a Requirement oid,
         *        value is the amount credit the student can get for the course
         * @param fromRequest true if retrieve the courses from request
         * @return void
         */
        private void getCoursesTaking(String studentOid,
                                      String districtContextYearOid,
                                      String programStudiesOid,
                                      HashMap<String, List<SchoolCourse>> coursesTaking,
                                      HashMap<String, Double> creditsInProgress,
                                      boolean fromRequest) {
            Collection<String> programOids = new HashSet<String>();
            programOids.add(programStudiesOid);

            /*
             * Include all sub-programs for the student.
             */
            X2Criteria subProgramCriteria = new X2Criteria();
            subProgramCriteria.addEqualTo(GraduationRequirement.COL_PROGRAM_STUDIES_OID, programStudiesOid);
            subProgramCriteria.addEqualTo(GraduationRequirement.COL_TYPE,
                    Integer.valueOf(GraduationRequirement.TYPE_PROGRAM));
            subProgramCriteria.addNotEmpty(GraduationRequirement.COL_SUB_PROGRAM_STUDIES_OID,
                    m_broker.getPersistenceKey());

            SubQuery subProgramQuery = new SubQuery(GraduationRequirement.class,
                    GraduationRequirement.COL_SUB_PROGRAM_STUDIES_OID, subProgramCriteria);
            programOids.addAll(m_broker.getSubQueryCollectionByQuery(subProgramQuery));

            /*
             * The criteria for all the requirements of the student has
             */
            X2Criteria requirementCriteria = new X2Criteria();
            requirementCriteria.addIn(GraduationRequirement.COL_PROGRAM_STUDIES_OID, programOids);
            requirementCriteria.addEqualTo(GraduationRequirement.COL_TYPE,
                    Integer.valueOf(GraduationRequirement.TYPE_CREDIT));

            /*
             * Retrieve the courses list by requirement map.
             */
            X2Criteria courseRequirementCriteria = new X2Criteria();
            courseRequirementCriteria.addAndCriteria(requirementCriteria.copyWithAdjustedPath(
                    GraduationCourseRequirement.REL_REQUIREMENT, GraduationCourseRequirement.COL_REQUIREMENT_OID));

            // Also include any course oids for courses the student has taken in prior years, but is
            // still considered in progress.
            if (!MapUtils.isEmpty(m_coursesAlreadyTakenButConsideredInProgress)) {
                X2Criteria currentYearCourseRequirementCriteria = new X2Criteria();
                currentYearCourseRequirementCriteria.addEqualTo(
                        GraduationCourseRequirement.REL_COURSE + "." + Course.COL_DISTRICT_CONTEXT_OID,
                        districtContextYearOid);

                X2Criteria previouslyTakenCourseRequirementCriteria = new X2Criteria();
                previouslyTakenCourseRequirementCriteria.addIn(GraduationCourseRequirement.COL_COURSE_OID,
                        m_coursesAlreadyTakenButConsideredInProgress.keySet());

                // OR the two criteria together so current year and prior year already taken courses
                // are queried.
                currentYearCourseRequirementCriteria.addOrCriteria(previouslyTakenCourseRequirementCriteria);

                // AND the original criteria to the newly created and combined criteria.
                courseRequirementCriteria.addAndCriteria(currentYearCourseRequirementCriteria);
            } else {
                // If the student has no courses already taken but still in progress, simplify by
                // adding the district context restriction to the original criteria.
                courseRequirementCriteria.addEqualTo(
                        GraduationCourseRequirement.REL_COURSE + "." + Course.COL_DISTRICT_CONTEXT_OID,
                        districtContextYearOid);
            }

            String[] courseRequirementColms = new String[] {GraduationCourseRequirement.COL_REQUIREMENT_OID,
                    GraduationCourseRequirement.COL_COURSE_OID};
            ReportQueryByCriteria requestQuery = new ReportQueryByCriteria(GraduationCourseRequirement.class,
                    courseRequirementColms, courseRequirementCriteria);
            Map<String, Collection<String>> coursesByRequirement =
                    m_broker.getGroupedColumnCollectionByQuery(requestQuery, 1000);

            /*
             * Retrieve the school courses list by course map.
             */
            X2Criteria schoolCourseCriteria = new X2Criteria();
            schoolCourseCriteria.addEqualTo(SchoolCourse.REL_COURSE + "." + Course.COL_DISTRICT_CONTEXT_OID,
                    districtContextYearOid);

            Map<String, Collection<String>> repeatByCourse = new HashMap<String, Collection<String>>();
            if (fromRequest) {
                X2Criteria requestCriteria = new X2Criteria();
                requestCriteria.addEqualTo(CourseRequest.COL_STUDENT_OID, studentOid);
                requestCriteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID, districtContextYearOid);
                requestCriteria.addEqualTo(CourseRequest.COL_ALTERNATE_INDICATOR, Boolean.FALSE);
                schoolCourseCriteria.addAndCriteria(requestCriteria
                        .copyWithAdjustedPath(CourseRequest.REL_SCHOOL_COURSE, CourseRequest.COL_SCHOOL_COURSE_OID));

                String[] courseRequestColms = new String[] {CourseRequest.COL_SCHOOL_COURSE_OID, X2BaseBean.COL_OID};
                ReportQueryByCriteria courseRequestQuery =
                        new ReportQueryByCriteria(CourseRequest.class, courseRequestColms, requestCriteria);
                repeatByCourse = m_broker.getGroupedColumnCollectionByQuery(courseRequestQuery, 20);
            } else {
                SisStudent student = (SisStudent) m_broker.getBeanByOid(SisStudent.class, studentOid);

                Collection<String> schools = new ArrayList<String>();
                if (student != null) {
                    schools.add(student.getSchoolOid());
                }

                X2Criteria secondarySchoolCriteria = new X2Criteria();
                secondarySchoolCriteria.addEqualTo(StudentSchool.COL_STUDENT_OID, studentOid);
                secondarySchoolCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, districtContextYearOid);
                secondarySchoolCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));

                SubQuery secondarySchoolQuery =
                        new SubQuery(StudentSchool.class, StudentSchool.COL_SCHOOL_OID, secondarySchoolCriteria);
                schools.addAll(m_broker.getSubQueryCollectionByQuery(secondarySchoolQuery));

                if (DEBUG_MODE) {
                    if (!CollectionUtils.isEmpty(schools)) {
                        String schoolOids = "";
                        for (String schoolOid : schools) {
                            schoolOids += schoolOid + " ";
                        }
                        updateSystemLog("School oids - " + schoolOids);
                    } else {
                        updateSystemLog("No school oids");
                    }
                }

                X2Criteria activeScheduleCriteria = new X2Criteria();
                activeScheduleCriteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                        districtContextYearOid);
                activeScheduleCriteria.addNotEmpty(SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        m_broker.getPersistenceKey());
                if (!schools.isEmpty()) {
                    if (schools.size() > 1) {
                        activeScheduleCriteria.addIn(SchoolScheduleContext.COL_SCHOOL_OID, schools);
                    } else {
                        activeScheduleCriteria.addEqualTo(SchoolScheduleContext.COL_SCHOOL_OID,
                                schools.iterator().next());
                    }
                }

                SubQuery activeScheduleOidQuery = new SubQuery(SchoolScheduleContext.class,
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, activeScheduleCriteria);

                if (DEBUG_MODE) {
                    Collection<String> activeScheduleOids =
                            m_broker.getSubQueryCollectionByQuery(activeScheduleOidQuery);
                    if (!CollectionUtils.isEmpty(activeScheduleOids)) {
                        String activeScheduleOidConcat = "";
                        for (String activeScheduleOid : activeScheduleOids) {
                            activeScheduleOidConcat += activeScheduleOid + " ";
                        }
                        updateSystemLog("Active Schedule OIDs - " + activeScheduleOidConcat);
                    } else {
                        updateSystemLog("No Active Schedule OIDs");
                    }
                }

                /*
                 * Current year's transcript with credits
                 */
                X2Criteria transcriptCriteria = new X2Criteria();
                transcriptCriteria.addEqualTo(Transcript.COL_STUDENT_OID, studentOid);
                transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, districtContextYearOid);
                transcriptCriteria.addEqualTo(Transcript.COL_EXCLUDE_CREDIT_INDICATOR, Boolean.FALSE);
                transcriptCriteria.addNotEmpty(Transcript.COL_MASTER_SCHEDULE_OID, m_broker.getPersistenceKey());
                transcriptCriteria.addGreaterThan(Transcript.COL_TOTAL_CREDIT, Double.valueOf(0.0));

                SubQuery transcriptMasterQuery =
                        new SubQuery(Transcript.class, Transcript.COL_MASTER_SCHEDULE_OID, transcriptCriteria);
                Collection<String> sectionOids = m_broker.getSubQueryCollectionByQuery(transcriptMasterQuery);

                if (DEBUG_MODE) {
                    if (!CollectionUtils.isEmpty(sectionOids)) {
                        String sectionOidString = "";
                        for (String sectionOid : sectionOids) {
                            sectionOidString += " " + sectionOid;
                        }

                        updateSystemLog("Section OIDs for sections with transcripts with credits: " + sectionOidString);
                    }
                }

                /*
                 * Current year active terms
                 */
                X2Criteria studentScheduleTermDateCriteria = new X2Criteria();
                studentScheduleTermDateCriteria
                        .addIn(ScheduleTermDate.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                                ScheduleTerm.COL_SCHEDULE_OID, activeScheduleOidQuery);
                studentScheduleTermDateCriteria.addGreaterOrEqualThan(ScheduleTermDate.COL_END_DATE, new PlainDate());
                SubQuery activeTermsQuery = new SubQuery(ScheduleTermDate.class,
                        ScheduleTermDate.COL_SCHEDULE_TERM_OID,
                        studentScheduleTermDateCriteria);
                Collection<String> termOids = m_broker.getSubQueryCollectionByQuery(activeTermsQuery);

                X2Criteria scheduleCriteria = new X2Criteria();
                scheduleCriteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, studentOid);

                /*
                 * Student schedules happening in terms that are still active.
                 *
                 * AddIn is still used since it will only be current year terms and the list of
                 * terms should
                 * not be too large
                 */
                if (!CollectionUtils.isEmpty(termOids)) {
                    if (termOids.size() > 1) {
                        scheduleCriteria.addIn(
                                StudentSchedule.REL_SECTION + PATH_DELIMITER + Section.COL_SCHEDULE_TERM_OID, termOids);
                    } else {
                        scheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                                Section.COL_SCHEDULE_TERM_OID, termOids.iterator().next());
                    }

                    if (DEBUG_MODE) {
                        String terms = "";
                        for (String termOid : termOids) {
                            terms += " " + termOid;
                        }

                        updateSystemLog("Term OIDs: " + terms);
                    }
                } else {
                    /*
                     * If no terms are active then there should be no schedules
                     * and force it to a dummy term.
                     */
                    scheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION +
                            ModelProperty.PATH_DELIMITER +
                            Section.COL_SCHEDULE_TERM_OID, "___dummyOid___");

                    if (DEBUG_MODE) {
                        updateSystemLog("No Term OIDs exist.");
                    }
                }

                /*
                 * Student schedules excluding sections already assigned with credits.
                 */
                if (!CollectionUtils.isEmpty(sectionOids)) {
                    if (sectionOids.size() > 1) {
                        scheduleCriteria.addNotIn(StudentSchedule.COL_SECTION_OID, sectionOids);
                    } else {
                        scheduleCriteria.addNotEqualTo(StudentSchedule.COL_SECTION_OID, sectionOids.iterator().next());
                    }
                }

                SubQuery schedCourseOidQuery = new SubQuery(StudentSchedule.class,
                        StudentSchedule.REL_SECTION + "." + Section.COL_SCHOOL_COURSE_OID, scheduleCriteria);
                schoolCourseCriteria.addIn(X2BaseBean.COL_OID, schedCourseOidQuery);

                String[] studentSchedColms = new String[] {
                        StudentSchedule.REL_SECTION + "." + Section.COL_SCHOOL_COURSE_OID, X2BaseBean.COL_OID};
                ReportQueryByCriteria studentScheduleQuery =
                        new ReportQueryByCriteria(StudentSchedule.class, studentSchedColms, scheduleCriteria);
                repeatByCourse = m_broker.getGroupedColumnCollectionByQuery(studentScheduleQuery, 20);
            }

            QueryByCriteria schooolCourseQuery = new QueryByCriteria(SchoolCourse.class, schoolCourseCriteria);
            schooolCourseQuery.addOrderByAscending(Course.COL_NUMBER);
            Map<String, Collection<SchoolCourse>> schoolCourseByCourseMap =
                    m_broker.getGroupedCollectionByQuery(schooolCourseQuery, SchoolCourse.COL_COURSE_OID, 100);

            if (DEBUG_MODE) {
                if (!MapUtils.isEmpty(schoolCourseByCourseMap)) {
                    for (String courseOid : schoolCourseByCourseMap.keySet()) {
                        Collection<SchoolCourse> schoolCourses = schoolCourseByCourseMap.get(courseOid);
                        if (!CollectionUtils.isEmpty(schoolCourses)) {
                            for (SchoolCourse schoolCourse : schoolCourses) {
                                updateSystemLog("CourseOid: " + courseOid + " - SchoolCourse: "
                                        + schoolCourse.getNumber() + " - " + schoolCourse.getOid());
                            }
                        } else {
                            updateSystemLog("No school courses for course: " + courseOid);
                        }
                    }
                } else {
                    updateSystemLog("No school course by course map values");
                }
            }

            /*
             * If there is an intermediate catalog then we need to map CSK_CRS_OID to
             * CRS_CRS_OID_PARENT
             */
            String organizationOwnedCourses =
                    PreferenceManager.getPreferenceValue(OrganizationManager.getRootOrganization(m_broker),
                            SystemPreferenceDefinition.SYS_COURSE_INTERMEDIATEOWNER_ORGANIZATION);

            Map<String, Course> parentCourseMap = new HashMap<String, Course>();
            if (!StringUtils.isEmpty(organizationOwnedCourses)) {
                X2Criteria parentCourseCriteria = new X2Criteria();
                parentCourseCriteria.addEqualTo(Course.COL_DISTRICT_CONTEXT_OID, districtContextYearOid);
                parentCourseCriteria.addIn(X2BaseBean.COL_OID, schoolCourseByCourseMap.keySet());

                QueryByCriteria parentCourseQuery = new QueryByCriteria(Course.class, parentCourseCriteria);
                parentCourseQuery.addOrderByAscending(Course.COL_NUMBER);
                parentCourseMap = m_broker.getMapByQuery(parentCourseQuery, Course.COL_PARENT_COURSE_OID, 100);
            }

            for (String requirementOid : coursesByRequirement.keySet()) {
                Collection<String> courseOidsForRequirement = coursesByRequirement.get(requirementOid);
                LinkedHashSet<SchoolCourse> schoolCoursesForRequirement = new LinkedHashSet<SchoolCourse>();

                for (String coursOid : courseOidsForRequirement) {
                    Collection<SchoolCourse> schoolCourses =
                            !StringUtils.isEmpty(organizationOwnedCourses) && parentCourseMap.containsKey(coursOid)
                                    ? schoolCourseByCourseMap.get(parentCourseMap.get(coursOid).getOid())
                                    : schoolCourseByCourseMap.get(coursOid);

                    if (!CollectionUtils.isEmpty(schoolCourses)) {
                        schoolCourses = getUserDefinedCourseDescriptions(studentOid, schoolCourses);

                        schoolCoursesForRequirement.addAll(schoolCourses);

                        for (SchoolCourse schoolCourse : schoolCourses) {
                            int repeatFactor = repeatByCourse.containsKey(schoolCourse.getOid())
                                    ? repeatByCourse.get(schoolCourse.getOid()).size()
                                    : 1;
                            if (schoolCourse != null) {
                                creditsInProgress.put(schoolCourse.getOid(),
                                        schoolCourse.getCredit() == null ? Double.valueOf(0)
                                                : Double.valueOf(
                                                        schoolCourse.getCredit().doubleValue() * repeatFactor));
                            }
                        }
                    }

                    // Also add any other transcript school courses already taken but still
                    // considered in progress.
                    if (!MapUtils.isEmpty(m_coursesAlreadyTakenButConsideredInProgress)
                            && m_coursesAlreadyTakenButConsideredInProgress.get(coursOid) != null) {
                        Collection<SchoolCourse> coursesTakenButInProgress =
                                m_coursesAlreadyTakenButConsideredInProgress.get(coursOid);
                        schoolCoursesForRequirement.addAll(coursesTakenButInProgress);

                        for (SchoolCourse schoolCourse : coursesTakenButInProgress) {
                            // Update the credits in progress for these additional courses.
                            Double creditsForSchoolCourse = creditsInProgress.get(schoolCourse.getOid());
                            if (creditsForSchoolCourse == null) {
                                creditsForSchoolCourse = Double.valueOf(0);
                            }
                            Double creditsForThisSchoolCourse = schoolCourse.getCredit() == null ? Double.valueOf(0)
                                    : Double.valueOf(schoolCourse.getCredit().doubleValue());

                            // Total the existing credit value with the current school course credit
                            // value.
                            creditsForSchoolCourse = Double.valueOf(
                                    creditsForSchoolCourse.doubleValue() + creditsForThisSchoolCourse.doubleValue());

                            creditsInProgress.put(schoolCourse.getOid(), creditsForSchoolCourse);
                        }
                    }
                }

                if (schoolCoursesForRequirement != null && !schoolCoursesForRequirement.isEmpty()) {
                    // Courses taking is a list of courses a student is taking that does not have a
                    // transcript with a credit value greater than 0 for it. It does not look at any
                    // marks.
                    coursesTaking.put(requirementOid, new LinkedList<SchoolCourse>(schoolCoursesForRequirement));
                }
            }
        }

        /**
         * Returns the credits waived for the passed student and program of studies.
         *
         * @param studentOid String
         * @param programStudiesOid String
         * @param creditsWaived Key is a Requirement oid
         *        value is the amount credit waived for the requirement
         * @return void
         * @throws JDOMException exception
         * @throws IOException Signals that an I/O exception has occurred.
         */
        private void getCreditWaived(String studentOid, String programStudiesOid, HashMap<String, Double> creditsWaived)
                throws JDOMException, IOException {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(GraduationStudentWaiver.REL_STUDENT_PROGRAM + ModelProperty.PATH_DELIMITER
                    + GraduationStudentProgram.COL_STUDENT_OID, studentOid);
            criteria.addEqualTo(GraduationStudentWaiver.REL_STUDENT_PROGRAM + ModelProperty.PATH_DELIMITER
                    + GraduationStudentProgram.COL_PROGRAM_STUDIES_OID, programStudiesOid);

            QueryByCriteria query = new QueryByCriteria(GraduationStudentWaiver.class, criteria);
            Collection<GraduationStudentWaiver> waivers = m_broker.getCollectionByQuery(query);

            GraduationHelper.parseStudentProgramStudies(waivers, creditsWaived, new HashMap<String, String>());
        }

        /**
         * Returns the credit value from the map. If there is no value default to 0 and set in the
         * map.
         *
         * @param creditMap HashMap<String,Double>
         * @param mapKey String
         * @return Double
         */
        private Double getMapCredits(HashMap<String, Double> creditMap, String mapKey) {
            Double credits = creditMap.get(mapKey);

            if (credits == null) {
                credits = Double.valueOf(0);
                creditMap.put(mapKey, credits);
            }

            return credits;
        }


        /**
         * Determine whether or not we should use partial credits when counting this course
         * for this requirement.
         *
         * @param requirementOid String
         * @param schoolCourseOid String
         * @param creditsFromTranscript Double
         * @param partialCreditsMap Map<String,Map<String,Double>>
         * @return double
         */
        private double getPossibleCredits(String requirementOid,
                                          String schoolCourseOid,
                                          Double creditsFromTranscript,
                                          Map<String, Map<String, Double>> partialCreditsMap) {

            double credits = creditsFromTranscript == null ? 0.0 : creditsFromTranscript.doubleValue();


            if (partialCreditsMap.get(requirementOid) != null &&
                    partialCreditsMap.get(requirementOid).get(schoolCourseOid) != null) {
                credits = partialCreditsMap.get(requirementOid).get(schoolCourseOid).doubleValue();
            }
            return credits;
        }

        /**
         * Returns the list of requirements that are part of the requirement group of the passed
         * requirement
         * and its program study.
         *
         * @param programStudiesOid String
         * @param parentRequirmentOid String
         * @return LinkedList<Requirement>
         */
        private LinkedList<GraduationRequirement> getSubRequirements(String programStudiesOid,
                                                                     String parentRequirmentOid) {
            HashMap<String, Collection<String>> subRequirementOidMap = new HashMap<String, Collection<String>>();
            LinkedList<GraduationRequirement> m_subRequirements = null;

            GraduationRequirement requirement =
                    (GraduationRequirement) m_broker.getBeanByOid(GraduationRequirement.class, parentRequirmentOid);

            getOtherRequirement(requirement, subRequirementOidMap);

            if (!subRequirementOidMap.isEmpty()) {
                String subRequirementRelation = subRequirementOidMap.keySet().iterator().next();
                Collection<String> subRequirementOids = subRequirementOidMap.get(subRequirementRelation);

                X2Criteria criteria = new X2Criteria();
                criteria.addIn(X2BaseBean.COL_OID, subRequirementOids);

                QueryByCriteria query = new QueryByCriteria(GraduationRequirement.class, criteria);
                query.addOrderBy(GraduationRequirement.COL_CODE, true);

                m_subRequirements = new LinkedList<GraduationRequirement>(m_broker.getCollectionByQuery(query));
            } else {
                m_subRequirements = new LinkedList<GraduationRequirement>();
            }

            return m_subRequirements;
        }

        /**
         * Returns the credits gained by the student for the passed program of study.
         *
         * @param studentOid String
         * @param userData UserDataContainer
         * @param programStudiesOid String
         * @param coursesGainedCredits Key is a Requirement oid,
         *        value is the list courses gained the credits for the requirement
         * @param coursesTaken Key is a Requirement oid,
         *        value is the list courses the student has taken towards the requirement
         * @param coursesToTranscripts HashMap<String,List<String>>
         * @param creditsGained Key is a Requirement oid, value is the amount credits gained for the
         *        requirement
         * @param rawCreditsGained Key is a Requirement oid, value is the actual raw amount credits
         *        gained for the requirement without scaling.
         * @param creditsWaived Key is a requirement oid, value is the amount credits waived for the
         *        requirement
         * @param creditsRequired Key is a Requirement oid, value is the amount credits required for
         *        the requirement
         * @param creditsByCourse Key is a Course oid, value is the amount credits gained by the
         *        student for the course
         * @param gradeLevelByCourse Key is a Course oid, value is the grade level the student is in
         *        when taking the course
         * @param partialCourseRequirments Map<String,List<GraduationCourseRequirement>>
         * @param equivalentCourseDesc Key is the requirement and value is a a map of course
         *        descriptions where
         *        key is course oid, value is the course on the transcript.
         * @param otherRequirementValues Key is requirement, value is the map of field value pairs
         *        for other type requirement criteria fields.
         * @param satisfiedOtherRequirementOids List of other requirements which are satisfied.
         * @param customColumnHeaders List<String>
         * @param customColumnValues Map<String,Map<String,String>>
         * @return void
         * @throws FilterException exception
         */
        private void getUnitsGainedForProgram(String studentOid,
                                              UserDataContainer userData,
                                              String programStudiesOid,
                                              HashMap<String, List<SchoolCourse>> coursesGainedCredits,
                                              HashMap<String, List<SchoolCourse>> coursesTaken,
                                              HashMap<String, List<String>> coursesToTranscripts,
                                              HashMap<String, Double> creditsGained,
                                              HashMap<String, Double> rawCreditsGained,
                                              HashMap<String, Double> creditsWaived,
                                              HashMap<String, Double> creditsRequired,
                                              HashMap<String, Double> creditsByCourse,
                                              HashMap<String, String> gradeLevelByCourse,
                                              Map<String, List<GraduationCourseRequirement>> partialCourseRequirments,
                                              Map<String, Map<String, String>> equivalentCourseDesc,
                                              Map<String, Map<String, Object>> otherRequirementValues,
                                              List<String> satisfiedOtherRequirementOids,
                                              List<String> customColumnHeaders,
                                              Map<String, Map<String, String>> customColumnValues)
                throws FilterException {
            Set<GraduationRequirement> requirementsProcessed = new HashSet<GraduationRequirement>();

            X2Criteria requirementCriteria = new X2Criteria();
            requirementCriteria.addEqualTo(GraduationRequirement.COL_PROGRAM_STUDIES_OID, programStudiesOid);

            QueryByCriteria requirementQuery = new QueryByCriteria(GraduationRequirement.class, requirementCriteria);
            requirementQuery.addOrderByAscending(GraduationRequirement.COL_REQUIRED_UNIT);

            determineBestAssignments(programStudiesOid,
                    studentOid,
                    coursesGainedCredits,
                    coursesTaken,
                    coursesToTranscripts,
                    creditsGained,
                    rawCreditsGained,
                    creditsWaived,
                    creditsByCourse,
                    gradeLevelByCourse,
                    requirementsProcessed,
                    partialCourseRequirments,
                    equivalentCourseDesc,
                    customColumnHeaders,
                    customColumnValues);

            QueryIterator requirementIterator = m_broker.getIteratorByQuery(requirementQuery);
            try {
                while (requirementIterator.hasNext()) {
                    GraduationRequirement requirement = (GraduationRequirement) requirementIterator.next();
                    if (requirement.getType() != GraduationRequirement.TYPE_REQUIRMENT_GROUP) {
                        creditsRequired.put(requirement.getOid(),
                                Double.valueOf(requirement.getRequiredUnit().doubleValue()));
                    }

                    if (requirement.getType() == GraduationRequirement.TYPE_CREDIT) {
                        if (!requirementsProcessed.contains(requirement)) {
                            creditsGained.put(requirement.getOid(), Double.valueOf(0.0));
                            rawCreditsGained.put(requirement.getOid(), Double.valueOf(0.0));
                            coursesGainedCredits.put(requirement.getOid(), new ArrayList<SchoolCourse>());
                            requirementsProcessed.add(requirement);
                            equivalentCourseDesc.put(requirement.getOid(), new HashMap<String, String>());
                        }
                    } else {
                        getUnitsGainedForRequirement(studentOid,
                                userData,
                                requirement,
                                coursesGainedCredits,
                                coursesTaken,
                                coursesToTranscripts,
                                creditsGained,
                                rawCreditsGained,
                                creditsWaived,
                                creditsRequired,
                                creditsByCourse,
                                gradeLevelByCourse,
                                requirementsProcessed,
                                partialCourseRequirments,
                                equivalentCourseDesc,
                                otherRequirementValues,
                                satisfiedOtherRequirementOids,
                                customColumnHeaders,
                                customColumnValues);
                    }
                }
            } finally {
                requirementIterator.close();
            }
        }

        /**
         * Returns the units gained by the student for the passed requirement.
         *
         * @param studentOid String
         * @param userData UserDataContainer
         * @param requirement GraduationRequirement
         * @param coursesGainedCredits Key is a Requirement oid,
         *        value is the list courses gained the credits for the requirement
         * @param coursesTaken Key is a Requirement oid,
         *        value is the list courses the student has taken towards the requirement
         * @param coursesToTranscripts Key is a school course oid, value is a list of transcripts
         *        associated with it.
         * @param creditsGained Key is a Requirement oid, value is the amount credits gained for the
         *        requirement
         * @param rawCreditsGained Key is a Requirement oid, value is the actual raw amount credits
         *        gained for the requirement without scaling.
         * @param creditsWaived Key is a requirement oid, value is the value waived for that
         *        requirement.
         * @param creditsRequired Key is a Requirement oid, value is the amount credits required for
         *        the requirement
         * @param creditsByCourse Key is a SchoolCourse oid, value is the amount credits gained by
         *        the student for the course
         * @param gradeLevelByCourse Key is a SchoolCourse oid, value is the grade level the student
         *        is in when taking the course
         * @param requirementsProcessed List of requirements that have already been processed in
         *        this program.
         * @param partialcourseRequirements Map<String,List<GraduationCourseRequirement>>
         * @param equivalentCourseDesc Key is the requirement and value is a a map of course
         *        descriptions where
         *        key is course oid, value is the course on the transcript.
         * @param otherRequirementValues Key is requirement, value is the map of field value pairs
         *        for other type requirement criteria fields.
         * @param satisfiedOtherRequirementOids List of satisfied other requirements to handle 0.0
         *        credit other type requirements
         * @param customColumnHeaders List<String>
         * @param customColumnValues Map<String,Map<String,String>>
         * @return void
         * @throws FilterException exception
         */
        private void getUnitsGainedForRequirement(String studentOid,
                                                  UserDataContainer userData,
                                                  GraduationRequirement requirement,
                                                  HashMap<String, List<SchoolCourse>> coursesGainedCredits,
                                                  HashMap<String, List<SchoolCourse>> coursesTaken,
                                                  HashMap<String, List<String>> coursesToTranscripts,
                                                  HashMap<String, Double> creditsGained,
                                                  HashMap<String, Double> rawCreditsGained,
                                                  HashMap<String, Double> creditsWaived,
                                                  HashMap<String, Double> creditsRequired,
                                                  HashMap<String, Double> creditsByCourse,
                                                  HashMap<String, String> gradeLevelByCourse,
                                                  Set<GraduationRequirement> requirementsProcessed,
                                                  Map<String, List<GraduationCourseRequirement>> partialcourseRequirements,
                                                  Map<String, Map<String, String>> equivalentCourseDesc,
                                                  Map<String, Map<String, Object>> otherRequirementValues,
                                                  List<String> satisfiedOtherRequirementOids,
                                                  List<String> customColumnHeaders,
                                                  Map<String, Map<String, String>> customColumnValues)
                throws FilterException {
            if (requirement != null) {
                if (requirement.getType() == GraduationRequirement.TYPE_CREDIT) {
                    if (!requirementsProcessed.contains(requirement)) {
                        creditsGained.put(requirement.getOid(), Double.valueOf(0.0));
                        rawCreditsGained.put(requirement.getOid(), Double.valueOf(0.0));
                        coursesGainedCredits.put(requirement.getOid(), new ArrayList<SchoolCourse>());
                        requirementsProcessed.add(requirement);
                        equivalentCourseDesc.put(requirement.getOid(), new HashMap<String, String>());
                    }
                } else if (requirement.getType() == GraduationRequirement.TYPE_PROGRAM) {
                    HashMap<String, Double> subProgramCreditsGained = new HashMap<String, Double>();
                    HashMap<String, Double> subProgramExtraCreditsGained = new HashMap<String, Double>();
                    getUnitsGainedForProgram(studentOid, userData, requirement.getSubProgramStudiesOid(),
                            coursesGainedCredits,
                            coursesTaken, coursesToTranscripts, subProgramCreditsGained, subProgramExtraCreditsGained,
                            creditsWaived, creditsRequired, creditsByCourse, gradeLevelByCourse,
                            partialcourseRequirements, equivalentCourseDesc, otherRequirementValues,
                            satisfiedOtherRequirementOids,
                            customColumnHeaders, customColumnValues);

                    creditsGained.putAll(subProgramCreditsGained);

                    double totalCredits = 0;
                    for (String requirementOid : subProgramCreditsGained.keySet()) {
                        totalCredits += subProgramCreditsGained.get(requirementOid).doubleValue();
                    }

                    double rawTotalCredits = 0;
                    for (String requirementOid : subProgramExtraCreditsGained.keySet()) {
                        rawTotalCredits += subProgramExtraCreditsGained.get(requirementOid).doubleValue();
                    }

                    /*
                     * Scale the subprogram's total credits
                     */
                    double scaledTotal = requirement.getSubProgramStudies() != null &&
                            requirement.getSubProgramStudies().getTotalCredit() != null &&
                            requirement.getSubProgramStudies().getTotalCredit().doubleValue() > 0
                                    ? totalCredits * requirement.getRequiredUnit().doubleValue()
                                            / requirement.getSubProgramStudies().getTotalCredit().doubleValue()
                                    : totalCredits;

                    creditsGained.put(requirement.getOid(), Double.valueOf(scaledTotal));
                    rawCreditsGained.put(requirement.getOid(), Double.valueOf(rawTotalCredits));
                } else {
                    HashMap<String, Collection<String>> subRequirementOidMap =
                            new HashMap<String, Collection<String>>();
                    Collection<Filter> filters = getOtherRequirement(requirement, subRequirementOidMap);

                    if (requirement.getType() == GraduationRequirement.TYPE_OTHER) {
                        SisStudent student = (SisStudent) m_broker.getBeanByOid(SisStudent.class, studentOid);

                        X2Criteria otherCriteria = new X2Criteria();
                        otherCriteria.addEqualTo(X2BaseBean.COL_OID, studentOid);

                        String requirementField = requirement.getEvaluationField();
                        Map<String, Object> filterValues = new HashMap<String, Object>();

                        X2Criteria checkCriteria = new X2Criteria();
                        X2Criteria requirementCriteria = new X2Criteria();
                        String tableClassName = null;

                        for (Filter filter : filters) {
                            filter.applyTo(otherCriteria, userData);

                            if (filter.getField().isSimple()) {
                                /*
                                 * If the field is simple or there is only one filter then set the
                                 * value pair
                                 * to be fieldname : value.
                                 */
                                if (filter.getField().getBeanPath().equals(requirementField) || filters.size() == 1) {
                                    String valuePair = filter.getValue() + ":"
                                            + String.valueOf(student.getFieldValueByProperty(filter.getField()));
                                    filterValues.put(filter.getField().getField().getUserShortName(), valuePair);
                                }
                            } else if (!StringUtils.isEmpty(requirementField)) {
                                List<DataDictionaryRelationship> relations = filter.getField().getRelationships();

                                if (relations.size() == 1) {
                                    /*
                                     * For single relation we need to determine the relation type
                                     * and potentially
                                     * create new criteria to find related objects.
                                     */
                                    DataDictionaryRelationship relation = relations.get(0);

                                    if (relation.getRelatedRelationType()
                                            .equals(DataDictionaryRelationship.ONE_TYPE_CODE)) {
                                        /*
                                         * Similar to simple field we just pull data directly.
                                         */
                                        if (filter.getField().getBeanPath().equals(requirementField)
                                                || filters.size() == 1) {
                                            String valuePair = filter.getValue() + ":" + String
                                                    .valueOf(student.getFieldValueByProperty(filter.getField()));
                                            filterValues.put(filter.getField().getField().getUserShortName(),
                                                    valuePair);
                                        }
                                    } else if (relation.getRelatedRelationType()
                                            .equals(DataDictionaryRelationship.MANY_TYPE_CODE)) {
                                        String relationOfRequirement = requirementField.substring(0,
                                                requirementField.indexOf(ModelProperty.PATH_DELIMITER));

                                        if (relation.getRelatedJavaName().equals(relationOfRequirement)) {
                                            /*
                                             * Create a new filter based on related table so we can
                                             * create criteria to return
                                             * related records.
                                             */
                                            String relatedDictionaryPath = filter.getField().getDictionaryPath()
                                                    .substring(filter.getField().getDictionaryPath()
                                                            .indexOf(ModelProperty.PATH_DELIMITER) + 1);
                                            Filter newFilter = new Filter(new ModelProperty(relatedDictionaryPath,
                                                    student.getPersistenceKey()),
                                                    filter.getOperator(), filter.getConnector(), filter.getSource(),
                                                    filter.getValue(), filter.getLocale());
                                            tableClassName = relation.getRelatedDataTable().getClassName();

                                            if (filter.getField().getBeanPath().equals(requirementField)) {
                                                newFilter.applyTo(requirementCriteria, userData);
                                            } else {
                                                newFilter.applyTo(checkCriteria, userData);
                                            }
                                        }
                                    }
                                } else if (!relations.isEmpty()) {
                                    DataDictionaryRelationship relation = relations.get(0);
                                    if (relation.getRelatedRelationType()
                                            .equals(DataDictionaryRelationship.MANY_TYPE_CODE)) {
                                        String relationOfRequirement = requirementField.substring(0,
                                                requirementField.indexOf(ModelProperty.PATH_DELIMITER));
                                        /*
                                         * Create a new filter based on related table so we can
                                         * create criteria to return
                                         * related records.
                                         */
                                        if (!filter.getField().getBeanPath().equals(requirementField) &&
                                                relation.getRelatedJavaName().equals(relationOfRequirement)) {
                                            String relatedDictionaryPath = filter.getField().getDictionaryPath()
                                                    .substring(filter.getField().getDictionaryPath()
                                                            .indexOf(ModelProperty.PATH_DELIMITER) + 1);
                                            Filter newFilter = new Filter(new ModelProperty(relatedDictionaryPath,
                                                    student.getPersistenceKey()),
                                                    filter.getOperator(), filter.getConnector(), filter.getSource(),
                                                    filter.getValue(), filter.getLocale());
                                            newFilter.applyTo(checkCriteria, userData);
                                            tableClassName = relation.getRelatedDataTable().getClassName();
                                        }
                                    }
                                }
                            }
                        }

                        if (!StringUtils.isEmpty(requirementField) && !checkCriteria.isEmpty()
                                && !StringUtils.isEmpty(tableClassName)) {
                            checkCriteria.addEqualTo("studentOid", studentOid);
                            loadRelatedValues(requirementField, checkCriteria, requirementCriteria, tableClassName,
                                    filterValues);
                        }

                        otherRequirementValues.put(requirement.getOid(), filterValues);

                        QueryByCriteria otherQuery = new QueryByCriteria(SisStudent.class, otherCriteria);
                        if (!m_broker.getCollectionByQuery(otherQuery).isEmpty()) {
                            creditsGained.put(requirement.getOid(),
                                    Double.valueOf(requirement.getRequiredUnit().doubleValue()));
                            rawCreditsGained.put(requirement.getOid(),
                                    Double.valueOf(requirement.getRequiredUnit().doubleValue()));
                            satisfiedOtherRequirementOids.add(requirement.getOid());
                        }
                    } else if (requirement.getType() == GraduationRequirement.TYPE_REQUIRMENT_GROUP) {
                        if (!subRequirementOidMap.isEmpty()) {
                            String subRequirementType = subRequirementOidMap.keySet().iterator().next();
                            Collection<String> subRequirementOids = subRequirementOidMap.get(subRequirementType);

                            if (!subRequirementOids.isEmpty()) {
                                for (String subOid : subRequirementOids) {
                                    GraduationRequirement subRequirement = (GraduationRequirement) m_broker
                                            .getBeanByOid(GraduationRequirement.class, subOid);
                                    if (!requirement.equals(subRequirement)) {
                                        getUnitsGainedForRequirement(studentOid, userData, subRequirement,
                                                coursesGainedCredits,
                                                coursesTaken, coursesToTranscripts, creditsGained,
                                                rawCreditsGained, creditsWaived, creditsRequired, creditsByCourse,
                                                gradeLevelByCourse,
                                                requirementsProcessed, partialcourseRequirements,
                                                equivalentCourseDesc, otherRequirementValues,
                                                satisfiedOtherRequirementOids,
                                                customColumnHeaders, customColumnValues);
                                    }
                                }

                                if (subRequirementType.equals(GraduationManager.REQUIREMENT_LIST_RELATION_ANY)) {
                                    /*
                                     * For "or" credits group, the credits gained for the
                                     * requirement is the max credits
                                     * gained from any of the alternates.
                                     *
                                     * The number of required credits is the required credits for
                                     * sub-requirement
                                     * that will give the student most benefits.
                                     */
                                    double maxCreditGained = -1;
                                    double maxRawCreditGained = -1;
                                    double maxCreditWaived = -1;

                                    double required = -1;
                                    double satisfyPercent = 0;
                                    double currentSatisfiedPercent = 0;

                                    for (String subOid : subRequirementOids) {
                                        GraduationRequirement subRequirement = (GraduationRequirement) m_broker
                                                .getBeanByOid(GraduationRequirement.class, subOid);
                                        if (subRequirement != null) {
                                            if (required == -1) {
                                                required = subRequirement.getRequiredUnit().doubleValue();
                                            }

                                            double waivedCredits = creditsWaived.get(subOid) == null ? 0.0
                                                    : creditsWaived.get(subOid).doubleValue();
                                            double credits = creditsGained.get(subOid) == null ? 0.0
                                                    : creditsGained.get(subOid).doubleValue();
                                            double rawCredits = rawCreditsGained.get(subOid) == null ? 0.0
                                                    : rawCreditsGained.get(subOid).doubleValue();

                                            currentSatisfiedPercent =
                                                    subRequirement.getRequiredUnit().doubleValue() == 0 ? 0
                                                            : (waivedCredits + rawCredits) * 100
                                                                    / subRequirement.getRequiredUnit().doubleValue();

                                            /*
                                             * Calculate which requirement has the highest satisfy
                                             * rate and use
                                             * it as the required.
                                             */
                                            if (currentSatisfiedPercent > satisfyPercent) {
                                                required = subRequirement.getRequiredUnit().doubleValue();
                                                satisfyPercent = (rawCredits + waivedCredits) * 100 / required;
                                                maxCreditGained = credits;
                                                maxRawCreditGained = rawCredits;
                                                maxCreditWaived = waivedCredits;
                                            }
                                        }
                                    }

                                    creditsRequired.put(requirement.getOid(), Double.valueOf(required));
                                    creditsGained.put(requirement.getOid(),
                                            Double.valueOf(maxCreditGained < 0 ? 0 : maxCreditGained));
                                    rawCreditsGained.put(requirement.getOid(),
                                            Double.valueOf(maxRawCreditGained < 0 ? 0 : maxRawCreditGained));

                                    if (maxCreditWaived > 0) {
                                        creditsWaived.put(requirement.getOid(), Double.valueOf(maxCreditWaived));
                                    }
                                } else if (subRequirementType.equals(GraduationManager.REQUIREMENT_LIST_RELATION_ALL)) {
                                    /*
                                     * For "and" credits group, the credits gained for the
                                     * requirement is the total
                                     * requirement gained and scaled back to the requirement.
                                     */
                                    double totalCreditsGained = 0;
                                    double totalRawCreditsGained = 0;
                                    double totalWaived = 0;

                                    for (String subOid : subRequirementOids) {
                                        Double credits = creditsGained.get(subOid);
                                        if (credits != null && credits.doubleValue() > 0) {
                                            totalCreditsGained += credits.doubleValue();
                                        }

                                        Double rawCredits = rawCreditsGained.get(subOid);
                                        if (rawCredits != null && rawCredits.doubleValue() > 0) {
                                            totalRawCreditsGained += rawCredits.doubleValue();
                                        }

                                        Double waivedCredits = creditsWaived.get(subOid);
                                        if (waivedCredits != null && waivedCredits.doubleValue() > 0) {
                                            totalWaived += waivedCredits.doubleValue();
                                        }
                                    }

                                    if (totalWaived > 0) {
                                        creditsWaived.put(requirement.getOid(),
                                                Double.valueOf(new BigDecimal(String.valueOf(totalWaived))
                                                        .setScale(4, RoundingMode.HALF_UP).doubleValue()));
                                    }

                                    rawCreditsGained.put(requirement.getOid(),
                                            Double.valueOf(new BigDecimal(String.valueOf(totalRawCreditsGained))
                                                    .setScale(4, RoundingMode.HALF_UP).doubleValue()));

                                    if (requirement.getRequiredUnit() != null &&
                                            requirement.getRequiredUnit().doubleValue() > 0 &&
                                            totalCreditsGained > requirement.getRequiredUnit().doubleValue()) {
                                        totalCreditsGained = requirement.getRequiredUnit().doubleValue();
                                    }
                                    creditsGained.put(requirement.getOid(),
                                            Double.valueOf(new BigDecimal(String.valueOf(totalCreditsGained))
                                                    .setScale(4, RoundingMode.HALF_UP).doubleValue()));

                                    /*
                                     * For "AND" requirement group, the number of required
                                     * requirements
                                     * is the total credits set on parent requirement.
                                     */
                                    creditsRequired.put(requirement.getOid(),
                                            Double.valueOf(requirement.getRequiredUnit().doubleValue()));
                                }
                            }
                        }
                    }
                }
            }
        }

        /**
         * Returns a collection of schoolCourses, based on the original, where the course
         * descriptions have been replaced with user defined descriptions from the student's
         * transcript records.
         *
         * @param studentOid - student oid whose transcripts to examine
         * @param schoolCourses - list of school courses to be modified
         *
         * @return Collection<SchoolCourse>
         */
        private Collection<SchoolCourse> getUserDefinedCourseDescriptions(String studentOid,
                                                                          Collection<SchoolCourse> schoolCourses) {
            LinkedHashSet<SchoolCourse> schoolCourses2 = new LinkedHashSet<SchoolCourse>();
            LinkedHashSet<String> schoolCourseOids = new LinkedHashSet<String>(schoolCourses.size());

            /*
             * build a map from the schoolCourse set
             */
            for (SchoolCourse schoolCourse : schoolCourses) {
                schoolCourseOids.add(schoolCourse.getOid());
            }

            Criteria criteria = new Criteria();
            criteria.addEqualTo(Transcript.REL_STUDENT, studentOid);
            criteria.addIn(Transcript.REL_SCHOOL_COURSE, schoolCourseOids);
            QueryByCriteria query = new QueryByCriteria(Transcript.class, criteria);
            Map<String, Transcript> transcripts =
                    m_broker.getMapByQuery(query, Transcript.COL_SCHOOL_COURSE_OID, schoolCourseOids.size());

            /*
             * Copy the school course collection to a new collection, replacing courses
             * that have a user define description in the student transcripts
             */
            for (SchoolCourse schoolCourse : schoolCourses) {
                Transcript transcript = transcripts.get(schoolCourse.getOid());
                if (transcript != null && transcript.getUserDescriptionIndicator() &&
                        !StringUtils.isEmpty(transcript.getCourseDescription())) {
                    /*
                     * clone the school course to avoid contaminating the cache
                     * when changing the course description
                     */
                    schoolCourse = schoolCourse.clone();
                    schoolCourse.setDescription(transcript.getCourseDescription());
                }
                schoolCourses2.add(schoolCourse);
            }

            return schoolCourses2;
        }

        /**
         * Applies the values based on the given criteria to the filter values.
         *
         * @param requirementField String
         * @param checkCriteria General criteria that checks matching records but not the required
         *        field
         * @param requirementCriteria A check of the required field
         * @param tableClassName String
         * @param filterValues Map<String,Object>
         */
        private void loadRelatedValues(String requirementField,
                                       X2Criteria checkCriteria,
                                       X2Criteria requirementCriteria,
                                       String tableClassName,
                                       Map<String, Object> filterValues) {
            try {
                requirementCriteria.addAndCriteria(checkCriteria);
                QueryByCriteria query = new QueryByCriteria(Class.forName(tableClassName), requirementCriteria);

                if (m_broker.getCount(query) == 0) {
                    query = new QueryByCriteria(Class.forName(tableClassName), checkCriteria);
                }

                String fieldPath =
                        requirementField.substring(requirementField.lastIndexOf(ModelProperty.PATH_DELIMITER) + 1);
                query.addOrderByDescending(fieldPath);

                Collection<X2BaseBean> objects = m_broker.getCollectionByQuery(query);
                if (!objects.isEmpty()) {
                    X2BaseBean bean = (X2BaseBean) objects.toArray()[0];
                    String value = String.valueOf(bean.getFieldValueByBeanPath(fieldPath));
                    String valuePair = "empty" + ":" + value;
                    filterValues.put(requirementField, valuePair);
                }
            } catch (ClassNotFoundException e) {
                // We don't want to throw an exception here
                // silently catch and continue.
            }
        }

        /**
         * Applies BC-specific business logic to graduation summary results.
         * <ul>
         * <li>Puts precendence on the non-elective requirements and fills those if the courses are
         * used in electives
         * <li>Ensures only one course is applied to non-elective requirements checking both the
         * gained credit courses
         * and the taking courses
         * </ul>
         *
         * @param programStudiesOid String
         * @param coursesGainedCredits HashMap<String,List<SchoolCourse>>
         * @param coursesTaken HashMap<String,List<SchoolCourse>>
         * @param coursesTaking HashMap<String,List<SchoolCourse>>
         * @param rawCreditsGained HashMap<String,Double>
         * @param creditsWaived HashMap<String,Double>
         * @param creditsRequired HashMap<String,Double>
         * @param creditsByCourse HashMap<String,Double>
         * @param creditsInProgress HashMap<String,Double>
         * @param satisfiedOtherRequirementOids List<String>
         */
        private void processBcRules(String programStudiesOid,
                                    HashMap<String, List<SchoolCourse>> coursesGainedCredits,
                                    HashMap<String, List<SchoolCourse>> coursesTaken,
                                    HashMap<String, List<SchoolCourse>> coursesTaking,
                                    HashMap<String, Double> rawCreditsGained,
                                    HashMap<String, Double> creditsWaived,
                                    HashMap<String, Double> creditsRequired,
                                    HashMap<String, Double> creditsByCourse,
                                    HashMap<String, Double> creditsInProgress,
                                    List<String> satisfiedOtherRequirementOids) {
            List<GraduationRequirement> requirements = getDirectRequirements(programStudiesOid, null);

            SortedMap<String, Double> sortedCredits =
                    new TreeMap<String, Double>(new CreditMapValueComparator(creditsByCourse));
            sortedCredits.putAll(creditsByCourse);

            // Find the elective requirement
            String electiveOid = null;
            for (GraduationRequirement requirement : requirements) {
                if (ELECTIVE_REQUIREMENT_CATEGORY.equals(requirement.getFieldA001())) {
                    electiveOid = requirement.getOid();
                    break;
                }
            }

            // After redistribution, check complete status and move in-progress (taking) courses
            for (GraduationRequirement requirement : requirements) {
                if (!requirement.getOid().equals(electiveOid)) {
                    processCompleteRequirement(requirement.getOid(),
                            coursesTaking,
                            rawCreditsGained,
                            creditsWaived,
                            creditsRequired,
                            creditsInProgress,
                            satisfiedOtherRequirementOids);
                }
            }
        }

        /**
         * Checks if the requirement is being met based on the completed and in-progress courses.
         *
         * @param requirementOid String
         * @param coursesTaking HashMap<String,List<SchoolCourse>>
         * @param rawCreditsGained HashMap<String,Double>
         * @param creditsWaived HashMap<String,Double>
         * @param creditsRequired HashMap<String,Double>
         * @param creditsInProgress HashMap<String,Double>
         * @param satisfiedOtherRequirementOids List<String>
         * @return boolean
         */
        private void processCompleteRequirement(String requirementOid,
                                                HashMap<String, List<SchoolCourse>> coursesTaking,
                                                HashMap<String, Double> rawCreditsGained,
                                                HashMap<String, Double> creditsWaived,
                                                HashMap<String, Double> creditsRequired,
                                                HashMap<String, Double> creditsInProgress,
                                                List<String> satisfiedOtherRequirementOids) {
            int completePercent = 0;


            completePercent = getRequirementSatisfiedStatus(requirementOid,
                    rawCreditsGained,
                    creditsWaived,
                    creditsRequired,
                    satisfiedOtherRequirementOids);

            if (completePercent < 100) {
                processInProgressCredits(requirementOid,
                        creditsInProgress,
                        coursesTaking,
                        creditsRequired,
                        rawCreditsGained);
            } else {
                /*
                 * If requirement is met, don't track what is currently 'taking' as the only
                 * requirement that allows
                 * overage is the elective requirement
                 */
                List<SchoolCourse> courses = coursesTaking.get(requirementOid);
                if (!CollectionUtils.isEmpty(courses)) {
                    courses.clear();
                }
            }
        }

        /**
         * Returns the number of in-progress credits for the passed requirement based on the
         * in-progress courses for
         * each requirement. If a course is used in progress for the current requirment, it is
         * removed as an in-progress
         * course for all other requirements (so it is only counted once).
         *
         * @param requirementOid String
         * @param creditsInProgress HashMap<String,Double>
         * @param coursesTaking HashMap<String,List<SchoolCourse>>
         * @param creditsRequiredMap HashMap<String,Double>
         * @param rawCreditsGained HashMap<String,Double>
         */
        private void processInProgressCredits(String requirementOid,
                                              HashMap<String, Double> creditsInProgress,
                                              HashMap<String, List<SchoolCourse>> coursesTaking,
                                              HashMap<String, Double> creditsRequiredMap,
                                              HashMap<String, Double> rawCreditsGained) {
            double creditsRequired = getTotalRequired(null, requirementOid, creditsRequiredMap);
            double creditsEarned = getTotalCreditsGained(null, requirementOid, rawCreditsGained);

            List<SchoolCourse> coursesToRemove = new LinkedList<SchoolCourse>();
            List<SchoolCourse> currentCourses = coursesTaking.get(requirementOid);

            double credits = 0.0;

            if (!CollectionUtils.isEmpty(currentCourses)) {
                /*
                 * Tally potential credits for courses being taken. Once the required credits are
                 * met stop
                 */
                for (SchoolCourse course : currentCourses) {
                    Double inProgress = creditsInProgress.get(course.getOid());
                    if (inProgress != null) {
                        credits += inProgress.doubleValue();
                    }

                    coursesToRemove.add(course);

                    if (credits + creditsEarned >= creditsRequired) {
                        break;
                    }
                }

                // Update the list of 'taking' courses for each requirement
                for (String otherRequirementOid : coursesTaking.keySet()) {
                    List<SchoolCourse> otherCourses = coursesTaking.get(otherRequirementOid);

                    if (!otherRequirementOid.equals(requirementOid)) {
                        // For other requirements, remove the course from their list of taken
                        // courses
                        for (SchoolCourse course : coursesToRemove) {
                            otherCourses.remove(course);
                        }
                    } else {
                        // Remove any other courses from the taking list
                        otherCourses.retainAll(coursesToRemove);
                    }
                }
            }
        }

        /**
         * Includes the course in the gained credits.
         *
         * @param coursesGainedCredits HashMap<String,List<SchoolCourse>>
         * @param schoolCourse SchoolCourse
         * @param requirement GraduationRequirement
         */
        private void trackIncludedCourses(HashMap<String, List<SchoolCourse>> coursesGainedCredits,
                                          SchoolCourse schoolCourse,
                                          GraduationRequirement requirement) {
            List<SchoolCourse> coursesIncluded = coursesGainedCredits.get(requirement.getOid());

            if (coursesIncluded == null) {
                coursesIncluded = new ArrayList<SchoolCourse>();
                coursesGainedCredits.put(requirement.getOid(), coursesIncluded);
            }

            coursesIncluded.add(schoolCourse);
        }

        /**
         * If the transcript record requires an exam, confirm there is a blended mark if necessary.
         *
         * @param transcript Transcript
         * @return boolean
         */
        private boolean validateMarks(Transcript transcript) {
            SchoolCourse schoolCourse = transcript.getSchoolCourse();

            boolean examinable = lookupExamStatus(schoolCourse);
            String trax = (String) transcript.getFieldValueByAlias(TRN_TRAX_ALIAS);

            String grade = getFinalGrade(transcript, examinable, trax);

            return !StringUtils.isEmpty(grade);
        }
    }
}
