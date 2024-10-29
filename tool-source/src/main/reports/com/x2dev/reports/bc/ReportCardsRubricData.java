/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import static com.x2dev.sis.tools.reports.TranscriptReportGrid.COL_STUDENT;
import com.follett.fsc.core.framework.persistence.CollectionCriteriaHelper;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.StaffSchoolAssociation;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.reports.GradeReportJavaSource;
import com.x2dev.sis.tools.reports.RubricTranscriptReportGrid;
import com.x2dev.sis.tools.reports.TranscriptReportGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Java source for BC's report cards. This report utilizes subreport to use a single source file for
 * all levels of
 * report cards (Kindergarten, Intermediate, Middle, Primary, Secondary).
 * <p>
 * Functional changes from standard version:
 * <ul>
 * <li>List out daily absences and tardies by month
 * <li>Includes course attendance tallies by term
 * <li>Builds a display of teachers different from the student's homeroom teacher
 * <li>Displays the numeric grade in additional to the letter grade
 * </ul>
 *
 * @author Follett School Solutions
 */
public class ReportCardsRubricData extends GradeReportJavaSource {
    private static final long serialVersionUID = 1L;

    /*
     * Input parameters.
     */
    private static final String INPUT_PARAM_COMMENT_COURSES = "commentCourses";
    private static final String INPUT_PARAM_IS_FRENCH = "isFrench";
    private static final String INPUT_PARAM_REPORT_ID = "reportId";
    private static final String INPUT_PARAM_REPORT_TYPE = "reportCardType";
    private static final String INPUT_INCLUDE_SCHOOL_MESSAGE_PARAM = "includeSchoolMessage";
    private static final String INPUT_PARAM_INCLUDE_SECONDARY = "secondaryStudent";
    private static final String INPUT_PARAM_SIGNATURE = "includeSignature";
    private static final String INPUT_PARAM_PERFORMANCE_SCALE = "includePerformance";
    private static final String INPUT_PARAM_COMMENTS = "includeTermComments";
    private static final String INPUT_PARAM_PRINT_BOTH = "printBothSides";
    private static final String INPUT_PARAM_TEACHER_SIGNATURE = "teacherSignature";
    private static final String INPUT_PARAM_GPA = "includeGPA";
    private static final String INPUT_PARAM_INFO_PAGE = "includeInfoPage";
    private static final String INPUT_PARAM_FINAL_MARK = "includeFinalMark";
    private static final String INPUT_PARAM_ATTENDANCE = "includeAttendance";
    private static final String INPUT_PARAM_FINAL_EXAM = "includeFinalExam";
    private static final String INPUT_PARAM_FIRST_NAME_TO_DISPLAY = "firstNameType";
    private static final String INPUT_PARAM_LAST_NAME_TO_DISPLAY = "lastNameType";
    private static final String INPUT_PARAM_LEGAL_SIZE = "printLegalSize";

    /*
     * Report parameters
     */
    private static final String PARAM_ATTENDANCE = "includeAttendance";
    private static final String PARAM_ATTENDANCES = "attendances";
    private static final String PARAM_BOTH_SIDES = "printBothSides";
    private static final String PARAM_CONTEXT = "context";
    private static final String PARAM_GRADE_LEVEL = "gradeLevel";
    private static final String PARAM_HOMEROOM = "homeroom";
    private static final String PARAM_HOMEROOM_TEACHER = "homeroomTeacher";
    private static final String PARAM_ADDNL_TEACHERS = "additionalTeacherMap";
    private static final String PARAM_FINAL_MARK = "includeFinalMark";
    private static final String PARAM_FINAL_EXAM = "includeFinalExam";
    private static final String PARAM_FIRST_NAME_TO_DISPLAY = "firstNameType";
    private static final String PARAM_GRADE_TERM_DATES = "gradeTermDates";
    private static final String PARAM_GPA = "includeGPA";
    private static final String PARAM_INFO_PAGE = "includeInfoPage";
    private static final String PARAM_LAST_NAME_TO_DISPLAY = "lastNameType";
    private static final String PARAM_MESSAGE = "reportMessage";
    private static final String PARAM_PERFORMANCE_SCALE = "performanceScale";
    private static final String PARAM_SIGNATURE = "signature";
    private static final String PARAM_TEACHER_SIGNATURE = "teacherSignature";
    private static final String PARAM_TERM_COMMENT_MAP = "termCommentMap";

    /*
     * Column constants
     */
    private static final String ABSENT_POSTFIX = "_absent";
    private static final String LATE_POSTFIX = "_late";
    private static final String GRADE_POSTFIX = "_grade";

    private static final List<String> COLUMNS = Arrays.asList(new String[] {"Tri 1", "Tri 2", "Tri 3", "Final Rubric"});
    private static final List<String> COMMENT_COLUMNS =
            Arrays.asList(new String[] {"Tri 1 Com", "Tri 2 Com", "Tri 3 Com"});

    /*
     * Report ID constants
     */
    private static final String FRENCH_REPORT_SUFFIX = "-F";
    private static final String LEGAL_SIZE_SUFFIX = "-L";

    /*
     * General constants
     */
    private static final String DELIMITER = "_";
    private static final int TOTAL_MONTH_INDEX = 12;
    private static final int TOTAL_TERM_INDEX = 4;
    private static final String VALUE_LEGAL = "legal";
    private static final String CONST_ALIAS_STF_PRIM_HR_TEACHER = "cust-STF-primary-homeroom-teacher";

    /*
     * Members
     */
    private Map<String, Map<String, RubricPerformanceDescriptor>> m_criterionDescriptorsByRating;
    private boolean m_isInStaffView;
    private Map<String, String> m_gradeLevel;
    private Map<String, String> m_homeroom;
    private Map<String, String> m_homeroomTeacherByStudent;
    private Map<String, String> m_homeroomTeacherByHomeroom;
    private Map<String, String> m_homeroomAddnlTeachers;
    private Map<String, Integer> m_homeroomAddnlTeachersNo;
    private DecimalFormat m_numberFormatter;
    private ReportDataGrid m_reportGrid;
    private CollectionCriteriaHelper m_studentCollectionCriteriaHelper;
    private Criteria m_termCommentCriteria;

    /**
     * Gets the data break column.
     *
     * @return String
     * @see com.x2dev.sis.tools.reports.GradeReportJavaSource#getDataBreakColumn()
     */
    @Override
    public String getDataBreakColumn() {
        return COL_STUDENT;
    }

    /**
     * Builds the criteria.
     *
     * @see com.x2dev.sis.tools.reports.GradeReportJavaSource#buildCriteria()
     */
    @Override
    protected void buildCriteria() {
        m_transcriptCriteria = new X2Criteria();
        m_studentCriteria = new X2Criteria();
        m_assessmentCriteria = new X2Criteria();
        m_classAttendanceCriteria = new X2Criteria();
        m_enrollmentCriteria = new X2Criteria();
        m_scheduleCriteria = new X2Criteria();

        boolean tempTableInUse = isCurrentQueryContainsUnion();
        if (!tempTableInUse) {
            getBroker().beginTransaction();
        }

        if (m_currentStudent != null) {
            /*
             * Running for one student
             */
            m_transcriptCriteria.addEqualTo(Transcript.COL_STUDENT_OID, m_currentStudent.getOid());
            m_studentCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
            m_assessmentCriteria.addEqualTo(StudentAssessment.COL_STUDENT_OID, m_currentStudent.getOid());
            m_classAttendanceCriteria.addEqualTo(StudentPeriodAttendance.COL_STUDENT_OID, m_currentStudent.getOid());
            m_enrollmentCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, m_currentStudent.getOid());
            m_scheduleCriteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, m_currentStudent.getOid());

            Collection<String> identifiers = new LinkedList<String>();
            identifiers.add(m_currentStudent.getOid());

            if (!tempTableInUse) {
                m_studentCollectionCriteriaHelper = new CollectionCriteriaHelper(identifiers, getBroker());
            }
        } else {
            /*
             * Running for multiple students
             */
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(m_studentCriteria, queryBy.replace("student.", ""), queryString, SisStudent.class,
                    SisStudent.class, X2BaseBean.COL_OID);
            if (tempTableInUse) {
                if (m_isInStaffView) {
                    addUserCriteria(m_transcriptCriteria, queryBy, queryString, SisStudent.class,
                            Transcript.COL_STUDENT_OID);
                    addUserCriteria(m_assessmentCriteria, queryBy, queryString, SisStudent.class,
                            StudentAssessment.COL_STUDENT_OID);
                    addUserCriteria(m_classAttendanceCriteria, queryBy, queryString, SisStudent.class,
                            StudentPeriodAttendance.COL_STUDENT_OID);
                    addUserCriteria(m_enrollmentCriteria, queryBy, queryString, SisStudent.class,
                            StudentEnrollment.COL_STUDENT_OID);
                    addUserCriteria(m_scheduleCriteria, queryBy, queryString, SisStudent.class,
                            StudentSchedule.COL_STUDENT_OID);
                } else {
                    addUserCriteria(m_transcriptCriteria, queryBy, queryString, Transcript.class, SisStudent.class,
                            Transcript.COL_STUDENT_OID);
                    addUserCriteria(m_assessmentCriteria, queryBy, queryString, StudentAssessment.class,
                            SisStudent.class, StudentAssessment.COL_STUDENT_OID);
                    addUserCriteria(m_classAttendanceCriteria, queryBy, queryString, StudentPeriodAttendance.class,
                            SisStudent.class, StudentPeriodAttendance.COL_STUDENT_OID);
                    addUserCriteria(m_enrollmentCriteria, queryBy, queryString, StudentEnrollment.class,
                            SisStudent.class, StudentEnrollment.COL_STUDENT_OID);
                    addUserCriteria(m_scheduleCriteria, queryBy, queryString, StudentSchedule.class, SisStudent.class,
                            StudentSchedule.COL_STUDENT_OID);
                }
            }

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            boolean includeSecondary = ((Boolean) getParameter(INPUT_PARAM_INCLUDE_SECONDARY)).booleanValue();

            if (!queryBy.contains(CURRENT_KEY)) {
                addSchoolCriteria(m_studentCriteria, "", activeOnly, includeSecondary);
                if (tempTableInUse) {
                    addSchoolCriteria(m_transcriptCriteria, Transcript.REL_STUDENT + PATH_DELIMITER, activeOnly,
                            includeSecondary);
                    addSchoolCriteria(m_assessmentCriteria, StudentAssessment.REL_STUDENT + PATH_DELIMITER, activeOnly,
                            includeSecondary);
                    addSchoolCriteria(m_classAttendanceCriteria, StudentPeriodAttendance.REL_STUDENT + PATH_DELIMITER,
                            activeOnly, includeSecondary);
                    addSchoolCriteria(m_enrollmentCriteria, StudentEnrollment.REL_STUDENT + PATH_DELIMITER, activeOnly,
                            includeSecondary);
                    addSchoolCriteria(m_scheduleCriteria, StudentSchedule.REL_STUDENT + PATH_DELIMITER, activeOnly,
                            includeSecondary);
                }
            }

            if (!tempTableInUse) {
                /*
                 * Build a SubsetCriteriaWrapper from the student criteria
                 */
                ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class,
                        new String[] {X2BaseBean.COL_OID}, m_studentCriteria);

                Collection<String> identifiers = new LinkedList<String>();
                ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        String identifier = row[0].toString();
                        identifiers.add(identifier);
                    }
                } finally {
                    iterator.close();
                }

                m_studentCollectionCriteriaHelper = new CollectionCriteriaHelper(identifiers, getBroker());
                m_studentCollectionCriteriaHelper.applyToCriteria(Transcript.COL_STUDENT_OID,
                        (X2Criteria) m_transcriptCriteria);

                m_studentCollectionCriteriaHelper.applyToCriteria(StudentAssessment.COL_STUDENT_OID,
                        (X2Criteria) m_assessmentCriteria);
                m_studentCollectionCriteriaHelper.applyToCriteria(StudentPeriodAttendance.COL_STUDENT_OID,
                        (X2Criteria) m_classAttendanceCriteria);
                m_studentCollectionCriteriaHelper.applyToCriteria(StudentEnrollment.COL_STUDENT_OID,
                        (X2Criteria) m_enrollmentCriteria);
                m_studentCollectionCriteriaHelper.applyToCriteria(StudentSchedule.COL_STUDENT_OID,
                        (X2Criteria) m_scheduleCriteria);
            }
        }

        if (m_excludeEmpty) {
            Criteria columnCriteria = new Criteria();

            if (m_gradeTerm != null) {
                columnCriteria.addEqualTo(TranscriptColumnDefinition.COL_GRADE_TERM_OID, m_gradeTerm.getOid());
            }

            if (!m_rubricReport) {
                if (TranscriptColumnDefinition.GRADE_TYPE_TERM == m_reportType) {
                    columnCriteria.addEqualTo(TranscriptColumnDefinition.COL_REPORT_TYPE,
                            Integer.valueOf(TranscriptColumnDefinition.GRADE_TYPE_TERM));
                } else {
                    columnCriteria.addEqualTo(TranscriptColumnDefinition.COL_REPORT_TYPE,
                            Integer.valueOf(TranscriptColumnDefinition.GRADE_TYPE_PROGRESS));
                }
            }

            QueryByCriteria columnQuery = new QueryByCriteria(TranscriptColumnDefinition.class, columnCriteria);
            QueryIterator columnIterator = getBroker().getIteratorByQuery(columnQuery);

            Criteria emptyCriteria = null;
            try {
                int count = 0;
                while (columnIterator.hasNext()) {
                    TranscriptColumnDefinition column = (TranscriptColumnDefinition) columnIterator.next();

                    X2Criteria emptyColumnCriteria = new X2Criteria();
                    emptyColumnCriteria.addNotEmpty(column.getTranscriptBeanAttribute(),
                            getBroker().getPersistenceKey());
                    emptyColumnCriteria.addEqualTo(Transcript.COL_TRANSCRIPT_DEFINITION_OID,
                            column.getTranscriptDefinitionOid());

                    if (count == 0) {
                        emptyCriteria = emptyColumnCriteria;
                    } else {
                        /*
                         * This will always be true. emptyCriteria initialized in first iteration of
                         * loop.
                         * This "if" is only here to prevent potential NPE compiler warning.
                         */
                        if (emptyCriteria != null) {
                            emptyCriteria.addOrCriteria(emptyColumnCriteria);
                        }
                    }

                    count++;
                }
            } finally {
                columnIterator.close();
            }

            if (emptyCriteria != null) {
                m_transcriptCriteria.addAndCriteria(emptyCriteria);
            }
        }

        if (m_context != null) {
            m_transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, m_context.getOid());
        }

        if (getBooleanParameter(USE_REPORT_CARD_HIDE_IND_PARAM)) {
            m_transcriptCriteria.addNotEqualTo(Transcript.COL_REPORT_CARD_HIDE_IND, Boolean.TRUE);
            m_transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_HIDE_REPORT_CARD_INDICATOR, Boolean.TRUE);
        }

        if (getBooleanParameter(USE_TRANSCRIPT_HIDE_IND_PARAM)) {
            m_transcriptCriteria.addNotEqualTo(Transcript.COL_TRANSCRIPT_HIDE_IND, Boolean.TRUE);
            m_transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_HIDE_TRANSCRIPT_INDICATOR, Boolean.TRUE);
        }

        /*
         * If specified, only include grade levels defined in the input definition
         */
        String includedGradeLevels = (String) getParameter(INCLUDED_GRADE_LEVELS_PARAM);
        if (!StringUtils.isEmpty(includedGradeLevels)) {
            Collection<String> levels = StringUtils.convertDelimitedStringToList(includedGradeLevels, ',', true);
            m_transcriptCriteria.addIn(Transcript.COL_GRADE_LEVEL, levels);
        }

        /*
         * Only include schedule records from the current year's active schedules
         */
        m_scheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        /*
         * Load transcript records for the comment courses to load comments
         */
        m_termCommentCriteria = m_transcriptCriteria.copy(true, true, true);

        String courseNumbers = (String) getParameter(INPUT_PARAM_COMMENT_COURSES);
        if (!StringUtils.isEmpty(courseNumbers)) {
            List<String> numberList = StringUtils.convertDelimitedStringToList(courseNumbers, ',', true);
            m_termCommentCriteria.addIn(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER,
                    numberList);

            // If not including term comments, don't include the courses on the report (to prompt no
            // results)
            Boolean includeComments = (Boolean) getParameter(INPUT_PARAM_COMMENTS);
            if (includeComments == null || !includeComments.booleanValue()) {
                m_transcriptCriteria.addNotIn(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER,
                        numberList);
            }
        } else {
            addNoMatchCriteria(m_termCommentCriteria);
        }
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.GradeReportJavaSource#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        m_reportGrid = (ReportDataGrid) runStandardGradeReport();

        loadStudentYearlyBasedData();

        buildReports();

        addCustomParameters();

        boolean tempTableInUse = isCurrentQueryContainsUnion();
        if (m_studentCollectionCriteriaHelper != null && !tempTableInUse) {
            m_studentCollectionCriteriaHelper.cleanup();
        }

        if (!tempTableInUse) {
            getBroker().commitTransaction(); // end transaction to dispose of temp table created in
                                             // buildCriteria
        }

        return m_reportGrid;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.reports.GradeReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_numberFormatter = new DecimalFormat("0.#");
        m_homeroom = new HashMap<String, String>(2096);
        m_homeroomTeacherByStudent = new HashMap<String, String>(2096);
        m_homeroomTeacherByHomeroom = new HashMap<String, String>(2096);
        m_homeroomAddnlTeachers = new HashMap<String, String>(2096);
        m_homeroomAddnlTeachersNo = new HashMap<String, Integer>(2096);
        m_gradeLevel = new HashMap<String, String>(2096);

        loadCriterionDescriptors();
        loadHomeRoomAdditionalTeachers();

        super.initialize();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see
     *      com.x2dev.sis.tools.reports.GradeReportJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_isInStaffView = userData.getApplicationContext() == ApplicationContext.STAFF;
    }

    /**
     * Adds additional parameters used by the report.
     */
    private void addCustomParameters() {
        /*
         * Load attendance
         */
        Map<String, BigDecimal[]> attendances = new HashMap<String, BigDecimal[]>();
        attendances.putAll(loadDailyAttendance(m_context.getStartDate(), m_context.getEndDate()));

        boolean includePeriodAttendance = getBooleanParameter(INPUT_PARAM_ATTENDANCE);
        if (includePeriodAttendance) {
            attendances.putAll(loadCourseAbsences(m_context, getSchool()));
        }
        addParameter(PARAM_ATTENDANCES, attendances);

        /*
         * Set general parameters
         */
        addParameter(PARAM_CONTEXT, m_context);
        addParameter(PARAM_GRADE_TERM_DATES, loadGradeTermDates(getSchool(), m_context));
        addParameter(PARAM_HOMEROOM, m_homeroom);
        addParameter(PARAM_HOMEROOM_TEACHER, m_homeroomTeacherByStudent);
        addParameter(PARAM_ADDNL_TEACHERS, m_homeroomAddnlTeachers);
        addParameter(PARAM_GRADE_LEVEL, m_gradeLevel);

        if (getBooleanParameter(INPUT_INCLUDE_SCHOOL_MESSAGE_PARAM)) {
            addParameter(PARAM_MESSAGE, getParameter(MESSAGE_PARAM));
        }

        /*
         * Set layout parameters
         */
        addParameter(PARAM_SIGNATURE, Boolean.valueOf(getBooleanParameter(INPUT_PARAM_SIGNATURE)));
        addParameter(PARAM_PERFORMANCE_SCALE, Boolean.valueOf(getBooleanParameter(INPUT_PARAM_PERFORMANCE_SCALE)));
        addParameter(PARAM_BOTH_SIDES, Boolean.valueOf(getBooleanParameter(INPUT_PARAM_PRINT_BOTH)));
        addParameter(PARAM_TEACHER_SIGNATURE, Boolean.valueOf(getBooleanParameter(INPUT_PARAM_TEACHER_SIGNATURE)));
        addParameter(PARAM_GPA, Boolean.valueOf(getBooleanParameter(INPUT_PARAM_GPA)));
        addParameter(PARAM_INFO_PAGE, Boolean.valueOf(getBooleanParameter(INPUT_PARAM_INFO_PAGE)));
        addParameter(PARAM_FINAL_MARK, Boolean.valueOf(getBooleanParameter(INPUT_PARAM_FINAL_MARK)));
        addParameter(PARAM_ATTENDANCE, Boolean.valueOf(includePeriodAttendance));
        addParameter(PARAM_FINAL_EXAM, Boolean.valueOf(getBooleanParameter(INPUT_PARAM_FINAL_EXAM)));
        addParameter(PARAM_FIRST_NAME_TO_DISPLAY,
                Boolean.valueOf(!VALUE_LEGAL.equals(getParameter(INPUT_PARAM_FIRST_NAME_TO_DISPLAY))));
        addParameter(PARAM_LAST_NAME_TO_DISPLAY,
                Boolean.valueOf(!VALUE_LEGAL.equals(getParameter(INPUT_PARAM_LAST_NAME_TO_DISPLAY))));

        /*
         * Build ID of subreport
         */
        String reportId = (String) getParameter(INPUT_PARAM_REPORT_ID);
        String reportType = (String) getParameter(INPUT_PARAM_REPORT_TYPE);
        if (!StringUtils.isEmpty(reportId) && !StringUtils.isEmpty(reportType)) {
            String languageCode = getBooleanParameter(INPUT_PARAM_IS_FRENCH) ? FRENCH_REPORT_SUFFIX : "";
            String size = getBooleanParameter(INPUT_PARAM_LEGAL_SIZE) ? LEGAL_SIZE_SUFFIX : "";

            String formatId = reportId + reportType + languageCode + size;
            setFormatId(formatId);
        }

        /*
         * Load additional parameters
         */
        loadTermCommentMap();
    }

    /**
     * Adds the criteria to find the students to include for the current school.
     *
     * @param criteria Criteria
     * @param prefix String
     * @param activeOnly boolean
     * @param includeSecondary boolean
     */
    private void addSchoolCriteria(Criteria criteria, String prefix, boolean activeOnly, boolean includeSecondary) {
        X2Criteria studentCriteria = new X2Criteria();

        if (isSchoolContext()) {
            studentCriteria.addEqualTo(prefix + m_helper.getSchoolOidField(), getSchool().getOid());
        } else if (!isSchoolContext()) {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            if (isCurrentQueryContainsUnion()) {
                SubQuery schoolQuery = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, schoolCriteria);
                studentCriteria.addIn(prefix + m_helper.getSchoolOidField(), schoolQuery);
            } else {
                studentCriteria.addAndCriteria(schoolCriteria.copyWithAdjustedPath(prefix +
                        m_helper.getSchoolRelationship(), prefix + m_helper.getSchoolOidField()));
            }
        }

        if (activeOnly) {
            studentCriteria.addAndCriteria(m_helper.getActiveStudentCriteria(prefix));
        }

        if (includeSecondary) {
            studentCriteria.addOrCriteria(StudentManager.getSecondaryStudentCriteria(prefix +
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER,
                    getCurrentContext().getOid(), getSchool().getOid(), null, null,
                    getBroker().getPersistenceKey()));
        }

        criteria.addAndCriteria(studentCriteria);
    }

    /**
     * Processes the report grid. Only the first rubric score is included in the report and the
     * display for additional
     * teachers is built.
     */
    private void buildReports() {
        String lastTranscriptOid = "";

        m_reportGrid.beforeTop();
        while (m_reportGrid.next()) {
            Transcript transcript = (Transcript) m_reportGrid.get(TranscriptReportGrid.COL_TRANSCRIPT_HEADER);

            if (lastTranscriptOid.equals(transcript.getOid())) {
                // Remove all rubric rows except the first
                m_reportGrid.deleteRow();
            } else {
                lastTranscriptOid = transcript.getOid();
                translateRubricGrade(m_reportGrid);
            }
        }

        m_reportGrid.beforeTop();
    }

    /**
     * Returns array of zeros.
     *
     * @param size Array size.
     *
     * @return BigDecimal[]
     */
    private BigDecimal[] getEmptyArray(int size) {
        BigDecimal[] result = new BigDecimal[size + 1];

        for (int i = 0; i <= size; ++i) {
            result[i] = new BigDecimal(0);
        }

        return result;
    }

    /**
     * Increases the value by the specified index.
     *
     * @param array BigDecimal[]
     * @param index int
     * @param value BigDecimal
     */
    private void increaseArrayValue(BigDecimal[] array, int index, BigDecimal value) {
        if (value == null) {
            value = new BigDecimal(1);
        }

        if (array[index] == null) {
            array[index] = value;
        } else {
            array[index] = array[index].add(value);
        }
    }

    /**
     * Loads section absences.
     *
     * @param context DistrictSchoolYearContext
     * @param school School
     * @return Map of BigDecimal arrays keyed by student OID.
     */
    private Map<String, BigDecimal[]> loadCourseAbsences(DistrictSchoolYearContext context, School school) {
        Map<String, BigDecimal[]> result = new HashMap<String, BigDecimal[]>();

        BigDecimal[] emptyAttendances = getEmptyArray(TOTAL_TERM_INDEX);

        /*
         * Query section absences into a map keyed to the student OID
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addBetween(StudentPeriodAttendance.COL_DATE, context.getStartDate(), context.getEndDate());
        criteria.addEqualTo(StudentPeriodAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

        if (!isCurrentQueryContainsUnion()) {
            SubQuery students = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentCriteria);
            criteria.addIn(StudentAttendance.COL_STUDENT_OID, students);
        } else {
            m_studentCollectionCriteriaHelper.applyToCriteria(StudentPeriodAttendance.COL_STUDENT_OID, criteria);
        }

        QueryByCriteria query = new QueryByCriteria(StudentPeriodAttendance.class, criteria);
        query.addOrderByAscending(StudentPeriodAttendance.COL_STUDENT_OID);
        query.addOrderByAscending(
                StudentPeriodAttendance.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.COL_SCHOOL_COURSE_OID);
        query.addOrderByAscending(StudentPeriodAttendance.COL_DATE);

        Map<String, Collection<GradeTermDate>> gradeTermDatesByYear = new HashMap<String, Collection<GradeTermDate>>();

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentPeriodAttendance attendance = (StudentPeriodAttendance) iterator.next();
                Section section = attendance.getMasterSchedule();

                String courseOid = section.getSchoolCourseOid();
                String contextOid = section.getSchedule().getDistrictContextOid();

                Collection<GradeTermDate> gradeTermDates = gradeTermDatesByYear.get(contextOid);
                if (gradeTermDates == null) {
                    gradeTermDates = GradesManager.getGradeTermDates(school.getOid(), contextOid, getBroker());
                    gradeTermDatesByYear.put(contextOid, gradeTermDates);
                }

                GradeTerm term = null;
                for (GradeTermDate termDate : gradeTermDates) {
                    if (!termDate.getStartDate().after(attendance.getDate())
                            && !termDate.getEndDate().before(attendance.getDate())) {
                        term = termDate.getGradeTerm();
                        break;
                    }
                }

                if (term != null) {
                    int termNum = term.getGradeTermNum() - 1;

                    BigDecimal[] array =
                            result.get(attendance.getStudentOid() + DELIMITER + courseOid + ABSENT_POSTFIX);
                    if (array == null) {
                        array = emptyAttendances.clone();
                        result.put(attendance.getStudentOid() + DELIMITER + courseOid + ABSENT_POSTFIX, array);
                    }

                    increaseArrayValue(array, termNum, null);
                    increaseArrayValue(array, TOTAL_TERM_INDEX, null);
                }
            }
        } finally {
            iterator.close();
        }

        return result;
    }

    /**
     * Loads the lookup maps of the rubric performances by criterion OID.
     */
    private void loadCriterionDescriptors() {
        QueryByCriteria query = new QueryByCriteria(RubricPerformanceDescriptor.class);
        query.addOrderByAscending(RubricPerformanceDescriptor.COL_RUBRIC_CRITERION_OID);
        query.addOrderByAscending(RubricPerformanceDescriptor.COL_ID);

        m_criterionDescriptorsByRating = getBroker().getNestedMapByQuery(query,
                RubricPerformanceDescriptor.COL_RUBRIC_CRITERION_OID,
                RubricPerformanceDescriptor.COL_ID,
                2048,
                32);
    }

    /**
     * Loads the lookup map of Homeroom Additional Teachers to Homeroom.
     */
    private void loadHomeRoomAdditionalTeachers() {
        /*
         * Query SisStaff where Homeroom is not null Or Homeroom2 is not null
         * And School is Staff School or Staff School Association (for the current year)
         * And Staff is Active
         */
        X2Criteria criteriaHomeroom = new X2Criteria();
        criteriaHomeroom.addNotNull(SisStaff.COL_HOMEROOM);
        X2Criteria criteriaHomeroom2 = new X2Criteria();
        criteriaHomeroom2.addNotNull(SisStaff.COL_HOMEROOM2);
        criteriaHomeroom2.addOrCriteria(criteriaHomeroom);

        X2Criteria criteriaSchools1a = new X2Criteria();
        criteriaSchools1a.addEqualTo(StaffSchoolAssociation.COL_SCHOOL_OID, getSchool().getOid());
        criteriaSchools1a.addEqualTo(StaffSchoolAssociation.COL_DISTRICT_CONTEXT_OID,
                getOrganization().getCurrentContextOid());
        SubQuery subSchools1Staff =
                new SubQuery(StaffSchoolAssociation.class, StaffSchoolAssociation.COL_STAFF_OID, criteriaSchools1a);
        X2Criteria criteriaSchools1 = new X2Criteria();
        criteriaSchools1.addIn(X2BaseBean.COL_OID, subSchools1Staff);
        X2Criteria criteriaSchools2 = new X2Criteria();
        criteriaSchools2.addEqualTo(SisStaff.REL_SCHOOL + PATH_DELIMITER + X2BaseBean.COL_OID, getSchool().getOid());
        criteriaSchools2.addOrCriteria(criteriaSchools1);

        X2Criteria criteriaStaff = new X2Criteria();
        criteriaStaff.addNotEqualTo(SisStaff.COL_STATUS, SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
        criteriaStaff.addAndCriteria(criteriaHomeroom2);
        criteriaStaff.addAndCriteria(criteriaSchools2);

        // Load map by Homeroom of Staff Names meeting the criteria defined above, for Homeroom
        // should not be Primary Teacher
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(CONST_ALIAS_STF_PRIM_HR_TEACHER);
        String primHomeroomTeacherField = "";
        if (field != null) {
            primHomeroomTeacherField = field.getJavaName();
        }
        String[] columns = new String[] {SisStaff.COL_HOMEROOM,
                primHomeroomTeacherField,
                SisStaff.COL_HOMEROOM2,
                SisStaff.COL_NAME_VIEW};
        ReportQueryByCriteria staffQuery = new ReportQueryByCriteria(SisStaff.class, columns, criteriaStaff);
        staffQuery.addOrderByAscending(SisStaff.COL_NAME_VIEW);

        ReportQueryIterator staffIterator = getBroker().getReportQueryIteratorByQuery(staffQuery);
        try {
            while (staffIterator.hasNext()) {
                Object[] data = (Object[]) staffIterator.next();
                String homeroom = (data[0] == null) ? null : data[0].toString();
                String primHomeroomTeacher = (data[1] == null) ? "0" : data[1].toString();
                String homeroom2 = (data[2] == null) ? null : data[2].toString();
                String staffNameView = (data[3] == null) ? "" : data[3].toString();

                if (homeroom != null) {
                    if ("1".equals(primHomeroomTeacher) && !m_homeroomTeacherByHomeroom.containsKey(homeroom)) {
                        m_homeroomTeacherByHomeroom.put(homeroom, staffNameView);
                    } else if (!("1".equals(primHomeroomTeacher))) {
                        String addnlTeachers = m_homeroomAddnlTeachers.get(homeroom);
                        Integer addnlTeachersNo = m_homeroomAddnlTeachersNo.get(homeroom);
                        if (addnlTeachersNo == null) {
                            addnlTeachersNo = Integer.valueOf(1);
                            m_homeroomAddnlTeachersNo.put(homeroom, addnlTeachersNo);
                            m_homeroomAddnlTeachers.put(homeroom, staffNameView);
                        } else if ((addnlTeachersNo.intValue() < 3)
                                && (!(addnlTeachers.contains(staffNameView)))) {
                            addnlTeachersNo = Integer.valueOf(addnlTeachersNo.intValue() + 1);
                            m_homeroomAddnlTeachersNo.put(homeroom, addnlTeachersNo);
                            addnlTeachers += "; " + staffNameView;
                            m_homeroomAddnlTeachers.put(homeroom, addnlTeachers);
                        }
                    }
                }

                if (homeroom2 != null) {
                    String addnlTeachers = m_homeroomAddnlTeachers.get(homeroom2);
                    Integer addnlTeachersNo = m_homeroomAddnlTeachersNo.get(homeroom2);
                    if (addnlTeachersNo == null) {
                        addnlTeachersNo = Integer.valueOf(1);
                        m_homeroomAddnlTeachersNo.put(homeroom2, addnlTeachersNo);
                        m_homeroomAddnlTeachers.put(homeroom2, staffNameView);
                    } else if ((addnlTeachersNo.intValue() < 3)
                            && (!(addnlTeachers.contains(staffNameView)))) {
                        addnlTeachersNo = Integer.valueOf(addnlTeachersNo.intValue() + 1);
                        m_homeroomAddnlTeachersNo.put(homeroom2, addnlTeachersNo);
                        addnlTeachers += "; " + staffNameView;
                        m_homeroomAddnlTeachers.put(homeroom2, addnlTeachers);
                    }
                }
            }
        } finally {
            staffIterator.close();
        }
    }

    /**
     * Loads daily attendance totals by month.
     *
     * @param startDate Start date.
     * @param endDate End date.
     * @return Map of BigDecimal arrays keyed by student oid.
     */
    private Map<String, BigDecimal[]> loadDailyAttendance(PlainDate startDate, PlainDate endDate) {
        Map<String, BigDecimal[]> result = new HashMap<String, BigDecimal[]>();

        Calendar calendar = Calendar.getInstance();
        BigDecimal[] emptyAttendances = getEmptyArray(TOTAL_MONTH_INDEX);

        // Query out daily attendance records into a map keyed to the student OID
        X2Criteria criteria = new X2Criteria();
        criteria.addBetween(StudentAttendance.COL_DATE, startDate, endDate);

        if (!isCurrentQueryContainsUnion()) {
            m_studentCollectionCriteriaHelper.applyToCriteria(StudentAttendance.COL_STUDENT_OID, criteria);
        } else {
            SubQuery students = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentCriteria);
            criteria.addIn(StudentAttendance.COL_STUDENT_OID, students);
        }

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        query.addOrderByAscending(StudentAttendance.COL_STUDENT_OID);
        query.addOrderByAscending(StudentAttendance.COL_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentAttendance attendance = (StudentAttendance) iterator.next();
                String studentOid = attendance.getStudentOid();

                BigDecimal[] absents = result.get(studentOid + ABSENT_POSTFIX);
                if (absents == null) {
                    absents = emptyAttendances.clone();
                }

                BigDecimal[] late = result.get(studentOid + LATE_POSTFIX);
                if (late == null) {
                    late = emptyAttendances.clone();
                }

                calendar.setTime(attendance.getDate());
                int month = calendar.get(Calendar.MONTH);

                if (attendance.getAbsentIndicator()) {
                    increaseArrayValue(absents, month, attendance.getPortionAbsent());
                    increaseArrayValue(absents, TOTAL_MONTH_INDEX, attendance.getPortionAbsent());
                }

                if (attendance.getTardyIndicator()) {
                    increaseArrayValue(late, month, null);
                    increaseArrayValue(late, TOTAL_MONTH_INDEX, null);
                }

                result.put(studentOid + ABSENT_POSTFIX, absents);
                result.put(studentOid + LATE_POSTFIX, late);
            }
        } finally {
            iterator.close();
        }

        /*
         * For students with perfect attendance, attach them with empty absent and late array.
         */
        SubQuery allStudents = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentCriteria);
        Collection<String> allStudentOids = getBroker().getSubQueryCollectionByQuery(allStudents);
        for (String studentOid : allStudentOids) {
            if (!result.containsKey(studentOid + ABSENT_POSTFIX)) {
                result.put(studentOid + ABSENT_POSTFIX, getEmptyArray(TOTAL_MONTH_INDEX));
            }

            if (!result.containsKey(studentOid + LATE_POSTFIX)) {
                result.put(studentOid + LATE_POSTFIX, getEmptyArray(TOTAL_MONTH_INDEX));
            }
        }

        return result;
    }

    /**
     * Loads array of gradeTermDates using specific school and context.
     *
     * @param school School
     * @param context DistrictSchoolYearContext
     * @return GradeTermDate[]
     */
    private GradeTermDate[] loadGradeTermDates(School school, DistrictSchoolYearContext context) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(GradeTermDate.COL_SCHOOL_OID, school.getOid());
        criteria.addEqualTo(GradeTermDate.COL_DISTRICT_CONTEXT_OID, context.getOid());
        criteria.addEqualTo(GradeTermDate.REL_GRADE_TERM + PATH_DELIMITER +
                GradeTerm.COL_GRADE_TERM_DEFINITION_OID, m_gradeTerm.getGradeTermDefinitionOid());

        QueryByCriteria query = new QueryByCriteria(GradeTermDate.class, criteria);
        query.addOrderByAscending(GradeTermDate.COL_START_DATE);

        Collection<GradeTermDate> result = getBroker().getCollectionByQuery(query);

        return result.toArray(new GradeTermDate[16]);
    }

    /**
     * Load student yearly based data such as homeroom and homeroom teachers.
     */
    private void loadStudentYearlyBasedData() {
        ReportQueryByCriteria studentQuery = null;
        if (m_context.getOid().equals(getOrganization().getRootOrganization().getCurrentContextOid())) {
            String[] columns = new String[] {X2BaseBean.COL_OID, SisStudent.COL_HOMEROOM, SisStudent.COL_GRADE_LEVEL};
            studentQuery = new ReportQueryByCriteria(SisStudent.class, columns, m_studentCriteria);
        } else {
            X2Criteria studentCriteria = new X2Criteria();
            studentCriteria.addEqualTo(StudentContextAttributes.COL_CONTEXT_OID, m_context.getOid());

            if (isCurrentQueryContainsUnion()) {
                SubQuery students = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentCriteria);
                studentCriteria.addIn(StudentContextAttributes.COL_STUDENT_OID, students);
            } else {
                m_studentCollectionCriteriaHelper.applyToCriteria(StudentContextAttributes.COL_STUDENT_OID,
                        studentCriteria);
            }

            String[] columns =
                    new String[] {StudentContextAttributes.COL_STUDENT_OID, StudentContextAttributes.COL_HOMEROOM,
                            SisStudent.COL_HOMEROOM_TEACHER, StudentContextAttributes.COL_GRADE_LEVEL};
            studentQuery = new ReportQueryByCriteria(StudentContextAttributes.class, columns, studentCriteria);
        }
        ReportQueryIterator studentIterator = getBroker().getReportQueryIteratorByQuery(studentQuery);
        try {
            while (studentIterator.hasNext()) {
                Object[] data = (Object[]) studentIterator.next();
                String studentOid = data[0].toString();
                String homeroom = (data[1] == null) ? null : data[1].toString();
                String gradeLevel = (data[2] == null) ? null : data[2].toString();

                m_homeroom.put(studentOid, homeroom);
                m_homeroomTeacherByStudent.put(studentOid, m_homeroomTeacherByHomeroom.get(homeroom));
                m_gradeLevel.put(studentOid, gradeLevel);
            }
        } finally {
            studentIterator.close();
        }
    }

    /**
     * Load behavior comments, if necessary. Comments for the current term are loaded into a Map
     * keyed to the
     * student OID.
     */
    private void loadTermCommentMap() {
        Map<String, String> commentMap = new HashMap<String, String>(2048);

        Boolean includeComments = (Boolean) getParameter(INPUT_PARAM_COMMENTS);
        if (Boolean.TRUE.equals(includeComments)) {
            String column = COMMENT_COLUMNS.get(m_gradeTerm.getGradeTermNum() - 1);
            if (!StringUtils.isEmpty(column)) {
                TranscriptReportGrid transcriptGrid = new TranscriptReportGrid(m_termCommentCriteria,
                        new String[] {X2BaseBean.COL_OID},
                        false,
                        false,
                        false,
                        getOrganization(),
                        getBroker());

                while (transcriptGrid.next()) {
                    String studentOid = transcriptGrid.getStudent().getOid();
                    String comment = (String) transcriptGrid.get(column);

                    commentMap.put(studentOid, comment);
                }
            }
        }

        addParameter(PARAM_TERM_COMMENT_MAP, commentMap);
    }

    /**
     * Translates the provided score to the points based on the criterion's performance descriptors.
     * The numeric value
     * is used to display the "approaching/meeting/exceeding" expectations.
     *
     * @param grid ReportDataGrid
     */
    private void translateRubricGrade(ReportDataGrid grid) {
        RubricCriterion criterion = (RubricCriterion) grid.get(RubricTranscriptReportGrid.COL_CRITERION);

        if (criterion != null) {
            Map<String, RubricPerformanceDescriptor> ratingMap = m_criterionDescriptorsByRating.get(criterion.getOid());

            if (ratingMap != null) {
                for (String column : COLUMNS) {
                    String columnHeader = TranscriptReportGrid.RUBRIC_FIELD_PREFIX + column;
                    String score = (String) grid.get(columnHeader);

                    // Track original grade for display on the intermediate report
                    grid.set(columnHeader + GRADE_POSTFIX, score);

                    if (!StringUtils.isEmpty(score)) {
                        RubricPerformanceDescriptor descriptor = ratingMap.get(score);
                        if (descriptor != null) {
                            grid.set(columnHeader, m_numberFormatter.format(descriptor.getPoints()));
                        }
                    }
                }
            }
        }
    }
}
