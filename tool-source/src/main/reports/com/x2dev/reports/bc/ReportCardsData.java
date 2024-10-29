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
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import static com.x2dev.sis.tools.reports.TranscriptReportGrid.COL_STUDENT;
import static com.x2dev.sis.tools.reports.TranscriptReportGrid.COL_TRANSCRIPT_HEADER;
import com.follett.fsc.core.framework.persistence.CollectionCriteriaHelper;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.framework.persistence.X2Query;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
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
import net.sf.jasperreports.engine.JRDataSource;
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
 * @author Follett Software Company
 */
public class ReportCardsData extends GradeReportJavaSource {
    private static final long serialVersionUID = 1L;

    /*
     * Input parameters.
     */
    private static final String INPUT_PARAM_COMMENT_COURSES = "commentCourses";
    private static final String INPUT_PARAM_DISPLAY_BLENDED = "displayBlended";
    private static final String INPUT_PARAM_REPORT_ID = "reportId";
    private static final String INPUT_PARAM_REPORT_TYPE = "reportCardType";
    private static final String INPUT_PARAM_IS_FRENCH = "isFrench";
    private static final String INPUT_INCLUDE_SCHOOL_MESSAGE_PARAM = "includeSchoolMessage";
    private static final String INPUT_PARAM_INCLUDE_SECONDARY = "secondaryStudent";
    private static final String INPUT_PARAM_SIGNATURE = "includeSignature";
    private static final String INPUT_PARAM_PERFORMANCE_SCALE = "includePerformance";
    private static final String INPUT_PARAM_COMMENTS = "includeTermComments";
    private static final String INPUT_PARAM_INFO_PAGE = "includeInfoPage";
    private static final String INPUT_PARAM_ATTENDANCE = "includeAttendance";
    private static final String INPUT_PARAM_BEHAVIOUR = "includeBehaviour";
    private static final String INPUT_PARAM_BEHAVIOR_COURSES = "behaviorCourse";
    private static final String INPUT_PARAM_FIRST_NAME_TO_DISPLAY = "firstNameType";
    private static final String INPUT_PARAM_LAST_NAME_TO_DISPLAY = "lastNameType";
    private static final String INPUT_PARAM_LEGAL_SIZE = "printLegalSize";

    /*
     * Report parameters
     */
    private static final String PARAM_ADDITIONAL_TEACHERS = "additionalTeachers";
    private static final String PARAM_ATTENDANCES = "attendances";
    private static final String PARAM_BEHAVIOR_COMMENT_MAP = "behaviorCommentMap";
    private static final String PARAM_CONTEXT = "context";
    private static final String PARAM_GRADE_LEVEL = "gradeLevel";
    private static final String PARAM_GRADE_TERM_DATES = "gradeTermDates";
    private static final String PARAM_HOMEROOM = "homeroom";
    private static final String PARAM_HOMEROOM_TEACHER = "homeroomTeacher";
    private static final String PARAM_MESSAGE = "reportMessage";
    private static final String PARAM_SIGNATURE = "signature";
    private static final String PARAM_PERFORMANCE_SCALE = "performanceScale";
    private static final String PARAM_INFO_PAGE = "includeInfoPage";
    private static final String PARAM_FIRST_NAME_TO_DISPLAY = "firstNameType";
    private static final String PARAM_LAST_NAME_TO_DISPLAY = "lastNameType";
    private static final String PARAM_TERM_COMMENT_MAP = "termCommentMap";

    private static final String PARAM_GRADE_TERM = "gradeTerm";

    /*
     * Column constants
     */
    private static final String ABSENT_POSTFIX = "_absent";
    private static final List<String> COLUMNS = Arrays.asList(new String[] {"Q1 Grade",
            "Q2 Grade",
            "Q3 Grade",
            "Q4 Grade",
            "Tri 1",
            "Tri 2",
            "Tri 3",
            "Tri 4",
            "Final"});

    private static final List<String> COMMENT_COLUMNS = Arrays.asList(new String[] {"Q1 Com|Tri 1 Com",
            "Q2 Com|Tri 2 Com",
            "Q3 Com|Tri 3 Com",
            "Q4 Com|Tri 4 Com"});

    /*
     * Report ID constants
     */
    private static final String FRENCH_REPORT_SUFFIX = "-F";
    private static final String LEGAL_SIZE_SUFFIX = "-L";

    /*
     * Alias fields
     */
    private static final String BLENDED_MARK_ALIAS = "trn-blended-mark";
    private static final String TRAX_ALIAS = "trn-trax-override";

    /*
     * General constants
     */
    private static final String DELIMITER = "_";
    private static final String LATE_POSTFIX = "_late";
    private static final String NUM_POSTFIX = " Num";
    private static final int TOTAL_MONTH_INDEX = 12;
    private static final int TOTAL_TERM_INDEX = 4;
    private static final String TRAX_COMPLETE_SUFFIX = "(Q)";
    private static final String VALUE_LEGAL = "legal";

    /*
     * Members
     */
    private Map<String, Collection<String>> m_additionalTeachers;
    private Criteria m_behaviorCriteria;
    private Map<String, Collection<TranscriptColumnDefinition>> m_colDefMap;
    private DecimalFormat m_decimalFormat;
    private boolean m_displayBlended;
    private Map<String, String> m_gradeLevel;
    private GradesManager m_gradesManager;
    private Map<String, String> m_homeroomTeacher;
    private Map<String, String> m_homeroom;
    private ReportDataGrid m_reportGrid;
    private Criteria m_termCommentCriteria;
    private GradeTermDate m_termDate;
    private boolean m_isInStaffView;

    private CollectionCriteriaHelper m_studentCollectionCriteriaHelper;

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
     * @see com.x2dev.sis.tools.reports.GradeReportJavaSource#addDefaultParameters()
     */
    @Override
    protected void addDefaultParameters() {
        super.addDefaultParameters();
        if (m_gradeTerm != null) {
            addParameter(PARAM_GRADE_TERM, m_gradeTerm);
        }
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
                    X2BaseBean.COL_OID);
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
         * Load transcript records for the behavior courses to load comments
         */
        m_behaviorCriteria = m_transcriptCriteria.copy(true, true, true);

        String courseNumbers = (String) getParameter(INPUT_PARAM_BEHAVIOR_COURSES);
        if (!StringUtils.isEmpty(courseNumbers)) {
            List<String> numberList = StringUtils.convertDelimitedStringToList(courseNumbers, ',', true);
            m_behaviorCriteria.addIn(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER,
                    numberList);
            m_transcriptCriteria.addNotIn(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER,
                    numberList);
        } else {
            addNoMatchCriteria(m_behaviorCriteria);
        }

        /*
         * Load transcript records for the comment courses to load comments
         */
        m_termCommentCriteria = m_transcriptCriteria.copy(true, true, true);

        String commentCourses = (String) getParameter(INPUT_PARAM_COMMENT_COURSES);
        if (!StringUtils.isEmpty(commentCourses)) {
            List<String> numberList = StringUtils.convertDelimitedStringToList(commentCourses, ',', true);
            m_termCommentCriteria.addIn(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER,
                    numberList);
            m_transcriptCriteria.addNotIn(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER,
                    numberList);
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
        // Load students yearly based data
        loadStudentYearlyBasedData();

        m_reportGrid = (ReportDataGrid) runStandardGradeReport();

        initColDefMap(m_reportGrid);

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
     * Gets the transcript sort order.
     *
     * @return String[]
     * @see com.x2dev.sis.tools.reports.GradeReportJavaSource#getTranscriptSortOrder()
     */
    @Override
    protected String[] getTranscriptSortOrder() {
        String[] sortArray = new String[3];

        sortArray[0] = Transcript.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME;

        if (m_context != null) {
            sortArray[1] = Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_REPORT_SEQUENCE_NUMBER;
        } else {
            sortArray[1] =
                    Transcript.REL_DISTRICT_CONTEXT + PATH_DELIMITER + SisDistrictSchoolYearContext.COL_SCHOOL_YEAR;
        }

        sortArray[2] = Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER;

        return sortArray;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.reports.GradeReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_additionalTeachers = new HashMap<String, Collection<String>>(2096);
        m_homeroomTeacher = new HashMap<String, String>(2096);
        m_homeroom = new HashMap<String, String>(2096);
        m_gradeLevel = new HashMap<String, String>(2096);
        m_colDefMap = new HashMap<String, Collection<TranscriptColumnDefinition>>();
        m_decimalFormat = new DecimalFormat("0.####");
        m_displayBlended = getBooleanParameter(INPUT_PARAM_DISPLAY_BLENDED);
        m_gradesManager = new GradesManager(getBroker());

        if (!isCurrentQueryContainsUnion()) {
            getBroker().beginTransaction();
        }

        super.initialize();

        if (m_gradeTerm != null && isSchoolContext()) {
            m_termDate = GradesManager.getGradeTermDate(m_gradeTerm.getOid(),
                    getSchool().getOid(),
                    m_context.getOid(),
                    getBroker());
        }
    }

    /**
     * Run standard grade report.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.GradeReportJavaSource#runStandardGradeReport()
     */
    @Override
    protected JRDataSource runStandardGradeReport() throws Exception {
        TranscriptReportGrid grid = null;

        String[] studentSortOrder = getUserSortOrderAsStringArray((String) getParameter(STUDENT_SORT_PARAM));

        if (StringUtils.isNumeric(m_startGrade) && StringUtils.isNumeric(m_endGrade)) {
            grid = new TranscriptReportGridBC(m_transcriptCriteria,
                    studentSortOrder,
                    getTranscriptSortOrder(),
                    m_convertNumeric,
                    m_convertReference,
                    m_localizeReference,
                    Integer.parseInt(m_startGrade),
                    Integer.parseInt(m_endGrade),
                    getOrganization(),
                    getBroker());
        } else if (m_rubricReport) {
            grid = new RubricTest(m_transcriptCriteria,
                    studentSortOrder,
                    getTranscriptSortOrder(),
                    m_convertNumeric,
                    m_convertReference,
                    m_localizeReference,
                    true,
                    getOrganization(),
                    getBroker());
        } else {
            grid = new TranscriptReportGridBC(m_transcriptCriteria,
                    studentSortOrder,
                    getTranscriptSortOrder(),
                    m_convertNumeric,
                    m_convertReference,
                    m_localizeReference,
                    false,
                    getOrganization(),
                    getBroker());
        }

        if (getBooleanParameter(ITERATE_GRID_PARAM)) {
            iterateGrid(grid);
        }

        addDefaultParameters();

        return grid;
    }

    /**
     * The Class RubricTest.
     */
    private class RubricTest extends RubricTranscriptReportGrid {

        /**
         * Instantiates a new rubric test.
         *
         * @param transcriptCriteria Criteria
         * @param studentSortOrder String[]
         * @param transcriptSortOrder String[]
         * @param convertNumeric boolean
         * @param convertReference boolean
         * @param localizeReference boolean
         * @param schoolGradesOnly boolean
         * @param organization Organization
         * @param broker X2Broker
         */
        public RubricTest(Criteria transcriptCriteria,
                String[] studentSortOrder, String[] transcriptSortOrder,
                boolean convertNumeric, boolean convertReference,
                boolean localizeReference, boolean schoolGradesOnly,
                Organization organization, X2Broker broker) {
            super(transcriptCriteria, studentSortOrder, transcriptSortOrder,
                    convertNumeric, convertReference, localizeReference,
                    schoolGradesOnly, organization, broker);
        }

        /**
         * @see com.x2dev.sis.tools.reports.TranscriptReportGrid#buildTranscriptQuery(com.follett.fsc.core.framework.persistence.X2Query)
         */
        @Override
        protected void buildTranscriptQuery(X2Query transcriptQuery) {
            super.buildTranscriptQuery(transcriptQuery);
            transcriptQuery.setDistinct(true);
        }
    }

    /**
     * Override TranscriptReportGrid.
     */
    private class TranscriptReportGridBC extends TranscriptReportGrid {

        /**
         * Instantiates a new transcript report grid BC.
         *
         * @param transcriptCriteria Criteria
         * @param studentSortOrder String[]
         * @param transcriptSortOrder String[]
         * @param convertNumeric boolean
         * @param convertReference boolean
         * @param localizeReference boolean
         * @param schoolGradesOnly boolean
         * @param organization Organization
         * @param broker X2Broker
         */
        public TranscriptReportGridBC(Criteria transcriptCriteria,
                String[] studentSortOrder, String[] transcriptSortOrder,
                boolean convertNumeric, boolean convertReference,
                boolean localizeReference, boolean schoolGradesOnly,
                Organization organization, X2Broker broker) {
            super(transcriptCriteria, studentSortOrder, transcriptSortOrder,
                    convertNumeric, convertReference, localizeReference,
                    schoolGradesOnly, organization, broker);
        }

        /**
         * Instantiates a new transcript report grid BC.
         *
         * @param transcriptCriteria Criteria
         * @param studentSortOrder String[]
         * @param transcriptSortOrder String[]
         * @param convertNumeric boolean
         * @param convertReference boolean
         * @param localizeReference boolean
         * @param specificStartGrade int
         * @param specificEndGrade int
         * @param organization Organization
         * @param broker X2Broker
         */
        public TranscriptReportGridBC(Criteria transcriptCriteria,
                String[] studentSortOrder, String[] transcriptSortOrder,
                boolean convertNumeric, boolean convertReference,
                boolean localizeReference, int specificStartGrade,
                int specificEndGrade, Organization organization, X2Broker broker) {
            super(transcriptCriteria, studentSortOrder, transcriptSortOrder,
                    convertNumeric, convertReference, localizeReference,
                    specificStartGrade, specificEndGrade, organization, broker);
        }

        /**
         * @see com.x2dev.sis.tools.reports.TranscriptReportGrid#buildTranscriptQuery(com.follett.fsc.core.framework.persistence.X2Query)
         */
        @Override
        protected void buildTranscriptQuery(X2Query transcriptQuery) {
            super.buildTranscriptQuery(transcriptQuery);
            transcriptQuery.setDistinct(true);
        }
    }

    /**
     * Adds additional parameters used by the report.
     */
    private void addCustomParameters() {
        /*
         * Load attendance
         */
        Map<String, BigDecimal[]> attendances = new HashMap<String, BigDecimal[]>();
        attendances.putAll(loadClassAbsences(m_context, getSchool()));

        boolean includeAttendance = getBooleanParameter(INPUT_PARAM_ATTENDANCE);
        if (includeAttendance) {
            attendances.putAll(loadDailyAttendance(m_context.getStartDate(), m_context.getEndDate()));
        }
        addParameter(PARAM_ATTENDANCES, attendances);

        /*
         * Set general parmaters
         */
        addParameter(PARAM_CONTEXT, m_context);
        addParameter(PARAM_ADDITIONAL_TEACHERS, m_additionalTeachers);
        addParameter(PARAM_HOMEROOM_TEACHER, m_homeroomTeacher);
        addParameter(PARAM_HOMEROOM, m_homeroom);
        addParameter(PARAM_GRADE_LEVEL, m_gradeLevel);
        addParameter(PARAM_GRADE_TERM_DATES, loadGradeTermDates(getSchool(), m_context));

        if (getBooleanParameter(INPUT_INCLUDE_SCHOOL_MESSAGE_PARAM)) {
            addParameter(PARAM_MESSAGE, getParameter(MESSAGE_PARAM));
        }

        /*
         * Set layout paramters
         */
        addParameter(PARAM_SIGNATURE, Boolean.valueOf(getBooleanParameter(INPUT_PARAM_SIGNATURE)));
        addParameter(PARAM_PERFORMANCE_SCALE, Boolean.valueOf(getBooleanParameter(INPUT_PARAM_PERFORMANCE_SCALE)));
        addParameter(PARAM_INFO_PAGE, Boolean.valueOf(getBooleanParameter(INPUT_PARAM_INFO_PAGE)));
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
         * Load behavior and term comments, if necessary
         */
        loadBehaviorCommentMap();
        loadTermCommentMap();
    }

    /**
     * Adds the criteria to find the students to include for the current school.
     *
     * @param criteria Criteria
     * @param prefix String
     * @param activeOnly boolean
     * @param includeSecondary boolean
     * @return Criteria
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
                studentCriteria.addAndCriteria(schoolCriteria.copyWithAdjustedPath(
                        prefix + m_helper.getSchoolRelationship(), prefix + m_helper.getSchoolOidField()));
            }
        }

        if (activeOnly) {
            studentCriteria.addAndCriteria(m_helper.getActiveStudentCriteria(prefix));
        }

        if (includeSecondary && isSchoolContext()) {
            studentCriteria.addOrCriteria(StudentManager.getSecondaryStudentCriteria(prefix +
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER,
                    getCurrentContext().getOid(), getSchool().getOid(), null, null,
                    getBroker().getPersistenceKey()));
        }

        criteria.addAndCriteria(studentCriteria);
    }

    /**
     * Processes the report grid. Any custom displays are built.
     *
     * @return ReportDataGrid
     */
    private void buildReports() {
        m_reportGrid.beforeTop();

        while (m_reportGrid.next()) {
            if (m_displayBlended) {
                // Must be done before loadNumericAndLetterGrades for proper grade displays
                setBlendedMark(m_reportGrid);
            }

            loadNumericAndLetterGrades(m_reportGrid);
            loadAdditionalTeachers(m_reportGrid);
        }

        m_reportGrid.beforeTop();
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
            formattedGrade = m_decimalFormat.format(decimal);
        }

        return formattedGrade;
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
     * Initialize map of column definitions for all transcripts in the grid.
     *
     * @param grid ReportDataGrid
     */
    private void initColDefMap(ReportDataGrid grid) {
        HashMap<String, Collection<TranscriptColumnDefinition>> columnDefsByDefinition =
                new HashMap<String, Collection<TranscriptColumnDefinition>>();

        while (grid.next()) {
            Transcript transcript = (Transcript) grid.get(COL_TRANSCRIPT_HEADER);
            TranscriptDefinition transcriptDefinition = transcript.getTranscriptDefinition();

            Collection<TranscriptColumnDefinition> columnDefs =
                    columnDefsByDefinition.get(transcriptDefinition.getOid());
            if (columnDefs == null) {
                columnDefs = transcriptDefinition.getTranscriptColumnDefinitions();
                columnDefsByDefinition.put(transcriptDefinition.getOid(), columnDefs);
            }
            m_colDefMap.put(transcript.getOid(), columnDefs);
        }
    }

    /**
     * Loads additional teachers map keyed by student OID.
     *
     * @param grid Grid.
     */
    private void loadAdditionalTeachers(ReportDataGrid grid) {
        SisStudent student = (SisStudent) grid.get(COL_STUDENT);
        String homeroomTeacher = m_homeroomTeacher.get(student.getOid());
        Transcript transcript = (Transcript) grid.get(COL_TRANSCRIPT_HEADER);

        Collection<String> currentTeachers = m_additionalTeachers.get(student.getOid());
        if (currentTeachers == null) {
            currentTeachers = new LinkedList<String>();
            m_additionalTeachers.put(student.getOid(), currentTeachers);
        }

        /*
         * Lookup staff name to compare against homeroom teacher display
         */
        String nameDisplay = "";
        SisStaff staff = null;

        if (transcript.getMasterSchedule() != null && transcript.getMasterSchedule().getPrimaryStaff() != null) {
            staff = transcript.getMasterSchedule().getPrimaryStaff();
        } else {
            staff = transcript.getTeacher();
        }

        if (staff != null) {
            nameDisplay = staff.getPerson().getLastName() + ", " + staff.getPerson().getFirstName().substring(0, 1);
        }

        /*
         * If different from homeroom teacher, add to display
         */
        if (!nameDisplay.equals(homeroomTeacher) && !currentTeachers.contains(nameDisplay)) {
            currentTeachers.add(nameDisplay);
        }
    }

    /**
     * Load student yearly based data such as homeroom and homeroom teacher.
     */
    private void loadStudentYearlyBasedData() {
        ReportQueryByCriteria studentQuery = null;
        if (m_context.getOid().equals(getOrganization().getRootOrganization().getCurrentContextOid())) {
            String[] columns = new String[] {X2BaseBean.COL_OID, SisStudent.COL_HOMEROOM,
                    SisStudent.COL_HOMEROOM_TEACHER, SisStudent.COL_GRADE_LEVEL};
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
                            StudentContextAttributes.COL_HOMEROOM_TEACHER, StudentContextAttributes.COL_GRADE_LEVEL};
            studentQuery = new ReportQueryByCriteria(StudentContextAttributes.class, columns, studentCriteria);
        }
        ReportQueryIterator studentIterator = getBroker().getReportQueryIteratorByQuery(studentQuery);
        try {
            while (studentIterator.hasNext()) {
                Object[] data = (Object[]) studentIterator.next();
                String studentOid = data[0].toString();
                String homeroom = data[1] == null ? null : data[1].toString();
                String homeroomTeacher = data[2] == null ? null : data[2].toString();
                String gradeLevel = data[3] == null ? null : data[3].toString();

                m_homeroomTeacher.put(studentOid, homeroomTeacher);
                m_homeroom.put(studentOid, homeroom);
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
    private void loadBehaviorCommentMap() {
        loadCommentMap(INPUT_PARAM_BEHAVIOUR, m_behaviorCriteria, PARAM_BEHAVIOR_COMMENT_MAP);
    }

    /**
     * Loads section absences.
     *
     * @param context DistrictSchoolYearContext
     * @param school School
     * @return Map of BigDecimal arrays keyed by student oid.
     */
    private Map<String, BigDecimal[]> loadClassAbsences(DistrictSchoolYearContext context, School school) {
        Map<String, BigDecimal[]> result = new HashMap<String, BigDecimal[]>();

        BigDecimal[] emptyAttendances = getEmptyArray(TOTAL_TERM_INDEX);

        /*
         * Determine end date to use
         */
        PlainDate endDate = context.getEndDate();
        if (m_termDate != null) {
            endDate = m_termDate.getEndDate();
        }

        /*
         * Query section absences into a map keyed to the student OID
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addBetween(StudentPeriodAttendance.COL_DATE, context.getStartDate(), endDate);
        criteria.addEqualTo(StudentPeriodAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

        if (isCurrentQueryContainsUnion()) {
            SubQuery students = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentCriteria);
            criteria.addIn(StudentAttendance.COL_STUDENT_OID, students);
        } else {
            m_studentCollectionCriteriaHelper.applyToCriteria(StudentPeriodAttendance.COL_STUDENT_OID, criteria);
        }

        Map<String, Collection<GradeTermDate>> gradeTermDatesByYear = new HashMap<String, Collection<GradeTermDate>>();

        QueryByCriteria query = new QueryByCriteria(StudentPeriodAttendance.class, criteria);
        query.addOrderByAscending(StudentPeriodAttendance.COL_STUDENT_OID);
        query.addOrderByAscending(
                StudentPeriodAttendance.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.COL_COURSE_VIEW);
        query.addOrderByAscending(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID);
        query.addOrderByAscending(StudentPeriodAttendance.COL_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentPeriodAttendance attendance = (StudentPeriodAttendance) iterator.next();
                SisStudent student = attendance.getStudent();

                String sectionOid = attendance.getMasterScheduleOid();
                String yearOid = attendance.getMasterSchedule().getSchedule().getDistrictContextOid();

                Collection<GradeTermDate> gradeTermDates = gradeTermDatesByYear.get(yearOid);
                if (gradeTermDates == null) {
                    gradeTermDates = GradesManager.getGradeTermDates(school.getOid(), yearOid, getBroker());
                    gradeTermDatesByYear.put(yearOid, gradeTermDates);
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

                    BigDecimal[] array = result.get(student.getOid() + DELIMITER + sectionOid + ABSENT_POSTFIX);
                    if (array == null) {
                        array = emptyAttendances.clone();
                        result.put(student.getOid() + DELIMITER + sectionOid + ABSENT_POSTFIX, array);
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
     * Loads the comments into a map keyed to the student OID.
     *
     * @param inputParameter String
     * @param criteria Criteria
     * @param mapParameter String
     */
    private void loadCommentMap(String inputParameter, Criteria criteria, String mapParameter) {
        Map<String, String> commentMap = new HashMap<String, String>(2048);

        Boolean includeComments = (Boolean) getParameter(inputParameter);
        if (Boolean.TRUE.equals(includeComments)) {
            String column = COMMENT_COLUMNS.get(m_gradeTerm.getGradeTermNum() - 1);
            if (!StringUtils.isEmpty(column)) {
                List<String> columns = StringUtils.convertDelimitedStringToList(column, '|');
                TranscriptReportGrid transcriptGrid = new TranscriptReportGrid(criteria,
                        new String[] {X2BaseBean.COL_OID},
                        false,
                        false,
                        false,
                        getOrganization(),
                        getBroker());

                while (transcriptGrid.next()) {
                    String studentOid = transcriptGrid.getStudent().getOid();
                    String comment1 = (String) transcriptGrid.get(columns.get(0));
                    String comment2 = (String) transcriptGrid.get(columns.get(1));

                    commentMap.put(studentOid, StringUtils.coalesce(comment1, comment2));
                }
            }
        }

        addParameter(mapParameter, commentMap);
    }

    /**
     * Loads daily attendance totals by month.
     *
     * @param startDate Start date.
     * @param endDate End date.
     * @return Map of BigDecimal arrays keyed by student OID.
     */
    private Map<String, BigDecimal[]> loadDailyAttendance(PlainDate startDate, PlainDate endDate) {
        Map<String, BigDecimal[]> result = new HashMap<String, BigDecimal[]>();

        Calendar calendar = Calendar.getInstance();
        BigDecimal[] emptyAttendances = getEmptyArray(TOTAL_MONTH_INDEX);

        // Query out daily attendance records into a map keyed to the student OID
        X2Criteria criteria = new X2Criteria();
        criteria.addBetween(StudentAttendance.COL_DATE, startDate, endDate);

        if (isCurrentQueryContainsUnion()) {
            SubQuery students = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentCriteria);
            criteria.addIn(StudentAttendance.COL_STUDENT_OID, students);
        } else {
            m_studentCollectionCriteriaHelper.applyToCriteria(StudentAttendance.COL_STUDENT_OID, criteria);
        }

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        query.addOrderByAscending(StudentAttendance.COL_STUDENT_OID);
        query.addOrderByAscending(StudentAttendance.COL_DATE);
        Map<String, Collection<StudentAttendance>> attendancesMap = getBroker().getGroupedCollectionByQuery(query,
                StudentAttendance.COL_STUDENT_OID,
                2048);

        // Process students (this is done so results are entered for students with no attendance
        // records)
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, m_studentCriteria);
        QueryIterator studentIterator = getBroker().getIteratorByQuery(studentQuery);

        try {
            while (studentIterator.hasNext()) {
                SisStudent student = (SisStudent) studentIterator.next();

                BigDecimal[] absents = emptyAttendances.clone();
                BigDecimal[] late = emptyAttendances.clone();

                result.put(student.getOid() + ABSENT_POSTFIX, absents);
                result.put(student.getOid() + LATE_POSTFIX, late);

                Collection<StudentAttendance> attendances = attendancesMap.get(student.getOid());
                if (attendances != null) {
                    for (StudentAttendance attendance : attendances) {
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
                    }
                }
            }
        } finally {
            studentIterator.close();
        }

        return result;
    }

    /**
     * Injects numeric grades into specified grid.
     *
     * @param grid Grid to inject values to.
     */
    private void loadNumericAndLetterGrades(ReportDataGrid grid) {
        Transcript transcript = (Transcript) grid.get(COL_TRANSCRIPT_HEADER);
        Collection<TranscriptColumnDefinition> columnDefs = m_colDefMap.get(transcript.getOid());

        for (TranscriptColumnDefinition columnDef : columnDefs) {
            String gradeColumnHeader = columnDef.getGradeColumnHeader();

            if (COLUMNS.contains(gradeColumnHeader)) {
                GradeScale gradeScale = columnDef.getGradeScale();

                String gradeValue = (String) grid.get(gradeColumnHeader);
                BigDecimal gradeNumeric = null;
                String gradeLetter = null;

                if (StringUtils.isNumeric(gradeValue)) {
                    gradeNumeric = new BigDecimal(gradeValue);

                    if (gradeScale != null) {
                        gradeLetter = m_gradesManager.getLetterValue(gradeNumeric, gradeScale,
                                transcript.getSchool(), transcript.getSchoolCourseOid());
                    }
                } else if (!StringUtils.isEmpty(gradeValue)) {
                    /*
                     * Always convert numeric grade to letter grade, but not the other way around.
                     */
                    gradeLetter = gradeValue;
                }

                grid.set(gradeColumnHeader, gradeLetter);
                grid.set(gradeColumnHeader + NUM_POSTFIX, gradeNumeric);
            }
        }
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
     * Load behavior comments, if necessary. Comments for the current term are loaded into a Map
     * keyed to the
     * student OID.
     */
    private void loadTermCommentMap() {
        loadCommentMap(INPUT_PARAM_COMMENTS, m_termCommentCriteria, PARAM_TERM_COMMENT_MAP);
    }

    /**
     * Sets the blended mark on the grid if the parent course is examinable.
     *
     * @param grid void
     */
    private void setBlendedMark(ReportDataGrid grid) {
        Transcript transcript = (Transcript) grid.get(COL_TRANSCRIPT_HEADER);
        Course course = transcript.getSchoolCourse().getCourse();

        String finalGrade = transcript.getFinalGrade();
        String blendGrade = formatNumericGrade((String) transcript.getFieldValueByAlias(BLENDED_MARK_ALIAS));

        boolean examinable = course.getExamRequiredIndicator() || course.getRootCourse().getExamRequiredIndicator();
        String trax = (String) transcript.getFieldValueByAlias(TRAX_ALIAS);

        if (examinable && trax != null && trax.endsWith(TRAX_COMPLETE_SUFFIX)) {
            blendGrade = finalGrade;
        } else if (!examinable) {
            blendGrade = null;
        }

        grid.set("Final", StringUtils.coalesce(blendGrade, finalGrade));
    }
}
