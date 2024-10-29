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
import com.follett.fsc.core.framework.persistence.QueryUtils;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.Selection;
import com.follett.fsc.core.k12.beans.SelectionObject;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.SortProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ContextList;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.nav.FilterException;
import com.x2dev.reports.bc.SchoolTranscriptData.BcGraduationManager;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.JDOMException;

/**
 * Java source for Diploma Verification report.
 *
 * @author Follett Software
 */
public class DiplomaVerificationData extends ReportJavaSourceNet {

    /**
     * Helper class to store and accumulate row values for the course list.
     */
    public class CourseInfo {
        PlainDate completionDate;
        Double creditsEarned;
        Double creditsInProgress;
        GraduationRequirement requirement;
        SchoolCourse schoolCourse;
        Transcript transcript;
    }

    private static final long serialVersionUID = 1L;

    /*
     * Input Parameters
     */
    private static final String ACTIVE_ONLY_PARAM = "activeOnly";
    private static final String INCLUDE_SECONDARY_PARAM = "secondaryStudent";
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String SORT_PARAM = "sort";

    /*
     * Report fields
     */
    private static final String FIELD_COMPLETION_DATE = "Completion Date";
    private static final String FIELD_COURSE_DESCRIPTION = "courseDescription";
    private static final String FIELD_COURSE_CODE = "Course Code";
    private static final String FIELD_DIPLOMA_CATEGORY = "diplomaCategory";
    private static final String FIELD_EARNED_CREDIT = "earnedCredit";
    private static final String FIELD_EXAM_MARK = "examMark";
    private static final String FIELD_FINAL_MARK = "finalMark";
    private static final String FIELD_GRAD_PROGRAM = "gradProgram";
    private static final String FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String FIELD_INTERIM_MARK = "interimMark";
    private static final String FIELD_IS_REPEAT = "isRepeat";
    private static final String FIELD_POTENTIAL_CREDIT = "potentialCredit";
    private static final String FIELD_PROV_EXAM_DATE = "provExamDate";
    private static final String FIELD_SCHOOL_COURSE = "schoolCourse";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_NOTES = "notes";
    private static final String FIELD_ZERO_CREDIT = "zeroCredit";
    private static final String FIELD_TRANSCRIPT = "transcript";

    /*
     * Subreport constants
     */
    private static final String GRADUATION_REPORT_ID = "BC-GRQ-001-GRAD";
    private static final String TRANSCRIPT_REPORT_ID = "BC-GRQ-001-TRN";
    private static final String REQUREMENTS_GRID = "reportDataGrid1";
    private static final String TRANSCRIPTS_GRID = "reportDataGrid2";

    /*
     * allowed grade levels for this report
     */
    private static final String GRADE_10 = "10";
    private static final String GRADE_11 = "11";
    private static final String GRADE_12 = "12";

    /*
     * Other constants
     */
    private static final String CODE_DESC_PREFIX = "Code-Description_";
    private static final Double DOUBLE_ZERO = Double.valueOf("0.0");
    private static final String MET_PREFIX = "Met_";
    private static final String UNIT_PREFIX = "Unit_";

    /*
     * Field aliases
     */
    private static final String ALIAS_COMPLETION_DATE = "trn-completion-date";
    private static final String ALIAS_COURSE_END_DATE = "trn-end-date";
    private static final String ALIAS_COURSE_CODE = "crs-external-code";
    private static final String EXAM_COURSE_END_DATE_ALIAS = "examCourseEndDate";
    private static final String EXAM_DATE_1_ALIAS = "examSessionDate1";
    private static final String EXAM_DATE_2_ALIAS = "examSessionDate2";
    private static final String EXAM_MARK_1_ALIAS = "examMark1";
    private static final String EXAM_MARK_2_ALIAS = "examMark2";
    private static final String FINAL_MARK_1_ALIAS = "blendedMark1";
    private static final String FINAL_MARK_2_ALIAS = "blendedMark2";
    private static final String PERIOD_1_GRADE_ALIAS = "trn-grade-period1";
    private static final String PERIOD_2_GRADE_ALIAS = "trn-grade-period2";
    private static final String PERIOD_3_GRADE_ALIAS = "trn-grade-period3";
    private static final String PERIOD_4_GRADE_ALIAS = "trn-grade-period4";
    private static final String TRANSCRIPT_BLENDED_ALIAS = "trn-blended-mark";
    private static final String TRANSCRIPT_TRAX_ALIAS = "trn-trax-override";

    private Map<String, DataDictionary> m_assessmentDictionaries;
    private Map<String, Map<String, List<StudentAssessment>>> m_assessmentMap;
    private DateAsStringConverter m_dateConverter;
    private Map<String, TranscriptColumnDefinition> m_finalColumnMap;
    private Collection<String> m_gradeLevels;
    private GradesManager m_gradesManager;
    private BcGraduationManager m_graduationManager;
    private StudentContextReportHelper m_helper;
    private Map<String, Collection<GraduationRequirement>> m_programRequirementMap;
    private Map<String, String> m_requirementCodeMap;
    private Collection<String> m_schoolOids;
    private SchoolTranscriptData m_schoolTranscriptData;
    private LinkedHashMap<String, Boolean> m_sortProperties;
    private SisStudent m_student;
    private Map<String, Collection<StudentSchedule>> m_studentScheduleMap;
    private Collection<X2BaseBean> m_temporaryBeans;
    private Map<String, PlainDate> m_termEndDateMap;
    private Map<String, Map<String, Collection<Transcript>>> m_transcriptsByNumber;
    private Map<String, Collection<Transcript>> m_transcriptMap;

    /*
     * Criteria objects
     */
    private X2Criteria m_assessmentCriteria;
    private X2Criteria m_programCriteria;
    private X2Criteria m_scheduleCriteria;
    private X2Criteria m_transcriptCriteria;

    /**
     * Apply current sort.
     *
     * @param query QueryByCriteria
     * @see
     *      com.follett.fsc.core.k12.tools.ToolJavaSource#applyCurrentSort(org.apache.ojb.broker.query.
     *      QueryByCriteria)
     */
    @Override
    protected void applyCurrentSort(QueryByCriteria query) {
        // The prefix is necessary because the table being queried and the context list table are
        // different.
        String sortPropertyPrefix = GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER;

        for (String beanPath : m_sortProperties.keySet()) {
            if (!QueryUtils.containsSortOrder(query, beanPath)) {
                query.addOrderBy(sortPropertyPrefix + beanPath, m_sortProperties.get(beanPath).booleanValue());

                /*
                 * Sort fields should be OUTER rather than INNER joins so that no records are
                 * eliminated from the list just because the sort fields changed!
                 */
                int position = beanPath.lastIndexOf(ModelProperty.PATH_DELIMITER);
                if (position > -1) {
                    query.setPathOuterJoin(sortPropertyPrefix + beanPath.substring(0, position));
                }
            }
        }
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws FilterException exception
     * @throws JDOMException exception
     * @throws IOException Signals that an I/O exception has occurred.
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws FilterException, JDOMException, IOException {
        ReportDataGrid grid = new ReportDataGrid(300, 2);

        QueryByCriteria query = new QueryByCriteria(GraduationStudentProgram.class, m_programCriteria);
        query.addOrderByAscending(GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER +
                m_helper.getSchoolRelationship() + PATH_DELIMITER + SisSchool.COL_NAME);
        query.addOrderByAscending(GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolOidField());

        String sortBy = (String) getParameter(SORT_PARAM);
        if (sortBy.startsWith(SELECTION_SPECIAL_CASE_PREFIX) && sortBy.contains(CURRENT_KEY)) {
            // Manually update sortBy because query table is not the table in the list for the
            // current sort.
            applyCurrentSort(query);
        } else {
            applyUserSort(query, sortBy);
        }

        query.addOrderByDescending(GraduationStudentProgram.COL_PRIMARY_INDICATOR);

        /*
         * Execute the query and return the results
         */
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisStudent lastStudent = null;

            while (iterator.hasNext()) {
                GraduationStudentProgram program = (GraduationStudentProgram) iterator.next();
                SisStudent student = program.getStudent();

                if (!ObjectUtils.match(student, lastStudent)) {
                    grid.append();
                    grid.set(FIELD_STUDENT, student);
                    grid.set(FIELD_GRAD_PROGRAM, program.getProgramStudies().getName());

                    processGradRequirements(student, program, grid);
                }

                lastStudent = student;
            }
        } finally {
            iterator.close();
        }

        /*
         * Cleanup temporary data
         */
        if (!m_temporaryBeans.isEmpty()) {
            for (X2BaseBean bean : m_temporaryBeans) {
                getBroker().deleteBean(bean);
            }
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_helper = new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());
        m_temporaryBeans = new ArrayList<X2BaseBean>();

        loadSchoolOids();

        initGradeLevels();
        buildCriteria();

        m_assessmentDictionaries = new HashMap<String, DataDictionary>(64);
        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                getLocale(), true);
        m_gradesManager = new GradesManager(getBroker());
        m_programRequirementMap = new HashMap<String, Collection<GraduationRequirement>>(16);
        m_termEndDateMap = new HashMap<String, PlainDate>(1024);

        m_schoolTranscriptData = new SchoolTranscriptData();
        m_graduationManager = m_schoolTranscriptData.new BcGraduationManager(getBroker());

        buildAssessmentsMap();
        loadFinalGradeColumnMap();
        loadRequirementCodes();
        loadTermDates();
        loadStudentSchedules(); // Must be run AFTER loadTermDates()
        loadTranscriptsMaps();

        /*
         * Set subreports
         */
        Report subrepGradRequirements = ReportUtils.getReport(GRADUATION_REPORT_ID, getBroker());
        Report subrepTranscriptDetail = ReportUtils.getReport(TRANSCRIPT_REPORT_ID, getBroker());
        addParameter("reportInputString1", new ByteArrayInputStream(subrepGradRequirements.getCompiledFormat()));
        addParameter("reportInputString2", new ByteArrayInputStream(subrepTranscriptDetail.getCompiledFormat()));
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
        m_student = userData.getCurrentRecord(SisStudent.class);
        m_sortProperties = loadSortProperties(userData);
    }

    /**
     * Adds the criteria to filter students based on school and enrollments status.
     *
     * @param criteria Criteria
     * @param prefix String
     * @param activeOnly boolean
     * @param includeSecondary boolean
     * @return Criteria
     * @Param criteria
     */
    private void addSchoolCriteria(Criteria criteria, String prefix, boolean activeOnly, boolean includeSecondary) {
        X2Criteria schoolCriteria = new X2Criteria();

        if (isSchoolContext()) {
            if (!m_schoolOids.isEmpty() && m_schoolOids.size() > 1) {
                X2Criteria schoolsCriteria = new X2Criteria();

                Selection selObject = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());
                for (String oid : m_schoolOids) {
                    SelectionObject object =
                            X2BaseBean.newInstance(SelectionObject.class, getBroker().getPersistenceKey());
                    object.setObjectOid(oid);
                    selObject.addToSelectionObjects(object);
                }
                getBroker().saveBean(selObject);

                Criteria subCriteria = new Criteria();
                subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, selObject.getOid());
                subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX
                        + X2BaseBean.COL_OID);

                schoolsCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));

                schoolCriteria.addAndCriteria(schoolsCriteria.copyWithAdjustedPath(
                        prefix + m_helper.getSchoolRelationship(),
                        prefix + m_helper.getSchoolOidField()));

                m_temporaryBeans.add(selObject);
                m_temporaryBeans.addAll(selObject.getSelectionObjects());
            } else if (!m_schoolOids.isEmpty()) {
                schoolCriteria.addEqualTo(prefix + m_helper.getSchoolOidField(), m_schoolOids.iterator().next());
            }
        }

        if (activeOnly) {
            schoolCriteria.addAndCriteria(m_helper.getActiveStudentCriteria(prefix));
        }

        if (includeSecondary && getSchool() != null) {
            schoolCriteria.addOrCriteria(StudentManager.getSecondaryStudentCriteria(
                    prefix + SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER,
                    getCurrentContext().getOid(), getSchool().getOid(), null, null,
                    getBroker().getPersistenceKey()));
        }

        criteria.addAndCriteria(schoolCriteria);
    }

    /**
     * Adds the student assessment record to the list of assessments based on the parent value.
     *
     * @param assessmentMap Map<String,List<StudentAssessment>>
     * @param assessment StudentAssessment
     * @param parent String
     */
    private void appendToList(Map<String, List<StudentAssessment>> assessmentMap,
                              StudentAssessment assessment,
                              String parent) {
        List<StudentAssessment> assessments = assessmentMap.get(parent);

        if (assessments == null) {
            assessments = new LinkedList<StudentAssessment>();
            assessmentMap.put(parent, assessments);
        }

        assessments.add(assessment);
    }

    /**
     * Build assessment map student / assessment owner / StudentAssessment.
     */
    private void buildAssessmentsMap() {
        m_assessmentMap = new HashMap<String, Map<String, List<StudentAssessment>>>(4096);

        /*
         * Load transcript for students with programs of studies included in the report
         */
        X2Criteria sectionCriteria = new X2Criteria();
        sectionCriteria.addNotEmpty(StudentAssessment.COL_MASTER_SCHEDULE_OID, getBroker().getPersistenceKey());

        X2Criteria courseCriteria = new X2Criteria();
        courseCriteria.addNotEmpty(StudentAssessment.COL_SCHOOL_COURSE_OID, getBroker().getPersistenceKey());

        X2Criteria codeCriteria = new X2Criteria();
        codeCriteria.addNotEmpty(StudentAssessment.COL_FIELD_B010, getBroker().getPersistenceKey());

        sectionCriteria.addOrCriteria(courseCriteria);
        sectionCriteria.addOrCriteria(codeCriteria);
        m_assessmentCriteria.addAndCriteria(sectionCriteria);

        /*
         * Execute query to load map based on student OID and course number
         */
        QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, m_assessmentCriteria);
        query.addOrderByAscending(StudentAssessment.COL_STUDENT_OID);
        query.addOrderByAscending(StudentAssessment.COL_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisStudent lastStudent = null;
            Map<String, List<StudentAssessment>> assessments = new HashMap<String, List<StudentAssessment>>(512);

            while (iterator.hasNext()) {
                StudentAssessment bean = (StudentAssessment) iterator.next();
                SisStudent student = bean.getStudent();

                if (!ObjectUtils.match(lastStudent, student)) {
                    assessments = new HashMap<String, List<StudentAssessment>>(512);
                    m_assessmentMap.put(student.getOid(), assessments);
                }

                /*
                 * Include assessments based on the section and course OIDs.
                 */
                String sectionOid = bean.getMasterScheduleOid();
                String courseOid = bean.getSchoolCourseOid();
                String courseCode = bean.getFieldB010();

                if (!StringUtils.isEmpty(sectionOid)) {
                    appendToList(assessments, bean, sectionOid);
                }

                if (!StringUtils.isEmpty(courseOid)) {
                    appendToList(assessments, bean, courseOid);
                }

                if (!StringUtils.isEmpty(courseCode)) {
                    appendToList(assessments, bean, courseCode);
                }

                lastStudent = student;
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Build the criteria objects based on user-input.
     *
     * @return Criteria
     */
    private void buildCriteria() {
        m_assessmentCriteria = new X2Criteria();
        m_programCriteria = new X2Criteria();
        m_scheduleCriteria = new X2Criteria();
        m_transcriptCriteria = new X2Criteria();

        if (m_student != null) {
            m_assessmentCriteria.addEqualTo(StudentAssessment.COL_STUDENT_OID, m_student.getOid());
            m_programCriteria.addEqualTo(GraduationStudentProgram.COL_STUDENT_OID, m_student.getOid());
            m_scheduleCriteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, m_student.getOid());
            m_transcriptCriteria.addEqualTo(Transcript.COL_STUDENT_OID, m_student.getOid());
        } else {
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);
            addUserCriteria(m_assessmentCriteria, queryBy, queryString, StudentAssessment.class, SisStudent.class,
                    StudentAssessment.COL_STUDENT_OID);
            addUserCriteria(m_programCriteria, queryBy, queryString, GraduationStudentProgram.class, SisStudent.class,
                    GraduationStudentProgram.COL_STUDENT_OID);
            addUserCriteria(m_scheduleCriteria, queryBy, queryString, StudentSchedule.class, SisStudent.class,
                    StudentSchedule.COL_STUDENT_OID);
            addUserCriteria(m_transcriptCriteria, queryBy, queryString, Transcript.class, SisStudent.class,
                    Transcript.COL_STUDENT_OID);

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            boolean includeSecondary = ((Boolean) getParameter(INCLUDE_SECONDARY_PARAM)).booleanValue();

            if (!queryBy.contains(CURRENT_KEY)) {
                addSchoolCriteria(m_assessmentCriteria, StudentAssessment.REL_STUDENT + PATH_DELIMITER, activeOnly,
                        includeSecondary);
                addSchoolCriteria(m_programCriteria, GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER, activeOnly,
                        includeSecondary);
                addSchoolCriteria(m_scheduleCriteria, StudentSchool.REL_STUDENT + PATH_DELIMITER, activeOnly,
                        includeSecondary);
                addSchoolCriteria(m_transcriptCriteria, Transcript.REL_STUDENT + PATH_DELIMITER, activeOnly,
                        includeSecondary);
            }

        }
        /*
         * This section of code was moved from line 570 (above the brace)
         * to resolve T30357770. This was handled by Technical Support.
         *
         * Report should work only with 10-12 grade students
         */
        String gradeLevelField = m_helper.getGradeLevelField();
        m_assessmentCriteria.addIn(StudentAssessment.REL_STUDENT + PATH_DELIMITER + gradeLevelField, m_gradeLevels);
        m_programCriteria.addIn(GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER + gradeLevelField, m_gradeLevels);
        m_scheduleCriteria.addIn(StudentSchedule.REL_STUDENT + PATH_DELIMITER + gradeLevelField, m_gradeLevels);
        m_transcriptCriteria.addIn(Transcript.REL_STUDENT + PATH_DELIMITER + gradeLevelField, m_gradeLevels);
    }

    /**
     * Checks if the transcript record is repeated. Repeats are allowed if the related Course record
     * allows repeats.
     *
     * @param transcript Transcript
     * @param repeatOids Collection<String>
     * @return boolean
     */
    private boolean checkRepeat(Transcript transcript, Collection<String> repeatOids) {
        // Check the course 'allow repeat indicator'
        Course course = transcript.getSchoolCourse().getCourse();

        // The transcript are gathered based on the external course number on root course, so we
        // only need to
        // look at the root course "allow repeat" flag.
        return course.getRootCourse().getAllowRepeatIndicator() && repeatOids.contains(transcript.getOid());
    }

    /**
     * Compare the final grade on two transcripts.
     *
     * @param t1 Transcript
     * @param t2 Transcript
     * @return int
     */
    private int compareTranscriptGrades(Transcript t1, Transcript t2) {
        int comparison = 0;

        try {
            BigDecimal grade1 = getNumericGrade(t1);
            BigDecimal grade2 = getNumericGrade(t2);

            if (grade1 == null) {
                grade1 = BigDecimal.ZERO;
            }

            if (grade2 == null) {
                grade2 = BigDecimal.ZERO;
            }

            comparison = grade1.compareTo(grade2);
        } catch (Exception e) {
            comparison = 0;
        }

        return comparison;
    }

    /**
     * Review information for the entered course and flag with appropriate note, if any.
     * <ul>
     * <li>R1 - No assessment record exists for the course with required exam
     * <li>R2 - Failing Blended mark achieved for the course with required exam
     * <li>R3 - No blended mark exists for the course with required exam
     * <li>R4 - Currently there is no indicator to identify that the exam is deferred
     * <li>R5 - TRAX Override field on student transcript is selected as 'Equivalency Not Writing'
     * for the course
     * with required exam
     * <li>O1 - No assessment record exists for courses with optional exam
     * <li>O2 - Failing Blended marks achieved for courses with optional exam
     * <li>O3 - No blended mark exists for courses with optional exam
     * <li>O4 - Currently there is no indicator to identify that the exam is deferred
     * <li>O5 - TRAX override field on student transcript is selected as ?Equivalency Not Writing?
     * for courses
     * with optional exam
     * <li>O6 - Student opted out of Provincial exam for courses with optional exam
     * </ul>
     * Please note that Ministry has removed the optional exams in 2011. So the notes O1 to O6 will
     * be applicable
     * only to historical data.
     *
     * @param transcript Transcript
     * @param assessments List<StudentAssessment>
     * @return String
     */
    private String determineNotes(Transcript transcript, List<StudentAssessment> assessments) {
        Collection<String> notes = new LinkedList<String>();

        /*
         * Load information
         */
        SchoolCourse schoolCourse = transcript.getSchoolCourse();
        Course rootCourse = schoolCourse.getCourse().getRootCourse();

        String blendedMark = "";
        String trax = "";
        String traxSelection = "Equivalent Not Writing (Q)";

        boolean blendedFail = false;
        boolean deferredIndicator = true;
        boolean optOut = false;
        boolean qTrax = false;

        if (transcript != null) {
            blendedMark = (String) transcript.getFieldValueByAlias(TRANSCRIPT_BLENDED_ALIAS);
            trax = (String) transcript.getFieldValueByAlias(TRANSCRIPT_TRAX_ALIAS);
            qTrax = trax != null && trax.endsWith("(Q)");

            if (StringUtils.isNumeric(blendedMark)) {
                blendedFail = (Double.parseDouble(blendedMark)) < 50;
            }
        }

        /*
         * Check for note scenarios
         */
        if (rootCourse.getExamRequiredIndicator()) {
            if (!qTrax && CollectionUtils.isEmpty(assessments)) {
                notes.add("R1");
            } else if (!qTrax && blendedFail) {
                notes.add("R2");
            } else if (!qTrax && StringUtils.isEmpty(blendedMark)) {
                notes.add("R3");
            }

            if (!deferredIndicator) {
                notes.add("R4");
            }

            if (traxSelection.equals(trax)) {
                notes.add("R5");
            }
        } else if (!rootCourse.getExamRequiredIndicator() && rootCourse.getDistrictContext().getSchoolYear() <= 2011) {
            if (!qTrax && CollectionUtils.isEmpty(assessments)) {
                notes.add("O1");
            } else if (!qTrax && blendedFail) {
                notes.add("O2");
            } else if (!qTrax && StringUtils.isEmpty(blendedMark)) {
                notes.add("O3");
            }

            if (!deferredIndicator) {
                notes.add("O4");
            }

            if (traxSelection.equals(trax)) {
                notes.add("O5");
            }

            if (optOut) {
                notes.add("O6");
            }
        }

        return StringUtils.convertCollectionToDelimitedString(notes, ",");
    }

    /**
     * Determines if the assessment and transcript course code values match. Only the first 7
     * characters
     * are considered when determining if they are a match.
     *
     * @param assessmentCourseCode String
     * @param transcriptCourseCode String
     * @return boolean
     */
    private boolean doesCourseCodeMatch(String assessmentCourseCode, String transcriptCourseCode) {
        boolean isMatch = false;

        if (!StringUtils.isEmpty(assessmentCourseCode) &&
                !StringUtils.isEmpty(transcriptCourseCode)) {
            // Reduce each value to only the first 7 characters
            assessmentCourseCode =
                    assessmentCourseCode.length() > 7 ? assessmentCourseCode.substring(0, 7) : assessmentCourseCode;
            transcriptCourseCode =
                    transcriptCourseCode.length() > 7 ? transcriptCourseCode.substring(0, 7) : transcriptCourseCode;

            if (assessmentCourseCode.equals(transcriptCourseCode)) {
                isMatch = true;
            }
        }

        return isMatch;
    }

    /**
     * Returns whether or not the transcript earned full credits. This takes into account the
     * potential credit
     * override value on the transcript record. If there is no potential credit, the student is
     * assumed to have
     * earned full credit.
     *
     * @param transcript Transcript
     * @return boolean
     */
    private boolean earnedFullCredit(Transcript transcript) {
        boolean fullCredit = false;

        if (transcript.getTotalCredit() != null) {
            BigDecimal potentialCredit = transcript.getSchoolCourse().getCredit();
            if (StringUtils.isNumeric(transcript.getPotentialCredit())) {
                potentialCredit = new BigDecimal(transcript.getPotentialCredit());
            }

            fullCredit = potentialCredit == null || transcript.getTotalCredit().compareTo(potentialCredit) >= 0;
        }

        return fullCredit;
    }

    /**
     * Returns a list of the assessments for the matching section or course OID for the student.
     *
     * @param student Student
     * @param sectionOid String
     * @param schoolCourseOid String
     * @param courseCode String
     * @return List<StudentAssessment>
     */
    private List<StudentAssessment> getAssociatedAssessments(Student student,
                                                             String sectionOid,
                                                             String schoolCourseOid,
                                                             String courseCode) {
        List<StudentAssessment> assessments = null;

        Map<String, List<StudentAssessment>> assessmentsMap = m_assessmentMap.get(student.getOid());
        if (assessmentsMap != null) {
            assessments = assessmentsMap.get(sectionOid);

            if (assessments == null) {
                assessments = assessmentsMap.get(schoolCourseOid);
            }

            if (assessments == null) {
                assessments = assessmentsMap.get(courseCode);
            }
        }

        return assessments;
    }

    /**
     * Returns the course code used for determining duplicate courses.
     *
     * @param course SchoolCourse
     * @return String
     */
    private String getCourseCode(SchoolCourse course) {
        String code = null;

        if (course != null && course.getCourse() != null) {
            code = (String) course.getCourse().getRootCourse().getFieldValueByAlias(ALIAS_COURSE_CODE);
        }
        return code;
    }

    /**
     * Gets the final grade on the transcript. If the transcript is for an examinable
     * course, the blend mark is returned. TRAX code is considered.
     *
     * Wrapper for SchoolTranscriptData.getFinalGrade(transcript, examinable, trax);
     *
     * @param transcript Transcript
     * @return String
     */
    private String getFinalGrade(Transcript transcript) {
        String trax = (String) transcript.getFieldValueByAlias(TRANSCRIPT_TRAX_ALIAS);
        boolean examinable = m_schoolTranscriptData.lookupExamStatus(transcript.getSchoolCourse());

        return m_schoolTranscriptData.getFinalGrade(transcript, examinable, trax);
    }

    /**
     * Returns the most recent term grade for the transcript record.
     *
     * @param transcript Transcript
     * @return String
     */
    private String getInterimMark(Transcript transcript) {
        String mark = "";

        if (StringUtils.isEmpty(getFinalGrade(transcript))) {
            String period1 = (String) transcript.getFieldValueByAlias(PERIOD_1_GRADE_ALIAS);
            String period2 = (String) transcript.getFieldValueByAlias(PERIOD_2_GRADE_ALIAS);
            String period3 = (String) transcript.getFieldValueByAlias(PERIOD_3_GRADE_ALIAS);
            String period4 = (String) transcript.getFieldValueByAlias(PERIOD_4_GRADE_ALIAS);

            mark = StringUtils.coalesce(period4, StringUtils.coalesce(period3, StringUtils.coalesce(period2, period1)));
        }

        return mark;
    }

    /**
     * Converts the grade to a numeric value, if possible. If not possible, a NULL value is
     * returned.
     *
     * @param transcript Transcript
     * @return BigDecimal
     */
    private BigDecimal getNumericGrade(Transcript transcript) {
        BigDecimal value = null;

        String grade = getFinalGrade(transcript);

        if (StringUtils.isNumeric(grade)) {
            value = new BigDecimal(grade);
        } else if (!StringUtils.isEmpty(grade)) {
            TranscriptColumnDefinition column = m_finalColumnMap.get(transcript.getTranscriptDefinitionOid());
            if (column != null) {
                GradeScale scale = column.getGradeScale();
                if (scale != null) {
                    value = m_gradesManager.getNumericValue(grade, scale, transcript.getSchool(),
                            transcript.getSchoolCourseOid());
                }
            }
        }

        return value;
    }

    /**
     * Returns collection of requirements associated with graduation program. If needed the
     * requirements are loaded
     * into memory.
     *
     * @param program GraduationProgram
     * @return Collection<GraduationRequirement>
     */
    private Collection<GraduationRequirement> getRequirementsOfProgram(GraduationProgram program) {
        Collection<GraduationRequirement> requirements = m_programRequirementMap.get(program.getOid());

        if (requirements == null) {
            requirements = program.getRequirements(getBroker());
            m_programRequirementMap.put(program.getOid(), requirements);
        }

        return requirements;
    }

    /**
     * Returns the end date of the section based on the related schedule term.
     *
     * @param section Section
     * @return Plain date
     */
    private PlainDate getTermEndDate(Section section) {
        PlainDate date = null;

        if (section != null) {
            ScheduleTerm term = section.getScheduleTerm();
            date = m_termEndDateMap.get(term.getOid());
        }

        return date;
    }

    /**
     * Returns the reason the transcript earned 0 credits.
     *
     * @param transcript Transcript
     * @param isRepeat boolean
     * @return String 0-8/F/M/W
     */
    private String getZeroCreditDesignation(Transcript transcript, boolean isRepeat) {
        String value = null;

        String finalGrade = getFinalGrade(transcript);
        if (!StringUtils.isEmpty(finalGrade)) {
            if (transcript.getTotalCredit() == null || transcript.getTotalCredit().doubleValue() < 0.01) {
                if (isFailing(transcript)) {
                    value = "0";
                }
            } else if (!earnedFullCredit(transcript)) {
                value = "F";
            }

            if (StringUtils.isEmpty(value) && isRepeat) {
                value = "2";
            }
        }

        return value;
    }

    /**
     * Returns collection with allowed grade levels for report.
     *
     * @return Collection<String>
     */
    private void initGradeLevels() {
        m_gradeLevels = new ArrayList<String>(3);
        m_gradeLevels.add(GRADE_10);
        m_gradeLevels.add(GRADE_11);
        m_gradeLevels.add(GRADE_12);
    }

    /**
     * Checks the final grade against the grade scale to determine if it is a failing grade (one
     * that does not
     * earn credit).
     *
     * @param transcript Transcript
     * @return boolean
     */
    private boolean isFailing(Transcript transcript) {
        boolean isFail = false;

        String finalGrade = getFinalGrade(transcript);
        if (!StringUtils.isEmpty(finalGrade)) {
            TranscriptColumnDefinition column = m_finalColumnMap.get(transcript.getTranscriptDefinitionOid());
            if (column != null) {
                GradeScale gradeScale = column.getGradeScale();

                String letterValue = null;

                if (StringUtils.isNumeric(finalGrade)) {
                    BigDecimal numericValue = new BigDecimal(finalGrade);
                    letterValue = m_gradesManager.getLetterValue(numericValue, gradeScale,
                            transcript.getSchool(), transcript.getSchoolCourseOid());
                } else {
                    letterValue = finalGrade;
                }

                GradeScaleGradeDefinition gradeDefinition = m_gradesManager.getGradeDefinition(letterValue, gradeScale,
                        transcript.getSchoolOid(), transcript.getSchoolCourseOid());
                if (gradeDefinition != null && !gradeDefinition.getCreditIndicator()) {
                    isFail = true;
                }
            }
        }

        return isFail;
    }

    /**
     * Loads the column holding the final grade into a map keyed to the transcript definition OID.
     */
    private void loadFinalGradeColumnMap() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));
        criteria.addOrEqualTo(TranscriptColumnDefinition.REL_DATA_FIELD_CONFIG + PATH_DELIMITER +
                DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER +
                DataField.COL_JAVA_NAME, Transcript.COL_FINAL_GRADE);

        QueryByCriteria query = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);

        m_finalColumnMap =
                getBroker().getMapByQuery(query, TranscriptColumnDefinition.COL_TRANSCRIPT_DEFINITION_OID, 32);
    }

    /**
     * Loads a lookup map of requirement codes keyed to the requirement OID.
     */
    private void loadRequirementCodes() {
        m_requirementCodeMap = new HashMap<String, String>(1024);

        QueryByCriteria query = new QueryByCriteria(GraduationRequirement.class);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                GraduationRequirement requirement = (GraduationRequirement) iterator.next();
                m_requirementCodeMap.put(requirement.getOid(), requirement.getCode());
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads the school OIDs included in the reporting.
     */
    private void loadSchoolOids() {
        Criteria criteria = new Criteria();
        criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

        SubQuery schools = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, criteria);
        m_schoolOids = getBroker().getSubQueryCollectionByQuery(schools);
    }

    /**
     * Gets the map of current sort property bean paths and sort ascending values
     * off of the user data current list.
     *
     * @param userData UserDataContainer
     * @return LinkedHashMap<String, Boolean>
     */
    private LinkedHashMap<String, Boolean> loadSortProperties(UserDataContainer userData) {
        LinkedHashMap<String, Boolean> currentSortProperties = new LinkedHashMap<String, Boolean>();

        ContextList currentList = userData.getCurrentList();
        if (currentList != null) {
            List<SortProperty> sortProperties = currentList.getSortProperties();
            for (SortProperty sortProperty : sortProperties) {
                currentSortProperties.put(sortProperty.getBeanPath(), Boolean.valueOf(sortProperty.getSortAscending()));
            }
        }

        return currentSortProperties;
    }

    /**
     * Loads the current year Student Schedule records into a map keyed to the school course OID.
     * This is nested in a
     * Map keyed to the student OID.
     */
    private void loadStudentSchedules() {
        m_scheduleCriteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER + Section.COL_SCHEDULE_TERM_OID,
                m_termEndDateMap.keySet());
        m_scheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, m_scheduleCriteria);
        query.addOrderByAscending(StudentSchedule.COL_STUDENT_OID);
        query.addOrderByAscending(StudentSchedule.COL_SECTION_OID);

        m_studentScheduleMap = getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 4096);
    }

    /**
     * Loads a map of the end date of the current year schedule terms keyed to the term OID.
     */
    private void loadTermDates() {
        /*
         * Query for current term dates in the current year and for the students in the selected
         * school(s)
         */
        Criteria criteria = new Criteria();
        criteria.addGreaterOrEqualThan(ScheduleTermDate.COL_END_DATE, new PlainDate());
        criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        if (!m_schoolOids.isEmpty() && m_schoolOids.size() > 1) {
            X2Criteria schoolsCriteria = new X2Criteria();

            Selection selObject = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());
            for (String oid : m_schoolOids) {
                SelectionObject object = X2BaseBean.newInstance(SelectionObject.class, getBroker().getPersistenceKey());
                object.setObjectOid(oid);
                selObject.addToSelectionObjects(object);
            }
            getBroker().saveBean(selObject);

            Criteria subCriteria = new Criteria();
            subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, selObject.getOid());
            subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX
                    + X2BaseBean.COL_OID);

            schoolsCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));

            criteria.addAndCriteria(
                    schoolsCriteria.copyWithAdjustedPath(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                            ScheduleTerm.REL_SCHEDULE,
                            ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                                    Schedule.COL_SCHOOL_OID));

            m_temporaryBeans.add(selObject);
            m_temporaryBeans.addAll(selObject.getSelectionObjects());
        } else if (!m_schoolOids.isEmpty()) {
            criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, m_schoolOids.iterator().next());
        }


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
                    m_termEndDateMap.put(term.getOid(), termDate.getEndDate());
                }

                lastTerm = term;
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Build transcript map according to input definitions. Two maps are built - one of all
     * transcripts keyed to
     * student OID and a nested map by course number for tracking repeats.
     *
     * @return QueryByCriteria
     */
    private void loadTranscriptsMaps() {
        m_transcriptMap = new HashMap<String, Collection<Transcript>>(4096);
        m_transcriptsByNumber = new HashMap<String, Map<String, Collection<Transcript>>>(4096);

        /*
         * Load transcript for students with programs of studies included in the report
         */
        m_transcriptCriteria.addIn(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_GRADE_LEVEL,
                m_gradeLevels);

        /*
         * Execute query and load maps
         */
        QueryByCriteria query = new QueryByCriteria(Transcript.class, m_transcriptCriteria);
        query.addOrderByAscending(Transcript.COL_STUDENT_OID);
        query.addOrderBy(Transcript.COL_COURSE_DESCRIPTION, true);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisStudent lastStudent = null;
            Map<String, Collection<Transcript>> numberMap = new HashMap<String, Collection<Transcript>>(128);
            Collection<Transcript> transcripts = new LinkedList<Transcript>();

            while (iterator.hasNext()) {
                Transcript transcript = (Transcript) iterator.next();
                SisStudent student = transcript.getStudent();
                SchoolCourse course = transcript.getSchoolCourse();

                if (course != null) {
                    String courseCode = StringUtils.coalesce(getCourseCode(course), course.getNumber());

                    if (!ObjectUtils.match(student, lastStudent)) {
                        numberMap = new HashMap<String, Collection<Transcript>>(128);
                        transcripts = new LinkedList<Transcript>();

                        m_transcriptsByNumber.put(student.getOid(), numberMap);
                        m_transcriptMap.put(student.getOid(), transcripts);
                    }

                    // Add record to collection
                    transcripts.add(transcript);

                    // Add record to number map
                    Collection<Transcript> numberList = numberMap.get(courseCode);
                    if (numberList == null) {
                        numberList = new LinkedList<Transcript>();
                        numberMap.put(courseCode, numberList);
                    }
                    numberList.add(transcript);

                    lastStudent = student;
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Pulls the assessment's extended dictionary from the map. If this is the first encounter of
     * the assessment
     * definition the dictionary is loaded.
     *
     * @param definition AssessmentDefinition
     * @return DataDictionary
     */
    private DataDictionary lookupAssessmentDictionary(AssessmentDefinition definition) {
        DataDictionary dictionary = m_assessmentDictionaries.get(definition.getOid());

        if (dictionary == null) {
            dictionary = DataDictionary.getDistrictDictionary(definition, getBroker().getPersistenceKey());
            m_assessmentDictionaries.put(definition.getOid(), dictionary);
        }

        return dictionary;
    }

    /**
     * Get the requirement code that the transcript was applied to.
     *
     * @param transcript Transcript
     * @param creditsByCourse HashMap<String,Double>
     * @param coursesTaking HashMap<String,List<SchoolCourse>>
     * @param programStudies GraduationProgram
     * @param status String
     * @return String
     */
    private String lookupRequirementCode(Transcript transcript,
                                         HashMap<String, Double> creditsByCourse,
                                         HashMap<String, List<SchoolCourse>> coursesTaking,
                                         GraduationProgram programStudies,
                                         String status) {
        String code = null;
        String requirementOid = null;

        // Check for records with the transcript OID included (course | requirement | transcript)
        for (String key : creditsByCourse.keySet()) {
            if (key.contains(transcript.getOid())) {
                List<String> oids = StringUtils.convertDelimitedStringToList(key, "|");
                requirementOid = oids.get(1);
            }
        }

        // If not found check for match based on course OID (course | requirement)
        for (String key : creditsByCourse.keySet()) {
            if (key.contains(transcript.getSchoolCourseOid()) && key.contains("|")) {
                List<String> oids = StringUtils.convertDelimitedStringToList(key, "|");
                requirementOid = oids.get(1);
            }
        }

        if (!StringUtils.isEmpty(requirementOid)) {
            code = m_requirementCodeMap.get(requirementOid);
        } else {
            for (String reqOid : coursesTaking.keySet()) {
                List<SchoolCourse> courses = coursesTaking.get(reqOid);
                if (courses.contains(transcript.getSchoolCourse())) {
                    code = m_requirementCodeMap.get(reqOid);
                    break;
                }
            }
        }

        if (code == null && !SchoolTranscriptData.STATUS_INVALID.equals(status)) {
            code = m_schoolTranscriptData.determineCodeByRequirementPriority(transcript.getSchoolCourse(),
                    getRequirementsOfProgram(programStudies));
        }

        return code;
    }

    /**
     * Get the requirement code that the student schedule is being applied to.
     *
     * @param schedule StudentSchedule
     * @param coursesTaking HashMap<String,List<SchoolCourse>>
     * @return String
     */
    private String lookupRequirementCode(StudentSchedule schedule, HashMap<String, List<SchoolCourse>> coursesTaking) {
        String code = null;

        for (String requirementOid : coursesTaking.keySet()) {
            List<SchoolCourse> courses = coursesTaking.get(requirementOid);
            if (courses.contains(schedule.getSection().getSchoolCourse())) {
                code = m_requirementCodeMap.get(requirementOid);
                break;
            }
        }

        return code;
    }

    /**
     * Process the course information from the graduation information and prepare course student
     * grid.
     *
     * @param student Student
     * @param creditsByCourse HashMap<String,Double>
     * @param coursesTaking HashMap<String,List<SchoolCourse>>
     * @param creditsInProgress HashMap<String,Double>
     * @param programStudies GraduationProgram
     * @return ReportDataGrid
     */
    private ReportDataGrid processCourses(Student student,
                                          HashMap<String, Double> creditsByCourse,
                                          HashMap<String, List<SchoolCourse>> coursesTaking,
                                          HashMap<String, Double> creditsInProgress,
                                          GraduationProgram programStudies) {
        ReportDataGrid subgrid = new ReportDataGrid();

        /*
         * Iterate over transcript records and track if they are repeats (a repeat is a duplicate
         * course number with
         * a lower grade).
         */
        Collection<String> repeatOids = new LinkedList<String>();
        Map<String, Collection<Transcript>> transcriptsByNumber = m_transcriptsByNumber.get(student.getOid());

        if (transcriptsByNumber != null) {
            for (Entry<String, Collection<Transcript>> entry : transcriptsByNumber.entrySet()) {
                List<Transcript> transcripts = new ArrayList(entry.getValue());
                Collections.sort(transcripts, m_schoolTranscriptData.new BcRepeatTranscriptComparator());

                Transcript highGradeTranscript = null;

                for (Transcript currentTranscript : transcripts) {
                    if (highGradeTranscript == null) {
                        highGradeTranscript = currentTranscript;
                    } else {
                        if (compareTranscriptGrades(currentTranscript, highGradeTranscript) > 0) {
                            repeatOids.add(highGradeTranscript.getOid());
                            highGradeTranscript = currentTranscript;
                        } else {
                            repeatOids.add(currentTranscript.getOid());
                        }
                    }
                }
            }
        }

        // Track section OIDs so schedule records are not added on top of transcripts
        Collection<String> sectionOids = new LinkedList<String>();

        /*
         * Add transcript records
         */
        Collection<Transcript> transcripts = m_transcriptMap.get(student.getOid());
        if (!CollectionUtils.isEmpty(transcripts)) {
            for (Transcript transcript : transcripts) {
                SchoolCourse schoolCourse = transcript.getSchoolCourse();
                boolean isRepeat = checkRepeat(transcript, repeatOids);
                sectionOids.add(transcript.getMasterScheduleOid());

                subgrid.append();

                String trax = (String) transcript.getFieldValueByAlias(TRANSCRIPT_TRAX_ALIAS);
                boolean examinable = m_schoolTranscriptData.lookupExamStatus(transcript.getSchoolCourse());
                String status = m_schoolTranscriptData.checkStatus(transcript, examinable, trax);

                BigDecimal earnedCredit = null;
                BigDecimal potentialCredit = null;
                if (SchoolTranscriptData.STATUS_COMPLETE.equals(status)) {
                    earnedCredit = isRepeat ? BigDecimal.ZERO : transcript.getTotalCredit();

                } else if (SchoolTranscriptData.STATUS_PROGRESS.equals(status)) {
                    potentialCredit = schoolCourse.getCredit();
                } else if (isRepeat) {
                    // For uncompleted transcript or failed transcript, the earnedCredit should be
                    // 0.
                    earnedCredit = BigDecimal.ZERO;
                }

                subgrid.set(FIELD_SCHOOL_COURSE, schoolCourse);
                subgrid.set(FIELD_GRADE_LEVEL, schoolCourse.getGradeLevel());
                subgrid.set(FIELD_COURSE_DESCRIPTION, schoolCourse.getDescription());
                subgrid.set(FIELD_COURSE_CODE, schoolCourse.getNumber());
                subgrid.set(FIELD_DIPLOMA_CATEGORY, isRepeat ? null
                        : lookupRequirementCode(transcript, creditsByCourse, coursesTaking, programStudies, status));
                subgrid.set(FIELD_TRANSCRIPT, transcript);
                subgrid.set(FIELD_INTERIM_MARK, getInterimMark(transcript));
                subgrid.set(FIELD_IS_REPEAT, Boolean.valueOf(isRepeat));
                subgrid.set(FIELD_POTENTIAL_CREDIT, potentialCredit);
                subgrid.set(FIELD_EARNED_CREDIT, earnedCredit);
                subgrid.set(FIELD_ZERO_CREDIT, getZeroCreditDesignation(transcript, isRepeat));

                /*
                 * Completion date can either be the completion date or course end date
                 */
                String dateAsString =
                        StringUtils.coalesce((String) transcript.getFieldValueByAlias(ALIAS_COMPLETION_DATE),
                                (String) transcript.getFieldValueByAlias(ALIAS_COURSE_END_DATE));

                try {
                    subgrid.set(FIELD_COMPLETION_DATE, m_dateConverter.parseSystemString(dateAsString));
                } catch (Exception e) {
                    AppGlobals.getLog().warning("Unable to parse date string: " + dateAsString);
                }

                /*
                 * Set assessment fields on the grid
                 */
                String sectionOid = transcript.getMasterScheduleOid();
                String courseOid = transcript.getSchoolCourseOid();
                String courseCode = schoolCourse.getCourse().getRootCourse().getNumber();
                List<StudentAssessment> assessments =
                        getAssociatedAssessments(student, sectionOid, courseOid, courseCode);

                if (!CollectionUtils.isEmpty(assessments)) {
                    String dateDisplay = "";
                    String examDisplay = "";
                    String markDisplay = "";

                    for (StudentAssessment assessment : assessments) {
                        DataDictionary dictionary = lookupAssessmentDictionary(assessment.getAssessmentDefinition());

                        // Ensure the assessment is valid for this transcript.
                        if (isAssessmentValidForTranscript(assessment, transcript, dictionary)) {
                            /*
                             * Date/Exam/Final 1
                             */
                            String date = (String) assessment.getFieldValueByAlias(EXAM_DATE_1_ALIAS, dictionary);
                            String exam = (String) assessment.getFieldValueByAlias(EXAM_MARK_1_ALIAS, dictionary);
                            String mark = (String) assessment.getFieldValueByAlias(FINAL_MARK_1_ALIAS, dictionary);

                            dateDisplay += StringUtils.isEmpty(date) ? "" : (date + "\n");
                            examDisplay += StringUtils.isEmpty(exam) ? "" : (exam + "\n");
                            markDisplay += StringUtils.isEmpty(mark) ? "" : (mark + "\n");

                            /*
                             * Date/Exam/Final 2
                             */
                            date = (String) assessment.getFieldValueByAlias(EXAM_DATE_2_ALIAS, dictionary);
                            exam = (String) assessment.getFieldValueByAlias(EXAM_MARK_2_ALIAS, dictionary);
                            mark = (String) assessment.getFieldValueByAlias(FINAL_MARK_2_ALIAS, dictionary);

                            dateDisplay += StringUtils.isEmpty(date) ? "" : (date + "\n");
                            examDisplay += StringUtils.isEmpty(exam) ? "" : (exam + "\n");
                            markDisplay += StringUtils.isEmpty(mark) ? "" : (mark + "\n");
                        }
                    }

                    subgrid.set(FIELD_PROV_EXAM_DATE, dateDisplay.trim());
                    subgrid.set(FIELD_EXAM_MARK, examDisplay.trim());
                    subgrid.set(FIELD_FINAL_MARK, markDisplay.trim());
                }

                /*
                 * Calculate notes for current record
                 */
                subgrid.set(FIELD_NOTES, determineNotes(transcript, assessments));
            }
        }

        /*
         * Add records from current schedule that are not already included as a transcript record.
         */
        Collection<StudentSchedule> studentSchedules = m_studentScheduleMap.get(student.getOid());
        if (!CollectionUtils.isEmpty(studentSchedules)) {
            for (StudentSchedule schedule : studentSchedules) {
                if (!sectionOids.contains(schedule.getSectionOid())) {
                    SchoolCourse schoolCourse = schedule.getSection().getSchoolCourse();

                    subgrid.append();
                    subgrid.set(FIELD_SCHOOL_COURSE, schoolCourse);
                    subgrid.set(FIELD_GRADE_LEVEL, schoolCourse.getGradeLevel());
                    subgrid.set(FIELD_COURSE_DESCRIPTION, schoolCourse.getDescription());
                    subgrid.set(FIELD_COURSE_CODE, schoolCourse.getNumber());
                    subgrid.set(FIELD_DIPLOMA_CATEGORY, lookupRequirementCode(schedule, coursesTaking));
                    subgrid.set(FIELD_POTENTIAL_CREDIT, schoolCourse.getCredit());
                    subgrid.set(FIELD_COMPLETION_DATE, getTermEndDate(schedule.getSection()));
                }
            }
        }

        List<String> sort = Arrays.asList(new String[] {FIELD_GRADE_LEVEL, FIELD_COURSE_CODE, FIELD_COMPLETION_DATE});
        List<Boolean> order = Arrays.asList(new Boolean[] {Boolean.TRUE, Boolean.TRUE, Boolean.TRUE});
        subgrid.sort(sort, order, false);
        subgrid.beforeTop();

        return subgrid;
    }

    /**
     * Determines if an assessment is valid for the transcript. Allows for additional criteria to be
     * used
     * to limit which assessments display on the report.
     *
     * @param assessment StudentAssessment
     * @param transcript Transcript
     * @param dictionary DataDictionary
     * @return boolean
     */
    private boolean isAssessmentValidForTranscript(StudentAssessment assessment,
                                                   Transcript transcript,
                                                   DataDictionary dictionary) {
        boolean isValid = false;

        // If both objects have a matching (non-null) master schedule oid, the assessment is valid.
        if (!StringUtils.isEmpty(assessment.getMasterScheduleOid())
                && !StringUtils.isEmpty(transcript.getMasterScheduleOid()) &&
                assessment.getMasterScheduleOid().equals(transcript.getMasterScheduleOid())) {
            isValid = true;
        }

        // If the assessment is not linked by master schedule oid, then both the course code
        // (FieldB010) and Course End Date must match.
        if (!isValid) {
            String assessmentCourseEndDate =
                    (String) assessment.getFieldValueByAlias(EXAM_COURSE_END_DATE_ALIAS, dictionary);
            String transcriptCourseEndDate = (String) transcript.getFieldValueByAlias(ALIAS_COURSE_END_DATE);

            String assessmentCourseCode = assessment.getFieldB010();
            String transcriptCourseCode = transcript.getFieldA128();

            // A null-null date is a match, also empty string/empty string date is a match.
            if (doesCourseCodeMatch(assessmentCourseCode, transcriptCourseCode) &&
                    StringUtils.isEqual(assessmentCourseEndDate, transcriptCourseEndDate)) {
                isValid = true;
            }
        }

        return isValid;
    }

    /**
     * Prepare the data for the graduation requirements sub report.
     *
     * @param student SisStudent
     * @param program GraduationStudentProgram
     * @param grid ReportDataGrid
     * @throws FilterException exception
     * @throws JDOMException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void processGradRequirements(SisStudent student, GraduationStudentProgram program, ReportDataGrid grid)
            throws FilterException, JDOMException, IOException {
        /*
         * Get a map of the courses with partial credit course requirements.
         */
        X2Criteria partialCourseReqCriteria = new X2Criteria();
        partialCourseReqCriteria.addEqualTo(GraduationCourseRequirement.REL_REQUIREMENT + ModelProperty.PATH_DELIMITER +
                GraduationRequirement.COL_PROGRAM_STUDIES_OID, program.getProgramStudiesOid());
        partialCourseReqCriteria.addNotEqualTo(GraduationCourseRequirement.COL_PARTIAL_CREDIT, DOUBLE_ZERO);

        QueryByCriteria partialQuery = new QueryByCriteria(GraduationCourseRequirement.class, partialCourseReqCriteria);
        Map<String, List<GraduationCourseRequirement>> partialCourseRequirments =
                getBroker().getGroupedCollectionByQuery(partialQuery,
                        GraduationCourseRequirement.COL_COURSE_OID,
                        100);

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

        m_graduationManager.determineGraduationStatus(student,
                null,
                program.getProgramStudiesOid(),
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
                partialCourseRequirments,
                new HashMap<String, Map<String, String>>(),
                otherRequirementValues,
                satisfiedOtherRequirementOids,
                null,
                new LinkedList<String>());

        /*
         * Set course information
         */
        ReportDataGrid subgrid =
                processCourses(student, creditsByCourse, coursesTaking, creditsInProgress, program.getProgramStudies());
        grid.set(TRANSCRIPTS_GRID, subgrid);

        /*
         * Set requirement information
         */
        subgrid = new ReportDataGrid();
        subgrid.append();
        int batchCounter = 0;

        Collection<GraduationRequirement> requirements = getRequirementsOfProgram(program.getProgramStudies());
        if (!CollectionUtils.isEmpty(requirements)) {
            for (GraduationRequirement requirement : requirements) {
                String requirementOid = requirement.getOid();
                ++batchCounter;

                if (batchCounter == 4) {
                    batchCounter = 1;
                    subgrid.append();
                }

                subgrid.set(CODE_DESC_PREFIX + batchCounter,
                        requirement.getCode() + "-" + requirement.getDescription());
                BigDecimal requiredUnit = requirement.getRequiredUnit();
                double progressCredits = m_graduationManager
                        .getRequirementCreditsInProgress(coursesTaking.get(requirement.getOid()), creditsInProgress);

                double completeStatus = m_graduationManager.getRequirementSatisfiedStatus(requirementOid,
                        creditsGained,
                        creditsWaived,
                        creditsRequired,
                        satisfiedOtherRequirementOids);
                double progressStatus = m_graduationManager.getRequirementInprogressStatus(requirementOid,
                        progressCredits,
                        creditsRequired);

                boolean requirementsMeet = (completeStatus + progressStatus) >= 100;
                subgrid.set(UNIT_PREFIX + batchCounter, requiredUnit);
                subgrid.set(MET_PREFIX + batchCounter, Boolean.valueOf(requirementsMeet));
            }
        }

        subgrid.beforeTop();
        grid.set(REQUREMENTS_GRID, subgrid);
    }
}
