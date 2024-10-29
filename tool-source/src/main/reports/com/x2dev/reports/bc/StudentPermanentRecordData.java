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
import com.follett.fsc.core.framework.persistence.CollectionCriteriaHelper;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.StudentAlert;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.GradeLevelHistory;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.assessment.RubricManager;
import com.x2dev.sis.tools.reports.RubricTranscriptReportGrid;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.sis.tools.reports.TranscriptReportGrid;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import java.io.ByteArrayInputStream;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Report to pull various student information including demographics, elementary grades/attendance,
 * and
 * middle/secondary grades/attendance.
 *
 * @author Follett Software Company
 */
public class StudentPermanentRecordData extends ReportJavaSourceNet {
    /*
     * Report columns
     */
    private static final String COL_STUDENT = "student";
    private static final String COL_HAS_LEGAL_ALERTS = "hasLegalAlerts";
    private static final String COL_HAS_MEDICAL_ALERTS = "hasMedicalAlerts";
    private static final String COL_SUBREPORT_DATA_SCHOOLING_RECORD = "reportData2";
    private static final String COL_SUBREPORT_DATA_STUDENT_RECORD_INCLUSIONS = "reportData3";
    private static final String COL_SUBREPORT_DATA_ACHIEVEMENT_ATTENDANCE_RECORD = "reportData4";
    private static final String COL_SUBREPORT_DATA_ACHIEVEMENT_ATTENDANCE = "reportData5";

    /*
     * Sub-report IDs
     */
    private static final String SUBREPORT_ID_SCHOOLING_RECORD = "BC-STD-002-ENR";
    private static final String SUBREPORT_ID_STUDENT_RECORD_INCLUSIONS = "BC-STD-002-INC";
    private static final String SUBREPORT_ID_ACHIEVEMENT_ATTENDANCE_RECORD = "BC-STD-002-ACH";
    private static final String SUBREPORT_ID_ACHIEVEMENT_ATTENDANCE = "BC-STD-002-ATT";

    /*
     * Grid fields
     */
    private static final String FIELD_CONTEXT = "context";
    private static final String FIELD_COURSE = "course";
    private static final String FIELD_COURSES = "courses";
    private static final String FIELD_DAILY_ABSENCES = "dailyAbsences";
    private static final String FIELD_GRADE_LEVEL = "grade";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_TRANSCRIPT = "transcript";

    /*
     * Enrollment grid fields
     */
    private static final String FIELD_ENTRY = "entry";
    private static final String FIELD_WITHDRAWAL = "withdrawal";
    private static final String FIELD_WITHDRAWAL_REASON = "reason";

    /*
     * Secondary grid fields
     */
    private static final String FIELD_ASSESSMENT_DATE = "assessmentDate";
    private static final String FIELD_COMPLETION_DATE = "completionDate";
    private static final String FIELD_LETTER_GRADE = "letterGrade";
    private static final String FIELD_NUMERIC_GRADE = "numericGrade";

    /*
     * Inclusion grid fields & constants
     */
    private static final String DESCRIPTION_HOMESCHOOL = "Home Schooled";
    private static final String DESCRIPTION_LEGAL = "Legal document on file that impact student";
    private static final String DESCRIPTION_MEDICAL = "Medical Condition Exists - please see student file";
    private static final String FIELD_INCUSION_DATE = "date";
    private static final String FIELD_INCLUSION_DESCRIPTION = "description";
    private static final String FIELD_INCLUSION_EXPIRY = "expiryDate";
    private static final String SUBGRADE_HS = "HS";

    /*
     * Elementary constants
     */
    private static final List<String> COMMENT_COURSE_NUMBERS =
            Arrays.asList("XTC---K", "XTC--01", "XTC--02", "XTC--03");
    private static final String PROMOTION_COMMENT_HEADER = "Tri 3 Com";

    /*
     * Report format variables
     */
    private static final String REPORT_FORMAT_SCHOOLING_RECORD = "reportFormat2";
    private static final String REPORT_FORMAT_STUDENT_RECORD_INCLUSIONS = "reportFormat3";
    private static final String REPORT_FORMAT_ACHIEVEMENT_ATTENDANCE_RECORD = "reportFormat4";
    private static final String REPORT_FORMAT_ACHIEVEMENT_ATTENDANCE = "reportFormat5";

    /*
     * Report parameters
     */
    private static final String ACTIVE_ONLY_PARAM = "activeOnly";
    private static final String COUNTRY_TABLE_OID = "birthCountryTableOid";
    private static final String GRADUATION_CODE_PARAM = "graduationCodeMap";
    private static final String GRADUATION_PROGRAM_PARAM = "graduationProgramMap";
    private static final String INCLUDE_SECONDARY_PARAM = "secondaryStudent";
    private static final String INITIAL_REASON_PARAM = "initialReasonMap";
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String SORT_PARAM = "sort";

    /*
     * Other constants
     */
    private static final String EVENT_INCLUSION_KEYWORD = "Inclusion";
    private static final int INITIAL_MAP_SIZE = 4096;

    /*
     * Alias fields
     */
    private static final String ASSESSMENT_DATE_1_ALIAS = "examSessionDate1";
    private static final String ASSESSMENT_DATE_2_ALIAS = "examSessionDate2";
    private static final String ASSESSMENT_EXAM_1_ALIAS = "examMark1";
    private static final String ASSESSMENT_EXAM_2_ALIAS = "examMark2";
    private static final String BIRTH_COUNTRY_ALIAS = "std-country-of-birth";
    private static final String COMPLETION_DATE_ALIAS = "trn-completion-date";
    private static final String COURSE_END_DATE_ALIAS = "trn-end-date";
    private static final String HIDE_ON_REPORT_ALIAS = "hide-on-psr";
    private static final String STUDENT_SUBGRADE = "std-sub-grade";
    private static final String TRANSCRIPT_EXAM_ALIAS = "trn-prov-exam-mark";

    private Map<String, Map<String, BigDecimal>> m_absencesCount;
    private Map<String, DataDictionary> m_assessmentDictionaryMap;
    private Collection<DistrictSchoolYearContext> m_contexts;
    private SisStudent m_currentStudent;
    private Collection<String> m_elementaryGradeLevels;
    private EnrollmentManager m_enrollmentManager;
    private Map<String, ReferenceCode> m_entryCodesMap;
    private DateAsStringConverter m_dateStringConverter;
    private GradesManager m_gradesManager;
    private StudentContextReportHelper m_helper;
    private Map<String, TranscriptColumnDefinition> m_finalColumnMap;
    private Map<String, String> m_initialReasonMap;
    private Map<String, Collection<RubricRatingScalePoints>> m_ratingScaleMap;
    private Map<String, ReferenceCode> m_withdrawalCodesMap;

    /*
     * Student data maps
     */
    private Map<String, Map<String, Collection<StudentAssessment>>> m_assessmentMap;
    private Map<String, ReportDataGrid> m_elementaryTranscriptMap;
    private Map<String, Collection<StudentEventTracking>> m_inclusionMap;
    private Map<String, Collection<Transcript>> m_secondaryTranscriptMap;
    private Map<String, Collection<StudentAlert>> m_studentLegalAlert;
    private Map<String, Collection<StudentAlert>> m_studentMedicalAlert;
    private Map<String, Collection<StudentSchedule>> m_scheduleMap;

    /*
     * Data criteria
     */
    private X2Criteria m_assessmentCriteria;
    private X2Criteria m_dailyCriteria;
    private X2Criteria m_elementaryCriteria;
    private X2Criteria m_eventCriteria;
    private X2Criteria m_graduationCriteria;
    private X2Criteria m_legalCriteria;
    private X2Criteria m_medicalCriteria;
    private X2Criteria m_secondaryCriteria;
    private X2Criteria m_scheduleCriteria;
    private X2Criteria m_studentCriteria;

    private CollectionCriteriaHelper m_studentCollectionCriteriaHelper;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        grid = buildReportData(buildStudentQuery(m_studentCriteria), grid);

        boolean tempTableInUse = isCurrentQueryContainsUnion();
        if (m_studentCollectionCriteriaHelper != null && !tempTableInUse) {
            m_studentCollectionCriteriaHelper.cleanup();
        }

        if (!tempTableInUse) {
            getBroker().commitTransaction(); // end transaction to dispose of temp table created in
                                             // buildCriteria
        }

        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_helper = new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

        collectReportEntities();

        if (!isCurrentQueryContainsUnion()) {
            getBroker().beginTransaction(); // begin transaction for temp table created in
                                            // buildCritera
        }
        buildCriteria();

        m_studentLegalAlert = loadStudentAlerts(m_legalCriteria);
        m_studentMedicalAlert = loadStudentAlerts(m_medicalCriteria);

        m_elementaryTranscriptMap = loadTranscriptGrid(m_elementaryCriteria);
        m_secondaryTranscriptMap = loadTranscriptMap(m_secondaryCriteria);

        loadAssessmentMap();
        loadEventMap();
        loadScheduleMap();
        loadGraduationPrograms();

        m_absencesCount = new HashMap<String, Map<String, BigDecimal>>();
        m_assessmentDictionaryMap = new HashMap<String, DataDictionary>(256);
        m_dateStringConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                getLocale(), true);
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_gradesManager = new GradesManager(getBroker());
        m_initialReasonMap = new HashMap<String, String>(INITIAL_MAP_SIZE);

        loadTranscriptColumns();
        initializeRatingScaleMap();

        /*
         * Load entry codes
         */
        m_entryCodesMap = new HashMap<String, ReferenceCode>(256);
        String tableOid = PreferenceManager.getPreferenceValue(getOrganization().getRootOrganization(),
                SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        if (!StringUtils.isEmpty(tableOid)) {
            ReferenceTable table = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, tableOid);
            for (ReferenceCode code : table.getReferenceCodes(getBroker())) {
                m_entryCodesMap.put(code.getCode(), code);
            }
        }

        /*
         * Load withdrawal codes
         */
        m_withdrawalCodesMap = new HashMap<String, ReferenceCode>(256);
        tableOid = PreferenceManager.getPreferenceValue(getOrganization().getRootOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
        if (!StringUtils.isEmpty(tableOid)) {
            ReferenceTable table = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, tableOid);
            for (ReferenceCode code : table.getReferenceCodes(getBroker())) {
                m_withdrawalCodesMap.put(code.getCode(), code);
            }
        }

        /*
         * Get reference table OIDs for description lookup on format
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        // Birth country
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(BIRTH_COUNTRY_ALIAS);
        if (field != null) {
            addParameter(COUNTRY_TABLE_OID, field.getReferenceTableOid());
        }

        /*
         * School Year
         */
        QueryByCriteria yearQuery = new QueryByCriteria(DistrictSchoolYearContext.class, new X2Criteria());
        m_contexts = getBroker().getCollectionByQuery(yearQuery);
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
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Adds the assessment to the map if the exam score is the highest in the map.
     *
     * @param assessmentMap Map<String,Collection<StudentAssessment>>
     * @param key String
     * @param assessment StudentAssessment
     */
    private void addAssessment(Map<String, Collection<StudentAssessment>> assessmentMap,
                               String key,
                               StudentAssessment assessment) {
        Collection<StudentAssessment> assessments = assessmentMap.get(key);

        if (assessments == null) {
            assessments = new LinkedList<StudentAssessment>();
            assessmentMap.put(key, assessments);
        }

        assessments.add(assessment);
    }

    /**
     * Add additional parameters for use in the report format.
     */
    private void addParameters() {
        addParameter(INITIAL_REASON_PARAM, m_initialReasonMap);
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

            SubQuery schoolQuery = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, schoolCriteria);
            studentCriteria.addIn(prefix + m_helper.getSchoolOidField(), schoolQuery);
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
     * Build criteria for students and related data.
     */
    private void buildCriteria() {
        m_studentCriteria = new X2Criteria();
        m_assessmentCriteria = new X2Criteria();
        m_dailyCriteria = new X2Criteria();
        m_eventCriteria = new X2Criteria();
        m_graduationCriteria = new X2Criteria();
        m_scheduleCriteria = new X2Criteria();

        X2Criteria transcriptCriteria = new X2Criteria();
        X2Criteria alertCriteria = new X2Criteria();

        boolean tempTableInUse = isCurrentQueryContainsUnion();

        if (m_currentStudent != null) {
            m_studentCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
            m_assessmentCriteria.addEqualTo(StudentAssessment.COL_STUDENT_OID, m_currentStudent.getOid());
            m_dailyCriteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, m_currentStudent.getOid());
            m_eventCriteria.addEqualTo(StudentEventTracking.COL_STUDENT_OID, m_currentStudent.getOid());
            m_scheduleCriteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, m_currentStudent.getOid());
            m_graduationCriteria.addEqualTo(GraduationStudentProgram.COL_STUDENT_OID, m_currentStudent.getOid());
            transcriptCriteria.addEqualTo(Transcript.COL_STUDENT_OID, m_currentStudent.getOid());
            alertCriteria.addEqualTo(StudentAlert.COL_STUDENT_OID, m_currentStudent.getOid());
        } else {
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(m_studentCriteria, queryBy.replace("student.", ""), queryString, SisStudent.class,
                    SisStudent.class, X2BaseBean.COL_OID);
            if (tempTableInUse) {
                addUserCriteria(m_assessmentCriteria, queryBy, queryString, StudentAssessment.class, SisStudent.class,
                        StudentAssessment.COL_STUDENT_OID);
                addUserCriteria(m_dailyCriteria, queryBy, queryString, StudentAttendance.class, SisStudent.class,
                        StudentAttendance.COL_STUDENT_OID);
                addUserCriteria(m_eventCriteria, queryBy, queryString, StudentEventTracking.class, SisStudent.class,
                        StudentEventTracking.COL_STUDENT_OID);
                addUserCriteria(m_graduationCriteria, queryBy, queryString, GraduationStudentProgram.class,
                        SisStudent.class, GraduationStudentProgram.COL_STUDENT_OID);
                addUserCriteria(m_scheduleCriteria, queryBy, queryString, StudentSchedule.class, SisStudent.class,
                        StudentSchedule.COL_STUDENT_OID);
                addUserCriteria(transcriptCriteria, queryBy, queryString, Transcript.class, SisStudent.class,
                        Transcript.COL_STUDENT_OID);
                addUserCriteria(alertCriteria, queryBy, queryString, StudentAlert.class, SisStudent.class,
                        StudentAlert.COL_STUDENT_OID);
            }

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            boolean includeSecondary = ((Boolean) getParameter(INCLUDE_SECONDARY_PARAM)).booleanValue();

            if (!queryBy.contains(CURRENT_KEY)) {
                addSchoolCriteria(m_studentCriteria, "", activeOnly, includeSecondary);
                if (tempTableInUse) {
                    addSchoolCriteria(m_assessmentCriteria, StudentAssessment.REL_STUDENT + PATH_DELIMITER, activeOnly,
                            includeSecondary);
                    addSchoolCriteria(m_dailyCriteria, StudentAttendance.REL_STUDENT + PATH_DELIMITER, activeOnly,
                            includeSecondary);
                    addSchoolCriteria(m_eventCriteria, StudentEventTracking.REL_STUDENT + PATH_DELIMITER, activeOnly,
                            includeSecondary);
                    addSchoolCriteria(m_graduationCriteria, GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER,
                            activeOnly, includeSecondary);
                    addSchoolCriteria(m_scheduleCriteria, StudentSchedule.REL_STUDENT + PATH_DELIMITER, activeOnly,
                            includeSecondary);
                    addSchoolCriteria(transcriptCriteria, Transcript.REL_STUDENT + PATH_DELIMITER, activeOnly,
                            includeSecondary);
                    addSchoolCriteria(alertCriteria, StudentAlert.REL_STUDENT + PATH_DELIMITER, activeOnly,
                            includeSecondary);
                }
            }

            /*
             * Build a SubsetCriteriaWrapper from the student criteria
             */
            if (!tempTableInUse) {
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
                m_studentCollectionCriteriaHelper.applyToCriteria(Transcript.COL_STUDENT_OID, transcriptCriteria);

                m_studentCollectionCriteriaHelper.applyToCriteria(StudentAssessment.COL_STUDENT_OID,
                        m_assessmentCriteria);
                m_studentCollectionCriteriaHelper.applyToCriteria(StudentAttendance.COL_STUDENT_OID, m_dailyCriteria);
                m_studentCollectionCriteriaHelper.applyToCriteria(StudentEventTracking.COL_STUDENT_OID,
                        m_eventCriteria);
                m_studentCollectionCriteriaHelper.applyToCriteria(GraduationStudentProgram.COL_STUDENT_OID,
                        m_graduationCriteria);
                m_studentCollectionCriteriaHelper.applyToCriteria(StudentSchedule.COL_STUDENT_OID, m_scheduleCriteria);
                m_studentCollectionCriteriaHelper.applyToCriteria(StudentAlert.COL_STUDENT_OID, alertCriteria);
            }
        }

        /*
         * Check school course alias "hide-on-psr"
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(HIDE_ON_REPORT_ALIAS);

        if (field != null && SchoolCourse.OBJECT_PREFIX.equals(field.getDataTable().getObjectPrefix())) {
            transcriptCriteria.addAndCriteria(buildHideCriteria(field, Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER));
            m_scheduleCriteria.addAndCriteria(buildHideCriteria(field,
                    StudentSchedule.REL_SECTION + PATH_DELIMITER + Section.REL_SCHOOL_COURSE + PATH_DELIMITER));
        } else {
            AppGlobals.getLog().warning("Alias '" + HIDE_ON_REPORT_ALIAS + "' not found on School Course table");
        }

        /*
         * Transcript criteria is split at grade 5
         */
        m_elementaryGradeLevels = loadGradeLevels(5, true);
        m_elementaryCriteria = transcriptCriteria.copy();
        m_elementaryCriteria.addIn(Transcript.COL_GRADE_LEVEL, m_elementaryGradeLevels);


        m_secondaryCriteria = transcriptCriteria.copy();
        m_secondaryCriteria.addNotIn(Transcript.COL_GRADE_LEVEL, m_elementaryGradeLevels);
        m_scheduleCriteria.addNotIn(StudentSchedule.REL_STUDENT + PATH_DELIMITER + m_helper.getGradeLevelField(),
                m_elementaryGradeLevels);

        /*
         * Filter alerts by type
         */
        m_legalCriteria = alertCriteria.copy();
        m_legalCriteria.addEqualTo(StudentAlert.COL_ALERT_TYPE,
                Integer.valueOf(StudentAlert.AlertType.LEGAL.ordinal()));

        m_medicalCriteria = alertCriteria.copy();
        m_medicalCriteria.addEqualTo(StudentAlert.COL_ALERT_TYPE,
                Integer.valueOf(StudentAlert.AlertType.MEDICAL.ordinal()));

        /*
         * Only include assessments with a related section or course
         */
        X2Criteria sectionCriteria = new X2Criteria();
        sectionCriteria.addNotEmpty(StudentAssessment.COL_MASTER_SCHEDULE_OID, getBroker().getPersistenceKey());

        X2Criteria courseCriteria = new X2Criteria();
        courseCriteria.addNotEmpty(StudentAssessment.COL_SCHOOL_COURSE_OID, getBroker().getPersistenceKey());

        sectionCriteria.addOrCriteria(courseCriteria);
        m_assessmentCriteria.addAndCriteria(sectionCriteria);

        /*
         * Only pull current year schedules
         */
        m_scheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
    }

    /**
     * Build a fourth part of report - Elementary transcript/attendance totals.
     *
     * @param student SisStudent
     * @param rubricGrid ReportDataGrid
     * @return data grid for current sub-report
     */
    private ReportDataGrid buildElementarySubreport(SisStudent student, ReportDataGrid rubricGrid) {
        ReportDataGrid subgrid = new ReportDataGrid();
        Collection<DistrictSchoolYearContext> yearsIncludedInGrid = new HashSet<DistrictSchoolYearContext>();

        if (rubricGrid != null) {
            Map<String, Map<String, String>> progress = new HashMap<String, Map<String, String>>();
            Map<String, String> promotionCommentMap = new HashMap<String, String>(32);

            /*
             * Iterate over the transcripts and load the grades and courses to display by year
             */
            Transcript lastTranscript = null;
            while (rubricGrid.next()) {
                Transcript transcript = (Transcript) rubricGrid.get(TranscriptReportGrid.COL_TRANSCRIPT_HEADER);
                String year = transcript.getDistrictContext().getContextId();

                // If a transcript record has multiple rubric scores - only use the first
                if (!ObjectUtils.match(transcript, lastTranscript)) {
                    // Create an entry in progress for the year even if there are no grades
                    Map<String, String> ratingMap = progress.get(year);
                    if (ratingMap == null) {
                        ratingMap = new TreeMap<String, String>();
                        progress.put(year, ratingMap);
                    }

                    String rating = getRubricRating(rubricGrid);
                    if (!StringUtils.isEmpty(rating)) {
                        String coursesView = ratingMap.get(rating);
                        coursesView =
                                (coursesView != null ? coursesView : "") + " *" + transcript.getCourseDescription();

                        ratingMap.put(rating, coursesView);
                    }

                    if (COMMENT_COURSE_NUMBERS.contains(transcript.getSchoolCourse().getNumber())) {
                        promotionCommentMap.put(year, (String) rubricGrid.get(PROMOTION_COMMENT_HEADER));
                    }
                }

                lastTranscript = transcript;
            }

            /*
             * Iterate over the transcripts once again, but only to get the individual years to
             * display
             */
            rubricGrid.beforeTop();
            while (rubricGrid.next()) {
                Transcript transcript = (Transcript) rubricGrid.get(TranscriptReportGrid.COL_TRANSCRIPT_HEADER);

                DistrictSchoolYearContext context = transcript.getDistrictContext();
                String year = context.getContextId();

                Map<String, String> ratingMap = progress.get(year);
                if (ratingMap != null) {
                    subgrid.append();
                    subgrid.set(FIELD_CONTEXT, context);
                    subgrid.set(FIELD_SCHOOL, transcript.getSchool());
                    subgrid.set(FIELD_GRADE_LEVEL, transcript.getGradeLevel());
                    subgrid.set(FIELD_COURSES, formatRatings(ratingMap, promotionCommentMap.get(year)));
                    subgrid.set(FIELD_DAILY_ABSENCES,
                            getDailyAbsencesCount(transcript.getStudent(), context, transcript.getSchool()));

                    progress.remove(year);
                    yearsIncludedInGrid.add(context);
                }
            }
        }

        /*
         * Include addtional attendance records outside transcript
         */
        buildAdditionalAttendanceData(student, subgrid, yearsIncludedInGrid, m_elementaryGradeLevels);

        // Append a blank row if the grid is empty
        if (subgrid.bottomRowNumber() == -1) {
            subgrid.append();
        }

        String[] sortColumns = new String[] {FIELD_CONTEXT + PATH_DELIMITER + DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                FIELD_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME,
                FIELD_COURSES};
        subgrid.sort(Arrays.asList(sortColumns), true);
        subgrid.beforeTop();

        return subgrid;
    }

    /**
     * Collects additional attendance data that is not driven by transcript and student schedule.
     *
     * @param student SisStudent
     * @param subgrid ReportDataGrid
     * @param yearsIncludedInGrid Collection<DistrictSchoolYearContext>
     * @param includedGradeLevels Collection<String>
     */
    private void buildAdditionalAttendanceData(SisStudent student,
                                               ReportDataGrid subgrid,
                                               Collection<DistrictSchoolYearContext> yearsIncludedInGrid,
                                               Collection<String> includedGradeLevels) {
        /*
         * For every school year the student has daily attendance records, but not in the grid, add
         * the grid.
         */
        GradeLevelHistory gradeLevelHistory =
                new GradeLevelHistory(student.getOid(), 14, student.getOrganization1(), getBroker());

        for (DistrictSchoolYearContext schoolYear : m_contexts) {
            if (!yearsIncludedInGrid.contains(schoolYear)) {
                String gradeLevel = gradeLevelHistory.getGradeLevel(student.getOid(), schoolYear.getSchoolYear());

                if (includedGradeLevels.contains(gradeLevel)) {
                    X2Criteria attendanceSchoolCriteria = m_dailyCriteria.copy();
                    attendanceSchoolCriteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());
                    attendanceSchoolCriteria.addBetween(StudentAttendance.COL_DATE, schoolYear.getStartDate(),
                            schoolYear.getEndDate());
                    attendanceSchoolCriteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

                    SubQuery attendanceSchoolQuery = new SubQuery(StudentAttendance.class,
                            StudentAttendance.COL_SCHOOL_OID, attendanceSchoolCriteria);
                    attendanceSchoolQuery.setDistinct(true);
                    Collection<String> schoolOids = getBroker().getSubQueryCollectionByQuery(attendanceSchoolQuery);

                    for (String schoolOid : schoolOids) {
                        SisSchool currentSchool = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);

                        subgrid.append();
                        subgrid.set(FIELD_CONTEXT, schoolYear);
                        subgrid.set(FIELD_SCHOOL, currentSchool);
                        subgrid.set(FIELD_GRADE_LEVEL, gradeLevel);
                        subgrid.set(FIELD_COURSES, "");
                        subgrid.set(FIELD_DAILY_ABSENCES, getDailyAbsencesCount(student, schoolYear, currentSchool));
                    }
                }
            }
        }
    }

    /**
     * Build a second part of report - Student enrollments.
     *
     * @param student SisStudent
     * @return data grid for current sub-report
     */
    private ReportDataGrid buildEnrollmentSubreport(SisStudent student) {
        ReportDataGrid subgrid = new ReportDataGrid();

        boolean entryFound = false;
        boolean withdrawlFound = false;
        boolean firstEntry = true;

        List<StudentEnrollment> enrollments = m_enrollmentManager.getOrderedEnrollment(student, null, null, null, true);
        for (StudentEnrollment enrollment : enrollments) {
            if (!entryFound && enrollment.getEnrollmentType().equals(StudentEnrollment.ENTRY)) {
                subgrid.append();
                subgrid.set(FIELD_SCHOOL, enrollment.getSchool());
                subgrid.set(FIELD_ENTRY, enrollment);

                entryFound = true;
                withdrawlFound = false;

                if (firstEntry) {
                    String reason = enrollment.getEnrollmentCode();

                    ReferenceCode code = m_entryCodesMap.get(enrollment.getEnrollmentCode());
                    if (code != null) {
                        reason = code.getDescription();
                    }

                    m_initialReasonMap.put(enrollment.getStudentOid(), reason);

                    firstEntry = false;
                }
            }

            if (entryFound && !withdrawlFound && StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                subgrid.set(FIELD_WITHDRAWAL, enrollment);

                String reason = enrollment.getEnrollmentCode();

                ReferenceCode code = m_withdrawalCodesMap.get(enrollment.getEnrollmentCode());
                if (code != null) {
                    reason = code.getDescription();
                }

                subgrid.set(FIELD_WITHDRAWAL_REASON, reason);

                withdrawlFound = true;
                entryFound = false;
            }
        }

        /*
         * Adding empty row in case of empty grid (to render empty table)
         */
        if (subgrid.rowCount() == 0) {
            subgrid.append();
        }

        subgrid.beforeTop();

        return subgrid;
    }

    /**
     * Checks the "hide-on-psr" field on the School Course to not include flagged records.
     *
     * @param field DataDictionaryField
     * @param schoolCoursePath String
     * @return Criteria
     */
    private Criteria buildHideCriteria(DataDictionaryField field, String schoolCoursePath) {
        X2Criteria hideCriteria = new X2Criteria();
        hideCriteria.addEmpty(schoolCoursePath + field.getJavaName(), getBroker().getPersistenceKey());
        hideCriteria.addOrEqualTo(schoolCoursePath + field.getJavaName(), BooleanAsStringConverter.FALSE);

        return hideCriteria;
    }

    /**
     * Build a third part of report - Student inclusions. Inclusions are based on Student Event
     * Tracking records with an
     * event type that begins with "Inclusion".
     *
     * @param student - current student for processing
     *
     * @return data grid for current sub-report
     */
    private ReportDataGrid buildInclusionSubreport(SisStudent student) {
        ReportDataGrid subgrid = new ReportDataGrid();

        /*
         * Check medical alert
         */
        Collection<StudentAlert> alerts = m_studentMedicalAlert.get(student.getOid());
        if (!CollectionUtils.isEmpty(alerts)) {
            subgrid.append();
            subgrid.set(FIELD_INCLUSION_DESCRIPTION, DESCRIPTION_MEDICAL);
        }

        /*
         * Check legal alert
         */
        alerts = m_studentLegalAlert.get(student.getOid());
        if (!CollectionUtils.isEmpty(alerts)) {
            subgrid.append();
            subgrid.set(FIELD_INCLUSION_DESCRIPTION, DESCRIPTION_LEGAL);
        }

        /*
         * Check homeschool
         */
        String subGrade = (String) student.getFieldValueByAlias(STUDENT_SUBGRADE);
        if (SUBGRADE_HS.equals(subGrade)) {
            subgrid.append();
            subgrid.set(FIELD_INCLUSION_DESCRIPTION, DESCRIPTION_HOMESCHOOL);
        }

        /*
         * Check inclusion events
         */
        Collection<StudentEventTracking> events = m_inclusionMap.get(student.getOid());
        if (!CollectionUtils.isEmpty(events)) {
            for (StudentEventTracking event : events) {
                subgrid.append();
                subgrid.set(FIELD_INCUSION_DATE, event.getEventDate());
                subgrid.set(FIELD_INCLUSION_DESCRIPTION, event.getComment());

                String expiryDate = (String) event.getFieldValueByAlias("trk-expiry-date");
                if (!StringUtils.isEmpty(expiryDate)) {
                    subgrid.set(FIELD_INCLUSION_EXPIRY, m_dateStringConverter.parseSystemString(expiryDate));
                }
            }
        } else {
            subgrid.append();
        }

        subgrid.beforeTop();

        return subgrid;
    }

    /**
     * Build report.
     *
     * @param query QueryByCriteria
     * @param grid ReportDataGrid
     * @return data grid for current report
     */
    private ReportDataGrid buildReportData(QueryByCriteria query, ReportDataGrid grid) {
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                grid.append();
                grid.set(COL_STUDENT, student);
                grid.set(COL_HAS_LEGAL_ALERTS, Boolean.valueOf(getStudentAlert(student, StudentAlert.AlertType.LEGAL)));
                grid.set(COL_HAS_MEDICAL_ALERTS, Boolean.valueOf(getStudentAlert(student, StudentAlert.AlertType.MEDICAL)));

                grid.set(COL_SUBREPORT_DATA_SCHOOLING_RECORD, buildEnrollmentSubreport(student));
                grid.set(COL_SUBREPORT_DATA_STUDENT_RECORD_INCLUSIONS, buildInclusionSubreport(student));

                grid.set(COL_SUBREPORT_DATA_ACHIEVEMENT_ATTENDANCE_RECORD,
                        buildElementarySubreport(student, m_elementaryTranscriptMap.get(student.getOid())));
                grid.set(COL_SUBREPORT_DATA_ACHIEVEMENT_ATTENDANCE, buildSecondarySubreport(student,
                        m_secondaryTranscriptMap.get(student.getOid()),
                        m_scheduleMap.get(student.getOid())));
            }
        } finally {
            students.close();
        }

        addParameters();

        grid.beforeTop();
        return grid;
    }

    /**
     * Build a fifth part of report - Secondary transcript/attendance.
     *
     * @param student SisStudent
     * @param transcripts - collection of the student's transcripts
     * @param schedules Collection<StudentSchedule>
     * @return data grid for current sub-report
     */
    private ReportDataGrid buildSecondarySubreport(SisStudent student,
                                                   Collection<Transcript> transcripts,
                                                   Collection<StudentSchedule> schedules) {
        ReportDataGrid subgrid = new ReportDataGrid();
        Collection<DistrictSchoolYearContext> yearsIncludedInGrid = new HashSet<DistrictSchoolYearContext>();

        DistrictSchoolYearContext currentContext = getCurrentContext();
        Collection<String> sectionOids = new LinkedList<>();

        if (!CollectionUtils.isEmpty(transcripts)) {
            for (Transcript transcript : transcripts) {
                SisSchool school = transcript.getSchool();
                DistrictSchoolYearContext context = transcript.getDistrictContext();
                SchoolCourse schoolCourse = transcript.getSchoolCourse();

                subgrid.append();
                subgrid.set(FIELD_TRANSCRIPT, transcript);
                subgrid.set(FIELD_CONTEXT, context);
                subgrid.set(FIELD_SCHOOL, school);
                subgrid.set(FIELD_COURSE, schoolCourse);
                subgrid.set(FIELD_DAILY_ABSENCES,
                        getDailyAbsencesCount(transcript.getStudent(), context, transcript.getSchool()));
                yearsIncludedInGrid.add(context);

                /*
                 * Load the completion date
                 */
                String completionDate =
                        StringUtils.coalesce((String) transcript.getFieldValueByAlias(COMPLETION_DATE_ALIAS),
                                (String) transcript.getFieldValueByAlias(COURSE_END_DATE_ALIAS));
                if (!StringUtils.isEmpty(completionDate)) {
                    subgrid.set(FIELD_COMPLETION_DATE, m_dateStringConverter.parseSystemString(completionDate));
                }

                /*
                 * Load the letter/numeric value for the final grade
                 */
                TranscriptDefinition transcriptDefinition = transcript.getTranscriptDefinition();
                if (transcriptDefinition != null) {
                    TranscriptColumnDefinition finalColumn = m_finalColumnMap.get(transcriptDefinition.getOid());
                    if (finalColumn != null) {
                        String grade =
                                (String) transcript.getFieldValueByBeanPath(finalColumn.getTranscriptBeanAttribute());

                        if (!StringUtils.isEmpty(grade)) {
                            String letterGrade = null;
                            BigDecimal numericGrade = null;

                            if (StringUtils.isNumeric(grade)) {
                                numericGrade = new BigDecimal(grade);
                                letterGrade = m_gradesManager.getLetterValue(numericGrade, finalColumn.getGradeScale(),
                                        transcript.getSchool(), transcript.getSchoolCourseOid());
                            } else {
                                letterGrade = grade;
                            }

                            subgrid.set(FIELD_LETTER_GRADE, letterGrade);
                            subgrid.set(FIELD_NUMERIC_GRADE, numericGrade);
                        }
                    }
                }

                /*
                 * Load assessment-related data
                 */
                Collection<StudentAssessment> assessments = null;
                if (transcript != null) {
                    Map<String, Collection<StudentAssessment>> assessmentsMap =
                            m_assessmentMap.get(transcript.getStudentOid());
                    if (assessmentsMap != null) {
                        assessments = assessmentsMap.get(transcript.getMasterScheduleOid());

                        if (assessments == null) {
                            assessments = assessmentsMap.get(transcript.getSchoolCourseOid());
                        }
                    }
                }

                if (assessments != null) {
                    for (StudentAssessment assessment : assessments) {
                        DataDictionary dictionary = getAssessmentDictionary(assessment.getAssessmentDefinition());

                        String transcriptExam = (String) transcript.getFieldValueByAlias(TRANSCRIPT_EXAM_ALIAS);
                        String assessmentExam1 =
                                (String) assessment.getFieldValueByAlias(ASSESSMENT_EXAM_1_ALIAS, dictionary);
                        String assessmentExam2 =
                                (String) assessment.getFieldValueByAlias(ASSESSMENT_EXAM_2_ALIAS, dictionary);

                        String examDate = null;
                        if (ObjectUtils.match(transcriptExam, assessmentExam1)) {
                            examDate = (String) assessment.getFieldValueByAlias(ASSESSMENT_DATE_1_ALIAS, dictionary);
                        } else if (ObjectUtils.match(transcriptExam, assessmentExam2)) {
                            examDate = (String) assessment.getFieldValueByAlias(ASSESSMENT_DATE_2_ALIAS, dictionary);
                        }

                        if (!StringUtils.isEmpty(examDate)) {
                            // If we found a matching exam date, save date to grid and break loop
                            subgrid.set(FIELD_ASSESSMENT_DATE, examDate);
                            break;
                        }
                    }
                }

                /*
                 * If current year transcript, add section to list to check when adding schedules
                 */
                if (currentContext.getOid().equals(transcript.getDistrictContextOid())) {
                    sectionOids.add(transcript.getMasterScheduleOid());
                }
            }
        }

        /*
         * Add current year sections
         */
        if (schedules != null) {
            for (StudentSchedule schedule : schedules) {
                if (!sectionOids.contains(schedule.getSectionOid())) {
                    subgrid.append();
                    subgrid.set(FIELD_TRANSCRIPT, getTempTranscript(schedule));
                    subgrid.set(FIELD_CONTEXT, currentContext);
                    subgrid.set(FIELD_SCHOOL, schedule.getSchedule().getSchool());
                    subgrid.set(FIELD_COURSE, schedule.getSection().getSchoolCourse());
                    subgrid.set(FIELD_DAILY_ABSENCES, getDailyAbsencesCount(schedule.getStudent(), currentContext,
                            schedule.getSchedule().getSchool()));

                    yearsIncludedInGrid.add(currentContext);
                }
            }
        }

        Collection<String> secondaryGradeLevels = loadGradeLevels(5, false);
        /*
         * Build additional attendance data for the student
         */
        buildAdditionalAttendanceData(student, subgrid, yearsIncludedInGrid, secondaryGradeLevels);

        /*
         * Adding empty row in case of empty grid (to render empty table)
         */
        if (subgrid.rowCount() == 0) {
            subgrid.append();
        }

        /*
         * Sort and reset grid
         */
        String[] sortColumns = new String[] {FIELD_CONTEXT + PATH_DELIMITER + DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                FIELD_TRANSCRIPT + PATH_DELIMITER + Transcript.COL_GRADE_LEVEL,
                FIELD_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME,
                FIELD_COMPLETION_DATE,
                FIELD_COURSE + PATH_DELIMITER + SchoolCourse.COL_DESCRIPTION};
        subgrid.sort(Arrays.asList(sortColumns), true);
        subgrid.beforeTop();

        return subgrid;
    }

    /**
     * Build query for students according to input definitions.
     *
     * @param criteria X2Criteria
     * @return QueryByCriteria
     */
    private QueryByCriteria buildStudentQuery(X2Criteria criteria) {
        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);
        query.addOrderByAscending(m_helper.getSchoolRelationship() + PATH_DELIMITER + SisSchool.COL_NAME);
        query.addOrderByAscending(m_helper.getSchoolOidField());
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        return query;
    }

    /**
     * Loading appropriate sub-report formats.
     */
    private void collectReportEntities() {
        // Collect report entities
        Report report2 = ReportUtils.getReport(SUBREPORT_ID_SCHOOLING_RECORD, getBroker());
        Report report3 = ReportUtils.getReport(SUBREPORT_ID_STUDENT_RECORD_INCLUSIONS, getBroker());
        Report report4 = ReportUtils.getReport(SUBREPORT_ID_ACHIEVEMENT_ATTENDANCE_RECORD, getBroker());
        Report report5 = ReportUtils.getReport(SUBREPORT_ID_ACHIEVEMENT_ATTENDANCE, getBroker());

        // Send them to report parameters as byte-streams
        addParameter(REPORT_FORMAT_SCHOOLING_RECORD, new ByteArrayInputStream(report2.getCompiledFormat()));
        addParameter(REPORT_FORMAT_STUDENT_RECORD_INCLUSIONS, new ByteArrayInputStream(report3.getCompiledFormat()));
        addParameter(REPORT_FORMAT_ACHIEVEMENT_ATTENDANCE_RECORD,
                new ByteArrayInputStream(report4.getCompiledFormat()));
        addParameter(REPORT_FORMAT_ACHIEVEMENT_ATTENDANCE, new ByteArrayInputStream(report5.getCompiledFormat()));
    }

    /**
     * Returns the DataDictionary associated with the assessment defintion. The values are cached in
     * memory for
     * quicker repeated lookup.
     *
     * @param definition AssessmentDefinition
     * @return DataDictionary
     */
    private DataDictionary getAssessmentDictionary(AssessmentDefinition definition) {
        DataDictionary dictionary = m_assessmentDictionaryMap.get(definition.getOid());

        if (dictionary == null) {
            dictionary = DataDictionary.getDistrictDictionary(definition, getBroker().getPersistenceKey());
            m_assessmentDictionaryMap.put(definition.getOid(), dictionary);
        }

        return dictionary;
    }

    /**
     * Get absence count for specified student and school year context. If map to extract
     * a count from is not found (not loaded yet), it will be loaded.<br/>
     * Since we don't care about all school year at once, each of required ones will be
     * loaded on first encounter.
     *
     * @param student SisStudent
     * @param context DistrictSchoolYearContext
     * @param school SisSchool
     * @return absences count as Long
     */
    private BigDecimal getDailyAbsencesCount(SisStudent student, DistrictSchoolYearContext context, SisSchool school) {
        BigDecimal count = null;

        if (student != null && context != null) {
            Map<String, BigDecimal> contextMap = m_absencesCount.get(context.getOid() + school.getOid());
            if (contextMap == null) {
                contextMap = loadDailyAbsencesMap(context, school);
            }

            count = contextMap.get(student.getOid());
        }

        if (count == null) {
            count = BigDecimal.ZERO;
        }

        return count;
    }

    /**
     * Returns the rating scale points map for the passed criterion. If there is no scale associated
     * with the
     * criterion an empty map is returned.
     *
     * @param criterion RubricCriterion
     * @return Collection<RubricRatingScalePoints>
     */
    private Collection<RubricRatingScalePoints> getRatingScaleMap(RubricCriterion criterion) {
        Collection<RubricRatingScalePoints> scales = new LinkedList<RubricRatingScalePoints>();

        if (criterion != null) {
            RubricRatingScale scale = criterion.getRubricRatingScale();
            if (scale != null) {
                scales = m_ratingScaleMap.get(scale.getOid());
            }
        }

        return scales;
    }

    /**
     * Returns the most recend rubric rating in the grid. The score is appended to the rating
     * sequence number (padded
     * to 5 digits) for an ordered display ("00010 Exceeding Expectations").
     *
     * @param rubricGrid ReportDataGrid
     * @return String
     */
    private String getRubricRating(ReportDataGrid rubricGrid) {
        String rating = "";

        RubricCriterion criterion = (RubricCriterion) rubricGrid.get(RubricTranscriptReportGrid.COL_CRITERION);
        Collection<RubricRatingScalePoints> scales = getRatingScaleMap(criterion);

        // Get score
        String t3Score = (String) rubricGrid.get(TranscriptReportGrid.RUBRIC_FIELD_PREFIX + "Tri 3");
        String finalScore = (String) rubricGrid.get(TranscriptReportGrid.COL_FINAL_GRADE);
        String score = StringUtils.coalesce(finalScore, t3Score);

        // Lookup score in the rating scale map and build rating display
        if (scales != null && scales.size() > 0) {
            RubricRatingScalePoints scalePoints = RubricManager.getRatingScalePoint(scales, score);
            if (scalePoints != null) {
                rating = StringUtils.padInt(scalePoints.getSequenceNumber(), 5) + " " + scalePoints.getName();
            }
        }

        return rating;
    }

    /**
     * Determine if appropriate student have alerts defined.
     *
     * @param student SisStudent
     * @param alertType AlertType
     * @return boolean
     */
    private boolean getStudentAlert(SisStudent student, StudentAlert.AlertType alertType) {
        boolean alertExist = false;
        Map<String, Collection<StudentAlert>> alertsMap = null;

        switch (alertType) {
            case LEGAL:
                alertsMap = m_studentLegalAlert;
                break;

            case MEDICAL:
                alertsMap = m_studentMedicalAlert;
                break;

            default:
                break;
        }

        if (alertsMap != null && alertsMap.containsKey(student.getOid())) {
            alertExist = alertsMap.get(student.getOid()).size() > 0;
        }

        return alertExist;
    }

    /**
     * Builds a new transcript record with identifying fields. The record is not saved so it will
     * not persist outside
     * of the report.
     *
     * @param schedule StudentSchedule
     * @return Transcript
     */
    private Transcript getTempTranscript(StudentSchedule schedule) {
        SisStudent student = schedule.getStudent();
        Section section = schedule.getSection();
        String contextOid = schedule.getSchedule().getDistrictContextOid();

        Transcript temp = X2BaseBean.newInstance(Transcript.class, getBroker().getPersistenceKey());
        temp.setStudentOid(student.getOid());
        temp.setGradeLevel(student.getGradeLevel(contextOid, getBroker()));
        temp.setDistrictContextOid(contextOid);
        temp.setCourseDescription(section.getDescription());
        temp.setTermCode(section.getTermView());
        temp.setTotalCredit(null);
        temp.setSchoolCourseOid(section.getSchoolCourseOid());
        temp.setMasterScheduleOid(section.getOid());

        return temp;
    }

    /**
     * Returns the array of transcript sort attributes.
     *
     * @return String[]
     */
    private String[] getTranscriptGridSortOrder() {
        String[] sortArray = new String[3];

        sortArray[0] = Transcript.REL_DISTRICT_CONTEXT + PATH_DELIMITER + SisDistrictSchoolYearContext.COL_SCHOOL_YEAR;
        sortArray[1] = Transcript.COL_COURSE_DESCRIPTION;
        sortArray[2] = Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER;

        return sortArray;
    }

    /**
     * Formats the ratings for the year for display on the report.
     *
     * @param ratingMap Map<String,String>
     * @param promotionComment String
     * @return String
     */
    private String formatRatings(Map<String, String> ratingMap, String promotionComment) {
        StringBuilder ratings = new StringBuilder(600);

        for (String key : ratingMap.keySet()) {
            // Append line breaks if there is more than 1 rating
            if (ratings.length() > 0) {
                ratings.append("\r\n");
            }

            // First 3 characters in the rating are the sequence number
            ratings.append(key.substring(6));

            ratings.append(": ");
            ratings.append(ratingMap.get(key));
        }

        if (!StringUtils.isEmpty(promotionComment)) {
            if (ratings.length() > 0) {
                ratings.append("\r\n");
            }

            ratings.append("Promotion comment: ");
            ratings.append(promotionComment);
        }

        return ratings.toString();
    }

    /**
     * Load the rating scale maps for lookup of the scale points by ID and rating scale OID.
     */
    private void initializeRatingScaleMap() {
        QueryByCriteria query = new QueryByCriteria(RubricRatingScalePoints.class);
        query.addOrderByAscending(RubricRatingScalePoints.COL_RUBRIC_RATING_SCALE_OID);
        query.addOrderByAscending(RubricRatingScalePoints.COL_POINTS);
        query.addOrderByAscending(RubricRatingScalePoints.COL_ID);

        m_ratingScaleMap =
                getBroker().getGroupedCollectionByQuery(query, RubricRatingScalePoints.COL_RUBRIC_RATING_SCALE_OID, 64);
    }

    /**
     * Execute query to load map of assessments based on student OID and sectionOid/courseOid.
     */
    private void loadAssessmentMap() {
        m_assessmentMap = new HashMap<String, Map<String, Collection<StudentAssessment>>>(INITIAL_MAP_SIZE);

        QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, m_assessmentCriteria);
        query.addOrderByAscending(StudentAssessment.COL_STUDENT_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisStudent lastStudent = null;
            Map<String, Collection<StudentAssessment>> assessments =
                    new HashMap<String, Collection<StudentAssessment>>(128);

            while (iterator.hasNext()) {
                StudentAssessment bean = (StudentAssessment) iterator.next();
                SisStudent student = bean.getStudent();

                if (!ObjectUtils.match(lastStudent, student)) {
                    assessments = new HashMap<String, Collection<StudentAssessment>>(512);
                    m_assessmentMap.put(student.getOid(), assessments);
                }

                if (!StringUtils.isEmpty(bean.getMasterScheduleOid())) {
                    addAssessment(assessments, bean.getMasterScheduleOid(), bean);
                } else if (!StringUtils.isEmpty(bean.getSchoolCourseOid())) {
                    addAssessment(assessments, bean.getSchoolCourseOid(), bean);
                }

                lastStudent = student;
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Load absences for school year context provided, for all available students.
     *
     * @param context to load absences count for
     * @param school SisSchool
     * @return a map keyed by Student OID for the context
     */
    private Map<String, BigDecimal> loadDailyAbsencesMap(DistrictSchoolYearContext context, SisSchool school) {
        Map<String, BigDecimal> results = new HashMap<String, BigDecimal>();

        X2Criteria criteria = m_dailyCriteria.copy();
        criteria.addBetween(StudentAttendance.COL_DATE, context.getStartDate(), context.getEndDate());
        criteria.addGreaterThan(StudentAttendance.COL_PORTION_ABSENT, new BigDecimal(0));
        criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, school.getOid());

        String[] columns = new String[] {StudentAttendance.COL_STUDENT_OID, "SUM(ATT_PORTION_ABSENT)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentAttendance.class, columns, criteria);
        query.addGroupBy(StudentAttendance.COL_STUDENT_OID);
        query.addOrderByAscending(StudentAttendance.COL_STUDENT_OID);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String stdOid = (String) row[0];
                BigDecimal absCount = new BigDecimal(row[1].toString());

                results.put(stdOid, absCount);
            }
        } finally {
            iterator.close();
        }

        m_absencesCount.put(context.getOid() + school.getOid(), results);

        return results;
    }

    /**
     * Execute query to load map of inclusion events based on student OID.
     */
    private void loadEventMap() {
        m_eventCriteria.addContainsIgnoreCase(StudentEventTracking.COL_EVENT_TYPE, EVENT_INCLUSION_KEYWORD);

        QueryByCriteria query = new QueryByCriteria(StudentEventTracking.class, m_eventCriteria);
        query.addOrderByAscending(StudentEventTracking.COL_STUDENT_OID);
        query.addOrderByAscending(StudentEventTracking.COL_EVENT_TYPE);

        m_inclusionMap =
                getBroker().getGroupedCollectionByQuery(query, StudentEventTracking.COL_STUDENT_OID, INITIAL_MAP_SIZE);
    }

    /**
     * Returns all grade levels with a numeric level below the threshold.
     *
     * @param threshold int
     * @param belowThreshold boolean
     * @return Collection<String>
     */
    private Collection<String> loadGradeLevels(int threshold, boolean belowThreshold) {
        Collection<String> levels = new LinkedList<String>();

        /*
         * Find grade level reference table
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field =
                dictionary.findDataDictionaryField(Transcript.class.getName(), Transcript.COL_GRADE_LEVEL);
        ReferenceTable table = field.getReferenceTable();

        /*
         * Compare the numeric level against the threshold and track any valid grade levels
         */
        for (ReferenceCode code : table.getReferenceCodes(getBroker())) {
            String numericLevel = code.getFieldA005();
            if (StringUtils.isNumeric(numericLevel)) {
                BigDecimal value = new BigDecimal(numericLevel);
                if (belowThreshold) {
                    if (value.doubleValue() < threshold) {
                        levels.add(code.getCode());
                    }
                } else {
                    if (value.doubleValue() >= threshold) {
                        levels.add(code.getCode());
                    }
                }
            }
        }

        return levels;
    }

    /**
     * Loads the primary (or first) graduation program for students keyed to the student OID.
     */
    private void loadGraduationPrograms() {
        Map<String, String> programMap = new HashMap<String, String>(INITIAL_MAP_SIZE);
        Map<String, String> programCodeMap = new HashMap<String, String>(INITIAL_MAP_SIZE);

        QueryByCriteria query = new QueryByCriteria(GraduationStudentProgram.class, m_graduationCriteria);
        query.addOrderByAscending(GraduationStudentProgram.COL_STUDENT_OID);
        query.addOrderByDescending(GraduationStudentProgram.COL_PRIMARY_INDICATOR);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisStudent lastStudent = null;
            while (iterator.hasNext()) {
                GraduationStudentProgram program = (GraduationStudentProgram) iterator.next();
                SisStudent student = program.getStudent();

                if (!ObjectUtils.match(student, lastStudent)) {
                    programMap.put(student.getOid(), program.getProgramStudies().getName());
                    programCodeMap.put(student.getOid(), program.getProgramStudies().getDiplomaType());
                }

                lastStudent = student;
            }
        } finally {
            iterator.close();
        }

        addParameter(GRADUATION_CODE_PARAM, programCodeMap);
        addParameter(GRADUATION_PROGRAM_PARAM, programMap);
    }

    /**
     * Execute query to load map of student schedules based on student OID.
     */
    private void loadScheduleMap() {
        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, m_scheduleCriteria);
        query.addOrderByAscending(StudentSchedule.COL_STUDENT_OID);

        m_scheduleMap =
                getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, INITIAL_MAP_SIZE);
    }

    /**
     * Load the alerts for students.
     *
     * @param alertCriteria Criteria
     * @return Map<String,Collection<StudentAlert>>
     */
    private Map<String, Collection<StudentAlert>> loadStudentAlerts(Criteria alertCriteria) {
        QueryByCriteria query = new QueryByCriteria(StudentAlert.class, alertCriteria);
        query.addOrderByAscending(StudentAlert.COL_STUDENT_OID);

        return getBroker().getGroupedCollectionByQuery(query, StudentAlert.COL_STUDENT_OID, INITIAL_MAP_SIZE);
    }

    /**
     * Loads the final transcript columns keyed to the transcript definition OID.
     */
    private void loadTranscriptColumns() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));

        QueryByCriteria query = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);
        m_finalColumnMap =
                getBroker().getMapByQuery(query, TranscriptColumnDefinition.COL_TRANSCRIPT_DEFINITION_OID, 16);
    }

    /**
     * Loads transcripts based on the passed criteria into a map keyed to the student OID.
     *
     * @param transcriptCriteria Criteria
     * @return Map<String, Collection<Transcript>>
     */
    private Map<String, Collection<Transcript>> loadTranscriptMap(Criteria transcriptCriteria) {
        QueryByCriteria query = new QueryByCriteria(Transcript.class, transcriptCriteria);
        query.addOrderByAscending(Transcript.COL_STUDENT_OID);
        query.addOrderByAscending(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER);
        query.addOrderByAscending(Transcript.COL_SCHOOL_COURSE_OID);

        return getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, INITIAL_MAP_SIZE);
    }

    /**
     * Loads a report transcript grid based on the passed criteria into a map keyed to the student
     * OID.
     *
     * @param transcriptCriteria Criteria
     * @return Map<String, ReportDataGrid>
     */
    private Map<String, ReportDataGrid> loadTranscriptGrid(Criteria transcriptCriteria) {
        Map<String, ReportDataGrid> transcriptMap = new HashMap<String, ReportDataGrid>(INITIAL_MAP_SIZE);

        TranscriptReportGrid transcriptGrid = new BCRubricTranscriptReportGrid(transcriptCriteria,
                new String[] {X2BaseBean.COL_OID},
                getTranscriptGridSortOrder(),
                false,
                false,
                false,
                false,
                getOrganization(),
                getBroker());

        /*
         * Iterate over grid and split by student
         */
        SisStudent lastStudent = null;
        ReportDataGrid studentGrid = new ReportDataGrid();

        while (transcriptGrid.next()) {
            SisStudent student = transcriptGrid.getStudent();

            if (!ObjectUtils.match(student, lastStudent)) {
                studentGrid.beforeTop();

                studentGrid = new ReportDataGrid();
                transcriptMap.put(student.getOid(), studentGrid);
            }

            studentGrid.append(transcriptGrid.getCurrentRow());

            lastStudent = student;
        }

        // Reset last student's grid
        studentGrid.beforeTop();

        return transcriptMap;
    }

    /**
     * BC Rubric Transcript Report Grid.
     *
     * @author X2 Development Corporation
     */
    private class BCRubricTranscriptReportGrid extends RubricTranscriptReportGrid {

        /**
         * Constructs a new BCRubricTranscriptReportGrid.
         *
         * @param transcriptCriteria Criteria
         * @param studentSortOrder String[]
         * @param strings String[]
         * @param convertNumeric boolean
         * @param convertReference boolean
         * @param localizeReference boolean
         * @param schoolGradesOnly boolean
         * @param organization Organization
         * @param broker X2Broker
         */
        public BCRubricTranscriptReportGrid(Criteria transcriptCriteria, String[] studentSortOrder,
                String[] strings, boolean convertNumeric, boolean convertReference, boolean localizeReference,
                boolean schoolGradesOnly, Organization organization, X2Broker broker) {
            super(transcriptCriteria, studentSortOrder, convertNumeric, convertReference, localizeReference,
                    schoolGradesOnly, organization, broker);
        }

        /**
         * Gets the numeric grade level.
         *
         * @param transcript Transcript
         * @return Integer
         * @see
         *      com.x2dev.sis.tools.reports.TranscriptReportGrid#getNumericGradeLevel(com.x2dev.sis.model
         *      .beans.Transcript)
         */
        @Override
        protected Integer getNumericGradeLevel(Transcript transcript) {
            Integer gradeLevel = super.getNumericGradeLevel(transcript);
            if ("KH".equals(transcript.getGradeLevel())) {
                gradeLevel = Integer.valueOf(0);
            }

            return gradeLevel;
        }
    }

}
