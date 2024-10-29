/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.KeyValueTrio;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.sql.Date;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Procedure for creating and deleting Course Exam Student Assessment records. It can be initiated
 * in the following locations:
 * - Student list � Procedure operates on all students for current year context only.
 * - Student > Schedule > Workspace � Procedure operates on a single student for current year
 * context only.
 * - Student > Transcript � Procedure operates on current selection of transcript records and is
 * not limited to current year context.
 *
 * Uses the following criteria to determine when to create or delete a Student Assessment record.
 * 
 * ----------------------------------------------------------------------------------------
 * | Student Schedule | Transcript | Trax Override | Student Assessment | Procedure Action |
 * | Exists? | Exists? | Create Exam? | Exists? | |
 * ----------------------------------------------------------------------------------------
 * | Y | N | | N | Create Assessment |
 * ----------------------------------------------------------------------------------------
 * | Y | Y | N | Y | Delete Assessment |
 * ----------------------------------------------------------------------------------------
 * | Y | Y | Y | N | Create Assessment |
 * ----------------------------------------------------------------------------------------
 * | N | N | | Y | Delete Assessment |
 * ----------------------------------------------------------------------------------------
 * | N | Y | N or blank | Y | Delete Assessment |
 * ----------------------------------------------------------------------------------------
 * | N | Y | Y | N | Create Assessment |
 * ----------------------------------------------------------------------------------------
 * 
 * @author Follett Software Company
 */
public class SynchronizeAssessmentCourseExam extends ProcedureJavaSource {
    /**
     * Serial Version UID
     */
    private static final long serialVersionUID = 1L;

    /**
     * Used to enable time logging.
     */
    private static final boolean ENABLE_TIME_TRACKING = false;

    /**
     * Course Exam Assessment Definition ID.
     */
    private static final String COURSE_EXAM_ID = "BC-CRS-EXM-DEF-ID";

    /**
     * Course Exam Session Month Aliases
     */
    private static final String COURSE_EXAM_SESSION_MONTH_1_ALIAS = "examSessionMonth1";
    private static final String COURSE_EXAM_SESSION_MONTH_2_ALIAS = "examSessionMonth2";
    private static final String COURSE_EXAM_SESSION_MONTH_3_ALIAS = "examSessionMonth3";
    private static final String COURSE_EXAM_SESSION_MONTH_4_ALIAS = "examSessionMonth4";
    private static final String COURSE_EXAM_SESSION_MONTH_5_ALIAS = "examSessionMonth5";
    private static final String COURSE_EXAM_SESSION_MONTH_6_ALIAS = "examSessionMonth6";

    /**
     * Extended Dictionary Create Exam column alias.
     */
    private static final String TRAX_CREATE_EXAM_ALIAS = "rcd-exam-create";

    /**
     * TRAX Override Extended Data Dictionary ID.
     */
    private static final String TRAX_EXTENDED_DICTIONARY_ID = "TRX-OVR-CRS-EXAM";

    /**
     * Trax Override field alias on the Student Transcript table.
     */
    private static final String TRN_TRAX_OVERRIDE_ALIAS = "trn-trax-override";

    /**
     * Course Exam Assessment Definition Column aliases.
     */
    private static final String COLUMN_EXAM_BLENDED_MARK_1 = "blendedMark1";
    private static final String COLUMN_EXAM_BLENDED_MARK_2 = "blendedMark2";
    private static final String COLUMN_EXAM_CODE = "examCode";
    private static final String COLUMN_EXAM_COURSE_END_DATE = "examCourseEndDate";
    private static final String COLUMN_EXAM_MARK_1 = "examMark1";
    private static final String COLUMN_EXAM_MARK_2 = "examMark2";
    private static final String COLUMN_EXAM_SESSION_DATE_1 = "examSessionDate1";
    private static final String COLUMN_EXAM_LOCATION_1 = "location1";

    private Map<String, String> m_assessmentColumnBeanPaths;
    private String m_courseExamAssessmentDefOid;
    private SisStudent m_currentStudent;
    private boolean m_isTranscriptList;
    private Map<String, String> m_scheduleTermEndDates;
    private Map<String, String> m_transcriptDefCourseEndDateJavaName;
    private Map<String, Boolean> m_traxOverrideReference;

    /**
     * Criteria used to query the three primary objects used in this procedure.
     */
    private X2Criteria m_assessmentCriteria;
    private X2Criteria m_scheduleCriteria;
    private X2Criteria m_transcriptCriteria;

    /**
     * Used for time tracking, if enabled.
     */
    private long startTime = 0;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Map<String, Map<String, StudentSchedule>> schedules = loadStudentSchedules();
        Map<String, Map<String, Collection<Transcript>>> transcripts = loadStudentTranscripts();
        Map<String, Map<String, Collection<StudentAssessment>>> assessments = loadStudentAssessments();

        Map<String, Collection<KeyValuePair>> courseSectionOids = loadCourseSectionOids();

        int assessmentsCreated = 0;
        int assessmentsDeleted = 0;

        /*
         * Iterate over each studentOid
         */
        for (String studentOid : courseSectionOids.keySet()) {
            if (ENABLE_TIME_TRACKING) {
                logMessage("Begin studentOid: " + studentOid + " at " + (System.currentTimeMillis() - startTime));
            }

            /*
             * First if the student has Course Exam assessment records that are not associated with
             * a Course or Transcript record
             * Only records that are not converted will be deleted.
             */
            X2Criteria studentAssessmentCriteria = new X2Criteria();
            studentAssessmentCriteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID,
                    m_courseExamAssessmentDefOid);
            studentAssessmentCriteria.addEqualTo(StudentAssessment.COL_STUDENT_OID, studentOid);
            studentAssessmentCriteria.addEmpty(StudentAssessment.COL_MASTER_SCHEDULE_OID,
                    getBroker().getPersistenceKey());
            studentAssessmentCriteria.addEmpty(StudentAssessment.COL_SCHOOL_COURSE_OID,
                    getBroker().getPersistenceKey());
            studentAssessmentCriteria.addBeginsWith(X2BaseBean.COL_OID, StudentAssessment.OBJECT_PREFIX);

            QueryByCriteria orphanAssessmentQuery =
                    new QueryByCriteria(StudentAssessment.class, studentAssessmentCriteria);
            int count = getBroker().getCount(orphanAssessmentQuery);
            if (count > 0) {
                getBroker().deleteByQuery(orphanAssessmentQuery);
                SisStudent student = (SisStudent) getBroker().getBeanByOid(SisStudent.class, studentOid);
                logMessage("Deleted " + String.valueOf(count) + " orphan assessment records for student: "
                        + student.getNameView());
            }

            Map<String, StudentSchedule> studentSchedules = schedules.get(studentOid) != null
                    ? schedules.get(studentOid) : new HashMap<String, StudentSchedule>();
            Map<String, Collection<Transcript>> studentTranscripts = transcripts.get(studentOid) != null
                    ? transcripts.get(studentOid) : new HashMap<String, Collection<Transcript>>();
            Map<String, Collection<StudentAssessment>> studentAssessments = assessments.get(studentOid) != null
                    ? assessments.get(studentOid) : new HashMap<String, Collection<StudentAssessment>>();

            Collection<KeyValuePair> courseSectionOidsForStudent = courseSectionOids.get(studentOid) != null
                    ? courseSectionOids.get(studentOid) : new ArrayList<KeyValuePair>();
            for (KeyValuePair courseSectionPair : courseSectionOidsForStudent) {
                String sectionOid = (String) courseSectionPair.getKey(); // could be null
                String schoolCourseOid = (String) courseSectionPair.getValue();

                StudentSchedule studentSchedule = null;
                Collection<Transcript> transcriptsForCourseSection = null;
                Collection<StudentAssessment> assessmentsForCourseSection = null;

                Collection<KeyValueTrio> matches = new ArrayList<KeyValueTrio>();

                /*
                 * If the sectionOid is not null, then it is possible to have a single
                 * StudentSchedule record.
                 */
                if (sectionOid != null) {
                    studentSchedule = studentSchedules.get(sectionOid);
                    transcriptsForCourseSection = studentTranscripts.get(sectionOid);
                    assessmentsForCourseSection = studentAssessments.get(sectionOid);
                } else {
                    transcriptsForCourseSection = studentTranscripts.get(schoolCourseOid);
                    assessmentsForCourseSection = studentAssessments.get(schoolCourseOid);
                }

                if (transcriptsForCourseSection != null) {
                    for (Transcript transcript : transcriptsForCourseSection) {
                        /*
                         * studentSchedule will only ever have a value when sectionOid is not null.
                         * In that case,
                         * the Collection<Transcript> and Collection<StudentAssessment> should each
                         * have a max of 1 object.
                         */
                        matches.add(new KeyValueTrio(studentSchedule, transcript, null));
                    }
                } else if (studentSchedule != null) {
                    matches.add(new KeyValueTrio(studentSchedule, null, null));
                }

                /*
                 * Used to store new KeyValueTris until the assessment iteration is complete.
                 */
                Collection<KeyValueTrio> newMatches = new ArrayList<KeyValueTrio>();

                if (assessmentsForCourseSection != null) {
                    for (StudentAssessment assessment : assessmentsForCourseSection) {
                        boolean matchFound = false;
                        for (KeyValueTrio match : matches) {
                            // This is the StudentAssessment value. Make sure one has not already
                            // been set on this match.
                            if (match.getValue2() != null) {
                                continue;
                            }

                            Transcript transcript = (Transcript) match.getValue1();
                            /*
                             * Attempt to find the match by the Transcript record.
                             */
                            if (transcript != null) {
                                if (StringUtils.isEmpty(transcript.getMasterScheduleOid())
                                        && StringUtils.isEmpty(assessment.getMasterScheduleOid())) {
                                    /*
                                     * Found a transcript with a matching SchoolCourseOID. Determine
                                     * if the Course End Date field on
                                     * the StudentAssessment record matches that of the Transcript
                                     * in order to confirm a match.
                                     */
                                    String assessmentCourseEndDate = (String) assessment.getFieldValueByBeanPath(
                                            m_assessmentColumnBeanPaths.get(COLUMN_EXAM_COURSE_END_DATE));

                                    String transcriptCourseEndDate = null;

                                    String beanPath = m_transcriptDefCourseEndDateJavaName
                                            .get(transcript.getTranscriptDefinitionOid());
                                    transcriptCourseEndDate = (String) transcript.getFieldValueByBeanPath(beanPath);

                                    if ((!StringUtils.isEmpty(transcriptCourseEndDate)
                                            && !StringUtils.isEmpty(assessmentCourseEndDate) &&
                                            transcriptCourseEndDate.equals(assessmentCourseEndDate)) ||
                                            (StringUtils.isEmpty(transcriptCourseEndDate)
                                                    && StringUtils.isEmpty(assessmentCourseEndDate))) {
                                        matchFound = true;
                                        matches.remove(match);
                                        match = new KeyValueTrio(studentSchedule, transcript, assessment);
                                        newMatches.add(match);
                                        break;
                                    }
                                } else {
                                    /*
                                     * If we have a match by MasterScheduleOid, there is no need to
                                     * check the course end date.
                                     */
                                    if (transcript.getMasterScheduleOid() != null &&
                                            transcript.getMasterScheduleOid()
                                                    .equals(assessment.getMasterScheduleOid())) {
                                        matchFound = true;
                                        matches.remove(match);
                                        match = new KeyValueTrio(studentSchedule, transcript, assessment);
                                        newMatches.add(match);
                                        break;
                                    }
                                }
                            } else {
                                /*
                                 * Attempt to find the match by the StudentSchedule record.
                                 */
                                StudentSchedule sched = (StudentSchedule) match.getKey();
                                if (sched != null && sched.getSectionOid().equals(assessment.getMasterScheduleOid())) {
                                    matchFound = true;
                                    matches.remove(match);
                                    match = new KeyValueTrio(studentSchedule, null, assessment);
                                    newMatches.add(match);
                                    break;
                                }
                            }
                        }

                        if (!matchFound) {
                            // This StudentAssessment record does not match any existing matches.
                            // Create a new KeyValueTrio.
                            newMatches.add(new KeyValueTrio(null, null, assessment));
                        }
                    }
                }

                matches.addAll(newMatches);

                /*
                 * Iterate through each match and determine if a StudentAssessment record should be
                 * created or deleted.
                 */
                for (KeyValueTrio match : matches) {
                    StudentSchedule schedule = (StudentSchedule) match.getKey();
                    Transcript transcript = (Transcript) match.getValue1();
                    StudentAssessment assessment = (StudentAssessment) match.getValue2();

                    boolean createExam = false;
                    if (transcript != null) {
                        String traxOverride = (String) transcript.getFieldValueByAlias(TRN_TRAX_OVERRIDE_ALIAS);
                        createExam = m_traxOverrideReference.get(traxOverride) != null
                                ? m_traxOverrideReference.get(traxOverride).booleanValue() : true;
                    }

                    if (schedule != null) {
                        if (transcript == null && assessment == null) {
                            assessment = createStudentAssessment(schedule, transcript);
                            if (assessment.getOid() != null) {
                                assessmentsCreated++;
                            }
                            continue;
                        }

                        if (transcript != null) {
                            if (!createExam && assessment != null) {
                                if (deleteStudentAssessment(assessment)) {
                                    assessmentsDeleted++;
                                }
                                continue;
                            }

                            if (createExam && assessment == null) {
                                assessment = createStudentAssessment(schedule, transcript);
                                if (assessment.getOid() != null) {
                                    assessmentsCreated++;
                                }
                                continue;
                            }
                        }
                    }

                    if (schedule == null) {
                        if (transcript == null && assessment != null) {
                            if (deleteStudentAssessment(assessment)) {
                                assessmentsDeleted++;
                            }
                            continue;
                        }

                        if (transcript != null) {
                            if (!createExam && assessment != null) {
                                if (deleteStudentAssessment(assessment)) {
                                    assessmentsDeleted++;
                                }
                                continue;
                            }

                            if (createExam && assessment == null) {
                                assessment = createStudentAssessment(schedule, transcript);
                                if (assessment.getOid() != null) {
                                    assessmentsCreated++;
                                }
                            }
                        }
                    }
                }
            }

            if (ENABLE_TIME_TRACKING) {
                logMessage("End studentOid: " + studentOid + " at " + (System.currentTimeMillis() - startTime));
            }
        }

        logMessage(assessmentsCreated + " assessments created.");
        logMessage(assessmentsDeleted + " assessments deleted.");

        if (ENABLE_TIME_TRACKING) {
            logMessage("Time elapsed: " + (System.currentTimeMillis() - startTime));
        }
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        if (ENABLE_TIME_TRACKING) {
            logMessage("Begin initialize method: " + (System.currentTimeMillis() - startTime));
        }

        loadCourseExamAssessmentData();

        // buildCriteria must occur before some of the load methods below.
        buildCriteria();
        loadTranscriptDefCourseEndDateJavaNames();
        loadScheduleTermEndDates();
        loadTraxOverrideReferenceCodes();

        if (ENABLE_TIME_TRACKING) {
            logMessage("End initialize method: " + (System.currentTimeMillis() - startTime));
        }
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
        if (ENABLE_TIME_TRACKING) {
            startTime = System.currentTimeMillis();
            logMessage("Begin time tracking.");
        }

        /*
         * If we're in the context of a single student, run for just that student.
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);

        /*
         * If we're in the context of a single students transcript records, run for the selected
         * records only.
         */
        if (userData.getCurrentList() != null && userData.getCurrentList().getDataClass().equals(Transcript.class)) {
            m_isTranscriptList = true;
        }
    }

    /**
     * Builds all Criteria objects.
     */
    private void buildCriteria() {
        m_transcriptCriteria = new X2Criteria();
        m_scheduleCriteria = new X2Criteria();
        m_assessmentCriteria = new X2Criteria();

        /*
         * Only query records tied to examinable Courses.
         */
        m_transcriptCriteria
                .addEqualTo(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        Course.COL_EXAM_REQUIRED_INDICATOR, Boolean.TRUE);
        m_scheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE +
                PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + Course.COL_EXAM_REQUIRED_INDICATOR,
                Boolean.TRUE);
        m_assessmentCriteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, m_courseExamAssessmentDefOid);

        // Student
        if (m_currentStudent != null) {
            m_scheduleCriteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, m_currentStudent.getOid());
            m_assessmentCriteria.addEqualTo(StudentAssessment.COL_STUDENT_OID, m_currentStudent.getOid());
            m_transcriptCriteria.addEqualTo(Transcript.COL_STUDENT_OID, m_currentStudent.getOid());
        } else {
            if (isSchoolContext()) {
                m_scheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                        getSchool().getOid());
                m_assessmentCriteria.addEqualTo(StudentAssessment.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                m_scheduleCriteria.addAndCriteria(getOrganizationCriteria(StudentSchedule.class));
                m_assessmentCriteria.addAndCriteria(getOrganizationCriteria(StudentAssessment.class));
            }
        }

        if (m_isTranscriptList) {
            m_transcriptCriteria.addAndCriteria(getCurrentCriteria());

            /*
             * Limit the StudentSchedule records to those attached to sections of the transcripts.
             */
            SubQuery masterScheduleSub =
                    new SubQuery(Transcript.class, Transcript.COL_MASTER_SCHEDULE_OID, m_transcriptCriteria.copy());
            m_scheduleCriteria.addIn(StudentSchedule.COL_SECTION_OID, masterScheduleSub);

            /*
             * Limit the assessments by those that match on either the MasterScheduleOID or
             * SchoolCourseOID where the
             * MasterScheduleOID is null to avoid finding StudentAssessments tied to sections of a
             * Course that a
             * student took but is not currently in the transcript list.
             */
            X2Criteria masterCriteria = new X2Criteria();
            masterCriteria.addIn(StudentAssessment.COL_MASTER_SCHEDULE_OID, masterScheduleSub);

            SubQuery courseSub =
                    new SubQuery(Transcript.class, Transcript.COL_SCHOOL_COURSE_OID, m_transcriptCriteria.copy());

            X2Criteria courseNoSectionCriteria = new X2Criteria();
            courseNoSectionCriteria.addIn(StudentAssessment.COL_SCHOOL_COURSE_OID, courseSub);
            courseNoSectionCriteria.addIsNull(Transcript.COL_MASTER_SCHEDULE_OID);

            masterCriteria.addOrCriteria(courseNoSectionCriteria);
            m_assessmentCriteria.addAndCriteria(masterCriteria);
        } else {
            if (isSchoolContext()) {
                m_transcriptCriteria.addEqualTo(Transcript.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                m_transcriptCriteria.addAndCriteria(getOrganizationCriteria(Transcript.class));
            }

            m_transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

            m_scheduleCriteria.addEqualToField(
                    StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                            + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    StudentSchedule.COL_SCHEDULE_OID);

            /*
             * If we are not within the context of a students transcript list, limit the assessments
             * by those
             * attached to current year courses.
             */
            m_assessmentCriteria
                    .addEqualTo(StudentAssessment.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE +
                            PATH_DELIMITER + Course.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        }
    }

    /**
     * Creates a StudentAssessment record for a Course Exam Assessment Definition.
     *
     * @param schedule StudentSchedule
     * @param transcript Transcript
     * @return StudentAssessment
     */
    private StudentAssessment createStudentAssessment(StudentSchedule schedule, Transcript transcript) {
        StudentAssessment assessment = X2BaseBean.newInstance(StudentAssessment.class, getBroker().getPersistenceKey());
        assessment.setAssessmentDefinitionOid(m_courseExamAssessmentDefOid);

        // Date the assessment is created.
        assessment.setDate(new PlainDate());
        Course course = null;

        if (transcript == null && schedule != null) // always based on transcript first if exists
        {
            assessment.setMasterScheduleOid(schedule.getSectionOid());
            assessment.setSchoolCourseOid(schedule.getSection().getSchoolCourseOid());
            assessment.setStudentOid(schedule.getStudentOid());
            assessment.setSchoolOid(schedule.getSection().getSchoolCourse().getSchoolOid());
            assessment.setGradeLevelCode(schedule.getStudent().getGradeLevel());
            assessment.setFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_LOCATION_1),
                    schedule.getSchedule().getSchool().getSchoolId());

            if (schedule.getSection().getSchoolCourse().getCourse().getParentCourse() != null) {
                course = schedule.getSection().getSchoolCourse().getCourse().getParentCourse();
            } else {
                course = schedule.getSection().getSchoolCourse().getCourse();
            }

            assessment.setFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_CODE), course.getExamCode());
            assessment.setFieldB010(course.getNumber());

            if (schedule.getSection().getScheduleTerm() != null) {
                String date = m_scheduleTermEndDates.get(schedule.getSection().getScheduleTermOid());

                if (date != null) {
                    assessment.setFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_COURSE_END_DATE),
                            date);

                    String month = getDefaultSessionMonth(date.toString(), course);
                    // examSessionDate1
                    assessment.setFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_SESSION_DATE_1),
                            String.valueOf(schedule.getSchedule().getDistrictContext().getSchoolYear()) + month);
                }
            }
        } else if (transcript != null) {
            /*
             * Use the Transcript to get the required information.
             */
            assessment.setMasterScheduleOid(transcript.getMasterScheduleOid());
            assessment.setSchoolCourseOid(transcript.getSchoolCourseOid());
            assessment.setStudentOid(transcript.getStudentOid());
            assessment.setSchoolOid(transcript.getSchoolOid());
            assessment.setGradeLevelCode(transcript.getGradeLevel());
            assessment.setFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_LOCATION_1),
                    transcript.getSchool().getSchoolId());

            if (transcript.getSchoolCourse().getCourse().getParentCourse() != null) {
                course = transcript.getSchoolCourse().getCourse().getParentCourse();
            } else {
                course = transcript.getSchoolCourse().getCourse();
            }

            assessment.setFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_CODE), course.getExamCode());
            assessment.setFieldB010(course.getNumber());

            String beanPath = m_transcriptDefCourseEndDateJavaName.get(transcript.getTranscriptDefinitionOid());
            if (beanPath != null) {
                String endDate = (String) transcript.getFieldValueByBeanPath(beanPath);
                if (!StringUtils.isEmpty(endDate)) {
                    // Course End Date
                    assessment.setFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_COURSE_END_DATE),
                            endDate);

                    String month = getDefaultSessionMonth(endDate, course);

                    // examSessionDate1
                    assessment.setFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_SESSION_DATE_1),
                            String.valueOf(transcript.getDistrictContext().getSchoolYear()) + month);
                }
            }
        }

        if (!StringUtils.isEmpty(course.getExamCode())) {
            getBroker().saveBeanForced(assessment);

            logMessage("Created assessment for student: " + assessment.getStudent().getNameView() + " and course: " +
                    (assessment.getMasterSchedule() != null ? assessment.getMasterSchedule().getCourseView()
                            : assessment.getSchoolCourse().getNumber())
                    + " with Exam Code: " +
                    assessment.getFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_CODE)) +
                    " course exam code " + course.getOid() + " / " + course.getExamCode());
        }

        return assessment;
    }

    /**
     * Deletes the passed in StudentAssessment object if it does not contain any scores.
     *
     * @param assessment StudentAssessment
     * @return boolean
     */
    private boolean deleteStudentAssessment(StudentAssessment assessment) {
        boolean deletedAssessment = false;

        /*
         * Do not delete the Student Assessment record if it has any score values in it.
         */
        if (assessment.getFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_MARK_1)) == null &&
                assessment.getFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_MARK_2)) == null &&
                assessment.getFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_BLENDED_MARK_1)) == null
                &&
                assessment.getFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_BLENDED_MARK_2)) == null
                &&
                assessment.getOid().startsWith(StudentAssessment.OBJECT_PREFIX)) {
            getBroker().deleteBean(assessment);
            deletedAssessment = true;

            logMessage("Deleted assessment for student: " + assessment.getStudent().getNameView() + " and course: " +
                    (assessment.getMasterSchedule() != null ? assessment.getMasterSchedule().getCourseView()
                            : assessment.getSchoolCourse().getNumber())
                    + " with Exam Code: " +
                    assessment.getFieldValueByBeanPath(m_assessmentColumnBeanPaths.get(COLUMN_EXAM_CODE)));
        }

        return deletedAssessment;
    }

    /**
     * Returns the default exam session month.
     *
     * Course Completion date: Default Session Month:
     * January January
     * February January
     * March April if exam is offered, or next available session for that exam (May or June)
     * April April if exam is offered, or next available session for that exam (May or June)
     * May May if exam is offered, otherwise June
     * June June
     * July June
     * August August if exam is offered, or next available session for that exam (Nov or Jan)
     * September November if exam is offered, otherwise January in the next year
     * October November if exam is offered, otherwise January in the next year
     * November November if exam is offered, otherwise January in the next year
     * December January in the next year
     *
     * @param courseEndDate String
     * @param course Course
     * @return String
     */
    private String getDefaultSessionMonth(String courseEndDate, Course course) {
        String sessionMonth = null;

        String defaultMonth1 = (String) course.getFieldValueByAlias(COURSE_EXAM_SESSION_MONTH_1_ALIAS);
        String defaultMonth2 = (String) course.getFieldValueByAlias(COURSE_EXAM_SESSION_MONTH_2_ALIAS);
        String defaultMonth3 = (String) course.getFieldValueByAlias(COURSE_EXAM_SESSION_MONTH_3_ALIAS);
        String defaultMonth4 = (String) course.getFieldValueByAlias(COURSE_EXAM_SESSION_MONTH_4_ALIAS);
        String defaultMonth5 = (String) course.getFieldValueByAlias(COURSE_EXAM_SESSION_MONTH_5_ALIAS);
        String defaultMonth6 = (String) course.getFieldValueByAlias(COURSE_EXAM_SESSION_MONTH_6_ALIAS);

        List<String> defaultMonths = Arrays.asList(defaultMonth1, defaultMonth2, defaultMonth3, defaultMonth4,
                defaultMonth5, defaultMonth6);

        if (courseEndDate != null && courseEndDate.length() > 8) {
            String courseEndDateMonth = courseEndDate.substring(5, 7);

            switch (courseEndDateMonth) {
                case "08":
                    if (defaultMonths.contains("August")) {
                        sessionMonth = "08";
                        break;
                    }
                    //$FALL-THROUGH$

                case "09":
                case "10":
                case "11":
                    if (defaultMonths.contains("November")) {
                        sessionMonth = "11";
                        break;
                    }
                    //$FALL-THROUGH$

                case "12":
                case "01":
                case "02":
                    sessionMonth = "01";
                    break;

                case "03":
                case "04":
                    if (defaultMonths.contains("April")) {
                        sessionMonth = "04";
                        break;
                    }
                    //$FALL-THROUGH$

                case "05":
                    if (defaultMonths.contains("May")) {
                        sessionMonth = "05";
                        break;
                    }
                    //$FALL-THROUGH$

                case "06":
                case "07":
                    sessionMonth = "06";
                    break;

                default: // Do nothing
                    break;
            }
        }

        return sessionMonth;
    }

    /**
     * Loads the Course Exam Assessment Definition OID.
     */
    private void loadCourseExamAssessmentData() {
        X2Criteria acdCriteria = new X2Criteria();
        acdCriteria.addEqualTo(AssessmentColumnDefinition.REL_ASSESSMENT_DEFINITION + PATH_DELIMITER +
                AssessmentDefinition.COL_ID, COURSE_EXAM_ID);

        QueryByCriteria query = new QueryByCriteria(AssessmentColumnDefinition.class, acdCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        m_assessmentColumnBeanPaths = new HashMap<String, String>();

        try {
            while (iterator.hasNext()) {
                AssessmentColumnDefinition column = (AssessmentColumnDefinition) iterator.next();
                m_assessmentColumnBeanPaths.put(column.getAlias(),
                        column.getDataFieldConfig().getDataField().getJavaName());

                if (StringUtils.isEmpty(m_courseExamAssessmentDefOid)) {
                    m_courseExamAssessmentDefOid = column.getAssessmentDefinitionOid();
                    logMessage("Found Assessment Definition: " + column.getAssessmentDefinition().getName());
                }
            }
        } finally {
            iterator.close();
        }

        if (StringUtils.isEmpty(m_courseExamAssessmentDefOid)) {
            logMessage("Unable to find the \"Course Exam\" assessment definition with ID: " + COURSE_EXAM_ID);
        }
    }

    /**
     * Loads a Map keyed on StudentOid containing a Collection of KeyValuePair objects.
     * The KeyValuePairs will have a key of MasterScheduleOid and a value of SchoolCourseOid. In
     * some cases, the
     * MasterScheduleOid will be null.
     *
     * This will be used to iterate through each MasterSchedule or SchoolCourse that a student has
     * related to
     * a StudentSchedule, StudentAssessment, or Transcript record being used by this procedure.
     *
     * @return Map<String, Collection<KeyValuePair>>
     */
    private Map<String, Collection<KeyValuePair>> loadCourseSectionOids() {
        if (ENABLE_TIME_TRACKING) {
            logMessage("End loading student assessments: " + (System.currentTimeMillis() - startTime));
            logMessage("Begin loading course section Oids: " + (System.currentTimeMillis() - startTime));
        }

        Map<String, Collection<KeyValuePair>> courseSectionPairs = new HashMap<String, Collection<KeyValuePair>>();

        // This is used to stored the added KeyValuePair Strings to ensure no duplicates are added.
        Map<String, ArrayList<String>> addedKeyValuesByStudent = new HashMap<String, ArrayList<String>>();

        /*
         * Transcript
         */
        String[] transcriptColumns = new String[] {Transcript.COL_STUDENT_OID,
                Transcript.COL_MASTER_SCHEDULE_OID,
                Transcript.COL_SCHOOL_COURSE_OID};
        ReportQueryByCriteria transcriptQuery =
                new ReportQueryByCriteria(Transcript.class, transcriptColumns, m_transcriptCriteria);
        ReportQueryIterator transcriptIterator = getBroker().getReportQueryIteratorByQuery(transcriptQuery);
        try {
            while (transcriptIterator.hasNext()) {
                Object[] record = (Object[]) transcriptIterator.next();

                String studentOid = (String) record[0];
                String sectionOid = (String) record[1];
                String schoolCourseOid = (String) record[2];

                Collection<KeyValuePair> courseSectionPairsForStudent = courseSectionPairs.get(studentOid);
                if (courseSectionPairsForStudent == null) {
                    courseSectionPairsForStudent = new ArrayList<KeyValuePair>();
                    courseSectionPairs.put(studentOid, courseSectionPairsForStudent);
                }
                ArrayList<String> addedKeyValues = addedKeyValuesByStudent.get(studentOid);
                if (addedKeyValues == null) {
                    addedKeyValues = new ArrayList<String>();
                    addedKeyValuesByStudent.put(studentOid, addedKeyValues);
                }

                // Add logic to remove duplicate pairs
                KeyValuePair pair = new KeyValuePair(sectionOid, schoolCourseOid);
                if (!addedKeyValues.contains(pair.toString())) {
                    courseSectionPairsForStudent.add(pair);
                    addedKeyValues.add(pair.toString());
                }
            }
        } finally {
            transcriptIterator.close();
        }

        /*
         * StudentSchedule
         */
        String[] scheduleColumns = new String[] {StudentSchedule.COL_STUDENT_OID,
                StudentSchedule.COL_SECTION_OID,
                StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.COL_SCHOOL_COURSE_OID};
        ReportQueryByCriteria scheduleQuery =
                new ReportQueryByCriteria(StudentSchedule.class, scheduleColumns, m_scheduleCriteria);
        ReportQueryIterator scheduleIterator = getBroker().getReportQueryIteratorByQuery(scheduleQuery);
        try {
            while (scheduleIterator.hasNext()) {
                Object[] record = (Object[]) scheduleIterator.next();

                String studentOid = (String) record[0];
                String sectionOid = (String) record[1];
                String schoolCourseOid = (String) record[2];

                Collection<KeyValuePair> courseSectionPairsForStudent = courseSectionPairs.get(studentOid);
                if (courseSectionPairsForStudent == null) {
                    courseSectionPairsForStudent = new ArrayList<KeyValuePair>();
                    courseSectionPairs.put(studentOid, courseSectionPairsForStudent);
                }
                ArrayList<String> addedKeyValues = addedKeyValuesByStudent.get(studentOid);
                if (addedKeyValues == null) {
                    addedKeyValues = new ArrayList<String>();
                    addedKeyValuesByStudent.put(studentOid, addedKeyValues);
                }

                // Add logic to remove duplicate pairs
                KeyValuePair pair = new KeyValuePair(sectionOid, schoolCourseOid);
                if (!addedKeyValues.contains(pair.toString())) {
                    courseSectionPairsForStudent.add(pair);
                    addedKeyValues.add(pair.toString());
                }
            }
        } finally {
            scheduleIterator.close();
        }

        /*
         * StudentAssessment
         */
        String[] assessmentColumns = new String[] {StudentAssessment.COL_STUDENT_OID,
                StudentAssessment.COL_MASTER_SCHEDULE_OID,
                StudentAssessment.COL_SCHOOL_COURSE_OID};
        ReportQueryByCriteria assessmentQuery =
                new ReportQueryByCriteria(StudentAssessment.class, assessmentColumns, m_assessmentCriteria);
        ReportQueryIterator assessmentIterator = getBroker().getReportQueryIteratorByQuery(assessmentQuery);
        try {
            while (assessmentIterator.hasNext()) {
                Object[] record = (Object[]) assessmentIterator.next();

                String studentOid = (String) record[0];
                String sectionOid = (String) record[1];
                String schoolCourseOid = (String) record[2];

                Collection<KeyValuePair> courseSectionPairsForStudent = courseSectionPairs.get(studentOid);
                if (courseSectionPairsForStudent == null) {
                    courseSectionPairsForStudent = new ArrayList<KeyValuePair>();
                    courseSectionPairs.put(studentOid, courseSectionPairsForStudent);
                }
                ArrayList<String> addedKeyValues = addedKeyValuesByStudent.get(studentOid);
                if (addedKeyValues == null) {
                    addedKeyValues = new ArrayList<String>();
                    addedKeyValuesByStudent.put(studentOid, addedKeyValues);
                }

                // Add logic to remove duplicate pairs
                KeyValuePair pair = new KeyValuePair(sectionOid, schoolCourseOid);
                if (!addedKeyValues.contains(pair.toString())) {
                    courseSectionPairsForStudent.add(pair);
                    addedKeyValues.add(pair.toString());
                }
            }
        } finally {
            assessmentIterator.close();
        }

        if (ENABLE_TIME_TRACKING) {
            logMessage("End loading course section Oids: " + (System.currentTimeMillis() - startTime));
        }

        return courseSectionPairs;
    }

    /**
     * Loads a map of the ScheduleTerm end dates keyed on the ScheduleTerm.
     */
    private void loadScheduleTermEndDates() {
        m_scheduleTermEndDates = new HashMap<String, String>();

        X2Criteria criteria = new X2Criteria();

        if (isSchoolContext()) {
            criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE +
                    PATH_DELIMITER + Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(ScheduleTermDate.class));
        }

        /*
         * From within a transcript list, you do not want to limit by current year active schedules
         * as the transcripts
         * may be from prior years. From the student list or student schedule workspace, everything
         * is from current year
         * active schedules.
         */
        if (!m_isTranscriptList) {
            // Active schedule
            criteria.addEqualToField(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE +
                    PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED +
                    PATH_DELIMITER + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, StudentSchedule.COL_SCHEDULE_OID);
        }

        String[] attributes = new String[] {ScheduleTermDate.COL_SCHEDULE_TERM_OID, ScheduleTermDate.COL_END_DATE};

        ReportQueryByCriteria query = new ReportQueryByCriteria(ScheduleTermDate.class, attributes, criteria);
        query.addOrderByDescending(ScheduleTermDate.COL_SCHEDULE_TERM_OID);
        query.addOrderByDescending(ScheduleTermDate.COL_END_DATE);

        ReportQueryIterator results = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (results.hasNext()) {
                Object[] row = (Object[]) results.next();
                m_scheduleTermEndDates.put((String) row[0], (new Date(((java.util.Date) row[1]).getTime())).toString());
            }
        } finally {
            results.close();
        }
    }

    /**
     * Loads a map keyed on StudentOid. The value is an inner map, keyed on either MasterScheduleOid
     * if it exists,
     * or SchoolCourseOid, containing a Collection of StudentAssessment objects.
     *
     * @return Map<String, Map<String, Collection<StudentAssessment>>>
     */
    private Map<String, Map<String, Collection<StudentAssessment>>> loadStudentAssessments() {
        if (ENABLE_TIME_TRACKING) {
            logMessage("End loading student transcripts: " + (System.currentTimeMillis() - startTime));
            logMessage("Begin loading student assessments: " + (System.currentTimeMillis() - startTime));
        }

        Map<String, Map<String, Collection<StudentAssessment>>> studentAssessmentsByCourseSectionMap =
                new HashMap<String, Map<String, Collection<StudentAssessment>>>();

        QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, m_assessmentCriteria);
        query.addOrderByAscending(StudentAssessment.COL_STUDENT_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                StudentAssessment assessment = (StudentAssessment) iterator.next();

                Map<String, Collection<StudentAssessment>> assessmentsByCourseSection =
                        studentAssessmentsByCourseSectionMap.get(assessment.getStudentOid());
                if (assessmentsByCourseSection == null) {
                    assessmentsByCourseSection = new HashMap<String, Collection<StudentAssessment>>();
                    studentAssessmentsByCourseSectionMap.put(assessment.getStudentOid(), assessmentsByCourseSection);
                }

                String oid = assessment.getMasterScheduleOid() != null ? assessment.getMasterScheduleOid()
                        : assessment.getSchoolCourseOid();

                Collection<StudentAssessment> storedAssessments = assessmentsByCourseSection.get(oid);
                if (storedAssessments == null) {
                    storedAssessments = new ArrayList<StudentAssessment>();
                    assessmentsByCourseSection.put(oid, storedAssessments);
                }

                storedAssessments.add(assessment);
            }
        } finally {
            iterator.close();
        }

        return studentAssessmentsByCourseSectionMap;
    }

    /**
     * Loads a map keyed on StudentOid. The value is an inner map, keyed on MasterScheduleOid,
     * containing a StudentSchedule.
     *
     * @return Map<String, Map<String, StudentSchedule>>
     */
    private Map<String, Map<String, StudentSchedule>> loadStudentSchedules() {
        if (ENABLE_TIME_TRACKING) {
            logMessage("Begin loading student schedules: " + (System.currentTimeMillis() - startTime));
        }

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, m_scheduleCriteria);
        query.addOrderByAscending(StudentSchedule.COL_STUDENT_OID);

        return getBroker().getNestedMapByQuery(query, StudentSchedule.COL_STUDENT_OID,
                StudentSchedule.COL_SECTION_OID, 0, 0);
    }

    /**
     * Loads a map keyed on StudentOid. The value is an inner map, keyed on either MasterScheduleOid
     * if it exists,
     * or SchoolCourseOid, containing a Collection of Transcript objects.
     *
     * @return Map<String, Map<String, Collection<Transcript>>>
     */
    private Map<String, Map<String, Collection<Transcript>>> loadStudentTranscripts() {
        if (ENABLE_TIME_TRACKING) {
            logMessage("End loading student schedules: " + (System.currentTimeMillis() - startTime));
            logMessage("Begin loading student transcripts: " + (System.currentTimeMillis() - startTime));
        }

        Map<String, Map<String, Collection<Transcript>>> studentTranscriptsByCourseSectionMap =
                new HashMap<String, Map<String, Collection<Transcript>>>();

        QueryByCriteria query = new QueryByCriteria(Transcript.class, m_transcriptCriteria);
        query.addOrderByAscending(Transcript.COL_STUDENT_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                Transcript transcript = (Transcript) iterator.next();

                Map<String, Collection<Transcript>> transcriptsByCourseSection =
                        studentTranscriptsByCourseSectionMap.get(transcript.getStudentOid());
                if (transcriptsByCourseSection == null) {
                    transcriptsByCourseSection = new HashMap<String, Collection<Transcript>>();
                    studentTranscriptsByCourseSectionMap.put(transcript.getStudentOid(), transcriptsByCourseSection);
                }

                String oid = transcript.getMasterScheduleOid() != null ? transcript.getMasterScheduleOid()
                        : transcript.getSchoolCourseOid();

                Collection<Transcript> storedTranscripts = transcriptsByCourseSection.get(oid);
                if (storedTranscripts == null) {
                    storedTranscripts = new ArrayList<Transcript>();
                    transcriptsByCourseSection.put(oid, storedTranscripts);
                }

                storedTranscripts.add(transcript);
            }
        } finally {
            iterator.close();
        }

        return studentTranscriptsByCourseSectionMap;
    }

    /**
     * Loads a map of java names to the Course End Date transcript column definition keyed on
     * Transcript Definition OID.
     */
    private void loadTranscriptDefCourseEndDateJavaNames() {
        /*
         * Loaded as needed.
         */
        m_transcriptDefCourseEndDateJavaName = new HashMap<String, String>();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_DATE));
        criteria.addEqualTo(TranscriptColumnDefinition.COL_DATE_TYPE,
                Integer.valueOf(TranscriptColumnDefinition.DATE_TYPE_COURSE_END_DATE));

        SubQuery subQuery =
                new SubQuery(Transcript.class, Transcript.COL_TRANSCRIPT_DEFINITION_OID, m_transcriptCriteria);

        criteria.addIn(TranscriptColumnDefinition.COL_TRANSCRIPT_DEFINITION_OID, subQuery);

        QueryByCriteria query = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                TranscriptColumnDefinition column = (TranscriptColumnDefinition) iterator.next();
                String javaName = column.getDataFieldConfig().getDataField().getJavaName();
                m_transcriptDefCourseEndDateJavaName.put(column.getTranscriptDefinitionOid(), javaName);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a Map of Trax Override Create Exam values keyed on ReferenceCode code. This is for the
     * "TRAX Override Code"
     * reference table.
     */
    private void loadTraxOverrideReferenceCodes() {
        m_traxOverrideReference = new HashMap<String, Boolean>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField dataDictionaryField = dictionary.findDataDictionaryFieldByAlias(TRN_TRAX_OVERRIDE_ALIAS);
        ReferenceTable referenceTable = dataDictionaryField.getReferenceTable();
        ExtendedDataDictionary extendedDataDictionary = referenceTable.getExtendedDataDictionary();

        /*
         * If the extended dictionary is null, it is not necessary to load the reference code values
         * as they are
         * on the extended dictionary.
         */
        if (extendedDataDictionary != null) {
            DataDictionary dataDictionary =
                    DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());

            if (dataDictionary != null) {
                X2Criteria referenceCodeCriteria = new X2Criteria();
                referenceCodeCriteria.addEqualTo(
                        ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                        referenceTable.getUserName());

                DataDictionaryField dictionaryField =
                        dataDictionary.findDataDictionaryFieldByAlias(TRAX_CREATE_EXAM_ALIAS);

                if (dictionaryField != null) {
                    String courseExamJavaName = dictionaryField.getJavaName();
                    String[] attributes = new String[] {ReferenceCode.COL_CODE, courseExamJavaName};

                    ReportQueryByCriteria query =
                            new ReportQueryByCriteria(ReferenceCode.class, attributes, referenceCodeCriteria);
                    ReportQueryIterator results = getBroker().getReportQueryIteratorByQuery(query);
                    try {
                        while (results.hasNext()) {
                            Object[] row = (Object[]) results.next();

                            String code = (String) row[0];
                            String createExam = (String) row[1];

                            m_traxOverrideReference.put(code, "1".equals(createExam) ? Boolean.TRUE : Boolean.FALSE);
                        }
                    } finally {
                        results.close();
                    }
                }
            }
        } else {
            logMessage("Unable to find the TRAX Override extended data dictionary with ID: "
                    + TRAX_EXTENDED_DICTIONARY_ID);
        }
    }
}
