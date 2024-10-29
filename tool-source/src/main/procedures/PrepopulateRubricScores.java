/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */


import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.BeanCopier;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.web.gradebook.LimitedColumnScoreGrid;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Steps over the current list of sections and attempts to pre-populate rubric scores from the prior
 * grading term
 * into the corresponding gradebook post columns for the current term.
 *
 * Pre-requisites:
 * <ul>
 * <li>Tool must be run from a list of sections
 * <li>All those sections must have had prepare grade input run for at least two terms (a current
 * and prior term)
 * <li>One or more sections are attached to courses that grade with rubrics. Non-rubric graded
 * sections are skipped.
 * </ul>
 *
 * @author Follett Software Company
 */
public class PrepopulateRubricScores extends ProcedureJavaSource {
    private static final boolean VERBOSE_OUTPUT = false;
    private static final String GRADE_TERM_OID_CURRENT = "gradeTermOidCurrent";
    private static final String GRADE_TERM_OID_PRIOR = "gradeTermOidPrior";
    private static final String REPORTING_TYPE = "reportingType";
    private static final String RUBRIC_PERFORMANCE_RELATION = "relRbaRapOid";

    private Collection<String> m_currentStudentOids;
    private QueryByCriteria m_query;
    private Map<String, TranscriptColumnDefinition> m_cacheTermColumns;

    private int m_sectionsSkipped = 0;
    private int m_sectionsUpdated = 0;

    /**
     * Steps over the current list of sections and attempts to pre-populate rubric scores from the
     * prior grading term
     * into the corresponding gradebook post columns for the current term.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        m_cacheTermColumns = new HashMap<String, TranscriptColumnDefinition>();

        String gradeTermCurrent = (String) getParameter(GRADE_TERM_OID_CURRENT);
        GradeTerm termCurrent = (GradeTerm) getBroker().getBeanByOid(GradeTerm.class, gradeTermCurrent);

        String gradeTermPrior = (String) getParameter(GRADE_TERM_OID_PRIOR);
        GradeTerm termPrior = (GradeTerm) getBroker().getBeanByOid(GradeTerm.class, gradeTermPrior);

        Integer reportingType = (Integer) getParameter(REPORTING_TYPE);

        if (termCurrent.equals(termPrior)) {
            recordMustSelectUniqueTermsError();
        } else {
            if (m_query == null) {
                recordUnableToFindSectionError();
            } else {
                QueryIterator masterIterator = getBroker().getIteratorByQuery(m_query);
                try {
                    while (masterIterator.hasNext()) {
                        MasterSchedule master = (MasterSchedule) masterIterator.next();
                        copyScoresForSection(master, termCurrent, termPrior, reportingType);
                    }
                } finally {
                    masterIterator.close();
                }
            }
        }
        recordOutputStatistics(m_sectionsUpdated, m_sectionsSkipped);
    }

    /**
     * Copies the posted rubric scores for the section from the prior term's transripts to the
     * current term's post columns.
     *
     * @param section MasterSchedule
     * @param currentTerm GradeTerm
     * @param priorTerm GradeTerm
     * @param reportingType Integer
     */
    private void copyScoresForSection(MasterSchedule section,
                                      GradeTerm currentTerm,
                                      GradeTerm priorTerm,
                                      Integer reportingType) {
        SchoolCourse course = section.getSchoolCourse();
        String rubricDefinitionOid = course.getRubricDefinitionOid();

        if (!StringUtils.isEmpty(rubricDefinitionOid) && !course.getHideGradeInputIndicator()) {
            TranscriptDefinition tDefinition = course.getTranscriptDefinition();
            if (tDefinition != null &&
                    (currentTerm != null &&
                            currentTerm.getGradeTermDefinitionOid().equals(tDefinition.getGradeTermDefinitionOid()))) {
                TranscriptColumnDefinition currentTermColumn =
                        getRubricTranscriptColumnForTerm(tDefinition, currentTerm.getOid(), reportingType);

                if (currentTermColumn == null) {
                    recordUnexpectedError("Rubric transcript column for current term could not be identified");
                    m_sectionsSkipped++;
                } else {
                    TranscriptColumnDefinition priorTermColumn =
                            getRubricTranscriptColumnForTerm(tDefinition, priorTerm.getOid(), reportingType);

                    GradebookColumnDefinition postColumnForCurrentTerm =
                            getPostColumnForTerm(section, currentTerm, rubricDefinitionOid, currentTermColumn);

                    if (postColumnForCurrentTerm == null) {
                        recordUnpreparedSectionError(section);
                        m_sectionsSkipped++;
                    } else if (priorTermColumn == null) {
                        recordUnexpectedError("Rubric transcript column for prior term could not be identified");
                        m_sectionsSkipped++;
                    } else {
                        X2Criteria rubricScoreCriteria = new X2Criteria();
                        rubricScoreCriteria.addEqualTo(TranscriptRubric.COL_TRANSCRIPT_COLUMN_DEFINITION_OID,
                                priorTermColumn.getOid());
                        rubricScoreCriteria.addEqualTo(
                                TranscriptRubric.REL_TRANSCRIPT + PATH_DELIMITER + Transcript.COL_MASTER_SCHEDULE_OID,
                                section.getOid());

                        BeanQuery rubricScoreQuery = new BeanQuery(TranscriptRubric.class, rubricScoreCriteria);
                        Collection<TranscriptRubric> transcriptRubrics =
                                getBroker().getCollectionByQuery(rubricScoreQuery);

                        if (transcriptRubrics.isEmpty()) {
                            recordNoPriorTermScores(section, priorTerm);
                        } else {
                            boolean updatePerformed = false;

                            // Step over each student in section (who previously was scored against this
                            // rubric)
                            for (TranscriptRubric transcriptRubric : transcriptRubrics) {
                                String studentOid = transcriptRubric.getTranscript().getStudentOid();

                                // If we have a current list of students, filter down to only those
                                // students to avoid creating orphan RBA/RAP records
                                if (m_currentStudentOids == null || m_currentStudentOids.contains(studentOid)) {
                                    RubricAssessment rubricAssessment = transcriptRubric.getRubricAssessment();

                                    X2Criteria performanceCriteria = new X2Criteria();
                                    performanceCriteria
                                            .addEqualTo(
                                                    RubricAssessmentPerformance.REL_RUBRIC_ASSESSMENT + PATH_DELIMITER +
                                                            RubricAssessment.REL_SCORES + PATH_DELIMITER +
                                                            GradebookScore.COL_STUDENT_OID, studentOid);
                                    performanceCriteria
                                            .addEqualTo(
                                                    RubricAssessmentPerformance.REL_RUBRIC_ASSESSMENT + PATH_DELIMITER +
                                                            RubricAssessment.REL_SCORES + PATH_DELIMITER +
                                                            GradebookScore.COL_COLUMN_DEFINITION_OID,
                                                    postColumnForCurrentTerm.getOid());
                                    BeanQuery performanceQuery =
                                            new BeanQuery(RubricAssessmentPerformance.class, performanceCriteria);

                                    Collection<RubricAssessmentPerformance> alreadyEnteredPerformances =
                                            getBroker().getCollectionByQuery(performanceQuery);

                                    if (!alreadyEnteredPerformances.isEmpty()) {
                                        Collection<String> alreadyCoveredCriterion =
                                                CollectionUtils.getPropertySet(alreadyEnteredPerformances,
                                                        RubricAssessmentPerformance.COL_RUBRIC_CRITERION_OID);
                                        String preEnteredAssessmentOid =
                                                alreadyEnteredPerformances.iterator().next().getRubricAssessmentOid();

                                        for (RubricAssessmentPerformance performance : rubricAssessment
                                                .getAssessmentPerformances(getBroker())) {
                                            if (!alreadyCoveredCriterion
                                                    .contains(performance.getRubricCriterionOid())) {
                                                RubricAssessmentPerformance newPerformance =
                                                        (RubricAssessmentPerformance) performance.copyBean();
                                                newPerformance.setRubricAssessmentOid(preEnteredAssessmentOid);
                                                getBroker().saveBeanForced(newPerformance);

                                                recordRubricPerformanceCopyForStudent(studentOid, newPerformance);
                                                updatePerformed = true;
                                            }
                                        }
                                    } else {
                                        BeanCopier copier = new BeanCopier(getBroker());
                                        LinkedList<String> relationships = new LinkedList<String>();
                                        relationships.add(RUBRIC_PERFORMANCE_RELATION);
                                        RubricAssessment clonedAssessment =
                                                (RubricAssessment) copier.copy(rubricAssessment, relationships);

                                        GradebookScore score = new GradebookScore(getBroker().getPersistenceKey());
                                        score.setStudentOid(studentOid);
                                        score.setRubricAssessmentOid(clonedAssessment.getOid());
                                        score.setColumnDefinitionOid(postColumnForCurrentTerm.getOid());
                                        score.setScore("");

                                        getBroker().saveBeanForced(score);
                                        recordRubricScoreCopyForStudent(studentOid, clonedAssessment);
                                        updatePerformed = true;
                                    }
                                } else {
                                    recordSkippingNoLongerEnrolledStudent(studentOid, section.getOid());
                                }
                            }

                            if (updatePerformed) {
                                m_sectionsUpdated++;
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Resolves the GCD record associated with the passed term and section, which is used for
     * anchoring rubric scores.
     *
     * @param section MasterSchedule
     * @param gradeTerm GradeTerm
     * @param rubricDefinitionOid String
     * @param transcriptColumn TranscriptColumnDefinition
     * @return GradebookColumnDefinition
     */
    private GradebookColumnDefinition getPostColumnForTerm(MasterSchedule section,
                                                           GradeTerm gradeTerm,
                                                           String rubricDefinitionOid,
                                                           TranscriptColumnDefinition transcriptColumn) {
        X2Criteria postColumnsCriteria = new X2Criteria();
        postColumnsCriteria.addEqualTo(GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID, section.getOid());
        postColumnsCriteria.addEqualTo(GradebookColumnDefinition.COL_TRANSCRIPT_COLUMN_DEFINITION_OID,
                transcriptColumn.getOid());
        postColumnsCriteria.addEqualTo(GradebookColumnDefinition.COL_GRADE_TERM_OID, gradeTerm.getOid());// probably
                                                                                                         // redundant
                                                                                                         // with
                                                                                                         // above
        postColumnsCriteria.addEqualTo(GradebookColumnDefinition.COL_RUBRIC_DEFINITION_OID, rubricDefinitionOid);

        BeanQuery postColumnsQuery = new BeanQuery(GradebookColumnDefinition.class, postColumnsCriteria);

        return (GradebookColumnDefinition) getBroker().getBeanByQuery(postColumnsQuery);
    }

    /**
     * Resolves the transcript column definition used for rubric scores for the passed transcript
     * definition and term.
     * If none exists, null is returned.
     *
     * @param tDefinition TranscriptDefinition
     * @param gradeTermOid String
     * @param reportingType Integer
     * @return TranscriptColumnDefinition
     */
    private TranscriptColumnDefinition getRubricTranscriptColumnForTerm(TranscriptDefinition tDefinition,
                                                                        String gradeTermOid,
                                                                        Integer reportingType) {
        TranscriptColumnDefinition transcriptColumn = null;

        if (tDefinition != null && !StringUtils.isEmpty(gradeTermOid)) {
            String columnLookupKey = tDefinition.getOid() + gradeTermOid;
            if (m_cacheTermColumns.containsKey(columnLookupKey)) {
                transcriptColumn = m_cacheTermColumns.get(columnLookupKey);
            } else {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(TranscriptColumnDefinition.COL_TRANSCRIPT_DEFINITION_OID, tDefinition.getOid());
                criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                        Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_RUBRIC));
                criteria.addEqualTo(TranscriptColumnDefinition.COL_GRADE_TERM_OID, gradeTermOid);
                criteria.addEqualTo(TranscriptColumnDefinition.COL_REPORT_TYPE, reportingType);

                BeanQuery query = new BeanQuery(TranscriptColumnDefinition.class, criteria);
                transcriptColumn = (TranscriptColumnDefinition) getBroker().getBeanByQuery(query);

                m_cacheTermColumns.put(columnLookupKey, transcriptColumn);
            }
        }

        return transcriptColumn;
    }

    /**
     * Prints an error message if the user selected two identical terms when running this procedure.
     */
    private void recordMustSelectUniqueTermsError() {
        logMessage("No sections updated; two unique terms must be selected.");
    }

    /**
     * If VERBOSE, print an error message indicating that no prior rubric scores were found for the
     * passed section.
     *
     * @param section MasterSchedule
     * @param priorTerm GradeTerm
     */
    private void recordNoPriorTermScores(MasterSchedule section, GradeTerm priorTerm) {
        if (VERBOSE_OUTPUT) {
            logMessage("No rubric assessment scores were found for the section " + section.getCourseView()
                    + ", for the prior term " + priorTerm.getGradeTermId());
        }
    }

    /**
     * Prints high level statistics indicating how many sections were effected by this procedure.
     *
     * @param sectionsUpdated int
     * @param sectionsSkipped int
     */
    private void recordOutputStatistics(int sectionsUpdated, int sectionsSkipped) {
        logMessage(sectionsUpdated + " sections updated, " + sectionsSkipped + " sections skipped.");
    }

    /**
     * If VERBOSE, print a message describing when we copy individual RAP scores.
     *
     * @param studentOid String
     * @param newPerformance RubricAssessmentPerformance
     */
    private void recordRubricPerformanceCopyForStudent(String studentOid, RubricAssessmentPerformance newPerformance) {
        if (VERBOSE_OUTPUT) {
            logMessage("Copied rubric performance record for student=" + studentOid + ", criterion="
                    + newPerformance.getRubricCriterionOid());
        }
    }

    /**
     * If VERBOSE, print a message describing the cloning of rubric assessments.
     *
     * @param studentOid String
     * @param clonedAssessment RubricAssessment
     */
    private void recordRubricScoreCopyForStudent(String studentOid, RubricAssessment clonedAssessment) {
        if (VERBOSE_OUTPUT) {
            logMessage("Cloned: student=" + studentOid + ", assessment clone=" + clonedAssessment.getOid());
        }
    }

    /**
     * Prints a message describing students that are skipped due to no longer being enrolled in the
     * passed section.
     *
     * @param studentOid String
     * @param sectionOid String
     */
    private void recordSkippingNoLongerEnrolledStudent(String studentOid, String sectionOid) {
        if (VERBOSE_OUTPUT) {
            logMessage("Skipping student " + studentOid
                    + " with prior rubric scores since they are no longer enrolled in the section " + sectionOid + ".");
        }
    }

    /**
     * Prints an error message indicating that the procedure is likely not being run from a
     * supported location.
     */
    private void recordUnableToFindSectionError() {
        logMessage(
                "Unable to resolve section(s) to run for. Procedure must be run from School View > Grade Input or Staff View > Gradebook");
    }

    /**
     * Writes the passed string to the log- assumed use is for unhandled errors.
     *
     * @param string String
     */
    private void recordUnexpectedError(String string) {
        logMessage(string);
    }

    /**
     * Prints an error message indicating that grade input has not been prepared for the destination
     * term.
     *
     * @param section MasterSchedule
     */
    private void recordUnpreparedSectionError(MasterSchedule section) {
        logMessage("Unable to copy rubric scores for section " + section.getCourseView()
                + "; grade input has not yet been prepared for the selected term.");
    }

    /**
     * Returns a query which identifies master schedule records accessible in the current context.
     *
     * @param userData UserDataContainer
     * @return QueryByCriteria
     */
    private QueryByCriteria resolveMasterScheduleQuery(UserDataContainer userData) {
        QueryByCriteria sectionQuery = null;

        if (ApplicationContext.SCHOOL == userData.getCurrentNode().getNavConfig().getApplicationContext()) {
            if (userData.getCurrentNode().isList()
                    && userData.getCurrentList().getDataClass() == MasterSchedule.class) {
                sectionQuery = userData.getCurrentList().getQuery();
            } else {
                X2BaseBean masterSchedule = userData.getCurrentRecord(MasterSchedule.class);

                if (masterSchedule != null) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(X2BaseBean.COL_OID, masterSchedule.getOid());
                    sectionQuery = new QueryByCriteria(MasterSchedule.class, criteria);
                }
            }
        } else if (ApplicationContext.STAFF == userData.getCurrentNode().getNavConfig().getApplicationContext()) {
            if (userData.getCurrentNode().isList()
                    && userData.getCurrentList().getDataClass() == ScheduleTeacher.class) {
                QueryByCriteria scheduleTeacherQuery = userData.getCurrentList().getQuery();
                ModelBroker broker = new ModelBroker(userData.getPrivilegeSet());

                Collection<ScheduleTeacher> scheduleTeacherRecords = broker.getCollectionByQuery(scheduleTeacherQuery);

                if (scheduleTeacherRecords.size() > 0) {
                    Collection<String> oids = CollectionUtils.getPropertyCollection(scheduleTeacherRecords,
                            ScheduleTeacher.COL_SECTION_OID);

                    X2Criteria sectionCriteria = new X2Criteria();
                    sectionCriteria.addIn(X2BaseBean.COL_OID, oids);

                    sectionQuery = new QueryByCriteria(MasterSchedule.class, sectionCriteria);
                }
            } else {
                ScheduleTeacher scheduleTeacher = userData.getCurrentRecord(ScheduleTeacher.class);

                if (scheduleTeacher != null) {
                    String sectionOid = scheduleTeacher.getSectionOid();

                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(X2BaseBean.COL_OID, sectionOid);
                    sectionQuery = new QueryByCriteria(MasterSchedule.class, criteria);
                }
            }
        }

        // If we are on a score grid, store the list of current students for use in filtering later
        if (userData.getCurrentNode().isGrid() && userData.getSessionObject() instanceof LimitedColumnScoreGrid) {
            LimitedColumnScoreGrid grid = (LimitedColumnScoreGrid) userData.getSessionObject();

            m_currentStudentOids = CollectionUtils.getPropertyCollection(grid.getStudents(), X2BaseBean.COL_OID);
        }

        return sectionQuery;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_query = resolveMasterScheduleQuery(userData);
    }
}
