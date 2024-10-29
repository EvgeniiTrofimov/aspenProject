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
package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.gradebook.GradebookManager;
import com.x2dev.sis.web.gradebook.LimitedColumnScoreGrid;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Procedure to create a new transcript record and/or a new student schedule record for any student
 * transcript records
 * that have a null final grade.
 *
 * @author X2 Development Corporation
 */
public class ScheduleTranscriptCopyProcedure extends ProcedureJavaSource implements SessionAwareProcedure {
    // Input parameters
    private static final String ALIAS_NAME = LimitedColumnScoreGrid.ALIAS_PRESET_COURSE_FIELDS;
    private static final String COMPLETION_DATE_ALIAS = "trn-completion-date";
    private static final String END_DATE_ALIAS = "trn-end-date";
    private static final String PARAM_COPY_SCHEDULES = "copySchedules";
    private static final String PARAM_COPY_TRANSCRIPTS = "copyTranscripts";
    private static final String PARAM_COPY_GRADEBOOK = "copyGradebook";
    private static final String START_DATE_ALIAS = "trn-start-date";

    // Used for logging totals
    private int m_copiedAssignments = 0;
    private int m_copiedCategories = 0;
    private int m_copiedScores = 0;
    private int m_copiedComments = 0;
    private int m_copiedSubmissions = 0;
    private int m_copiedSchedules = 0;
    private int m_copiedTranscripts = 0;

    /**
     * Sets the user data.
     *
     * @param userData void
     * @see com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure#setUserData(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void setUserData(UserDataContainer userData) {
        // Do nothing here
    }

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        boolean copySchedules = ((Boolean) getParameter(PARAM_COPY_SCHEDULES)).booleanValue();
        boolean copyTranscripts = ((Boolean) getParameter(PARAM_COPY_TRANSCRIPTS)).booleanValue();
        boolean copyGradebook = ((Boolean) getParameter(PARAM_COPY_GRADEBOOK)).booleanValue();

        if (this.isSchoolContext()) {
            DistrictSchoolYearContext lastYear = getLastYearContext();
            if (lastYear != null) {
                if (copyTranscripts || copySchedules || copyGradebook) {
                    Map<String, MasterSchedule> sectionMap = getSectionMap();

                    // for transcript copy
                    List<String> currentStudentTranscripts = new ArrayList<String>();

                    // for schedule copy
                    List<String> currentStudentSchedules = new ArrayList<String>();
                    List<String> pastStudentSchedules = new ArrayList<String>();

                    // For gradebook copy
                    Map<String, String> currentAssignments = new HashMap<String, String>();
                    Map<String, String> currentCategories = new HashMap<String, String>();

                    // If we aren't copying the gradebook, no need to run the queries to set the
                    // info
                    if (copyGradebook) {
                        currentCategories = getCurrentGradebookCategories();
                        currentAssignments = getCurrentGradebookAssignments();
                    }
                    // If we aren't copying the schedules, no need to run the queries to set the
                    // info
                    if (copySchedules) {
                        currentStudentSchedules = getCurrentStudentSchedules();
                        pastStudentSchedules = getPastStudentSchedules(lastYear);
                    }
                    // If we aren't copying the transcripts, no need to run the queries to set the
                    // info
                    if (copyTranscripts) {
                        currentStudentTranscripts = getCurrentStudentTranscripts();
                    }

                    try (QueryIterator iterator = getBroker().getIteratorByQuery(getTranscriptList(lastYear))) {
                        while (iterator.hasNext()) {
                            Transcript trn = (Transcript) iterator.next();
                            if (trn.getMasterSchedule() != null) {
                                MasterSchedule section = sectionMap.get(trn.getMasterSchedule().getCourseView());
                                if (section != null) {
                                    if (copySchedules) {
                                        if (pastStudentSchedules
                                                .contains(trn.getStudentOid() + trn.getMasterScheduleOid())) {
                                            if (!currentStudentSchedules
                                                    .contains(trn.getStudentOid() + section.getOid())) {
                                                copySchedules(trn, section);
                                            } else {
                                                logMessage("SKIPPED--Schedule already exists for: "
                                                        + trn.getStudent().getNameView() + " for course: "
                                                        + section.getCourseView());
                                            }
                                        } else {
                                            logMessage("SKIPPED--No student schedule for: "
                                                    + trn.getStudent().getNameView() + " for course: "
                                                    + section.getCourseView());
                                        }
                                    }

                                    if (copyGradebook) {
                                        copyGradebookItems(trn, section, currentAssignments, currentCategories);
                                    }

                                    if (copyTranscripts) {
                                        if (!currentStudentTranscripts
                                                .contains(trn.getStudentOid() + section.getOid())) {
                                            copyTranscripts(trn, section);
                                        } else {
                                            logMessage("SKIPPED--Transcript already exists for: "
                                                    + trn.getStudent().getNameView() + " for course: "
                                                    + section.getCourseView());
                                        }
                                    }

                                } else {
                                    logMessage("SKIPPED--No section available for: " + trn.getStudent().getNameView()
                                            + " for course: "
                                            + trn.getMasterSchedule().getCourseView());
                                }
                            } else {
                                logMessage("SKIPPED--No section on transcript for: " + trn.getStudent().getNameView());
                            }
                        }
                    }
                    if (copySchedules) {
                        logMessage("\n");
                        logMessage("---------------------------------");
                        logMessage("Total schedules copied: " + m_copiedSchedules);
                        logMessage("\n");
                    }
                    if (copyTranscripts) {
                        logMessage("\n");
                        logMessage("---------------------------------");
                        logMessage("Total transcripts copied: " + m_copiedTranscripts);
                    }
                    if (copyGradebook) {
                        logMessage("\n");
                        logMessage("---------------------------------");
                        logMessage("Total assignments copied: " + m_copiedAssignments);
                        logMessage("\n");
                        logMessage("---------------------------------");
                        logMessage("Total grades copied: " + m_copiedScores);
                        logMessage("\n");
                        logMessage("---------------------------------");
                        logMessage("Total categories copied: " + m_copiedCategories);
                        logMessage("\n");
                        logMessage("---------------------------------");
                        logMessage("Total comments copied: " + m_copiedComments);
                        logMessage("\n");
                        logMessage("---------------------------------");
                        logMessage("Total student submissions copied: " + m_copiedSubmissions);
                    }
                }
            } else {
                logMessage("There is no last year school context, procedure cannot be run");
            }
        } else {

            logMessage("This procedure can only be run in the school view. No action was taken.");
        }
    }

    /**
     * Loops through the assignments and copies them.
     *
     * @param trn Transcript
     * @param section MasterSchedule
     * @param currentAssignments Map<String,String>
     * @param currentCategories Map<String,String>
     */
    private void copyGradebookItems(Transcript trn,
                                    MasterSchedule section,
                                    Map<String, String> currentAssignments,
                                    Map<String, String> currentCategories) {
        Collection<GradebookColumnDefinition> assignments = trn.getMasterSchedule().getGradebookColumnDefinitions();
        GradebookManager manager = new GradebookManager(getBroker());
        for (GradebookColumnDefinition assignment : assignments) {
            String newCategoryOid = null;
            if (assignment.getColumnType() != null) {
                newCategoryOid =
                        currentCategories.get(assignment.getColumnType().getColumnType() + section.getCourseView());
            }
            String newAssignmentOid = currentAssignments.get(assignment.getColumnName() + section.getCourseView());
            if (StringUtils.isEmpty(newCategoryOid)) {
                if (assignment.getColumnType() != null) {
                    newCategoryOid = copyCategories(assignment.getColumnType(), section, currentCategories);
                }
            }
            if (StringUtils.isEmpty(newAssignmentOid)) {
                newAssignmentOid = copyAssignments(assignment, section, newCategoryOid, currentAssignments);
            }
            GradebookScore score = manager.getGradebookScore(assignment.getOid(), trn.getStudentOid());
            GradebookComment comment = manager.getGradebookComment(assignment.getOid(), trn.getStudentOid());
            GradebookStudentSubmission submission =
                    manager.getGradebookStudentSubmission(assignment.getOid(), trn.getStudentOid());
            copyGradebook(score, comment, submission, newAssignmentOid);
        }
    }

    /**
     * Copies the categories for a passed assignment and a new section. This is done as a precursor
     * for copying student
     * gradebook scores.
     *
     * @param category GradebookColumnType
     * @param section MasterSchedule
     * @param currentCategories Map<String,String>
     * @return String
     */
    private String copyCategories(GradebookColumnType category,
                                  MasterSchedule section,
                                  Map<String, String> currentCategories) {
        GradebookColumnType newCategory = (GradebookColumnType) category.copyBean();
        newCategory.setMasterScheduleOid(section.getOid());
        newCategory.setStaffOid(section.getPrimaryStaffOid());
        getBroker().saveBeanForced(newCategory);
        currentCategories.put(newCategory.getColumnType() + section.getCourseView(), newCategory.getOid());
        logMessage("Copied category: " + newCategory.getColumnType() + " for course: " + section.getCourseView());
        m_copiedCategories++;

        return newCategory.getOid();
    }

    /**
     * Copies the passed assignment for the passed in section. This sets the assignment dates ahead
     * by 1 year. This is
     * done as a precursor for copying student gradebook scores.
     *
     * @param assignment GradebookColumnDefinition
     * @param section MasterSchedule
     * @param newCategoryOid String
     * @param currentAssignments Map<String,String>
     * @return String
     */
    private String copyAssignments(GradebookColumnDefinition assignment,
                                   MasterSchedule section,
                                   String newCategoryOid,
                                   Map<String, String> currentAssignments) {
        GradebookColumnDefinition newAssignment = (GradebookColumnDefinition) assignment.copyBean();
        newAssignment.setMasterScheduleOid(section.getOid());
        newAssignment.setStaffOid(section.getPrimaryStaffOid());
        newAssignment.setDistrictContextOid(section.getSchedule().getDistrictContextOid());
        long shiftBy = DateUtils.DAY_TO_MS_FACTOR * 365;
        if (assignment.getDateAssigned() != null) {
            PlainDate newAssignedValue = new PlainDate(assignment.getDateAssigned().getTime() + shiftBy);
            newAssignment.setDateAssigned(newAssignedValue);
        }
        if (assignment.getDateDue() != null) {
            PlainDate newDueValue = new PlainDate(assignment.getDateDue().getTime() + shiftBy);
            newAssignment.setDateDue(newDueValue);
        }
        if (assignment.getSubmissionOpenDate() != null) {
            PlainDate newDueValue = new PlainDate(assignment.getSubmissionOpenDate().getTime() + shiftBy);
            newAssignment.setSubmissionOpenDate(newDueValue);
        }
        if (assignment.getSubmissionCloseDate() != null) {
            PlainDate newDueValue = new PlainDate(assignment.getSubmissionCloseDate().getTime() + shiftBy);
            newAssignment.setSubmissionCloseDate(newDueValue);
        }
        newAssignment.setColumnTypeOid(newCategoryOid);

        getBroker().saveBeanForced(newAssignment);
        currentAssignments.put(newAssignment.getColumnName() + section.getCourseView(), newAssignment.getOid());
        logMessage("Copied assignment: " + assignment.getColumnName() + " for course: " + section.getCourseView());
        m_copiedAssignments++;

        // Once we have a assignment OID, copy over the attachments
        Collection<AssignmentRepositoryItem> attachments = assignment.getAssignmentRepositoryItems();
        if (attachments != null && attachments.size() > 0) {
            for (AssignmentRepositoryItem attachment : attachments) {
                AssignmentRepositoryItem newAttachment = new AssignmentRepositoryItem(getBroker().getPersistenceKey());
                newAttachment.setColumnDefinitionOid(newAssignment.getOid());
                newAttachment.setContentItemOid(attachment.getContentItemOid());

                newAssignment.addToAssignmentRepositoryItems(newAttachment);
            }

            logMessage("Copied " + attachments.size() + " attachments for assignment: " + assignment.getColumnName());
            getBroker().saveBeanForced(newAssignment);
        }
        return newAssignment.getOid();
    }

    /**
     * Copies student scores, comments, and submission into the current year gradebook column
     * definition.
     *
     * @param score GradebookScore
     * @param comment GradebookComment
     * @param submission GradebookStudentSubmission
     * @param newAssignmentOid String
     */
    private void copyGradebook(GradebookScore score,
                               GradebookComment comment,
                               GradebookStudentSubmission submission,
                               String newAssignmentOid) {
        if (score != null) {
            GradebookScore newScore = (GradebookScore) score.copyBean();
            newScore.setColumnDefinitionOid(newAssignmentOid);

            boolean updateCourseStartDate = false;
            boolean updateCourseEndDate = false;

            if (score.getColumnDefinition().isPostColumn()
                    && TranscriptColumnDefinition.DATE_TYPE_COURSE_END_DATE == score.getColumnDefinition()
                            .getTranscriptColumnDefinition().getDateType()
                    && TranscriptColumnDefinition.COLUMN_TYPE_DATE == score.getColumnDefinition()
                            .getTranscriptColumnDefinition().getColumnTypeCode()) {
                updateCourseEndDate = true;
            } else if (score.getColumnDefinition().isPostColumn()
                    && TranscriptColumnDefinition.DATE_TYPE_COURSE_START_DATE == score.getColumnDefinition()
                            .getTranscriptColumnDefinition().getDateType()
                    && TranscriptColumnDefinition.COLUMN_TYPE_DATE == score.getColumnDefinition()
                            .getTranscriptColumnDefinition().getColumnTypeCode()) {
                updateCourseStartDate = true;
            } else {
                logMessage(
                        "Copied score for assignment: " + score.getColumnDefinition().getColumnName() + " for student: "
                                + score.getStudent().getNameView());
            }

            if (updateCourseEndDate) {
                PlainDate endDate = null;
                MasterSchedule section = newScore.getColumnDefinition().getMasterSchedule();
                if (section != null && section.getScheduleTerm() != null) {
                    Collection<ScheduleTermDate> termDates =
                            section.getScheduleTerm().getScheduleTermDates(getBroker());
                    if (!CollectionUtils.isEmpty(termDates)) {
                        for (ScheduleTermDate termDate : termDates) {

                            if (endDate == null || !endDate.after(termDate.getEndDate())) {
                                endDate = termDate.getEndDate();
                            }
                        }
                    }

                    newScore.setScore(endDate.toString());
                    logMessage(
                            "Updated course end date to " + endDate + " for assignment: "
                                    + score.getColumnDefinition().getColumnName() + " for student: "
                                    + score.getStudent().getNameView());
                }
            } else if (updateCourseStartDate) {
                PlainDate startDate = null;
                MasterSchedule section = newScore.getColumnDefinition().getMasterSchedule();
                if (section != null && section.getScheduleTerm() != null) {
                    Collection<ScheduleTermDate> termDates =
                            section.getScheduleTerm().getScheduleTermDates(getBroker());
                    if (!CollectionUtils.isEmpty(termDates)) {
                        for (ScheduleTermDate termDate : termDates) {
                            if (startDate == null || !startDate.before(termDate.getStartDate())) {
                                startDate = termDate.getStartDate();
                            }
                        }
                    }
                }
                logMessage(
                        "Updated course start date to " + startDate + " for assignment: "
                                + score.getColumnDefinition().getColumnName() + " for student: "
                                + score.getStudent().getNameView());
                newScore.setScore(startDate.toString());
            }

            m_copiedScores++;
            getBroker().saveBeanForced(newScore);
        }
        if (comment != null) {
            GradebookComment newComment = (GradebookComment) comment.copyBean();
            newComment.setColumnDefinitionOid(newAssignmentOid);
            getBroker().saveBeanForced(newComment);
            logMessage(
                    "Copied comment for assignment: " + comment.getColumnDefinition().getColumnName() + " for student: "
                            + comment.getStudent().getNameView());
            m_copiedComments++;
        }
        if (submission != null) {
            GradebookStudentSubmission newSubmission = (GradebookStudentSubmission) submission.copyBean();
            newSubmission.setGradebookColumnDefinitionOid(newAssignmentOid);
            getBroker().saveBeanForced(newSubmission);
            // PBI-8120 Procedure was returning NullPointerException because this was using the
            // score object
            logMessage("Copied student submission for assignment: "
                    + submission.getGradebookColumnDefinition().getColumnName()
                    + " for student: "
                    + submission.getStudent().getNameView());
            m_copiedSubmissions++;
        }
    }

    /**
     * Copies the schedules based on the passed student transcript and master schedule section.
     *
     * @param trn Transcript
     * @param section MasterSchedule
     */
    private void copySchedules(Transcript trn, MasterSchedule section) {
        StudentSchedule newStudentSchedule = new StudentSchedule(getBroker().getPersistenceKey());
        newStudentSchedule.setStudentOid(trn.getStudentOid());
        newStudentSchedule.setContentTermCode(section.getContentTermCodes());
        newStudentSchedule.setTermView(section.getTermView());
        newStudentSchedule.setSectionOid(section.getOid());
        newStudentSchedule.setScheduleOid(section.getScheduleOid());
        newStudentSchedule.setScheduleDisplay(section.getScheduleDisplay());
        getBroker().saveBeanForced(newStudentSchedule);
        logMessage("Copied student schedule for: " + trn.getStudent().getNameView() + " for course: "
                + section.getCourseView());
        m_copiedSchedules++;
    }

    /**
     * Copies the student transcript based on the passed old transcript and master schedule section.
     * It is assumed that
     * the course description and rubricdefinition remains from year to year.
     *
     * @param trn Transcript
     * @param section MasterSchedule
     */
    private void copyTranscripts(Transcript trn,
                                 MasterSchedule section) {
        Transcript transcript = (Transcript) trn.copyBean();
        transcript.setDistrictContextOid(getSchool().getCurrentContextOid());
        transcript.setMasterScheduleOid(section.getOid());
        transcript.setSchoolCourseOid(section.getSchoolCourseOid());
        transcript.setTeacherOid(section.getPrimaryStaffOid());
        transcript.setSchoolOid(getSchool().getOid());
        transcript.setCourseDescription(section.getDescription());
        transcript.setFieldValueByAlias(ALIAS_NAME, BooleanAsStringConverter.TRUE);
        PlainDate startDate = null;
        PlainDate endDate = null;
        for (ScheduleTermDate termDate : section.getScheduleTerm().getScheduleTermDates()) {
            if (startDate == null || startDate.after(termDate.getStartDate())) {
                startDate = termDate.getStartDate();
            }
            if (endDate == null || endDate.before(termDate.getEndDate())) {
                endDate = termDate.getEndDate();
            }
        }
        transcript.setFieldValueByAlias(START_DATE_ALIAS, startDate.toString());
        transcript.setFieldValueByAlias(END_DATE_ALIAS, endDate.toString());

        getBroker().saveBeanForced(transcript);
        // Marking the copied (old) TRN as copied so it can be cleaned up.
        trn.setCopyContinueInd(true);
        getBroker().saveBeanForced(trn);
        logMessage("Copied student transcript for: " + trn.getStudent().getNameView() + " for course: "
                + section.getCourseView());
        m_copiedTranscripts++;
    }

    /**
     * Populates the map for the current year gradebook assignments.
     *
     * @return Map<String, String>
     *
     */
    private Map<String, String> getCurrentGradebookAssignments() {
        Map<String, String> currentAssignments = new HashMap<String, String>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(GradebookColumnDefinition.REL_MASTER_SCHEDULE + "." + MasterSchedule.COL_SCHEDULE_OID,
                ((SisSchool) getSchool()).getActiveScheduleOid());
        String[] columns = new String[] {GradebookColumnDefinition.COL_COLUMN_NAME,
                GradebookColumnDefinition.REL_MASTER_SCHEDULE + "." + MasterSchedule.COL_COURSE_VIEW,
                X2BaseBean.COL_OID};
        ReportQueryByCriteria query = new ReportQueryByCriteria(GradebookColumnDefinition.class, columns, criteria);

        try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String key = (String) row[0] + (String) row[1];
                currentAssignments.put(key, (String) row[2]);
            }
        }
        return currentAssignments;
    }

    /**
     * Populates the map for the current year gradebook categories.
     *
     * @return Map<String, String>
     *
     */
    private Map<String, String> getCurrentGradebookCategories() {
        Map<String, String> currentCategories = new HashMap<String, String>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(GradebookColumnType.REL_MASTER_SCHEDULE + "." + MasterSchedule.COL_SCHEDULE_OID,
                ((SisSchool) getSchool()).getActiveScheduleOid());
        String[] columns = new String[] {GradebookColumnType.COL_COLUMN_TYPE,
                GradebookColumnType.REL_MASTER_SCHEDULE + "." + MasterSchedule.COL_COURSE_VIEW, X2BaseBean.COL_OID};
        ReportQueryByCriteria query = new ReportQueryByCriteria(GradebookColumnType.class, columns, criteria);

        try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String key = (String) row[0] + (String) row[1];
                currentCategories.put(key, (String) row[2]);
            }
        }
        return currentCategories;
    }

    /**
     * Gets the student and section oid information from the current student schedules and puts this
     * into an ArrayList.
     * This is used to see if a student schedule record is already created for this student/section
     * combination.
     *
     * @return List<String>
     *
     */
    private List<String> getCurrentStudentSchedules() {
        List<String> currentStudentSchedules = new ArrayList<String>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentSchedule.COL_SCHEDULE_OID, ((SisSchool) getSchool()).getActiveScheduleOid());
        String[] columns = new String[] {StudentSchedule.COL_STUDENT_OID, StudentSchedule.COL_SECTION_OID};
        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentSchedule.class, columns, criteria);

        try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String studentSection = (String) row[0] + (String) row[1];
                currentStudentSchedules.add(studentSection);
            }
        }
        return currentStudentSchedules;
    }

    /**
     * Gets the student and section oid information from the current student transcripts and puts
     * this into an
     * ArrayList. This is used to see if a transcript record is already created for this
     * student/section combination.
     *
     * @return List<String>
     *
     */
    private List<String> getCurrentStudentTranscripts() {
        List<String> currentStudentTranscripts = new ArrayList<String>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        criteria.addEqualTo(Transcript.COL_SCHOOL_OID, getSchool().getOid());
        String[] columns = new String[] {Transcript.COL_STUDENT_OID, Transcript.COL_MASTER_SCHEDULE_OID};
        ReportQueryByCriteria query = new ReportQueryByCriteria(Transcript.class, columns, criteria);

        try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String studentTranscript = (String) row[0] + (String) row[1];
                currentStudentTranscripts.add(studentTranscript);
            }
        }
        return currentStudentTranscripts;
    }

    /**
     * Gets last year's districtschoolyearcontext. This is used to get last year's transcript
     * records.
     *
     * @return DistrictSchoolYearContext
     */
    private DistrictSchoolYearContext getLastYearContext() {
        Criteria yearCriteria = new Criteria();
        Integer schoolYear = Integer.valueOf(getCurrentContext().getSchoolYear() - 1);
        yearCriteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, schoolYear);
        BeanQuery yearQuery = new BeanQuery(DistrictSchoolYearContext.class, yearCriteria);
        return (DistrictSchoolYearContext) getBroker().getBeanByQuery(yearQuery);
    }

    /**
     * Gets the student and section oid information from last year's student schedules and puts this
     * into an ArrayList.
     * This is used to see if a student schedule record is existed for this student/section
     * combination.
     *
     * @param lastYear DistrictSchoolYearContext
     * @return List<String>
     */
    private List<String> getPastStudentSchedules(DistrictSchoolYearContext lastYear) {
        List<String> pastStudentSchedules = new ArrayList<String>();

        X2Criteria scheduleCriteria = new X2Criteria();
        scheduleCriteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, lastYear.getOid());
        scheduleCriteria.addNotEmpty(SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, getBroker().getPersistenceKey());
        scheduleCriteria.addEqualTo(SchoolScheduleContext.COL_SCHOOL_OID, getSchool().getOid());

        String[] scheduleColumn = new String[] {SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID};
        ReportQueryByCriteria scheduleQuery =
                new ReportQueryByCriteria(SchoolScheduleContext.class, scheduleColumn, scheduleCriteria);
        String scheduleOid = null;
        try (ReportQueryIterator scheduleIterator = getBroker().getReportQueryIteratorByQuery(scheduleQuery)) {
            while (scheduleIterator.hasNext()) {
                Object[] row = (Object[]) scheduleIterator.next();
                scheduleOid = (String) row[0];
            }
        }

        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentSchedule.COL_SCHEDULE_OID, scheduleOid);
        String[] columns = new String[] {StudentSchedule.COL_STUDENT_OID, StudentSchedule.COL_SECTION_OID};
        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentSchedule.class, columns, criteria);

        try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String studentSection = (String) row[0] + (String) row[1];
                pastStudentSchedules.add(studentSection);
            }
        }
        return pastStudentSchedules;
    }

    /**
     * Returns a hashmap of the master schedule using the course view as the key. This is used to
     * find current sections
     * that are the same as last year's section. This loads all of the masterschedule beans into
     * memory, so this might
     * cause slowness.
     *
     * @return HashMap<String, MasterSchedule>
     */
    private HashMap<String, MasterSchedule> getSectionMap() {
        Criteria masterCriteria = new Criteria();
        masterCriteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, ((SisSchool) getSchool()).getActiveScheduleOid());
        BeanQuery masterQuery = new BeanQuery(MasterSchedule.class, masterCriteria);
        return (HashMap<String, MasterSchedule>) getBroker().getMapByQuery(masterQuery, MasterSchedule.COL_COURSE_VIEW,
                2000);
    }

    /**
     * This returns a query to get the list of transcripts for the given districtschoolyearcontext.
     *
     * @param context DistrictSchoolYearContext
     * @return BeanQuery
     */
    private BeanQuery getTranscriptList(DistrictSchoolYearContext context) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEmpty(Transcript.COL_FINAL_GRADE, getBroker().getPersistenceKey());
        DataDictionaryField field = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                .findDataDictionaryFieldByAlias(COMPLETION_DATE_ALIAS);
        if (field != null) {
            criteria.addEmpty(field.getJavaName(), getBroker().getPersistenceKey());
        }
        criteria.addEqualTo(Transcript.COL_SCHOOL_OID, getSchool().getOid());
        criteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, context.getOid());
        criteria.addNotEqualTo(Transcript.REL_STUDENT + "." + Student.COL_ENROLLMENT_STATUS, "Inactive");
        criteria.addNotEqualTo(Transcript.REL_STUDENT + "." + Student.COL_ENROLLMENT_STATUS, "Withdrawn");
        return new BeanQuery(Transcript.class, criteria);
    }
}
