/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2015 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleAttributes;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.model.business.schedule.StudentScheduleChangeManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.concurrent.TimeUnit;

/**
 * Processes pending Student Schedule Change records.
 *
 * Runs as a scheduled job within a school context. All pending records with an effective date on or
 * before the current
 * run date will be queried and processed. The order for processing is oldest effective date to
 * newest. Within each
 * date, Add records will be processed first, then Drop records, and finally Update records.
 *
 *
 * @author Follett Software Company
 */
public class ProcessFutureScheduleChanges extends ProcedureJavaSource {
    /**
     * Extensive logging will appear in the job results log when enabled.
     */
    private static final boolean m_enableLogging = true;

    /**
     * This is solely used to determine run time of procedure if logging is enabled.
     */
    private long m_procedureInitialized = 0;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        DateFormat formatter = null;

        if (m_enableLogging) {
            m_procedureInitialized = System.currentTimeMillis();

            formatter = new SimpleDateFormat("HH:mm");

            logMessage("***** Process Future Schedule Changes Procedure Initiated *****");
            logMessage("Processing start time: " + formatter.format(new PlainTime(m_procedureInitialized)));
        }

        School school = getSchool();

        // This procedure must only run for a single school.
        if (school != null) {
            if (m_enableLogging) {
                logMessage("-- Begin processing for school: " + school.getName());
            }

            Schedule schedule = ((SisSchool) school).getActiveSchedule();
            if (schedule != null) {
                StudentScheduleChangeManager sscm = new StudentScheduleChangeManager(getBroker());
                ScheduleManager scheduleManager = new ScheduleManager(getBroker());

                // Query all pending records with an effective date on or before today (using
                // school's TimeZone).
                Collection<StudentScheduleChange> pendingChanges =
                        sscm.getPendingRecords(new PlainDate(OrganizationManager.getTimeZone(school)),
                                null,
                                schedule.getOid(),
                                null,
                                null,
                                null,
                                null,
                                false,
                                StudentManager.getActiveStudentCodeList(school.getOrganization1()));

                // Maintain a unique list of Students who have pending records processed for further
                // updates.
                HashSet<SisStudent> studentsUpdated = new HashSet<SisStudent>();

                // Maintain a unique list of Sections to recalculate enrollment totals.
                HashSet<Section> sectionsUpdated = new HashSet<Section>();

                // Process each individual pending StudentScheduleChange record.
                for (StudentScheduleChange studentScheduleChange : pendingChanges) {
                    sscm.processPendingStudentScheduleChange(studentScheduleChange, true);

                    studentsUpdated.add(studentScheduleChange.getStudent());
                    sectionsUpdated.add(studentScheduleChange.getMasterSchedule());

                    if (m_enableLogging) {
                        logMessage("---- Processed pending " + studentScheduleChange.getChangeTypeCode() +
                                " change record for student: " + studentScheduleChange.getStudent().getNameView() +
                                " and section: " + studentScheduleChange.getMasterSchedule().getCourseView() + ".");
                    }
                }

                // Iterate over each student, adjust activities and load studies.
                for (SisStudent student : studentsUpdated) {
                    if (m_enableLogging) {
                        logMessage("---- Adjusting activities for student: " + student.getNameView());
                    }

                    sscm.adjustActivitiesAndRecordChanges((SisUser) getUser(), getBroker(), student.getOid(), schedule);

                    loadStudies(schedule, scheduleManager, student);

                    // Update student group attributes
                    scheduleManager.updateStudentGroupAttributes(student.getOid(), schedule, StudentSchedule.class,
                            StudentScheduleAttributes.GroupCode.House.ordinal());
                    scheduleManager.updateStudentGroupAttributes(student.getOid(), schedule, StudentSchedule.class,
                            StudentScheduleAttributes.GroupCode.Team.ordinal());
                }

                if (m_enableLogging) {
                    logMessage("-- End processing for school: " + school.getName());
                    logMessage("-- Recalculating future enrollment totals.");
                }

                // Recalculate current and future enrollment totals.
                scheduleManager.recalculateEnrollmentTotalForAllMasters(schedule, MasterSchedule.class, sectionsUpdated);

                if (m_enableLogging) {
                    long endTimeMillis = System.currentTimeMillis();
                    logMessage("***** Process Future Schedule Changes Procedure Completed *****");
                    logMessage("Processing end time: " + formatter.format(new PlainTime(endTimeMillis)));
                    logMessage("Total runtime: " + getDuration(endTimeMillis - m_procedureInitialized));
                }
            }
        }
    }

    /**
     * Converts the milliseconds into Hours, Minutes, and Seconds for readability.
     *
     * @param milliseconds long
     * @return String
     */
    private String getDuration(long milliseconds) {
        long hours = TimeUnit.MILLISECONDS.toHours(milliseconds);
        milliseconds -= TimeUnit.HOURS.toMillis(hours);

        long minutes = TimeUnit.MILLISECONDS.toMinutes(milliseconds);
        milliseconds -= TimeUnit.MINUTES.toMillis(minutes);

        long seconds = TimeUnit.MILLISECONDS.toSeconds(milliseconds);

        return hours + " Hours, " + minutes + " Minutes, " + seconds + " Seconds";
    }

    /**
     * Loads studies for a student, when applicable.
     *
     * @param schedule Schedule
     * @param scheduleManager ScheduleManager
     * @param student SisStudent
     */
    private void loadStudies(Schedule schedule, ScheduleManager scheduleManager, SisStudent student) {
        if (schedule.useStudyHall() && !student.getStudyExcludeIndicator(schedule.getStudentScheduleOid())) {
            // Only load the student into study hall sections if the "Use StudyHall" flag is set to
            // be true.
            Collection existingSections = scheduleManager.getStudentSchedule(MasterSchedule.class,
                    student.getOid(),
                    schedule.getOid(),
                    null);

            Collection<String> existingStudySections = scheduleManager.getSectionOidsForStudent(schedule,
                    student,
                    null,
                    MasterSchedule.class,
                    SchoolCourse.MASTER_TYPE_STUDY);

            Collection studySections = scheduleManager.loadStudyForOneStudent(student,
                    schedule,
                    MasterSchedule.class,
                    existingSections,
                    existingStudySections,
                    new Object[2],
                    new ArrayList(0));

            if (!CollectionUtils.isEmpty(studySections)) {
                if (m_enableLogging) {
                    logMessage("---- New studies added for student: " + student.getNameView());
                }

                // Save all the study sections.
                Collection<String> masterOidsAddedNew = new ArrayList<String>();
                Collection existingMasterOidsForStudent =
                        scheduleManager.getSectionOidsForStudent(schedule, student, null, MasterSchedule.class, null);

                scheduleManager.addStudentSchedules(MasterSchedule.class,
                        studySections,
                        new ArrayList(), // For studies, we do not have any sections to delete
                        (SisUser) getUser(),
                        existingMasterOidsForStudent,
                        masterOidsAddedNew,
                        null,
                        false,
                        null); // Creating studies on the fly - default effectiveDate to today
            } else if (m_enableLogging) {
                logMessage("---- No new studies added for student: " + student.getNameView());
            }
        }
    }
}
