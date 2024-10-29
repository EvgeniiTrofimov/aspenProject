/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Massachusetts state report for SCS export.
 * This class implements the data export for Mass SCS export.
 *
 * @author X2 Development Corporation
 */
public class Scs extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the SCS export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class SCSEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        Scs m_scsData = null;
        List<ScheduleInfo> m_schedules = null;

        /**
         * Instantiates a new SCS entity.
         */
        /*
         * Public no argument constructor for dynamic instantiation.
         */
        public SCSEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";
            ScheduleInfo info = getScheduleInfo();
            if (info != null) {
                name += info.m_section.getCourseView();
            }

            return name;
        }

        /**
         * Returns the schedule info record for the schedule record(s)
         * based in the current row value.
         *
         * @return ScheduleInfo
         */
        public ScheduleInfo getScheduleInfo() {
            ScheduleInfo info = null;
            if (m_schedules != null && getCurrentRow() < m_schedules.size() && getCurrentRow() >= 0) {
                info = m_schedules.get(getCurrentRow());
            }
            return info;
        }

        /**
         * Initialize the entity for the student bean provided.
         * This method finds the student schedule and student schedule change records for the
         * student
         * and generates a list of reportable schedule items.
         * The entity can produce multiple rows from these results.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            Map<String, ScheduleInfo> scheduleMap = new HashMap<String, ScheduleInfo>();
            m_scsData = (Scs) data;
            SisStudent student = (SisStudent) bean;
            PlainDate gradDate = null;
            EnrollmentSnapshot snapshot =
                    new EnrollmentSnapshot(student, m_scsData.m_reportDate, getData().getBroker());

            /*
             * Determine membership days. This aligns reported students with students reported in
             * SIMS.
             * Report only students with membership days prior to report date.
             */
            String membershipDaysStr = getMembershipDays(this, snapshot, data.getFieldDefinition(1));
            int membershipDays = 0;
            if (!StringUtils.isEmpty(membershipDaysStr)) {
                try {
                    membershipDays = Integer.parseInt(membershipDaysStr);
                } catch (NumberFormatException nfe) {
                    // Default to zero. Will not report.
                }
            }

            if (membershipDays > 0 && student.getSchool() != null && student.getSchool().getActiveSchedule() != null) {
                // Find students who withdrew in the school year but before the first reporting
                // period.
                PlainDate withdrawalDate = null;
                PlainDate firstReportDate = m_scsData.m_reportDate;
                if (m_scsData.m_priorReportDates.length > 0) {
                    firstReportDate = m_scsData.m_priorReportDates[0];
                }
                PlainDate schoolStartDate = student.getSchool().getActiveSchedule().getStartDate();

                /*
                 * See if this is a graduated senior based on the withdrawal code
                 * and any enrollment record that might contain it.
                 * If so, capture the withdrawal date (graduation date).
                 *
                 * See if this student has withdrawn before the first reporting date (Oct 1).
                 * If so, report their last known schedule.
                 */
                if (m_scsData.m_withdrawlCodes != null && m_scsData.m_withdrawlCodes.size() > 0) {
                    for (StudentEnrollment enrollment : student.getEnrollments()) {
                        if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()) &&
                                enrollment.getEnrollmentCode() != null &&
                                m_scsData.m_withdrawlCodes.contains(enrollment.getEnrollmentCode().toUpperCase())) {
                            gradDate = enrollment.getEnrollmentDate();
                        }
                    }
                }

                if (!StudentManager.isActiveStudent(data.getOrganization(), student.getEnrollmentStatus())) {
                    for (StudentEnrollment enrollment : student.getEnrollments()) {
                        // Find last enrollment withdrawal record.
                        if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()) &&
                                enrollment.getSchoolOid().equals(student.getSchoolOid()) &&
                                (withdrawalDate == null || withdrawalDate.before(enrollment.getEnrollmentDate()))) {
                            withdrawalDate = enrollment.getEnrollmentDate();
                        }
                    }
                }
                if (withdrawalDate != null &&
                        firstReportDate.after(withdrawalDate) && // firstReportDate == {Oct. 1}
                        (schoolStartDate.before(withdrawalDate) ||
                                schoolStartDate.equals(withdrawalDate))) {
                    // Keep the withdrawal date.
                } else {
                    withdrawalDate = null;
                }


                /*
                 * Collect all schedule relevant to this student.
                 * Then assign transcript records to the student schedule records if possible.
                 *
                 * schedules = collection of student schedules for this student.
                 *
                 * scheduleChanges = collection of student schedule change drops for this student.
                 *
                 * transcriptMap = map from student schedule OID or Schedule Change OID to a student
                 * transcript is assigned to it.
                 * (prevent duplicate/multi assignment.)
                 * transcriptsAssigned = set of transcript OID for those that have already been
                 * assigned.
                 */
                Collection<StudentSchedule> schedules = m_scsData.m_scheduleMap.get(student.getOid());
                Collection<StudentScheduleChange> scheduleChanges = m_scsData.m_scheduleChangeMap.get(student.getOid());
                Collection<Transcript> transcripts = m_scsData.m_transcriptMap.get(student.getOid());
                // String activeScheduleOid = snapshot.getSchool().getActiveScheduleOid();
                List<String> activeScheduleOids = ((Scs) getData()).m_activeScheduleOids;

                // Create a list of StudentScheduleChange Oids of sections that were either added
                // and dropped or dropped and added the same day.
                // These are to be excluded.
                StudentScheduleChange lastStudentScheduleChange = null;
                ArrayList<String> changeSameDayAddedDropped = new ArrayList<String>();
                if (scheduleChanges != null) {
                    for (StudentScheduleChange studentScheduleChange : scheduleChanges) {
                        if (lastStudentScheduleChange != null) {
                            if ((studentScheduleChange.getStudentOid()
                                    .equals(lastStudentScheduleChange.getStudentOid()))
                                    && (studentScheduleChange.getMasterScheduleOid()
                                            .equals(lastStudentScheduleChange.getMasterScheduleOid()))
                                    && (studentScheduleChange.getEffectiveDate()
                                            .equals(lastStudentScheduleChange.getEffectiveDate()))
                                    && (StudentScheduleChange.CODE_ADD
                                            .equals(lastStudentScheduleChange.getChangeTypeCode()))
                                    && (StudentScheduleChange.CODE_DROP
                                            .equals(studentScheduleChange.getChangeTypeCode()))) {
                                String sectionOid = studentScheduleChange.getMasterScheduleOid();
                                changeSameDayAddedDropped.add(sectionOid);
                            }
                        }
                        lastStudentScheduleChange = studentScheduleChange;
                    }
                }

                // Add current schedule sections to the map.
                if (schedules != null) {
                    for (StudentSchedule schedule : schedules) {
                        if (schedule.getScheduleOid() != null && activeScheduleOids.contains(schedule.getScheduleOid())
                                && !changeSameDayAddedDropped.contains(schedule.getSectionOid())) {
                            ScheduleInfo info = new ScheduleInfo();
                            info.m_section = schedule.getSection();

                            scheduleMap.put(schedule.getSectionOid(), info);
                        }
                    }
                }

                // Add reportable dropped schedule add/drop sections to the map.
                if (scheduleChanges != null) {
                    checkScheduleChanges(scheduleChanges, scheduleMap, gradDate, withdrawalDate);
                }

                // try to match schedule to transcript on master schedule oid.
                if (transcripts != null) {
                    for (Transcript trn : transcripts) {
                        ScheduleInfo info = scheduleMap.get(trn.getMasterScheduleOid());
                        if (info != null) {
                            info.m_transcript = trn;
                        }
                    }
                }
            }

            // Put these into a List for ordered processing.
            m_schedules = new ArrayList<ScheduleInfo>(20);
            for (ScheduleInfo info : scheduleMap.values()) {
                m_schedules.add(info);
            }
            /*
             * With m_schedules populated, we now know how many schedule records for this student.
             */
            setRowCount(m_schedules.size());
            setCurrentRow(0);
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * Search through schedule change records for other sections to report.
         *
         * Loop through schedule change records for each section, in date order.
         * If a section is found that is not already in the students schedule map,
         * check it's chain of change records against the report dates and its term dates.
         * If it was in session on a report date or at the end of its term, include it.
         * If it was dropped after a report date and not completed in the term, include
         * it as withdrawn.
         * Some will have been dropped after the end of a term but should still be counted.
         *
         * @param scheduleChanges Collection<StudentScheduleChange>
         * @param scheduleMap Map<String,ScheduleInfo>
         * @param gradDate PlainDate
         * @param withdrawalDate PlainDate
         */
        private void checkScheduleChanges(Collection<StudentScheduleChange> scheduleChanges,
                                          Map<String, ScheduleInfo> scheduleMap,
                                          PlainDate gradDate,
                                          PlainDate withdrawalDate) {
            PlainDate[] priorReportDates = ((Scs) getData()).m_priorReportDates;
            List<String> activeScheduleOids = ((Scs) getData()).m_activeScheduleOids;
            PlainDate reportDate = ((Scs) getData()).m_reportDate;
            PlainDate checkEndDate = null;
            String lastSectionOid = null;
            MasterSchedule lastSection = null;
            PlainDate beginDate = null;
            PlainDate termStart = null;
            PlainDate termEnd = null;

            // isSession indicates the section was in session on a report date or term end date.
            boolean inSession = false;
            // isWithdrawn indicates the section was withdrawn before the term end date.
            boolean isWithdrawn = false;

            for (StudentScheduleChange change : scheduleChanges) {
                if (change.getScheduleOid() != null && activeScheduleOids.contains(change.getScheduleOid())) {
                    // Check for a new section.
                    if (lastSectionOid == null || !lastSectionOid.equals(change.getMasterScheduleOid())) {
                        // Save the working section if necessary.
                        if (lastSectionOid != null &&
                                inSession &&
                                !scheduleMap.keySet().contains(lastSectionOid) &&
                                (termStart.before(reportDate) || termStart.equals(reportDate))) {
                            // If the last record was an add, check it against the end date.
                            if (!isWithdrawn &&
                                    (checkEndDate.after(beginDate) || checkEndDate.equals(beginDate))) {
                                inSession = true;
                                isWithdrawn = false;
                            }

                            // Create the section record if it is reportable (was in session).
                            if (inSession) {
                                ScheduleInfo info = new ScheduleInfo();
                                info.m_section = lastSection;
                                info.m_withdrawn = isWithdrawn;
                                scheduleMap.put(lastSectionOid, info);
                            }
                        }
                        lastSectionOid = change.getMasterScheduleOid();
                        lastSection = change.getMasterSchedule();
                        termStart = null;
                        termEnd = null;
                        inSession = false;
                        isWithdrawn = false;
                        for (ScheduleTermDate termDate : lastSection.getScheduleTerm().getScheduleTermDates()) {
                            if (termStart == null || termStart.after(termDate.getStartDate())) {
                                termStart = termDate.getStartDate();
                            }
                            if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                                termEnd = termDate.getEndDate();
                            }
                        }
                        // When checking if a section is in session, check if it is in session on
                        // report date if
                        // the report date is before the end of term. Avoid sections added after
                        // report date.
                        // Check for graduation date. Graduated seniors can be withdrawn before the
                        // end of term and
                        // still need to be reported as of their graduation date.
                        // Check for early withdrawal date, if the student withdrew before the first
                        // reporting period.
                        checkEndDate = termEnd;
                        if (reportDate.before(checkEndDate)) {
                            checkEndDate = reportDate;
                        }
                        if (gradDate != null && !checkEndDate.after(gradDate)) {
                            checkEndDate = gradDate;
                        }
                        if (withdrawalDate != null && withdrawalDate.before(checkEndDate)) {
                            checkEndDate = withdrawalDate;
                        }
                        if (StudentScheduleChange.CODE_DROP.equals(change.getChangeTypeCode())) {
                            beginDate = termStart;
                        } else if (StudentScheduleChange.CODE_ADD.equals(change.getChangeTypeCode())) {
                            beginDate = change.getEffectiveDate();
                        }
                    }

                    // For a section, see if its dates compare with report dates or term dates.
                    if (StudentScheduleChange.CODE_DROP.equals(change.getChangeTypeCode())) {
                        // Check prior report dates.
                        for (PlainDate priorDate : priorReportDates) {
                            if ((priorDate.after(beginDate) || priorDate.equals(beginDate)) &&
                                    (priorDate.before(change.getEffectiveDate())
                                            || priorDate.equals(change.getEffectiveDate()))) {
                                inSession = true;
                            }
                        }
                        // Check against term end date or report date.
                        if ((checkEndDate.after(beginDate) || checkEndDate.equals(beginDate)) &&
                                (checkEndDate.before(change.getEffectiveDate())
                                        || checkEndDate.equals(change.getEffectiveDate()))) {
                            inSession = true;
                        }

                        // See if it withdrew in the term/reporting term.
                        if (checkEndDate.after(change.getEffectiveDate())
                                || checkEndDate.equals(change.getEffectiveDate())) {
                            isWithdrawn = true;
                        }
                    } else if (StudentScheduleChange.CODE_ADD.equals(change.getChangeTypeCode())) {
                        if (change.getEffectiveDate().before(checkEndDate) ||
                                change.getEffectiveDate().equals(checkEndDate)) {
                            beginDate = change.getEffectiveDate();
                            isWithdrawn = false;
                        } else {
                            // change.getChangeDate().after(checkEndDate)
                            // Remove the section
                            // Section was added after the checkEndDate, assuming same as reportDate
                            scheduleMap.remove(change.getMasterScheduleOid());
                        }
                    }
                }
                if (lastSectionOid != null &&
                        inSession &&
                        !scheduleMap.keySet().contains(lastSectionOid) &&
                        (termStart.before(reportDate) || termStart.equals(reportDate))) {
                    // If the last record was an add, check it against the end date.
                    if (!isWithdrawn &&
                            (checkEndDate.after(beginDate) || checkEndDate.equals(beginDate))) {
                        inSession = true;
                        isWithdrawn = false;
                    }

                    // Create the section record if it is reportable (was in session).
                    if (inSession) {
                        ScheduleInfo info = new ScheduleInfo();
                        info.m_section = lastSection;
                        info.m_withdrawn = isWithdrawn;
                        scheduleMap.put(lastSectionOid, info);
                    }
                }
            }
        }

        /**
         * Returns the number of days the student has been a member from the start of school to the
         * report date.
         * This is the same method used by SIMS. Changes to one should be reflected in the other.
         *
         * @param entity StateReportEntity
         * @param snapshot EnrollmentSnapshot
         * @param field FieldDefinition
         * @return String
         * @throws X2BaseException exception
         */
        private String getMembershipDays(StateReportEntity entity, EnrollmentSnapshot snapshot, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String count = null;

            String adjustedCount = (String) WebUtils.getProperty(student, m_scsData.m_adjustedMembershipCount);
            if (!StringUtils.isEmpty(adjustedCount)) {
                count = adjustedCount;
            } else {
                // Check the active schedule for the school.
                SisSchool school = snapshot.getSchool();
                Schedule sched = null;
                if (school != null) {
                    sched = school.getActiveSchedule();
                    if (sched == null) {
                        entity.addRetrievalError(field.getFieldId(),
                                new StateReportValidationError(entity, field,
                                        "No active schedule: " + school.getName(), school.getName()));
                    } else {
                        try {
                            count = String.valueOf(
                                    m_scsData.m_enrollmentManager.getMembershipTotal(
                                            student,
                                            m_scsData.getCalendarDays(snapshot.getSchool(), student.getCalendarCode()),
                                            m_scsData.m_firstDayMembers.contains(student.getOid()),
                                            m_scsData.getCurrentContext().getStartDate(),
                                            m_scsData.m_reportDate,
                                            null));
                        } catch (Exception e) {
                            entity.addRetrievalError(field.getFieldId(),
                                    new StateReportValidationError(entity, field,
                                            "Could not calculate membership: exception", e.getMessage()));
                        }
                    }
                }
            }

            return count;
        }
    }

    /**
     * A container class for schedule information for one scheduled class.
     * This will contain one of a StudentSchedule or StudentScheduleChange,
     * and optionally one Transcript record.
     *
     * @author X2 Development Corporation
     */
    protected static class ScheduleInfo {
        public MasterSchedule m_section;
        public PlainDate m_addDate;
        public PlainDate m_dropDate;
        public Transcript m_transcript;
        public boolean m_withdrawn = false;
    }

    /*
     * Field id constants. These field id constants are for identifying fields in
     * the export field definition.
     */
    // private static final String SCS_01_LOCAL_ID = "LASID";
    // private static final String SCS_02_STATE_ID = "SASID";
    // private static final String SCS_03_SCHOOL_ID = "SchoolID";
    // private static final String SCS_04_COURSE_CODE = "CourseCode";
    // private static final String SCS_05_SUBJECT_AREA = "SubjectArea";
    // private static final String SCS_06_SECTION = "Section";
    // private static final String SCS_07_TERM_CODE = "TermCode";
    private static final String SCS_08_ENROLLMENT_STATUS = "EnrollmentStatus";
    // private static final String SCS_09_COURSE_LEVEL = "CourseLevel";
    // private static final String SCS_10_CREDIT_AVAILABLE = "CreditAvailable";
    // private static final String SCS_11_CREDIT_EARNED = "CreditsEarned";
    // private static final String SCS_12_LETTER_GRADE = "LetterGrade";
    private static final String SCS_13_NUMERIC_GRADE = "NumericGrade";
    private static final String SEPARATOR_COMMA = ",";


    /**
     * Enrollment status strings.
     * Enrollment status is calculated from schedule records and term date ranges.
     * It cannot be assigned through a reference code.
     */
    protected static final String SCS_ENROLLMENT_STATUS_ENROLLED = "01";
    protected static final String SCS_ENROLLMENT_STATUS_WITHDRAWN = "02";
    protected static final String SCS_ENROLLMENT_STATUS_COMPLETED = "03";
    protected static final String SCS_ENROLLMENT_STATUS_INCOMPLETE = "04";
    protected static final String SCS_ENROLLMENT_STATUS_EXCUSED = "05";

    /**
     * Grade scale strings.
     */
    protected static final String SCS_GRADE_LETTER_WITHDRAWN = "21";
    protected static final String SCS_GRADE_LETTER_WITHDRAWN_PASS = "22";
    protected static final String SCS_GRADE_LETTER_WITHDRAWN_FAIL = "23";
    protected static final String SCS_GRADE_LETTER_INCOMPLETE = "40";
    protected static final String SCS_GRADE_LETTER_EXCUSED = "50";
    protected static final String SCS_GRADE_LETTER_NOT_REQUIRED = "55";
    protected static final String SCS_GRADE_LETTER_UNGRADED = "66";
    protected static final String SCS_GRADE_LETTER_AUDIT = "77";
    protected static final String SCS_GRADE_LETTER_IN_PROGRESS = "88";
    protected static final String SCS_GRADE_LETTER_TYPE_NOT_REPORTED = "99";

    protected static final String SCS_GRADE_SCALE_WITHDRAWN = "21111";
    protected static final String SCS_GRADE_SCALE_WITHDRAWN_PASS = "22222";
    protected static final String SCS_GRADE_SCALE_WITHDRAWN_FAIL = "23333";
    protected static final String SCS_GRADE_SCALE_INCOMPLETE = "40000";
    protected static final String SCS_GRADE_SCALE_EXCUSED = "50000";
    protected static final String SCS_GRADE_SCALE_NOT_REQUIRED = "55555";
    protected static final String SCS_GRADE_SCALE_UNGRADED = "66666";
    protected static final String SCS_GRADE_SCALE_AUDIT = "77777";
    protected static final String SCS_GRADE_SCALE_IN_PROGRESS = "88888";
    protected static final String SCS_GRADE_SCALE_TYPE_NOT_REPORTED = "99999";

    /**
     * Retriever calc for transcript grade as a parameter for two types of graded:
     * letter grade.
     * number grade.
     */
    protected static final String SCS_GRADE_TYPE_LETTER = "letter";
    protected static final String SCS_GRADE_TYPE_NUMBER = "number";

    /*
     * Field alias for the adjusted district code and adjusted school code. This alias is optional.
     */
    private static final String ADJUSTED_DISTRICT_CODE_FIELD = "DOE ADJUSTED DISTRICT";
    private static final String ADJUSTED_SCHOOL_CODE_FIELD = "DOE Adjusted School";

    /*
     * Field aliases.
     */
    private static final String DOE_DISTRICT_ID = "DOE District ID";
    private static final String DOE_SCHOOL_ID = "DOE 15";
    private static final String DOE_GRADE_CODE = "DOE GRADE CODE";
    private static final String DOE_SUBJECT_AREA = "WA10-MTC";
    private static final String CRS_EXCLUDE_STATUS = "EPIMS CRS Status";
    private static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    private static final String ALIAS_SCHOOL_CRS = "DOE SCHOOL CRS";
    private static final String ADJUSTED_MEMBERSHIP_COUNT_FIELD = "DOE Adjusted Membership";
    private static final String ALIAS_SKL_SIF_DISTRICT_ID = "skl-sif-district-id";

    /*
     * Field alias/field value for querying options on the export
     */
    private static final String DOE_STATUS_FIELD_REPORT_CODE = "Report";
    private static final String DOE_STATUS_FIELD = "DOE Status";

    // ---------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------- X2-specific code below
    // ---------------------------------------------------------------------------------------------

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_PERIOD_PARAM = "reportingPeriod";

    /**
     * Name for the report date in October parameter. The corresponding values is a PlainDate
     * object.
     */
    public static final String REPORT_OCT_DATE_PARAM = "reportDateOct";

    /**
     * Name for the report date in March parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_MAR_DATE_PARAM = "reportDateMar";

    /**
     * Name for the report date in EOY parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_EOY_DATE_PARAM = "reportDateEoy";

    /**
     * Name for the "require report status" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the "use adjusted school code" parameter. The value is a Boolean.
     */
    public static final String USE_ADJUSTED_SCHOOL_CODE_PARAM = "useAdjustedSchool";

    /**
     * First withdrawl code for graduated seniors.
     */
    public static final String GRAD_W_CODE_1_PARAM = "withdrawl1";

    /**
     * Second withdrawl code for graduated seniors.
     */
    public static final String GRAD_W_CODE_2_PARAM = "withdrawl2";

    public static final String PARAM_INCLUDE_SIF_SCHOOL = "includeSifSchoolId";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected List<String> m_activeScheduleOids;
    protected String m_adjustedMembershipCount;
    protected String m_adjustedSchoolCode;
    protected String m_doeExcludeCourse;
    protected String m_doeSchoolOverrideCourse;
    protected String m_doeStatusField;
    protected String m_doeSubjectArea;
    protected EnrollmentManager m_enrollmentManager;
    protected Set m_firstDayMembers;
    protected Map<String, Collection<GradeScaleGradeDefinition>> m_gradeScales;
    protected PlainDate m_reportDate;
    protected PlainDate m_reportDateOct;
    protected PlainDate m_reportDateMar;
    protected PlainDate m_reportDateEoy;
    protected PlainDate m_priorReportDates[];
    protected Integer m_reportingPeriod;
    protected Map<String, Collection<StudentSchedule>> m_scheduleMap;
    protected Map<String, Collection<StudentScheduleChange>> m_scheduleChangeMap;
    protected String m_schoolIdField;
    protected HashMap m_schoolsToCalendars;
    protected Map<String, Collection<Transcript>> m_transcriptMap;
    protected DecimalFormat m_gradeFormat;
    protected Set<String> m_withdrawlCodes;
    protected Boolean m_useAdjustedSchoolCode;
    private String m_sklDstrIdField = null;
    private List<String> m_includeSifSchoolIds = null;

    /**
     * Returns credit value for the section course.
     *
     * This returns a BigDecimal object.
     * The field definition should use a numeric
     * formatter to format the value appropriately.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCredit implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            BigDecimal credit = null;
            ScheduleInfo info = ((SCSEntity) entity).getScheduleInfo();
            MasterSchedule section = info.m_section;
            credit = section.getSchoolCourse().getCredit();

            // Returning null will use the default value of "9999" "not applicable".
            // No credit means not applicable.
            if (credit != null && credit.doubleValue() == 0) {
                credit = null;
            }
            return credit;
        }
    }

    /**
     * Returns credit value from the student transcript.
     *
     * This returns a BigDecimal object.
     * The field definition should use a numeric
     * formatter to format the value appropriately.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCreditEarned implements FieldRetriever {
        private final BigDecimal DECIMAL_ZERO = new BigDecimal(0);

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            BigDecimal credit = null;
            BigDecimal availableCredit = null;
            ScheduleInfo info = ((SCSEntity) entity).getScheduleInfo();

            MasterSchedule section = info.m_section;
            availableCredit = section.getSchoolCourse().getCredit();

            // Get credits earned.
            String enrollmentStatus = entity.getFieldValue(SCS_08_ENROLLMENT_STATUS);
            if (!SCS_ENROLLMENT_STATUS_ENROLLED.equals(enrollmentStatus) &&
                    info.m_transcript != null) {
                credit = info.m_transcript.getTotalCredit();
            }
            if (credit == null) {
                credit = DECIMAL_ZERO;
            }

            // If available credit is null/zero (not applicable) and credit is zero/null,
            // then make it null to indicate not applicable.
            if ((availableCredit == null || availableCredit.doubleValue() == 0) &&
                    credit.doubleValue() == 0) {
                credit = null;
            }
            return credit;
        }
    }

    /**
     * Returns course number from the section in the schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCourseCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String courseCode = null;
            ScheduleInfo info = ((SCSEntity) entity).getScheduleInfo();
            MasterSchedule section = info.m_section;
            courseCode = section.getSchoolCourse().getNumber();
            return courseCode;
        }
    }

    /**
     * Returns course level information from the section in the schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCourseLevel implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String levelCode = null;
            ScheduleInfo info = ((SCSEntity) entity).getScheduleInfo();
            MasterSchedule section = info.m_section;
            if (section != null) {
                String academicLevel = section.getSchoolCourse().getAcademicLevel();
                if (StringUtils.isEmpty(academicLevel)) {
                    academicLevel = section.getSchoolCourse().getCourse().getAcademicLevel();
                }
                levelCode = data.lookupStateValue(SchoolCourse.class, SchoolCourse.COL_ACADEMIC_LEVEL, academicLevel);
            }

            return levelCode;
        }
    }

    /**
     * Returns the enrollment status from the schedule or schedule change record
     * and the term dates.
     */
    protected class RetrieveEnrollmentStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ScheduleInfo info = ((SCSEntity) entity).getScheduleInfo();
            Scs scsData = (Scs) data;
            PlainDate reportDate = ((Scs) data).m_reportDate;
            String status = null;

            if (info.m_withdrawn) {
                status = SCS_ENROLLMENT_STATUS_WITHDRAWN;
            } else {
                /*
                 * Identify if the scheduled class is in term or out of term
                 * based on term dates and report date.
                 * Use min/max dates to identify split terms (2,4 when reporting during 3)
                 */
                boolean inTerm = false;
                MasterSchedule section = info.m_section;
                section.getScheduleTerm();
                PlainDate minDate = null;
                PlainDate maxDate = null;
                for (ScheduleTermDate dates : section.getScheduleTerm().getScheduleTermDates()) {
                    if (maxDate == null || maxDate.before(dates.getEndDate())) {
                        maxDate = dates.getEndDate();
                    }
                    if (minDate == null || minDate.after(dates.getStartDate())) {
                        minDate = dates.getStartDate();
                    }
                    if (reportDate.before(dates.getEndDate()) && reportDate.after(dates.getStartDate())) {
                        inTerm = true;
                        break;
                    }
                }

                /*
                 * Determine status from term.
                 * In term, between split terms or after end of term.
                 * (Before start of term should not be reported at all, would be a bug.)
                 */
                if (inTerm) {
                    // In term course. enrolled for Oct, Mar. Complete for EOY.
                    if (scsData.m_reportingPeriod.intValue() == 2) {
                        status = SCS_ENROLLMENT_STATUS_COMPLETED;
                    } else {
                        status = SCS_ENROLLMENT_STATUS_ENROLLED;
                    }
                } else if (minDate != null && (minDate.before(reportDate) || minDate.equals(reportDate)) &&
                        maxDate != null && (maxDate.after(reportDate) || maxDate.equals(reportDate))) {
                    // Non consecutive multi-term course, between active terms. enrolled for Oct,
                    // Mar. Complete for EOY.
                    if (scsData.m_reportingPeriod.intValue() == 2) {
                        status = SCS_ENROLLMENT_STATUS_COMPLETED;
                    } else {
                        status = SCS_ENROLLMENT_STATUS_ENROLLED;
                    }
                } else if (maxDate != null && (maxDate.before(reportDate) || maxDate.equals(reportDate))) {
                    // After end of all terms, complete.
                    status = SCS_ENROLLMENT_STATUS_COMPLETED;
                }
            }

            /*
             * Examine final grade in transcript. If it is coded as incomplete or excused then
             * change status.
             * A "40" as a final grade score code indicates an incomplete.
             * A "50" as a final grade score code indicates an excused.
             */
            Scs scs = (Scs) data;
            Transcript transcript = info.m_transcript;

            if (transcript != null) {
                String finalGrade = transcript.getFinalGrade();

                // See if the grade in the final grade column is a grade scale value (letter grade).
                if (!StringUtils.isEmpty(finalGrade)) {
                    Collection<GradeScaleGradeDefinition> scales =
                            scs.m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                    if (scales != null) {
                        for (GradeScaleGradeDefinition gsg : scales) {
                            if (gsg.getGradeCode().equals(finalGrade)) {
                                String letterGrade = (String) gsg.getFieldValueByAlias(DOE_GRADE_CODE);
                                if (SCS_GRADE_LETTER_INCOMPLETE.equals(letterGrade)) {
                                    status = SCS_ENROLLMENT_STATUS_INCOMPLETE;
                                } else if (SCS_GRADE_LETTER_EXCUSED.equals(letterGrade)) {
                                    status = SCS_ENROLLMENT_STATUS_EXCUSED;
                                }
                            }
                        }
                    }
                }
            }

            return status;
        }
    }

    /**
     * Returns the school code for the given student. This method considers the adjusted school code
     * value.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchool implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            // Check the student adjusted school code.
            SisStudent student = (SisStudent) entity.getBean();
            String schoolId = null;
            if (m_useAdjustedSchoolCode != null && m_useAdjustedSchoolCode.booleanValue()) {
                schoolId = (String) WebUtils.getProperty(student, m_adjustedSchoolCode);
            }

            if (StringUtils.isEmpty(schoolId)) {
                // Get the section and school.
                // 1. Check the course for a school override.
                // 2. Get the school from the section.
                ScheduleInfo info = ((SCSEntity) entity).getScheduleInfo();
                MasterSchedule section = info.m_section;
                Course course = section.getSchoolCourse().getCourse();
                if (!StringUtils.isEmpty(m_doeSchoolOverrideCourse)) {
                    schoolId = (String) getProperty(course, m_doeSchoolOverrideCourse);
                }
                if (StringUtils.isEmpty(schoolId)) {
                    SisSchool school = section.getSchoolCourse().getSchool();
                    schoolId = (String) school.getFieldValueByBeanPath(m_schoolIdField);
                }
            }

            return schoolId;
        }
    }

    /**
     * Returns the course and section number information from the section in the schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSectionNumber implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String courseSection = null;
            ScheduleInfo info = ((SCSEntity) entity).getScheduleInfo();
            MasterSchedule section = info.m_section;
            courseSection = section.getCourseView();
            return courseSection;
        }
    }

    /**
     * Returns course level information from the section in the schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSubjectArea implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            // The subject area field alias is the field parameter.
            String alias = (String) field.getParameter();

            String courseCode = null;
            ScheduleInfo info = ((SCSEntity) entity).getScheduleInfo();
            MasterSchedule section = info.m_section;
            courseCode = (String) section.getSchoolCourse().getCourse().getFieldValueByAlias(alias);
            if (courseCode != null) {
                courseCode = courseCode.trim();
            }

            return courseCode;
        }
    }

    /**
     * Returns transcript grade from the transcript record associated with this section or class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTranscriptGrade implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Scs scs = (Scs) data;
            ScheduleInfo info = ((SCSEntity) entity).getScheduleInfo();
            String parameter = (String) field.getParameter();
            DecimalFormat decimalFormat = ((Scs) data).m_gradeFormat;
            String numericGrade = null;
            String letterGrade = null;

            String enrollmentStatus = entity.getFieldValue(SCS_08_ENROLLMENT_STATUS);
            if (SCS_ENROLLMENT_STATUS_ENROLLED.equals(enrollmentStatus)) {
                letterGrade = SCS_GRADE_LETTER_IN_PROGRESS;
                numericGrade = SCS_GRADE_SCALE_IN_PROGRESS;
            } else if (SCS_ENROLLMENT_STATUS_WITHDRAWN.equals(enrollmentStatus)) {
                // Generate an unattributed withdrawn value.
                letterGrade = SCS_GRADE_LETTER_WITHDRAWN;
                numericGrade = SCS_GRADE_SCALE_WITHDRAWN;

                // Check the transcript final grade.
                // If it translates to WITHDRAWN_PASS or WITHDRAWAN_FAIL, use those instead.
                Transcript transcript = info.m_transcript;
                if (transcript != null) {
                    String finalGrade = transcript.getFinalGrade();

                    // See if the grade in the final grade column is a grade scale value (letter
                    // grade).
                    if (!StringUtils.isEmpty(finalGrade)) {
                        Collection<GradeScaleGradeDefinition> scales =
                                scs.m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                        if (scales != null) {
                            for (GradeScaleGradeDefinition gsg : scales) {
                                if (gsg.getGradeCode().equals(finalGrade)) {
                                    letterGrade = (String) gsg.getFieldValueByAlias(DOE_GRADE_CODE);
                                    if (SCS_GRADE_LETTER_WITHDRAWN_PASS.equals(letterGrade)) {
                                        letterGrade = SCS_GRADE_LETTER_WITHDRAWN_PASS;
                                        numericGrade = SCS_GRADE_SCALE_WITHDRAWN_PASS;
                                    } else if (SCS_GRADE_LETTER_WITHDRAWN_FAIL.equals(letterGrade)) {
                                        letterGrade = SCS_GRADE_LETTER_WITHDRAWN_FAIL;
                                        numericGrade = SCS_GRADE_SCALE_WITHDRAWN_FAIL;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
            } else if (SCS_ENROLLMENT_STATUS_INCOMPLETE.equals(enrollmentStatus)) {
                letterGrade = SCS_GRADE_LETTER_INCOMPLETE;
                numericGrade = SCS_GRADE_SCALE_INCOMPLETE;
            } else if (SCS_ENROLLMENT_STATUS_EXCUSED.equals(enrollmentStatus)) {
                letterGrade = SCS_GRADE_LETTER_EXCUSED;
                numericGrade = SCS_GRADE_SCALE_EXCUSED;
            } else if (SCS_ENROLLMENT_STATUS_COMPLETED.equals(enrollmentStatus)) {
                /*
                 * Determine if the course is complete, or is not graded.
                 */
                MasterSchedule section = info.m_section;

                /*
                 * Check ungraded course.
                 */
                SchoolCourse schoolCourse = section.getSchoolCourse();
                if (schoolCourse.getTermGradesTermMap() != null && !schoolCourse.getTermGradesTermMap().contains("1")) {
                    letterGrade = SCS_GRADE_LETTER_UNGRADED;
                    numericGrade = SCS_GRADE_SCALE_UNGRADED;
                } else {
                    Transcript transcript = info.m_transcript;
                    if (transcript != null) {
                        String finalGrade = transcript.getFinalGrade();

                        // See if the grade in the final grade column is a grade scale value (letter
                        // grade).
                        if (!StringUtils.isEmpty(finalGrade)) {
                            Collection<GradeScaleGradeDefinition> scales =
                                    scs.m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                            if (scales != null) {
                                for (GradeScaleGradeDefinition gsg : scales) {
                                    if (gsg.getGradeCode().equals(finalGrade)) {
                                        letterGrade = (String) gsg.getFieldValueByAlias(DOE_GRADE_CODE);
                                        if (gsg.getNoNumericIndicator()) {
                                            numericGrade = SCS_GRADE_SCALE_TYPE_NOT_REPORTED;
                                        } else {
                                            // Limit numeric value to 100 if it is greater.
                                            BigDecimal gradeValue = gsg.getGradeValue();
                                            if (gradeValue.floatValue() > 100.0) {
                                                gradeValue = BigDecimal.valueOf(100.0);
                                            }
                                            numericGrade = decimalFormat.format(gradeValue);
                                        }
                                        break;
                                    }
                                }
                            }

                            if (letterGrade == null && numericGrade == null) {
                                try {
                                    BigDecimal bdNumericGrade = new BigDecimal(finalGrade);
                                    if (SCS_GRADE_TYPE_LETTER.equals(parameter) && scales != null) {
                                        BigDecimal lastCutoff = new BigDecimal(-1);
                                        // look up the letter grade from the numeric grade.
                                        for (GradeScaleGradeDefinition gsg : scales) {
                                            if (!gsg.getNoNumericIndicator() &&
                                                    gsg.getGradeCutoffValue().compareTo(bdNumericGrade) <= 0 &&
                                                    gsg.getGradeCutoffValue().compareTo(lastCutoff) > 0) {
                                                letterGrade = (String) gsg.getFieldValueByAlias(DOE_GRADE_CODE);
                                                lastCutoff = gsg.getGradeCutoffValue();
                                            }
                                        }
                                    }
                                    // Limit numeric value to 100 if it is greater.
                                    if (bdNumericGrade.floatValue() > 100.0) {
                                        bdNumericGrade = BigDecimal.valueOf(100.0);
                                    }

                                    numericGrade = decimalFormat.format(bdNumericGrade);
                                } catch (NumberFormatException nfe) {
                                    // grade is not numeric.
                                }
                            }
                        }
                    }
                }
            }

            // check for special letter codes (audit, not graded)
            if (SCS_GRADE_LETTER_AUDIT.equals(letterGrade)) {
                numericGrade = SCS_GRADE_SCALE_AUDIT;
            }
            if (SCS_GRADE_LETTER_NOT_REQUIRED.equals(letterGrade)) {
                numericGrade = SCS_GRADE_SCALE_NOT_REQUIRED;
            }

            // Check return type for the calculation.
            Object returnObject = null;
            if (SCS_GRADE_TYPE_LETTER.equals(parameter)) {
                if (letterGrade == null) {
                    letterGrade = SCS_GRADE_LETTER_TYPE_NOT_REPORTED;
                }
                returnObject = letterGrade;
            } else {
                if (numericGrade == null) {
                    numericGrade = SCS_GRADE_SCALE_TYPE_NOT_REPORTED;
                }
                returnObject = numericGrade;
            }

            return returnObject;
        }
    }

    /**
     * Returns course level information from the section in the schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTermCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String termCode = null;
            ScheduleInfo info = ((SCSEntity) entity).getScheduleInfo();
            MasterSchedule section = info.m_section;
            termCode = section.getScheduleTerm().getCode();
            termCode = data.lookupStateValue(ScheduleTerm.class, ScheduleTerm.COL_CODE, termCode);
            return termCode;
        }
    }

    /**
     * Validate transcript grade values.
     * Compare to enrollment status.
     * - Enrolled should report only 88888.
     * - Incomplete or other should report only 55555.
     * - Complete should report only 99999 or a grade (not 88888 or 55555).
     *
     * @author X2 Development Corporation
     */
    protected class ValidateCreditEarned implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String SCS08 = entity.getFieldValue(SCS_08_ENROLLMENT_STATUS);
            double credit = 0;
            try {
                credit = Double.parseDouble(value);
            } catch (NumberFormatException nfe) {
                // Invalid value. Ignore, will report elsewhere.
            } catch (NullPointerException npe) {
                // Invalid value. Ignore, will report elsewhere.
            }

            if ((SCS_ENROLLMENT_STATUS_ENROLLED.equals(SCS08) ||
                    SCS_ENROLLMENT_STATUS_WITHDRAWN.equals(SCS08)) &&
                    credit != 0 && credit != 9999) {
                errors.add(new StateReportValidationError(entity, field,
                        "Enrollment code SCS08 requires SCS11 Credit Earned to be " + STYLE_BOLD + "0" + STYLE_END,
                        "SCS08=" + STYLE_BOLD + SCS08 + STYLE_END + ", SCS11=" + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate transcript grade values.
     * Compare to enrollment status.
     * - Enrolled should report only 88888.
     * - Incomplete or other should report only 55555.
     * - Complete should report only 99999 or a grade (not 88888 or 55555).
     *
     * @author X2 Development Corporation
     */
    protected class ValidateTranscriptGrade implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String parameter = (String) field.getParameter();
            String SCS08 = entity.getFieldValue(SCS_08_ENROLLMENT_STATUS);

            if (SCS_ENROLLMENT_STATUS_ENROLLED.equals(SCS08)) {
                if (SCS_GRADE_TYPE_LETTER.equals(parameter) && !SCS_GRADE_LETTER_IN_PROGRESS.equals(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Enrollment code SCS08 Enrolled requires grade letter " + STYLE_BOLD + "88" + STYLE_END,
                            "SCS12=" + STYLE_BOLD + value + STYLE_END));
                } else if (SCS_GRADE_TYPE_NUMBER.equals(parameter) && !SCS_GRADE_SCALE_IN_PROGRESS.equals(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Enrollment code SCS08 Enrolled requires grade value " + STYLE_BOLD + "88888" + STYLE_END,
                            "SCS13=" + STYLE_BOLD + value + STYLE_END));
                }
            } else if (SCS_ENROLLMENT_STATUS_COMPLETED.equals(SCS08)) {
                if (SCS_GRADE_TYPE_LETTER.equals(parameter) &&
                        (SCS_GRADE_LETTER_IN_PROGRESS.equals(value))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Enrollment code SCS08 Complete may not have grade letter " + STYLE_BOLD + "88" + STYLE_END,
                            "SCS12=" + STYLE_BOLD + value + STYLE_END));
                } else if (SCS_GRADE_TYPE_NUMBER.equals(parameter) &&
                        (SCS_GRADE_SCALE_IN_PROGRESS.equals(value))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Enrollment code SCS08 Complete may not have grade value " + STYLE_BOLD + "88888"
                                    + STYLE_BOLD,
                            "SCS13=" + STYLE_BOLD + value + STYLE_END));
                }
            }

            if (SCS_GRADE_TYPE_LETTER.equals(parameter)) {
                String SCS13 = entity.getFieldValue(SCS_13_NUMERIC_GRADE);
                if (SCS_GRADE_LETTER_TYPE_NOT_REPORTED.equals(value)
                        && SCS_GRADE_SCALE_TYPE_NOT_REPORTED.equals(SCS13)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Grade letter SCS12 and Grade score SCS13 cannot both be " + STYLE_BOLD + "99999"
                                    + STYLE_END,
                            "SCS12=" + STYLE_BOLD + value + STYLE_END + ", SCS13=" + STYLE_BOLD + SCS13 + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Get the heading from the export format, and add the district code to it.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        StringBuilder sb = new StringBuilder(super.getHeading().replace("\n", "").replace("\r", ""));

        String code = (String) getOrganization().getFieldValueByAlias(DOE_DISTRICT_ID);
        if (StringUtils.isEmpty(code)) {
            code = "[INSERT DISTRICT ID HERE]";
            addSetupError("Using a placeholder for the district ID.",
                    "Set the " + STYLE_BOLD + DOE_DISTRICT_ID + STYLE_END +
                            " alias in the Data Dictionary and update that field with the correct ID.");
        } else {
            sb.append(code);
        }

        sb.append(ExportJavaSource.FORMAT_EOL_WINDOWS);

        return sb.toString();
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     * 
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        // Load initialization data
        initializeFields();

        /*
         * Get core parameters
         */
        m_reportingPeriod = (Integer) getParameter(REPORT_PERIOD_PARAM);
        m_useAdjustedSchoolCode = (Boolean) getParameter(USE_ADJUSTED_SCHOOL_CODE_PARAM);
        m_gradeFormat = new DecimalFormat("000.0");
        m_reportDateOct = (PlainDate) getParameter(REPORT_OCT_DATE_PARAM);
        m_reportDateMar = (PlainDate) getParameter(REPORT_MAR_DATE_PARAM);
        m_reportDateEoy = (PlainDate) getParameter(REPORT_EOY_DATE_PARAM);

        /*
         * Calculate the reporting date from the reporting period parameter if not provided.
         */
        // October 1
        if (m_reportDateOct == null) {
            Calendar rptDateCal = Calendar.getInstance();
            rptDateCal.setTimeInMillis(getCurrentContext().getStartDate().getTime());
            rptDateCal.set(Calendar.MONTH, 9);
            rptDateCal.set(Calendar.DAY_OF_MONTH, 1);
            m_reportDateOct = new PlainDate(rptDateCal.getTimeInMillis());
        }

        // March 1
        if (m_reportDateMar == null) {
            Calendar rptDateCal = Calendar.getInstance();
            rptDateCal.setTimeInMillis(getCurrentContext().getEndDate().getTime());
            rptDateCal.set(Calendar.MONTH, 2);
            rptDateCal.set(Calendar.DAY_OF_MONTH, 1);
            m_reportDateMar = new PlainDate(rptDateCal.getTimeInMillis());
        }

        // EOY (last day of calendar year)
        if (m_reportDateEoy == null) {
            m_reportDateEoy = new PlainDate(getCurrentContext().getEndDate().getTime());
        }

        switch (m_reportingPeriod.intValue()) {
            case 0: // Oct
                m_priorReportDates = new PlainDate[0];
                m_reportDate = m_reportDateOct;
                break;

            case 1: // Mar
                m_priorReportDates = new PlainDate[1];
                m_priorReportDates[0] = m_reportDateOct;
                m_reportDate = m_reportDateMar;
                break;

            default: // EOY
                m_priorReportDates = new PlainDate[2];
                m_priorReportDates[0] = m_reportDateOct;
                m_priorReportDates[1] = m_reportDateMar;
                m_reportDate = m_reportDateEoy;

                // Get graduated senior withdrawl codes.
                m_withdrawlCodes = new HashSet<String>();
                String wc = (String) getParameter(GRAD_W_CODE_1_PARAM);
                if (!StringUtils.isEmpty(wc)) {
                    m_withdrawlCodes.add(wc.toUpperCase());
                }
                wc = (String) getParameter(GRAD_W_CODE_2_PARAM);
                if (!StringUtils.isEmpty(wc)) {
                    m_withdrawlCodes.add(wc.toUpperCase());
                }
                break;
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER +
                            translateAliasToJavaName("DOE15", true));
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(SCSEntity.class);

            // Load a map of student transcript records my student.
            loadStudentSchedules(studentCriteria);
            loadStudentScheduleChanges(studentCriteria);
            loadTranscripts(studentCriteria);

            // Build maps of retriever functions and validator functions
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("SCS-SchoolId", new RetrieveSchool());
            calcs.put("SCS-CourseCode", new RetrieveCourseCode());
            calcs.put("SCS-SubjectArea", new RetrieveSubjectArea());
            calcs.put("SCS-SectionNumber", new RetrieveSectionNumber());
            calcs.put("SCS-TermCode", new RetrieveTermCode());
            calcs.put("SCS-EnrollStatus", new RetrieveEnrollmentStatus());
            calcs.put("SCS-CourseLevel", new RetrieveCourseLevel());
            calcs.put("SCS-Credit", new RetrieveCredit());
            calcs.put("SCS-CreditEarned", new RetrieveCreditEarned());
            calcs.put("SCS-Grade", new RetrieveTranscriptGrade());

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put("SCS-CreditEarned", new ValidateCreditEarned());
            validators.put("SCS-Grade", new ValidateTranscriptGrade());

            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Returns a set of calendar in session days for a school and calendar.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of in session dates.
     */
    protected Set getCalendarDays(SisSchool school, String calendar) {
        if (!m_schoolsToCalendars.containsKey(school.getOid())) {
            PlainDate startDate = school.getActiveSchedule().getStartDate();
            Map calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, m_reportDate);
            m_schoolsToCalendars.put(school.getOid(), calendarData);
        }

        return (Set) ((Map) m_schoolsToCalendars.get(school.getOid())).get(calendar);
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        /*
         * Who should be included? Primary students and, optionally, summer withdrawals.
         *
         * The students that belong to each group depend upon how this export is being run:
         *
         * Case 1: The export is being run for either (A) the entire district or (B) a single school
         * without "nested districts" (99% of all cases)
         *
         * Case 2: The export is being run for a single school with "nested districts"
         *
         * ----------------------------------------------------------------------------------------
         *
         * Q: What are "nested districts" anyway?
         *
         * A: A single X2 district could really represent multiple districts as far as the DOE is
         * concerned. For example, Nauset is a single X2 district but only the middle and high
         * schools are in the Nauset Regional School Organization. All the elementary schools belong
         * to their own districts according to the DOE. These "nested districts" can be
         * represented in X2 by setting different Adjusted Organization Code values on the school
         * records. If "nested districts" are used then ALL schools should have an Adjusted
         * Organization Code (even the archive school).
         * 
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case 1:
         *
         * Students in an active, non-archived school in the district
         * 
         * ----------------------------------------------------------------------------------------
         * 
         * Primary students, case 2:
         *
         * Students in the select school as well as students who have withdrawn from the selected
         * school between the start of the school year and the report date (both dates inclusive)
         * and are now in a school with a different adjusted district code
         * 
         * ----------------------------------------------------------------------------------------
         *
         * Summer withdrawals, case 1:
         *
         * Students who withdrew during the summer (start/end dates inclusive) and are now in the
         * archive school
         * 
         * ----------------------------------------------------------------------------------------
         * 
         * Summer withdrawals, case 2:
         *
         * Students who withdrew from the selected school during the summer (start/end dates
         * inclusive) and are now either in the archive school or a school with a different
         * adjusted district code
         * 
         * ----------------------------------------------------------------------------------------
         *
         * TODO:
         *
         * There are two shortcomings with "nested districts" for students who transfer from one X2
         * school to another:
         *
         * 1. The school code will not be correct for summer transfers (since this procedure
         * uses an enrollment snapshot based on the reporting date)
         * 
         * 2. The enrollment status will not be correct for any students (since the student stays
         * active during a transfer)
         * 
         * Item 1 could be fixed by changing how this procedure works. Item 2 would require
         * additional information to be stored in an enrollment record. Right now users will have to
         * manually adjust these two fields after the export has finished.
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ADJUSTED_DISTRICT_CODE_FIELD);

        String adjustedDistrictCode = null;
        if (isSchoolContext() && field != null) {
            adjustedDistrictCode = (String) getSchool().getFieldValueByBeanPath(field.getJavaName());
        }

        boolean useNestedDistricts = !StringUtils.isEmpty(adjustedDistrictCode);

        /*
         * Primary students
         */
        Criteria primaryCriteria = new Criteria();
        if (isSchoolContext()) {
            primaryCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());

            if (useNestedDistricts) {
                PlainDate startDate = ((SisSchool) getSchool()).getActiveSchedule().getStartDate();

                Criteria withdrawalsCriteria = new Criteria();
                withdrawalsCriteria.addIn(X2BaseBean.COL_OID, getStudentWithdrawalQuery(startDate, m_reportDate));
                withdrawalsCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + field.getJavaName(),
                        adjustedDistrictCode);

                primaryCriteria.addOrCriteria(withdrawalsCriteria);
            }
        } else if (m_includeSifSchoolIds != null) {
            primaryCriteria.addIn(ConductIncident.REL_SCHOOL + ModelProperty.PATH_DELIMITER
                    + m_sklDstrIdField, m_includeSifSchoolIds);
        } else {
            primaryCriteria.addEqualTo(SisStudent.COL_ORGANIZATION1_OID, getOrganization().getOid());
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        X2Criteria reportingCriteria = new X2Criteria();
        reportingCriteria.addAndCriteria(primaryCriteria);

        return reportingCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        Boolean requireReportStatus = (Boolean) getParameter(REQUIRE_REPORT_STATUS_PARAM);
        if (requireReportStatus.booleanValue()) {
            userCriteria.addEqualTo(m_doeStatusField, DOE_STATUS_FIELD_REPORT_CODE);
        }

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        /*
         * Then combine the user criteria with the general reporting criteria
         */
        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(userCriteria);
        studentCriteria.addAndCriteria(getReportingCriteria());

        return studentCriteria;
    }

    /**
     * Returns a query that finds the students who withdrew during the given date range (filtered
     * by school as appropriate) and never re-entered the district again.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return A SubQuery that returns the student OID from StudentEnrollment records
     */
    private SubQuery getStudentWithdrawalQuery(PlainDate startDate, PlainDate endDate) {
        /*
         * Retrieve all the Entry records of the Summer withdrawals after the Summer end date.
         */
        Criteria entryCriteria = new Criteria();
        entryCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        entryCriteria.addGreaterThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);

        if (isSchoolContext()) {
            entryCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        /*
         * Retrieve all the Summer withdrawal records that never re-entered the district again.
         */
        Criteria withdrawalCriteria = new Criteria();
        withdrawalCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        withdrawalCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        withdrawalCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);
        withdrawalCriteria.addNotIn(StudentEnrollment.COL_STUDENT_OID,
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, entryCriteria));

        if (isSchoolContext()) {
            withdrawalCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        return new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, withdrawalCriteria);
    }

    /**
     * Set up maps and objects for report usage.
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_sklDstrIdField = translateAliasToJavaName(ALIAS_SKL_SIF_DISTRICT_ID, true);
        // Load Input Definition Parameters
        String includeIds = (String) getParameter(PARAM_INCLUDE_SIF_SCHOOL);
        if (!StringUtils.isEmpty(includeIds)) {
            List<String> rcdOids = new ArrayList<String>(Arrays.asList(includeIds.split(SEPARATOR_COMMA)));
            X2Criteria sifDistrIdCriteria = new X2Criteria();
            sifDistrIdCriteria.addIn(X2BaseBean.COL_OID, rcdOids);
            QueryByCriteria byCriteria = new QueryByCriteria(ReferenceCode.class, sifDistrIdCriteria);
            Collection<ReferenceCode> refCodes = getBroker().getCollectionByQuery(byCriteria);
            m_includeSifSchoolIds = new ArrayList();
            for (ReferenceCode code : refCodes) {
                m_includeSifSchoolIds.add(code.getCode());
            }
        }

        m_schoolsToCalendars = new HashMap();
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_firstDayMembers = m_enrollmentManager.getMembershipAsOf(getCurrentContext().getStartDate(),
                getOrganization(), true);

        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_adjustedSchoolCode = translateAliasToJavaName(ADJUSTED_SCHOOL_CODE_FIELD, true);
        m_doeStatusField = translateAliasToJavaName(DOE_STATUS_FIELD, true);
        m_doeSubjectArea = translateAliasToJavaName(DOE_SUBJECT_AREA, true);
        m_doeExcludeCourse = translateAliasToJavaName(CRS_EXCLUDE_STATUS, false);
        if (StringUtils.isEmpty(m_doeExcludeCourse)) {
            m_doeExcludeCourse = translateAliasToJavaName(ALIAS_EXCLUDE_CRS, false);
        }
        m_adjustedMembershipCount = translateAliasToJavaName(ADJUSTED_MEMBERSHIP_COUNT_FIELD, true);
        m_schoolIdField = translateAliasToJavaName(DOE_SCHOOL_ID, true);
        m_doeSchoolOverrideCourse = translateAliasToJavaName(ALIAS_SCHOOL_CRS, false);
    }

    /**
     * Load a map of sets of student schedule records by student oid for the students in the export.
     *
     * @param studentCriteria Criteria
     */
    private void loadStudentSchedules(Criteria studentCriteria) {
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        X2Criteria scheduleCriteria = new X2Criteria();
        scheduleCriteria.addIn(StudentSchedule.COL_STUDENT_OID, subQuery);
        scheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, "Class");

        // From active Schedule
        scheduleCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                Schedule.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + ModelProperty.PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);
        // section term started before report date.
        scheduleCriteria.addLessOrEqualThan(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE_TERM_DATES + ModelProperty.PATH_DELIMITER +
                ScheduleTermDate.COL_START_DATE, m_reportDate);

        // section course must have a subject area.
        scheduleCriteria.addNotEmpty(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                m_doeSubjectArea, getBroker().getPersistenceKey());

        // Course exclude from EPIMS/SCS not set.
        if (m_doeExcludeCourse != null) {
            scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                    m_doeExcludeCourse, BooleanAsStringConverter.TRUE);
        }

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, scheduleCriteria);
        m_scheduleMap = getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 500);
    }

    /**
     * Load a map of sets of student schedule change records by student oid for the students in the
     * export.
     * This method only loads Drops and does extra filtering to eliminate schedule changes
     * that happen before the scheduled term begins.
     *
     * @param studentCriteria Criteria
     */
    private void loadStudentScheduleChanges(Criteria studentCriteria) {
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        X2Criteria scheduleCriteria = new X2Criteria();
        scheduleCriteria.addIn(StudentScheduleChange.COL_STUDENT_OID, subQuery);

        // From Class type section
        scheduleCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, "Class");

        // From active Schedule
        scheduleCriteria.addEqualToField(StudentScheduleChange.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                Schedule.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + ModelProperty.PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentScheduleChange.COL_SCHEDULE_OID);

        // section term started before report date.
        scheduleCriteria.addLessOrEqualThan(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE_TERM_DATES + ModelProperty.PATH_DELIMITER +
                ScheduleTermDate.COL_START_DATE, m_reportDate);

        // section course must have a subject area.
        scheduleCriteria.addNotEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                m_doeSubjectArea, getBroker().getPersistenceKey());

        // Course exclude from EPIMS/SCS not set.
        if (m_doeExcludeCourse != null) {
            scheduleCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                    m_doeExcludeCourse, BooleanAsStringConverter.TRUE);
        }

        scheduleCriteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);

        QueryByCriteria query = new QueryByCriteria(StudentScheduleChange.class, scheduleCriteria);
        query.addOrderBy(StudentScheduleChange.COL_STUDENT_OID, true);
        query.addOrderBy(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, true);
        query.addOrderBy(StudentScheduleChange.COL_EFFECTIVE_DATE, true);
        query.addOrderBy(StudentScheduleChange.COL_TIMESTAMP, true);
        m_scheduleChangeMap =
                getBroker().getGroupedCollectionByQuery(query, StudentScheduleChange.COL_STUDENT_OID, 100);
        /*
         * m_scheduleChangeMap = new HashMap<String, Collection<StudentScheduleChange>>();
         * QueryIterator iterator = getBroker().getIteratorByQuery(query);
         * try
         * {
         * Collection<StudentScheduleChange> sccs = null;
         * String lastStudentOid = null;
         * while (iterator.hasNext())
         * {
         * try
         * {
         * StudentScheduleChange scc = (StudentScheduleChange) iterator.next();
         * // Date changeDate = new Date(scc.getTimestamp());
         * // PlainDate qDate =
         * scc.getMasterSchedule().getScheduleTerm().getScheduleTermDates().iterator().next().
         * getStartDate();
         * // if (changeDate.after(qDate))
         * {
         * if (lastStudentOid == null || !lastStudentOid.equals(scc.getStudentOid()))
         * {
         * sccs = new HashSet<StudentScheduleChange>();
         * m_scheduleChangeMap.put(scc.getStudentOid(), sccs);
         * lastStudentOid = scc.getStudentOid();
         * }
         * sccs.add(scc);
         * }
         * }
         * catch (Exception e)
         * {
         * // In case some student/schedule change has missing elements.
         * // End up ignoring this change.
         * }
         * }
         * }
         * finally
         * {
         * iterator.close();
         * }
         */
        // Load a list of all active schedule Oids
        scheduleCriteria = new X2Criteria();
        scheduleCriteria.addEqualToField(Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                X2BaseBean.COL_OID);
        query = new QueryByCriteria(Schedule.class, scheduleCriteria);
        Collection<Schedule> schedules = getBroker().getCollectionByQuery(query);
        m_activeScheduleOids = new ArrayList<String>(schedules.size());
        for (Schedule schedule : schedules) {
            m_activeScheduleOids.add(schedule.getOid());
        }
    }

    /**
     * Loads a map of sets of transcript records by studentOid for students in the export.
     *
     * @param studentCriteria Criteria
     */
    private void loadTranscripts(Criteria studentCriteria) {
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        X2Criteria transcriptCriteria = new X2Criteria();
        transcriptCriteria.addNotEmpty(Transcript.COL_MASTER_SCHEDULE_OID, getBroker().getPersistenceKey());
        transcriptCriteria.addIn(Transcript.COL_STUDENT_OID, subQuery);
        transcriptCriteria.addLessOrEqualThan(Transcript.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER
                + DistrictSchoolYearContext.COL_START_DATE, m_reportDate);
        transcriptCriteria.addGreaterOrEqualThan(
                Transcript.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER + DistrictSchoolYearContext.COL_END_DATE,
                m_reportDate);
        QueryByCriteria query = new QueryByCriteria(Transcript.class, transcriptCriteria);
        m_transcriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 500);

        /*
         * Load a map of grade scale definitions by grade scale oid.
         */
        Map<String, Collection<GradeScaleGradeDefinition>> gradeScalesByGsd;
        // transcriptCriteria = new Criteria();
        query = new QueryByCriteria(GradeScaleGradeDefinition.class, null);
        gradeScalesByGsd =
                getBroker().getGroupedCollectionByQuery(query, GradeScaleGradeDefinition.COL_GRADE_SCALE_OID, 10);

        /*
         * Remap grade scales by transcript definition Oid for easier retrieval.
         */
        m_gradeScales = new HashMap<String, Collection<GradeScaleGradeDefinition>>();
        transcriptCriteria = new X2Criteria();
        transcriptCriteria.addEqualTo(TranscriptColumnDefinition.COL_DATA_FIELD_CONFIG_OID, "fddX2000000533");
        query = new QueryByCriteria(TranscriptColumnDefinition.class, transcriptCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                TranscriptColumnDefinition gcd = (TranscriptColumnDefinition) iterator.next();
                Collection<GradeScaleGradeDefinition> gsgs = gradeScalesByGsd.get(gcd.getGradeScaleOid());
                if (gsgs != null) {
                    m_gradeScales.put(gcd.getTranscriptDefinitionOid(), gsgs);
                }
            }
        } finally {
            iterator.close();
        }
    }
}
