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
package com.x2dev.procedures.statereporting.md;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Maryland State Report: Attendance export.
 * This class implements the data export for MD Attendance export.
 *
 * @author X2 Development Corporation
 */
public class Attendance extends MDStudentReportData {
    /**
     * Implementation of StateReportEntity to be used by the MD Attendance export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class AttendanceEntity extends MDStudentReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        Attendance m_attendanceData = null;
        Date m_birthDate = null;
        SisSchool m_overrideSchool = null;
        List<MembershipAttendance> m_schoolList = new ArrayList<MembershipAttendance>();
        EnrollmentSnapshot m_snapshot = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public AttendanceEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Check enrollment school/membership
         * to determine if the student should be reported.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            if (m_schoolList.size() == 0) {
                error = new StateReportValidationError(getEntityName(), "School membership",
                        "No school membership days", "");
            }

            return error;
        }

        /**
         * Gets the current format definition id.
         *
         * @return String
         */
        @Override
        public String getCurrentFormatDefinitionId() {
            return m_attendanceData.isEOYExport() ? FIELD_KEY_EOY
                    : m_attendanceData.isEarlyExport() ? FIELD_KEY_EARLY : null;
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
                    ", SASID: " + student.getStateId();

            MembershipAttendance memb = getMembershipAttendance();
            if (memb != null) {
                SisSchool school = (SisSchool) getData().getBroker().getBeanByOid(SisSchool.class, memb.getSchoolOid());
                if (school != null) {
                    name += ", SCHOOL: " + school.getName();
                }
            }

            name += "]";

            return name;
        }

        /**
         * Returns the MembershipAttendance record for the current index.
         *
         * @return MembershipAttendance
         */
        public MembershipAttendance getMembershipAttendance() {
            MembershipAttendance att = null;
            if (m_schoolList.size() > getCurrentRow()) {
                att = m_schoolList.get(getCurrentRow());
            }
            return att;
        }

        /**
         * Returns the override school if the student has an override school.
         *
         * @return School
         */
        public SisSchool getOverrideSchool() {
            return m_overrideSchool;
        }

        /**
         * Gets the school.
         *
         * @param data MDStudentReportData
         * @return Sis school
         * @see com.x2dev.procedures.statereporting.md.MDStudentReportData.MDStudentReportEntity#getSchool()
         */
        @Override
        public SisSchool getSchool(MDStudentReportData data) {
            SisSchool school = getOverrideSchool();
            if (school == null) {
                school = (SisSchool) data.getBroker().getBeanByOid(SisSchool.class,
                        getMembershipAttendance().getSchoolOid());
            }

            return school;
        }

        /**
         * Return the enrollment snapshot that is used by some
         * fieldRetrievers to get enrollment data.
         *
         * @param reportDate PlainDate
         * @return the EnrollmentSnapshot for the student.
         */
        public EnrollmentSnapshot getSnapshot(PlainDate reportDate) {
            if (m_snapshot == null) {
                m_snapshot = getSnapshot((SisStudent) getBean(), reportDate);
            }
            return m_snapshot;
        }

        /**
         * Intitialize.
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

            m_attendanceData = (Attendance) data;
            SisStudent student = (SisStudent) bean;

            m_overrideSchool = m_attendanceData.getOverrideSchool(student);

            /*
             * Determine which, and how many, schools the student attended in the year.
             * One entry for each school the student attended in the school year, with
             * attendance for that school.
             * 1. Start with the current snapshot.
             * 2. Work back through all enrollment activity to the beginning of the year.
             */
            EnrollmentSnapshot snapshot = getSnapshot(m_attendanceData.m_reportDate);
            SisSchool school = null;
            if (snapshot != null && snapshot.getSchool() != null) {
                school = snapshot.getSchool();
            } else {
                school = student.getSchool();
            }
            String enrollCode = snapshot.getEnrollmentStatus();
            String withdrawCode = null;
            PlainDate firstDate = m_attendanceData.getSchoolStartDate();
            PlainDate lastDate = m_attendanceData.m_reportDate;
            String lastType = "E";
            if (StudentManager.isActiveStudent(m_attendanceData.getOrganization(), enrollCode)) {
                lastType = "W";
            }
            int lastYog = 0;
            boolean BOYchecked = false;

            List<StudentEnrollment> enrollments = m_attendanceData.m_enrollmentManager.getOrderedEnrollment(student,
                    null,
                    m_attendanceData.m_reportDate,
                    null,
                    false);

            // Search the enrollments for a pair of enrollment records (E,W) on start of school and
            // summer withdrawal dates.
            // If so, pull them out and set aside as a summer withdrawal record.
            StudentEnrollment summerWithdrawalEntry = null;
            StudentEnrollment summerWithdrawalExit = null;
            boolean summerWithdrawal = false;

            // If there is a withdrawal on the summer start start date, the enrollment record should
            // be stored.
            for (StudentEnrollment enrollment : enrollments) {
                String enrollmentType = enrollment.getEnrollmentType();
                if (null != enrollment && null != enrollmentType
                        && enrollmentType.equals(StudentEnrollment.WITHDRAWAL)
                        && null != m_attendanceData && null != m_attendanceData.getSummerStartDate()
                        && m_attendanceData.getSummerStartDate().equals(enrollment.getEnrollmentDate())) {
                    summerWithdrawalExit = enrollment;
                }
            }

            // If there is a summer withdrawal record, remove it and set summerWithdrawal to true.
            if (summerWithdrawalExit != null) {
                // Reset current status to reflect a withdrawn student.
                // The enrollment snapshot above would have reported an active student
                lastType = "E";
                enrollments.remove(summerWithdrawalExit);
                summerWithdrawal = true;

                // Search for corresponding enrollment record for the same school following the
                // summer exit record.
                for (StudentEnrollment enrollment : enrollments) {
                    if (enrollment.getEnrollmentType().equals(StudentEnrollment.ENTRY) &&
                            m_attendanceData.getSchoolStartDate().equals(enrollment.getEnrollmentDate()) &&
                            summerWithdrawalExit.getSchoolOid().equals(enrollment.getSchoolOid())) {
                        summerWithdrawalEntry = enrollment;
                    }
                }

                // If an enrollment record of this type exists remove it.
                if (summerWithdrawalEntry != null) {
                    enrollments.remove(summerWithdrawalEntry);
                }
            }

            for (StudentEnrollment enrollment : enrollments) {
                // Check preferences for entry/withdrawal membership days.
                // Find the actual date
                PlainDate enrollRecDate = findMembershipDate(enrollment);

                // Do not report activity after the report date.
                // Can reset initial status for when we work into applicable enrollment records.
                if (enrollRecDate.after(m_attendanceData.m_reportDate)) {
                    if (enrollment.getEnrollmentType().equals(StudentEnrollment.ENTRY)) {
                        // Assume the student was not active before the entry.
                        // lastType will be set.
                        lastType = "E";
                    } else if (enrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                        // assume the student was active before a withdrawal, but count them as
                        // active
                        // at end of reporting period.
                        school = enrollment.getSchool();
                        firstDate = m_attendanceData.getSchoolStartDate();
                        lastDate = m_attendanceData.m_reportDate;
                        withdrawCode = null;
                        lastYog = enrollment.getYog();
                        lastType = "W";
                    }
                    continue;
                }

                // On the first record before the start of school, check to
                // create artificial R02 if necessary.
                if (enrollRecDate.before(m_attendanceData.getSchoolStartDate()) && !BOYchecked) {
                    if (!"E".equals(lastType) && m_attendanceData.getGenerateR02()) {
                        firstDate = m_attendanceData.getSchoolStartDate();
                        enrollCode = "R02";
                        lastType = "E";
                        addMembership(school.getOid(), enrollCode, withdrawCode, firstDate, lastDate, m_attendanceData,
                                lastYog, false, null);
                    }
                    BOYchecked = true;
                }

                // If before reporting range, exit loop.
                if ((m_attendanceData.getReportPeriod() < 2
                        && enrollRecDate.before(m_attendanceData.getSchoolStartDate())) ||
                        (m_attendanceData.getReportPeriod() == 2
                                && enrollRecDate.before(m_attendanceData.getSummerStartDate()))) {
                    // Exit the loop.
                    break;
                }

                if (enrollment.getEnrollmentType().equals(StudentEnrollment.ENTRY)) {
                    firstDate = enrollment.getEnrollmentDate();
                    enrollCode = enrollment.getEnrollmentCode();
                    school = enrollment.getSchool();
                    lastYog = enrollment.getYog();
                    String accountabilitySchool = m_attendanceData.getAccountabilitySchool(enrollment);
                    addMembership(school.getOid(), enrollCode, withdrawCode, firstDate, lastDate, m_attendanceData,
                            lastYog,
                            false,
                            accountabilitySchool);
                } else if (enrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                    /*
                     * if (lastType.equals(StudentEnrollment.ENTRY))
                     * {
                     * addMembership(schoolOid, enrollCode, withdrawCode, firstDate, lastDate,
                     * m_attendanceData, lastYog);
                     * membCount++;
                     * }
                     */
                    // Set up the data for the time prior to the enrollment.
                    school = enrollment.getSchool();
                    firstDate = m_attendanceData.getSchoolStartDate();
                    lastDate = enrollment.getEnrollmentDate();
                    withdrawCode = enrollment.getEnrollmentCode();
                    lastYog = enrollment.getYog();
                } else {
                    enrollCode = enrollment.getEnrollmentCode();
                    school = enrollment.getSchool();
                }
                lastType = enrollment.getEnrollmentType();
            }
            /*
             * if (enrCount > 0 && "W".equals(lastType))
             * {
             * addMembership(schoolOid, enrollCode, withdrawCode, firstDate, lastDate,
             * m_attendanceData, lastYog);
             * membCount++;
             * }
             */

            // Now apply summer withdrawal if present. Only for September report.
            if (summerWithdrawal && ((Attendance) getData()).getReportPeriod() == 2) {
                addMembership(summerWithdrawalExit.getSchoolOid(),
                        StudentEnrollment.ENTRY,
                        summerWithdrawalExit.getEnrollmentCode(),
                        m_attendanceData.getSchoolStartDate(),
                        summerWithdrawalExit.getEnrollmentDate(),
                        m_attendanceData,
                        summerWithdrawalExit.getYog(),
                        true,
                        null);
            }

            setRowCount(m_schoolList.size());
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
         * Adds a school and date range to the students list of schools attended in the school year.
         *
         * @param schoolOid String
         * @param enrollCode String
         * @param withdrawCode String
         * @param firstDate PlainDate
         * @param lastDate PlainDate
         * @param attendanceData Attendance
         * @param yog int
         * @param summerWithdrawal boolean
         */
        private void addMembership(String schoolOid,
                                   String enrollCode,
                                   String withdrawCode,
                                   PlainDate firstDate,
                                   PlainDate lastDate,
                                   Attendance attendanceData,
                                   int yog,
                                   boolean summerWithdrawal,
                                   String accountabilitySchool) {
            SisStudent student = (SisStudent) getBean();
            // check that the school is not archive/inactive.
            SisSchool school = (SisSchool) m_attendanceData.getBroker().getBeanByOid(SisSchool.class, schoolOid);

            if ((getData().isSchoolContext() && getData().getSchool().getOid().equals(schoolOid)) ||
                    !getData().isSchoolContext()) {
                if (school != null &&
                        ((!school.getArchiveIndicator() && !school.getInactiveIndicator()) ||
                                ((Attendance) getData()).getReportPeriod() == 2
                                        && !StringUtils.isEmpty(withdrawCode))) {
                    /*
                     * Get the attendance days for the student in the school.
                     *
                     * NOTE: Student calendar code would be for the current school
                     * when reporting a prior school. We only have the current calendar code.
                     * This could report incorrect membership days if:
                     * 1. The calendar code is different between the student in the prior school and
                     * current school.
                     * 2. The calendars in the prior school indicated by the students current
                     * calendar ID
                     * and their actual calendar ID for that school are different enough to
                     * calculate
                     * different day counts.
                     */

                    // If the student has an override school, get their membership and school days
                    // from the override school.
                    SisSchool useSchool = school;
                    if (m_overrideSchool != null) {
                        useSchool = m_overrideSchool;
                    }
                    Set<PlainDate> schoolSessionDays =
                            attendanceData.getCalendarDays(useSchool, student.getCalendarCode());
                    MembershipAttendance membership = new MembershipAttendance(schoolOid, enrollCode, withdrawCode,
                            firstDate, lastDate, yog, summerWithdrawal, accountabilitySchool);
                    int memberDays = m_attendanceData.m_enrollmentManager.getMembershipTotal(
                            student,
                            schoolSessionDays,
                            true,
                            firstDate,
                            lastDate,
                            school);
                    // See if there are member days.
                    // If this is the EOY or September report, include summer withdrawals (which may
                    // not have membership days)
                    // Otherwise, exclude them.
                    if (summerWithdrawal) {
                        membership.setMembership(0);
                        membership.setAbsent(0);
                        membership.setSchoolMembership(0);
                        membership.setEnrollCode("000");
                        membership.setEnrollDate(null);
                        m_schoolList.add(membership);
                    } else if (memberDays > 0 || !StringUtils.isEmpty(withdrawCode)) {
                        List<StudentAttendance> absences = attendanceData.m_attendance.get(student.getOid());
                        float absenceDays = 0;
                        float absenceDaysUnex = 0;
                        if (absences != null) {
                            boolean includeLastDate = true;
                            if (!StringUtils.isEmpty(withdrawCode) && !attendanceData.getWithdrawalIsMembership()) {
                                includeLastDate = false;
                            }
                            for (StudentAttendance attendance : absences) {
                                if ((attendance.getDate().equals(firstDate) || attendance.getDate().after(firstDate)) &&
                                        ((attendance.getDate().equals(lastDate) && includeLastDate)
                                                || attendance.getDate().before(lastDate))
                                        &&
                                        attendance.getPortionAbsent() != null) {
                                    absenceDays += attendance.getPortionAbsent().floatValue();
                                    if (!attendance.getExcusedIndicator()) {
                                        absenceDaysUnex += attendance.getPortionAbsent().floatValue();
                                    }
                                }
                            }
                        }

                        int schMemb = (schoolSessionDays == null ? 0 : schoolSessionDays.size());

                        membership.setMembership(memberDays);
                        membership.setAbsent(absenceDays);
                        membership.setAbsentUnex(absenceDaysUnex);
                        membership.setSchoolMembership(schMemb);
                        m_schoolList.add(0, membership);
                    }
                }
            }
        }

        /**
         * For a given enrollment record, find the enrollment date as a membership date.
         * System preferences can indicate that an entry or withdrawal is not a membership date
         * but the previous/next date is.
         * <p>
         * If those preferences are set, then look up the appropriate date.
         * <br>
         * 1. Find a prev/next membership date if available.
         * <br>
         * 2. If no membership date is available (summer activity) then keep the specified
         * date.
         *
         *
         * @param enrollment StudentEnrollment
         * @return PlainDate
         */
        private PlainDate findMembershipDate(StudentEnrollment enrollment) {
            PlainDate enrollmentDate = enrollment.getEnrollmentDate();
            PlainDate newEnrollmentDate = null;
            SisStudent student = (SisStudent) getBean();
            SisSchool school = enrollment.getSchool();
            Set<PlainDate> schoolSessionDays = m_attendanceData.getCalendarDays(school, student.getCalendarCode());

            if (schoolSessionDays != null) {
                if (enrollment.getEnrollmentType().equals(StudentEnrollment.ENTRY)
                        && !m_attendanceData.getEntryIsMembership()) {
                    for (PlainDate membDate : schoolSessionDays) {
                        if (membDate.after(enrollmentDate)
                                && (newEnrollmentDate == null || membDate.before(newEnrollmentDate))) {
                            newEnrollmentDate = membDate;
                        }
                    }
                } else if (enrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)
                        && !m_attendanceData.getWithdrawalIsMembership()) {
                    for (PlainDate membDate : schoolSessionDays) {
                        if (membDate.before(enrollmentDate)
                                && (newEnrollmentDate == null || membDate.after(newEnrollmentDate))) {
                            newEnrollmentDate = membDate;
                        }
                    }
                }
            }
            if (newEnrollmentDate == null) {
                newEnrollmentDate = enrollmentDate;
            }

            return enrollmentDate;
        }

        /**
         * Returns the enrollment snapshot for the student as of the report date.
         *
         * @param student SisStudent
         * @param reportDate PlainDate
         * @return EnrollmentSnapshot
         */
        private EnrollmentSnapshot getSnapshot(SisStudent student, PlainDate reportDate) {
            EnrollmentSnapshot snapshot = new EnrollmentSnapshot(student, reportDate, getData().getBroker());
            return snapshot;
        }
    }

    /**
     * A class for storing membership and attendance statistics for averages checking.
     *
     * @author X2 Development Corporation
     */
    protected static class MembershipAttendance {
        /*
         * Instance for school, dates, counts and codes.
         */
        float m_absent;
        float m_absentUnex;
        String m_accountabilitySchool;
        PlainDate m_beginDate;
        PlainDate m_endDate;
        String m_enrollCode;
        int m_lastYog;
        int m_membership;
        int m_schoolMembership;
        String m_schoolOid;
        boolean m_summerWithdrawal;
        String m_withdrawCode;

        /**
         * constructor, set initial attendance and membership counts.
         *
         * @param schoolOid String
         * @param enrollCode String
         * @param withdrawCode String
         * @param beginDate PlainDate
         * @param endDate PlainDate
         * @param yog int
         * @param summerWithdrawal boolean
         */
        protected MembershipAttendance(String schoolOid, String enrollCode, String withdrawCode, PlainDate beginDate,
                PlainDate endDate, int yog, boolean summerWithdrawal, String accountabilitySchool) {
            m_schoolOid = schoolOid;
            m_enrollCode = enrollCode;
            m_withdrawCode = withdrawCode;
            m_beginDate = beginDate;
            m_endDate = endDate;
            m_lastYog = yog;
            m_summerWithdrawal = summerWithdrawal;
            m_accountabilitySchool = accountabilitySchool;
        }

        /**
         * Return the accumulated absent count.
         *
         * @return float
         */
        protected float getAbsent() {
            return m_absent;
        }

        /**
         * Return the accumulated unexcused absent count.
         *
         * @return float
         */
        protected float getAbsentUnex() {
            return m_absentUnex;
        }

        /**
         * @return the m_accountabilitySchool
         */
        protected String getAccountabilitySchool() {
            return m_accountabilitySchool;
        }

        /**
         * Returns the membership begin date for the enrollment period.
         *
         * @return PlainDate
         */
        protected PlainDate getBeginDate() {
            return m_beginDate;
        }

        /**
         * Returns the membership end date for the enrollment period.
         *
         * @return PlainDate
         */
        protected PlainDate getEndDate() {
            return m_endDate;
        }

        /**
         * Returns the students enrollment code for the current membership period.
         *
         * @return String
         */
        protected String getEnrollCode() {
            return m_enrollCode;
        }

        /**
         * Return the last YOG of the enrollment record.
         * This allows grade calculation in case the student YOG is off.
         *
         * @return int
         */
        protected int getLastYog() {
            return m_lastYog;
        }

        /**
         * Return the accumulated membership count for the student.
         *
         * @return int
         */
        protected int getMembership() {
            return m_membership;
        }

        /**
         * Return the accumulated membership count for the school.
         *
         * @return int
         */
        protected int getSchoolMembership() {
            return m_schoolMembership;
        }

        /**
         * Returns the school for this record.
         *
         * @return String
         */
        protected String getSchoolOid() {
            return m_schoolOid;
        }

        /**
         * Returns the summer withdrawal indicator;.
         *
         * @return boolean
         */
        protected boolean getSummerWithdrawal() {
            return m_summerWithdrawal;
        }

        /**
         * Returns the withdraw code for the students enrollment segment.
         *
         * @return String
         */
        protected String getWithdrawCode() {
            return m_withdrawCode;
        }

        /**
         * Set the accumulated absence count.
         *
         * @param absent void
         */
        protected void setAbsent(float absent) {
            m_absent = absent;
        }

        /**
         * Set the accumulated unexcused absence count.
         *
         * @param absent void
         */
        protected void setAbsentUnex(float absent) {
            m_absentUnex = absent;
        }

        /**
         * @param accountabilitySchool
         */
        protected void setAccountabilitySchool(String accountabilitySchool) {
            this.m_accountabilitySchool = accountabilitySchool;
        }

        /**
         * Set the enrollment code. Override for summer withdrawals.
         *
         * @param enrollCode void
         */
        protected void setEnrollCode(String enrollCode) {
            m_enrollCode = enrollCode;
        }

        /**
         * Set the enrollment code. Override for summer withdrawals.
         *
         * @param enrollDate void
         */
        protected void setEnrollDate(PlainDate enrollDate) {
            m_beginDate = enrollDate;
        }

        /**
         * Set the accumulated membership count for the student.
         *
         * @param membership void
         */
        protected void setMembership(int membership) {
            m_membership = membership;
        }

        /**
         * Set the accumulated membership count for the school.
         *
         * @param schoolMembership void
         */
        protected void setSchoolMembership(int schoolMembership) {
            m_schoolMembership = schoolMembership;
        }

    }

    public static final String FIELD_KEY_EARLY = "EARLY";
    public static final String FIELD_KEY_EOY = "EOY";

    /**
     * Name of parameter for the generate R02 indicator.
     */
    public static final String INPUT_PARAM_GENERATE_R02 = "generateR02";

    /**
     * Name for the report period parameter. The corresponding values is a Integer object.
     */
    public static final String INPUT_PARAM_REPORT_PERIOD = "reportPeriod";

    /**
     * Name for the "summer end date" parameter. The corresponding values is a java.sql.Date object.
     */
    public static final String INPUT_PARAM_SUMMER_END_DATE = "summerEndDate";

    /**
     * Name for the "summer start date" parameter. The corresponding values is a java.sql.Date
     * object.
     */
    public static final String INPUT_PARAM_SUMMER_START_DATE = "summerStartDate";

    /**
     * Name for the "school start date" parameter. The corresponding value is a java.sql.Date
     * object.
     */
    public static final String INPUT_PARAM_SCHOOL_START_DATE = "schoolStartDate";

    public static final Integer REPORT_PERIOD_EARLY = Integer.valueOf(0);
    public static final Integer REPORT_PERIOD_EOY = Integer.valueOf(1);
    public static final Integer REPORT_PERIOD_SEPTEMBER = Integer.valueOf(2);

    /**
     * Field parameter values for RetrieveHomeless indicating type of homeless information.
     */
    public static final String VALUE_HOMELESS_CODE = "CODE";
    public static final String VALUE_HOMELESS_RESIDENCE = "RESIDENCE";
    public static final String VALUE_HOMELESS_MCKINNEY = "MCKINNEY";
    public static final String VALUE_HOMELESS_OTHER = "OTHER";
    public static final String VALUE_HOMELESS_UY = "UY";

    /*
     * Field definition names for value lookup.
     */
    protected static final String FIELD_WITHDRAWAL_STATUS = "Withdrawal Status";
    protected static final String FIELD_SCHOOL_CODE = "School Number";

    /**
     * Aliases
     */
    private static final String ALIAS_ACCOUNTABILITY_SCHOOL = "all-enr-AccountabilitySchool";
    private static final String ALIAS_ADDR_GEO_ID = "all-adr-GeolocationID";
    private static final String ALIAS_CRS_DUAL_ENROLLMENT = "DOE DUAL ENROLLMENT";

    /*
     * Field aliases in the program table for program values.
     * alternate sources for some program data.
     */
    // private static final String DOE_PROGRAM_TAS = "DOE PR TAS";
    private static final String DOE_SPED_STATUS = "DOE special ed";
    private static final String DOE_SPED_END = "DOE SPED END";

    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, List<StudentAttendance>> m_attendance;
    protected EnrollmentManager m_enrollmentManager;
    protected Boolean m_entryIsMembership;
    protected DataDictionaryField m_fieldAccountabilitySchool;
    protected String m_fieldAddrGeoId;
    protected String m_fieldCrsDualEnr;
    protected Boolean m_generateR02;
    protected Integer m_reportPeriod;
    protected Map<String, List<StudentSchedule>> m_sscByStdOidMap;
    protected PlainDate m_schoolStartDate;
    protected Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars;
    protected String m_spedEndDate;
    protected String m_spedStatus;
    protected PlainDate m_summerEndDate;
    protected PlainDate m_summerStartDate;
    protected Boolean m_withdrawalIsMembership;

    protected class Retrieve_92_93_94 implements FieldRetriever {
        private static final String FIELD_GRADE = "Grade";
        private final List<String> GRADES_PROCESSED = Arrays.asList("11", "92", "93", "94");


        private static final String CALC_ID = "ATT_92_93_94_ONLY";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String gradeLevel = entity.getFieldValue(FIELD_GRADE);
            if (GRADES_PROCESSED.contains(gradeLevel)) {
                value = entity.getBean().getFieldValueByBeanPath(field.getBeanPath());
            }
            return value;
        }

    }

    /**
     * The Class RetrieveDualEnrollment.
     */
    protected class RetrieveDualEnrollment implements FieldRetriever {

        private static final String CALC_ID = "DUAL-ENR";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return (m_sscByStdOidMap.get(entity.getBean().getOid()) != null
                    && !m_sscByStdOidMap.get(entity.getBean().getOid()).isEmpty()) ? "Y" : "N";
        }
    }

    /**
     * Returns the state equivalent for the enrollment status, code or date of the student based on
     * the membership period.
     * The student may have an multiple membership periods. The membership period comes from the
     * entry and current index.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEnrollment implements FieldRetriever {
        private static final String CALC_PARAM_GEOLOCATION_ID = "GEOLOCATION_ID";
        private static final String VALUE_ENROLL_ACCOUNTABILITY_SCHOOL = "ACC_SCHOOL";
        private static final String VALUE_ENROLL_ENTRY_CODE = "ENTRY_CODE";
        private static final String VALUE_ENROLL_ENTRY_STATUS = "ENTRY_STATUS";
        private static final String VALUE_ENROLL_ENTRY_DATE = "ENTRY_DATE";
        private static final String VALUE_ENROLL_WITHDRAWAL_CODE = "WITHDRAW_CODE";
        private static final String VALUE_ENROLL_WITHDRAWAL_STATUS = "WITHDRAW_STATUS";
        private static final String VALUE_ENROLL_WITHDRAWAL_DATE = "WITHDRAW_DATE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            MembershipAttendance membership = ((AttendanceEntity) entity).getMembershipAttendance();
            SisStudent std = (SisStudent) entity.getBean();
            String param = (String) field.getParameter();
            Object value = null;
            if (VALUE_ENROLL_ENTRY_STATUS.equals(param)) {
                String code = membership.getEnrollCode();
                if (code != null && code.length() > 0) {
                    value = code.substring(0, 1);
                }
            } else if (VALUE_ENROLL_ENTRY_CODE.equals(param)) {
                String code = membership.getEnrollCode();
                if (code != null && code.length() > 2) {
                    value = code.substring(1, 3);
                }
            } else if (VALUE_ENROLL_ENTRY_DATE.equals(param)) {
                value = membership.getBeginDate();
            } else if (VALUE_ENROLL_WITHDRAWAL_STATUS.equals(param)) {
                String code = membership.getWithdrawCode();
                if (code != null && code.length() > 0) {
                    value = code.substring(0, 1);
                }
            } else if (VALUE_ENROLL_WITHDRAWAL_CODE.equals(param)) {
                String code = membership.getWithdrawCode();
                if (code != null && code.length() > 2) {
                    value = code.substring(1, 3);
                }
            } else if (VALUE_ENROLL_WITHDRAWAL_DATE.equals(param)) {
                String code = membership.getWithdrawCode();
                if (code != null) {
                    value = membership.getEndDate();
                }
            } else if (VALUE_ENROLL_ACCOUNTABILITY_SCHOOL.equals(param)) {
                value = membership.getAccountabilitySchool();
                if (value == null) {
                    value = entity.getFieldValue(FIELD_SCHOOL_CODE);
                }
            } else if (CALC_PARAM_GEOLOCATION_ID.equals(param)) {
                SisAddress stdAdr = std.getPerson().getPhysicalAddress();
                if (stdAdr != null) {
                    value = stdAdr.getFieldValueByBeanPath(m_fieldAddrGeoId);
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the homeless indicator based on homeless status on the report date.
     *
     * @author X2 Development Corporation
     */
    /**
     * Retrieve membership days for a student.
     * This retrieves the MembershipAttendance for the current segment and applies to one school.
     *
     * This returns one of three values based on the Field parameter (Integer):
     * 1 = Days in attendance (student membership - student absences)
     * 2 = Days absent
     * 3 = Days not in membership (school membership days - student membership days)
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveMembership implements FieldRetriever {
        private static final String VALUE_MEMBERSHIP_ATTENDANCE = "ATTENDANCE";
        private static final String VALUE_MEMBERSHIP_ABSENT = "ABSENT";
        private static final String VALUE_MEMBERSHIP_ABSENTUNEX = "ABSENTUNEX";
        private static final String VALUE_MEMBERSHIP_NOT_BELONG = "NOT BELONGING";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            MembershipAttendance membership = ((AttendanceEntity) entity).getMembershipAttendance();
            String param = (String) field.getParameter();
            SisSchool school =
                    (SisSchool) data.getBroker().getBeanByOid(SisSchool.class, membership.getSchoolOid());

            float attendCount = 0;
            if (VALUE_MEMBERSHIP_ATTENDANCE.equals(param)) {
                // Get days in attendance count. This is membership minus absences.
                float stdMemb = membership.getMembership();
                float absentCount = membership.getAbsent();
                attendCount = stdMemb - absentCount;
                if (attendCount < 0) {
                    entity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity, field,
                                    "Attendance less than zero",
                                    "Attendance=" + STYLE_BOLD + Float.toString(attendCount) + STYLE_END));
                    attendCount = 0;
                }
            } else if (VALUE_MEMBERSHIP_ABSENT.equals(param)) {
                // Get days absent.
                attendCount = membership.getAbsent();
            } else if (VALUE_MEMBERSHIP_ABSENTUNEX.equals(param)) {
                // Get days absent.
                attendCount = membership.getAbsentUnex();
            } else if (VALUE_MEMBERSHIP_NOT_BELONG.equals(param)) {
                // Get school not in membership days.

                // First, get all membership days in this school for this student.
                // This includes multiple memberships, so check all memberships for this school
                // Oid.
                float stdMemb = membership.getMembership();
                float schoolMemb = membership.getSchoolMembership();
                attendCount = schoolMemb - stdMemb;
                if (attendCount < 0) {
                    SisStudent student = (SisStudent) entity.getBean();
                    entity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity, field,
                                    "Nonmembership less than zero",
                                    "Nonmembership=" + STYLE_BOLD + Float.toString(attendCount) + STYLE_END +
                                            ", School=" + STYLE_BOLD + school.getName() + STYLE_END +
                                            ", Calendar=" + STYLE_BOLD + student.getCalendarCode() + STYLE_END));
                    attendCount = 0;
                }
            }

            /*
             * Export format is four digits with implied decimal. ( 23.5 ==> "0235" )
             * Multiply by ten to get the single decimal place above the decimal,
             * then allow the field formatter to format the numeric with four zeros.
             */
            value = Float.valueOf(Double.valueOf(attendCount * 10.0).floatValue());
            return value;
        }
    }

    /**
     * The Class RetrievePromotionStatus.
     */
    protected class RetrievePromotionStatus implements FieldRetriever {
        private static final String ALIAS_NON_PROMOTION = "all-std-NonPromotion";

        private String m_fieldNonPromotion = null;
        private Map<String, Map<String, PlainDate>> m_mapSchoolLastDate = new HashMap();

        /**
         * Instantiates a new retrieve promotion status.
         */
        protected RetrievePromotionStatus() {
            super();
            m_fieldNonPromotion = translateAliasToJavaName(ALIAS_NON_PROMOTION, false);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = "01";
            AttendanceEntity attEnt = (AttendanceEntity) entity;
            MembershipAttendance membAtt = attEnt.getMembershipAttendance();
            SisStudent student = (SisStudent) entity.getBean();
            if (membAtt.getEndDate() != null && !StringUtils.isEmpty(membAtt.getSchoolOid())) {
                SisSchool school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, membAtt.getSchoolOid());
                if (school != null) {
                    String calendar = student.getCalendarCode();
                    PlainDate lastSchoolDate = getLastSchoolDate(school, calendar);
                    if (lastSchoolDate != null && lastSchoolDate.after(membAtt.getEndDate())) {
                        value = "00";
                    }
                }
            }
            if (!StringUtils.isEmpty(m_fieldNonPromotion)) {
                String code = (String) student.getFieldValueByBeanPath(m_fieldNonPromotion);
                if (!StringUtils.isEmpty(code)) {
                    code = lookupStateValue(SisStudent.class, m_fieldNonPromotion, code);
                    if (!StringUtils.isEmpty(code)) {
                        value = code;
                    }
                }
            }
            return value;
        }

        /**
         * Gets the last school date.
         *
         * @param school SisSchool
         * @param calendar String
         * @return Plain date
         */
        private PlainDate getLastSchoolDate(SisSchool school, String calendar) {
            PlainDate date = null;
            Map<String, PlainDate> mapDate = m_mapSchoolLastDate.get(school.getOid());
            if (mapDate == null) {
                mapDate = new HashMap();
                m_mapSchoolLastDate.put(school.getOid(), mapDate);
            }
            if (!mapDate.containsKey(calendar)) {

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
                criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER
                        + SchoolCalendar.COL_CALENDAR_ID, calendar);
                criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER
                        + SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
                criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER
                        + SchoolCalendar.COL_SCHOOL_OID, school.getOid());
                BeanQuery query = new BeanQuery(SchoolCalendarDate.class, criteria);
                query.addOrderByDescending(SchoolCalendarDate.COL_DATE);
                SchoolCalendarDate calDate = getCalendarDate(calendar, school.getOid(), true);
                if (calDate == null) {
                    calDate = getCalendarDate(calendar, school.getOid(), false);
                }
                date = calDate == null ? null : calDate.getDate();
                mapDate.put(calendar, date);
            } else {
                date = mapDate.get(calendar);
            }
            return date;
        }

        /**
         * Gets the calendar date.
         *
         * @param calendar String
         * @param schoolOid String
         * @param matchCalendar boolean
         * @return School calendar date
         */
        private SchoolCalendarDate getCalendarDate(String calendar, String schoolOid, boolean matchCalendar) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER
                    + SchoolCalendar.COL_CALENDAR_ID, calendar);
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER
                    + SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
            if (matchCalendar) {
                criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER
                        + SchoolCalendar.COL_SCHOOL_OID, schoolOid);
            }

            BeanQuery query = new BeanQuery(SchoolCalendarDate.class, criteria);
            query.addOrderByDescending(SchoolCalendarDate.COL_DATE);
            return (SchoolCalendarDate) getBroker().getBeanByQuery(query);
        }
    }

    /**
     * The Class RetrieveReportPeriod.
     */
    protected class RetrieveReportPeriod implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            if (((String) field.getParameter()).contains(Integer.toString(((Attendance) data).getReportPeriod()))) {
                value = getProperty(entity.getBean(), field.getBeanPath());
            }
            return value;
        }

        /**
         * Looks up the value at the bean path in the X2BaseBean.
         *
         * @param bean X2BaseBean
         * @param beanPath String
         * @return Object
         * @throws X2BaseException exception
         */
        private Object getProperty(X2BaseBean bean, String beanPath) throws X2BaseException {
            Object value = null;
            if (beanPath.charAt(0) != StateReportData.LABEL_PREFIX_CHAR) {
                value = WebUtils.getProperty(bean, beanPath);
            }
            return value;
        }

    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        // Load initialization data
        initializeFields();

        /*
         * Set up converters, formatters, reference lookup tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */

        if (isEOYExport()) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExportFormatDefinition.COL_PROCEDURE_ID, getProcedureId() + "-" + FIELD_KEY_EOY);
            ExportFormatDefinition efd = (ExportFormatDefinition) getBroker()
                    .getBeanByQuery(new QueryByCriteria(ExportFormatDefinition.class, criteria));
            if (efd != null) {
                setEfdOid(efd.getOid());
            }
        } else if (isEarlyExport()) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExportFormatDefinition.COL_PROCEDURE_ID, getProcedureId() + "-" + FIELD_KEY_EARLY);
            ExportFormatDefinition efd = (ExportFormatDefinition) getBroker()
                    .getBeanByQuery(new QueryByCriteria(ExportFormatDefinition.class, criteria));
            if (efd != null) {
                setEfdOid(efd.getOid());
            }
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria studentCriteria = getStudentCriteria();

            // Set the query to be used for student selection.
            setQuery(getStudentQuery());
            setEntityClass(AttendanceEntity.class);

            // Load codes and support data.
            loadEnrollmentData(studentCriteria);

            // Load student attendance maps.
            loadAttendance(studentCriteria);
            loadStudentSchedules(studentCriteria);
            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("ATT-ATTEND", new RetrieveMembership());
            calcs.put("ATT-ENROLL", new RetrieveEnrollment());
            calcs.put("ATT-RPT-PERIOD", new RetrieveReportPeriod());
            calcs.put("ATT-PROMOTION", new RetrievePromotionStatus());
            calcs.put(RetrieveDualEnrollment.CALC_ID, new RetrieveDualEnrollment());
            calcs.put(Retrieve_92_93_94.CALC_ID, new Retrieve_92_93_94());
            super.addCalcs(calcs);

        }
    }


    /**
     * Sets the parameters.
     *
     * @param parameters Map<String,Object>
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#setParameters(java.util.Map)
     */
    @Override
    public void setParameters(Map<String, Object> parameters) {
        super.setParameters(parameters);
    }

    /**
     * Gets the accountability school.
     *
     * @param enrollment StudentEnrollment
     * @return String
     */
    protected String getAccountabilitySchool(StudentEnrollment enrollment) {
        String value = null;
        if (getReportPeriod() == REPORT_PERIOD_EARLY.intValue()) {
            if (m_fieldAccountabilitySchool != null) {
                value = (String) enrollment.getFieldValueByBeanPath(m_fieldAccountabilitySchool.getJavaName());
                if (!StringUtils.isEmpty(value) && m_fieldAccountabilitySchool.hasReferenceTable()) {
                    value = lookupReferenceCodeByRefTbl(m_fieldAccountabilitySchool.getReferenceTableOid(), value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            if (StringUtils.isEmpty(value)) {
                value = (String) enrollment.getSchool().getFieldValueByAlias(DOE_SCHOOL_CODE);
            }
        }
        return value;
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    protected Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
        Map<String, Set<PlainDate>> calendarData = null;
        Set<PlainDate> calendarDates = null;
        if (school != null && !m_schoolsToCalendars.containsKey(school.getOid())) {
            PlainDate startDate = null;
            if (school.getActiveSchedule() != null) {
                startDate = school.getActiveSchedule().getStartDate();
            } else {
                startDate = getCurrentContext().getStartDate();
            }
            calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, m_reportDate);
            m_schoolsToCalendars.put(school.getOid(), calendarData);
        }

        if (school != null) {
            calendarData = m_schoolsToCalendars.get(school.getOid());
            calendarDates = calendarData.get(calendar);
        }
        return calendarDates;
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */

    @Override
    protected Criteria getReportingCriteria() {
        /*
         * Who should be included? Primary students and, optionally, summer withdrawals.
         *
         * The students that belong to each group depend upon how this export is being run:
         *
         * The export is being run for either (A) the entire district or (B) a single school
         *
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case A:
         *
         * Students in an active, non-archived school in the district
         * with enrollment activity (E,W) within the year
         *
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case B:
         *
         * Students with enrollment activity (E,W) within the year.
         *
         * ----------------------------------------------------------------------------------------
         *
         * Summer withdrawals:
         *
         * Students who withdrew during the summer (start/end dates inclusive) and are now in the
         * archive school
         *
         */

        // Select students with primary school, or students with
        // enrollment activity (E,W) in the school this year.
        X2Criteria enrollCriteria = new X2Criteria();
        enrollCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        X2Criteria enrollCriteria2 = new X2Criteria();
        enrollCriteria2.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        enrollCriteria2.addOrCriteria(enrollCriteria);

        // With Enrollment records within the active date range.
        X2Criteria activityCriteria = new X2Criteria();
        if (getSummerStartDate() != null) {
            activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, getSummerStartDate());
        } else {
            activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, getSummerEndDate());
        }
        activityCriteria.addAndCriteria(enrollCriteria2);

        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        enrollCriteria = new X2Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        X2Criteria primaryCriteria = new X2Criteria();
        if (isSchoolContext()) {
            primaryCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            primaryCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            primaryCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        studentCriteria.addOrCriteria(enrollCriteria);
        primaryCriteria.addAndCriteria(studentCriteria);

        X2Criteria reportingCriteria = new X2Criteria();
        reportingCriteria.addAndCriteria(primaryCriteria);

        /*
         * Summer transactions
         */
        if (getSummerStartDate() != null && getSummerEndDate() != null) {
            X2Criteria schoolCriteria = new X2Criteria();

            schoolCriteria.addOrEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);

            X2Criteria summerCriteria = new X2Criteria();
            summerCriteria.addAndCriteria(schoolCriteria);

            summerCriteria.addIn(X2BaseBean.COL_OID,
                    getStudentWithdrawalQuery(getSummerStartDate(), getSummerEndDate()));

            reportingCriteria.addOrCriteria(summerCriteria);
        }

        return reportingCriteria;
    }


    /**
     * Checks if is EOY export.
     *
     * @return true, if is EOY export
     */
    protected boolean isEarlyExport() {
        return getReportPeriod() == REPORT_PERIOD_EARLY.intValue();
    }

    /**
     * Checks if is EOY export.
     *
     * @return true, if is EOY export
     */
    protected boolean isEOYExport() {
        return getReportPeriod() == REPORT_PERIOD_EOY.intValue();
    }

    /**
     * Gets the entry is membership.
     *
     * @return boolean
     */
    private boolean getEntryIsMembership() {
        if (m_entryIsMembership == null) {
            m_entryIsMembership = Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                    SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE));
        }
        return m_entryIsMembership.booleanValue();
    }

    /**
     * Gets the generate R02.
     *
     * @return boolean
     */
    private boolean getGenerateR02() {
        if (m_generateR02 == null) {
            m_generateR02 = ((Boolean) getParameter(INPUT_PARAM_GENERATE_R02));
        }
        return m_generateR02.booleanValue();
    }

    /**
     * Gets the report period.
     *
     * @return int
     */
    private int getReportPeriod() {
        if (m_reportPeriod == null) {
            m_reportPeriod = ((Integer) getParameter(INPUT_PARAM_REPORT_PERIOD));
        }
        return m_reportPeriod.intValue();
    }


    /**
     * Gets the school start date.
     *
     * @return Plain date
     */
    private PlainDate getSchoolStartDate() {
        if (m_schoolStartDate == null) {
            m_schoolStartDate = (PlainDate) getParameter(INPUT_PARAM_SCHOOL_START_DATE);
        }
        return m_schoolStartDate;
    }


    /**
     * Returns a query that finds the students who withdrew during the given date range (filtered
     * by school as appropriate).
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return A SubQuery that returns the student OID from StudentEnrollment records
     */
    private SubQuery getStudentWithdrawalQuery(PlainDate startDate, PlainDate endDate) {
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);

        if (isSchoolContext()) {
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        return new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);
    }

    /**
     * Gets the summer end date.
     *
     * @return Plain date
     */
    private PlainDate getSummerEndDate() {
        if (m_summerEndDate == null) {
            m_summerEndDate = (PlainDate) getParameter(INPUT_PARAM_SUMMER_END_DATE);
        }
        return m_summerEndDate;
    }


    /**
     * Gets the summer start date.
     *
     * @return Plain date
     */
    private PlainDate getSummerStartDate() {
        if (m_summerStartDate == null) {
            m_summerStartDate = (PlainDate) getParameter(INPUT_PARAM_SUMMER_START_DATE);
        }
        return m_summerStartDate;
    }


    /**
     * Gets the withdrawal is membership.
     *
     * @return boolean
     */
    private boolean getWithdrawalIsMembership() {
        if (m_withdrawalIsMembership == null) {
            m_withdrawalIsMembership = Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                    SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE));
        }
        return m_withdrawalIsMembership.booleanValue();
    }


    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_spedStatus = translateAliasToJavaName(DOE_SPED_STATUS, true);
        m_spedEndDate = translateAliasToJavaName(DOE_SPED_END, true);
        m_fieldCrsDualEnr = translateAliasToJavaName(ALIAS_CRS_DUAL_ENROLLMENT, true);
        m_fieldAccountabilitySchool = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_ACCOUNTABILITY_SCHOOL);
        if (m_fieldAccountabilitySchool == null) {
            String aliasMsg =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
            addSetupError(aliasMsg, ALIAS_ACCOUNTABILITY_SCHOOL);
        }
        m_fieldAddrGeoId = translateAliasToJavaName(ALIAS_ADDR_GEO_ID, true);
    }

    /**
     * Loads a map of absence attendance records by student.
     *
     * @param studentCriteria Criteria
     */
    private void loadAttendance(Criteria studentCriteria) {
        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Select all attendance absence records for each student in the reporting date range.
        // Group by student.
        Criteria criteria = new Criteria();
        criteria.addIn(StudentAttendance.COL_STUDENT_OID, studentsSubQuery);
        criteria.addGreaterOrEqualThanField(StudentAttendance.COL_DATE,
                StudentAttendance.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_START_DATE);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        query.addOrderBy(StudentAttendance.COL_STUDENT_OID, true);
        query.addOrderBy(StudentAttendance.COL_DATE, true);

        m_attendance = getBroker().getGroupedCollectionByQuery(query, StudentAttendance.COL_STUDENT_OID, 100);
    }

    /**
     * Prepare enrollment data required by this export.
     * Load set of non-promoted students from non-promoted snapshot.
     *
     * @param studentCriteria Criteria
     */
    private void loadEnrollmentData(Criteria studentCriteria) {
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_schoolsToCalendars = new HashMap<String, Map<String, Set<PlainDate>>>();
    }

    /**
     * Loads a map of student schedules records by student.
     *
     * @param studentCriteria Criteria
     */
    private void loadStudentSchedules(Criteria studentCriteria) {
        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Select all attendance absence records for each student in the reporting date range.
        // Group by student.
        Criteria criteria = new Criteria();
        criteria.addIn(StudentSchedule.COL_STUDENT_OID, studentsSubQuery);
        criteria.addEqualTo(StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                Schedule.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        criteria.addEqualTo(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER + SchoolCourse.REL_COURSE
                + ModelProperty.PATH_DELIMITER + m_fieldCrsDualEnr, Boolean.TRUE);
        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, criteria);
        m_sscByStdOidMap = getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 100);
    }
}
