/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolDistrictContext;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchoolCalendar;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentAttendance;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class TnToolBean {
    public static class TnEnrollment extends ToolEnrollment {

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolEnrollment.FULL_DEFINITION;

        public TnEnrollment(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }
    }


    public static class TnSchool extends ToolSchool {

        private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";

        public static final ToolBeanColumn FIELD_SKL_STATE_ID =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_SKL_STATE_ID);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION =
        ToolSchool.FULL_DEFINITION
        .expand(FIELD_SKL_STATE_ID)
        .expandSort(ToolSchool.FIELD_NAME);

        /**
         * @param columns
         * @param data
         */
        public TnSchool(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

    }

    public static class TnStudent extends ToolStudent {

        private static final String ALIAS_STD_EXCLUDE_FROM_REPORTING = "DOE EXCLUDE STD";
        private static final String ALIAS_STUDENT_EIS_STATE_ID = "DOE EIS STATE ID";

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_EXCLUDE_FROM_REPORTING =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_EXCLUDE_FROM_REPORTING);
        public static final ToolBeanColumn FIELD_EIS_STATE_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STUDENT_EIS_STATE_ID);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION =
        ToolStudent.FULL_DEFINITION
        .expand(FIELD_EIS_STATE_ID);

        /**
         * @param columns
         * @param data
         */
        public TnStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private List<TnStudentEnrollmentSpan> m_enrollmentSpans;

        public String getEISStateId() {
            return getValueString(FIELD_EIS_STATE_ID);
        }

        public Set<PlainDate> getMembershipDates(X2Broker broker,
                                                 TnSchool school,
                                                 List<ToolSchoolCalendarDate> csdList,
                                                 Collection<PlainDate> inSessionDays) {
            Set<PlainDate> datesToMap = new HashSet<PlainDate>();
            for (PlainDate date : inSessionDays) {
                boolean isEnrolled = false;
                for (TnStudentEnrollmentSpan enrollmentSpan : getTnStudentEnrollmentSpans(broker)) {
                    if (enrollmentSpan.getSchool() != null &&
                            enrollmentSpan.getSchool().getOid().equals(school.getOid()) &&
                            !date.before(enrollmentSpan.getFirstActiveDate()) &&
                            (enrollmentSpan.getLastActiveDate() == null ||
                            !date.after(enrollmentSpan.getLastActiveDate()))) {
                        isEnrolled = true;
                        /*
                         * Check if the student is enrolled and withdrawn on the same day.
                         * If yes, then we don't want to count this student.
                         */
                        String enrType = enrollmentSpan.getFirstActiveEnrollment().getEnrollmentType();
                        if ((enrollmentSpan.getFirstActiveEnrollment() != null && (StudentEnrollment.ENTRY
                                .equals(enrType)
                                || StudentEnrollment.YOG_CHANGE
                                .equals(enrType))
                                &&
                                enrollmentSpan.getFirstInactiveEnrollment() != null && StudentEnrollment.WITHDRAWAL
                                .equals(enrollmentSpan.getFirstInactiveEnrollment().getEnrollmentType())
                                &&
                                (enrollmentSpan.getFirstActiveEnrollment().getEnrollmentDate()
                                        .equals(enrollmentSpan.getFirstInactiveEnrollment().getEnrollmentDate())
                                        || (csdList != null
                                        && !csdList.isEmpty()
                                        && enrollmentSpan.getFirstInactiveEnrollment().getEnrollmentDate()
                                        .equals(csdList.get(0).getDate()))))) {
                            isEnrolled = false;
                        } else {
                            // Ok, the date belongs to this span, go to next date.
                            break;
                        }
                    }
                }
                if (isEnrolled) {
                    datesToMap.add(date);
                }
            }
            return datesToMap;
        }

        /**
         * @param broker
         * @param b
         * @return
         */
        public Collection<TnStudentEnrollmentSpan> getTnStudentEnrollmentSpans(X2Broker broker) {
            if (m_enrollmentSpans == null) {
                Collection<String> preferenceStudentActiveStatuses =
                        (Collection<String>) ToolBean.getPreference(PREFERENCE_ACTIVE_STUDENT_CODES);
                boolean yogBreak = (Boolean) ToolBean.getPreference(PREFERENCE_SPAN_BREAK_ON_YOG);
                boolean statusBreak = (Boolean) ToolBean.getPreference(PREFERENCE_SPAN_BREAK_ON_STATUS);

                // Get the list of student enrollment records.
                List<ToolEnrollment> enrollments = getEnrollments(broker);

                // Determine starting status (current status). This should be based on the latest
                // enrollment record if possible, or the student otherwise.
                // isOpen indicates the current enrollment status is active.
                String enrollStatus = null;
                if (enrollments != null && enrollments.size() > 0) {
                    ToolEnrollment currentEnrollment = enrollments.iterator().next();
                    enrollStatus = currentEnrollment.getStatusCode();
                }
                if (StringUtils.isEmpty(enrollStatus)) {
                    enrollStatus = getEnrollmentStatus();
                }
                // Current status of student.
                boolean isActive = preferenceStudentActiveStatuses.contains(enrollStatus);
                // If the enrollment span has a non-withdrawal record in it so far.
                // Used to determine if another withdrawal signifies a break in the span.
                boolean hasNonWithdrawals = isActive;

                // Work through enrollment records going backward in time and build spans.
                // This will build all spans, regardless of the setting of limit.
                m_enrollmentSpans = new ArrayList<TnStudentEnrollmentSpan>();

                List<TnEnrollment> currentEnrollments = new ArrayList<TnEnrollment>();
                if (enrollments != null) {
                    /*
                     * Since we need to look ahead in the list of enrollments to combine Y and E
                     * records
                     * on the same date,
                     * we need to create an array list and navigate using an index.
                     */
                    enrollments = new ArrayList(enrollments);
                    for (int index = 0; index < enrollments.size(); ++index) {
                        TnEnrollment enrollment = (TnEnrollment) enrollments.get(index);
                        if (StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType())) {
                            // Only report a YOG as a break if the student is active or there are
                            // other
                            // records in the span already. Not for inactive students between spans.
                            isActive = preferenceStudentActiveStatuses.contains(enrollment.getStatusCode());
                            if (yogBreak && (isActive || currentEnrollments.size() > 0)) {
                                // Complete the previous span. Start a new one.
                                currentEnrollments.add(0, enrollment);
                                m_enrollmentSpans.add(0, new TnStudentEnrollmentSpan(broker, currentEnrollments));
                                /*
                                 * check to see if next span should be combined. This will occur if
                                 * the
                                 * next span is an E
                                 * record on the same date at the same school.
                                 */
                                if (index + 1 < enrollments.size()) {
                                    TnEnrollment nextEnrollment = (TnEnrollment) enrollments.get(index + 1);
                                    if (yogEliminate(broker, enrollment, nextEnrollment)) {
                                        // remove this span - the YOG_CHANGE will be included in the
                                        // next span
                                        m_enrollmentSpans.remove(0);
                                        currentEnrollments.remove(0);
                                    } else {
                                        currentEnrollments = new ArrayList<TnEnrollment>();
                                    }
                                } else {
                                    currentEnrollments = new ArrayList<TnEnrollment>();
                                }
                            }
                            currentEnrollments.add(0, enrollment);
                            hasNonWithdrawals = true;
                        } else if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) {
                            // Only report a STATUS as a break if the student is active or there are
                            // other
                            // records in the span already. Not for inactive students between spans.
                            isActive = preferenceStudentActiveStatuses.contains(enrollment.getStatusCode());
                            if (statusBreak && (isActive || currentEnrollments.size() > 0)) {
                                // Complete the previous span. Start a new one.
                                currentEnrollments.add(0, enrollment);
                                m_enrollmentSpans.add(0, new TnStudentEnrollmentSpan(broker, currentEnrollments));
                                currentEnrollments = new ArrayList<TnEnrollment>();
                            }
                            currentEnrollments.add(0, enrollment);
                            hasNonWithdrawals = true;
                        } else if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                            if (hasNonWithdrawals && !currentEnrollments.isEmpty()) {
                                m_enrollmentSpans.add(0, new TnStudentEnrollmentSpan(broker, currentEnrollments));
                                currentEnrollments = new ArrayList<TnEnrollment>();
                            }

                            isActive = preferenceStudentActiveStatuses.contains(enrollment.getStatusCode());
                            currentEnrollments.add(0, enrollment);
                            hasNonWithdrawals = false;
                        } else if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                            currentEnrollments.add(0, enrollment);
                            isActive = preferenceStudentActiveStatuses.contains(enrollment.getStatusCode());
                            hasNonWithdrawals = true;
                        }
                    }
                    if (hasNonWithdrawals && !currentEnrollments.isEmpty()) {
                        m_enrollmentSpans.add(0, new TnStudentEnrollmentSpan(broker, currentEnrollments));
                        currentEnrollments = new ArrayList<TnEnrollment>();
                    }
                }

                // remove spans without an active interval
                Iterator<TnStudentEnrollmentSpan> iterator = m_enrollmentSpans.iterator();
                while (iterator.hasNext()) {
                    TnStudentEnrollmentSpan span = iterator.next();
                    if (span.getFirstActiveEnrollment() == null) {
                        iterator.remove();
                    }
                }
            }
            return m_enrollmentSpans;
        }

    }

    public static class TnStudentAttendance extends ToolStudentAttendance {

        public static final ToolBeanColumn FIELD_OTHER_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.otherCode());
        public static final ToolBeanColumn FIELD_OTHER_CODE_02 =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.otherCode02());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudentAttendance.FULL_DEFINITION.expand(
                FIELD_OTHER_CODE,
                FIELD_OTHER_CODE_02);

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public TnStudentAttendance(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the other code.
         *
         * @return boolean
         */
        public String getOtherCode() {
            return getValueString(FIELD_OTHER_CODE);
        }

        /**
         * Gets the other code 02.
         *
         * @return boolean
         */
        public String getOtherCode02() {
            return getValueString(FIELD_OTHER_CODE_02);
        }

    }

    public static class TnStudentEnrollmentSpan {

        /*
         * Instance variables
         */
        X2Broker m_broker;
        List<TnEnrollment> m_enrollments;
        PlainDate m_firstActiveDate;
        TnEnrollment m_firstActiveEnrollment;
        TnEnrollment m_firstEntry;
        TnEnrollment m_firstInactiveEnrollment;
        TnEnrollment m_firstWithdrawal;
        PlainDate m_lastActiveDate;
        Integer m_membershipDays;
        boolean m_preferenceMemberOnEntry;
        boolean m_preferenceMemberOnWithdrawal;
        TnSchool m_school;
        TnStudent m_student;
        Collection<String> m_studentActiveStatuses;
        List<TnStudentAttendance> m_studentAttendance;

        /**
         * Constructor:
         * Find other values from the enrollment list.
         *
         * @param enrollments List<>
         * @param helper TNStudentHistoryHelper
         */
        TnStudentEnrollmentSpan(X2Broker broker, List<TnEnrollment> enrollments) {
            m_broker = broker;
            Organization organization = ToolBean.DistrictManager.getOrganization(broker);
            DistrictSchoolYearContext context =
                    (DistrictSchoolYearContext) ToolBean.getPreference(PREFERENCE_CURRENT_CONTEXT);

            PlainDate districtBeginDate = context.getStartDate();

            m_studentActiveStatuses = StudentManager.getActiveStudentCodeList(organization);
            m_preferenceMemberOnEntry = Boolean.valueOf(PreferenceManager.getPreferenceValue(organization,
                    SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();
            m_preferenceMemberOnWithdrawal = Boolean.valueOf(PreferenceManager.getPreferenceValue(organization,
                    SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

            boolean splitYog = (Boolean) ToolBean.getPreference(PREFERENCE_SPAN_BREAK_ON_YOG);
            boolean splitStatus = (Boolean) ToolBean.getPreference(PREFERENCE_SPAN_BREAK_ON_STATUS);


            boolean isActive = false;
            boolean isInactiveAgain = false;
            m_enrollments = enrollments;
            for (TnEnrollment enrollment : enrollments) {
                // Get student and school from the enrollment record.
                if (m_student == null) {
                    m_student = (TnStudent) enrollment.getStudent(broker);
                }
                if (m_school == null) {
                    m_school = (TnSchool) enrollment.getSchool(broker);
                }

                if (!isActive) {
                    // Active code, or sometimes empty status and a non-withdrawal record.
                    String statusCode = enrollment.getStatusCode();
                    if (m_studentActiveStatuses.contains(statusCode) ||
                            (StringUtils.isEmpty(statusCode) &&
                                    !StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()))) {
                        isActive = true;
                        m_firstActiveEnrollment = enrollment;
                        if (m_preferenceMemberOnEntry) {
                            m_firstActiveDate = enrollment.getEnrollmentDate();
                            if (m_firstActiveDate.before(districtBeginDate)) {
                                m_firstActiveDate = districtBeginDate;
                            }
                        } else {
                            // Lookup next in-session date for the school.
                            m_firstActiveDate = findSessionDate(broker, enrollment, true);
                        }
                    }
                } else if (!isInactiveAgain) {
                    String statusCode = enrollment.getStatusCode();
                    if (!m_studentActiveStatuses.contains(statusCode)) {
                        isInactiveAgain = true;
                        m_firstInactiveEnrollment = enrollment;
                        if (m_preferenceMemberOnWithdrawal) {
                            m_lastActiveDate = enrollment.getEnrollmentDate();
                        } else {
                            // Lookup previous in-session date for the school.
                            m_lastActiveDate = findSessionDate(broker, enrollment, false);
                        }
                    }
                }

                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) &&
                        m_firstEntry == null) {
                    m_firstEntry = enrollment;
                }
                if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()) &&
                        m_firstWithdrawal == null) {
                    m_firstWithdrawal = enrollment;
                }
            }

            // If no end-of-enrollment records was found (in case of YOG, STATUS) determine if
            // the last record should be treated as the exit record.
            if ((m_lastActiveDate == null || m_firstInactiveEnrollment == null) &&
                    m_enrollments.size() > 1) {
                TnEnrollment enrollment = m_enrollments.get(m_enrollments.size() - 1);

                if ((splitYog && StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType())
                        && !yogEliminate(m_broker, enrollment, m_enrollments.get(m_enrollments.size() - 2))) ||
                        (splitStatus && StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) ||
                        StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                    m_firstInactiveEnrollment = enrollment;
                    if (m_preferenceMemberOnWithdrawal) {
                        m_lastActiveDate = enrollment.getEnrollmentDate();
                    } else {
                        m_lastActiveDate = findSessionDate(broker, enrollment, false);
                    }
                }
            }
        }

        /**
         * Constructor to create copy of span.
         *
         * @param span TNStudentEnrollmentSpan
         */
        private TnStudentEnrollmentSpan(TnStudentEnrollmentSpan span) {
            this.m_enrollments = span.m_enrollments;
            this.m_firstActiveDate = span.m_firstActiveDate;
            this.m_firstActiveEnrollment = span.m_firstActiveEnrollment;
            this.m_firstEntry = span.m_firstEntry;
            this.m_firstInactiveEnrollment = span.m_firstInactiveEnrollment;
            this.m_firstWithdrawal = span.m_firstWithdrawal;
            this.m_broker = span.m_broker;
            this.m_lastActiveDate = span.m_lastActiveDate;
            this.m_membershipDays = span.m_membershipDays;
            this.m_preferenceMemberOnEntry = span.m_preferenceMemberOnEntry;
            this.m_preferenceMemberOnWithdrawal = span.m_preferenceMemberOnWithdrawal;
            this.m_studentActiveStatuses = span.m_studentActiveStatuses;
            this.m_school = span.m_school;
            this.m_student = span.m_student;
            this.m_studentAttendance = span.m_studentAttendance;
        }

        /**
         * Find the nearest in session date to the date provided.
         * <br>
         * parameter "after" indicates if the nearest date should be
         * after the date provided. Otherwise, the nearest in session date
         * before the date provided is returned.
         *
         * @param enrollmentDate PlainDate
         * @param after boolean
         * @return PlainDate
         */
        private PlainDate findSessionDate(X2Broker broker, TnEnrollment enrollment, boolean after) {
            PlainDate nearestDate = null;
            DistrictSchoolYearContext x2Context =
                    (DistrictSchoolYearContext) ToolBean.getPreference(PREFERENCE_CURRENT_CONTEXT);
            ToolDistrictContext context = ToolBean.getBeanByOid(broker, ToolDistrictContext.class, x2Context.getOid());
            /*
             * TODO: IMPORTANT
             * Before using with any context sensitive tool the use of the context helper must be
             * supported
             * String studentCalendar =
             * (String) getStudentValueByBeanPath(m_student, SisStudent.COL_CALENDAR_CODE);
             */

            ToolSchoolCalendar calendarToUse = ToolSchoolCalendar.findBestCalendar(broker, m_student,
                    Arrays.asList(m_school), enrollment, context);

            PlainDate enrollmentDate = enrollment.getEnrollmentDate();
            Calendar cal = Calendar.getInstance();
            cal.setTime(enrollmentDate);
            cal.add(Calendar.DATE, after ? 1 : -1);
            // enrollment date is not included on either side of the match
            nearestDate = calendarToUse != null
                    ? calendarToUse.findFirstInSessionDate(m_broker, new PlainDate(cal.getTime()), after)
                            : null;
            if (nearestDate == null) {
                if (after) {
                    nearestDate = enrollmentDate;
                } else {
                    cal.setTime(enrollmentDate);
                    cal.add(Calendar.DATE, -1);
                    nearestDate = new PlainDate(cal.getTime());
                }
            }
            return nearestDate;
        }

        /**
         * Returns copy of this span.
         *
         * @return TNStudentEnrollmentSpan
         */
        public TnStudentEnrollmentSpan getCopy() {
            return new TnStudentEnrollmentSpan(this);
        }

        /**
         * Return the first active date for the student in the enrollment span.
         * <p>
         * This finds the first enrollment record in the span that indicates
         * the student is active. It then checks the system preference for membership
         * on entry date to determine if this is the first active date or looks up
         * the next in session date as the first active date.
         * <p>
         * This value will be adjusted to fit the current school year. It is not
         * representative of reportable enrollment dates. First and last active dates
         * are most useful for counting membership and attendance days for an
         * enrollment span.
         *
         * @return PlainDate
         */
        public PlainDate getFirstActiveDate() {
            return m_firstActiveDate;
        }

        /**
         * Return the first enrollment record to indicate active status.
         * This is usually considered to be the entry record for the enrollment span.
         *
         * @return TnEnrollment
         */
        public TnEnrollment getFirstActiveEnrollment() {
            return m_firstActiveEnrollment;
        }

        /**
         * Return the first entry enrollment record to indicate active status.
         *
         * @return TnEnrollment
         */
        public TnEnrollment getFirstEntryEnrollment() {
            return m_firstEntry;
        }

        /**
         * Return the first enrollment record to indicate inactive status after having been active
         * in this span.
         * This is usually considered to be the exit record for the enrollment span.
         *
         * @return TnEnrollment
         */
        public TnEnrollment getFirstInactiveEnrollment() {
            return m_firstInactiveEnrollment;
        }

        /**
         * Return the most recent enrollment record of the specified types that exists on or before
         * the specified date.
         *
         * @param date as of date to find enrollment records for.
         * @param types a String that includes a combination of the four TnEnrollment type
         *        constants ('E','W','S','Y').
         *        <br>
         *        EX: "ES" to search for only Entry or Status Change records.
         *
         * @return TnEnrollment
         */
        public TnEnrollment getEnrollmentForDate(PlainDate date, String types) {
            TnEnrollment lastEnrollment = null;
            for (TnEnrollment enrollment : m_enrollments) {
                if (!enrollment.getEnrollmentDate().after(date)) {
                    if (types.contains(enrollment.getEnrollmentType())) {
                        lastEnrollment = enrollment;
                    }
                }
            }
            return lastEnrollment;
        }

        /**
         * Return the list of enrollments in this span.
         *
         * @return List<TnEnrollment>
         */
        public List<TnEnrollment> getEnrollments() {
            return m_enrollments;
        }

        /**
         * Return the last active date for the student in the enrollment span.
         * <p>
         * This finds the first enrollment record in the span that indicates
         * the student is inactive after having been active previously. It then
         * checks the system preference for membership on withdrawal date to
         * determine if this is the last active date or looks up the previous
         * in session date as the last active date.
         * <p>
         * This value will be adjusted to fit the current school year. It is not
         * representative of reportable enrollment dates. First and last active dates
         * are most useful for counting membership and attendance days for an
         * enrollment span.
         *
         * @return PlainDate
         */
        public PlainDate getLastActiveDate() {
            return m_lastActiveDate;
        }

        /**
         * Return the school for the enrollment span.
         *
         * @return School
         */
        public TnSchool getSchool() {
            return m_school;
        }

        /**
         * Gets the yog.
         *
         * @return int
         */
        /*
         * get the yog from the first active enrollment, unless there exists a YOG_CHANGE record
         * immediately following the first active enrollment on the same date for the same school.
         * In this case, use the yog from the YOG_CHANGE record.
         */
        public int getYog() {
            int value = 0;
            if (m_firstActiveEnrollment != null) {
                value = m_firstActiveEnrollment.getYog();
                Iterator<TnEnrollment> enrollments = m_enrollments.iterator();
                while (enrollments.hasNext()) {
                    TnEnrollment enrollment = enrollments.next();
                    if (enrollment == m_firstActiveEnrollment) {
                        if (enrollments.hasNext()) {
                            TnEnrollment nextEnrollment = enrollments.next();
                            if (yogEliminate(m_broker, nextEnrollment, enrollment)) {
                                value = nextEnrollment.getYog();
                            }
                        }
                        break;
                    }
                }
            }
            return value;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder value = new StringBuilder();

            value.append("School:");
            value.append(m_school == null ? m_school : m_school.getName());
            value.append(";");

            value.append("First Active Date:");
            value.append(m_firstActiveDate);
            value.append(";");

            value.append("Last Active Date:");
            value.append(m_lastActiveDate);
            value.append(";");

            value.append("First Active:{");
            if (m_firstActiveEnrollment == null) {
                value.append("null");
            } else {
                value.append("Date:");
                value.append(m_firstActiveEnrollment.getEnrollmentDate());
                value.append(";");
                value.append("Type:");
                value.append(m_firstActiveEnrollment.getEnrollmentType());
                value.append(";");
                value.append("Code:");
                value.append(m_firstActiveEnrollment.getEnrollmentCode());
                value.append(";");
                value.append("Yog:");
                value.append(m_firstActiveEnrollment.getYog());
                value.append(";");
            }
            value.append("}");

            value.append("First Inactive:{");
            if (m_firstInactiveEnrollment == null) {
                value.append("null");
            } else {
                value.append("Date:");
                value.append(m_firstInactiveEnrollment.getEnrollmentDate());
                value.append(";");
                value.append("Type:");
                value.append(m_firstInactiveEnrollment.getEnrollmentType());
                value.append(";");
                value.append("Code:");
                value.append(m_firstInactiveEnrollment.getEnrollmentCode());
                value.append(";");
                value.append("Yog:");
                value.append(m_firstInactiveEnrollment.getYog());
                value.append(";");
            }
            value.append("}");

            value.append("Withdrawal:{");
            if (m_firstWithdrawal == null) {
                value.append("null");
            } else {
                value.append("Date:");
                value.append(m_firstWithdrawal.getEnrollmentDate());
                value.append(";");
                value.append("Type:");
                value.append(m_firstWithdrawal.getEnrollmentType());
                value.append(";");
                value.append("Code:");
                value.append(m_firstWithdrawal.getEnrollmentCode());
                value.append(";");
                value.append("Yog:");
                value.append(m_firstActiveEnrollment.getYog());
                value.append(";");
            }
            value.append("}");
            return value.toString();
        }

        /**
         * Sets the first active enrollment.
         *
         * @param enrollment void
         */
        public void setFirstActiveEnrollment(TnEnrollment enrollment) {
            m_firstActiveEnrollment = enrollment;
        }
    }

    public static final String PREFERENCE_ACTIVE_STUDENT_CODES = "activeStudentCodes";
    public static final String PREFERENCE_CURRENT_CONTEXT = "currentDistrictContext";
    public static final String PREFERENCE_SPAN_BREAK_ON_STATUS = "spanBreakOnStatus";
    public static final String PREFERENCE_SPAN_BREAK_ON_YOG = "spanBreakOnYog";

    /**
     * Yog eliminate.
     *
     * @param yogEnrollment StudentEnrollment
     * @param previousEnrollment StudentEnrollment
     * @return true, if successful
     */
    static boolean yogEliminate(X2Broker broker, TnEnrollment yogEnrollment, TnEnrollment previousEnrollment) {
        return (StudentEnrollment.YOG_CHANGE.equals(yogEnrollment.getEnrollmentType()) &&
                StudentEnrollment.ENTRY.equals(previousEnrollment.getEnrollmentType()) &&
                (yogEnrollment.getEnrollmentDate() != null && previousEnrollment.getEnrollmentDate() != null
                && yogEnrollment.getEnrollmentDate().equals(previousEnrollment.getEnrollmentDate()))
                &&
                (yogEnrollment.getSchool(broker) != null && previousEnrollment.getSchool(broker) != null
                && yogEnrollment.getSchool(broker).equals(previousEnrollment.getSchool(broker))));
    }


}
