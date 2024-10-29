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
package com.x2dev.procedures.statereporting.ct;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Connecticut state report for SASID export.
 * This class implements the data export for CT SASID export.
 *
 * @author X2 Development Corporation
 */
public class SasidRegister extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the SASID export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class SasidRegisterEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        SasidRegister m_registerData = null;
        List<MembershipAttendance> m_schoolList = new ArrayList<MembershipAttendance>();
        SisPerson m_studentPerson = null;
        SisStudent m_student = null;

        /**
         * Instantiates a new sasid register entity.
         */
        /*
         * Public no argument constructor for dynamic instantiation.
         */
        public SasidRegisterEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Check enrollment membership count and membership days parameter
         * to determine if the student should be reported.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            if (m_registerData.m_requireFacility != null && m_registerData.m_requireFacility.booleanValue()) {
                // Check the student outplaced or sped school. If there is no state code then
                // exclude the student.
                SisStudent student = (SisStudent) getBean();
                SisSchool school = student.getSchool();
                String temp = (String) school.getFieldValueByAlias(OUTSIDE_PLACEMENT_SCHOOL_ALIAS);
                if ("1".equalsIgnoreCase(temp)) {
                    String schoolId = (String) student.getFieldValueByAlias(OUTSIDE_PLACEMENT_FACILITY_ALIAS);
                    if (m_registerData.m_outplacementFacilities.containsKey(schoolId)) {
                        ReferenceCode refCode = m_registerData.m_outplacementFacilities.get(schoolId);
                        schoolId = refCode.getStateCode();
                        if (StringUtils.isEmpty(schoolId)) {
                            error = new StateReportValidationError(this,
                                    getData().getFieldDefinition(SASID_6_FACILITY_1),
                                    "Outplaced Facility has no state code", refCode.getCode());
                        }
                    } else {
                        error = new StateReportValidationError(this, getData().getFieldDefinition(SASID_6_FACILITY_1),
                                "Outplaced Facility has no state code", schoolId);
                    }
                }

                temp = (String) school.getFieldValueByAlias(SPECIAL_EDUCATION_SCHOOL_ALIAS);
                if ("1".equalsIgnoreCase(temp)) {
                    String schoolId = (String) student.getFieldValueByAlias(SPECIAL_EDUCATION_FACILITY_ALIAS);
                    if (m_registerData.m_specialEducationFacilities.containsKey(schoolId)) {
                        ReferenceCode refCode = m_registerData.m_specialEducationFacilities.get(schoolId);
                        schoolId = refCode.getStateCode();
                        if (StringUtils.isEmpty(schoolId)) {
                            error = new StateReportValidationError(this,
                                    getData().getFieldDefinition(SASID_6_FACILITY_1),
                                    "Special Ed Facility has no state code", refCode.getCode());
                        }
                    } else {
                        error = new StateReportValidationError(this, getData().getFieldDefinition(SASID_6_FACILITY_1),
                                "Special Ed Facility has no state code", schoolId);
                    }
                }
            }

            // Check the "Require membership days" option and exclude if the student has no
            // membership days.
            if (m_registerData.m_requireMemberDay != null && m_registerData.m_requireMemberDay.booleanValue()) {
                // Get membership days parameter.
                int membershipCountAsInt = 0;

                // Get enrollment count (SASID 25). (Force calculation of membership days and
                // storage of count).
                String membershipCount = getFieldValue(SASID_25_MEMBERSHIP_DAYS);
                if (membershipCount != null) {
                    try {
                        membershipCountAsInt = Integer.parseInt(membershipCount);
                    } catch (NumberFormatException nfe) {
                        // invalid format, will be reported elsewhere.
                    }
                }

                // check enrollment count and membership days parameter.
                if (membershipCountAsInt == 0) {
                    // Student filtered.
                    error = new StateReportValidationError(this, getData().getFieldDefinition(SASID_25_MEMBERSHIP_DAYS),
                            "0 member days - excluded from export", "");
                }
            }

            return error;
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

            return name;
        }

        /**
         * Returns the MembershipAttendance record for the current index.
         *
         * @return MembershipAttendance
         */
        public MembershipAttendance getMembershipAttendance() {
            return m_schoolList.get(getCurrentRow());
        }

        /**
         * Gets the student person.
         *
         * @return Sis person
         */
        public SisPerson getStudentPerson() {
            return m_studentPerson;
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public SisStudent getStudent() {
            return m_student;
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

            m_registerData = (SasidRegister) data;
            /*
             * Determine all changes for the student in the reporting period.
             * Take a snapshot of the student status at the beginning and the end and
             * compare to find all reportable changes in status.
             */
            m_student = (SisStudent) bean;
            m_studentPerson = m_student.getPerson();

            // Find the last enrollment record before each date.
            int innerEnrollmentRecords = 0;
            StudentEnrollment beginEnrollment = null;
            StudentEnrollment endEnrollment = null;
            Collection<StudentEnrollment> enrollments = m_registerData.m_studentEnrollements.get(m_student.getOid());
            if (enrollments != null) {
                for (StudentEnrollment enrollment : enrollments) {
                    if (m_registerData.m_priorReportDate.after(enrollment.getEnrollmentDate()) ||
                            m_registerData.m_priorReportDate.equals(enrollment.getEnrollmentDate())) {
                        beginEnrollment = enrollment;
                    }
                    if (m_registerData.m_reportDate.after(enrollment.getEnrollmentDate()) ||
                            m_registerData.m_reportDate.equals(enrollment.getEnrollmentDate())) {
                        endEnrollment = enrollment;
                    }
                    if ((m_registerData.m_reportDate.after(enrollment.getEnrollmentDate()) ||
                            m_registerData.m_reportDate.equals(enrollment.getEnrollmentDate())) &&
                            m_registerData.m_priorReportDate.before(enrollment.getEnrollmentDate())) {
                        innerEnrollmentRecords++;
                    }
                }
            }
            /*
             * Look for differences between the records. Check for:
             * School.
             * YOG/Grade change.
             * Status change.
             * enrollment (entry or withdrawal).
             */
            SisSchool school = null;
            PlainDate entryDate = null;
            PlainDate exitDate = null;
            String exitType = null;
            String exitStatus = null;
            if (endEnrollment == null && beginEnrollment == null) {
                // No report. Student enrolled after the end date.
            } else if (endEnrollment != null && endEnrollment.equals(beginEnrollment)) {
                // Same enrollment record. Check for grade level change for EOY rollover.
                if (StudentManager.isActiveStudent(getData().getOrganization(), endEnrollment.getStatusCode())) {
                    String beginGradeLevel = getGradeLevelForStudent(m_student, endEnrollment.getYog(),
                            m_registerData.m_priorReportDate);
                    String endGradeLevel =
                            getGradeLevelForStudent(m_student, endEnrollment.getYog(), m_registerData.m_reportDate);
                    if (beginGradeLevel != null && !beginGradeLevel.equals(endGradeLevel)) {
                        // Assume school year change and use district school year dates.
                        DistrictSchoolYearContext beginContext = getSchoolYearForDate(m_registerData.m_priorReportDate);
                        DistrictSchoolYearContext endContext = getSchoolYearForDate(m_registerData.m_reportDate);
                        if (!beginContext.equals(endContext)) {
                            entryDate = endContext.getStartDate();
                            exitDate = beginContext.getEndDate();
                        } else {
                            entryDate = endContext.getStartDate();
                            exitDate = endContext.getStartDate();
                        }
                        MembershipAttendance membership =
                                new MembershipAttendance(SASID_TYPE_C, endEnrollment.getSchool(),
                                        entryDate, exitDate, exitType, null);
                        membership.setGradeLevel(endGradeLevel);
                        membership.setOldSchool(endEnrollment.getSchool());
                        calculateAttendance(membership);
                        m_schoolList.add(membership);
                    }
                }
            } else if (endEnrollment != null &&
                    StudentManager.isActiveStudent(getData().getOrganization(), endEnrollment.getStatusCode()) &&
                    (beginEnrollment == null ||
                            !StudentManager.isActiveStudent(getData().getOrganization(),
                                    beginEnrollment.getStatusCode())))

            {
                // Report if the end status is active and
                // the begin status does not exist or is not active,
                school = endEnrollment.getSchool();

                // Find the first enrollment E record, or failing that the first enrollment record.
                StudentEnrollment checkEnrollment = null;
                for (StudentEnrollment enrollment : enrollments) {
                    if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) &&
                            enrollment.getSchoolOid().equals(endEnrollment.getSchoolOid())) {
                        checkEnrollment = enrollment;
                    }
                }
                if (checkEnrollment == null) {
                    checkEnrollment = enrollments.iterator().next();
                }
                if (checkEnrollment != null) {
                    entryDate = checkEnrollment.getEnrollmentDate();
                } else {
                    entryDate = endEnrollment.getEnrollmentDate();
                }

                // Get grade level as of the last enrollment record and YOG.
                String gradeLevel =
                        getGradeLevelForStudent(m_student, endEnrollment.getYog(), endEnrollment.getEnrollmentDate());

                MembershipAttendance membership = new MembershipAttendance(SASID_TYPE_R, school,
                        entryDate, null, null, null);
                membership.setGradeLevel(gradeLevel);
                m_schoolList.add(membership);
            } else if (endEnrollment != null && beginEnrollment != null) {
                if (StudentManager.isActiveStudent(getData().getOrganization(), beginEnrollment.getStatusCode()) &&
                        StudentManager.isActiveStudent(getData().getOrganization(), endEnrollment.getStatusCode())) {
                    // both ends active, check for a change of school or grade.

                    boolean gradeChange = false;
                    boolean schoolChange = false;
                    if (!beginEnrollment.getSchoolOid().equals(endEnrollment.getSchoolOid())) {
                        schoolChange = true;
                        school = endEnrollment.getSchool();
                        exitType = "01"; // transfer in district
                    }

                    String beginGradeLevel = getGradeLevelForStudent(m_student, beginEnrollment.getYog(),
                            m_registerData.m_priorReportDate);
                    String endGradeLevel =
                            getGradeLevelForStudent(m_student, endEnrollment.getYog(), m_registerData.m_reportDate);
                    if (endGradeLevel != null && !endGradeLevel.equals(beginGradeLevel)) {
                        gradeChange = true;
                    }

                    if (schoolChange || gradeChange) {
                        // For a school change, find enrollment records representing the school exit
                        // and entry to get dates.
                        SisSchool oldSchool = null;
                        if (schoolChange) {
                            StudentEnrollment exitEnrollment = null;
                            StudentEnrollment entryEnrollment = null;
                            // Find Last withdrawal for the old school before report date.
                            // The withdrawal may be before the prior report date.
                            for (StudentEnrollment enrollment : enrollments) {
                                if ((m_registerData.m_reportDate.after(enrollment.getEnrollmentDate()) ||
                                        m_registerData.m_reportDate.equals(enrollment.getEnrollmentDate())) &&
                                        StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()) &&
                                        beginEnrollment.getSchoolOid().equals(enrollment.getSchoolOid())) {
                                    exitEnrollment = enrollment;
                                }
                            }
                            if (exitEnrollment != null) {
                                exitDate = exitEnrollment.getEnrollmentDate();
                                oldSchool = exitEnrollment.getSchool();
                            }
                            // Find the first entry for the new school after the withdrawal date
                            // and after prior date.
                            for (StudentEnrollment enrollment : enrollments) {
                                if (m_registerData.m_priorReportDate.before(enrollment.getEnrollmentDate()) &&
                                        StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) &&
                                        endEnrollment.getSchoolOid().equals(enrollment.getSchoolOid())) {
                                    entryEnrollment = enrollment;
                                    break;
                                }
                            }
                            if (entryEnrollment != null) {
                                entryDate = entryEnrollment.getEnrollmentDate();
                            }
                        } else {
                            // a grade change, look for a YOG record. Get dates from that.
                            StudentEnrollment yogEnrollment = null;
                            for (StudentEnrollment enrollment : enrollments) {
                                if ((m_registerData.m_reportDate.after(enrollment.getEnrollmentDate()) ||
                                        m_registerData.m_reportDate.after(enrollment.getEnrollmentDate())) &&
                                        m_registerData.m_priorReportDate.before(enrollment.getEnrollmentDate()) &&
                                        StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType())) {
                                    yogEnrollment = enrollment;
                                }
                            }
                            if (yogEnrollment != null) {
                                entryDate = yogEnrollment.getEnrollmentDate();
                                Calendar cal = Calendar.getInstance();
                                cal.setTime(entryDate);
                                cal.add(Calendar.DAY_OF_YEAR, -1);
                                exitDate = new PlainDate(cal.getTimeInMillis());
                            } else {
                                // If none, assume school year change and use district school year
                                // dates.
                                DistrictSchoolYearContext beginContext =
                                        getSchoolYearForDate(m_registerData.m_priorReportDate);
                                DistrictSchoolYearContext endContext =
                                        getSchoolYearForDate(m_registerData.m_reportDate);
                                if (!beginContext.equals(endContext)) {
                                    entryDate = endContext.getStartDate();
                                    exitDate = beginContext.getEndDate();
                                }
                            }
                            oldSchool = endEnrollment.getSchool();
                        }

                        MembershipAttendance membership =
                                new MembershipAttendance(SASID_TYPE_C, endEnrollment.getSchool(),
                                        entryDate, exitDate, exitType, null);
                        membership.setGradeLevel(endGradeLevel);
                        membership.setOldSchool(oldSchool);
                        calculateAttendance(membership);
                        m_schoolList.add(membership);
                    }
                } else if (StudentManager.isActiveStudent(getData().getOrganization(), beginEnrollment.getStatusCode())
                        &&
                        !StudentManager.isActiveStudent(getData().getOrganization(), endEnrollment.getStatusCode())) {
                    // An unregister. Don't need much, just the date. Find the last withdrawal
                    // record before report date.
                    StudentEnrollment exitEnrollment = null;
                    // Find Last withdrawal before report date.
                    for (StudentEnrollment enrollment : enrollments) {
                        if ((m_registerData.m_reportDate.after(enrollment.getEnrollmentDate()) ||
                                m_registerData.m_reportDate.equals(enrollment.getEnrollmentDate())) &&
                                StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                            exitEnrollment = enrollment;
                        }
                    }
                    if (exitEnrollment != null) {
                        String exitTypeCode = exitEnrollment.getEnrollmentCode();
                        ReferenceCode exitRefCode = m_registerData.m_withdrawalCodes.get(exitTypeCode);
                        if (exitRefCode != null) {
                            exitType = exitRefCode.getStateCode();
                        }
                        String exitStatusCode = exitEnrollment.getReasonCode();
                        exitRefCode = m_registerData.m_withdrawalReasons.get(exitStatusCode);
                        if (exitRefCode != null) {
                            exitStatus = exitRefCode.getStateCode();
                        }

                        MembershipAttendance membership =
                                new MembershipAttendance(SASID_TYPE_U, exitEnrollment.getSchool(),
                                        null, exitEnrollment.getEnrollmentDate(), exitType, exitStatus);
                        membership.setOldSchool(exitEnrollment.getSchool());
                        calculateAttendance(membership);
                        m_schoolList.add(membership);
                    }
                }
            }

            // If there are enrollment records in the gap, but no change reported,
            // print a warning in the validation report.
            if (innerEnrollmentRecords > 0 && m_schoolList.size() == 0) {
                data.addSetupError(toString(), "Enrollment records exist when no event reported.");
            }

            /*
             * If the user selected only one type of record, filter the
             * list down to that type.
             */
            if (!StringUtils.isEmpty(m_registerData.m_recordType)) {
                Iterator<MembershipAttendance> iterator = m_schoolList.iterator();
                while (iterator.hasNext()) {
                    MembershipAttendance membership = iterator.next();
                    if (!m_registerData.m_recordType.equals(membership.getType())) {
                        iterator.remove();
                    }
                }
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
         * Calculate the membership and attendance for a membership record.
         * Must determine the appropriate school year to report.
         * It may be the prior year for late summer withdrawals.
         *
         * @param membership MembershipAttendance
         */
        private void calculateAttendance(MembershipAttendance membership) {
            SisSchool oldSchool = membership.getOldSchool();
            SisStudent student = (SisStudent) getBean();
            PlainDate lastDate = membership.getExitDate();
            if (lastDate != null) {
                // If the last date is before the start of school, find the previous school year
                // context (for summer withdrawals and changes).
                DistrictSchoolYearContext attContext = getSchoolYearForDate(membership.getExitDate());
                if (attContext != null) {
                    Collection<SchoolCalendar> calendars = oldSchool.getSchoolCalendars();
                    for (SchoolCalendar calendar : calendars) {
                        if (calendar.getDistrictContextOid().equals(attContext.getOid()) &&
                                calendar.getCalendarId().equals(student.getCalendarCode())) {
                            PlainDate minDate = null;
                            for (SchoolCalendarDate calDate : calendar.getSchoolCalendarDates(getData().getBroker())) {
                                if (calDate.getInSessionIndicator()) {
                                    if (minDate == null || minDate.after(calDate.getDate())) {
                                        minDate = calDate.getDate();
                                    }
                                }
                            }
                            if (minDate != null && minDate.after(lastDate)) {
                                int newYear = attContext.getSchoolYear() - 1;
                                for (DistrictSchoolYearContext context : m_registerData.m_contexts) {
                                    if (context.getSchoolYear() == newYear) {
                                        attContext = context;
                                        break;
                                    }
                                }
                            }
                            break;
                        }
                    }

                    // Look up membership and attendance counts in the specified school year.
                    if (attContext != null && lastDate != null && membership.getExitDate() != null) {
                        PlainDate beginDate = attContext.getStartDate();
                        Collection<StudentEnrollment> enrollments =
                                m_registerData.m_studentEnrollements.get(m_student.getOid());
                        StudentEnrollment lastEntryEnrollment = null;
                        for (StudentEnrollment enrollment : enrollments) {
                            if ((membership.getExitDate().after(enrollment.getEnrollmentDate()) ||
                                    membership.getExitDate().equals(enrollment.getEnrollmentDate())) &&
                                    StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) &&
                                    enrollment.getSchoolOid().equals(oldSchool.getOid())) {
                                lastEntryEnrollment = enrollment;
                            }
                        }
                        if (lastEntryEnrollment != null &&
                                lastEntryEnrollment.getEnrollmentDate() != null &&
                                lastEntryEnrollment.getEnrollmentDate().after(attContext.getStartDate())) {
                            beginDate = lastEntryEnrollment.getEnrollmentDate();
                        }

                        /*
                         * Get the attendance days for the student in the school.
                         *
                         * NOTE: Student calendar code would be for the current school
                         * when reporting a prior school. We only have the current calendar code.
                         * This could report incorrect membership days if:
                         * 1. The calendar code is different between the student in the prior school
                         * and current school.
                         * 2. The calendars in the prior school indicated by the students current
                         * calendar ID
                         * and their actual calendar ID for that school are different enough to
                         * calculate
                         * different day counts.
                         */
                        int memberDays = m_registerData.m_enrollmentManager.getMembershipTotal(
                                student,
                                m_registerData.getCalendarDays(oldSchool, attContext, student.getCalendarCode()),
                                true,
                                beginDate,
                                lastDate,
                                oldSchool);
                        // See if there are member days.
                        if (memberDays > 0) {
                            // Find absences days for the date range provided.
                            Criteria criteria = new Criteria();
                            criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());
                            criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, oldSchool.getOid());
                            criteria.addGreaterThan(StudentAttendance.COL_DATE, beginDate);
                            criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, lastDate);
                            criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

                            ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(StudentAttendance.class,
                                    new String[] {"SUM(ATT_PORTION_ABSENT)"}, criteria);

                            Float absencesCount = Float.valueOf(0);
                            ReportQueryIterator iterator =
                                    getData().getBroker().getReportQueryIteratorByQuery(reportQuery);
                            try {
                                if (iterator.hasNext()) {
                                    Object[] row = (Object[]) iterator.next();
                                    if (row[0] != null) {
                                        absencesCount = Float.valueOf(row[0].toString());
                                    }
                                }
                            } finally {
                                iterator.close();
                            }

                            float absenceDays = 0;
                            if (absencesCount != null) {
                                absenceDays = absencesCount.floatValue();
                            }

                            membership.setMembershipDays(memberDays);
                            membership.setAbsent(absenceDays);
                        }
                    }
                }
            }
        }

        /**
         * Calculate and return the student grade level code for the student, YOG and report date.
         * The date may be in any school year. The appropriate district school year will be used.
         * The district school year boundary will determine when the grade level changes.
         *
         * @param student Student
         * @param yog int
         * @param checkDate PlainDate
         * @return String
         */
        private String getGradeLevelForStudent(Student student, int yog, PlainDate checkDate) {
            String gradeLevel = null;

            DistrictSchoolYearContext context = getSchoolYearForDate(checkDate);
            if (context == null) {
                context = m_registerData.getCurrentContext();
            }
            int schoolYear = context.getSchoolYear();
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getData().getBroker());

            List gradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, schoolYear,
                    m_registerData.m_gradeLevelMap);

            if (schoolYear == m_registerData.getCurrentContext().getSchoolYear() &&
                    gradeLevels.contains(student.getGradeLevel())) {
                gradeLevel = student.getGradeLevel();
            } else {
                gradeLevel = (String) gradeLevels.get(0);
            }

            return gradeLevel;
        }

        /**
         * Return the district school year context that contains the date provided.
         *
         * @param checkDate PlainDate
         * @return DistrictSchoolYearContext
         */
        private DistrictSchoolYearContext getSchoolYearForDate(PlainDate checkDate) {
            DistrictSchoolYearContext syContext = null;
            for (DistrictSchoolYearContext context : m_registerData.m_contexts) {
                if ((context.getStartDate().before(checkDate) || context.getStartDate().equals(checkDate)) &&
                        (context.getEndDate().after(checkDate) || context.getEndDate().equals(checkDate))) {
                    syContext = context;
                    break;
                }
            }
            return syContext;
        }
    }

    /**
     * A class for storing events pertaining to a student.
     *
     * @author X2 Development Corporation
     */
    protected static class MembershipAttendance {
        /*
         * Instance for school, dates, counts and codes.
         */
        float m_absent;
        PlainDate m_entryDate;
        PlainDate m_exitDate;
        String m_exitType;
        String m_exitStatus;
        int m_membershipDays;
        SisSchool m_school;
        SisSchool m_oldSchool;
        String m_type;
        String m_gradeLevel;

        /**
         * Constructor, set initial attendance and membership counts.
         *
         * @param type String
         * @param school SisSchool
         * @param entryDate PlainDate
         * @param exitDate PlainDate
         * @param exitCode String
         * @param exitStatus String
         */
        protected MembershipAttendance(String type, SisSchool school, PlainDate entryDate, PlainDate exitDate,
                String exitCode, String exitStatus) {
            m_entryDate = entryDate;
            m_exitDate = exitDate;
            m_school = school;
            m_type = type;
            m_exitType = exitCode;
            m_exitStatus = exitStatus;
        }

        /**
         * Return the number of days absent.
         *
         * @return float
         */
        protected float getAbsent() {
            return m_absent;
        }

        /**
         * Returns the membership begin date for the enrollment period.
         *
         * @return PlainDate
         */
        protected PlainDate getEntryDate() {
            return m_entryDate;
        }

        /**
         * Returns the membership end date for the enrollment period.
         *
         * @return PlainDate
         */
        protected PlainDate getExitDate() {
            return m_exitDate;
        }

        /**
         * Returns the students exit type.
         *
         * @return String
         */
        protected String getExitType() {
            return m_exitType;
        }

        /**
         * Returns the students exit status.
         *
         * @return String
         */
        protected String getExitStatus() {
            return m_exitStatus;
        }

        /**
         * Returns the gradeLevel.
         *
         * @return String
         */
        protected String getGradeLevel() {
            return m_gradeLevel;
        }

        /**
         * Return the accumulated membership count for the student.
         *
         * @return int
         */
        protected int getMembershipDays() {
            return m_membershipDays;
        }

        /**
         * Returns the oldSchool.
         *
         * @return School
         */
        protected SisSchool getOldSchool() {
            return m_oldSchool;
        }

        /**
         * Returns the school for this record.
         *
         * @return String
         */
        protected SisSchool getSchool() {
            return m_school;
        }

        /**
         * Returns the school for this record.
         *
         * @return String
         */
        protected String getSchoolOid() {
            return m_school.getOid();
        }

        /**
         * Returns the type for the students enrollment segment.
         *
         * @return String
         */
        protected String getType() {
            return m_type;
        }

        /**
         * Set the accumulated absent days.
         *
         * @param absent void
         */
        protected void setAbsent(float absent) {
            m_absent = absent;
        }

        /**
         * Sets the entry date to a new date.
         *
         * @param entryDate void
         */
        protected void setEntryDate(PlainDate entryDate) {
            m_entryDate = entryDate;
        }

        /**
         * Sets the gradeLevel.
         *
         * @param gradeLevel void
         */
        protected void setGradeLevel(String gradeLevel) {
            m_gradeLevel = gradeLevel;
        }

        /**
         * Set the accumulated membership count for the student.
         *
         * @param membership void
         */
        protected void setMembershipDays(int membership) {
            m_membershipDays = membership;
        }

        /**
         * Sets the oldSchool.
         *
         * @param oldSchool void
         */
        protected void setOldSchool(SisSchool oldSchool) {
            m_oldSchool = oldSchool;
        }

        /**
         * Set the record type.
         *
         * @param type void
         */
        protected void setRecordType(String type) {
            m_type = type;
        }

        /**
         * Set the school.
         *
         * @param school void
         */
        protected void setSchool(SisSchool school) {
            m_school = school;
        }
    }

    private static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";

    /*
     * Field id constants. These field id constants are for identifying fields in
     * the export field definition.
     */
    private static final String SASID_25_MEMBERSHIP_DAYS = "MEMBERSHIP_DAYS";
    private static final String SASID_6_FACILITY_1 = "FACILITY_1";
    /*
     * Other internal constants
     */
    // A record type of C happens when a student changes school or grade level
    // Before the start of school, most students have a record type of C
    // retained students do not have a C
    private static final String SASID_TYPE_C = "C";
    // new students entering the district or re-entering the district
    private static final String SASID_TYPE_R = "R";
    // withdrawals
    private static final String SASID_TYPE_U = "U";
    private static final String POLIO_SERIES_DEFINITION_OID = "himX2IPV";

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
     * Name for the "require at lease one member day" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";

    /**
     * Name for the "require facility code" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_FACILITY_PARAM = "requireFacility";

    /**
     * Name for the prior report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String PRIOR_REPORT_DATE_PARAM = "priorReportDate";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the enumerated "record type" parameter. The value is an Integer.
     */
    public static final String RECORD_TYPE_PARAM = "recordType";

    /**
     * Name for the option to report empty or non-empty sasid only. The value is a Integer.
     */
    public static final String SASID_SELECTION_PARAM = "sasidSelection";

    public static final String RESIDENT_TOWN_ALIAS = "PSIS04";

    // on student
    public static final String NEXUS_ALIAS = "NEXUS";

    // on student, the facility a student is placed at
    public static final String OUTSIDE_PLACEMENT_FACILITY_ALIAS = "Outside Placement Facility";
    public static final String SPECIAL_EDUCATION_FACILITY_ALIAS = "Special Education Facility";

    // on school
    public static final String OUTSIDE_PLACEMENT_SCHOOL_ALIAS = "Outplacement School";
    public static final String SCHOOL_STATE_ID_ALIAS = "StateId";
    public static final String SPECIAL_EDUCATION_SCHOOL_ALIAS = "Special Education School";

    /**
     * Other internal constants
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Collection<DistrictSchoolYearContext> m_contexts;
    protected EnrollmentManager m_enrollmentManager;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected PlainDate m_priorReportDate;
    protected Pattern m_illegalNameCharacters;
    protected PlainDate m_reportDate;
    protected Boolean m_requireMemberDay;
    protected Boolean m_requireFacility;
    protected HashMap m_schoolsToCalendars;
    protected Map<String, Collection<StudentEnrollment>> m_studentEnrollements;
    protected Map<String, Collection<HealthImmunizationDose>> m_studentPolioImmunization;
    protected String m_exitStatusPath;
    protected String m_recordType;
    protected Map<String, ReferenceCode> m_residentTownCodes;
    protected TreeMap m_gradeLevelMap;
    protected Map<String, ReferenceCode> m_outplacementFacilities;
    protected Map<String, ReferenceCode> m_specialEducationFacilities;
    protected Map<String, ReferenceCode> m_nexusCodes;
    protected Map<String, ReferenceCode> m_spedCodes;
    protected String m_birthTownBeanPath;
    protected Map<String, ReferenceCode> m_withdrawalCodes;
    protected Map<String, ReferenceCode> m_withdrawalReasons;

    /**
     * A map of reference codes for race codes, for use in the race code retriever.
     */
    protected Map<String, ReferenceCode> m_raceCodes;

    /**
     * This retriever will allow us to filter values based on the type of record we generated.
     * We pass in the allowed types for a field.
     */
    protected class FilterByType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            MembershipAttendance memb = sasidEntity.getMembershipAttendance();
            String type = memb.getType();
            String param = (String) field.getParameter();

            if (param.indexOf(type) > -1) {
                value = getProperty(entity.getBean(), field.getBeanPath());
            }

            return value;
        }
    }

    /**
     * Retrieve the record type.
     *
     * This returns one of the three values:
     * R = Register
     * C = Change
     * U = Unregister
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            MembershipAttendance memb = sasidEntity.getMembershipAttendance();
            String type = memb.getType();

            return type;
        }
    }

    /**
     * Retrieve the hispanic indicator from the person record.
     * Only return for Register or Change records.
     * Blank for Unregister records.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveHispanicOrLatinoIndicator implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;

            boolean hispanicOrLatinoInd = sasidEntity.getStudentPerson().getHispanicLatinoIndicator();

            String indicator = "N";
            if (hispanicOrLatinoInd) {
                indicator = "Y";
            }

            MembershipAttendance memb = sasidEntity.getMembershipAttendance();

            if (memb.getType().equals(SASID_TYPE_U)) {
                indicator = "";
            }

            return indicator;
        }
    }

    /**
     * The Class RetrieveResidentTown.
     */
    protected class RetrieveResidentTown implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            String defaultValue = entity.getFieldValue("DISTRICT");
            String residentTown = (String) getProperty(entity.getBean(), field.getBeanPath());
            String residentTownStateCode = null;
            MembershipAttendance memb = sasidEntity.getMembershipAttendance();

            if (!memb.getType().equals(SASID_TYPE_U)) {
                if (m_residentTownCodes.containsKey(residentTown)) {
                    ReferenceCode refCode = m_residentTownCodes.get(residentTown);
                    residentTownStateCode = refCode.getStateCode();
                }

                if (StringUtils.isEmpty(residentTownStateCode)) {
                    residentTownStateCode = defaultValue;
                }
            }

            return residentTownStateCode;
        }
    }

    /**
     * Retrieve the school Id from the membership record.
     * This is the school for the Student Enrollment record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFacilityCodes implements FieldRetriever {

        @SuppressWarnings("unused")
        private static final String CALC_PARAM_FACILITY_1 = "FACILITY-1";
        @SuppressWarnings("unused")
        private static final String CALC_PARAM_FACILITY_2 = "FACILITY-2";

        private String m_fieldSklOutpl;
        private String m_fieldSklSped;
        private String m_fieldSklStateId;
        private String m_fieldStdOutplFacility;
        private String m_fieldStdSpedFacility;

        /**
         *
         */
        public RetrieveFacilityCodes() {
            m_fieldSklOutpl = translateAliasToJavaName(OUTSIDE_PLACEMENT_SCHOOL_ALIAS, true);
            m_fieldSklSped = translateAliasToJavaName(SPECIAL_EDUCATION_SCHOOL_ALIAS, true);
            m_fieldSklStateId = translateAliasToJavaName(SCHOOL_STATE_ID_ALIAS, true);
            m_fieldStdOutplFacility = translateAliasToJavaName(OUTSIDE_PLACEMENT_FACILITY_ALIAS, true);
            m_fieldStdSpedFacility = translateAliasToJavaName(SPECIAL_EDUCATION_FACILITY_ALIAS, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String schoolIdState = null;
            SisSchool school = student.getSchool();

            String temp = (String) school.getFieldValueByBeanPath(m_fieldSklOutpl);

            if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(temp)) {
                String schoolId = (String) student.getFieldValueByBeanPath(m_fieldStdOutplFacility);
                if (!StringUtils.isEmpty(schoolId)) {
                    schoolIdState = lookupStateValue(SisStudent.class, m_fieldStdOutplFacility, schoolId);
                }
            }

            temp = (String) school.getFieldValueByBeanPath(m_fieldSklSped);
            if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(temp)) {
                String schoolId = (String) student.getFieldValueByBeanPath(m_fieldStdSpedFacility);
                if (!StringUtils.isEmpty(schoolId)) {
                    schoolIdState = lookupStateValue(SisStudent.class, m_fieldStdSpedFacility, schoolId);

                }
            }

            return schoolIdState != null ? schoolIdState
                    : student.getSchool().getFieldValueByBeanPath(m_fieldSklStateId);
        }
    }

    /**
     * Retrieve the district enrollment date.
     *
     * This is the date at which the student first entered a school and began membership in
     * the district in the format MMDDYYYY where "M" = Month, "D" = Day, "Y" = Year. If a
     * student has re-entered your district, it will reflect the date of re-entry. In either
     * case the date must be after their membership in a prior district ends. This is a
     * mandatory field in the Register module and disallowed in the rest of the collections.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveMembershipDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SasidRegisterEntity attEnt = (SasidRegisterEntity) entity;
            MembershipAttendance memb = attEnt.getMembershipAttendance();
            Object membershipDate = null;
            String param = (String) field.getParameter();

            if (field.getFormatter() != null) {

                if (("ENTRY".equals(param) && memb.getType().equals(SASID_TYPE_R))) {
                    membershipDate = memb.getEntryDate();
                } else if ("EXIT".equals(param) && memb.getType().equals(SASID_TYPE_U)) {
                    membershipDate = memb.getExitDate();
                } else if ("C-ENTRY".equals(param)
                        && (memb.getType().equals(SASID_TYPE_R) || memb.getType().equals(SASID_TYPE_C))) {
                    membershipDate = memb.getEntryDate();
                } else if ("C-EXIT".equals(param)
                        && (memb.getType().equals(SASID_TYPE_U) || memb.getType().equals(SASID_TYPE_C))) {
                    membershipDate = memb.getExitDate();
                }
            }
            return membershipDate;
        }
    }

    /**
     * Retrieve the polio vaccination date.
     *
     * Date of student?s first polio vaccination in the format MMDDYYYY
     * where "M" = Month, "D" = Day, "Y" = Year. This is a tie-breaker element.
     * This field is optional for register and disallowed for change and unregister.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePolioDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SasidRegisterEntity attEnt = (SasidRegisterEntity) entity;
            MembershipAttendance memb = attEnt.getMembershipAttendance();
            String studentOid = entity.getBean().getOid();
            Object polioDate = null;

            if (memb.getType().equals(SASID_TYPE_R)) {
                if (m_studentPolioImmunization.containsKey(studentOid)) {
                    Collection<HealthImmunizationDose> immunizations = m_studentPolioImmunization.get(studentOid);
                    for (HealthImmunizationDose immunization : immunizations) {
                        polioDate = immunization.getDate();
                        break;
                    }
                }
            }
            return polioDate;
        }
    }

    /**
     * Retrieve the exit type.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveExitType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SasidRegisterEntity attEnt = (SasidRegisterEntity) entity;
            MembershipAttendance memb = attEnt.getMembershipAttendance();
            String exitType = memb.getExitType();
            return exitType;
        }
    }

    /**
     * Retrieve the exit status.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveExitStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SasidRegisterEntity attEnt = (SasidRegisterEntity) entity;
            MembershipAttendance memb = attEnt.getMembershipAttendance();
            String exitStatus = memb.getExitStatus();
            return exitStatus;
        }
    }

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
    protected class RetrieveMembershipDays implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SasidRegisterEntity attEnt = (SasidRegisterEntity) entity;
            MembershipAttendance memb = attEnt.getMembershipAttendance();
            String param = (String) field.getParameter();

            float attendCount = 0;
            if (param != null && param.equals("PRESENT")) {
                // Get days in attendance count. This is membership minus absences.
                float stdMemb = memb.getMembershipDays();
                float absentCount = memb.getAbsent();
                attendCount = stdMemb - absentCount;
                if (attendCount < 0) {
                    entity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity, field,
                                    "Attendance less than zero",
                                    "Attendance=" + STYLE_BOLD + Float.toString(attendCount) + STYLE_END));
                    attendCount = 0;
                }
            } else if (param != null && param.equals("MEMBERSHIP")) {
                // Get all membership days in this school for this student.
                attendCount = memb.getMembershipDays();
            }

            /*
             * Convert to string for display.
             * set to null if type "R" since thid field is disallowed for register records.
             */
            String membershipCountAsString = null;
            if (!memb.getType().equals(SASID_TYPE_R)) {
                membershipCountAsString = Integer.toString((int) attendCount);
            }

            return membershipCountAsString;
        }
    }

    /***
     * For Greenwich, nexus district is a student in SPED or a value is entered in a nexus field.
     * For others, use the student sped status code to determine active.
     *
     * If there is not value in the nexus field for a SPED value, use the district code.
     */
    protected class RetrieveNexus implements FieldRetriever {
        private String activeSpedCode = null;

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String nexus = "";

            String defaultNexusDistrict = entity.getFieldValue("DISTRICT");

            if (activeSpedCode == null) {
                activeSpedCode = PreferenceManager.getPreferenceValue(data.getOrganization(),
                        SisPreferenceConstants.SPED_ACTIVE_CODE);
            }

            SisStudent student = (SisStudent) entity.getBean();

            String spedStatus = student.getSpedStatusCode();
            String sped = (String) student.getFieldValueByAlias("PSIS16");

            if (activeSpedCode.equals(spedStatus) || !StringUtils.isEmpty(sped)) {
                if (m_spedCodes.containsKey(sped)) {
                    ReferenceCode refCode = m_spedCodes.get(sped);
                    if (refCode != null) {
                        if ("Y".equalsIgnoreCase(refCode.getStateCode())) {
                            nexus = defaultNexusDistrict;
                        }
                    }
                }
            }

            String nexusFieldValue = (String) student.getFieldValueByAlias(NEXUS_ALIAS);
            if (!StringUtils.isEmpty(nexusFieldValue)) {
                if (m_nexusCodes.containsKey(nexusFieldValue)) {
                    ReferenceCode refCode = m_nexusCodes.get(nexusFieldValue);
                    if (refCode != null) {
                        String stateNexusCode = refCode.getStateCode();
                        {
                            if (!StringUtils.isEmpty(stateNexusCode)) {
                                nexus = stateNexusCode;
                            }
                        }
                    }
                }
            }

            return nexus;
        }
    }

    /***
     * For Greenwich, nexus district is a student in SPED or a value is entered in a nexus field.
     * If there is not value in the nexus field for a SPED value, use the district code.
     * The parameter passed in is the default nexus code for students that do not have one set.
     */
    protected class RetrieveNexusDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            PlainDate nexusDate = null;

            SisStudent student = (SisStudent) entity.getBean();
            SasidRegisterEntity attEnt = (SasidRegisterEntity) entity;
            MembershipAttendance memb = attEnt.getMembershipAttendance();

            String param = (String) field.getParameter();
            String nexusDistrict = entity.getFieldValue("NEXUS_DISTRICT");
            nexusDate = (PlainDate) getProperty(student, field.getBeanPath());

            if (!StringUtils.isEmpty(nexusDistrict)) {
                if (nexusDate == null) {
                    if (memb.getType().equals(SASID_TYPE_R) && "ENTRY".equals(param)) {
                        nexusDate = memb.getEntryDate();
                    } else if (memb.getType().equals(SASID_TYPE_U) && "EXIT".equals(param)) {
                        nexusDate = memb.getExitDate();
                    }
                }
            } else {
                nexusDate = null;
            }

            return nexusDate;
        }
    }

    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a true/false value
     * indicating the presence of the race code record.
     *
     * The calculation parameter should be a string with three characters:
     * character 1:
     * The character to return if the requested race code is present.
     * character 2:
     * The character to return if the requested race code is not present.
     * character(s) 3+:
     * The reference code state code value in the reference table for race codes.
     * In CT, these are arbitraily defined for this report and NOT defined
     * by the CT DOE, this is (based on values from MA):
     * "1" - Caucasian
     * "2" - African
     * "4" - Asian
     * "8" - Native/Alaskan
     * "16" - Pacific
     *
     * Ex: "YN4" searches for the Asian code, returns "Y" if present, "N" otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            String trueChar = param.substring(0, 1);
            String falseChar = param.substring(1, 2);
            String requestCode = param.substring(2);

            SisStudent student = (SisStudent) entity.getBean();

            String raceCode = falseChar;
            Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());

            // Find the reference code that we are looking for.
            ReferenceCode refCode = m_raceCodes.get(requestCode);
            if (refCode != null && races != null) {
                for (Race race : races) {
                    if (refCode.getCode().equals(race.getRaceCode())) {
                        raceCode = trueChar;
                        break;
                    }
                }
            }

            SasidRegisterEntity attEnt = (SasidRegisterEntity) entity;
            MembershipAttendance memb = attEnt.getMembershipAttendance();
            // do not report race if record type = U
            if (memb.getType().equals(SASID_TYPE_U)) {
                raceCode = "";
            }

            return raceCode;
        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * For first and last names.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = null;
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            MembershipAttendance membership = sasidEntity.getMembershipAttendance();
            String type = membership.getType();
            String param = (String) field.getParameter();

            if (param.indexOf(type) > -1) {
                value = (String) getProperty(entity.getBean(), field.getBeanPath());
                if (value != null) {
                    Matcher matcher = m_illegalNameCharacters.matcher(value);
                    value = matcher.replaceAll("");
                }
            }

            return value;
        }
    }

    /**
     * Validate membership date.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateMembershipDate implements FieldValidator {

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

            SasidRegisterEntity attEnt = (SasidRegisterEntity) entity;
            MembershipAttendance memb = attEnt.getMembershipAttendance();
            Object membershipDate = memb.getEntryDate();
            String param = (String) field.getParameter();

            if (("ENTRY".equals(param) && memb.getType().equals(SASID_TYPE_R)) ||
                    ("EXIT".equals(param) && memb.getType().equals(SASID_TYPE_U))) {
                if (membershipDate == null) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Membership date is invalid.",
                            "SASID04=" + STYLE_BOLD + membershipDate + STYLE_END + ", SASID04=" + STYLE_BOLD + value
                                    + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        /*
         * Get core parameters
         */
        m_priorReportDate = (PlainDate) getParameter(PRIOR_REPORT_DATE_PARAM);
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_requireFacility = (Boolean) getParameter(REQUIRE_FACILITY_PARAM);
        m_requireMemberDay = (Boolean) getParameter(REQUIRE_MEMBER_DAY_PARAM);
        m_recordType = (String) getParameter(RECORD_TYPE_PARAM);

        // all records
        if (m_recordType.equalsIgnoreCase("A")) {
            m_recordType = null;
        }

        m_residentTownCodes = getReferenceCodesForAlias(RESIDENT_TOWN_ALIAS);
        m_gradeLevelMap = StudentManager.buildGradeLevelMap(getBroker());

        String withdrawalTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
        m_withdrawalCodes = getReferenceCodeRefTableOid(withdrawalTableOid);

        withdrawalTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_REASONS);
        m_withdrawalReasons = getReferenceCodeRefTableOid(withdrawalTableOid);

        m_exitStatusPath = translateAliasToJavaName("SASID24", false);
        if (StringUtils.isEmpty(m_exitStatusPath)) {
            m_exitStatusPath = StudentEnrollment.COL_STATUS_CODE;
        }
        m_outplacementFacilities = getReferenceCodesForAlias(OUTSIDE_PLACEMENT_FACILITY_ALIAS);
        m_specialEducationFacilities = getReferenceCodesForAlias(SPECIAL_EDUCATION_FACILITY_ALIAS);

        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

        // Get race code reference codes for use in the race retriever.
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbRaceCodes");
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);

        m_nexusCodes = getReferenceCodesForAlias(NEXUS_ALIAS);

        m_spedCodes = getReferenceCodesForAlias("PSIS16");


        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            X2Criteria studentCriteria = getStudentCriteria();
            String fieldExcludeStd = translateAliasToJavaName(ALIAS_EXCLUDE_STD, true);
            if (!StringUtils.isEmpty(fieldExcludeStd)) {
                studentCriteria.addNotEqualTo(fieldExcludeStd, BooleanAsStringConverter.TRUE);
            }

            QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCriteria);

            Integer sort = (Integer) getParameter(SORT_PARAM);
            switch (sort == null ? 0 : sort.intValue()) {
                case 0: // Name
                    studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(Student.COL_YOG);
                    studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentQuery.addOrderByAscending(Student.REL_SCHOOL + PATH_DELIMITER +
                            translateAliasToJavaName("DOE15", true));
                    studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentQuery.addOrderByAscending(Student.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentQuery.addOrderByAscending(Student.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(SasidRegisterEntity.class);

            int count = getBroker().getCount(studentQuery);

            /*
             * Load the race codes for all students included in the export.
             */
            SubQuery subQuery = new SubQuery(Student.class, Student.COL_PERSON_OID, studentCriteria);
            Criteria raceCriteria = new Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);

            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, count);

            // Load Polio immunization map.
            loadPolioImmunizations(studentCriteria, count);

            // Load codes and support data from database.
            loadEnrollmentData(studentCriteria, count);

            // Build maps of retriever functions and validator functions
            HashMap calcs = new HashMap<String, FieldRetriever>();

            calcs.put("SASID-CLEAN", new RetrieveStripNameChar());
            calcs.put("SASID-DATE", new RetrieveMembershipDate());
            calcs.put("SASID-EXIT-STATUS", new RetrieveExitStatus());
            calcs.put("SASID-EXIT-TYPE", new RetrieveExitType());
            calcs.put("SASID-FACILITY", new RetrieveFacilityCodes());
            calcs.put("SASID-FILTER", new FilterByType());
            calcs.put("SASID-HISPANIC-IND", new RetrieveHispanicOrLatinoIndicator());
            calcs.put("SASID-MEMBER-DAYS", new RetrieveMembershipDays());
            calcs.put("SASID-NEXUS", new RetrieveNexus());
            calcs.put("SASID-NEXUS-DATE", new RetrieveNexusDate());
            calcs.put("SASID-POLIO-DATE", new RetrievePolioDate());
            calcs.put("SASID-RACE", new RetrieveRace());
            calcs.put("SASID-REC-TYPE", new RetrieveType());
            calcs.put("SASID-TOWN", new RetrieveResidentTown());

            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put("SASID-ENTRY-DATE", new ValidateMembershipDate());

            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param context DistrictSchoolYearContext
     * @param calendar String
     * @return Set of PlainDate objects
     */
    protected Set getCalendarDays(SisSchool school, DistrictSchoolYearContext context, String calendar) {
        String mapKey = school.getOid() + "-" + context.getOid();
        if (!m_schoolsToCalendars.containsKey(mapKey)) {
            PlainDate startDate = null;
            Schedule schedule = null;
            if (school != null) {
                schedule = school.getActiveSchedule();
            }
            if (schedule != null) {
                startDate = schedule.getStartDate();
            }
            PlainDate endDate = m_reportDate;
            if (startDate != null && startDate.after(context.getStartDate())) {
                startDate = context.getStartDate();
            }
            if (m_reportDate.after(context.getEndDate())) {
                endDate = context.getEndDate();
            }
            Map calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, endDate, context.getOid());
            m_schoolsToCalendars.put(mapKey, calendarData);
        }

        return (Set) ((Map) m_schoolsToCalendars.get(mapKey)).get(calendar);
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
     * Builds a criteria for the students that should be reported
     * (not considering user input parameters).
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        /*
         * Primary students
         */
        Criteria primaryCriteria = new Criteria();
        Criteria activeCriteria = new Criteria();
        Criteria enrollmentCriteria = new Criteria();

        activeCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
        enrollmentCriteria.addGreaterThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_priorReportDate);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);

        if (isSchoolContext()) {
            activeCriteria.addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            activeCriteria.addEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            activeCriteria.addEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        SubQuery enrollmentSubQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);

        Criteria orCriteria = new Criteria();
        orCriteria.addIn(X2BaseBean.COL_OID, enrollmentSubQuery);

        primaryCriteria.addAndCriteria(activeCriteria);
        primaryCriteria.addOrCriteria(orCriteria);
        return primaryCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private X2Criteria getStudentCriteria() {
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        // Option for empty, non-empty or all sasid.
        Integer sasidSelection = (Integer) getParameter(SASID_SELECTION_PARAM);
        if (sasidSelection != null) {
            if (sasidSelection.intValue() == 1) {
                userCriteria.addEmpty(Student.COL_STATE_ID, getBroker().getPersistenceKey());
            } else if (sasidSelection.intValue() == 2) {
                userCriteria.addNotEmpty(Student.COL_STATE_ID, getBroker().getPersistenceKey());
            }
        }

        // Student selection criteria.
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(Student.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(Student.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(Student.COL_STATE_ID, queryString);
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
        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addAndCriteria(userCriteria);
        studentCriteria.addAndCriteria(getReportingCriteria());

        return studentCriteria;
    }

    /**
     * Loads the enrollment data required by this export.
     *
     * @param studentCriteria Criteria
     * @param count int
     */
    private void loadEnrollmentData(Criteria studentCriteria, int count) {
        m_contexts = getBroker().getCollectionByQuery(new QueryByCriteria(DistrictSchoolYearContext.class));

        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_schoolsToCalendars = new HashMap();

        ArrayList typeCodes = new ArrayList(2);
        typeCodes.add(StudentEnrollment.ENTRY);
        typeCodes.add(StudentEnrollment.WITHDRAWAL);
        typeCodes.add(StudentEnrollment.YOG_CHANGE);

        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentsSubQuery);
        criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, typeCodes);
        criteria.addNotNull(StudentEnrollment.COL_ENROLLMENT_DATE);
        criteria.addNotEmpty(StudentEnrollment.COL_SCHOOL_OID, getBroker().getPersistenceKey());

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, true);
        query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, true);

        m_studentEnrollements =
                getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, count);
    }

    /**
     * Load student immunizations into a map by student Oid.
     *
     * @param studentCriteria Criteria
     * @param count int
     */
    private void loadPolioImmunizations(Criteria studentCriteria, int count) {
        /*
         * Load student polio immunization.
         */
        SubQuery subQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
        Criteria polioCriteria = new Criteria();
        polioCriteria.addEqualTo(HealthImmunizationDose.REL_IMMUNIZATION_SERIES + PATH_DELIMITER +
                HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID, POLIO_SERIES_DEFINITION_OID);
        polioCriteria.addIn(HealthImmunizationDose.COL_STUDENT_OID, subQuery);

        QueryByCriteria polioQuery = new QueryByCriteria(HealthImmunizationDose.class, polioCriteria);
        polioQuery.addOrderByAscending(HealthImmunizationDose.COL_DATE);

        m_studentPolioImmunization =
                getBroker().getGroupedCollectionByQuery(polioQuery, HealthImmunizationDose.COL_STUDENT_OID, count);
    }

    /**
     * Return a map of reference codes for the field identified by the alias.
     *
     * @param alias String
     * @return Map<code, ReferenceCode>
     */
    private Map<String, ReferenceCode> getReferenceCodesForAlias(String alias) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField =
                dictionary.findDataDictionaryFieldByAlias(alias);

        Map<String, ReferenceCode> refCodes = new HashMap<String, ReferenceCode>();

        if (dictionaryField == null) {
            addSetupError("data dictionary alias is missing",
                    "alias: " + alias + " is missing from the data dictionary");
        } else {
            ReferenceTable refTable = dictionaryField.getReferenceTable();
            if (refTable == null) {
                addSetupError("missing reference table", "reference table not found for the " + alias + " alias ");
            } else {
                refCodes = refTable.getCodeMap(getBroker());
            }

        }

        return refCodes;
    }

    /**
     * Return a map of reference codes for the reference table identified by the reference table
     * oid.
     *
     * @param refTblOid String
     * @return Map<code, ReferenceCode>
     */
    private Map<String, ReferenceCode> getReferenceCodeRefTableOid(String refTblOid) {
        Map<String, ReferenceCode> refCodes = new HashMap<String, ReferenceCode>();
        ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, refTblOid);
        if (refTable == null) {
            addSetupError("missing reference table", "reference table not found for RTB_OID " + refTblOid);
        } else {
            refCodes = refTable.getCodeMap(getBroker());
        }
        return refCodes;
    }
}
